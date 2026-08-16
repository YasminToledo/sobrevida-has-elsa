#============================================================
# Figuras.R
#
# Redesenha, em versão pronta para publicação, as figuras que os
# scripts de análise já geraram
#============================================================

suppressPackageStartupMessages({
  library(survival)
  library(ggplot2)
  library(scales)
  library(cowplot)
  library(viridisLite)
})

# os .rds ficam na pasta do projeto; o script funciona rodando de lá
# ou de uma subpasta (ex.: "pasta sem título")
acha <- function(arq) {
  for (caminho in c(arq, file.path("..", arq)))
    if (file.exists(caminho)) return(caminho)
  stop("não encontrei ", arq, " nem em . nem em ..", call. = FALSE)
}

RAIZ <- dirname(acha("base_surv2.rds"))
DIR  <- file.path(RAIZ, "figuras_publicacao")
dir.create(DIR, showWarnings = FALSE)

FONTE   <- "Helvetica"
DPI     <- 300
CINZA   <- "grey25"

#------------------------------------------------------------
# 0. Utilitários de estilo e de formatação em português
#------------------------------------------------------------

tema_pub <- function(base = 10) {
  theme_bw(base_size = base, base_family = FONTE) +
    theme(
      panel.border      = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(linewidth = 0.25, colour = "grey92"),
      axis.line         = element_line(linewidth = 0.4, colour = CINZA),
      axis.ticks        = element_line(linewidth = 0.4, colour = CINZA),
      axis.text         = element_text(colour = CINZA),
      # títulos dos eixos e do painel propositalmente maiores que os
      # rótulos dos eixos: é o que sobrevive à redução da figura na revista
      axis.title        = element_text(colour = "black", size = rel(1.25)),
      axis.title.x      = element_text(margin = margin(t = 5)),
      axis.title.y      = element_text(margin = margin(r = 5)),
      legend.key        = element_blank(),
      legend.background = element_blank(),
      legend.title      = element_blank(),
      legend.key.height = unit(0.9, "lines"),
      plot.title        = element_text(face = "bold", size = rel(1.35), hjust = 0,
                                       margin = margin(b = 4)),
      plot.subtitle     = element_text(size = rel(0.95), colour = CINZA, hjust = 0),
      plot.margin       = margin(5, 8, 4, 4)
    )
}

# vírgula decimal
vg <- function(x, dig = 2) sub("\\.", ",", formatC(x, format = "f", digits = dig))

# p com DUAS casas decimais, como no texto de Resultados.
# Abaixo de 0,01 duas casas arredondariam para 0,00, então esses
# valores ficam com três casas (e p < 0,001 vira desigualdade).
fmt_p <- function(p) {
  if (is.na(p))    return("")
  if (p < 0.001)   return("p < 0,001")
  if (p < 0.01)    return(paste0("p = ", vg(p, 3)))
  paste0("p = ", vg(p, 2))
}

# nota de rodapé para as variáveis com valores faltantes:
# o log-rank e as curvas usam só quem tem informação
nota_na <- function(v, dados) {
  k <- sum(is.na(dados[[v]]))
  if (k == 0) "" else sprintf("\n%d sem informação", k)
}

p_logrank <- function(v, dados) {
  sd <- survdiff(as.formula(paste("Surv(tempo_anos, HAS_status_num) ~", v)),
                 data = dados)
  pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
}

p_peto <- function(v, dados) {
  sd <- survdiff(as.formula(paste("Surv(tempo_anos, HAS_status_num) ~", v)),
                 data = dados, rho = 1)
  pchisq(sd$chisq, length(sd$n) - 1, lower.tail = FALSE)
}

# paleta segura para daltônicos
OKABE <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
           "#56B4E9", "#8C6D31", "#000000")

paleta <- function(k, ordenada = TRUE) {
  if (ordenada) viridisLite::mako(k, begin = 0.12, end = 0.78, direction = -1)
  else OKABE[seq_len(k)]
}

TIPOS <- c("solid", "22", "42", "1343", "73")   # também legível em P&B

salvar <- function(objeto, nome, largura, altura) {
  # PNG em 300 dpi (submissão) ...
  png(file.path(DIR, paste0(nome, ".png")),
      width = largura, height = altura, units = "in", res = DPI, bg = "white")
  print(objeto); dev.off()

  # ... e PDF vetorial (edição / impressão sem perda)
  arq_pdf <- file.path(DIR, paste0(nome, ".pdf"))
  if (capabilities("aqua")) {
    grDevices::quartz(file = arq_pdf, type = "pdf",
                      width = largura, height = altura)
  } else {
    pdf(arq_pdf, width = largura, height = altura,
        family = FONTE, encoding = "ISOLatin1")
  }
  print(objeto); dev.off()

  message("figura salva: ", file.path(DIR, nome), ".png / .pdf")
}

#------------------------------------------------------------
# 0.1 Extração de curvas (sem refazer nenhuma estimativa)
#------------------------------------------------------------

# data.frame com os degraus de um survfit, um grupo por estrato
extrai_surv <- function(fit, niveis = NULL) {
  grupo <- if (is.null(fit$strata)) rep("Todos", length(fit$time))
           else rep(sub("^[^=]*=", "", names(fit$strata)), fit$strata)

  d <- data.frame(
    tempo    = fit$time,
    surv     = fit$surv,
    lower    = if (is.null(fit$lower)) NA_real_ else fit$lower,
    upper    = if (is.null(fit$upper)) NA_real_ else fit$upper,
    n.censor = fit$n.censor,
    grupo    = grupo,
    stringsAsFactors = FALSE
  )

  # ponto inicial (t = 0, S = 1) de cada grupo
  ini <- do.call(rbind, lapply(unique(d$grupo), function(g)
    data.frame(tempo = 0, surv = 1, lower = 1, upper = 1,
               n.censor = 0, grupo = g, stringsAsFactors = FALSE)))

  d <- rbind(ini, d)
  d <- d[order(match(d$grupo, unique(d$grupo)), d$tempo), ]

  if (!is.null(niveis)) d$grupo <- factor(d$grupo, levels = niveis)
  d
}

# coordenadas em escada, para a faixa de IC acompanhar os degraus
escada <- function(d) {
  do.call(rbind, lapply(split(d, d$grupo), function(g) {
    g <- g[order(g$tempo), ]
    n <- nrow(g)
    if (n < 2) return(g)
    data.frame(
      tempo = c(as.vector(rbind(g$tempo[-n], g$tempo[-1])), g$tempo[n]),
      surv  = c(rep(g$surv[-n],  each = 2), g$surv[n]),
      lower = c(rep(g$lower[-n], each = 2), g$lower[n]),
      upper = c(rep(g$upper[-n], each = 2), g$upper[n]),
      grupo = g$grupo[1],
      stringsAsFactors = FALSE
    )
  }))
}

# número de participantes em risco (do próprio survfit)
em_risco <- function(fit, tempos, niveis = NULL) {
  s <- summary(fit, times = tempos, extend = TRUE)
  g <- if (is.null(fit$strata)) rep("Todos", length(s$time))
       else sub("^[^=]*=", "", as.character(s$strata))
  d <- data.frame(tempo = s$time, n = s$n.risk, grupo = g,
                  stringsAsFactors = FALSE)
  if (!is.null(niveis)) d$grupo <- factor(d$grupo, levels = niveis)
  d
}

QUEBRAS <- seq(0, 12, by = 2)

# painel da tabela de risco, alinhado com a curva
#   linhas = número de faixas reservadas; deixa a tabela com a mesma
#   altura em painéis com números diferentes de categorias
painel_risco <- function(tr, niveis, base = 9, rotular = TRUE, linhas = NULL) {
  niveis_y <- rev(niveis)
  if (!is.null(linhas) && linhas > length(niveis))
    niveis_y <- c(strrep(" ", seq_len(linhas - length(niveis))), niveis_y)

  tr$grupo <- factor(as.character(tr$grupo), levels = niveis_y)
  ggplot(tr, aes(tempo, grupo, label = n)) +
    # no tempo 0 o número é alinhado à esquerda, para não invadir o rótulo
    geom_text(aes(hjust = ifelse(tempo == 0, 0, 0.5)),
              size = base / .pt * 0.95, family = FONTE, colour = CINZA) +
    scale_x_continuous(limits = c(0, 13.2), breaks = QUEBRAS, expand = c(0, 0)) +
    scale_y_discrete(limits = niveis_y,
                     labels = if (rotular) waiver() else function(x) rep("", length(x))) +
    labs(x = NULL, y = NULL, title = "Participantes em risco") +
    theme_bw(base_size = base, base_family = FONTE) +
    theme(
      panel.grid   = element_blank(),
      panel.border = element_blank(),
      axis.line    = element_blank(),
      axis.ticks   = element_blank(),
      axis.text.x  = element_blank(),
      axis.text.y  = element_text(colour = CINZA, hjust = 1,
                                  margin = margin(r = 5)),
      plot.title   = element_text(size = rel(1.15), colour = "black", hjust = 0),
      plot.margin  = margin(0, 8, 2, 4)
    )
}

# curva de Kaplan-Meier em estilo de publicação
grafico_km <- function(fit, niveis, rotulo_grupo = NULL, ordenada = TRUE,
                       ic = TRUE, base = 10, legenda = c(0.02, 0.06),
                       n_por_grupo = NULL, anotacao = NULL,
                       quebra_rotulo = 24) {

  d  <- extrai_surv(fit, niveis)
  dd <- escada(d)
  k  <- length(niveis)

  cores <- paleta(k, ordenada)
  rot   <- if (is.null(n_por_grupo)) niveis
           else sprintf("%s (n = %d)", niveis, n_por_grupo)

  # se algum rótulo for longo demais para o painel, o n vai para a
  # segunda linha - em todos, para a legenda ficar uniforme
  if (any(nchar(rot) > quebra_rotulo)) rot <- sub(" \\(n = ", "\n(n = ", rot)

  p <- ggplot()

  if (ic && !all(is.na(dd$lower)))
    p <- p + geom_ribbon(data = dd,
                         aes(tempo, ymin = lower, ymax = upper, fill = grupo),
                         alpha = 0.14, show.legend = FALSE)

  p <- p +
    geom_step(data = d, aes(tempo, surv, colour = grupo, linetype = grupo),
              direction = "hv", linewidth = 0.6) +
    geom_point(data = subset(d, n.censor > 0),
               aes(tempo, surv, colour = grupo),
               shape = 3, size = 0.9, stroke = 0.5, show.legend = FALSE) +
    scale_colour_manual(values = cores, labels = rot, drop = FALSE) +
    scale_fill_manual(values = cores, drop = FALSE) +
    scale_linetype_manual(values = TIPOS[seq_len(k)], labels = rot, drop = FALSE) +
    scale_x_continuous(limits = c(0, 13.2), breaks = QUEBRAS, expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                       labels = function(x) vg(x, 1), expand = c(0, 0)) +
    labs(x = "Tempo de acompanhamento (anos)",
         y = "Probabilidade de permanecer sem HAS") +
    tema_pub(base) +
    theme(legend.position = "inside",
          legend.position.inside = legenda,
          legend.justification = c(0, 0),
          legend.text = element_text(size = rel(1.0)))

  if (!is.null(anotacao))
    p <- p + annotate("text", x = 13.0, y = 0.97, label = anotacao,
                      hjust = 1, vjust = 1, size = base / .pt * 0.85,
                      family = FONTE, colour = CINZA)

  if (!is.null(rotulo_grupo)) p <- p + labs(title = rotulo_grupo)
  p
}

# curva + tabela de risco, com os eixos alinhados
km_com_tabela <- function(fit, niveis, ..., alturas = c(1, 0.20)) {
  g1 <- grafico_km(fit, niveis, ...)
  g2 <- painel_risco(em_risco(fit, QUEBRAS, niveis), niveis)
  cowplot::plot_grid(g1, g2, ncol = 1, rel_heights = alturas,
                     align = "v", axis = "lr")
}

#============================================================
# 1. BASES - exatamente como nos scripts originais
#============================================================

base_surv2 <- readRDS(acha("base_surv2.rds"))

# reagrupamento do IMC (idêntico ao "Script cox - trab sobrevida.R")
base_surv2$imc_cat3 <- factor(
  ifelse(base_surv2$imc_onda1 %in% c("Magreza", "Eutrofia"),
         "Sem excesso de peso",
         as.character(base_surv2$imc_onda1)),
  levels = c("Sem excesso de peso", "Sobrepeso", "Obesidade")
)

vars_cox <- c("tempo_anos", "HAS_status_num", "sexo", "idade_cat", "racacor",
              "escolaridade", "imc_cat3", "ativfisica", "tabagismo", "usoalcool")

base_cox <- base_surv2[complete.cases(base_surv2[, vars_cox]), ]

covars <- c("sexo", "idade_cat", "racacor", "escolaridade",
            "imc_cat3", "ativfisica", "tabagismo", "usoalcool")

rotulos <- c(sexo         = "Sexo",
             idade_cat    = "Faixa etária",
             racacor      = "Raça/cor",
             escolaridade = "Escolaridade",
             imc_cat3     = "Estado nutricional (IMC)",
             ativfisica   = "Atividade física",
             tabagismo    = "Tabagismo",
             usoalcool    = "Uso de álcool")

# variáveis com categorias ordenadas (paleta sequencial)
ORDENADA <- c(sexo = FALSE, idade_cat = TRUE, racacor = FALSE,
              escolaridade = TRUE, imc_onda1 = TRUE, imc_cat3 = TRUE,
              ativfisica = TRUE, tabagismo = TRUE, usoalcool = TRUE)

cat("\n--- conferência das bases ---\n")
cat("base_surv2: n =", nrow(base_surv2),
    "| eventos =", sum(base_surv2$HAS_status_num == 1), "\n")
cat("base_cox:   n =", nrow(base_cox),
    "| eventos =", sum(base_cox$HAS_status_num == 1), "\n")

#============================================================
# 2. FIGURA 1 - histogramas (dois painéis numa figura só)
#    binagem idêntica: hist(..., breaks = 12)
#============================================================

hist_pub <- function(x, titulo, cor, base = 9.5) {
  h <- hist(x, breaks = 12, plot = FALSE)          # mesmos intervalos do script
  d <- data.frame(esq = head(h$breaks, -1),
                  dir = tail(h$breaks, -1),
                  n   = h$counts)

  ggplot(d) +
    geom_rect(aes(xmin = esq, xmax = dir, ymin = 0, ymax = n),
              fill = cor, colour = "white", linewidth = 0.3) +
    scale_x_continuous(breaks = pretty(range(h$breaks), 7)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.06))) +
    labs(x = "Tempo (anos)", y = "Número de participantes", title = titulo) +
    tema_pub(base) +
    theme(panel.grid.major.x = element_blank(),
          plot.margin = margin(5, 8, 4, 14))
}

f1a <- hist_pub(base_surv2$tempo_anos[base_surv2$HAS_status_num == 1],
                "Tempo até o desenvolvimento de HAS", "#3B6E8F")
f1b <- hist_pub(base_surv2$tempo_anos[base_surv2$HAS_status_num == 0],
                "Tempo até a censura", "#8FA9B8")

f1 <- cowplot::plot_grid(f1a, f1b, ncol = 2,
                         labels = c("A", "B"), label_size = 13,
                         label_fontfamily = FONTE)

salvar(f1, "Figura_1_histogramas", 9.0, 3.9)

#============================================================
# 3. FIGURA 2 - painel principal de Kaplan-Meier
#    (A) coorte toda, (B) sexo, (C) IMC, (D) uso de álcool
#    Substitui as antigas figuras 3, 4 e 5.
#============================================================

# célula do painel: curva + tabela de participantes em risco
celula_km <- function(fit, niveis, titulo, ordenada, ic, anotacao,
                      n_por_grupo = NULL, legenda = c(0.01, 0.02),
                      base = 9, sem_legenda = FALSE, extra = NULL,
                      linhas = 4) {

  g <- grafico_km(fit, niveis, rotulo_grupo = titulo, ordenada = ordenada,
                  ic = ic, base = base, n_por_grupo = n_por_grupo,
                  legenda = legenda, anotacao = anotacao) +
    labs(x = "Tempo (anos)", y = "P(permanecer sem HAS)") +
    theme(legend.text = element_text(size = rel(0.95)),
          legend.key.height = unit(0.85, "lines"),
          plot.margin = margin(5, 8, 4, 14))

  if (sem_legenda) g <- g + theme(legend.position = "none")
  if (!is.null(extra)) g <- g + extra

  # todas as células reservam o mesmo espaço para a tabela, para que
  # as quatro curvas fiquem com a mesma altura
  tab <- painel_risco(em_risco(fit, QUEBRAS, niveis), niveis,
                      base = base * 0.9, rotular = TRUE, linhas = linhas)

  cowplot::plot_grid(g, tab, ncol = 1,
                     rel_heights = c(1, 0.10 + 0.05 * linhas),
                     align = "v", axis = "lr")
}

#--- A. coorte toda -----------------------------------------
KM <- survfit(Surv(tempo_anos, HAS_status_num) ~ 1, data = base_surv2)

med     <- summary(KM)$table
mediana <- unname(med["median"])
lcl     <- unname(med["0.95LCL"])
ucl     <- unname(med["0.95UCL"])

pA <- celula_km(
  KM, "Todos", "Coorte completa", ordenada = FALSE, ic = TRUE,
  sem_legenda = TRUE,
  anotacao = sprintf("n = %d; %d eventos\nMediana = %s anos\n(IC95%%: %s–%s)",
                     KM$n, sum(KM$n.event),
                     vg(mediana, 1), vg(lcl, 2), vg(ucl, 2)),
  extra = list(
    geom_hline(yintercept = 0.5, linetype = "22", linewidth = 0.35,
               colour = "grey45"),
    geom_segment(aes(x = mediana, xend = mediana, y = 0, yend = 0.5),
                 linetype = "22", linewidth = 0.35, colour = "grey45")
  )
)

#--- B. sexo -------------------------------------------------
KMsexo      <- survfit(Surv(tempo_anos, HAS_status_num) ~ sexo, data = base_surv2)
n_sexo      <- as.numeric(table(base_surv2$sexo))
p_lr_sexo   <- p_logrank("sexo", base_surv2)
p_peto_sexo <- p_peto("sexo",   base_surv2)

pB <- celula_km(
  KMsexo, levels(base_surv2$sexo), "Sexo", ordenada = FALSE, ic = TRUE,
  n_por_grupo = n_sexo,
  anotacao = paste0("Log-rank: ", fmt_p(p_lr_sexo),
                    nota_na("sexo", base_surv2))
)

#--- C. estado nutricional (IMC, quatro categorias) ----------
KMimc     <- survfit(Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
                     data = base_surv2)
niv_imc   <- levels(base_surv2$imc_onda1)
n_imc     <- as.numeric(table(base_surv2$imc_onda1))
p_lr_imc  <- p_logrank("imc_onda1", base_surv2)

pC <- celula_km(
  KMimc, niv_imc, "Estado nutricional (IMC)", ordenada = TRUE, ic = FALSE,
  n_por_grupo = n_imc,
  anotacao = paste0("Log-rank: ", fmt_p(p_lr_imc),
                    nota_na("imc_onda1", base_surv2))
)

#--- D. uso de álcool ----------------------------------------
KMalcool   <- survfit(Surv(tempo_anos, HAS_status_num) ~ usoalcool,
                      data = base_surv2)
niv_alc    <- levels(base_surv2$usoalcool)
n_alc      <- as.numeric(table(base_surv2$usoalcool))
med_alc    <- summary(KMalcool)$table[, "median"]
p_lr_alc   <- p_logrank("usoalcool", base_surv2)
p_peto_alc <- p_peto("usoalcool",    base_surv2)

pD <- celula_km(
  KMalcool, niv_alc, "Uso de álcool", ordenada = TRUE, ic = FALSE,
  n_por_grupo = n_alc,
  anotacao = paste0("Log-rank: ", fmt_p(p_lr_alc),
                    nota_na("usoalcool", base_surv2))
)

f2 <- cowplot::plot_grid(pA, pB, pC, pD, ncol = 2,
                         labels = LETTERS[1:4], label_size = 13,
                         label_fontfamily = FONTE)

# A4 retrato (210 x 297 mm) com margens de 2,5 cm -> 6,3 x 9,7 pol
salvar(f2, "Figura_2_km_paineis_principais", 6.3, 8.6)

cat("mediana KM global:", vg(mediana, 2), "(", vg(lcl, 2), "-", vg(ucl, 2), ")\n")
cat("sexo   -> log-rank", fmt_p(p_lr_sexo), "| Peto", fmt_p(p_peto_sexo), "\n")
cat("IMC    -> log-rank", fmt_p(p_lr_imc), "\n")
cat("álcool -> log-rank", fmt_p(p_lr_alc), "| Peto", fmt_p(p_peto_alc),
    "| medianas:", paste(vg(med_alc, 2), collapse = " / "), "\n")

#============================================================
# 4. FIGURA 3 - painel com seis variáveis (n = 391)
#    mesma seleção de variáveis da figura 2x3 do script original
#============================================================

vars_painel <- c("idade_cat", "racacor", "escolaridade",
                 "ativfisica", "tabagismo", "imc_onda1")

rot_painel <- c(idade_cat    = "Faixa etária",
                racacor      = "Raça/cor",
                escolaridade = "Escolaridade",
                ativfisica   = "Atividade física",
                tabagismo    = "Tabagismo",
                imc_onda1    = "Estado nutricional (IMC)")

paineis <- lapply(vars_painel, function(v) {

  fit <- survfit(as.formula(paste("Surv(tempo_anos, HAS_status_num) ~", v)),
                 data = base_surv2)

  niv <- levels(base_surv2[[v]])
  nn  <- as.numeric(table(base_surv2[[v]]))

  # rótulos longos (escolaridade) sairiam por cima das curvas:
  # nesses painéis a legenda entra em corpo menor, numa linha só
  comprimento <- max(nchar(sprintf("%s (n = %d)", niv, nn)))
  tam_legenda <- if (comprimento > 24) 0.78 else 0.95

  grafico_km(fit, niv, rotulo_grupo = rot_painel[[v]],
             ordenada = ORDENADA[[v]], ic = FALSE, base = 9.5,
             n_por_grupo = nn, legenda = c(0.01, 0.02),
             quebra_rotulo = 60,
             anotacao = paste0("Log-rank: ", fmt_p(p_logrank(v, base_surv2)),
                               nota_na(v, base_surv2))) +
    # os títulos dos eixos são únicos para a figura toda (ver abaixo)
    labs(x = NULL, y = NULL) +
    theme(legend.text = element_text(size = rel(tam_legenda)),
          legend.key.height = unit(0.85, "lines"),
          plot.margin = margin(5, 8, 4, 14))   # espaço para a letra do painel
})

grade6 <- cowplot::plot_grid(plotlist = paineis, ncol = 3,
                             labels = LETTERS[1:6], label_size = 13,
                             label_fontfamily = FONTE)

# um único título por eixo libera espaço e permite manter a fonte grande
f6 <- cowplot::ggdraw() +
  cowplot::draw_plot(grade6, x = 0.028, y = 0.042,
                     width = 0.972, height = 0.958) +
  cowplot::draw_label("Probabilidade de permanecer sem HAS",
                      x = 0.014, y = 0.52, angle = 90,
                      size = 12.5, fontfamily = FONTE) +
  cowplot::draw_label("Tempo de acompanhamento (anos)",
                      x = 0.51, y = 0.018,
                      size = 12.5, fontfamily = FONTE) +
  theme(plot.background = element_rect(fill = "white", colour = NA))

# A4 paisagem (297 x 210 mm) com margens de 2,5 cm -> 9,7 x 6,3 pol
salvar(f6, "Figura_3_km_paineis_seis_variaveis", 9.7, 6.3)

#============================================================
# 5. FIGURA S1 - risco acumulado (Nelson-Aalen), n = 391
#    Corresponde aos plot(..., fun = "cumhaz") do
#    "Script trabalho sobrevida.R" (sete variáveis; o script não
#    desenha risco acumulado por faixa etária).
#============================================================

vars_ch <- c("sexo", "racacor", "escolaridade", "imc_onda1",
             "ativfisica", "tabagismo", "usoalcool")

rot_ch <- c(rotulos, imc_onda1 = "Estado nutricional (IMC)")

paineis_ch <- lapply(vars_ch, function(v) {

  fit <- survfit(as.formula(paste("Surv(tempo_anos, HAS_status_num) ~", v)),
                 data = base_surv2)

  niv <- levels(base_surv2[[v]])
  k   <- length(niv)

  grupo <- rep(sub("^[^=]*=", "", names(fit$strata)), fit$strata)

  d <- rbind(
    data.frame(tempo = 0, ch = 0, grupo = niv, stringsAsFactors = FALSE),
    data.frame(tempo = fit$time, ch = fit$cumhaz, grupo = grupo,
               stringsAsFactors = FALSE)
  )
  d$grupo <- factor(d$grupo, levels = niv)

  comprimento <- max(nchar(niv))

  ggplot(d, aes(tempo, ch, colour = grupo, linetype = grupo)) +
    geom_step(direction = "hv", linewidth = 0.55) +
    scale_colour_manual(values = paleta(k, ORDENADA[[v]]), drop = FALSE) +
    scale_linetype_manual(values = TIPOS[seq_len(k)], drop = FALSE) +
    scale_x_continuous(limits = c(0, 13.2), breaks = QUEBRAS, expand = c(0, 0)) +
    scale_y_continuous(labels = function(z) vg(z, 1),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Tempo (anos)", y = "Risco acumulado de HAS",
         title = rot_ch[[v]]) +
    tema_pub(9.5) +
    theme(legend.position = "inside",
          legend.position.inside = c(0.02, 0.98),
          legend.justification = c(0, 1),
          legend.text = element_text(size = rel(if (comprimento > 20) 0.8 else 0.95)),
          legend.key.height = unit(0.85, "lines"),
          plot.margin = margin(5, 8, 4, 16))
})

fS1 <- cowplot::plot_grid(plotlist = paineis_ch, ncol = 4,
                          labels = LETTERS[seq_along(vars_ch)], label_size = 13,
                          label_fontfamily = FONTE)
salvar(fS1, "Figura_S1_risco_acumulado", 13, 7.0)

#============================================================
# 6. MODELO 5 E DIAGNÓSTICOS
#    Modelo 5: "Script cox - trab sobrevida.R"
#    Schoenfeld, partição do tempo, deviance e DFBETAS:
#    "Script Schoenfeld.R" - mesmas chamadas, mesma base (n = 375)
#============================================================

mod5 <- coxph(
  Surv(tempo_anos, HAS_status_num) ~
    sexo + idade_cat + racacor + escolaridade +
    imc_cat3 + ativfisica + tabagismo + usoalcool,
  data = base_cox
)

# cox.zph por TERMO: é o objeto que o "Script Schoenfeld.R" desenha
ph_mod5 <- cox.zph(mod5)

# a versão por coeficiente é impressa no script, mas não desenhada
ph_mod5_coef <- cox.zph(mod5, terms = FALSE)

cat("Schoenfeld GLOBAL:", fmt_p(ph_mod5$table["GLOBAL", "p"]), "\n")

#------------------------------------------------------------
# Partição do tempo em < 4 anos e >= 4 anos
# (código idêntico ao "Script Schoenfeld.R")
#------------------------------------------------------------

base_cox$status_antes4 <- ifelse(base_cox$tempo_anos <  4, base_cox$HAS_status_num, 0)
base_cox$status_apos4  <- ifelse(base_cox$tempo_anos >= 4, base_cox$HAS_status_num, 0)

y_antes4 <- Surv(base_cox$tempo_anos, base_cox$status_antes4)
y_apos4  <- Surv(base_cox$tempo_anos, base_cox$status_apos4)

mod5_antes4 <- coxph(
  y_antes4 ~ sexo + idade_cat + racacor + escolaridade +
    imc_cat3 + ativfisica + tabagismo + usoalcool,
  data = base_cox, x = TRUE
)

mod5_apos4 <- coxph(
  y_apos4 ~ sexo + idade_cat + racacor + escolaridade +
    imc_cat3 + ativfisica + tabagismo + usoalcool,
  data = base_cox, x = TRUE
)

zph_antes4 <- cox.zph(mod5_antes4)
zph_apos4  <- cox.zph(mod5_apos4)

cat("eventos < 4 anos:", sum(base_cox$status_antes4),
    "| eventos >= 4 anos:", sum(base_cox$status_apos4), "\n")
cat("Schoenfeld GLOBAL < 4 anos:", fmt_p(zph_antes4$table["GLOBAL", "p"]),
    "| >= 4 anos:", fmt_p(zph_apos4$table["GLOBAL", "p"]), "\n")

#------------------------------------------------------------
# 6.1 Resíduos de Schoenfeld - um painel por termo
#     A curva suavizada e a banda de +-2 EP vêm do próprio
#     survival:::plot.cox.zph (plot = FALSE); nada é reajustado.
#------------------------------------------------------------

paineis_zph <- function(zph, base = 9.5) {

  # marcas do eixo do tempo, como plot.cox.zph faz na transformação "km"
  xx   <- zph$x
  indx <- !duplicated(xx)
  apr1 <- approx(xx[indx], zph$time[indx],
                 seq(min(xx), max(xx), length = 17)[2 * (1:8)])
  num  <- signif(apr1$y, 2)
  val  <- approx(zph$time[indx], xx[indx], num)$y
  lab  <- sub("\\.", ",", format(num))

  termos <- colnames(zph$y)

  lapply(seq_along(termos), function(i) {

    aj  <- plot(zph[i], plot = FALSE)          # x, yhat, +-2 EP
    cur <- data.frame(x = aj$x, y = aj$y[, 1],
                      lo = aj$y[, 3], hi = aj$y[, 2])
    res <- data.frame(x = zph$x, y = zph$y[, i])

    ggplot() +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey75") +
      geom_point(data = res, aes(x, y), shape = 16, size = 0.5,
                 colour = "grey55", alpha = 0.6) +
      geom_ribbon(data = cur, aes(x, ymin = lo, ymax = hi),
                  fill = "#0072B2", alpha = 0.15) +
      geom_line(data = cur, aes(x, y), colour = "#0072B2", linewidth = 0.6) +
      scale_x_continuous(breaks = val, labels = lab) +
      scale_y_continuous(labels = function(z) vg(z, 1)) +
      labs(x = "Tempo (anos)", y = expression(beta(t)),
           title = rotulos[[termos[i]]],
           subtitle = fmt_p(zph$table[i, "p"])) +
      tema_pub(base) +
      theme(plot.margin = margin(5, 8, 4, 16))
  })
}

fS2 <- cowplot::plot_grid(plotlist = paineis_zph(ph_mod5), ncol = 4,
                          labels = LETTERS[1:8], label_size = 13,
                          label_fontfamily = FONTE)
salvar(fS2, "Figura_S2_schoenfeld_modelo5", 13, 7.0)

fS3 <- cowplot::plot_grid(plotlist = paineis_zph(zph_antes4), ncol = 4,
                          labels = LETTERS[1:8], label_size = 13,
                          label_fontfamily = FONTE)
salvar(fS3, "Figura_S3_schoenfeld_antes_4anos", 13, 7.0)

fS4 <- cowplot::plot_grid(plotlist = paineis_zph(zph_apos4), ncol = 4,
                          labels = LETTERS[1:8], label_size = 13,
                          label_fontfamily = FONTE)
salvar(fS4, "Figura_S4_schoenfeld_apos_4anos", 13, 7.0)

#------------------------------------------------------------
# 6.2 Resíduos deviance dos dois períodos
#     Mesmo critério exploratório do script: |resíduo| > 2
#------------------------------------------------------------

painel_deviance <- function(res, titulo, corte = 2, base = 9.5) {

  d    <- data.frame(indice = seq_along(res), r = as.numeric(res))
  fora <- d[abs(d$r) > corte, ]

  ggplot(d, aes(indice, r)) +
    geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey75") +
    geom_hline(yintercept = c(-corte, corte), linetype = "22",
               linewidth = 0.35, colour = "#D55E00") +
    geom_point(shape = 16, size = 0.9, colour = "#0072B2", alpha = 0.65) +
    geom_point(data = fora, shape = 21, size = 1.9, stroke = 0.5,
               colour = "#D55E00", fill = NA) +
    geom_text(data = fora, aes(label = indice), vjust = -0.9,
              size = base / .pt * 0.75, family = FONTE, colour = CINZA) +
    scale_y_continuous(labels = function(z) vg(z, 1)) +
    labs(x = "Índice da observação", y = "Resíduo deviance",
         title = titulo,
         subtitle = sprintf("%d observações com |resíduo| > %d; amplitude %s a %s",
                            nrow(fora), corte, vg(min(d$r), 2), vg(max(d$r), 2))) +
    tema_pub(base) +
    theme(plot.margin = margin(5, 8, 4, 16))
}

res_dev_antes4 <- resid(mod5_antes4, type = "deviance")
res_dev_apos4  <- resid(mod5_apos4,  type = "deviance")

fS5 <- cowplot::plot_grid(
  painel_deviance(res_dev_antes4, "Período < 4 anos"),
  painel_deviance(res_dev_apos4,  "Período ≥ 4 anos"),
  ncol = 2, labels = c("A", "B"), label_size = 13, label_fontfamily = FONTE)

salvar(fS5, "Figura_S5_residuos_deviance", 9.5, 4.2)

cat("deviance < 4 anos: |r| > 2 em", sum(abs(res_dev_antes4) > 2), "observações\n")
cat("deviance >= 4 anos: |r| > 2 em", sum(abs(res_dev_apos4) > 2), "observações\n")

#------------------------------------------------------------
# 6.3 DFBETAS por variável
#     resid(modelo, type = "dfbetas"), como no script.
#     O script desenha dois painéis 2x2 por modelo
#     (sociodemográficas e comportamentais); aqui os oito
#     painéis entram numa figura só, por modelo.
#
#     OBS: no "Script Schoenfeld.R" o bloco do período < 4 anos
#     usa res_esco_antes4 sem tê-lo criado - só res_esco_apos4
#     é definido. Aqui o objeto do período < 4 anos é criado da
#     mesma forma que o do período >= 4 anos.
#------------------------------------------------------------

paineis_dfbetas <- function(modelo, base = 9.5) {

  dfb <- resid(modelo, type = "dfbetas")
  colnames(dfb) <- names(coef(modelo))

  lapply(covars, function(v) {

    if (v == "sexo") {
      # como no script: DFBETAS do coeficiente de sexo, por categoria
      d <- data.frame(cat = base_cox$sexo, valor = dfb[, "sexoFeminino"])
    } else {
      cols <- grep(paste0("^", v), colnames(dfb), value = TRUE)
      d <- do.call(rbind, lapply(cols, function(cc)
        data.frame(cat = sub(paste0("^", v), "", cc), valor = dfb[, cc])))
      d$cat <- factor(d$cat, levels = sub(paste0("^", v), "", cols))
    }

    ggplot(d, aes(cat, valor)) +
      geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey75") +
      geom_boxplot(width = 0.55, linewidth = 0.35, outlier.size = 0.5,
                   outlier.colour = "grey45", fill = "#CFE0EA",
                   colour = "#2F5D75") +
      scale_y_continuous(labels = function(z) vg(z, 1)) +
      scale_x_discrete(labels = function(z) vapply(z, function(w)
        paste(strwrap(w, width = 12), collapse = "\n"), character(1))) +
      labs(x = NULL, y = "DFBETAS", title = rotulos[[v]]) +
      tema_pub(base) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size = rel(0.85)),
            plot.margin = margin(5, 8, 4, 16))
  })
}

fS6 <- cowplot::plot_grid(plotlist = paineis_dfbetas(mod5_antes4), ncol = 4,
                          labels = LETTERS[1:8], label_size = 13,
                          label_fontfamily = FONTE)
salvar(fS6, "Figura_S6_dfbetas_antes_4anos", 13, 7.0)

fS7 <- cowplot::plot_grid(plotlist = paineis_dfbetas(mod5_apos4), ncol = 4,
                          labels = LETTERS[1:8], label_size = 13,
                          label_fontfamily = FONTE)
salvar(fS7, "Figura_S7_dfbetas_apos_4anos", 13, 7.0)

#============================================================
# 6. LEGENDAS SUGERIDAS
#    Os números vêm dos objetos já calculados acima.
#============================================================

legendas <- c(
  "# Legendas das figuras",
  "",
  "Valores gerados por `07_figuras_publicacao.R`. Nenhuma estimativa foi",
  "recalculada de forma diferente dos scripts originais.",
  "",
  sprintf("**Figura 1.** Distribuição do tempo de seguimento: (A) até o desenvolvimento de hipertensão arterial sistêmica (HAS), entre os participantes que apresentaram o desfecho (n = %d); (B) até a censura, entre os que não desenvolveram HAS até o fim do seguimento (n = %d). Intervalos de classe definidos por `hist(..., breaks = 12)`.",
          sum(base_surv2$HAS_status_num == 1),
          sum(base_surv2$HAS_status_num == 0)),
  "",
  sprintf("**Figura 2.** Curvas de Kaplan-Meier do tempo até o desenvolvimento de HAS (n = %d; %d eventos): (A) coorte completa, (B) segundo sexo, (C) segundo estado nutricional pelo IMC e (D) segundo uso de álcool. Em A, a área sombreada é o intervalo de confiança de 95%%, a linha tracejada marca a sobrevida de 50%% e a mediana foi de %s anos (IC95%%: %s-%s); em B as áreas sombreadas também são IC95%%. Marcas verticais indicam censuras. Valores de p do teste de log-rank: sexo %s; IMC %s; uso de álcool %s. Os testes de Peto deram %s para sexo e %s para uso de álcool. Medianas por uso de álcool: %s. A categoria Magreza do IMC tem um único participante, que apresentou o evento. Em D, %d participante não tinha informação sobre uso de álcool e ficou de fora da curva e do teste. Abaixo de cada painel, o número de participantes em risco.",
          nrow(base_surv2), sum(KM$n.event),
          vg(mediana, 1), vg(lcl, 2), vg(ucl, 2),
          fmt_p(p_lr_sexo), fmt_p(p_lr_imc), fmt_p(p_lr_alc),
          fmt_p(p_peto_sexo), fmt_p(p_peto_alc),
          paste(sprintf("%s = %s anos", niv_alc, vg(med_alc, 2)), collapse = "; "),
          sum(is.na(base_surv2$usoalcool))),
  "",
  sprintf("**Figura 3.** Curvas de Kaplan-Meier do tempo até HAS segundo (A) faixa etária, (B) raça/cor, (C) escolaridade, (D) atividade física, (E) tabagismo e (F) estado nutricional segundo o IMC em quatro categorias. Valores de p do teste de log-rank. Os painéis B e D não somam os %d participantes da coorte: %d não tinham informação de raça/cor e %d não tinham informação de atividade física; essas observações ficam de fora tanto das curvas quanto do teste, e o n de cada categoria está na legenda de cada painel. No painel B, as categorias amarela (n = %d) e indígena (n = %d) têm pouquíssimos participantes — cada degrau corresponde a um ou dois eventos e o traçado dessas duas curvas não deve ser interpretado; nos modelos ajustados da coorte incidente (scripts 04 e 06) raça/cor é reagrupada em branca, negra (preta + parda) e outras (amarela + indígena), enquanto o Modelo 5 aqui apresentado mantém as cinco categorias. No painel F, a categoria Magreza tem um único participante, que apresentou o evento.",
          nrow(base_surv2),
          sum(is.na(base_surv2$racacor)),
          sum(is.na(base_surv2$ativfisica)),
          sum(base_surv2$racacor == "Amarela",  na.rm = TRUE),
          sum(base_surv2$racacor == "Indígena", na.rm = TRUE)),
  "",
  "## Material suplementar",
  "",
  sprintf("**Figura S1.** Risco acumulado de HAS (estimador de Nelson-Aalen) segundo (A) sexo, (B) raça/cor, (C) escolaridade, (D) estado nutricional pelo IMC, (E) atividade física, (F) tabagismo e (G) uso de álcool (n = %d). Corresponde às curvas `fun = \"cumhaz\"` do script de Kaplan-Meier.",
          nrow(base_surv2)),
  "",
  sprintf("**Figura S2.** Resíduos de Schoenfeld em função do tempo, um painel por variável do Modelo 5 (n = %d; %d eventos). Linha azul: ajuste suavizado com banda de +-2 erros-padrão; pontos cinza: resíduos individuais; linha horizontal: zero. Sob riscos proporcionais a curva deve ser aproximadamente horizontal. Valores de p do teste de Schoenfeld por variável; teste global %s.",
          nrow(base_cox), mod5$nevent, fmt_p(ph_mod5$table["GLOBAL", "p"])),
  "",
  sprintf("**Figura S3.** Resíduos de Schoenfeld do modelo ajustado apenas aos eventos ocorridos antes de 4 anos de seguimento (%d eventos). Teste global %s.",
          sum(base_cox$status_antes4), fmt_p(zph_antes4$table["GLOBAL", "p"])),
  "",
  sprintf("**Figura S4.** Resíduos de Schoenfeld do modelo ajustado apenas aos eventos ocorridos a partir de 4 anos de seguimento (%d eventos). Teste global %s.",
          sum(base_cox$status_apos4), fmt_p(zph_apos4$table["GLOBAL", "p"])),
  "",
  sprintf("**Figura S5.** Resíduos deviance por observação nos modelos (A) do período < 4 anos e (B) do período >= 4 anos. Linhas tracejadas em -2 e +2, o critério exploratório usado; as observações que ultrapassam esse limite estão circuladas e identificadas pelo índice na base de casos completos. Resíduo positivo indica evento mais precoce que o predito. Ultrapassam o limite %d observações em A e %d em B.",
          sum(abs(res_dev_antes4) > 2), sum(abs(res_dev_apos4) > 2)),
  "",
  "**Figura S6.** DFBETAS do modelo do período < 4 anos, por variável: (A) sexo, (B) faixa etária, (C) raça/cor, (D) escolaridade, (E) estado nutricional pelo IMC, (F) atividade física, (G) tabagismo e (H) uso de álcool. Cada caixa reúne os DFBETAS padronizados de um coeficiente; valores afastados de zero indicam observações que puxam aquele coeficiente. No painel A, os DFBETAS do coeficiente de sexo feminino estão separados pelas categorias de sexo, como no script de origem.",
  "",
  "**Figura S7.** DFBETAS do modelo do período >= 4 anos, com a mesma organização da Figura S6.",
  "",
  "---",
  "",
  "Arquivos: PNG em 300 dpi para submissão e PDF vetorial para edição.",
  "Paleta segura para daltonismo; linhas também diferenciadas por traço,",
  "de modo que as figuras continuam legíveis em preto e branco."
)

writeLines(legendas, file.path(DIR, "LEGENDAS.md"))

cat("\nTodas as figuras foram gravadas em ", DIR, "/\n", sep = "")
