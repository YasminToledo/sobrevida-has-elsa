#============================================================
# PRESSUPOSTO DE RISCOS PROPORCIONAIS - MODELO 5
#============================================================

library(survival)

# Teste de Schoenfeld
ph_mod5 <- cox.zph(mod5)

# Resultado do teste
ph_mod5

# Gráficos dos resíduos de Schoenfeld
plot(ph_mod5)

#Verificando todas as categorias  
ph_mod5_coef <- cox.zph(mod5, terms = FALSE)
ph_mod5_coef

#Particionando o tempo da categoria atividade física em tempo de acompanhamento menor e maior que 4 anos

# Eventos antes de 4 anos
base_cox$status_antes4 <- ifelse(
  base_cox$tempo_anos < 4,
  base_cox$HAS_status_num,
  0
)

# Eventos a partir de 4 anos
base_cox$status_apos4 <- ifelse(
  base_cox$tempo_anos >= 4,
  base_cox$HAS_status_num,
  0
)

# Objetos de sobrevivência
y_antes4 <- Surv(
  base_cox$tempo_anos,
  base_cox$status_antes4
)

y_apos4 <- Surv(
  base_cox$tempo_anos,
  base_cox$status_apos4
)

# Conferir número de eventos
table(base_cox$status_antes4)
table(base_cox$status_apos4)

sum(base_cox$status_antes4)
sum(base_cox$status_apos4)

# Modelo 5 - antes de 4 anos
mod5_antes4 <- coxph(
  y_antes4 ~ sexo + idade_cat + racacor + escolaridade +
    imc_cat3 + ativfisica + tabagismo + usoalcool,
  data = base_cox,
  x = TRUE
)

summary(mod5_antes4)


# Modelo 5 - a partir de 4 anos
mod5_apos4 <- coxph(
  y_apos4 ~ sexo + idade_cat + racacor + escolaridade +
    imc_cat3 + ativfisica + tabagismo + usoalcool,
  data = base_cox,
  x = TRUE
)

summary(mod5_apos4)

# TABELA CONJUNTA DOS MODELOS <4 ANOS E >=4 ANOS
#Converter modelos para tabelas do excel
install.packages("broom")
install.packages("writexl")

library(broom)
library(writexl)


# Extrair resultados
tab_antes4 <- broom::tidy(
  mod5_antes4,
  exponentiate = TRUE,
  conf.int = TRUE
)

tab_apos4 <- broom::tidy(
  mod5_apos4,
  exponentiate = TRUE,
  conf.int = TRUE
)

# Criar coluna HR (IC95%)
tab_antes4$HR_IC95 <- paste0(
  sprintf("%.2f", tab_antes4$estimate),
  " (",
  sprintf("%.2f", tab_antes4$conf.low),
  "-",
  sprintf("%.2f", tab_antes4$conf.high),
  ")"
)

tab_apos4$HR_IC95 <- paste0(
  sprintf("%.2f", tab_apos4$estimate),
  " (",
  sprintf("%.2f", tab_apos4$conf.low),
  "-",
  sprintf("%.2f", tab_apos4$conf.high),
  ")"
)

# Manter apenas as colunas necessárias
tab_antes4 <- tab_antes4[, c("term", "HR_IC95", "p.value")]
tab_apos4  <- tab_apos4[, c("term", "HR_IC95", "p.value")]

# Renomear
names(tab_antes4) <- c(
  "Variavel",
  "HR_IC95_antes4",
  "p_antes4"
)

names(tab_apos4) <- c(
  "Variavel",
  "HR_IC95_apos4",
  "p_apos4"
)

# Juntar os dois modelos
tabela_conjunta <- merge(
  tab_antes4,
  tab_apos4,
  by = "Variavel",
  all = TRUE
)

# Arredondar p
tabela_conjunta$p_antes4 <- sprintf("%.3f", tabela_conjunta$p_antes4)
tabela_conjunta$p_apos4  <- sprintf("%.3f", tabela_conjunta$p_apos4)

# Visualizar
tabela_conjunta

writexl::write_xlsx(
  tabela_conjunta,
  "tabela_cox_4anos_formatada.xlsx"
)

#-----------------SCHOENFELD--------------------
# Schoenfeld - antes de 4 anos
zph_antes4 <- cox.zph(mod5_antes4)
zph_antes4

# Schoenfeld - a partir de 4 anos
zph_apos4 <- cox.zph(mod5_apos4)
zph_apos4

# SCHOENFELD - GRÁFICOS < 4 ANOS -----------------------------

# Refazer o teste, se necessário
zph_antes4 <- cox.zph(mod5_antes4)

# Organizar 8 gráficos em uma única figura
par(
  mfrow = c(3, 3),
  mar = c(4, 4, 3, 1)
)

# Sexo
plot(zph_antes4[1],
     main = "Sexo",
     xlab = "Tempo (anos)")

# Faixa etária
plot(zph_antes4[2],
     main = "Faixa etária",
     xlab = "Tempo (anos)")

# Raça/cor
plot(zph_antes4[3],
     main = "Raça/cor",
     xlab = "Tempo (anos)")

# Escolaridade
plot(zph_antes4[4],
     main = "Escolaridade",
     xlab = "Tempo (anos)")

# IMC
plot(zph_antes4[5],
     main = "IMC",
     xlab = "Tempo (anos)")

# Atividade física
plot(zph_antes4[6],
     main = "Atividade física",
     xlab = "Tempo (anos)")

# Tabagismo
plot(zph_antes4[7],
     main = "Tabagismo",
     xlab = "Tempo (anos)")

# Uso de álcool
plot(zph_antes4[8],
     main = "Uso de álcool",
     xlab = "Tempo (anos)")

# Voltar ao padrão
par(mfrow = c(1, 1))

# SCHOENFELD - GRÁFICOS >= 4 ANOS

# Refazer o teste, se necessário
zph_apos4 <- cox.zph(mod5_apos4)

par(
  mfrow = c(3, 3),
  mar = c(4, 4, 3, 1)
)

plot(zph_apos4[1],
     main = "Sexo",
     xlab = "Tempo (anos)")

plot(zph_apos4[2],
     main = "Faixa etária",
     xlab = "Tempo (anos)")

plot(zph_apos4[3],
     main = "Raça/cor",
     xlab = "Tempo (anos)")

plot(zph_apos4[4],
     main = "Escolaridade",
     xlab = "Tempo (anos)")

plot(zph_apos4[5],
     main = "IMC",
     xlab = "Tempo (anos)")

plot(zph_apos4[6],
     main = "Atividade física",
     xlab = "Tempo (anos)")

plot(zph_apos4[7],
     main = "Tabagismo",
     xlab = "Tempo (anos)")

plot(zph_apos4[8],
     main = "Uso de álcool",
     xlab = "Tempo (anos)")

par(mfrow = c(1, 1))

#------------------------------------------------
# RESÍDUOS DEVIANCE - MODELO < 4 ANOS

res_dev_antes4 <- resid(
  mod5_antes4,
  type = "deviance"
)

# Visualizar os resíduos
res_dev_antes4

# Gráfico
par(mfrow = c(1, 1))

plot(
  res_dev_antes4,
  xlab = "Índice do indivíduo",
  ylab = "Resíduo deviance",
  main = "Resíduos deviance - período < 4 anos",
  pch = 19
)

abline(h = 0, lty = 3)

#descobrir quais indivíduos têm resíduos deviance
which(abs(res_dev_antes4) > 2)

#ver os valores dos resíduos
res_dev_antes4[abs(res_dev_antes4) > 2]

#marcar os resíduos no gráfico
ind_antes4 <- which(abs(res_dev_antes4) > 2)

plot(
  res_dev_antes4,
  xlab = "Índice do indivíduo",
  ylab = "Resíduo deviance",
  main = "Resíduos deviance - período < 4 anos",
  pch = 19
)

abline(h = 0, lty = 3)
abline(h = c(-2, 2), lty = 2)

text(
  ind_antes4,
  res_dev_antes4[ind_antes4],
  labels = names(res_dev_antes4[ind_antes4]),
  pos = 3,
  cex = 0.7
)

ind_antes4 <- which(abs(res_dev_antes4) > 2)

base_cox[ind_antes4, ]

# Vendo os dados dos indivíduos com resíduos deviance > 2
dados_aberrantes_antes4 <- base_cox[
  ind_antes4,
  c(
    "tempo_anos",
    "HAS_status_num",
    "sexo",
    "idade_cat",
    "racacor",
    "escolaridade",
    "imc_cat3",
    "ativfisica",
    "tabagismo",
    "usoalcool"
  )
]

dados_aberrantes_antes4$residuo_deviance <-
  res_dev_antes4[ind_antes4]

dados_aberrantes_antes4

print(dados_aberrantes_antes4, n = 22, width = Inf)
#------------------------------------------------
# RESÍDUOS DEVIANCE - MODELO >= 4 ANOS

# Calcular os resíduos deviance
res_dev_apos4 <- resid(
  mod5_apos4,
  type = "deviance"
)

# Visualizar os resíduos
res_dev_apos4

# Gráfico inicial
par(mfrow = c(1, 1))

plot(
  res_dev_apos4,
  xlab = "Índice do indivíduo",
  ylab = "Resíduo deviance",
  main = "Resíduos deviance - período ≥ 4 anos",
  pch = 19
)

abline(h = 0, lty = 3)


#------------------------------------------------
# Identificar possíveis pontos aberrantes
# Critério exploratório: |resíduo deviance| > 2

# Descobrir quais indivíduos têm resíduos deviance > 2 em valor absoluto
which(abs(res_dev_apos4) > 2)

# Ver os valores dos resíduos
res_dev_apos4[abs(res_dev_apos4) > 2]


#------------------------------------------------
# Marcar os resíduos extremos no gráfico
#------------------------------------------------

ind_apos4 <- which(abs(res_dev_apos4) > 2)

plot(
  res_dev_apos4,
  xlab = "Índice do indivíduo",
  ylab = "Resíduo deviance",
  main = "Resíduos deviance - período ≥ 4 anos",
  pch = 19
)

abline(h = 0, lty = 3)
abline(h = c(-2, 2), lty = 2)

text(
  ind_apos4,
  res_dev_apos4[ind_apos4],
  labels = names(res_dev_apos4[ind_apos4]),
  pos = 3,
  cex = 0.7
)


#------------------------------------------------
# Visualizar os indivíduos sinalizados
#------------------------------------------------

base_cox[ind_apos4, ]


#------------------------------------------------
# Ver as principais características dos indivíduos
# com resíduos deviance elevados
#------------------------------------------------

dados_aberrantes_apos4 <- base_cox[
  ind_apos4,
  c(
    "tempo_anos",
    "HAS_status_num",
    "sexo",
    "idade_cat",
    "racacor",
    "escolaridade",
    "imc_cat3",
    "ativfisica",
    "tabagismo",
    "usoalcool"
  )
]

# Acrescentar o valor do resíduo deviance
dados_aberrantes_apos4$residuo_deviance <-
  res_dev_apos4[ind_apos4]

# Visualizar
dados_aberrantes_apos4

# Mostrar todas as linhas e colunas
print(
  dados_aberrantes_apos4,
  n = length(ind_apos4),
  width = Inf
)

#------------------------------------------------
# PONTOS INFLUENTES - MODELO < 4 ANOS
# DFBETAS 

par(
  mfrow = c(2, 2),
  mar = c(5, 5, 3, 1)
)

# Sexo
boxplot(
  res_esco_antes4[, "sexoFeminino"] ~ base_cox$sexo,
  xlab = "Sexo",
  ylab = "DFBETAS",
  main = "Sexo"
)
abline(h = 0, lty = 3)

# Faixa etária
boxplot(
  res_esco_antes4[, c(
    "idade_cat45 a 54 anos",
    "idade_cat55 a 64 anos",
    "idade_cat65 a 74 anos"
  )],
  names = c("45–54", "55–64", "65–74"),
  xlab = "Faixa etária",
  ylab = "DFBETAS",
  main = "Faixa etária"
)
abline(h = 0, lty = 3)

# Raça/cor
boxplot(
  res_esco_antes4[, c(
    "racacorParda",
    "racacorBranca",
    "racacorAmarela",
    "racacorIndígena"
  )],
  names = c("Parda", "Branca", "Amarela", "Indígena"),
  xlab = "Raça/cor",
  ylab = "DFBETAS",
  main = "Raça/cor"
)
abline(h = 0, lty = 3)

# Escolaridade
boxplot(
  res_esco_antes4[, c(
    "escolaridadeFundamental completo",
    "escolaridadeMédio completo",
    "escolaridadeSuperior completo"
  )],
  names = c("Fund.", "Médio", "Superior"),
  xlab = "Escolaridade",
  ylab = "DFBETAS",
  main = "Escolaridade"
)
abline(h = 0, lty = 3)

par(mfrow = c(1, 1))

# DFBETAS - MODELO < 4 ANOS
# FIGURA 2: variáveis antropométricas e comportamentais

par(
  mfrow = c(2, 2),
  mar = c(5, 5, 3, 1)
)

# IMC
boxplot(
  res_esco_antes4[, c(
    "imc_cat3Sobrepeso",
    "imc_cat3Obesidade"
  )],
  names = c("Sobrepeso", "Obesidade"),
  xlab = "IMC",
  ylab = "DFBETAS",
  main = "IMC"
)
abline(h = 0, lty = 3)

# Atividade física
boxplot(
  res_esco_antes4[, c(
    "ativfisicaModerada",
    "ativfisicaForte"
  )],
  names = c("Moderada", "Forte"),
  xlab = "Atividade física",
  ylab = "DFBETAS",
  main = "Atividade física"
)
abline(h = 0, lty = 3)

# Tabagismo
boxplot(
  res_esco_antes4[, c(
    "tabagismoEx-fumante",
    "tabagismoFumante"
  )],
  names = c("Ex-fumante", "Fumante"),
  xlab = "Tabagismo",
  ylab = "DFBETAS",
  main = "Tabagismo"
)
abline(h = 0, lty = 3)

# Uso de álcool
boxplot(
  res_esco_antes4[, c(
    "usoalcoolEx-usuário",
    "usoalcoolUsuário"
  )],
  names = c("Ex-usuário", "Usuário"),
  xlab = "Uso de álcool",
  ylab = "DFBETAS",
  main = "Uso de álcool"
)
abline(h = 0, lty = 3)

par(mfrow = c(1, 1))

#------------------------------------------------
# PONTOS INFLUENTES - MODELO >= 4 ANOS
# DFBETAS

res_esco_apos4 <- resid(
  mod5_apos4,
  type = "dfbetas"
)

# Nomear as colunas
colnames(res_esco_apos4) <- names(coef(mod5_apos4))


#------------------------------------------------
# FIGURA 1: variáveis sociodemográficas
#------------------------------------------------

par(
  mfrow = c(2, 2),
  mar = c(5, 5, 3, 1)
)

# Sexo
boxplot(
  res_esco_apos4[, "sexoFeminino"] ~ base_cox$sexo,
  xlab = "Sexo",
  ylab = "DFBETAS",
  main = "Sexo"
)
abline(h = 0, lty = 3)

# Faixa etária
boxplot(
  res_esco_apos4[, c(
    "idade_cat45 a 54 anos",
    "idade_cat55 a 64 anos",
    "idade_cat65 a 74 anos"
  )],
  names = c("45–54", "55–64", "65–74"),
  xlab = "Faixa etária",
  ylab = "DFBETAS",
  main = "Faixa etária"
)
abline(h = 0, lty = 3)

# Raça/cor
boxplot(
  res_esco_apos4[, c(
    "racacorParda",
    "racacorBranca",
    "racacorAmarela",
    "racacorIndígena"
  )],
  names = c("Parda", "Branca", "Amarela", "Indígena"),
  xlab = "Raça/cor",
  ylab = "DFBETAS",
  main = "Raça/cor"
)
abline(h = 0, lty = 3)

# Escolaridade
boxplot(
  res_esco_apos4[, c(
    "escolaridadeFundamental completo",
    "escolaridadeMédio completo",
    "escolaridadeSuperior completo"
  )],
  names = c("Fund.", "Médio", "Superior"),
  xlab = "Escolaridade",
  ylab = "DFBETAS",
  main = "Escolaridade"
)
abline(h = 0, lty = 3)

par(mfrow = c(1, 1))


#------------------------------------------------
# FIGURA 2: variáveis antropométricas e comportamentais
#------------------------------------------------

par(
  mfrow = c(2, 2),
  mar = c(5, 5, 3, 1)
)

# IMC
boxplot(
  res_esco_apos4[, c(
    "imc_cat3Sobrepeso",
    "imc_cat3Obesidade"
  )],
  names = c("Sobrepeso", "Obesidade"),
  xlab = "IMC",
  ylab = "DFBETAS",
  main = "IMC"
)
abline(h = 0, lty = 3)

# Atividade física
boxplot(
  res_esco_apos4[, c(
    "ativfisicaModerada",
    "ativfisicaForte"
  )],
  names = c("Moderada", "Forte"),
  xlab = "Atividade física",
  ylab = "DFBETAS",
  main = "Atividade física"
)
abline(h = 0, lty = 3)

# Tabagismo
boxplot(
  res_esco_apos4[, c(
    "tabagismoEx-fumante",
    "tabagismoFumante"
  )],
  names = c("Ex-fumante", "Fumante"),
  xlab = "Tabagismo",
  ylab = "DFBETAS",
  main = "Tabagismo"
)
abline(h = 0, lty = 3)

# Uso de álcool
boxplot(
  res_esco_apos4[, c(
    "usoalcoolEx-usuário",
    "usoalcoolUsuário"
  )],
  names = c("Ex-usuário", "Usuário"),
  xlab = "Uso de álcool",
  ylab = "DFBETAS",
  main = "Uso de álcool"
)
abline(h = 0, lty = 3)

# Voltar ao padrão
par(mfrow = c(1, 1))

