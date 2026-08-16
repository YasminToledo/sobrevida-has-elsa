#============================================================
# REAGRUPAMENTO DO IMC
# Magreza + Eutrofia = Sem excesso de peso
#============================================================

base_surv2$imc_cat3 <- factor(
  ifelse(
    base_surv2$imc_onda1 %in% c("Magreza", "Eutrofia"),
    "Sem excesso de peso",
    as.character(base_surv2$imc_onda1)
  ),
  levels = c(
    "Sem excesso de peso",
    "Sobrepeso",
    "Obesidade"
  )
)

# Conferir
table(base_surv2$imc_cat3, useNA = "ifany")


# Recriar a base comum dos modelos com a nova variável de IMC

vars_cox <- c(
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

base_cox <- base_surv2[
  complete.cases(base_surv2[, vars_cox]),
]

# Conferir n, eventos e censuras
nrow(base_cox)
sum(base_cox$HAS_status_num == 1)
sum(base_cox$HAS_status_num == 0)

# Conferir distribuição do novo IMC na base do Cox
table(base_cox$imc_cat3)

# Conferir eventos e censuras por IMC
table(base_cox$imc_cat3, base_cox$HAS_status_num)


#Salvar a base nova no computador
saveRDS(base_cox, file = "base_cox.rds")

#============================================================
# MODELOS DE COX SIMPLES 
#============================================================
# Cox simples - Sexo
cox_sexo <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ sexo,
  data = base_cox
)

summary(cox_sexo)

# Cox simples - Faixa etária
cox_idade <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ idade_cat,
  data = base_cox
)

summary(cox_idade)

# Cox simples - Raça/cor
cox_raca <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ racacor,
  data = base_cox
)

summary(cox_raca)

# Cox simples - Escolaridade
cox_escolaridade <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ escolaridade,
  data = base_cox
)

summary(cox_escolaridade)

# Cox simples - IMC
cox_imc <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ imc_cat3,
  data = base_cox
)

summary(cox_imc)

# Cox simples - Atividade física
cox_ativfisica <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ ativfisica,
  data = base_cox
)

summary(cox_ativfisica)

# Cox simples - Tabagismo
cox_tabagismo <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ tabagismo,
  data = base_cox
)

summary(cox_tabagismo)

# Cox simples - Uso de álcool
cox_alcool <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ usoalcool,
  data = base_cox
)

summary(cox_alcool)

#============================================================
# MODELOS DE COX ANINHADOS - mesmo n (n = 375)
#============================================================

library(survival)

# Modelo 1 - Sexo
mod1 <- coxph(
  Surv(tempo_anos, HAS_status_num) ~ sexo,
  data = base_cox
)

summary(mod1)

# Modelo 2 - Sexo + faixa etária
mod2 <- coxph(
  Surv(tempo_anos, HAS_status_num) ~
    sexo + idade_cat,
  data = base_cox
)

summary(mod2)

# Modelo 3 - Modelo 2 + raça/cor + escolaridade
mod3 <- coxph(
  Surv(tempo_anos, HAS_status_num) ~
    sexo + idade_cat +
    racacor + escolaridade,
  data = base_cox
)

summary(mod3)

# Modelo 4 - Modelo 3 + IMC
mod4 <- coxph(
  Surv(tempo_anos, HAS_status_num) ~
    sexo + idade_cat +
    racacor + escolaridade +
    imc_cat3,
  data = base_cox
)

summary(mod4)

# Modelo 5 - Modelo 4 + atividade física + tabagismo + uso de álcool

mod5 <- coxph(
  Surv(tempo_anos, HAS_status_num) ~
    sexo +
    idade_cat +
    racacor +
    escolaridade +
    imc_cat3 +
    ativfisica +
    tabagismo +
    usoalcool,
  data = base_cox
)

summary(mod5)

#============================================================
# COMPARAÇÃO DOS MODELOS ANINHADOS
# Teste da razão de verossimilhança
#============================================================

anova(
  mod1,
  mod2,
  mod3,
  mod4,
  mod5,
  test = "Chisq"
)


