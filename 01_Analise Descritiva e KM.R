setwd("C:/Users/apoio/Downloads/F")
library(readxl)
dados <- read_excel("dados.xlsx")
View(dados)

library(dplyr)

# Junta mantendo todas as linhas de basehas2023
base_final <- left_join(basehas2023, dados, by = "idelsa")

table(base_final$hmpa04)

#tirando quem não tinha diabetes na onda 1
base_final <- base_final %>%
  filter(hmpa04 != 0)

#tirando quem teve diabetes na gravidez

base_final <- base_final %>%
  filter(hmpa04 == 2)

# excluir todos que têm hipertensão na onda 1
base_final <- base_final %>%
  filter(hmpa02 == 0)

# n inicial: 15105
#excluir não diabéticos: -13636 restam 1322
#excluir diabetes na gravidez: -135
#excluir quem tem hipertensao HA gestacional na onda 1: -916
#n final: 418

#BASE FINAL

setwd("C:/Users/ferna/Downloads")
base_final <- readRDS("C:/Users/ferna/Downloads/base_final.rds")
View(base_final)

# Pacotes necessários
library(dplyr)
library(readxl)      # leitura de arquivos Excel
library(dplyr)       # manipulação de dados
library(janitor)     # tabelas de frequência e limpeza de nomes
library(ggplot2)     # gráficos para explorar
library(labelled)    # para aplicar labels em variáveis
library(survival)    #cox

#Criando uma base só com as variáveis que vamos usar
library(dplyr)

nova_base <- base_final %>%
  select(idelsa, centroa.x, rcta8, idadea, a_gidade, a_imc2, a_escolar, a_ativfisica, a_fumante, a_usodealcool, vifa29, cala2, hmpa02, hmpa04, mdga4, mdga2, a_famdmanybro, a_famdmanyfat, a_famdmanymom, a_famhptanybro, a_famhptanyfat, a_famhptanymom, a_has2_2, b_has2_2.x, c_has2.x, HAS_novo1, depoisEAS_HAS_novo1, dataEASanterior_HAS_novo1, dataEAS_HAS_novo1, rcpadataapini, antbdataapini, hmpcdataapini)

#Renomeando as variáveis:
library(dplyr)

nova_base <- nova_base %>%
  rename(
    sexo = rcta8, #1 = Masculino; 2 = Feminino
    idade_onda1 = idadea, #em anos
    idade_cat = a_gidade,  #1 = 35 a 44 anos; 2 = 45 a 54 anos; 3 = 55 a 64 anos; 4 = 65 a 74 anos
    imc_onda1 = a_imc2, #1 = Magreza; 2 = Eutrofia; 3 = Sobrepeso; 4 = Obeso
    escolaridade  = a_escolar, #1= Até fundamental incompleto; 2= Fundamental completo ; 3= Médio completo; 4= Superior Completo
    ativfisica = a_ativfisica, #1 = Fraca; 2 = Moderada; 3 = Forte
    tabagismo = a_fumante, #0 = Nunca fumou; 1 = Ex-fumante; 2 = Fumante
    usoalcool = a_usodealcool, #0 = Nunca usou; 1 = Ex-usuário; 2 = Usuário
    racacor = vifa29, #1 = Preta; 2 = Parda; 3 = Branca; 4 = Amarela; 5 = Indígena
    HA_autoref = hmpa02, #0 = Não; 1 = Sim (somente durante a gravidez); 2 = Sim
    DM_autoref = hmpa04, #0 = Não; 1 = Sim (somente durante a gravidez); 2 = Sim
    medHA = mdga4, #Medicamento para hipertensão nas últimas 2 semanas (Q13) 0 = Não; 1 = Sim
    medDM = mdga2, #Medicamento para diabetes nas últimas 2 semanas (Q11) 0 = Não; 1 = Sim
    DMirmao = a_famdmanybro, #Diabetes de irmão (qualquer idade) 0 = Não; 1 = Sim
    DMpai = a_famdmanyfat, #Diabetes do pai (qualquer idade) 0 = Não; 1 = Sim
    DMmae = a_famdmanymom, #Diabetes do pai (qualquer idade) 0 = Não; 1 = Sim
    HAirmao = a_famhptanybro, #Hipertensão de irmão (qualquer idade) 0 = Não; 1 = Sim
    HApai = a_famhptanyfat, #Hipertensão do pai (qualquer idade) 0 = Não; 1 = Sim
    HAmae = a_famhptanymom, #Hipertensão da mãe (qualquer idade) 0 = Não; 1 = Sim
    HAonda1 = a_has2_2, #Presença de hipertensão arterial sistêmica 0 = Não; 1 = Sim
    HAonda2 = b_has2_2.x, #Presença de Hipertensão Arterial Sistêmica 0= Não; 1= Sim
    HAonda3 = c_has2.x, #Presença de Hipertensão Arterial Sistêmica 0= Não; 1= Sim
    HAS_novo1 = HAS_novo1, # assume valor 1 para aqueles participantes que responderam "sim" para a pergunta "Depois da última entrevista telefônica ao ELSA, um médico informou que o/a Sr/Sra (nome do participante) apresenta hipertensão arterial (pressão alta)?". Corresponde ao primeiro "sim" do participante entre todas as EAS que respondeu
    depoisEAS_HAS_novo1 = depoisEAS_HAS_novo1, # assume valor 1 para aqueles participantes que responderam "sim" para a pergunta "Este diagnóstico foi depois da última entrevista telefônica ao ELSA?" e assume valor 0 para aqueles que responderam "não"
    dataEASanterior_HAS_novo1 = dataEASanterior_HAS_novo1, # data da EAS anterior à EAS em que foi realizado o primeiro relato positivo do participante à pergunta "Depois da última entrevista telefônica ao ELSA, um médico informou que o/a Sr/Sra (nome do participante) apresenta hipertensão arterial (pressão alta)?" (última data de EAS antes da dataEAS_HAS_novo1)
    dataEAS_HAS_novo1 = dataEAS_HAS_novo1, # data da EAS em que foi realizado o primeiro relato positivo do participante à pergunta "Depois da última entrevista telefônica ao ELSA, um médico informou que o/a Sr/Sra (nome do participante) apresenta hipertensão arterial (pressão alta)?"
    data_recrut = rcpadataapini, #Data recrutamento
    data_onda2 = antbdataapini, #Data de realização da antropometria na onda 2
    data_onda3 = hmpcdataapini #Data de realização da história médica pregressa onda 3
  )

#Salvar a base nova no computador
saveRDS(nova_base, file = "base_surv.rds")

#JUNTANDO A VARIAVEL DE HISTORICO FAMILIAR HA
#variável precisa ainda estar como numerica
base_surv <- base_surv %>%
  mutate(
    HAfamilia = if_else(
      (HAmae + HAirmao + HApai) > 0, "Sim", "Não"
    )
  )

tabyl(base_surv$HAfamilia)

#JUNTANDO A VARIAVEL DE HISTORICO FAMILIAR DM
#variável precisa ainda estar como numerica
base_surv <- base_surv %>%
  mutate(
    DMfamilia = if_else( 
      (DMmae + DMirmao +DMpai) > 0, "Sim", "Não"
    )
  )

tabyl(base_surv$DMfamilia)

#Criando a variável do status
library(dplyr)

base_surv <- base_surv %>%
  mutate(
    HAS_status = case_when(
      # Sem informação em nenhuma onda
      is.na(HAonda1) &
        is.na(HAonda2) &
        is.na(HAonda3) ~ NA_real_,

      # Hipertensão em pelo menos uma onda
      HAonda1 == 1 |
        HAonda2 == 1 |
        HAonda3 == 1 ~ 1,

      # Nenhum diagnóstico de HAS até a última onda observada
      TRUE ~ 0
    )
  )

##Cálculo do tempo total, tempo calendario
library(dplyr)

base_surv <- base_surv %>%
  mutate(
    data_final = case_when(
      
      # Evento com data específica
      HAS_status == 1 & !is.na(dataEAS_HAS_novo1) ~
        dataEAS_HAS_novo1,
      
      # Evento identificado na onda 1, sem data específica
      HAS_status == 1 &
        is.na(dataEAS_HAS_novo1) &
        HAonda1 %in% 1 &
        !is.na(data_recrut) ~
        data_recrut,
      
      # Primeiro evento identificado na onda 2
      HAS_status == 1 &
        is.na(dataEAS_HAS_novo1) &
        !(HAonda1 %in% 1) &
        HAonda2 %in% 1 &
        !is.na(data_onda2) ~
        data_onda2,
      
      # Primeiro evento identificado na onda 3
      HAS_status == 1 &
        is.na(dataEAS_HAS_novo1) &
        !(HAonda1 %in% 1) &
        !(HAonda2 %in% 1) &
        HAonda3 %in% 1 &
        !is.na(data_onda3) ~
        data_onda3,
      
      # Sem evento: censura na última onda com data disponível
      HAS_status == 0 & !is.na(data_onda3) ~ data_onda3,
      HAS_status == 0 & !is.na(data_onda2) ~ data_onda2,
      HAS_status == 0 & !is.na(data_recrut) ~ data_recrut,
      
      TRUE ~ as.Date(NA)
    ),
    
    ini = as.numeric(
      difftime(
        data_recrut,
        min(data_recrut, na.rm = TRUE),
        units = "days"
      )
    ),
    
    fim = as.numeric(
      difftime(
        data_final,
        min(data_recrut, na.rm = TRUE),
        units = "days"
      )
    ),
    
    tempo = as.numeric(
      difftime(
        data_final,
        data_recrut,
        units = "days"
      )
    ),
    
    tempo_anos = tempo / 365.25
  )

#Ver quem está com tempo 0 
base_surv %>%
  filter(tempo == 0) %>%
  count(HAS_status)

#Ver os censurados
base_surv %>%
  filter(tempo == 0, HAS_status == 0) %>%
  select(
    data_recrut,
    data_onda2,
    data_onda3,
    HAonda1,
    HAonda2,
    HAonda3
  )

#Excluir os censurados com tempo 0
base_surv2 <- base_surv %>%
  filter(!(tempo == 0 & HAS_status == 0))

#Ver os com desfecho mas com tempo de acompanhamento 0 
base_surv2 %>%
  filter(tempo == 0, HAS_status == 1) %>%
  select(
    data_recrut,
    dataEAS_HAS_novo1,
    data_onda2,
    data_onda3,
    HAonda1,
    HAonda2,
    HAonda3
  )

# Flag para tempos nao-positivos (comparecentes apenas a linha de base -> 0 seguimento)
base_surv2 <- base_surv2 %>%
  mutate(tempo_flag = ifelse(is.na(tempo) | tempo <= 0, "sem_seguimento", "ok"))
d_analitico <- base_surv2 %>%
  filter(tempo_flag != "sem_seguimento")

table(d_analitico$tempo_flag)

#Dividir os 28 participantes com evento em dois grupos: 
base_surv %>%
  filter(tempo == 0, HAS_status == 1) %>%
  mutate(
    possui_seguimento = !is.na(data_onda2) | !is.na(data_onda3)
  ) %>%
  count(possui_seguimento)

#FALSE --> não tem nenhuma data de seguimento
#TRUE --> tem data na onda 2 ou 3

# Excluir quem não tem seguimento
base_surv2 <- base_surv2 %>%
  filter(
    !(
      tempo == 0 &
        HAS_status == 1 &
        is.na(data_onda2) &
        is.na(data_onda3)
    )
  )

#Ajustar data final 
base_surv2 <- base_surv2 %>%
  mutate(
    data_final = case_when(
      tempo == 0 &
        HAS_status == 1 &
        !is.na(data_onda2) ~ data_onda2,
      
      tempo == 0 &
        HAS_status == 1 &
        is.na(data_onda2) &
        !is.na(data_onda3) ~ data_onda3,
      
      TRUE ~ data_final
    ),
    
    fim = as.numeric(
      difftime(
        data_final,
        min(data_recrut, na.rm = TRUE),
        units = "days"
      )
    ),
    
    tempo = as.numeric(
      difftime(
        data_final,
        data_recrut,
        units = "days"
      )
    ),
    
    tempo_anos = tempo / 365.25
  )

# Flag para tempos nao-positivos (comparecentes apenas a linha de base -> 0 seguimento)
base_surv2 <- base_surv2 %>%
  mutate(tempo_flag = ifelse(is.na(tempo) | tempo <= 0, "sem_seguimento", "ok"))
d_analitico <- base_surv2 %>%
  filter(tempo_flag != "sem_seguimento")

# Criando o Surv
#O comando Surv() tem como função combinar, em uma única 
#variável, a informação referente ao tempo de sobrevivência de
#cada indivíduo e a informação a respeito do status do paciente.

library(survival)
Surv(base_surv2$tempo, base_surv2$HAS_status)
library(survival)

surv_HAS <- Surv(
  time = base_surv2$tempo_anos,
  event = base_surv2$HAS_status
)

#Dando nome para as categorias das variáveis
library(dplyr)

base_surv2 <- base_surv2 %>%
  mutate(
    sexo = factor(
      sexo,
      levels = c(1, 2),
      labels = c("Masculino", "Feminino")
    ),
    
    idade_cat = factor(
      idade_cat,
      levels = c(1, 2, 3, 4),
      labels = c(
        "35 a 44 anos",
        "45 a 54 anos",
        "55 a 64 anos",
        "65 a 74 anos"
      )
    ),
    
    racacor = factor(
      racacor,
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Preta",
        "Parda",
        "Branca",
        "Amarela",
        "Indígena"
      )
    ),
    
    escolaridade = factor(
      escolaridade,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Até fundamental incompleto",
        "Fundamental completo",
        "Médio completo",
        "Superior completo"
      )
    ),
    
    imc_onda1 = factor(
      imc_onda1,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Magreza",
        "Eutrofia",
        "Sobrepeso",
        "Obesidade"
      )
    ),
    
    ativfisica = factor(
      ativfisica,
      levels = c(1, 2, 3),
      labels = c(
        "Fraca",
        "Moderada",
        "Forte"
      )
    ),
    
    tabagismo = factor(
      tabagismo,
      levels = c(0, 1, 2),
      labels = c(
        "Nunca fumou",
        "Ex-fumante",
        "Fumante"
      )
    ),
    
    usoalcool = factor(
      usoalcool,
      levels = c(0, 1, 2),
      labels = c(
        "Nunca usou",
        "Ex-usuário",
        "Usuário"
      )
    ),
    
    HAfamilia = case_when(
      HAirmao == 1 | HApai == 1 | HAmae == 1 ~ 1,
      HAirmao == 0 | HApai == 0 | HAmae == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    DMfamilia = case_when(
      DMirmao == 1 | DMpai == 1 | DMmae == 1 ~ 1,
      DMirmao == 0 | DMpai == 0 | DMmae == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    
    HAfamilia = factor(
      HAfamilia,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    ),
    
    DMfamilia = factor(
      DMfamilia,
      levels = c(0, 1),
      labels = c("Não", "Sim")
    ),
    
    HAS_status = factor(
      HAS_status,
      levels = c(0, 1),
      labels = c(
        "Não desenvolveu HAS",
        "Desenvolveu HAS"
      )
    )
  )

#Criar tabela 1
library(gtsummary)

tabela1 <- base_surv2 %>%
  select(
    HAS_status,
    sexo,
    idade_cat,
    racacor,
    escolaridade,
    imc_onda1,
    ativfisica,
    tabagismo,
    usoalcool,
    HAfamilia,
    DMfamilia
  ) %>%
  tbl_summary(
    by = HAS_status,
    
    statistic = all_categorical() ~ "{n} ({p}%)",
    
    missing = "ifany",
    missing_text = "Sem informação",
    
    label = list(
      sexo ~ "Sexo",
      idade_cat ~ "Faixa etária",
      racacor ~ "Raça/cor",
      escolaridade ~ "Escolaridade",
      imc_onda1 ~ "Estado nutricional segundo o IMC",
      ativfisica ~ "Atividade física",
      tabagismo ~ "Tabagismo",
      usoalcool ~ "Uso de álcool",
      HAfamilia ~ "História familiar de hipertensão",
      DMfamilia ~ "História familiar de diabetes"
    )
  ) %>%
  add_overall(last = TRUE) %>%
  add_p() %>%
  bold_labels()

tabela1

#Transformando HAS_status em numérico

base_surv2 <- base_surv2 %>%
  mutate(
    HAS_status_num = case_when(
      as.character(HAS_status) %in%
        c("1", "Desenvolveu HAS") ~ 1,
      
      as.character(HAS_status) %in%
        c("0", "Não desenvolveu HAS") ~ 0,
      
      TRUE ~ NA_real_
    )
  )

# Conferindo a nova variável
table(base_surv2$HAS_status_num, useNA = "ifany")

#Salvar a base nova no computador
base_surv2 <- base_surv2
saveRDS(base_surv2, file = "base_surv2.rds")

#Kaplan-meier
#Mediana do tempo até o evento
library(survival)

# Ajuste da curva de Kaplan-Meier
km <- survfit(surv_HAS ~ 1, data = base_surv2)

summary(km)
print(km)

#Descrever a sobrevivência segundo sexo
library(survival)

km_sexo <- survfit(
  surv_HAS ~ sexo,
  data = base_surv2
)

summary(km_sexo)

print(km_sexo)

#####Taxa de incidencia 

# 1. Número de eventos (participantes que desenvolveram HAS)

eventos <- sum(base_surv2$HAS_status_num == 1, na.rm = TRUE)

# 2. Total de pessoas-ano de acompanhamento

pessoas_ano <- sum(base_surv2$tempo_anos, na.rm = TRUE)

# 3. Calcular a taxa de incidência por 1.000 pessoas-ano

taxa_incidencia <- (eventos / pessoas_ano) * 1000

# 4. Calcular o IC95% da taxa utilizando distribuição de Poisson

teste_poisson <- poisson.test(
  x = eventos,
  T = pessoas_ano,
  conf.level = 0.95
)

# 5. Organizar os resultados

resultado_incidencia <- data.frame(
  
  Participantes = nrow(base_surv2),
  
  Eventos = eventos,
  
  Pessoas_ano = round(pessoas_ano, 2),
  
  Taxa_1000_pessoas_ano = round(taxa_incidencia, 2),
  
  IC95_inferior = round(teste_poisson$conf.int[1] * 1000, 2),
  
  IC95_superior = round(teste_poisson$conf.int[2] * 1000, 2)
  
)

# 6. Visualizar os resultados

resultado_incidencia

# TAXA DE INCIDÊNCIA DE HAS POR SEXO

library(dplyr)

resultado_sexo <- base_surv2 %>%
  group_by(sexo) %>%
  summarise(
    
    # Número de participantes
    Participantes = n(),
    
    # Número de eventos
    Eventos = sum(HAS_status_num == 1, na.rm = TRUE),
    
    # Total de pessoas-ano
    Pessoas_ano = sum(tempo_anos, na.rm = TRUE),
    
    # Taxa de incidência por 1000 pessoas-ano
    Taxa_1000_pessoas_ano = Eventos / Pessoas_ano * 1000,
    
    .groups = "drop"
    
  )

#-----------------------------------------------------------
# Calcular IC95% para cada sexo
#-----------------------------------------------------------

resultado_sexo <- resultado_sexo %>%
  rowwise() %>%
  mutate(
    
    poisson = list(
      poisson.test(
        x = Eventos,
        T = Pessoas_ano
      )
    ),
    
    IC95_inferior = poisson$conf.int[1] * 1000,
    
    IC95_superior = poisson$conf.int[2] * 1000
    
  ) %>%
  ungroup() %>%
  select(-poisson) %>%
  mutate(
    
    Pessoas_ano = round(Pessoas_ano, 2),
    
    Taxa_1000_pessoas_ano = round(Taxa_1000_pessoas_ano, 2),
    
    IC95_inferior = round(IC95_inferior, 2),
    
    IC95_superior = round(IC95_superior, 2)
    
  )

resultado_sexo

# Distribuição do tempo até o desenvolvimento de HAS

hist(
  base_surv2$tempo_anos[base_surv2$HAS_status_num == 1],
  breaks = 12,
  main = "Tempo até o desenvolvimento de HAS",
  ylab = "Frequência",
  xlab = "Tempo (anos)"
)

# Distribuição do tempo de acompanhamento dos participantes censurados

hist(
  base_surv2$tempo_anos[base_surv2$HAS_status_num == 0],
  breaks = 12,
  main = "Tempo até a censura",
  ylab = "Frequência",
  xlab = "Tempo (anos)"
)

#--------------------------------------------------
# KAPLAN-MEIER
# Tempo até o desenvolvimento de HAS
#--------------------------------------------------

library(survival)

# Ajuste da curva de Kaplan-Meier
KM <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ 1,
  data = base_surv2
)

# Mostrar resultados
KM

# Resumo detalhado
summary(KM)

# Gráfico de Kaplan-Meier
plot(
  KM,
  xlab = "Tempo de acompanhamento (anos)",
  ylab = "Probabilidade de permanecer sem HAS",
  main = "Curva de Sobrevivência"
)

# Linha horizontal indicando 50% de sobrevivência
abline(
  h = 0.5,
  lty = 2
)

#------------------------------------------------------------
# KAPLAN-MEIER SEGUNDO SEXO
# Tempo até o desenvolvimento de HAS
#------------------------------------------------------------

library(survival)

# 1. Kaplan-Meier estratificada por sexo

KMsexo <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ sexo,
  data = base_surv2
)

KMsexo


#------------------------------------------------------------
# 2. Curvas de Kaplan-Meier COM intervalo de confiança
#------------------------------------------------------------

plot(
  KMsexo,
  lty = 1:2,
  col = 1:2,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = TRUE
)

legend(
  "bottomleft",
  legend = c("Masculino", "Feminino"),
  lty = 1:2,
  col = 1:2
)

title("Curvas de Kaplan-Meier segundo sexo")


#------------------------------------------------------------
# 3. Curvas de Kaplan-Meier SEM intervalo de confiança
#------------------------------------------------------------

plot(
  KMsexo,
  lty = 1:2,
  col = 1:2,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = c("Masculino", "Feminino"),
  lty = 1:2,
  col = 1:2
)

title("Curvas de Kaplan-Meier segundo sexo")


#------------------------------------------------------------
# 4. Risco acumulado de desenvolver HAS segundo sexo
#------------------------------------------------------------

plot(
  KMsexo,
  lty = 1:2,
  col = 1:2,
  fun = "cumhaz",
  ylab = "Risco acumulado",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "topleft",
  legend = c("Masculino", "Feminino"),
  lty = 1:2,
  col = 1:2
)

title("Risco acumulado de HAS segundo sexo")


#------------------------------------------------------------
# 5. Teste de log-rank
#------------------------------------------------------------

logrank <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ sexo,
  data = base_surv2
)

logrank


#------------------------------------------------------------
# 6. Teste de Peto
# rho = 1 dá maior peso aos eventos mais precoces
#------------------------------------------------------------

peto <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ sexo,
  data = base_surv2,
  rho = 1
)

peto

#------------------------------------------------------------
# KAPLAN-MEIER SEGUNDO FAIXA ETÁRIA
#------------------------------------------------------------

library(survival)

# Kaplan-Meier segundo faixa etária
KMidade <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ idade_cat,
  data = base_surv2
)

KMidade


#------------------------------------------------------------
# Gráfico COM intervalo de confiança
#------------------------------------------------------------

plot(
  KMidade,
  lty = 1:4,
  col = 1:4,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = TRUE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$idade_cat),
  lty = 1:4,
  col = 1:4,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo faixa etária")


#------------------------------------------------------------
# Gráfico SEM intervalo de confiança
#------------------------------------------------------------

plot(
  KMidade,
  lty = 1:4,
  col = 1:4,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$idade_cat),
  lty = 1:4,
  col = 1:4,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo faixa etária")


#------------------------------------------------------------
# TESTE DE LOG-RANK
#------------------------------------------------------------

logrank_idade <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ idade_cat,
  data = base_surv2
)

logrank_idade


#------------------------------------------------------------
# TESTE DE PETO
#------------------------------------------------------------

peto_idade <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ idade_cat,
  data = base_surv2,
  rho = 1
)

peto_idade

#------------------------------------------------------------
# KAPLAN-MEIER SEGUNDO RAÇA/COR
# Tempo até o desenvolvimento de HAS
#------------------------------------------------------------

library(survival)

# 1. Kaplan-Meier segundo raça/cor

KMraca <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ racacor,
  data = base_surv2
)

KMraca


#------------------------------------------------------------
# 2. Gráfico COM intervalo de confiança
#------------------------------------------------------------

plot(
  KMraca,
  lty = 1:5,
  col = 1:5,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = TRUE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$racacor),
  lty = 1:5,
  col = 1:5,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo raça/cor")


#------------------------------------------------------------
# 3. Gráfico SEM intervalo de confiança
#------------------------------------------------------------

plot(
  KMraca,
  lty = 1:5,
  col = 1:5,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$racacor),
  lty = 1:5,
  col = 1:5,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo raça/cor")


#------------------------------------------------------------
# 4. Risco acumulado de desenvolver HAS segundo raça/cor
#------------------------------------------------------------

plot(
  KMraca,
  lty = 1:5,
  col = 1:5,
  fun = "cumhaz",
  ylab = "Risco acumulado de desenvolver HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "topleft",
  legend = levels(base_surv2$racacor),
  lty = 1:5,
  col = 1:5,
  cex = 0.8
)

title("Risco acumulado de HAS segundo raça/cor")


#------------------------------------------------------------
# 5. TESTE DE LOG-RANK
#------------------------------------------------------------

logrank_raca <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ racacor,
  data = base_surv2
)

logrank_raca


#------------------------------------------------------------
# 6. TESTE DE PETO
# rho = 1 dá maior peso aos eventos mais precoces
#------------------------------------------------------------

peto_raca <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ racacor,
  data = base_surv2,
  rho = 1
)

peto_raca

#------------------------------------------------------------
# KAPLAN-MEIER SEGUNDO ESCOLARIDADE
# Tempo até o desenvolvimento de HAS
#------------------------------------------------------------

library(survival)

# 1. Kaplan-Meier segundo escolaridade
KMescolar <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ escolaridade,
  data = base_surv2
)

KMescolar


#------------------------------------------------------------
# 2. Gráfico COM intervalo de confiança
#------------------------------------------------------------

plot(
  KMescolar,
  lty = 1:4,
  col = 1:4,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = TRUE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$escolaridade),
  lty = 1:4,
  col = 1:4,
  cex = 0.7
)

title("Curvas de Kaplan-Meier segundo escolaridade")


#------------------------------------------------------------
# 3. Gráfico SEM intervalo de confiança
#------------------------------------------------------------

plot(
  KMescolar,
  lty = 1:4,
  col = 1:4,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$escolaridade),
  lty = 1:4,
  col = 1:4,
  cex = 0.7
)

title("Curvas de Kaplan-Meier segundo escolaridade")


#------------------------------------------------------------
# 4. Risco acumulado de desenvolver HAS segundo escolaridade
#------------------------------------------------------------

plot(
  KMescolar,
  lty = 1:4,
  col = 1:4,
  fun = "cumhaz",
  ylab = "Risco acumulado de desenvolver HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "topleft",
  legend = levels(base_surv2$escolaridade),
  lty = 1:4,
  col = 1:4,
  cex = 0.7
)

title("Risco acumulado de HAS segundo escolaridade")


#------------------------------------------------------------
# 5. TESTE DE LOG-RANK
#------------------------------------------------------------

logrank_escolar <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ escolaridade,
  data = base_surv2
)

logrank_escolar


#------------------------------------------------------------
# 6. TESTE DE PETO
#------------------------------------------------------------

peto_escolar <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ escolaridade,
  data = base_surv2,
  rho = 1
)

peto_escolar

#------------------------------------------------------------
# KAPLAN-MEIER SEGUNDO IMC
# Tempo até o desenvolvimento de HAS
#------------------------------------------------------------

library(survival)

# 1. Kaplan-Meier segundo IMC

KMimc <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
  data = base_surv2
)

KMimc


#------------------------------------------------------------
# 2. Gráfico COM intervalo de confiança
#------------------------------------------------------------

plot(
  KMimc,
  lty = 1:4,
  col = 1:4,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = TRUE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$imc_onda1),
  lty = 1:4,
  col = 1:4,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo IMC")


#------------------------------------------------------------
# 3. Gráfico SEM intervalo de confiança
#------------------------------------------------------------

plot(
  KMimc,
  lty = 1:4,
  col = 1:4,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$imc_onda1),
  lty = 1:4,
  col = 1:4,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo IMC")


#------------------------------------------------------------
# 4. Risco acumulado de desenvolver HAS segundo IMC
#------------------------------------------------------------

plot(
  KMimc,
  lty = 1:4,
  col = 1:4,
  fun = "cumhaz",
  ylab = "Risco acumulado de desenvolver HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "topleft",
  legend = levels(base_surv2$imc_onda1),
  lty = 1:4,
  col = 1:4,
  cex = 0.8
)

title("Risco acumulado de HAS segundo IMC")


#------------------------------------------------------------
# 5. TESTE DE LOG-RANK
#------------------------------------------------------------

logrank_imc <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
  data = base_surv2
)

logrank_imc


#------------------------------------------------------------
# 6. TESTE DE PETO
#------------------------------------------------------------

peto_imc <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
  data = base_surv2,
  rho = 1
)

peto_imc

library(dplyr)
library(survival)

# Criar base para análise de sensibilidade
# O participante com Magreza NÃO é apagado da base original
base_imc_sens <- base_surv2 %>%
  filter(imc_onda1 != "Magreza") %>%
  droplevels()

# Conferir número de participantes em cada categoria
table(base_imc_sens$imc_onda1)

# Kaplan-Meier
KMimc_sens <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
  data = base_imc_sens
)

KMimc_sens

# Teste de log-rank
logrank_imc_sens <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
  data = base_imc_sens
)

logrank_imc_sens

# Teste de Peto
peto_imc_sens <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
  data = base_imc_sens,
  rho = 1
)

peto_imc_sens

#============================================================
# KAPLAN-MEIER: ATIVIDADE FÍSICA, TABAGISMO E USO DE ÁLCOOL
# Desfecho: desenvolvimento de HAS
#============================================================

library(survival)


#============================================================
# 1. ATIVIDADE FÍSICA
#============================================================

# Kaplan-Meier
KMativfisica <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ ativfisica,
  data = base_surv2
)

KMativfisica


# Curva de Kaplan-Meier
plot(
  KMativfisica,
  lty = 1:3,
  col = 1:3,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$ativfisica),
  lty = 1:3,
  col = 1:3,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo atividade física")


# Risco acumulado
plot(
  KMativfisica,
  lty = 1:3,
  col = 1:3,
  fun = "cumhaz",
  ylab = "Risco acumulado de desenvolver HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "topleft",
  legend = levels(base_surv2$ativfisica),
  lty = 1:3,
  col = 1:3,
  cex = 0.8
)

title("Risco acumulado de HAS segundo atividade física")


# Log-rank
logrank_ativfisica <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ ativfisica,
  data = base_surv2
)

logrank_ativfisica


# Peto
peto_ativfisica <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ ativfisica,
  data = base_surv2,
  rho = 1
)

peto_ativfisica



#============================================================
# 2. TABAGISMO
#============================================================

# Kaplan-Meier
KMtabagismo <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ tabagismo,
  data = base_surv2
)

KMtabagismo


# Curva de Kaplan-Meier
plot(
  KMtabagismo,
  lty = 1:3,
  col = 1:3,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$tabagismo),
  lty = 1:3,
  col = 1:3,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo tabagismo")


# Risco acumulado
plot(
  KMtabagismo,
  lty = 1:3,
  col = 1:3,
  fun = "cumhaz",
  ylab = "Risco acumulado de desenvolver HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "topleft",
  legend = levels(base_surv2$tabagismo),
  lty = 1:3,
  col = 1:3,
  cex = 0.8
)

title("Risco acumulado de HAS segundo tabagismo")


# Log-rank
logrank_tabagismo <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ tabagismo,
  data = base_surv2
)

logrank_tabagismo


# Peto
peto_tabagismo <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ tabagismo,
  data = base_surv2,
  rho = 1
)

peto_tabagismo



#============================================================
# 3. USO DE ÁLCOOL
#============================================================

# Kaplan-Meier
KMalcool <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ usoalcool,
  data = base_surv2
)

KMalcool


# Curva de Kaplan-Meier
plot(
  KMalcool,
  lty = 1:3,
  col = 1:3,
  ylab = "Probabilidade de permanecer sem HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$usoalcool),
  lty = 1:3,
  col = 1:3,
  cex = 0.8
)

title("Curvas de Kaplan-Meier segundo uso de álcool")


# Risco acumulado
plot(
  KMalcool,
  lty = 1:3,
  col = 1:3,
  fun = "cumhaz",
  ylab = "Risco acumulado de desenvolver HAS",
  xlab = "Tempo de acompanhamento (anos)",
  conf.int = FALSE
)

legend(
  "topleft",
  legend = levels(base_surv2$usoalcool),
  lty = 1:3,
  col = 1:3,
  cex = 0.8
)

title("Risco acumulado de HAS segundo uso de álcool")


# Log-rank
logrank_alcool <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ usoalcool,
  data = base_surv2
)

logrank_alcool


# Peto
peto_alcool <- survdiff(
  Surv(tempo_anos, HAS_status_num) ~ usoalcool,
  data = base_surv2,
  rho = 1
)

peto_alcool

#============================================================
# FIGURA COM CURVAS DE KAPLAN-MEIER
# Faixa etária, raça/cor, escolaridade, atividade física,
# tabagismo e IMC
#============================================================

library(survival)

# Criar os objetos Kaplan-Meier
KMidade <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ idade_cat,
  data = base_surv2
)

KMraca <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ racacor,
  data = base_surv2
)

KMescolar <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ escolaridade,
  data = base_surv2
)

KMativfisica <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ ativfisica,
  data = base_surv2
)

KMtabagismo <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ tabagismo,
  data = base_surv2
)

KMimc <- survfit(
  Surv(tempo_anos, HAS_status_num) ~ imc_onda1,
  data = base_surv2
)


#============================================================
# Organizar 6 gráficos em uma única figura
# 2 linhas x 3 colunas
#============================================================

par(mfrow = c(2, 3))


# 1. FAIXA ETÁRIA
plot(
  KMidade,
  main = "Faixa etária",
  col = 1:4,
  lty = 1:4,
  xlab = "Tempo (anos)",
  ylab = "Probabilidade de permanecer sem HAS",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$idade_cat),
  col = 1:4,
  lty = 1:4,
  cex = 0.6
)


# 2. RAÇA/COR
plot(
  KMraca,
  main = "Raça/cor",
  col = 1:5,
  lty = 1:5,
  xlab = "Tempo (anos)",
  ylab = "Probabilidade de permanecer sem HAS",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$racacor),
  col = 1:5,
  lty = 1:5,
  cex = 0.6
)


# 3. ESCOLARIDADE
plot(
  KMescolar,
  main = "Escolaridade",
  col = 1:4,
  lty = 1:4,
  xlab = "Tempo (anos)",
  ylab = "Probabilidade de permanecer sem HAS",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$escolaridade),
  col = 1:4,
  lty = 1:4,
  cex = 0.55
)


# 4. ATIVIDADE FÍSICA
plot(
  KMativfisica,
  main = "Atividade física",
  col = 1:3,
  lty = 1:3,
  xlab = "Tempo (anos)",
  ylab = "Probabilidade de permanecer sem HAS",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$ativfisica),
  col = 1:3,
  lty = 1:3,
  cex = 0.65
)


# 5. TABAGISMO
plot(
  KMtabagismo,
  main = "Tabagismo",
  col = 1:3,
  lty = 1:3,
  xlab = "Tempo (anos)",
  ylab = "Probabilidade de permanecer sem HAS",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$tabagismo),
  col = 1:3,
  lty = 1:3,
  cex = 0.65
)


# 6. IMC
plot(
  KMimc,
  main = "IMC",
  col = 1:4,
  lty = 1:4,
  xlab = "Tempo (anos)",
  ylab = "Probabilidade de permanecer sem HAS",
  conf.int = FALSE
)

legend(
  "bottomleft",
  legend = levels(base_surv2$imc_onda1),
  col = 1:4,
  lty = 1:4,
  cex = 0.65
)


# Voltar ao padrão de um gráfico por vez
par(mfrow = c(1, 1))

#Salvando o environment
save.image("C:/Users/ferna/Downloads/environment sobrevida.RData")




