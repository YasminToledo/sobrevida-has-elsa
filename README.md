# Tempo de sobrevida até ao desenvolvimento de hipertensão e os seus fatores preditivos em uma coorte de diabéticos no Brasil

Fernanda Marassi¹, Yasmin Toledo¹

¹ Escola Nacional de Saúde Pública Sergio Arouca, Fundação Oswaldo Cruz, Brazil

---

Código de análise do estudo. A coorte é formada por participantes do Estudo
Longitudinal de Saúde do Adulto (ELSA-Brasil) com diabetes mellitus e sem
hipertensão arterial sistêmica (HAS) na linha de base (Onda 1), acompanhados
até as Ondas 2 e 3 e as entrevistas anuais de saúde. O desfecho é o tempo até
o desenvolvimento de HAS, analisado por Kaplan-Meier e por regressão de Cox de
riscos proporcionais.

## Scripts

| Arquivo | O que faz |
|---|---|
| `01_Analise Descritiva e KM.R` | Constrói a base analítica a partir dos dados brutos: seleção da coorte, renomeação e rotulagem das variáveis, definição do desfecho, do tempo sob risco e da censura. Gera a tabela descritiva, as taxas de incidência, as curvas de Kaplan-Meier e os testes de log-rank e de Peto. |
| `02_Cox.R` | Reagrupa o IMC em três categorias, define a base de casos completos dos modelos (n = 375) e ajusta os modelos de Cox simples e os cinco modelos aninhados, comparados pelo teste da razão de verossimilhança. |
| `03_Schoenfeld.R` | Avalia o pressuposto de riscos proporcionais do Modelo 5 pelos resíduos de Schoenfeld, reajusta o modelo separando os eventos anteriores e posteriores a 4 anos de seguimento e examina resíduos deviance e DFBETAS dos dois períodos. |
| `04_Figuras.R` | Redesenha, em versão pronta para publicação, as figuras que os três scripts acima já produzem. Não refaz nenhuma estimativa: reaproveita as mesmas chamadas a `survfit`, `coxph`, `survdiff`, `cox.zph` e `resid`, e a curva suavizada de Schoenfeld vem do próprio `survival:::plot.cox.zph`. Grava PNG em 300 dpi e PDF vetorial em `figuras_publicacao/`, junto com as legendas sugeridas. |

## Ordem de execução

Os scripts compartilham objetos em memória e devem ser rodados na ordem
`01` → `02` → `03` → `04`, na mesma sessão. O script de figuras também roda
sozinho: ele lê `base_surv2.rds` e reconstrói o que precisa.

## Dados

**Os microdados não estão neste repositório.** Os dados individuais do
ELSA-Brasil são de acesso restrito e não podem ser redistribuídos; o acesso é
solicitado ao Comitê Diretivo do estudo. Os scripts esperam encontrar, na pasta
de trabalho, os arquivos intermediários gerados pelo script `01`
(`base_surv.rds`, `base_surv2.rds`) e pelo `02` (`base_cox.rds`).

Números de referência da coorte, para conferência: 391 participantes e 201
eventos na base descritiva; 375 participantes e 193 eventos na base de casos
completos usada nos modelos.

## Requisitos

R 4.5.1 ou superior, com os pacotes:

`survival`, `dplyr`, `readxl`, `janitor`, `labelled`, `gtsummary`, `ggplot2`,
`scales`, `cowplot`, `viridisLite`, `broom`, `writexl`.

```r
install.packages(c("survival", "dplyr", "readxl", "janitor", "labelled",
                   "gtsummary", "ggplot2", "scales", "cowplot",
                   "viridisLite", "broom", "writexl"))
```
