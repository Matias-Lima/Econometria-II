## Econometria II
## Aula 04 - Variáveis instrumentais em R


install.packages("AER")
install.packages("haven")
install.packages("tidyverse")
install.packages("xtable")
install.packages("knitr")
install.packages("dplyr")
install.packages("broom")
install.packages("stargazer")
install.packages("corrplot")
install.packages("readxl")
install.packages("magrittr")
install.packages("ggplot2")
install.packages("sjmisc")
install.packages("readr")
install.packages("tableone")


library("haven")
library("tidyverse")
library("readxl")
library("stargazer")
library("magrittr") 
#Para usar o pipes
# é usado principalmente para encadear operações em objetos de dados 
# é usado para acessar variaveis dentro de objetos de dados. 
library("ggplot2")
library("sjmisc")
library("readr")
library("tableone")
library("broom")
library("AER")
library("dplyr")
library("corrplot")


# Exercicio 

card <- read_dta("C:/Users/limam/Downloads/card.dta")

## O objetivo é estimar a seguinte equação 

y1 <- card$lwage
y2 <- card$educ
x1 <- cbind(card$exper, card$black, card$south, card$married, card$smsa)
x2 <- card$nearc4

# -----------------
# 1 Estágio 
# ----------------

ols_reg <- lm(y1 ~ y2 + x1, data = card)
summary(ols_reg)


# Correlação

corr1 <- lm(educ ~ nearc4, data = card)
summary(corr1)

# Salvar os valores preditos para educ

card$educ_pred <- corr1$fitted.values

IV <- lm(y1 ~ educ_pred, data = card)
summary(IV)


#
# 2 Estágio
#
iv_reg <- ivreg(y1 ~ y2 + x1 | x1 + x2,
             data = card)
summary(iv_reg)

stargazer(ols_reg, iv_reg, IV,
          type = "text",
          column.labels = c("OLS", "IV reg", "IV")
          )


# -----------------------
# Exercício 2
# -----------------------

options(scipen = 999)# Desligamos as notações cientificas
PNAD2014 <- read_csv("C:/Users/limam/Downloads/[Base de Dados] PNAD2014-adaptada.csv")

# ----------------
# Rodar OLS
# ----------------

OLS <- lm(log(salario) ~ educ, 
          data = PNAD2014)

summary(OLS)

# Testar correlação

corr2 <- lm(educ ~ educpai, 
            data = PNAD2014)
summary(corr2)

PNAD2014$educ_pred <- corr2$fitted.values

IV <- lm(log(salario) ~ educ_pred,
         data = PNAD2014)

stargazer(OLS, IV,
          type = "text",
          column.labels = c("OLS", "IV")
          
)


#---------------
# ------ Exercicio 3 ------------- 


estagio1 <- lm(educ ~ educpai + educmae,
              data = PNAD2014)

PNAD2014$educ_predM2E <- estagio1$fitted.values

estagio2 <- lm(log(salario) ~ educ_predM2E,
              data = PNAD2014)

summary(estagio2)


# Forma direta

SLS2_pacote <- ivreg(formula = log(salario) ~ educ | educmae + educpai,
                     data = PNAD2014)

stargazer(OLS, IV, estagio2, SLS2_pacote,
          type = "text",
          column.labels = c("OLS", "IV", "estagio2", "2SLS")
)

SLS2_raca <- ivreg(formula = log(salario) ~ educ + raca | educmae + educpai,
                     data = PNAD2014)

stargazer(OLS, IV, estagio2, SLS2_pacote, SLS2_raca,
          type = "text",
          column.labels = c("OLS", "IV", "estagio2", "2SLS", "2SLS - 2 modelo")
)

