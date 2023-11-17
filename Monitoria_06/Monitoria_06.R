#Econometria II
# Aula 6 - 17.11.2023
# Aplicando dados em Painel 

install.packages("tidyverse")
install.packages("xtable")
install.packages("dplyr")
install.packages("stargazer")
install.packages("readr")
install.packages("plm")
install.packages("AER")
install.packages("wooldridge")

library("stargazer")
library("tidyverse")
library("xtable")
library("plm")
library("readr")
library("AER")
library("dplyr")
library("wooldridge")


#---------------- Exercicio 1 

data("fertil1") # Efeito da educação na fertilidade


summary(fertil1)
freq_year <- table(fertil1$year)
print(freq_year)



reg1 <- lm(kids ~ educ +age + agesq + black + east +  northcen + west  + farm + othrural + town +
             smcity + y74  + y76  +y78  +y80+ y82  +y84,
           data = fertil1)
summary(reg1)

stargazer(reg1,
          type = "text")

# Ano base 72
# MUlheres em 82 teriam menos 52 do que em 74



#Possibilidade que educação seja endogeno

reg2_IV1 <- ivreg(kids ~ educ +age + agesq + black + east +  northcen + west  + farm + othrural + town +
             smcity + y74  + y76  +y78  +y80+ y82  +y84|
               meduc + feduc +age + agesq + black + east +  northcen + west  + farm + othrural + town +
               smcity + y74  + y76  +y78  +y80+ y82  + y84,
           data = fertil1)

stargazer(reg1,reg2_IV1,
          type = "text")


# A estimativa de MQ2E um efeito maior de certa forma maior da educação sobre a fertilidade.

reg3_res <- lm(kids ~ educ +age + agesq + black + east +  northcen + west  + farm + othrural + town +
             smcity + y74  + y76  +y78  +y80+ y82  +y84 + reg1$resid,
           data = fertil1)


stargazer(reg1, reg2_IV1, reg3_res,
          type = "text")


# ---------------------  Exercicio 2
#

data("jtrain") # Efeito do treinamento sobre a produtividade


freq_year_jtrain <- table(jtrain$year)
print(freq_year_jtrain)
summary(jtrain)


# Diferenciação

d.lscrap <- with(jtrain,
                 lscrap[year == 1988] - lscrap[year == 1987])
d.hrsemp <- with(jtrain,
                 hrsemp[year == 1988] - hrsemp[year == 1987])
d.grant <- with(jtrain,
                grant[year == 1988] - grant[year == 1987])


# Primeiro Estágio

reg_1estagio <- lm(d.hrsemp ~ d.grant,
                   data = jtrain)

stargazer(reg_1estagio,
          type = "text")

# Segundo Estágio

reg_2estagio <- ivreg(d.lscrap ~ d.hrsemp| d.grant,
                   data = jtrain)

stargazer(reg_1estagio,reg_2estagio,
          type = "text")

#Exercicio 3 ---------------- 

data("Crime") # Um Painel de 90 Unidades

MQ2EFD_reg <- plm(log(crmrte) ~ log(prbarr) + factor(year) | . - log(prbarr) + log(taxpc) + log(mix),
                  data = Crime,
                  model = "fd")

stargazer(MQ2EFD_reg,
          type = "text")
