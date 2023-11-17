install.packages("haven")
install.packages("tidyverse")
install.packages("readxl")
install.packages("stargazer")
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
# Para usar o pipes
# é usado principalmente para encadear operações em objetos de dados 
# é usado para acessar variaveis dentro de objetos de dados. 
library("ggplot2")
library("sjmisc")
library("readr")
library("tableone")

# Exercício 1

# Aqui colocar o PATH do arquivo localizado no seu computador 
titanic = read_excel("C:\\Users\\joao.mendonca\\Downloads\\titanic.xls")

titanic = titanic %>% mutate(d = case_when(class == "1st class" ~ 1,
                                           TRUE ~ 0),
                             sobrevivente = case_when(survived == "yes" ~ 1,
                                                      TRUE ~ 0),
                             sexo = case_when(sex == "man" ~ 1,
                                              TRUE ~ 0),
                             faixaetaria = case_when(age == "adults" ~ 1,
                                                     TRUE ~ 0))

summary(titanic$d)

ey1 = titanic %>% filter(d == 1) %>%
  pull(sobrevivente) %>%
  mean

ey0 = titanic %>% filter(d == 0) %>%
  pull(sobrevivente) %>%
  mean

sdo = ey1 - ey0
print(sdo)

# Estratificar os dados em quatros grupos:

titanic = titanic %>% mutate(s = case_when(sexo == 0 & faixaetaria == 1 ~ 1,
                                           sexo == 0 & faixaetaria == 0 ~ 2,
                                           sexo == 1 & faixaetaria == 1 ~ 3,
                                           sexo == 1 & faixaetaria == 0 ~ 4,
                                           TRUE ~ 0))

a = filter(titanic, s == 1 & d == 1)
ey11 = mean(a$sobrevivente)

ey11 = titanic %>% filter(s == 1 & d == 1) %$%
  mean(sobrevivente)

a = filter(titanic, s == 1 & d == 0)
ey10 = mean(a$sobrevivente)

a = filter(titanic, s == 2 & d == 1)
ey21 = mean(a$sobrevivente)

a = filter(titanic, s == 2 & d == 0)
ey20 = mean(a$sobrevivente)

a = filter(titanic, s == 3 & d == 1)
ey31 = mean(a$sobrevivente)

a = filter(titanic, s == 3 & d == 0)
ey30 = mean(a$sobrevivente)

a = filter(titanic, s == 4 & d == 1)
ey41 = mean(a$sobrevivente)

a = filter(titanic, s == 4 & d == 0)
ey40 = mean(a$sobrevivente)

diff1 = ey11 - ey10
diff2 = ey21 - ey20
diff3 = ey31 - ey30
diff4 = ey41 - ey40

# calcular o número de pessoas que não estava na primeira classe
obs = nrow(filter(titanic, d==0))

a = filter(titanic, s == 1 & d == 0)
wt1 = nrow(a)/obs
print(wt1)

wt1 = titanic %>% filter(s == 1 & d == 0) %$% 
  nrow(.)/obs
print(wt1)

wt1 = titanic %>% filter(s == 1 & d == 0) %>% 
  nrow()/obs
print(wt1)

a = filter(titanic, s == 2 & d == 0)
wt2 = nrow(a)/obs

a = filter(titanic, s == 3 & d == 0)
wt3 = nrow(a)/obs

a = filter(titanic, s == 4 & d == 0)
wt4 = nrow(a)/obs

# Ponderação correta, antes havia vários ruídos
wate = diff1*wt1 + diff2*wt2 + diff3*wt3 + diff4*wt4

stargazer(wate, 
          sdo,
          type = "text")
# Uma vez que condicionamos os fatores de confusão ao genero e faixa etária
# tem uma possibilidade de sobrevivencia muito menor associada a ele
# (mas ainda grande!)
# Isso nos daria estimativas de todos os contrafactuais dos quais poderiamos simplementes tirar a 
# a média das diferenças


#-ex 2

training_example = read_excel("C:\\Users\\joao.mendonca\\Downloads\\training_example.xls")
training_example = slice(training_example, 1:20)
# OR
training_example <- read_excel("C:\\Users\\limam\\Downloads\\training_example.xls") %>% 
  slice(1:20)


training_slice <- training_example %>% select(unit_treat, age_treat,earnings_treat)
#OR
training_slice = select(training_example, unit_treat, age_treat, earnings_treat)
training_slice = na.omit(training_slice)

media_treinantes = mean(training_slice$age_treat) # 24 anos
media_naotreinantes = mean(training_example$age_control)  # 31 anos

#Assim, as pessoas do grupo de controle são mais velhas
training_slice_earnings_treat = as.data.frame(t(training_slice$earnings_treat))

media_salariotreinantes = mean(as.numeric(training_slice_earnings_treat))
media_salarionaotreinantes = mean(training_example$earnings_control)

ggplot(training_example,
       aes(x=age_treat)) +
  stat_bin(bins = 10,
           na.rm = TRUE) +
  labs(title = "Idade dos tratados",
       x = "Idade",
       y = "Contagem")

ggplot(training_example,
       aes(x=age_control)) +
  geom_histogram(bins = 10,
           na.rm = TRUE) +
  labs(title = "Idade dos controles",
       x = "Idade",
       y = "Contagem")

training_slice_matching = select(training_example,
                                 unit_matched, age_matched, earnings_matched)
training_slice_matching = na.omit(training_slice_matching)

media_matching = mean(training_slice_matching$age_matched)

ggplot(training_example,
       aes(x=age_matched)) +
  geom_histogram(bins = 10,
                 na.rm = TRUE) +
  labs(title = "Idade no matching",
       x = "Idade",
       y = "Contagem")

media_salariomatching = mean(training_slice_matching$earnings_matched)









