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
install.packages("xtable")
install.packages("plm")
install.packages("sandwich")
install.packages("lmtest")
install.packages("rstatix")
install.packages("estimatr")
install.packages("ggrepel")
install.packages("sjlabelled")
install.packages("ggpubr")
install.packages("scales")


library("haven")
library("tidyverse")
library("xtable")
library("plm")
library("scales")
library("sandwich")
library("ggpubr")
library("sjlabelled")
library("ggrepel")
library("rstatix")
library("lmtest")
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


## Econometria II
## Aula 04 - Variáveis instrumentais em R


#-----------------------
# Exercício 1 
#-----------------------


data("EmplUK")  # painel desbalacenceado


form1 <- log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + lag(log(wage), 2) + lag(log(wage), 3) +
                      diff(log(capital), 2) +  diff(log(capital), 3)
                      
                      
reg1 <- plm(formula = form1,
            data = EmplUK,
            model = "within")

summary(reg1)

reg2 <- plm(formula = form1,
            data = EmplUK,
            model = "pooling")

summary(reg2)



#Visualizar os modelos
stargazer(reg1, reg2,
          type = "text",
          labels = c("reg1", "reg2")
)

#------------------------


form2 <- log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + lag(log(emp), 3) + lag(log(wage), 1) + 
  lag(log(wage), 2) + lag(log(wage), 3) +
  diff(log(capital), 2) +  diff(log(capital), 3)


reg3 <- plm(formula = form2,
            data = EmplUK,
            model = "pooling")


#Visualizar os modelos
stargazer(reg1, reg2, reg3,
          type = "text",
          labels = c("reg1", "reg2", "reg3")
)


#Efeitos Fixos

reg4_within1 <- plm(formula = form2,
                    data = EmplUK,
                    model = "within")

summary(reg4_within1)


#Para extrair o efeito fixo de cada unidade 
fixef(reg4_within1)

form3 <- log(emp) ~ lag(log(emp), 1) + lag(log(emp), 2) + 
  lag(log(wage), 2) + lag(log(wage), 3) +
  diff(log(capital), 2) +  diff(log(capital), 3) + as.factor(year)


reg5_within1 <- plm(formula = form2,
                    data = EmplUK,
                    model = "within")

summary(reg5_within1)

stargazer(reg1, reg4_within1, reg5_within1,
          type = "text",
          labels = c("reg1", "reg2", "reg3")
)

#--- Primeira diferenças 

reg6_pd1 <- plm(formula = form1,
                    data = EmplUK,
                    model = "fd")

summary(reg6_pd1)

# Between

reg7_between <- plm(formula = form1,
                data = EmplUK,
                model = "between")

summary(reg7_between)

#Efeitos Aleatórios
reg8_aleatorios <- plm(formula = form1,
                    data = EmplUK,
                    model = "random")

summary(reg8_aleatorios)

phtest(reg5_within1,reg8_aleatorios) # caso rejeitamos HO: modelos de efeitos fixos ->
pFtest(reg5_within1,reg8_aleatorios) # caso rejeitamos HO: EF ou EA ->

rm(list = ls(all.names = TRUE))

#--------------------
# Exercicio 2
#--------------------

#Investigar os efeitos dos subsídios concedidos pelo governo para o treinamento de 
# trabalhadores sobre a taxa de perda da produção das firmas.

jtrain <- read_csv("C:/Users/limam/Documents/Programação/R/Econometria R/Monitoria/Dados/jtrain.csv")

freq_anos <- table(jtrain$year)
print(freq_anos)

# verificar se o painel está balanceado

painel_balanceado <- ispbalanced(jtrain)
print(painel_balanceado)

# Algumas estatisticas descritivas

jtrain_somados <- jtrain %>%
                  group_by(year)%>%
                  summarize(total_scrap = sum(scrap, na.rm = TRUE))

print(jtrain_somados)

jtrain_subamostra <- jtrain %>% select(lscrap, union, d88, d89, grant, grant_1)
summary(jtrain_subamostra)

graph1 <- ggplot(jtrain[jtrain$fcode > 419459,],
                        aes(x= year,
                            y = sales,
                            group = fcode,
                            color = factor(fcode))) +
                geom_line() + 
                labs(title = "Gráfico de Linha para Vendas (Sales) Por código de Unidade (fcode > 4194590)",
                     x = "Ano",
                     y = "Vendas") +
                theme_minimal()

print(graph1)
                
# ------------------------
# POLs
# -----------------------
reg1 <- lm(lscrap ~ grant + grant_1 + union + d88,
           data = jtrain)
summary(reg1)

reg2 <- plm(lscrap ~ grant + grant_1 + union + d88,
           data = jtrain)
summary(reg2)

stargazer(reg1, reg1,
          type = "text",
          column.labels = c("OLS", "POLS")
)

#Sindicalização é significativa para explicar a taxa de perda

# Efeito aleatorio

reg3_aleatorios <- plm(lscrap ~ grant + grant_1 + union + d88, d89,
            data = jtrain,
            model = "random",
            random.method = "amemiya")
summary(reg3_aleatorios)


#Efeitos Fixos 
reg4_fixos <- plm(lscrap ~ grant + grant_1 + union + d88, d89,
                       data = jtrain,
                       model = "within")
summary(reg4_fixos)

phtest(reg3_aleatorios,reg4_fixos)

# Primeiras diferenças

reg5_pd1 <- plm(lscrap ~ grant + grant_1 + union + d88, d89,
                  data = jtrain,
                  model = "fd")
summary(reg5_pd1)

stargazer(reg1, reg1, reg3_aleatorios, reg4_fixos,reg5_pd1,
          type = "text")

rm(list = ls(all.names = TRUE))


# -------------------------------
# Exercicio 3 
# -----------------------------

# Análise busca verificar se o aumento do salário mínimo reduxio o emprego.

card_krueger <- read_csv("C:/Users/limam/Documents/Programação/R/Econometria R/Monitoria/Dados/card_krueger.csv")

card_krueger <- card_krueger %>% mutate(emptot = emppt + empft)

# estatisticas descritivas 

card_krueger %>% select(chain, state) %>%
                  table() %>%
                prop.table(margin = 2) %>%
                apply(MARGIN = 2,
                      FUN = scales::percent_format(accuracy = 0.1)) %>%
  noquote


# Primeira Onda(Média pré tratamento)

card_krueger %>% filter(observation == "February 1992") %>%
                 group_by(state) %>%
                 summarise(emptot = mean(emptot, na.rm = TRUE),
                           wage_st = mean(wage_st, na.rm = TRUE),
                           hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
                   pivot_longer(cols= -state,
                                names_to = 'variable') %>%
                   pivot_wider(names_from = -state,
                                values_from = value)

# segunda Onda  ()

card_krueger %>% filter(observation == "November 1992") %>%
                group_by(state) %>%
                summarise(emptot = mean(emptot, na.rm = TRUE),
                          wage_st = mean(wage_st, na.rm = TRUE),
                          hrsopen = mean(hrsopen, na.rm = TRUE)) %>%
                pivot_longer(cols= -state,
                             names_to = 'variable') %>%
                pivot_wider(names_from = -state,
                            values_from = value)

# 1 diferrença

dif_1 <- card_krueger %>% group_by(observation, state) %>%
                          summarise(emptot = mean(emptot, na.rm = TRUE))


NJ_antes <- dif_[1,3] # Grupo Tratado antes 
PA_antes <- dif_[2,3]  # Grupo Controle antes
NJ_depois <- dif_[3,3]  # Grupo Tratado depois  
PA_depois <- dif_[4,3]  # Grupo Controle depois

#ATT

#Calcular a diferença entre a diferença de novembro e fevereiro dentro dos grupos 
ATT1 <- (NJ_depois - NJ_antes) - (PA_depois - PA_antes)
print(ATT1)

#Calcular a diferença entre a diferença de novembro e fevereiro dentro dos grupos 
ATT2 <- (NJ_depois - PA_depois) - (NJ_antes - PA_antes)
print(ATT2)

#Did

# Criar duas variáveis ficticias 
card_krueger <- mutate(card_krueger,
                       time = ifelse(observation = "November 1992", 1, 0),
                       treated = ifelse(state == "New Jersey", 1, 0)
                       )

did_model <- lm(emptot ~ time + treated + time:treated,
                data = card_krueger)

summary(did_model)

#Efeitos Fixos

painel <- pdata.frame(card_krueger,"sheet")

did_within <- plm(emptot ~ time + treated + time:treated,
                data = painel,
                model = "within")
summary(did_within)


#Cluster --> erro padrão
coeftest(did_within,
         vcov = function(x) vcovHC(x,
                                   cluster = "group",
                                   type = "HC1")
        )

