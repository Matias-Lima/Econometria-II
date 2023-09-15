install.packages("dbplyr")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("readr")
install.packages("stargazer")
install.packages("tibble")

library(dbplyr)
library(tidyverse)
library(corrplot)
library(readr)
library(stargazer)
library(tibble)

#-------------------
#Exercício
#-------------------

base_mtcars <- mtcars
names(base_mtcars)

view(mtcars)

mean(mtcars$mpg)
sd(mtcars$mpg)
median(mtcars$mpg)

#puxar tudo de uma vezsó
summary(mtcars)

#criar um vetor
vetor_infor <- c(mean(base_mtcars$mpg),
                 sd(base_mtcars$mpg),
                 median(base_mtcars$mpg))

print(vetor_infor)            


cov(mtcars$mpg, mtcars$wt)
cor(mtcars$mpg, mtcars$wt)
cor(mtcars)

corr_mtcars <- base_mtcars %>%
  cor() %>%
  corrplot(method="circle")

#regressão

reg1 <- lm(mpg~hp+drat+wt,
           data=base_mtcars)

summary(reg1)

cor2 <- cor(base_mtcars) #para todas as variáveis
corr_mtcars_SEMPIPE <- corrplot(cor2,
                                method = "circle",
                                data = base_mtcars)

summary(cor2)


#-------------------
rm(list = ls())
car_sales <- read.csv("C:\\Users\\limam\\Downloads\\Car_sales.csv")

#analise dos dados
names(car_sales)
str(car_sales) #verificar se tem variáveis strings

summary(car_sales)
view(car_sales)

corr_mtcars <- car_sales %>%
  select(Sales_in_thousands,X__year_resale_value,
         Price_in_thousands, Engine_size, Horsepower,
         Wheelbase, Width, Length, Curb_weight, Fuel_efficiency,
         Fuel_capacity, Power_perf_factor) %>%
  cor(use = "complete.obs") %>%
  corrplot(method = "circle")

#Regressão

reg1 <- lm(Sales_in_thousands ~ Price_in_thousands,
           data = car_sales)
summary(reg1)

reg2 <- lm(Sales_in_thousands ~ Price_in_thousands + Engine_size,
           data = car_sales)
summary(reg2)

reg3 <- lm(Sales_in_thousands ~ Price_in_thousands + Engine_size +
             Horsepower + Curb_weight + I(Price_in_thousands^2),
           data = car_sales)
summary(reg3)

reg4 <- lm(Sales_in_thousands ~ Price_in_thousands + Engine_size +
             Horsepower + Curb_weight + I(Price_in_thousands^2) +
             as.factor(Vehicle_type) + as.factor(Manufacturer),
           data = car_sales)

#as factor é usado pra adicionar uma variável string

summary(reg4)
names(car_sales)
