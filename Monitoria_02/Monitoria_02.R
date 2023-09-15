#Econometria 2 - Aula 2 Aleatórização ---------- 25.08.2023
#
#Instalação dos Pacotes

install.packages("tidyverse")
install.packages("haven")
install.packages("stats")

library(tidyverse)
library(haven)
library(stats)


# Criação de uma que ler um arquivo no Github ----------------
read_data <- function(df)
{
full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", df, sep = "")  

df <- read_dta(full_path)
return(df)
}

yule <- read_data("yule.dta")

# Indico vocês a testarem em casa e fazer aquela análise primária nos dados

yulereg <- read_data("yule.dta") %>%
  lm(paup ~ outrelief + old + pop, .)
summary(yulereg)

#Uma mudança de 10pp em outrelief está associada a um aumento em 0.75209 em misária


#Poderia haver algo despercebido determinates da pobreza.


gap <- function()
  
     {sdo <- tibble(
        y1 = c(7,5,5,7,4,10,1,5,3,9),
        y0 = c(1,6,1,8,2,1,10,6,7,8),
        random = rnorm(10)
      ) %>%
       arrange(random) %>%
      mutate(
        d = c(rep(1,5), rep(0,5)),
        y = d *y1 + (1 - d) * y0
      )%>%
      pull(y)

 sdo <- mean(sdo[1:5] -sdo[6:10])
 return(sdo)
}

sim = replicate(10000,gap())
mean(sim)

#------------


  
ri <- read_data("ri.dta") %>% mutate(id = c(1:8))
view(ri)

ri <- ri %>% mutate_at(vars(y0, y1), as.numeric)
str(ri)

# Função mutate troca os valores por 0 quando NA
ri <- ri %>% mutate (y0 = if_else(is.na(y0), 0, y0))
ri <- ri %>% mutate (y1 = if_else(is.na(y1), 0, y1))  
  

# Para cada unidade há apenas um resultado potencial.

mediay1 <- mean(ri$y1)
mediay0 <- mean(ri$y0)


simples_tstatistic <- mediay1 - mediay0


# Calcular P-valor 
#-------------------
combo <- ri %$% as_tibble(t(combn(id, 4))) %>%
                transmute(treated1 = V1,
                          treated2 = V2,
                          treated3 = V3,
                          treated4 = V4) %>%
                mutate(permutation = 1:70) %>%
                crossing(., ri) %>%
                arrange(permutation, name) %>%
                mutate(d = case_when(id == treated1 | id == treated2 |
                                     id == treated3 | id == treated4 ~ 1,
                                     TRUE ~ 0)
         )

te1 <- combo %>%
      group_by(permutation) %>%
      filter(d == 1) %>%
      summarize(te1 = mean(y, na.rm = TRUE))

te0 <- combo %>%
      group_by(permutation) %>%
      filter(d == 0) %>%
      summarize(te0 = mean(y, na.rm = TRUE))
#----------------

n <- nrow(inner_join(te1,
                     te0,
                     by = "permutation"))
view(n)

# ------------

p_value <- inner_join(te1, te0, by = "permutation") %>%                                            
            mutate(ate = te1 - te0) %>%
  # Calculo do Valor absoluto da diferença mais simples na classificação da média
            select(permutation, ate) %>%
            arrange(desc(ate)) %>%
            mutate(rank = 1:nrow(.))%>%
            filter(permutation == 1) %>%
            pull(rank)

#Todas essas estatísticas de teste foram diferenças nos resultados por status de tratamento.
#podemos estar interessados em uma estatística de teste que possa detectar diferenças nas 
#nas  Distribuição de tratamento e controle.

# Estatística de Teste Kolmogorov-Smirnov

tb <- tibble(
  d = c(rep(0,20), rep(1,20)),
  y = c(0.22, -0.87, 2.39, -1.79, 0.37, -1.54, 1.28, -0.31, 
        -0.74, 1.72, 0.38, -0.17, -0.62, -1.10, 0.30, 0.15, 
         2.30, 0.19, -0.50, -0.90, -5.13, -2.19, 2.43, -3.83,
          0.5, -3.25, 4.32, 1.63, 5.18, -0.43, 7.11, 4.87, -3.10,
        -5.81, 3.76, 6.31, 2.58, 0.07, 5.76, 3.50)
  
)

kdensity_d1 <- tb %>% filter(d == 1)%>%
pull(y)
kdensity_d1 <- density(kdensity_d1)

kdensity_d0 <- tb %>% filter(d == 0)%>%
pull(y)
kdensity_d0 <- density(kdensity_d0)



kdensity_d0 <- tibble(x = kdensity_d0$x,
                      y = kdensity_d0$y,
                      d = 0)

kdensity_d1 <- tibble(x = kdensity_d1$x,
                      y = kdensity_d1$y,
                      d = 1)


kdensity <- full_join(kdensity_d1, kdensity_d0)
kdensity$d <- as_factor(kdensity$d)

ggplot(kdensity) +
geom_point(size = 0.3, aes(x,y, color = d)) +
xlim(-7,8)+
labs(title = " Teste Kolmogorov-Smirnov")+
scale_color_discrete(labels = c("Controle", "Tratamento"))

#------------------------
# THORTON
#------------------------
library(tidyverse)
library(haven)

read_data <- function(df)
  
  {
    full_path <- paste("https://github.com/scunning1975/mixtape/raw/master/", df, sep = "")  
    
    df <- read_dta(full_path)
    return(df)
  }  

hiv <- read_data("thornton_hiv.dta")

# Criando Permutações

tb <- NULL
permuteHIV <- function(df, random = TRUE){
tb <- df
# Nº de Tratados na Base
n_treated <- ceiling(nrow(tb/2))
n_control <- nrow(tb) - n_treated

if(random == TRUE){
tb <- tb %>%
 sample_frac(1) %>%
 mutate(any = c(rep(1, n_treated), rep(0, n_control)))
}

te1 <- tb  %>%
  filter(any == 1) %>%
  pull(got) %>%
  mean(na.rm = TRUE)


te0 <- tb  %>% 
  filter(any == 0) %>%
  pull(got) %>%
  mean(na.rm = TRUE)


  ate <- te1 - te0
  return(ate)
}

permuteHIV(hiv, random = FALSE)
iterations <- 1000
permutation <- tibble(
  iteration = c(seq(iterations)),
  ate = as.numeric(
    c(permuteHIV(hiv, random = FALSE), map(seq(iterations-1),~ permuteHIV(hiv, random = TRUE)))
   )
  )

# Calculando P-Valor

permutation <- permutation %>%
  arrange(-ate) %>% 
  mutate(rank = seq(iterations))

p_value <- permutation %>% 
  filter(iterations == 1) %>% 
  pull(rank)/iterations

print(permutation)

# Abrir a base de permutação: Mostra o experimento de Thornton sob nulo
# Produx valores p altamente significativos

#A simples diferença nos resultados médios foi igual a soma da média efeito do tratamento,
#ou o viés de seleção, e viés do efeito heterogeneo ponderado do tratamento.

#Assim, a simples diferença na média do estimador de resultados é tendencioso.
# A não ser que o segundo e terceiro termos sejam zerados.

#uma situação em eles zeram é sob independencia de tratamento, que é quando o tratamento
# o tratamento foi atribuido independentemente dos resultados potenciais. Quando é que independecia ocorre?
#A situação mais comumente enfretada é sob randomização física do tratamento para as unidades.


install.packages("randomizr")
library(randomizr)

# defina o tamanho da amostra 
n <- 100

# crie um dataframe simulado com duas variáveis: 'grupo' e 'resultado'
set.seed(123)
dados <- data.frame(grupo = factor(c(rep("Controle", n/2),
                                     rep("Tratamento",n/2))),
                    resultado = c(rnorm(n/2, mean = 10, sd = 2),
                                  rnorm(n/2, mean = 12, sd = 2.5))
                    )


#Use a função simple_ra() do pacote 'randomizr' para fazer a aleatorização

dados$simple <- simple_ra(N=100, num_arms = 2)

#calcule o ATE

ate <- with(dados, mean(resultados[grupo == "Tratamento"]) - mean(resultados[grupo == "Controle"]))
cat("ATE estimado:", ate, "\n")


