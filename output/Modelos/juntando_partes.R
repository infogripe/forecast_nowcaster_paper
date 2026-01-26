
library(dplyr)
library(tidyverse)
library(readr)

##Juntando dados com atraso

filePaths <- list.files("com_atraso/", pattern = ".csv", full.names = T)


teste<-filePaths %>% 
  map_dfr(~read_csv(.x, col_types = cols(.default = col_guess(), widw = col_character())))

table(teste$widw)
table(teste$tipo)


####juntando dados com corte de três semanas
filePaths2 <- list.files("corte_3_semanas/", pattern = ".csv", full.names = T)

corte<-filePaths2 %>% 
  map_dfr(~read_csv(.x, col_types = cols(.default = col_guess(), widw = col_character())))

table(corte$widw)

table(corte$tipo)

str(mediana)


####Lendo a mediana

mediana<-read.csv("Mediana/forecasting_mediana_srag_covid.csv")

mediana<- mediana %>% mutate(date_onset=ymd(date_onset),
                             base=ymd(base))

###Tranformando números em nomes das DRS

nomes_regioes<-sort(unique(corte$DRS))

mediana$DRS2<-mediana$DRS
mediana$DRS <- factor(mediana$DRS, levels = 1:17, labels = nomes_regioes)

junt<- bind_rows(list(mediana, corte, teste))


###Arrumando a variavel wdw

table(junt$widw)


junt<- junt %>% 
  mutate(widow=case_when(
    widw=="10" | widw== "8 ou 10" ~ "10",
    widw=="30"~"30")
  ) %>%
  select(!c(DRS2, widw))


write.csv(junt, "previsoes_controle.csv", row.names = FALSE)
