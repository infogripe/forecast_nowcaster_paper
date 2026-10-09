rm(list = ls())
gc()

if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('purrr')) {install.packages('purrr')};library('purrr')
if(!require ('readr')) {install.packages('readr')};library('readr')
if(!require ('vroom')) {install.packages('vroom')};library('vroom')
if(!require ('INLA')) {install.packages('INLA')};library('INLA')
if(!require ('doParallel')) {install.packages('doParallel')};library('doParallel')
if(!require ('sp')) {install.packages('sp')};library('sp')
if(!require ('sn')) {install.packages('sn')};library('sn')
if(!require ('snow')) {install.packages('snow')};library('snow')

library(nowcaster)

source("R/R_forecast/fct_forecast.R")

temp <- list.files("bases_brutas")

file.name<-as.list(temp)


############################################################################
######################SRAG  WDW 10 #########################################
#############################################################################

lista<-list()
erros <- list()

for (i in file.name) {
  
  boletim<- get(load(paste0("bases_brutas/",i)))
  
  dados_srag <- boletim |>
    mutate(date_report = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) |>
    select(DT_SIN_PRI, date_report, drs) |>
    rename_with(tolower)
  
  name_drs<- unique(boletim$drs)
  
  print(i)
  
  for (j in name_drs) {
    
    print(j)
    
    tryCatch({ if(j=="GRANDE SÃO PAULO"){
      
      
      dados_srag2<- dados_srag %>% filter(drs == j)
      data_corte<- max(dados_srag2$dt_sin_pri)-3
      dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
      
      dado_inla<-get_data_inla(dataset = dados_srag2, K=4, wdw = 8, Dmax = 8)
      
      now <- nowcast_sem_correcao(data.inla=dado_inla, wdw=8, k=4)
      ## Função de Nowcasting
      
    }else if(j=="CAMPINAS" | j=="SÃO JOSÉ DO RIO PRETO"){
      
      dados_srag2<- dados_srag %>% filter(drs == j)
      data_corte<- max(dados_srag2$dt_sin_pri)-3
      dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
      
      dado_inla<-get_data_inla(dataset = dados_srag2, K=4, wdw = 10, Dmax = 10)
      
      now <- nowcast_sem_correcao(data.inla=dado_inla, wdw=10, k=4)
      
    
      
    }else{
      dados_srag2 <- dados_srag %>% filter(drs == j)
      
      dado_inla<-get_data_inla(dataset = dados_srag2, K=4, wdw = 10, Dmax = 10)
      
      now <- nowcast_sem_correcao(data.inla=dado_inla, wdw=10, k=4)
    
    }
      
      nowcast_total<- now %>%
        as.data.frame() %>%
        mutate(base=as.character(i)) %>%
        mutate(DRS=j)

      lista[[length(lista) + 1]] <-  nowcast_total
      
    } , error = function(e) {
      
      # Salva informação do erro
      erros[[length(erros) + 1]] <<- data.frame(
        base = as.character(i),
        DRS = as.character(j),
        erro = conditionMessage(e),
        stringsAsFactors = FALSE
      )
      
      cat("\n*** INLA CRASHOU ***\n")
      cat("Base:", i, "\n")
      cat("DRS:", j, "\n")
      cat("Erro:", conditionMessage(e), "\n")
      cat("Continuando para a próxima DRS...\n\n")
      
    })
    
  }
  
}

big_data<-bind_rows(lista)
big_erros<-bind_rows(erros)


write.csv(big_data, "output/Models/4_weeks/forecasting_DRS_SRAG_com_atraso_4weeks.csv", row.names=FALSE)
write.csv(big_erros, "output/Models/4_weeks/erros_forecasting_DRS_srag_com_atraso_4weeks.csv", row.names = FALSE)

