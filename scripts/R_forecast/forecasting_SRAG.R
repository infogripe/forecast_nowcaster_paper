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

source("scripts/forecast/fct_forecast.R")

temp <- list.files("bases_brutas")

file.name<-as.list(temp)

#file.name<-file.name[56]

lista<-list()
erros <- list()

for (i in file.name) {
  
  boletim<- get(load(paste0("bases_brutas/",i)))
  
  dados_srag <- boletim |>
    mutate(DT_REPORT = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) |>
    select(DT_SIN_PRI, DT_REPORT, idade, drs)
  
  name_drs<- c("CAMPINAS","SÃO JOSÉ DO RIO PRETO")
  
  print(i)
  
  for (j in name_drs) {
    
    print(j)
    
    tryCatch({ if(j=="GRANDE SÃO PAULO"){
      
      
      dados_srag2<- dados_srag %>% filter(drs == j)
      data_corte<- max(dados_srag2$DT_SIN_PRI)-3
      dados_srag2<- dados_srag2 %>% filter(DT_SIN_PRI<=data_corte)
      now <- nowcasting_inla(dataset = dados_srag2,
                             data.by.week = T,
                             Dmax = 08,
                             date_onset = DT_SIN_PRI,
                             date_report = DT_REPORT ,
                             wdw = 08,
                             K = 4)
      ## Função de Nowcasting
      
     }else if(j=="CAMPINAS" |j=="SÃO JOSÉ DO RIO PRETO"){
        
        dados_srag2<- dados_srag %>% filter(drs == j)
        data_corte<- max(dados_srag2$DT_SIN_PRI)-3
        dados_srag2<- dados_srag2 %>% filter(DT_SIN_PRI<=data_corte)
        now <- nowcasting_inla(dataset = dados_srag2,
                               data.by.week = T,
                               Dmax = 10,
                               date_onset = DT_SIN_PRI,
                               date_report = DT_REPORT ,
                               wdw = 08,
                               K = 4)
      
    }else{
      dados_srag2 <- dados_srag %>% filter(drs == j)
      now <- nowcasting_inla(
        dataset=dados_srag2,
        data.by.week = T,
        date_onset = DT_SIN_PRI,
        date_report = DT_REPORT,
        Dmax = 10,
        K = 4,
        wdw=10)
    }
      
      ### objetos para exportação do foreach: ###
      dados_w_total<-now$data %>%
        group_by(dt_event) %>%
        summarise(obs=sum(Y, na.rm = TRUE))
      
      nowcast_total<-now$total %>%
        left_join(dados_w_total, by="dt_event")    %>%
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

write.csv(big_data, "output/Modelos/nowcaster/forecasting_DRS_srag_4weeks_camp.csv", row.names = FALSE)
write.csv(big_erros, "output/Modelos/nowcaster/erros_forecasting_DRS_srag_4weeks_camp.csv", row.names = FALSE)

rm(lista, big_data)

####2 weeks

lista<-list()
erros <- list()

for (i in file.name) {
  
  boletim<- get(load(paste0("bases_brutas/",i)))
  
  dados_srag <- boletim |>
    mutate(DT_REPORT = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) |>
    select(DT_SIN_PRI, DT_REPORT, idade, drs)
  
  name_drs<- unique(boletim$drs)
  
  print(i)
  
  for (j in name_drs) {
    
    print(j)
    
  tryCatch({ if(j=="GRANDE SÃO PAULO"){
      
      
      dados_srag2<- dados_srag %>% filter(drs == j)
      data_corte<- max(dados_srag2$DT_SIN_PRI)-3
      dados_srag2<- dados_srag2 %>% filter(DT_SIN_PRI<=data_corte)
      now <- nowcasting_inla(dataset = dados_srag2,
                             data.by.week = T,
                             Dmax = 08,
                             date_onset = DT_SIN_PRI,
                             date_report = DT_REPORT ,
                             wdw = 08,
                             K = 2)
      ## Função de Nowcasting
      
    }else{
      dados_srag2 <- dados_srag %>% filter(drs == j)
      now <- nowcasting_inla(
        dataset=dados_srag2,
        data.by.week = T,
        date_onset = DT_SIN_PRI,
        date_report = DT_REPORT,
        Dmax = 10,
        K = 2,
        wdw=10)
    }
    
    ### objetos para exportação do foreach: ###
    dados_w_total<-now$data %>%
      group_by(dt_event) %>%
      summarise(obs=sum(Y, na.rm = TRUE))
    
    nowcast_total<-now$total %>%
      left_join(dados_w_total, by="dt_event")    %>%
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


write.csv(big_data, "output/Modelos/nowcaster/forecasting_DRS_srag_2weeks.csv", row.names=FALSE)
write.csv(big_erros, "output/Modelos/nowcaster/erros_forecasting_DRS_srag_2weeks.csv", row.names = FALSE)




