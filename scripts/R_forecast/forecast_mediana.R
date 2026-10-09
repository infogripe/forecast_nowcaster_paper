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

###BAse 2 deu ruim com presidente prudente em SRAG


df<-read.csv("output/Models/2_weeks/bases_individuais/forecasting_DRS_srag_2weeks.csv")

df<- df %>%
  mutate(dt_event=ymd(dt_event))

file.name<-unique(as.list(df$base))

lista<-list()
erros <- list()

for (i in file.name) {
  
  data_b <- ymd(substr(i, 14, 24))
  
  boletim <- df %>%
           filter(base==i)
  
  name_drs <- unique(boletim$DRS[
    !is.na(boletim$DRS) & boletim$DRS != "ESTADO"
  ])
  
  print(i)
  
  for (j in name_drs) {
    
    print(j)
    
    tryCatch({
      
      data.inla<- boletim  %>%
        filter(DRS == j) %>%
        filter(dt_event<=max(dt_event)-14) %>%
        mutate(date_onset=ymd(dt_event))%>%
        mutate(Median=round(Median))%>%
        select (date_onset, Median) %>%
        mutate(Time=seq(1:n())) 
      
      ultima_data <- max(data.inla$date_onset)
      
      data.inla <- data.inla %>%
        add_row(
          date_onset = c(ultima_data + 7, ultima_data+14, ultima_data+21, ultima_data+28),
          Median = NA,
          Time = max(data.inla$Time) + 1:4
        ) %>%
        rename(Y=Median)
      
      
      now_s <- nowcast_mediana(
        data.inla  =  data.inla
      )
      
      for_srag <- now_s %>% 
        as.data.frame() %>%
        mutate(
          base = as.character(i),
          DRS = as.character(j)
        )
      
      lista[[length(lista) + 1]] <- for_srag
      
    }, error = function(e) {
      
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

write.csv(big_data, "output/Models/Mediana/forecasting_mediana_DRS_srag_4weeks.csv", row.names = FALSE)
write.csv(big_erros, "output/Models/Mediana/erros_forecasting_mediana_DRS_srag_4weeks.csv", row.names = FALSE)



