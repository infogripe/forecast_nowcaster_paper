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

n_cores <- detectCores()
cl <- makeCluster(n_cores)
registerDoParallel(cl)


library(nowcaster)

source("scripts/fct_forecast.R")

###BAse 2 deu ruim com presidente prudente em SRAG


temp = list.files(pattern="forecast")

file.name<-as.list(temp)

datalist<-list()

for (j in file.name) {
  
  print(j)
  
  data_b<-ymd(substr(j,14,24))
  
  boletim<-read.csv(j)
  
  agregado <- unique(boletim$DRS)[-1]
  agregado <- sort(agregado)
  
  comb <- function(x, y) {
    lapply(seq_along(x),
           function(i) rbind(x[[i]], y[[i]]))
  }
  print("Started forecasting SRAG")
  saida <- foreach (i=1:length(agregado),
                    .packages=c("tidyverse", "data.table", "scales", "purrr",
                                "readr", "vroom", "INLA", "snow", "nowcaster"), .combine="comb") %dopar% {
  
  boletim2<-boletim %>%  filter (DRS==agregado[i])
  
  
  now_s <- nowcast_mediana(data=boletim2, agente="srag")
  now_c <- nowcast_mediana(data=boletim2, agente="covid")
  
  
  for_srag <-now_s %>% as.data.frame() %>%
    mutate(base=data_b,
           DRS=i,
           widw= "8 ou 10",
           virus="srag",
           tipo="mediana") 

  for_covid <-now_c %>% as.data.frame() %>%
    mutate(base=data_b,
           DRS=i,
           widw= "8 ou 10",
           virus="covid",
           tipo="mediana")  
  
  junt<-bind_rows(for_srag, for_covid)
  
  list(junt)  }
  
  datalist[[j]]<-saida[1]
  
}

big_data<-bind_rows(datalist)
  
write.csv(big_data, paste("C:/Users/tatty/Documents/GitHub/forecast_nowcaster/output/Mediana/forecasting_mediana_srag_wdw_10_",data_b,".csv"), row.names = FALSE)

  


