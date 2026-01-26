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




#load("bases_brutas/boletim_2024_10_02.RData")

temp = list.files(pattern="boletim")

file.name<-as.list(temp)

################################################################################################################
#################### COVID WDW 10 ##############################################################################
################################################################################################################

datalist<-list()

#j<-file.name[[1]]
#rm(j)

for (j in file.name) {
  
  data_b<-ymd(substr(j,9,18))
  
#   data_b<-dmy(substr(j,10,19))
  
  ## Loading a base do dia
  boletim<-get(load(j))
  
  ###Dados para o forecasting por CIR
  dados_srag <- boletim |>
    filter(classi == 'SARS COV 2') |>
    mutate(date_report = pmax(DT_DIGITA, DT_PCR, na.rm = TRUE)) |>
    select(DT_SIN_PRI, date_report , drs) |>
    rename_with(tolower)
  
  #dados_srag <- boletim |>
  #  mutate(date_report = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) |>
  #  select(DT_SIN_PRI, date_report , drs) |>
  #  rename_with(tolower)
  
  agregado <- unique(boletim$drs)
  agregado <- sort(agregado)
  rm(boletim);gc()
  
  comb <- function(x, y) {
    lapply(seq_along(x),
           function(i) rbind(x[[i]], y[[i]]))
  }
  print("Started forecasting SRAG")
  saida <- foreach (i=1:length(agregado),
                    .packages=c("tidyverse", "data.table", "scales", "purrr",
                                "readr", "vroom", "INLA", "snow", "nowcaster"), .combine="comb") %dopar% {
                                  
                                  if(agregado[i]=="CAMPINAS" | agregado[i]=="SÃO JOSÉ DO RIO PRETO"){
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)-3
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=10, k=1)
                                    
                                  }else if(agregado[i]=="GRANDE SÃO PAULO"){
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)-3
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=8, k=1)
                                    
                                  }else{
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    ## Função de Nowcasting
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=10, k=1)
                                    
                                  }
                                  
                                  
                                  
                                  for_srag <-now %>% as.data.frame() %>%
                                    mutate(base=data_b,
                                           DRS=agregado[i],
                                           #   widw="8 ou 10",
                                           widw= "10",
                                           virus="covid",
                                           tipo="com_atraso") 
                                  
                                  
                                  list(for_srag)  }    
  
  
  datalist[[j]]<-saida[1]
  
  
}


big_data<-bind_rows(datalist)

write.csv(big_data, "C:/Users/tatty/Documents/GitHub/forecast_nowcaster/output/com_atraso/forecasting_com_atraso_covid_wdw_10_2.csv", row.names = FALSE)

rm(big_data, datalist, dados_srag, sivep_bruto, saida, data_b, j)




################################################################################################################
#################### COVID WDW 30 ##############################################################################
################################################################################################################


datalist<-list()

#j<-file.name[[1]]
#rm(j)

for (j in file.name) {
  
  data_b<-ymd(substr(j,9,18))
  
 # data_b<-dmy(substr(j,10,19))
  
  ## Loading a base do dia
  boletim<-get(load(j))

###Dados para o forecasting por CIR
dados_srag <- boletim |>
  filter(classi == 'SARS COV 2') |>
  mutate(date_report = pmax(DT_DIGITA, DT_PCR, na.rm = TRUE)) |>
  select(DT_SIN_PRI, date_report , drs) |>
  rename_with(tolower)

#dados_srag <- boletim |>
#  mutate(date_report = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) |>
#  select(DT_SIN_PRI, date_report , drs) |>
#  rename_with(tolower)

agregado <- unique(boletim$drs)
agregado <- sort(agregado)
rm(boletim);gc()

comb <- function(x, y) {
  lapply(seq_along(x),
         function(i) rbind(x[[i]], y[[i]]))
}
print("Started forecasting SRAG")
saida <- foreach (i=1:length(agregado),
                  .packages=c("tidyverse", "data.table", "scales", "purrr",
                              "readr", "vroom", "INLA", "snow", "nowcaster"), .combine="comb") %dopar% {
                                
                                if(agregado[i]=="CAMPINAS" | agregado[i]=="SÃO JOSÉ DO RIO PRETO"){
                                  dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                   data_corte<- max(dados_srag2$dt_sin_pri)-3
                                   dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                   
                                  now <- nowcast_sem_correcao(data=dados_srag2, wdw=30, k=1)
                                  
                                }else if(agregado[i]=="GRANDE SÃO PAULO"){
                                  dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)-3
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                  
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=30, k=1)
                                    
                                }else{
                                  dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                  data_corte<- max(dados_srag2$dt_sin_pri)
                                  dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                  ## Função de Nowcasting
                                  now <- nowcast_sem_correcao(data=dados_srag2, wdw=30, k=1)
                                  
                                }

              
                                
for_srag <-now %>% as.data.frame() %>%
        mutate(base=data_b,
        DRS=agregado[i],
     #   widw="8 ou 10",
        widw= "30",
        virus="covid",
        tipo="com_atraso") 

                                
list(for_srag)  }    


datalist[[j]]<-saida[1]


}


big_data<-bind_rows(datalist)

write.csv(big_data, "C:/Users/tatty/Documents/GitHub/forecast_nowcaster/output/com_atraso/forecasting_com_atraso_covid_wdw_30_2.csv", row.names = FALSE)

rm(big_data, datalist, dados_srag, sivep_bruto, saida, data_b, j)

############################################################################
######################SRAG  WDW 10 #########################################
#############################################################################

datalist<-list()


for (j in file.name) {
  
  data_b<-ymd(substr(j,9,18))
  
 #  data_b<-dmy(substr(j,10,19))
  
  ## Loading a base do dia
  boletim<-get(load(j))
  
  ###Dados para o forecasting por CIR
  #dados_srag <- boletim |>
  #  filter(classi == 'SARS COV 2') |>
  #  mutate(date_report = pmax(DT_DIGITA, DT_PCR, na.rm = TRUE)) |>
  #  select(DT_SIN_PRI, date_report , drs) |>
  #  rename_with(tolower)
  
  dados_srag <- boletim |>
    mutate(date_report = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) |>
    select(DT_SIN_PRI, date_report , drs) |>
    rename_with(tolower)
  
  agregado <- unique(boletim$drs)
  agregado <- sort(agregado)
  rm(boletim);gc()
  
  comb <- function(x, y) {
    lapply(seq_along(x),
           function(i) rbind(x[[i]], y[[i]]))
  }
  print("Started forecasting SRAG")
  saida <- foreach (i=1:length(agregado),
                    .packages=c("tidyverse", "data.table", "scales", "purrr",
                                "readr", "vroom", "INLA", "snow", "nowcaster"), .combine="comb") %dopar% {
                                  
                                  if(agregado[i]=="CAMPINAS" | agregado[i]=="SÃO JOSÉ DO RIO PRETO"){
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)-3
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=10, k=1)
                                    
                                  }else if(agregado[i]=="GRANDE SÃO PAULO"){
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)-3
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=8, k=1)
                                    
                                  }else{
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    ## Função de Nowcasting
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=10, k=1)
                                    
                                  }
                                  
                                  
                                  
                                  for_srag <-now %>% as.data.frame() %>%
                                    mutate(base=data_b,
                                           DRS=agregado[i],
                                           #   widw="8 ou 10",
                                           widw= "8 ou 10",
                                           virus="srag",
                                           tipo="com_atraso") 
                                  
                                  
                                  list(for_srag)  }    
  
  
  datalist[[j]]<-saida[1]
  
  
}


big_data<-bind_rows(datalist)

write.csv(big_data, "C:/Users/tatty/Documents/GitHub/forecast_nowcaster/output/com_atraso/forecasting_com_atraso_srag_wdw_10_2.csv", row.names = FALSE)


rm(big_data, datalist, dados_srag, sivep_bruto, saida, data_b, j)

####################################################################################
############### SRAG WDW 30 #########################################################
########################################################################################


datalist<-list()


for (j in file.name) {
  
  data_b<-ymd(substr(j,9,18))
  
 #  data_b<-dmy(substr(j,10,19))
  
  ## Loading a base do dia
  boletim<-get(load(j))
  
  ###Dados para o forecasting por CIR
  #dados_srag <- boletim |>
  #  filter(classi == 'SARS COV 2') |>
  #  mutate(date_report = pmax(DT_DIGITA, DT_PCR, na.rm = TRUE)) |>
  #  select(DT_SIN_PRI, date_report , drs) |>
  #  rename_with(tolower)
  
  dados_srag <- boletim |>
    mutate(date_report = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) |>
    select(DT_SIN_PRI, date_report , drs) |>
    rename_with(tolower)
  
  agregado <- unique(boletim$drs)
  agregado <- sort(agregado)
  rm(boletim);gc()
  
  comb <- function(x, y) {
    lapply(seq_along(x),
           function(i) rbind(x[[i]], y[[i]]))
  }
  print("Started forecasting SRAG")
  saida <- foreach (i=1:length(agregado),
                    .packages=c("tidyverse", "data.table", "scales", "purrr",
                                "readr", "vroom", "INLA", "snow", "nowcaster"), .combine="comb") %dopar% {
                                  
                                  if(agregado[i]=="CAMPINAS" | agregado[i]=="SÃO JOSÉ DO RIO PRETO"){
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)-3
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=30, k=1)
                                    
                                  }else if(agregado[i]=="GRANDE SÃO PAULO"){
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)-3
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=30, k=1)
                                    
                                  }else{
                                    dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                                    data_corte<- max(dados_srag2$dt_sin_pri)
                                    dados_srag2<- dados_srag2 %>% filter(dt_sin_pri<=data_corte)
                                    ## Função de Nowcasting
                                    now <- nowcast_sem_correcao(data=dados_srag2, wdw=30, k=1)
                                    
                                  }
                                  
                                  
                                  
                                  for_srag <-now %>% as.data.frame() %>%
                                    mutate(base=data_b,
                                           DRS=agregado[i],
                                           #   widw="8 ou 10",
                                           widw= "30",
                                           virus="srag",
                                           tipo="com_atraso") 
                                  
                                  
                                  list(for_srag)  }    
  
  
  datalist[[j]]<-saida[1]
  
  
}


big_data<-bind_rows(datalist)

write.csv(big_data, "C:/Users/tatty/Documents/GitHub/forecast_nowcaster/output/Mediana/forecasting_mediana_srag_covid.csv", row.names = FALSE)


rm(big_data)

