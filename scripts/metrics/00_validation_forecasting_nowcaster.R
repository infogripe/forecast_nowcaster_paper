rm(list = ls())
gc()

if(!require ('tidyverse')) {install.packages('tidyverse')};library('tidyverse')
if(!require ('data.table')) {install.packages('data.table')};library('data.table')
if(!require ('scales')) {install.packages('scales')};library('scales')
if(!require ('Metrics')) {install.packages('Metrics')};library('Metrics')
if(!require ('scoringutils')) {install.packages('scoringutils')};library('scoringutils')
if(!require ('readxl')) {install.packages('readxl')};library('readxl')


###baixando a base consolidada
#dir<-setwd("G:/CCD/CVE/RESPIRATORIAS")

##Base do dia
load("C:/Users/tatty/Documents/GitHub/01_SRAG/boletim.rData") ###Boletim do 19/02/2025


###Previsoes_controle

df<-read.csv("output/Modelos/nowcaster/forecasting_DRS_srag_covid_trim_23_24.csv")


##Calculando casos observados
conso<- boletim %>% select(DT_SIN_PRI, drs, classi)

rm(boletim)

##POR DRS
conso_total <- conso %>% dplyr::rename(DRS=drs) %>%
    dplyr::filter(DT_SIN_PRI>"2022-10-01") %>%
    dplyr::group_by(DRS, DT_SIN_PRI) %>%
    dplyr::summarise(srag=n(),
                                covid=sum(classi=="SARS COV 2", na.rm = TRUE))%>%
                      pivot_longer(cols=c("srag", "covid"),names_to = "virus", values_to = "casos")

rm(conso)


###Lendo as bases de forecasting

#####BASES SRAG pré-trim sem faixa etária
#df<-read.csv("C:/Users/tatty/Documents/GitHub/forecast_nowcaster/output/forecasting_sem_cor_atraso_wdw10.csv")

###Pegando só as estimativas de forecast

data_drs<- df %>% mutate(dt_event=ymd(dt_event))

str(data_drs)

#####Calculando número de casos observados para cada semana de forecasting

list_virus<-as.list(unique(data_drs$virus))

datalist2<-list()

for (i in list_virus) {

fore_drs<-data_drs %>% filter(virus==i)

sub_conso<-conso_total %>% filter(virus==i)

conso_drs<- fore_drs %>%
 dplyr::group_by(DRS, base) %>%
    dplyr::summarise(data_max=max(dt_event)+6,
            data_min=min(dt_event)) %>%
    dplyr:: mutate(week_min=as.integer(format(data_min, "%w"))) |>
    dplyr::mutate(week_min=replace(week_min, week_min==0,7))|>
    dplyr:: full_join(sub_conso) |>
    dplyr:: group_by(DRS, base, DT_SIN_PRI) |>
    dplyr:: filter(DT_SIN_PRI >= data_min & DT_SIN_PRI <= data_max)|>
     as.data.frame()


##definindo a semana epidemiológica de cada caso consolidado

dt_event <- Date()

for(j in 1:nrow(conso_drs)) {

  dt_event[j] <- floor_date(conso_drs$DT_SIN_PRI[j],
                            'week',
                            week_start = conso_drs$week_min[j])

}

conso_drs2 <- conso_drs |>
  #ungroup() |>
    dplyr::mutate(dt_event = dt_event) |>
    dplyr::group_by(virus, DRS, base, dt_event) |>
    dplyr::summarise(casos=sum(casos, na.rm = TRUE))|>
    as.data.frame()|>
    dplyr::select(base, dt_event, DRS, virus, casos)

join <- left_join(fore_drs,conso_drs2, by=c("virus","base", "DRS", "dt_event"))

datalist2[[i]]<-join


}

##Juntando tudo
dados <- bind_rows(datalist2)

##coloando 0 nas semanas que não tem casos registrados
dados$casos[is.na(dados$casos)]<-0

#######Fazendo tabela no modelo que ele calcula todos os scores
modelo<- dados %>% dplyr::select(DRS, Time, base, virus, projecao, dt_event, Median, LI, LS, LIb, LSb, casos) %>%
  dplyr::rename(observed=casos,
         "0.5"=Median,
         "0.025"=LI,
         "0.975"=LS,
         "0.25"=LIb,
         "0.75"=LSb)

modelo_full<-pivot_longer(data=modelo, names_to="quantile_level", values_to = "predicted", cols = c(7:11))

modelo_full$quantile_level<-as.numeric(modelo_full$quantile_level)

modelo_full<- modelo_full %>% 
  as_forecast_quantile() 


teste<-score(modelo_full, get_metrics(modelo_full, select=c("wis","overprediction","underprediction","dispersion", "ae_median"))) 


###Calculando a cobertura

df_cov<- dados %>%
    dplyr::  mutate(cov_50= case_when(casos>=LIb & casos<=LSb ~ 1,
                                           TRUE ~ 0),
                          cov_95= case_when(casos>=LI & casos<=LS ~ 1,
                                           TRUE ~ 0)) %>%
             select(DRS, base, virus, dt_event, cov_50, cov_95) 

valida_join<-full_join(teste, df_cov, by=c("DRS", "base", "virus", "dt_event"))



write.csv(valida_join, paste0("output/Metricas/metrica_nowcaster.csv"), row.names = FALSE)



