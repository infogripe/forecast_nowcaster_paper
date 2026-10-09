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
#df<-read.csv("output/Modelos/nowcaster/forecasting_DRS_srag_covid_trim_23_24.csv")
df<-read.csv("models/predictions_all_methods_four_weeks.csv")

data_drs<- df %>%   
  filter(base %in% c("2023-09-19"), DRS=="GRANDE SÃO PAULO") %>% 
  mutate(dt_event = ymd(date_onset))


##Calculando casos observados
conso<- boletim %>% select(DT_SIN_PRI, drs, classi)

##POR DRS
conso_total <- conso %>% dplyr::rename(DRS=drs) %>%
  filter(DRS=="GRANDE SÃO PAULO") %>%
  dplyr::filter(DT_SIN_PRI>"2022-10-01") %>%
  dplyr::group_by(DRS, DT_SIN_PRI) %>%
  dplyr::summarise(casos=n())


conso_drs <- data_drs %>%
  dplyr::group_by(metodo, DRS, base) %>%
  dplyr::summarise(
    data_max = max(dt_event) + 6 + 10 * 7,
    data_min = min(dt_event) - 10 * 7,
    .groups = "drop"
  ) %>%
  dplyr:: mutate(week_min=as.integer(format(data_min, "%w"))) %>%
  dplyr::mutate(week_min=replace(week_min, week_min==0,7))%>%
  dplyr::full_join(conso_total) %>%
  dplyr::group_by(metodo, DRS, base, DT_SIN_PRI) %>%
  dplyr::filter(
    DT_SIN_PRI >= data_min &
      DT_SIN_PRI <= data_max
  ) %>%
  as.data.frame()


dt_event <- Date()

for(j in 1:nrow(conso_drs)) {
  
  dt_event[j] <- floor_date(conso_drs$DT_SIN_PRI[j],
                            'week',
                            week_start = conso_drs$week_min[j])
  
}

conso_drs2 <- conso_drs |>
  #ungroup() |>
  dplyr::mutate(dt_event = dt_event) |>
  dplyr::group_by(metodo, DRS, base, dt_event) |>
  dplyr::summarise(casos=sum(casos, na.rm = TRUE))|>
  as.data.frame()|>
  dplyr::select(metodo, base, dt_event, DRS, casos)

dados  <- full_join(data_drs,conso_drs2, by=c("metodo","base", "DRS", "dt_event"))

##coloando 0 nas semanas que não tem casos registrados
dados$casos[is.na(dados$casos)]<-0


write.csv(dados, paste0("metrics/obsereved_data_SP.csv"), row.names = FALSE)



