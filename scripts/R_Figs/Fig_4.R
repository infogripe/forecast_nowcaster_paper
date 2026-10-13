library(readxl)
library(foreign)
library(stringi)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork)
###Gráfico diferentes períodos

###TRansformar tudo em log!!

load("Map/boletim.rData") ###Boletim do 19/02/2025

##Calculando casos observados
conso<- boletim %>% select(DT_SIN_PRI, drs, classi)

pop<-read.dbf("Map/DRS.dbf")

pop<- pop %>%
  select(DRS, POP) %>%
  mutate(
    DRS = str_to_upper(str_remove(DRS, "^[IVXLCDM]+\\s*"))
  )

##POR DRS
conso_total <- conso %>% dplyr::rename(DRS=drs) %>%
  dplyr::filter(DT_SIN_PRI>"2022-10-01") %>%
  mutate(epiweek=epiweek(DT_SIN_PRI),
         epiyear=epiyear(DT_SIN_PRI)) %>%
  #  mutate(week_start_date = floor_date(DT_SIN_PRI, unit = "week", week_start = 1)) %>%
  group_by(DRS) %>%
  complete(epiyear, epiweek = 1:52) %>%
  # complete(week_start_date=seq.Date(min(week_start_date), max(week_start_date), by="week"))
  # complete(week_start_date=seq.Date(min(week_start_date), max(week_start_date), by="week"))%>%
  dplyr::group_by(DRS, epiyear, epiweek) %>%
  dplyr::summarise(srag2=n())%>%
  ungroup()%>%
  mutate(
    DRS = stri_trans_general(DRS, "Latin-ASCII")
  ) %>%
  left_join(pop, by = "DRS") %>%
  mutate(inci=(srag2/POP)*100000) %>%
  arrange(epiyear, epiweek)%>%
  group_by(DRS) %>%
  mutate(srag=zoo::rollmean(inci, k=3, align = "center", fill=NA)) %>%
  ungroup() %>%
  filter(epiyear %in% c(2023,2024))  %>%
  filter(!(epiyear == 2023 & epiweek <= 20),
         !(epiyear == 2024 & epiweek >= 50)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0))%>%
  filter(semana_global<=82 & semana_global>=22)

conso_total_SP <- conso %>% dplyr::rename(DRS=drs) %>%
  dplyr::filter(DT_SIN_PRI>"2022-10-01") %>%
  mutate(epiweek=epiweek(DT_SIN_PRI),
         epiyear=epiyear(DT_SIN_PRI)) %>%
  #  mutate(week_start_date = floor_date(DT_SIN_PRI, unit = "week", week_start = 1)) %>%
  group_by(DRS) %>%
  complete(epiyear, epiweek = 1:52) %>%
  # complete(week_start_date=seq.Date(min(week_start_date), max(week_start_date), by="week"))
  # complete(week_start_date=seq.Date(min(week_start_date), max(week_start_date), by="week"))%>%
  dplyr::group_by(DRS, epiyear, epiweek) %>%
  dplyr::summarise(srag2=n())%>%
  ungroup()%>%
  mutate(
    DRS = stri_trans_general(DRS, "Latin-ASCII")
  ) %>%
  left_join(pop, by = "DRS") %>%
  group_by(epiyear, epiweek) %>%
  summarise(casos=sum(srag2, na.rm = TRUE),
            pop=sum(POP, na.rm = TRUE))%>%
  ungroup()%>%
  mutate(inci=(casos/pop)*100000) %>%
  filter(epiyear %in% c(2023,2024))  %>%
  filter(!(epiyear == 2023 & epiweek <= 20),
         !(epiyear == 2024 & epiweek >= 50)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  arrange(semana_global)%>%
  mutate(srag=zoo::rollmean(inci, k=3, align = "center", fill=NA)) %>%
  ungroup() %>%
  filter(semana_global<=82 & semana_global>=22)



###Pegando métricas
metricas<-read.csv("metrics/metrics_for_four_weeks.csv")
metricas_epi<-read.csv("epinow/metrics_for_four_weeks_epinow.csv")


junt<- metricas %>% 
  bind_rows(metricas_epi)%>%
  filter(horizon<=2) %>%
  mutate(metodo2= case_when(
    metodo== "nowcast" ~"Nowcaster",
    metodo=="median"~ "Median-corrected",
    metodo=="cut_3_weeks" ~ "Exclusion last 3 weeks",
    metodo=="delay" ~"Uncorrected data (baseline)",
    metodo=="Epinow2" ~ "Epinow2")) %>%
  mutate(base=ymd(base)) %>%
  mutate(epiweek=epiweek(base),
         epiyear=epiyear(base))

total<- junt %>%
   filter(horizon<=2) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  group_by(epiyear, epiweek, metodo2) %>% 
  dplyr::summarise(n= n(),
                   cob50=round(sum(cov_50, na.rm = TRUE)/n(),2),
                   cob95=round(sum(cov_95, na.rm = TRUE)/n(),2),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   WIS_lwr=round(quantile(wis, probs= 0.25),1),
                   WIS_upr=round(quantile(wis,probs=0.75),1),
                   dispersao=round(median(dispersion, na.rm=TRUE),1),
                   disp_lwr=round(quantile(dispersion, probs= 0.25),1),
                   disp_upr=round(quantile(dispersion,probs=0.75),1),
                   subprev=round(median(underprediction, na.rm=TRUE),1),
                   subprev_lwr=round(quantile(underprediction, probs= 0.25),1),
                   subprev_upr=round(quantile(underprediction,probs=0.75),1),
                   superprev=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1)) %>%
  ungroup()%>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  filter(semana_global<=82 & semana_global>=22) 

#####Relative WIS




####Calculando wis relativo para cada método

rel_wis<- junt %>%
  filter(horizon<=2, metodo %in% c("nowcast", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, wis) %>%
  pivot_wider(names_from = "metodo", values_from ="wis") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_wis"=wis/delay) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_wis, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_wis,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22)



###SPREAD
rel_spread_now<- junt %>%
  filter(horizon<=2, metodo %in% c("nowcast", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, dispersion) %>%
  pivot_wider(names_from = "metodo", values_from ="dispersion") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=nowcast/delay) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Nowcaster")
  

rel_spread_medi<- junt %>%
  filter(horizon<=2, metodo %in% c("median", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, dispersion) %>%
  pivot_wider(names_from = "metodo", values_from ="dispersion") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=median/delay) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Median-corrected") %>%
  bind_rows(rel_spread_now)

rel_spread_cut<- junt %>%
  filter(horizon<=2, metodo %in% c("cut_3_weeks", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, dispersion) %>%
  pivot_wider(names_from = "metodo", values_from ="dispersion") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=cut_3_weeks/delay) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Exclusion last 3 weeks")%>%
  bind_rows(rel_spread_medi)

rel_spread_epi<- junt %>%
  filter(horizon<=2, metodo %in% c("Epinow2", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, dispersion) %>%
  pivot_wider(names_from = "metodo", values_from ="dispersion") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=Epinow2/delay) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Epinow2")%>%
  bind_rows(rel_spread_cut)

rel_spread<- junt %>%
  filter(horizon<=2, metodo %in% c("delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, dispersion) %>%
  pivot_wider(names_from = "metodo", values_from ="dispersion") %>%
  filter(!if_any(5, is.na)) %>%
  mutate("rel_disp"=delay/delay) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Uncorrected data (baseline)")%>%
  bind_rows(rel_spread_epi)


###UNDERPREDITION

rel_under_now<- junt %>%
  filter(horizon<=2, metodo %in% c("nowcast", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, underprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="underprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=nowcast/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Nowcaster")


rel_under_medi<- junt %>%
  filter(horizon<=2, metodo %in% c("median", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, underprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="underprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=median/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Median-corrected") %>%
  bind_rows(rel_under_now)

rel_under_cut<- junt %>%
  filter(horizon<=2, metodo %in% c("cut_3_weeks", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, underprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="underprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=cut_3_weeks/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Exclusion last 3 weeks")%>%
  bind_rows(rel_under_medi)

rel_under_epi<- junt %>%
  filter(horizon<=2, metodo %in% c("Epinow2", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, underprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="underprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=Epinow2/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Epinow2")%>%
  bind_rows(rel_under_cut)

rel_under<- junt %>%
  filter(horizon<=2, metodo %in% c("delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, underprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="underprediction") %>%
  filter(!if_any(5, is.na)) %>%
  mutate("rel_disp"=delay/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, , na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Uncorrected data (baseline)")%>%
  bind_rows(rel_under_epi)


################OVERPREDICTION

###UNDERPREDITION

rel_over_now<- junt %>%
  filter(horizon<=2, metodo %in% c("nowcast", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, overprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="overprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=nowcast/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Nowcaster")


rel_over_medi<- junt %>%
  filter(horizon<=2, metodo %in% c("median", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, overprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="overprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=median/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Median-corrected") %>%
  bind_rows(rel_over_now)

rel_over_cut<- junt %>%
  filter(horizon<=2, metodo %in% c("cut_3_weeks", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, overprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="overprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=cut_3_weeks/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Exclusion last 3 weeks")%>%
  bind_rows(rel_over_medi)

rel_over_epi<- junt %>%
  filter(horizon<=2, metodo %in% c("Epinow2", "delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, overprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="overprediction") %>%
  filter(!if_any(5:6, is.na)) %>%
  mutate("rel_disp"=Epinow2/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Epinow2")%>%
  bind_rows(rel_over_cut)

rel_over<- junt %>%
  filter(horizon<=2, metodo %in% c("delay")) %>%   
  mutate(epiweek=epiweek(dt_event),
         epiyear=epiyear(dt_event)) %>%
  mutate(semana_global = epiweek + ifelse(epiyear == 2024, 52, 0)) %>%
  select(semana_global, DRS, base, horizon, metodo, overprediction) %>%
  pivot_wider(names_from = "metodo", values_from ="overprediction") %>%
  filter(!if_any(5, is.na)) %>%
  mutate("rel_disp"=delay/(delay+0.0001)) %>%
  group_by(semana_global) %>%
  summarise(spread=round(median(rel_disp, na.rm=TRUE),1),
            spread_lwr=round(quantile(rel_disp, probs= 0.25, na.rm=TRUE),1),
            spread_upr=round(quantile(rel_disp,probs=0.75, , na.rm=TRUE),1)) %>%
  filter(semana_global<=82 & semana_global>=22) %>%
  select(semana_global, spread, spread_lwr, spread_upr) %>%
  mutate(metodo="Uncorrected data (baseline)")%>%
  bind_rows(rel_over_epi)




a<-ggplot()+
  geom_line(
  data = conso_total,
  aes(x = semana_global, y = srag, group = DRS),
  color = "grey80",
  linewidth = 0.5
) +
  theme_bw()+
  geom_line(data = conso_total_SP,
            aes(x = semana_global, y = srag),
            color = "black", linewidth = 1) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(1, 104, 4),
                     labels = rep(1:52, 2)[seq(1, 104, 4)])+
  labs(
    x = "",
    y = 'log of Incidence of SARI\n(100 k hab)', 
    color = 'Method', 
    fill = ''
  ) +
  theme(
        axis.title = element_text(size = 11),
        plot.title = element_text(face = "bold", size = 13),
        strip.text = element_text(face = "bold", size=12),
        legend.text = element_blank())+
  guides(
    color = guide_legend(
      override.aes = list(
        shape = NA,
        linetype = 0
      )
    )
  )


##Coverage
b<-ggplot(total, aes(y=cob95  ,x=semana_global, colour=metodo2)) + 
  geom_line() +
  #geom_point(size = 3)+
  geom_point(size = 3, position=position_jitter(w=0.08))+
  theme_bw()+
  scale_x_continuous(breaks = seq(1, 104, 4),
                     labels = rep(1:52, 2)[seq(1, 104, 4)])+
  labs(
    x = "",
    y = '95% Coverage', 
    color = '', 
    fill = ''
  ) +
  theme(
        axis.title = element_text(size = 13),
        plot.title = element_text(face = "bold", size = 13),
        strip.text = element_text(face = "bold", size=12),
        legend.text = element_blank())+
  guides(
    color = guide_legend(
     override.aes = list(
        shape = NA,
        linetype = 0
      )
   )
  )


d<- ggplot(rel_spread, aes(y=log(spread)  ,x=semana_global, colour=metodo)) + 
  geom_line() +
  geom_point(size = 3,  position=position_jitter(w=0.08))+
  geom_errorbar(aes(ymin= log(spread_lwr),ymax= log(spread_upr),width=0.2))+
  theme_bw()+
  #scale_y_log10() +
  scale_x_continuous(breaks = seq(1, 104, 4),
                     labels = rep(1:52, 2)[seq(1, 104, 4)])+
  labs(
    x = "",
    y = 'Relative Spread (log)', 
    color = ' ', 
    fill = ''
  ) +
  theme(
    axis.title = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size=12),
    legend.text = element_text(size = 12),
    legend.position = "bottom")

e<- ggplot(rel_under, aes(y=spread  ,x=semana_global, colour=metodo)) + 
  geom_line() +
  geom_point(size = 3, position=position_jitter(w=0.08))+
  geom_errorbar(aes(ymin= spread_lwr,ymax= spread_upr,width=0.2))+
  theme_bw()+
  #scale_y_log10() +
  scale_x_continuous(breaks = seq(1, 104, 4),
                     labels = rep(1:52, 2)[seq(1, 104, 4)])+
  labs(
    x = "Epidemiological week",
    y = 'Relative Underestimation', 
    color = ' ', 
    fill = ''
  ) +
  theme(
    axis.title = element_text(size = 13),
    plot.title = element_text(face = "bold", size = 13),
    strip.text = element_text(face = "bold", size=13),
    legend.text = element_blank())+
  guides(
    color = guide_legend(
      override.aes = list(
        shape = NA,
        linetype = 0
      )
    )
  )

###REFAZER QUE EU ACABEI NAO SALAVANDO O FINAL!!!

tiff(filename=paste("plots/Fig.4.tiff"),
     width =10, height =12 , units = "in", res = 300)

a/b/e/d + plot_annotation(tag_levels = 'A')

dev.off()


