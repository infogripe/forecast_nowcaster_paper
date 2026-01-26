library(dplyr)


metricas<-read.csv("output/Metricas/metrica_modelos_controle.csv")

metrica_now<-read.csv("output/Metricas/metrica_nowcaster.csv")

metrica_now$tipo<-"nowcaster"

junt<-bind_rows(metricas, metrica_now)

str(junt)

#####Mediana

total<- junt %>%
  filter(!base %in% c("2023-07-04", "2023-07-26")) %>% ####Tirando bases muito próximos de outras para ficar com n igual dos modelos do nowcaster
 # filter(widow!=30 | is.na(widow)) %>%   
  group_by(virus, tipo, widow) %>% 
  dplyr::summarise(n= n(),
                   cob50=round(sum(cov_50*100)/n(),1),
                   cob95=round(sum(cov_95*100)/n(),1),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   dispersao=round(median(dispersion, na.rm=TRUE),1),
                   subprev=round(median(underprediction, na.rm=TRUE),1),
                   superprev=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1))



#sprintf("%.2f", total$WIS) 


write.csv2(total, "output/Metricas/metricas_mediana_virus.csv")


total_DRS<- junt %>%
  filter(!base %in% c("2023-07-04", "2023-07-26")) %>% ####Tirando bases muito próximos de outras para ficar com n igual dos modelos do nowcaster
  filter(widow!=30 | is.na(widow)) %>%   
  group_by(virus, tipo, widow, DRS) %>% 
  dplyr::summarise(n= n(),
                   cob50=round(sum(cov_50*100)/n(),1),
                   cob95=round(sum(cov_95*100)/n(),1),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   dispersao=round(median(dispersion, na.rm=TRUE),1),
                   subprev=round(median(underprediction, na.rm=TRUE),1),
                   superprev=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1))


write.csv(total_DRS, "output/Metricas/metricas_mediana_DRS.csv")

