library(dplyr)

#metricas<-valida_join

metricas<-read.csv("metrics/metrics_nowcast_four_weeks.csv")


#####Mediana

total<- metricas %>%
  filter(horizon>2) %>%   
#  group_by(metodo, horizon) %>% 
  dplyr::summarise(n= n(),
                   cob50=round((sum(cov_50, na.rm = TRUE)*100)/n(),1),
                   cob95=round((sum(cov_95, na.rm = TRUE)*100)/n(),1),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   dispersao=round(median(dispersion, na.rm=TRUE),1),
                   subprev=round(median(underprediction, na.rm=TRUE),1),
                   superprev=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1))



#sprintf("%.2f", total$WIS) 


write.csv2(total, "metrics/summary_metrics_nowcast.csv")


