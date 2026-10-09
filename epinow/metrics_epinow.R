metricas<-read.csv("metrics_for_four_weeks_epinow.csv")


#####Mediana

total<- metricas %>%
  filter(horizon<=2) %>% 
  group_by(DRS)%>%
  dplyr::summarise(n= n(),
                   cob50=round((sum(cov_50, na.rm = TRUE)*100)/n(),1),
                   cob95=round((sum(cov_95, na.rm = TRUE)*100)/n(),1),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   dispersao=round(median(dispersion, na.rm=TRUE),1),
                   subprev=round(median(underprediction, na.rm=TRUE),1),
                   superprev=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1))


#sprintf("%.2f", total$WIS) 


write.csv2(total, "summary_metrics_for_epinow.csv")



#####Mediana

total<- metricas %>%
  # filter(widow!=30 | is.na(widow)) %>%   
  group_by( horizon) %>%
  dplyr::summarise(n= n(),
                   cob50=round(sum(cov_50, na.rm = TRUE)/n(),2),
                   cob95=round(sum(cov_95, na.rm = TRUE)/n(),2),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   dispersao=round(median(dispersion, na.rm=TRUE),1),
                   subprev=round(median(underprediction, na.rm=TRUE),1),
                   superprev=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1)) 

tabela <- total %>%
  select( horizon, cob50, cob95) %>%
  pivot_wider(
    names_from = horizon,
    values_from = c(cob50, cob95),
    names_glue = "{.value}_{horizon}wk"
  )

write.csv2(tabela, "abela_cob_horizons_for_epinow.csv", row.names = FALSE)

tabela2 <- total %>%
  select(horizon, WIS) %>%
  pivot_wider(
    names_from = horizon,
    values_from = c(WIS),
    names_glue = "{.value}_{horizon}wk"
  )

write.csv2(tabela2, "tabela_wis_horizons_for_epinow.csv", row.names = FALSE)

tabela3 <- total %>%
  select( horizon, dispersao, subprev, superprev) %>%
  pivot_wider(
    names_from = horizon,
    values_from = c(dispersao, subprev, superprev),
    names_glue = "{.value}_{horizon}wk"
  )

write.csv2(tabela3, "tabela_wis_comp2_horizons_epinow.csv", row.names = FALSE)
