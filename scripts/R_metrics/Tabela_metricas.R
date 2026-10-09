library(dplyr)
library(tidyr)

metricas<-read.csv("metrics/metrics_for_four_weeks.csv")

unique(metricas$horizon)


#####Mediana

total<- metricas %>%
  # filter(widow!=30 | is.na(widow)) %>%   
  group_by(metodo, horizon) %>%
  dplyr::summarise(n= n(),
                   cob50=round(sum(cov_50, na.rm = TRUE)/n(),2),
                   cob95=round(sum(cov_95, na.rm = TRUE)/n(),2),
                               WIS=round(median(wis, na.rm=TRUE),1),
                               dispersao=round(median(dispersion, na.rm=TRUE),1),
                               subprev=round(median(underprediction, na.rm=TRUE),1),
                               superprev=round(median(overprediction, na.rm=TRUE),1),
                               MAE=round(median(ae_median, na.rm=TRUE),1)) %>%
  mutate(metodo2=case_when(
    metodo=="com_atraso" ~  "Uncorrected data",
    metodo=="corte 3 semanas" ~ "Exclusion last 3 weeks",
    metodo=="mediana" ~ "Median-corrected",
    metodo=="nowcaster" ~ "Nowcaster"
    
  ))

tabela <- total %>%
  select(metodo, horizon, cob50, cob95) %>%
  pivot_wider(
    names_from = horizon,
    values_from = c(cob50, cob95),
    names_glue = "{.value}_{horizon}wk"
  )

write.csv2(tabela, "output/Metrics/tabela_cob_horizons.csv", row.names = FALSE)

tabela2 <- total %>%
  select(metodo, horizon, WIS) %>%
  pivot_wider(
    names_from = horizon,
    values_from = c(WIS),
    names_glue = "{.value}_{horizon}wk"
  )

write.csv2(tabela2, "output/Metrics/tabela_wis_horizons.csv", row.names = FALSE)

tabela3 <- total %>%
  select(metodo, horizon, dispersao, subprev, superprev) %>%
  pivot_wider(
    names_from = horizon,
    values_from = c(dispersao, subprev, superprev),
    names_glue = "{.value}_{horizon}wk"
  )

write.csv2(tabela3, "output/Metrics/tabela_wis_comp2_horizons.csv", row.names = FALSE)
