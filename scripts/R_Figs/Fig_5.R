#####Gráfico DRS

library(dplyr)
library(tidyverse)
library(patchwork)
####Figuras paper

metricas<-read.csv("metrics/metrics_for_four_weeks.csv")
metricas_epi<-read.csv("epinow/metrics_for_four_weeks_epinow.csv")

head(metricas)

head(metricas_epi)


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

head(junt)

cover<- junt %>%
    group_by(metodo2, DRS) %>% 
  dplyr::summarise(n= n(),
                   cob50=round((sum(cov_50, na.rm = TRUE)*100)/n(),1),
                   cob95=round((sum(cov_95, na.rm = TRUE)*100)/n(),1))

#############DISPRSIONN


teste1<- junt %>%
  group_by(DRS, epiyear, epiweek) %>%
  filter(wis==min(wis)) %>%
  ungroup() %>%
  group_by(DRS, metodo2) %>%
  count() %>%
  group_by(DRS) %>% 
  mutate(per= round(prop.table(n) * 100, 2))

teste1$metodo2 = factor(teste1$metodo2, levels=c("Uncorrected data (baseline)","Exclusion last 3 weeks",
                                                 "Median-corrected", "Nowcaster", "Epinow2"))

cover$metodo2 = factor(cover$metodo2, levels=c("Uncorrected data (baseline)","Epinow2", "Exclusion last 3 weeks",
                                                 "Median-corrected", "Nowcaster"))

head(cover)

p0<-ggplot(cover, aes(
  x = DRS, 
  y = metodo2, 
  fill = cob95  # Usa os níveis específicos de cada estado já atribuídos
)) +
  geom_tile()+
  scale_fill_gradientn(
    colours=c("#c5f9d7", "#f7d486", "#f27a7d"),
   # colours=c("#ffcf67", "#d3321d"),
   # colours = c("#9a0101","#f57c00", "#ffe082",  "#72bad5", "#023f73"),
    values = scales::rescale(c(25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 78, 80, 82, 84, 85,87,90,92,93,95, 96, 97, 98, 99, 100)),
  )+
   labs(
    title = "",
    x = " ",
    y = "",
    fill="Coverage 95%"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 12),  
    legend.position = "bottom"
  ) 


plot_0<-ggplot(cover, aes(x = cob95, y = DRS, color = metodo2)) +
  geom_point(size = 3)+
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c(
    "Uncorrected data (baseline)"= '#973131',
    "Exclusion last 3 weeks"='#A6CDC6',
    "Median-corrected"= "#4F959D",
    "Nowcaster"= "#10375C",
    "Epinow2"="#D97706"))+
  labs(
    x = "95% PI Coverage",
    y = '', 
    color = '', 
    fill = ''
  ) +
  geom_vline(xintercept=95)+
  theme(legend.position = "top", axis.text=element_text(size=15),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=18, face="bold")
  )


plot1<-ggplot(teste1, aes(x=DRS, y=per, fill=metodo2)) +
  geom_bar(stat = "identity", alpha=0.6)+
  geom_text(aes(label = paste(round(per), "%")), 
            position = position_stack(vjust = .5), size=3.2)+
  scale_fill_manual(values = c(
    "Uncorrected data (baseline)"= '#973131',
    "Exclusion last 3 weeks"='#A6CDC6',
    "Median-corrected"= "#4F959D",
    "Nowcaster"= "#10375C",
    "Epinow2"="lightblue"))+
  theme_bw()+
  labs(fill = '', x="", y="Proportion of weeks\n(lowest wis)")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(legend.position = "top", axis.text=element_text(size=16),
        axis.text.x =element_blank(),
        legend.text = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=11),
        legend.title = element_text(size=10),
        strip.text = element_text(size=14),
        plot.title = element_text(size=16, face="bold")
  )

##############UNDERPREDICTION


teste3<- junt %>%
  group_by(DRS, epiyear, epiweek) %>%
  filter(underprediction==min(underprediction)) %>%
  ungroup() %>%
  group_by(DRS, metodo2) %>%
  count() %>%
  group_by(DRS) %>% 
  mutate(per= round(prop.table(n) * 100, 2))

teste3$metodo2 = factor(teste3$metodo2, levels=c("Uncorrected data (baseline)","Exclusion last 3 weeks",
                                             "Median-corrected", "Nowcaster", "Epinow2"))


plot2<-ggplot(teste3, aes(x=DRS, y=per, fill=metodo2)) +
  geom_bar(stat = "identity", alpha=0.6)+
  geom_text(aes(label = paste(round(per), "%")), 
            position = position_stack(vjust = .5), size=3.2)+
  scale_fill_manual(values = c(
    "Uncorrected data (baseline)"= '#973131',
    "Exclusion last 3 weeks"='#A6CDC6',
    "Median-corrected"= "#4F959D",
    "Nowcaster"= "#10375C",
    "Epinow2"="lightblue"))+
  theme_bw()+
  labs(fill = '', x="", y="Proportion of weeks\n(lowest underprediction)")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(legend.position = "top", axis.text=element_text(size=16),
        axis.text.x = element_blank(),
        legend.text = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=11),
        legend.title = element_text(size=12),
        strip.text = element_text(size=14),
        plot.title = element_text(size=16, face="bold")
  )+ guides(fill = guide_legend(override.aes = list(
    fill = NA
  )))

plot2.2<-ggplot(teste3, aes(x=DRS, y=per, fill=metodo2)) +
  geom_bar(stat = "identity", alpha=0.6)+
  geom_text(aes(label = paste(round(per), "%")), 
            position = position_stack(vjust = .5), size=3.2)+
  scale_fill_manual(values = c(
    "Uncorrected data (baseline)"= '#973131',
    "Exclusion last 3 weeks"='#A6CDC6',
    "Median-corrected"= "#4F959D",
    "Nowcaster"= "#10375C",
    "Epinow2"="lightblue"))+
  theme_bw()+
  labs(fill = '', x="", y="Proportion of weeks\n(lowest underprediction)")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(legend.position = "top", axis.text=element_text(size=16),
        axis.text.x =element_blank(),
        legend.text = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=11),
        legend.title = element_text(size=10),
        strip.text = element_text(size=14),
        plot.title = element_text(size=16, face="bold")
  )


#############DISPRSIONN


teste4<- junt %>%
  group_by(DRS, epiyear, epiweek) %>%
  filter(dispersion==min(dispersion)) %>%
  ungroup() %>%
  group_by(DRS, metodo2) %>%
  count() %>%
  group_by(DRS) %>% 
  mutate(per= round(prop.table(n) * 100, 2))

teste4$metodo2 = factor(teste4$metodo2, levels=c("Uncorrected data (baseline)","Exclusion last 3 weeks",
                                                 "Median-corrected", "Nowcaster", "Epinow2"))


plot3<-ggplot(teste4, aes(x=DRS, y=per, fill=metodo2)) +
  geom_bar(stat = "identity", alpha=0.6)+
  geom_text(aes(label = paste(round(per), "%")), 
            position = position_stack(vjust = .5), size=3.2)+
  scale_fill_manual(values = c(
    "Uncorrected data (baseline)"= '#973131',
    "Exclusion last 3 weeks"='#A6CDC6',
    "Median-corrected"= "#4F959D",
    "Nowcaster"= "#10375C",
    "Epinow2"="lightblue"))+
  theme_bw()+
  labs(fill = '', x="", y="Proportion of weeks\n(lowest dispersion)")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(legend.position = "top", axis.text=element_text(size=16),
        axis.text.x =  element_text(size=10),
        legend.text = element_blank(),
        axis.title.y = element_text(size=14),
        axis.text.y=element_text(size=11),
        legend.title = element_text(size=12),
        strip.text = element_text(size=14),
        plot.title = element_text(size=16, face="bold")
  )+
  guides(fill = guide_legend(override.aes = list(
    fill = NA
  )))


plot_WIS<- 
  #p0/
  plot1/
  plot2/ 
  plot3


png(filename=paste0("plots/Fig_5.png"),
    width =10 , height =12 , units = "in", res = 300)

plot_WIS

dev.off()





#####Underpredictiom

resumo <-junt %>%
  group_by(metodo2) %>%
  summarise(
    median = median(overprediction, na.rm = TRUE),
    min = min(overprediction, na.rm = TRUE),
    max = max(overprediction, na.rm = TRUE),
    
    Q1_over = quantile(overprediction, 0.25, na.rm = TRUE),
    Q3_over = quantile(overprediction, 0.75, na.rm = TRUE),
    
    .groups = "drop"
  )

write.csv2(resumo, "metrics/summary_overprediction.csv", row.names = FALSE)

resumo2 <-junt %>%
  group_by(DRS, metodo2) %>%
  summarise(
    median = median(overprediction, na.rm = TRUE),
    min = min(overprediction, na.rm = TRUE),
    max = max(overprediction, na.rm = TRUE),
    
    Q1_over = quantile(overprediction, 0.25, na.rm = TRUE),
    Q3_over = quantile(overprediction, 0.75, na.rm = TRUE),
    
    .groups = "drop"
  )

write.csv2(resumo2, "metrics/summary_overprediction_DRS.csv", row.names = FALSE)

