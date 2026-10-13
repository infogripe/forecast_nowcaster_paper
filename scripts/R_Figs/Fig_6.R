library(dplyr)
library(tidyverse)
library(patchwork)
####Figuras paper


#######PLot métricas só para o nowcaster por DRS

metricas<-read.csv("metrics/metrics_for_four_weeks.csv")

metricas_drs<- metricas %>%
  mutate(DRS = str_to_title(DRS))

metricas_DRS<- metricas %>%
  filter(horizon<=2, metodo=="nowcast") %>%   
  group_by(metodo, DRS) %>% 
  dplyr::summarise(n= n(),
                   cob50=round((sum(cov_50, na.rm = TRUE)*100)/n(),1),
                   cob95=round((sum(cov_95, na.rm = TRUE)*100)/n(),1),
                   WIS=round(median(wis, na.rm=TRUE),1),
                   Spread=round(median(dispersion, na.rm=TRUE),1),
                   Underprediction=round(median(underprediction, na.rm=TRUE),1),
                   Overprediction=round(median(overprediction, na.rm=TRUE),1),
                   MAE=round(median(ae_median, na.rm=TRUE),1)) %>%
  ungroup()

wis_drs<- metricas_DRS %>%
  dplyr::select(DRS, Spread, Underprediction, Overprediction) %>%
  pivot_longer(Spread:Overprediction, names_to = "WIS", values_to = "components")

# forçando ordem dos níveis
#ordem <- wis_drs %>%
#  group_by(DRS) %>%
 # summarise(total = sum(components, na.rm = TRUE)) %>%
 # arrange(desc(total))

#wis_drs2 <- wis_drs %>%
 # mutate(DRS = factor(DRS, levels = ordem$DRS))   

###WIS RELATIVO

rel_wis<- metricas %>%
  filter(horizon<=2) %>%
  filter(metodo %in% c("nowcast", "delay")) %>%
  group_by(DRS, metodo) %>% 
  dplyr::summarise(wis= median(wis, na.rm = TRUE)) %>%
  select(DRS, metodo, wis) %>%
  pivot_wider(names_from = "metodo", values_from ="wis") %>%
  mutate("Nowcaster"=nowcast/delay,
         "Uncorrected data (baseline)"=delay/delay) %>%
  select(DRS, "Nowcaster", "Uncorrected data (baseline)") %>%
  pivot_longer(2:3, values_to = "rel_wis", names_to = "metodo") 

####Figure 3 - paper

a<-wis_drs %>%
  #  ggplot(aes(x = components, y = reorder(DRS, WIS), fill = WIS)) +
  ggplot(aes(x = log1p(components), y= DRS, fill = WIS)) +
  geom_bar(stat = "identity")+
  scale_fill_manual("Decomposition WIS",values=c('pink','lightpink3','lightpink4'))+
  theme_bw()+
  #scale_x_discrete(guide = guide_axis(angle = 45))+
  theme(legend.position = "top", axis.text=element_text(size=15),
        axis.text.x = element_text(size=12),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_blank(),
        plot.title = element_text(size=18, face="bold")
  )+
  labs(x="Median WIS components (log)", y="")

#######Coverage

# forçando ordem dos níveis
#ordem2 <- metricas_DRS %>%
 # group_by(DRS) %>%
 # arrange(desc(cob95))

#metricas_drs2 <- metricas_drs %>%
#  mutate(DRS = factor(DRS, levels = ordem2$DRS))   

b<-ggplot(mapping = aes(x, y)) +
  geom_bar(data=metricas_DRS, aes(y=DRS  ,x=cob95, fill="95%"), stat = 'identity') +
  geom_bar(data=metricas_DRS, aes(y=DRS  ,x=cob50, fill="50%"), stat = 'identity') +
  scale_fill_manual(values = c(
    "95%"= 'skyblue1',
    "50%"='dodgerblue4'))+
  labs(fill = 'Prediction Interval', x="Coverage %", y="")+
  # scale_x_discrete(guide = guide_axis(angle = 45))+
  # geom_xline(yintercept = 95, col = "darkred", linetype='dotted', size=1)+
  # geom_xline(yintercept = 50, col = "red", linetype='dotted', size=1)+
  theme_bw()+
  theme(legend.position = "top", axis.text=element_text(size=15),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=10),
        axis.title.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=18, face="bold")
  )


rel_wis<- rel_wis%>% filter(metodo=="Nowcaster")


c<-ggplot(rel_wis, aes(x = rel_wis, y = DRS)) +
  geom_point(size = 3)+
  geom_point() +
  geom_vline(xintercept = 1, color="darkgray")+
  theme_bw()+
  labs(
    x = "Relative WIS",
    y = '', 
    color = '', 
    fill = ''
  ) +
  theme(legend.position = "top", axis.text=element_text(size=15),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        legend.title = element_text(size=12),
        plot.title = element_text(size=18, face="bold")
  )
  

tiff(filename=paste0("plots/Fig.6.tiff"), width =10 , height =12 , units = "in", res = 300)

b / a / c + plot_annotation(tag_levels = 'A')

dev.off()
