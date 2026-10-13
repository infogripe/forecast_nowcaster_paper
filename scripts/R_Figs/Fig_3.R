library(dplyr)
library(lubridate)
library(patchwork)
library(ggplot2)
#srag<-read.csv("now_INLA_srag_metricas.csv")
srag<-read.csv("window/janela_srag_metricas.csv")

srag<- srag %>% filter(DRS!="ESTADO")


met_srag<- srag %>% group_by(DRS,virus, janela) %>%
  summarise(cob50=sum(cov_50*100)/n(),
            cob95=sum(cov_95*100)/n(),
            WIS=median(interval_score),
            dispersion=median(dispersion),
            underprediction=median(underprediction),
            overprediction=median(overprediction),
            ae_median=median(ae_median)) %>%
  rename("Spread"="dispersion",
         "Underprediction"="underprediction",
         "Overprediction"="overprediction") %>%
  filter(janela!=9)


####Fixar ordem das janelas

met_srag$janela = factor(met_srag$janela, levels=c('4', "6", "8", "10", "12", "14", "16","18","20","22",
                                                     "24","26","28", "30","40","60", "80","100"))


unique(srag$DRS)

####Coverage for selected DRS

srag2<- met_srag %>% filter(DRS %in% c("GRANDE SÃO PAULO", "ARAÇATUBA", "SOROCABA"))

srag2 <- srag2 %>%
  mutate(DRS = factor(DRS,
                      levels = c("ARAÇATUBA", "GRANDE SÃO PAULO", "SOROCABA"),
                      labels = c("II - Araçatuba", "I - Grande São Paulo", "XVI - Sorocaba")))

###Coverage

cov<-ggplot(srag2) +
  geom_bar(aes(y=cob95  ,x=janela, fill="95%"), stat = 'identity') +
  geom_bar(aes(y=cob50  ,x=janela, fill="50%"), stat = 'identity') +
  scale_fill_manual(values = c(
    "95%"= 'skyblue1',
    "50%"='dodgerblue4'))+
  labs(x="window size (weeks)",
       y= "Coverage %",
       fill='Prediction Interval',
       title="A")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  geom_hline(yintercept = 95, col = "darkred", linetype='dotted', size=1)+
  geom_hline(yintercept = 50, col = "red", linetype='dotted', size=1)+
  theme_bw()+
  facet_wrap(~DRS,  ncol=1) +
  #facet_wrap(~DRS, labeller=labeller(DRS=as_labeller(drs_label)))+
  theme(legend.position = "bottom", 
        axis.text=element_text(size=10),
        legend.text = element_text(size=14),
        axis.title = element_text(size=15),
        legend.title = element_text(size=14),
        plot.title = element_text(size=16, face="bold"),
        strip.text = element_text(face = "bold", size=16)
  )


wis<-ggplot(srag2)+
  geom_point(aes(x=janela, y=log(WIS), color="WIS"), size=1.5)+
  geom_point(aes(x=janela, y=log(Spread), color="Spread"), size=1.3)+
  scale_color_manual(values = c("WIS" = "black", "Spread" = "#EE4B2B" ), 
                    breaks = c("WIS", "Spread"))+
  theme_bw()+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  labs(x="window size (weeks)",
       y= "log",
       color="",
       title="B",
       size= "")+
  facet_wrap(~DRS, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom", 
        axis.text=element_text(size=10),
        legend.text = element_text(size=14),
        axis.title = element_text(size=15),
        legend.title = element_text(size=14),
        plot.title = element_text(size=16, face="bold"),
        strip.text = element_text(face = "bold", size=16)
  )


plot<- cov + wis
       


tiff(filename=paste("plots/Fig.3.tiff"),
    width =10, height =10 , units = "in", res = 300)

plot(plot)

dev.off()


