library(aweek)
library(dplyr)
library(tidyr)
library(ggplot2)
library(zoo)
library(lubridate)
install.packages("ggpattern")
library(ggpattern)
update.packages("ggplot2")

#filter(base %in% c("2023-09-06", "2023-10-18", "2023-11-08","2024-02-14")) %>%
#load("C:/Users/tatty/Documents/GitHub/01_SRAG/boletim.rData") ###Boletim do 19/02/2025

conso<-read.csv("metrics/obsereved_data_SP.csv")


######Métricas

df<-read.csv("models/predictions_all_methods_four_weeks.csv")


df2<- df %>%
  filter(DRS=="GRANDE SÃO PAULO") %>%
  group_by(DRS, metodo) %>%
  filter(Time<=max(Time)-2) %>%
  ungroup()%>%
  mutate(date_onset=ymd(date_onset))%>%
  mutate(tipo2=case_when(
    metodo=="delay" ~  "Uncorrected data",
    metodo=="cut_3_weeks" ~ "Exclusion last 3 weeks",
    metodo=="median" ~ "Median-corrected",
    metodo=="nowcast" ~ "Nowcaster"
  )) %>%
  filter(base %in% c("2023-09-19","2024-02-14")) 

####Plotando nowcast

dfn1<- df2 %>%filter(metodo=="nowcast") %>%
  filter(base %in% c("2023-09-19")) %>%
  mutate(dado=case_when(
    Time>=10 ~ "forecast",
    Time<10 ~ "nowcast"
  ))

dfn1<- dfn1 %>%filter(Time==9)%>%
       mutate(dado="forecast")%>%
       bind_rows(dfn1)

conso1<- conso %>%
  mutate(dt_event=ymd(dt_event))%>%
  filter(metodo=="nowcast") %>%
  filter(dt_event<="2023-11-28")

dfn1_fore<- dfn1 %>%
  filter(dado=="forecast")

dfn1_now<- dfn1 %>%
  filter(dado=="nowcast")

dfn1_obs<- dfn1 %>%
  filter(Time<=9)

min<-min(dfn1_obs$date_onset-7)
max<-max(conso$date_onset)

a<-ggplot() +
  geom_col(data=dfn1_obs, aes(x=date_onset, y=obs_naive, fill="Observed data"))+
  geom_line(data = conso1, aes(x = dt_event, y = casos, color="Consolidated data"),linewidth = 0.8)+
  geom_line(data=dfn1_now,aes(x=date_onset, y=Median),color='#184E77', linewidth = 0.8)+
  geom_line(data=dfn1_fore,aes(x=date_onset, y=Median),color='#9B2226',  linewidth = 0.8)+
  labs(color = ' ')+
  geom_ribbon(data=dfn1_now, aes(x=date_onset, ymin=LI, ymax=LS, fill="Nowcast"), alpha = 0.4)+
  geom_ribbon(data=dfn1_fore, aes(x=date_onset, ymin=LI, ymax=LS, fill="Forecast"), alpha = 0.4)+
   scale_color_manual(values = c(
     "Consolidated data" = "black",
    "Nowcast"='#184E77',
    "Forecast" = '#9B2226'))+
  scale_fill_manual(values = c(
    "Observed data" = "lightblue",
    "Nowcast"='#184E77'  ,
    "Forecast"='#9B2226'))+
  labs(x = "Week",y = 'Number of SARI cases', fill="") +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%y")+
  theme_bw()+
  xlim(c(min,max))+
  ggtitle("Nowcaster")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=16, face="bold"),
        legend.position = "bottom",
        legend.text =element_text(size=12))


#####Mediana

####Plotando nowcast

dfm<- df2 %>%filter(metodo=="median") %>%
  filter(base %in% c("2023-09-19")) 

dfm1<- dfm %>%filter(Time>=8)%>%
  mutate(dado="forecast")

conso2<- conso %>%
  mutate(dt_event=ymd(dt_event))%>%
  filter(metodo=="median") %>%
  filter(dt_event<="2023-11-28")

dfm1_fore<- dfm1 %>%
  filter(dado=="forecast")

min2<-min(dfm$date_onset)-7
max2<-max(conso2$date_onset)

#help("geom_col_pattern")

b<-ggplot() +
  geom_col(data=dfn1_obs, aes(x=date_onset, y=obs_naive), fill="lightblue")+
  geom_col_pattern(data=dfm,
                   aes(x=date_onset, y=obs_naive, fill="Median of corrected data"), 
    pattern = 'stripe',
   # fill="white",
    colour  = 'black',
    alpha=0.1
  )+
  geom_line(data = conso2, aes(x = dt_event, y = casos),linewidth = 0.8, color="black")+
 geom_line(data=dfm,aes(x=date_onset, y=Median), linewidth = 0.8, color="lightgrey")+
  geom_line(data=dfm1_fore,aes(x=date_onset, y=Median), color= '#9B2226', linewidth = 0.8)+
  labs(color = ' ')+
  geom_ribbon(data=dfn1_now, aes(x=date_onset, ymin=LI, ymax=LS, fill="Fitted data"),  alpha = 0.5)+
  geom_ribbon(data=dfn1_fore, aes(x=date_onset, ymin=LI, ymax=LS, fill="Forecast"), alpha = 0.4)+
  scale_color_manual(values = c(
    "Fitted data"='lightgrey',
    "Forecast" = '#9B2226',
    "Consolidated data" = "black"))+
  scale_fill_manual(values = c(
    "Median of corrected data"="white",
    "Forecast" = '#9B2226',
    "Fitted data"='lightgrey'))+
  labs(x = "Week",y = 'Number of SARI cases', fill="") +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%y")+
  theme_bw()+
  xlim(c(min2,max2))+
  ggtitle("Median-corrected")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=16, face="bold"),
        legend.position = "bottom",
        legend.text =element_text(size=12))
######Sem correção

dfd<- df2 %>%filter(metodo=="delay") %>%
  filter(base %in% c("2023-09-19"))  

dfd1<- dfd %>%
  filter(Time>=9)%>%
  mutate(dado="forecast")

dfd2<- dfd %>%
  filter(Time<=9)%>%
  mutate(dado="fitted")

conso3<- conso %>%
  mutate(dt_event=ymd(dt_event))%>%
  filter(metodo=="delay") %>%
  filter(dt_event<="2023-11-28") 


min3<-min(dfd$date_onset)-7
max3<-max(conso3$date_onset)

c<-ggplot() +
  geom_col(data=dfd, aes(x=date_onset, y=obs_naive, fill="Observed data"))+
  geom_line(data = conso1, aes(x = dt_event, y = casos),linewidth = 0.8, color="black")+
  geom_line(data=dfd2,aes(x=date_onset, y=Median), color="lightgrey", linewidth = 0.8)+
  geom_line(data=dfd1,aes(x=date_onset, y=Median), color= '#9B2226',linewidth = 0.8)+
  labs(color = ' ')+
  geom_ribbon(data=dfd2, aes(x=date_onset, ymin=LI, ymax=LS, fill="Fitted data"), alpha = 0.5)+
  geom_ribbon(data=dfd1, aes(x=date_onset, ymin=LI, ymax=LS, fill="Forecast"), alpha = 0.4)+
  scale_color_manual(values = c(
    "Consolidated data" = "black",
    "Fitted data" = 'lightgrey',
    "Forecast" = '#9B2226'))+
  scale_fill_manual(values = c(
    "Observed data" = "lightblue",
    "Fitted data" = 'lightgrey',
    "Forecast" = '#9B2226'))+
  labs(x = "Week",y = 'Number of SARI cases', fill="") +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%y")+
  theme_bw()+
  xlim(c(min3,max3))+
  ggtitle("Uncorrected data")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=16, face="bold"),
        legend.position = "bottom",
        legend.text =element_text(size=12))

######Cut three weeks

######Sem correção

dfcut<- df2 %>%filter(metodo=="cut_3_weeks") %>%
  filter(base %in% c("2023-09-19")) 

dfcut2<- dfcut %>%
  filter(Time>=14)%>%
  mutate(dado="forecast (2 wks ahead)")

dfcut3<- dfcut %>%
  filter(Time<=14, Time>=11)%>%
  mutate(dado="forecast of cutted 3 week")

dfcut4<- dfcut %>%
  filter(Time<=11)%>%
  mutate(dado="fitted data")


conso4<- conso %>%
  mutate(dt_event=ymd(dt_event))%>%
  filter(metodo=="cut_3_weeks") %>%
  filter(dt_event<="2023-11-28")

min4<-min(dfcut$date_onset)-7
max4<-max(conso4$date_onset)



d<-ggplot() +
  geom_col(data=dfcut,aes(x=date_onset, y=obs_naive), fill="lightblue")+
  geom_col(data=dfn1_obs,aes(x=date_onset, y=obs_naive), fill="lightblue")+
  geom_col_pattern(data=dfcut,
                   aes(x=date_onset, y=obs_naive, fill="3-week\ncutoff data"), 
                   pattern = 'crosshatch',
                   # fill="white",
                   colour  = 'black',
                   alpha=0.1
  )+
  geom_line(data=dfcut4, aes(x=date_onset, y=Median),linewidth = 0.8, color='lightgrey')+
  geom_line(data=dfcut3, aes(x=date_onset, y=Median), color="#C65D0B", linewidth = 0.8)+
  geom_line(data = conso4, aes(x = dt_event, y = casos),linewidth = 0.8, color="black")+
  geom_line(data=dfcut2,aes(x=date_onset, y=Median), color= '#9B2226', linewidth = 0.8)+
  labs(color = ' ')+
   geom_ribbon(data=dfcut3, aes(x=date_onset, ymin=LI, ymax=LS, fill="Estimate last\n3 weeks"), alpha = 0.4)+
  geom_ribbon(data=dfcut2, aes(x=date_onset, ymin=LI, ymax=LS, fill="Forecast"), alpha = 0.4)+
  geom_ribbon(data=dfcut4, aes(x=date_onset, ymin=LI, ymax=LS, fill="Fitted data"), alpha = 0.5)+
  scale_color_manual(values = c(
    "Fitted data" = 'lightgrey' ,
    "Estimate for the last 3 weeks"="#C65D0B",
    "Consolidated data" = "black",
    "Forecast"= '#9B2226'))+
  scale_fill_manual(values = c(
    "3-week\ncutoff data" = "white",
    "Estimate last\n3 weeks" = "#C65D0B",
    "Forecast" = "#9B2226",
    "Fitted data" = "lightgrey"
  ))+
  labs(x = "Week",y = 'Number of SARI cases', fill="") +
  scale_x_date(date_breaks = "4 weeks", date_labels = "%m/%y")+
  theme_bw()+
  xlim(c(min4,max4))+
  ggtitle("Exclusion last 3 weeks")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=16, face="bold"),
        legend.position = "bottom",
        legend.text =element_text(size=12))

library(patchwork)

png(filename=paste0("plots/Fig1.png"),
    width =12 , height =8 , units = "in", res = 300)

(a | c) /
(b | d)

dev.off()












