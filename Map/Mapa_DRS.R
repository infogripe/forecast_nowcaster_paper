library(dplyr)
library(tidyr)
library(geobr)
library(tmap)
library(sf)
library(grid)
library(stringr)
library(lubridate)
library(ggplot2)
library(ggridges)

###Shape DRS
require(sf)
DRS <- read_sf(dsn = ".", layer = "DRS")

DRS<- DRS %>%   mutate(
  drs2 = str_to_upper(str_trim(str_remove(DRS, "^[IVXLCDM]+\\s+")))
)


load("C:/Users/tatty/Documents/GitHub/01_SRAG/boletim.rData") 


casos<- boletim %>%  
  filter(DT_SIN_PRI>="2021-08-08" & DT_SIN_PRI<="2024-10-06") %>%
 # filter(DT_SIN_PRI>="2021-01-01" & DT_SIN_PRI<"2024-01-01") %>%
  group_by(CO_MUN_RES) %>%
  summarise(srag=n()) 

#####ATRASO

dados_srag <- boletim |>
  mutate(DT_REPORT = pmax(DT_DIGITA, DT_NOTIFIC, na.rm = TRUE)) %>%
  #filter(DT_SIN_PRI>="2021-01-01" & DT_SIN_PRI<"2024-01-01") %>%
  filter(DT_SIN_PRI>="2021-08-08" & DT_SIN_PRI<="2024-10-06") %>%
  select(DT_SIN_PRI, DT_REPORT, idade, drs, classi) %>%
  mutate(week_sinpri=ceiling_date(DT_SIN_PRI, "week"),
         week_dt_report=ceiling_date(DT_REPORT, "week"))%>%
  mutate(atraso=DT_REPORT-DT_SIN_PRI)%>%
  mutate(drs2= iconv(drs, from = "UTF-8", to = "ASCII//TRANSLIT")) %>%
  left_join(DRS, by="drs2") %>%
  filter(!is.na(DRS))

##Read municipality

mun<-read_municipality(code_muni="all", simplified = TRUE, year = "2024",
                       showProgress = TRUE) %>%
     filter(abbrev_state=="SP") %>%
  mutate(CO_MUN_RES=substr(code_muni,1,6)) %>%
  left_join(casos, by="CO_MUN_RES")
     


##BR shape
BR<-read_country(year=2024)

# Defining the conditions and replacement values
conditions <- c("XIV Sao Joao da Boa Vista", "XV Sao Jose do Rio Preto")
replacement <- c("XIV SJ Boa Vista", "XV SJ do Rio Preto")

unique(DRS$DRS)

DRS$DRS <- ifelse(
  DRS$DRS %in% conditions,
  replacement[match(DRS$DRS, conditions)],
  DRS$DRS
)

unique(DRS$DRS)

brMap <- 
  tm_shape(BR) + 
  tm_borders(col= "black",
             alpha = 0.6, lwd=0.5)+
  tm_fill(col="white", aplpha=0.5, legend.show = TRUE)+
  tm_shape(DRS)+
  tm_fill(col="lightgrey", aplpha=0.5, legend.show = TRUE)+
  tm_borders(col= "black",
             alpha = 0.6, lwd=0.5)

mun$srag2 <- ifelse(is.na(mun$srag), 0, mun$srag)

sp<-tm_shape(mun)+
  tm_polygons(fill = "srag", 
              title="SARI cases",
              breaks = c(0, 50, 200, 1000, 5000, 15000, 94000),  
              palette="YlOrBr",
              border.col = "lightgrey", lty = "dashed",
              ctextNA = " "  ) +
# tm_fill("srag",
#          title = "Number of Cases",
##          style = "fisher",  palette="YlOrBr", n=5)+
  tm_shape(DRS) +
  tm_polygons(fill = "white", alpha = 0,  
              border.col = "black", lwd = 1.2)+
  tm_text(text = "Rotulo", col="black", fontface="bold")+
  #tm_graticules(lines=FALSE)+
  tm_compass(position = c("right", "bottom"))+
  tm_scalebar(position = c("right", "bottom"))+
  tm_layout(legend.position = c("left", "bottom"),
            legend.text.size = 0.6,      
            legend.title.size = 0.8)      



tiff("Figure_1_rev.tiff", units="mm", width=300, height=230, res=300)

ggplot(dados_srag, aes(x = atraso, y = DRS, fill=DRS)) + 
  geom_density_ridges( )+
  theme_bw()+
  scale_fill_viridis_d( ) +
  labs(fill = " ", y= " ") +
  scale_x_continuous(
    limits = c(0, 84),
    labels = function(x) round(x / 7),        
    name = "Weeks"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), 
    axis.title.x = element_text(size = 13),  
    axis.text.x  = element_text(size = 12),  
    axis.text.y  = element_text(size = 12),  
    legend.title = element_blank(), 
    legend.text  = element_blank(),
    strip.text   = element_text(size = 12, face = "bold"),
    legend.position = "none"
  )

print(sp, vp = viewport(0.75, 0.28, width = 0.50, height = 0.7))

print(brMap, vp = viewport(0.93, 0.42, width = 0.10, height = 0.12))

dev.off()

