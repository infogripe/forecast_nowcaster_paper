##Forecasting SRAG
comb <- function(x, y) {
  lapply(seq_along(x),
    function(i) rbind(x[[i]], y[[i]]))
}
print("Started forecasting SRAG")
saida <- foreach(i=1:length(agregado),
                  .packages=c("tidyverse", "data.table", "scales", "purrr",
                  "readr", "vroom", "INLA", "snow", "nowcaster"), .combine="comb") %dopar% {
                    if(agregado[i]=="ESTADO"){
                      data_corte<- max(dados_srag$DT_SIN_PRI)-3
                      dados_srag2<- dados_srag %>% filter(DT_SIN_PRI<=data_corte)
                      now <- nowcasting_inla(dataset = dados_srag2,
                                             data.by.week = T,
                                             Dmax = 10,
                                             date_onset = DT_SIN_PRI,
                                             date_report = DT_REPORT ,
                                             wdw = 10,
                                             age_col = idade,
                                             trajectories = T,
                                             K = 1)
                    }else if(agregado[i]=="GRANDE SÃO PAULO"){
                      dados_srag2<- dados_srag %>% filter(drs == agregado[i])
                      data_corte<- max(dados_srag2$DT_SIN_PRI)-3
                      dados_srag2<- dados_srag2 %>% filter(DT_SIN_PRI<=data_corte)
                      now <- nowcasting_inla(dataset = dados_srag2,
                                             data.by.week = T,
                                             # bins_age = c(0,10,18,60,120),
                                             Dmax = 08,
                                             date_onset = DT_SIN_PRI,
                                             date_report = DT_REPORT ,
                                             wdw = 08,
                                             # age_col = idade,
                                             trajectories = T,
                                             K = 1)
                    }else{
                    ## Função de Nowcasting
                    dados_srag2 <- dados_srag %>% filter(drs == agregado[i])
                    now <- nowcasting_inla(
                        dataset=dados_srag2,
                        data.by.week = T,
                        # bins_age = c(0,10,18,60,120),
                        # age_col = idade,
                        date_onset = DT_SIN_PRI,
                        date_report = DT_REPORT,
                        Dmax = 10,
                        K = 1,
                        trajectories = T,
                        wdw=10)
                    }
    ### objetos para exportação do foreach: ###
    nowcast_total<-now$total %>%
        filter(dt_event < max(.$dt_event))
    nowcast_age<-now$age
    nowcast_total$DRS<-agregado[i]
    nowcast_age$DRS<-agregado[i]
    dados_w_total<-now$data %>%
        group_by(date_onset) %>%
        tally()
    dados_w_total$DRS<-agregado[i]
    ##arrumando o output com os dados observados e as estimativas de nowcasting e forecasting
    for_srag<-now$data %>%
      rename(dt_event=date_onset) %>%
      group_by(dt_event) %>%
      tally() %>%
      full_join(now$total) %>%
      filter(dt_event > max(dt_event) - 180) %>%
      mutate(projecao="Estimado")
    for_srag <-for_srag %>%
      filter(row_number() == length(for_srag$n)-1) %>%
      bind_rows(for_srag) %>%
      arrange(dt_event)
    for_srag$projecao[(length(for_srag$n)-1):length(for_srag$n)]<-"Previsto"
    ## Dando stamp do nível de agregação
    for_srag$DRS<-agregado[i]
    ####Salvando as trajetórias
    trajetorias_total_srag <- now$trajectories  %>%
                       group_by(sample,Time,dt_event) %>%
                       summarise(Y=sum(Y))%>%
                       as.data.frame()
    ######Calculando atividade das últimas semanas
    intensidade<-get.intensity(trajetorias=trajetorias_total_srag,
                               nome_agravo = "SRAG",
                               nome_DRS=agregado[i])
    intensidade$DRS<-agregado[i]
    ###Calculando tendencia das útlimas semanas
    tendencia_4s_srag<-slope.estimate.quant(trajectories = trajetorias_total_srag, window = 4)%>%
                  as_tibble()
    tendencia_2s_srag<-slope.estimate.quant(trajectories = trajetorias_total_srag, window = 2)%>%
                  as_tibble()
    tendencia_4s_srag$DRS<-agregado[i]
    tendencia_2s_srag$DRS<-agregado[i]
    # tendencia_6s_srag<-slope.estimate.quant(
    #     trajectories = filter(trajetorias_total_srag,dt_event<max(trajetorias_total_srag$dt_event)),
    #     window = 6)%>%
    #     as_tibble()
    # tendencia_3s_srag<-slope.estimate.quant(
    #     trajectories = filter(trajetorias_total_srag,dt_event<max(trajetorias_total_srag$dt_event)),
    #     window = 3)%>%
    #     as_tibble()
    # tendencia_6s_srag$DRS<-agregado[i]
    # tendencia_3s_srag$DRS<-agregado[i]
    rm(trajetorias_total_srag)
    list(for_srag,
         intensidade,
         tendencia_4s_srag,
         tendencia_2s_srag#,
         # tendencia_6s_srag, tendencia_3s_srag
         )
    ###### final do foreach
                  }
## manejo comum dos outputs
for_srag <- saida[[1]]
intensidade_srag<-saida[[2]]
tendencia_4s_srag<- saida[[3]]
tendencia_2s_srag<- saida[[4]]
# tendencia_6s_srag<- saida[[5]]
# tendencia_3s_srag<- saida[[6]]
for_srag$virus <- "srag"
intensidade_srag$virus<- "srag"
##criando o df de tendência para o srag
v_objs <- grep("tendencia",ls(),value = TRUE)
lapply(v_objs,\(li){
 obji <- get(li)
 names(obji) <- c(gsub("_srag","",li),"DRS")
 assign(li,obji,envir = globalenv())
})
tendencia_srag<-left_join(tendencia_2s_srag, tendencia_4s_srag, by="DRS")
tendencia_srag$virus<- "srag"
# tendencia_srag_nowcast <-left_join(tendencia_3s_srag, tendencia_6s_srag, by="DRS") %>%
#     mutate(virus = "srag")
rm(list=v_objs)
###Fazendo de novo o forecasting por estado para tirar o nowcasting por faixa etária
data_corte<- max(dados_srag$DT_SIN_PRI)-3
dados_srag2<- dados_srag %>% filter(DT_SIN_PRI<=data_corte)
now2 <- nowcasting_inla(dataset = dados_srag2,
                        data.by.week = T,
                        bins_age = c(0,10,18,60,120),
                        Dmax = 10,
                        date_onset = DT_SIN_PRI,
                        date_report = DT_REPORT ,
                        wdw = 10,
                        age_col = idade,
                        trajectories = T,
                        K = 0)
DT_max <- max(dados_srag2 |>
                dplyr::pull(var = DT_REPORT),
              na.rm = T)
##Data máxima do dia da semana
DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
##Dados por faixa etária
nowcasting_sp_age<-dados_srag2 |>
  mutate(dt_event= DT_SIN_PRI - as.integer(format(DT_SIN_PRI, "%w")) -
           (6-DT_max_diadasemana))|>
  mutate(fx_etaria.num=
           case_when(idade>=0 & idade<=9 ~ 1,
                     idade>=10 & idade<=18 ~ 2,
                     idade>=19 & idade<=60 ~ 3,
                     idade>=61 ~ 4)) |>
  group_by(dt_event, fx_etaria.num) |>
  tally() |>
  filter(dt_event > max(dt_event) - 180) |>
  left_join(now2$age,
            by = c("dt_event", "fx_etaria.num"))
print("Finished forecasting")