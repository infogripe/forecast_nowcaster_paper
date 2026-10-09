library(EpiNow2)
library(dplyr)
library(ggplot2)
library(future)
library(future.apply)
library(fitdistrplus)

use.epiweek<-FALSE

name_drs<-unique(boletim$drs)

temp <- list.files("bases_brutas")

gen_time <- generation_time_opts(
  Gamma(mean = 3.2, sd = 2.1, max = 14)
)

options(mc.cores = 4)
plan(multisession, workers = 4)

resultados <- list()

for (i in temp) {
  
  cat("\n====================================\n")
  cat("BASE:", i, "\n")
  cat("====================================\n")
  
  boletim <- get(load(paste0("bases_brutas/", i)))
  
  dados_srag <- boletim %>%
    mutate(
      DT_REPORT = pmax(DT_NOTIFIC, DT_DIGITA, na.rm = TRUE),
      atraso = DT_REPORT - DT_SIN_PRI
    ) %>%
    dplyr::select(DT_SIN_PRI, drs, atraso, DT_REPORT)%>%
    filter(DT_SIN_PRI >= max(DT_SIN_PRI) - 180)

  # ----------------------------------------------------------
  # Paraleliza as DRS dessa base
  # ----------------------------------------------------------
  
  res_drs <- future_lapply(
    name_drs,
    function(j) {
      
      inicio <- Sys.time()
      
      cat("Iniciando:", j, "\n")
      
      tryCatch({
        
        # ----------------------------------------------------
        # DRS especiais
        # ----------------------------------------------------
        
        if (j %in% c(
          "GRANDE SÃO PAULO",
          "CAMPINAS",
          "SÃO JOSÉ DO RIO PRETO"
        )) {
          
          sub_data <- dados_srag %>%
            filter(drs == j) %>%
            mutate(
              date_onset = DT_SIN_PRI,
              report_date = DT_REPORT,
              delay = atraso
            ) 
          
          
          data_corte <- max(sub_data$DT_SIN_PRI) - 3
          
          data <- sub_data %>%
            # %>%
            group_by(DT_SIN_PRI) %>%
            summarise(confirm = n(), .groups = "drop") %>%
            rename(date = DT_SIN_PRI) %>%
            arrange(date) %>%
            tidyr::complete(
              date = seq(min(date), max(date), by = "day"),
              fill = list(confirm = 0)
            ) %>%
            filter(date >= max(date) - 70) %>%
            filter(date <= data_corte)
            
          
        } else {
          
          sub_data <- dados_srag %>%
            filter(drs == j) %>%
            mutate(
              date_onset = DT_SIN_PRI,
              report_date = DT_REPORT,
              delay = atraso
            ) 
          
          data <- sub_data %>%
            # %>%
            group_by(DT_SIN_PRI) %>%
            summarise(confirm = n(), .groups = "drop") %>%
            rename(date = DT_SIN_PRI) %>%
            arrange(date) %>%
            tidyr::complete(
              date = seq(min(date), max(date), by = "day"),
              fill = list(confirm = 0)
            ) %>%
            filter(date >= max(date) - 70)
          
          
        }
        
        # ----------------------------------------------------
        # Delay
        # ----------------------------------------------------
        atrasos <- sub_data %>%
           filter(atraso>0) %>%
         pull(atraso)%>%
          as.numeric()
        
        fit <- fitdist(
          atrasos,
          "lnorm"
        )
        
        max_delay <- as.numeric(
          quantile(atrasos, 0.95, na.rm = TRUE)
        )
        
        reporting_delay <- LogNormal(
          meanlog = fit$estimate["meanlog"],
          sdlog   = fit$estimate["sdlog"],
          max     = max_delay
        )
        
        # ----------------------------------------------------
        # EpiNow2
        # ----------------------------------------------------
        
        estimates <- epinow(
          data = data,
          generation_time = gt_opts(gen_time),
          delays = delay_opts(reporting_delay),
          CrIs = c(0.5, 0.95),
          forecast = forecast_opts(horizon = 28),
          stan = stan_opts(samples = 1000),
          output = "samples"
        )
        
        # ----------------------------------------------------
        # Predictions
        # ----------------------------------------------------
        
        now <- get_predictions(
          estimates,
          format = "sample"
        )
        
        DT_max <- max(now$date, na.rm = TRUE)
        DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
        use.epiweek <- FALSE
        
        now2 <- now %>%
          mutate(
            DT.sun.aux = as.integer(format(date, "%w")),
            dt.aux = date -
              DT.sun.aux +
              ifelse(
                use.epiweek,
                0,
                DT_max_diadasemana + 1 -
                  ifelse(DT_max_diadasemana + 1 > DT.sun.aux, 7, 0)
              ),
            date_onset = dt.aux -
              ifelse(date < dt.aux, 7, 0)
          ) %>%
          group_by(date_onset, sample) %>%
          summarise(
            Y = sum(predicted, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          group_by(date_onset) %>%
          summarise(
            Median = median(Y, na.rm = TRUE),
            LI = quantile(Y, 0.025, na.rm = TRUE),
            LS = quantile(Y, 0.975, na.rm = TRUE),
            LIb = quantile(Y, 0.25, na.rm = TRUE),
            LSb = quantile(Y, 0.75, na.rm = TRUE),
            .groups = "drop"
          ) %>% mutate(
            base = as.character(i),
            DRS = as.character(j)
          ) 
        
        
        now<- now %>%
          mutate(
            base = as.character(i),
            DRS = as.character(j)
          ) 
        
        fim <- Sys.time()
        
        tempo <- data.frame(
          base = i,
          DRS = j,
          inicio = inicio,
          fim = fim,
          tempo_minutos = as.numeric(
            difftime(fim, inicio, units = "mins")
          ),
          status = "OK"
        )
        
        cat(
          "FINALIZADA:", j,
          "|", round(tempo$tempo_minutos, 2),
          "min\n"
        )
        
        list(
          now=now,
          data = now2,
          tempo = tempo,
          erro = NULL
        )
        
      }, error = function(e) {
        
        fim <- Sys.time()
        
        erro <- data.frame(
          base = as.character(i),
          DRS = as.character(j),
          erro = conditionMessage(e),
          stringsAsFactors = FALSE
        )
        
        tempo <- data.frame(
          base = i,
          DRS = j,
          inicio = inicio,
          fim = fim,
          tempo_minutos = as.numeric(
            difftime(fim, inicio, units = "mins")
          ),
          status = "ERRO"
        )
        
        cat(
          "ERRO:", j,
          "|", conditionMessage(e), "\n"
        )
        
        list(
          now = now,
          data = now2,
          tempo = tempo,
          erro = NULL
        )
      })
    },
    
    # importante: envia as variáveis necessárias aos workers
    future.seed = TRUE
  )
  
  resultados[[i]] <- res_drs
}

plan(sequential)


big_data <- bind_rows(
  lapply(
    resultados,
    function(x) {
      bind_rows(
        lapply(x, `[[`, "data")
      )
    }
  )
)

big_data2 <- bind_rows(
  lapply(
    resultados,
    function(x) {
      bind_rows(
        lapply(x, `[[`, "now")
      )
    }
  )
)

big_erros <- bind_rows(
  lapply(
    resultados,
    function(x) {
      bind_rows(
        lapply(x, `[[`, "erro")
      )
    }
  )
)

big_times <- bind_rows(
  lapply(
    resultados,
    function(x) {
      bind_rows(
        lapply(x, `[[`, "tempo")
      )
    }
  )
)

write.csv(
  big_data,
  "epinow/forecasting_epinow_SRAG_4weeks.csv",
  row.names = FALSE
)

write.csv(
  big_data2,
  "epinow/forecasting_trajetorias_epinow_SRAG_4weeks.csv",
  row.names = FALSE
)

write.csv(
  big_erros,
  "epinow/erros_forecasting_epinow_SRAG_4weeks.csv",
  row.names = FALSE
)

write.csv(
  big_times,
  "epinow/times_forecasting_epinow_SRAG_4weeks.csv",
  row.names = FALSE
)
