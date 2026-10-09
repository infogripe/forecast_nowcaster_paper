nowcasting.summary <- function(trajetory, age = F){
  
  total.summy <- trajetory |>
    dplyr::group_by(Time, date_event, sample) |>
    dplyr::summarise(Y = sum(Y, na.rm = T)) |>
    dplyr::group_by(Time, date_event) |>
    dplyr::summarise(Median = stats::median(Y, na.rm = T),
                     LI = stats::quantile(Y, probs = 0.025, na.rm = T),
                     LS = stats::quantile(Y, probs = 0.975, na.rm = T),
                     LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                     LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                     .groups = "drop")
  if(age){
    age.summy <- trajetory |>
      dplyr::group_by(Time, date_event, fx_etaria, fx_etaria.num) |>
      dplyr::summarise(Median = stats::median(Y, na.rm = T),
                       LI = stats::quantile(Y, probs = 0.025, na.rm = T),
                       LS = stats::quantile(Y, probs = 0.975, na.rm = T),
                       LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                       LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                       .groups = "drop")
    
    output <- list()
    output$total <- total.summy
    output$age <- age.summy
    
  }else{
    output<- list()
    output$total <- total.summy
  }
  
  return(output)
  
}
  
  
  nowcast_sem_correcao<-function(data.inla, wdw, k ){
    
    
    model <- Y ~ 1 +
      f(Time,
        model = "rw2", ##Tempor estruturado por area não estruturada
        hyper = list("prec" = list(prior = "loggamma",
                                   param = c(0.01, 0.01)) 
        )) 
    
    
    
    output0 <- INLA::inla(model,
                          family = "nbinomial",
                          data = data.inla,
                          control.predictor = list(link = 1, compute = T),
                          control.compute = list( config = T, waic=T, dic=T),
                          control.family = list(
                            hyper = list("theta" = list(prior = "loggamma",
                                                        param = c(0.01, 0.01)))
                          )
    )
    
    srag.samples0.list <- INLA::inla.posterior.sample(n = 1000, output0)
    
    index.missing <- data.inla$Time
    
    
    vector.samples0 <- lapply(X = srag.samples0.list,
                              FUN = function(x, idx = index.missing){
                                stats::rnbinom(n = idx,
                                               mu = exp(x$latent[idx]),
                                               size = x$hyperpar[1]
                                ) * 1
                              } )
    
    ## Step 3: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}
    
    gg.age <- function(x, dados.gg, idx){
      data.aux <- dados.gg
      Tmin <- min(dados.gg$Time[idx])
      data.aux$Y[idx] <- x
      data.aggregated <- data.aux |>
        ## Selecionando apenas os dias faltantes a partir
        ## do domingo da respectiva ultima epiweek
        ## com dados faltantes
        dplyr::filter(Time >= Tmin  ) |>
        dplyr::group_by(Time, date_onset) |>
        dplyr::summarise(
          Y = sum(Y), .groups = "keep"
        )
      data.aggregated
    }
    
    ## Step 4: Applying the age aggregation on each posterior
    tibble.samples.0 <- lapply( X = vector.samples0,
                                FUN = gg.age,
                                dados = data.inla,
                                idx = index.missing)
    
    srag.pred.0 <- dplyr::bind_rows(tibble.samples.0, .id = "sample")
    
    total.summy <- srag.pred.0 |>
      dplyr::group_by(Time, date_onset, sample) |>
      dplyr::summarise(Y = sum(Y, na.rm = T)) |>
      dplyr::group_by(Time, date_onset) |>
      dplyr::summarise(Median = stats::median(Y, na.rm = T),
                       LI = stats::quantile(Y, probs = 0.025, na.rm = T),
                       LS = stats::quantile(Y, probs = 0.975, na.rm = T),
                       LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                       LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                       .groups = "drop") %>%
      left_join(data.inla, by=c("Time", "date_onset")) %>%
     dplyr::rename(obs_naive=Y)
    
    
    return(total.summy)
  
    
  }
  
  
####Nowcasting da mediana
  
  
  nowcast_mediana<-function(data.inla){
    
   
    
    model <- Y ~ 1 +
      f(Time,
        model = "rw2", 
        hyper = list("prec" = list(prior = "loggamma",
                                   param = c(0.01, 0.01)) 
        )) 
    
    
    
    output0 <- INLA::inla(model,
                          family = "nbinomial",
                          data = data.inla,
                          control.predictor = list(link = 1, compute = T),
                          control.compute = list( config = T, waic=T, dic=T),
                          control.family = list(
                           hyper = list("theta" = list(prior = "loggamma",
                                                       param = c(0.01, 0.01)))  
                         
                           
                          )
    )
    
    srag.samples0.list <- INLA::inla.posterior.sample(n = 1000, output0)
    
    index.missing <- data.inla$Time
    
    
    vector.samples0 <- lapply(X = srag.samples0.list,
                              FUN = function(x, idx = index.missing){
                                stats::rnbinom(n = idx,
                                               mu = exp(x$latent[idx]),
                                               size = x$hyperpar[1]
                                ) * 1
                              } )
    
    ## Step 3: Calculate N_{a,t} for each triangle sample {N_{t,a} : t=Tactual-Dmax+1,...Tactual}
    
    gg.age <- function(x, dados.gg, idx){
      data.aux <- dados.gg
      Tmin <- min(dados.gg$Time[idx])
      data.aux$Y[idx] <- x
      data.aggregated <- data.aux |>
        ## Selecionando apenas os dias faltantes a partir
        ## do domingo da respectiva ultima epiweek
        ## com dados faltantes
        dplyr::filter(Time >= Tmin  ) |>
        dplyr::group_by(Time, date_onset) |>
        dplyr::summarise(
          Y = sum(Y), .groups = "keep"
        )
      data.aggregated
    }
    
    ## Step 4: Applying the age aggregation on each posterior
    tibble.samples.0 <- lapply( X = vector.samples0,
                                FUN = gg.age,
                                dados = data.inla,
                                idx = index.missing)
    
    srag.pred.0 <- dplyr::bind_rows(tibble.samples.0, .id = "sample")
    
    total.summy <- srag.pred.0 |>
      dplyr::group_by(Time, date_onset, sample) |>
      dplyr::summarise(Y = sum(Y, na.rm = T)) |>
      dplyr::group_by(Time, date_onset) |>
      dplyr::summarise(Median = stats::median(Y, na.rm = T),
                       LI = stats::quantile(Y, probs = 0.025, na.rm = T),
                       LS = stats::quantile(Y, probs = 0.975, na.rm = T),
                       LIb = stats::quantile(Y, probs = 0.25, na.rm = T),
                       LSb = stats::quantile(Y, probs = 0.75, na.rm = T),
                       .groups = "drop") %>%
      left_join(data.inla, by=c("Time", "date_onset")) %>%
      dplyr::rename(obs_naive=Y)
    
    
    return(total.summy)
    
    
  }
  
  get_data_inla<-function(dataset, wdw, Dmax, K, use.epiweek=FALSE){
    
    ## K parameter of forecasting
    K.w<-7*K
    use.epiweek<-FALSE
    
    dataset<-dados_srag2
    
    ## Maximum date to be considered on the estimation (Last day)
    DT_max <- max(dataset |>
                    dplyr::pull(var = date_report),
                  na.rm = T) + K.w
    
    
    ## Day of the week of the last day
    DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
    
    ## Ignore data after the last Sunday of recording (Sunday as too)
    aux.trimming.date = ifelse( use.epiweek | DT_max_diadasemana == 6, DT_max_diadasemana + 1, 0)
    
    # Workaround check
    DT.sun.aux <- dt.aux <- Delay <- NULL
    
    
    ## Accounting for the maximum of days on the last week to be used
    data_w <- dataset |>
      dplyr::rename(date_report = date_report,
                    date_onset = dt_sin_pri) |>
      dplyr::filter(date_report <= DT_max - aux.trimming.date) |>
      dplyr::mutate(
        ## Onset date
        # Moving the date to sunday
        DT.sun.aux = as.integer(format(date_onset, "%w")),
        ## Altering the date for the first day of the week
        dt.aux = date_onset -
          # Last recording date (DT_max_diadasemana) is the last day of the new week format
          DT.sun.aux +
          ifelse( use.epiweek, 0, DT_max_diadasemana+1 -
                    ifelse(DT_max_diadasemana+1>DT.sun.aux,7, 0)
          ),
        date_onset = dt.aux - ifelse( date_onset < dt.aux, 7, 0),
        # Recording date
        DT.sun.aux = as.integer(format(date_report, "%w")),
        ## Altering the date for the first day of the week
        dt.aux = date_report -
          # Last recording date (DT_max_diadasemana) is the last day of the new week format
          DT.sun.aux +
          ifelse( use.epiweek, 0, DT_max_diadasemana+1 -
                    ifelse(DT_max_diadasemana+1 > DT.sun.aux,7, 0)
          ),
        date_report = dt.aux - ifelse( date_report < dt.aux, 7, 0),
        Delay = as.numeric(date_report - date_onset) / 7
      ) |>
      dplyr::select(-dt.aux, -DT.sun.aux) |>
      dplyr::filter(Delay >= 0)
    
    
    Tmax <- max(data_w |>
                  dplyr::pull(var = date_onset))
    
    data.inla <- data_w  |>
      ## Filter for dates
      dplyr::filter(date_onset >= Tmax - 7 * wdw,
                    Delay <= Dmax)  |>
      ## Group by on Onset dates, Amounts of delays and Stratum
      dplyr::group_by(date_onset)  |>
      ## Counting
      dplyr::tally(name = "Y")  |>
      dplyr::ungroup()
    
    date_k<-max(data.inla$date_onset) + 7*K
    
    dates <- range(data.inla$date_onset, date_k)
    
    
    tbl.date.aux <- tibble::tibble(
      date_onset = seq(dates[1], dates[2], by = 7)
    )  |>
      tibble::rowid_to_column(var = "Time")
    
    ## Joining auxiliary date tables
    data.inla <- data.inla  |>
      dplyr::right_join(tbl.date.aux)
    
    return(data.inla)
    
  }
  
  

