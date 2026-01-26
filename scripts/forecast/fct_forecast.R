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
  
  
  nowcast_sem_correcao<-function(data, wdw, k ){
    

    
    ##Data máxima rport
    DT_max <- max(data$date_report) + 7*k
    
    ##Data máxima do dia da semana
    DT_max_diadasemana <- as.integer(format(DT_max, "%w"))
    
    data_w <- data |>
      dplyr::filter(date_report <= DT_max ) |>
      dplyr::mutate(
        ## Altering the date for the first day of the week
        date_onset = dt_sin_pri -
          as.integer(format(dt_sin_pri, "%w")) -
          (6-DT_max_diadasemana),
        date_report = date_report -
          as.integer(format(date_report, "%w")) -
          (6-DT_max_diadasemana))
    
    ####Dado INLA
    ## Parameters of Nowcasting estimate
    
    Tmax <- max(data_w |>
                  dplyr::pull(var = date_onset))
    
    
    data.inla <- data_w  |>
      ## Filter for dates
      dplyr::filter(date_onset >= Tmax - 7 * wdw)  |>
      ## Group by on Onset dates, Amounts of delays and Stratum
      dplyr::group_by(date_onset)  |>
      ## Counting
      dplyr::tally(name = "Y")  |>
      dplyr::ungroup()
    
 
    
    ###Com forecast
    date_k<-max(data.inla$date_onset) + 7*k
    
    dates <- range(data.inla$date_onset, date_k)
  
    
    tbl.date.aux <- tibble::tibble(
      date_onset = seq(dates[1], dates[2], by = 7)
    )  |>
      tibble::rowid_to_column(var = "Time")
    
    ## Joining auxiliary date tables
    data.inla <- data.inla  |>
      dplyr::right_join(tbl.date.aux)
    
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
  
  
  nowcast_mediana<-function(data, agente){
    
    data.inla<- data  %>%
      filter (virus==agente) %>%
      filter(!is.na(Median)) %>%
      mutate(date_onset=ymd(dt_event))%>%
      mutate(Median=round(Median))%>%
      select (date_onset, Median) %>%
      mutate(Time=seq(1:n())) %>%
      mutate(Median = if_else(row_number() == n(), as.numeric(NA), Median))%>%
      rename(Y=Median)
    
    
    
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
  

