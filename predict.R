# left side is the names used in the code, right side is the internal names in CHAP
# Cases = number of cases
# E = population
# month = month
# ID_year = year
# ID_spat = location
# rainsum = rainfall
# meantemperature = mean_temperature

options(warn=1)

#libraries


#the predict function
predict_chap <- function(model_fn, hist_fn, future_fn, preds_fn, graph_fn){
  df <- read.csv(future_fn)
  
  #00 Functions
    #A lot of helper functions used throughout the next sections 
  
  #01 Load packages
    #will do this outside the function, have to make the docker image
  
  #02 Annual to monthly
    # probablye not neccessary for CHAP, but maybe, could improve inference
  
  #03 Pre-processing
  
  den <- read_obs("dengue") #this calls a funcion from 00 that searches through all
    # files with "dengue" and reads the first one, maybe alphabetically
  d1  <- min(ymd(den$tsdatetime)) #gets the min date on ymd format
  d2  <- max(den$tsdatetime) #gets the max date on some format?
  
  rm(den) #removes the object den from the workspace?
  
  myData <- hist_data("^observed.*?\\.csv") #loads cleaned historic data
  
  # forec
  fore <- fore_data("^forecast.*?\\.csv") # loads the cleaned future/forecast data
  
  # unlink
  unlink(paste0(file.path(input), "/",
                list.files(path=file.path(input),
                           pattern="population|rural|urban")))
    #this deletes some of the files created earlier names population, rural or urban
  
  # Correct tsdatetime - converts the dates in the tsdatetime column
  myData$tsdatetime <- ceiling_date(ymd(paste(year(myData$tsdatetime),
                                              month(myData$tsdatetime),
                                              "01", sep="-")), 'month') - days(1)
  fore$tsdatetime <- ceiling_date(ymd(paste(year(fore$tsdatetime),
                                            month(fore$tsdatetime),
                                            "01", sep="-")), 'month') - days(1)
  
  #library(lubridate)
  #ceiling_date(ymd("2020-01-01"), "month") - days(1)
  
  # Ensure unique dates - if non-unique rows they are averaged to one row instead
  myData %<>% group_by(areaid, tsdatetime) %>%
    dplyr::summarise_all(mean, na.rm=TRUE) 
  
  fore %<>% group_by(areaid, tsdatetime, ensmember) %>%
    dplyr::summarise_all(mean, na.rm=TRUE)
  
  # geog, reads the geojson file with id-column province, should be "id" in CHAP
  myMap <- rgdal::readOGR(file.path(input), "province") 
  
  # wrangling - makes new column for hist and future data
  minDate           <- min(myData$tsdatetime)
  names(myData)     <- gsub(" ", "_", names(myData)) # changes " " to "_" in colnames
  myData$year       <- factor(lubridate::year(myData$tsdatetime))
  myData$month      <- factor(lubridate::month(myData$tsdatetime))
  myData$dtr        <- myData$maximum_temperature - myData$minimum_temperature
  fore$year         <- factor(lubridate::year(fore$tsdatetime))
  fore$month        <- factor(lubridate::month(fore$tsdatetime))
  fore$dtr          <- fore$maximum_temperature - fore$minimum_temperature
  
  myData$incidence  <- (myData$dengue_cases / myData$population) * 1e5
  
  myData            <- inner_join(myData, myMap@data) 
    #@data gets the tabular data from myMap, then the dataframes are knitted together
    # and only locations present in both are kept, possibly also other shared columnnames
    # fails if no shared columnnames, presumably the only shared name is "province"
  
  # times for the prediction period
  d1 <- min(fore$tsdatetime)
  d2 <- max(fore$tsdatetime)
  
  # add land to forecast - I do not understand this part - the monthly limits d1 and d2
    # should make it so now annual data from annual2 is included, or is it not annual??
  df1         <- sel_land(annual2)  #annual2 is a wide format df of the annual data
  fore$areaid <- factor(fore$areaid)
  fore        <- inner_join(fore, df1)
  
  # sel
  newData <- myData %>%
    ungroup() %>%
    dplyr::select(areaid, tsdatetime,
                  year, month, population,
                  minimum_temperature, 
                  maximum_temperature,
                  precipitation_amount_per_day, 
                  nino34_anomaly,
                  specific_surface_humidity, dtr,
                  wind_speed, periurban_landcover, 
                  urban_landcover, rural_landcover, 
                  dengue_cases) %>%
    dplyr::mutate(ensmember="tsvalue_ensemble_00") %>%
    data.table()
  
  newFore <- fore %>%
    dplyr::mutate(dengue_cases=NA) %>%
    dplyr::select(areaid, tsdatetime,
                  year, month, population, 
                  minimum_temperature, 
                  maximum_temperature,
                  precipitation_amount_per_day, 
                  nino34_anomaly,
                  specific_surface_humidity, dtr,
                  wind_speed, periurban_landcover, 
                  urban_landcover, rural_landcover, 
                  dengue_cases, ensmember) %>%
    data.table()
  
  newData <- rbind(newData, newFore)
  
  # rollm
  
  copytmin <- function(x){
    x[is.na(x)] <- 0
    x <- x + mintemp$baseline
  }
  
  copytmax <- function(x){
    x[is.na(x)] <- 0
    x <- x + maxtemp$baseline
  }
  
  copypre <- function(x){
    x[is.na(x)] <- 0
    x <- x + meanpre$baseline
  }
  
  copyshum <- function(x){
    x[is.na(x)] <- 0
    x <- x + meanshum$baseline
  }
  
  copydtr <- function(x){
    x[is.na(x)] <- 0
    x <- x + meandtr$baseline
  }
  
  copynino <- function(x){
    x[is.na(x)] <- 0
    x <- x + meananom$baseline
  }
  
  movav <- function(x) {
    rollapply(x, width=3, FUN=mean,
              fill=NA, align="right")
  }
  
  movav2 <- function(x) {
    rollapply(x, width=4, FUN=mean,
              fill=NA, align="right")
    
  }
  
  mintemp <- dplyr::select(newData, areaid, tsdatetime, minimum_temperature,
                           ensmember) %>%
    tidyr::spread(ensmember, minimum_temperature) %>%
    dplyr::rename(baseline=tsvalue_ensemble_00) %>%
    dplyr::mutate(baseline=replace_na(baseline, 0)) 
  
  maxtemp <- dplyr::select(newData, areaid, tsdatetime, maximum_temperature,
                           ensmember) %>%
    tidyr::spread(ensmember, maximum_temperature) %>%
    dplyr::rename(baseline=tsvalue_ensemble_00) %>%
    dplyr::mutate(baseline=replace_na(baseline, 0)) 
  
  meanpre <- dplyr::select(newData, areaid, tsdatetime, 
                           precipitation_amount_per_day, ensmember) %>%
    tidyr::spread(ensmember, precipitation_amount_per_day) %>%
    dplyr::rename(baseline=tsvalue_ensemble_00) %>%
    dplyr::mutate(baseline=replace_na(baseline, 0)) 
  
  meanshum <- dplyr::select(newData, areaid, tsdatetime,
                            specific_surface_humidity, ensmember) %>%
    tidyr::spread(ensmember, specific_surface_humidity) %>%
    dplyr::rename(baseline=tsvalue_ensemble_00) %>%
    dplyr::mutate(baseline=replace_na(baseline, 0)) 
  
  meandtr <- dplyr::select(newData, areaid, tsdatetime, dtr,
                           ensmember) %>%
    tidyr::spread(ensmember, dtr) %>%
    dplyr::rename(baseline=tsvalue_ensemble_00) %>%
    dplyr::mutate(baseline=replace_na(baseline, 0)) 
  
  meananom <- dplyr::select(newData, areaid, tsdatetime, nino34_anomaly,
                            ensmember) %>%
    tidyr::spread(ensmember, nino34_anomaly) %>%
    dplyr::rename(baseline=tsvalue_ensemble_00) %>%
    dplyr::mutate(baseline=replace_na(baseline, 0)) 
  
  mintemp %<>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              copytmin) %>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              movav) %>%
    dplyr::filter(tsdatetime >=d1) %>%
    tidyr::gather(ensmember, tmin02, -(areaid:baseline)) %>%
    dplyr::select(-baseline) %>%
    data.table()
  
  maxtemp %<>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              copytmax) %>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              movav) %>%
    dplyr::filter(tsdatetime >=d1) %>%
    tidyr::gather(ensmember, tmax02, -(areaid:baseline)) %>%
    dplyr::select(-baseline) %>%
    data.table()
  
  meanpre %<>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              copypre) %>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              movav) %>%
    dplyr::filter(tsdatetime >=d1) %>%
    tidyr::gather(ensmember, pre02, -(areaid:baseline)) %>%
    dplyr::select(-baseline) %>%
    data.table()
  
  meanshum %<>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              copyshum) %>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              movav) %>%
    dplyr::filter(tsdatetime >=d1) %>%
    tidyr::gather(ensmember, shum02, -(areaid:baseline)) %>%
    dplyr::select(-baseline) %>%
    data.table()
  
  meandtr %<>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              copydtr) %>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              movav) %>%
    dplyr::filter(tsdatetime >=d1) %>%
    tidyr::gather(ensmember, dtr02, -(areaid:baseline)) %>%
    dplyr::select(-baseline) %>%
    data.table()
  
  meananom %<>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              copynino) %>%
    mutate_at(vars(matches('tsvalue_ensemble_')),
              movav2) %>%
    dplyr::filter(tsdatetime >=d1) %>%
    tidyr::gather(ensmember, nino3403, -(areaid:baseline)) %>%
    dplyr::select(-baseline) %>%
    data.table()
  
  newData %<>% group_by(areaid, ensmember) %>%
    dplyr::mutate(
      tmin02=rollapply(minimum_temperature, width=3, FUN=mean,
                       fill=NA, align="right"),
      tmax02=rollapply(maximum_temperature, width=3, FUN=mean,
                       fill=NA, align="right"),
      pre02=rollapply(precipitation_amount_per_day, width=3, FUN=mean,
                      fill=NA, align="right"),
      shum02=rollapply(specific_surface_humidity, width=3, FUN=mean,
                       fill=NA, align="right"),
      dtr02=rollapply(dtr, width=3, FUN=mean,
                      fill=NA, align="right"),
      nino3403=rollapply(nino34_anomaly, width=4, FUN=mean,
                         fill=NA, align="right")) 
  newData$tmin02[newData$tsdatetime >= d1] <- NA
  newData$tmax02[newData$tsdatetime >= d1] <- NA
  newData$pre02[newData$tsdatetime >= d1] <- NA
  newData$shum02[newData$tsdatetime >= d1] <- NA
  newData$dtr02[newData$tsdatetime >= d1] <- NA
  
  newData$areaid  <- as.numeric(as.character(newData$areaid))
  mintemp$areaid  <- as.numeric(as.character(mintemp$areaid))
  maxtemp$areaid  <- as.numeric(as.character(maxtemp$areaid))
  meanpre$areaid  <- as.numeric(as.character(meanpre$areaid))
  meanshum$areaid <- as.numeric(as.character(meanshum$areaid))
  meandtr$areaid  <- as.numeric(as.character(meandtr$areaid))
  meananom$areaid <- as.numeric(as.character(meananom$areaid))
  
  newData %<>% left_join(mintemp, by=c('areaid', 'tsdatetime', 'ensmember')) %>%
    dplyr::mutate(tmin02=coalesce(tmin02.x, tmin02.y)) %>%
    dplyr::select(-tmin02.x, -tmin02.y)
  newData %<>% left_join(maxtemp, by=c('areaid', 'tsdatetime', 'ensmember')) %>%
    dplyr::mutate(tmax02=coalesce(tmax02.x, tmax02.y)) %>%
    dplyr::select(-tmax02.x, -tmax02.y)
  newData %<>% left_join(meanpre, by=c('areaid', 'tsdatetime', 'ensmember')) %>%
    dplyr::mutate(pre02=coalesce(pre02.x, pre02.y)) %>%
    dplyr::select(-pre02.x, -pre02.y)
  newData %<>% left_join(meanshum, by=c('areaid', 'tsdatetime', 'ensmember')) %>%
    dplyr::mutate(shum02=coalesce(shum02.x, shum02.y)) %>%
    dplyr::select(-shum02.x, -shum02.y)
  newData %<>% left_join(meandtr, by=c('areaid', 'tsdatetime', 'ensmember')) %>%
    dplyr::mutate(dtr02=coalesce(dtr02.x, dtr02.y)) %>%
    dplyr::select(-dtr02.x, -dtr02.y)
  newData %<>% left_join(meananom, by=c('areaid', 'tsdatetime', 'ensmember')) %>%
    dplyr::mutate(nino3403=coalesce(nino3403.x, nino3403.y)) %>%
    dplyr::select(-nino3403.x, -nino3403.y)
  
  # New season
  newData %<>% dplyr::mutate(
    date2=tsdatetime %m-% months(6),
    month2=month(date2),
    year2=year(date2)
  )
  
  # IDs
  newData$areaid       <- factor(newData$areaid)
  newData$ID.area      <- as.numeric(newData$areaid)
  newData$ID.area1     <- as.numeric(newData$areaid)
  newData$ID.area2     <- as.numeric(newData$areaid)
  newData$ID.year      <- as.numeric(as.character(newData$year2))
  newData$ID.year1     <- as.numeric(as.character(newData$year2))
  newData$ID.month     <- as.numeric(as.character(newData$month2))
  newData$ID.month1    <- as.numeric(as.character(newData$month2))
  
  # Lagged obs
  newData %<>% group_by(areaid) %>%
    dplyr::mutate(dengueL1=lag(dengue_cases, 1))
  
  rm(myData, fore)
  
  
  # ----------------
  # Eof
  # ----------------
  
  #04 Fit models
  temp    <- poly2nb(myMap, queen=FALSE) #myMap is defined earlier in 03 pre-processing
  nb2INLA("vnm_graph", temp) #creates a file in the working directory
  vnm.adj <- paste(getwd(), "/vnm_graph", sep="") #which is read in here
  #maybe use a non-relative file path above, not sure
  
  #Model 0 - bym + iid_t_y + ar1_t_m, the last to are grouped on some area column
    # so the iid is per region per year and the ar1 is monthly for each region
    # with an iid distribuition, so shared hyperparameters? not sure what loglag is
  f0 <- dengue_cases ~ loglag +
    f(ID.area, model='bym', graph=vnm.adj, 
      adjust.for.con.comp=FALSE, constr=TRUE, 
      scale.model=TRUE, 
      hyper = list(prec.unstruct=list(prior='pc.prec',param=c(3, 0.01)),
                   prec.spatial=list(prior='pc.prec', param=c(3, 0.01)))) +
    f(ID.year, model='iid', 
      hyper=list(prec = list(prior='pc.prec',param = c(3, 0.01))),
      group=ID.area2, 
      control.group=list(model='iid',hyper = list(
        prec = list(prior='pc.prec',param=c(3, 0.01))))) + 
    f(ID.month1, model='ar1', 
      hyper = list(prec=list(prior='pc.prec',param=c(3, 0.01)),
                   rho = list(prior='pc.cor1', param = c(0.5, 0.75))),
      group=ID.area1, 
      control.group = list(model='iid',
                           hyper=list(prec=list(prior='pc.prec',
                                                param=c(3, 0.01)))))
  
  #Model 0 needs loglag, ID.area, ID.area1, ID.area2, ID.year, ID.month1
  
  #Model 1 - adds 6 environmental features
  f1 <- dengue_cases ~ loglag +
    # Spatial random effect
    f(ID.area, model='bym', graph=vnm.adj, 
      adjust.for.con.comp=FALSE, constr=TRUE, 
      scale.model=TRUE, 
      # Precision of unstructure random effects
      hyper = list(prec.unstruct=list(prior='pc.prec',param=c(3, 0.01)),
                   prec.spatial=list(prior='pc.prec', param=c(3, 0.01)))) +
    # Year random effect
    f(ID.year, model='iid', 
      hyper=list(prec = list(prior='pc.prec',param = c(3, 0.01))),
      group=ID.area2, 
      control.group=list(model='iid',hyper = list(
        prec = list(prior='pc.prec',param=c(3, 0.01))))) + 
    f(ID.month1, model='ar1', 
      hyper = list(prec=list(prior='pc.prec',param=c(3, 0.01)),
                   # Autocorrelation
                   rho = list(prior='pc.cor1', param = c(0.5, 0.75))),
      group=ID.area1, 
      control.group = list(model='iid',
                           hyper=list(prec=list(prior='pc.prec',
                                                param=c(3, 0.01))))) +
    # Remaining fixed effects
    periurban_landcover + urban_landcover +
    shum02 + wind_speed + dtr02 + nino3403 
  
  #Model 1 needs loglag, ID.area, ID.area1, ID.area2, ID.year, ID.month1 and 6 covariates
  
  #Model 2 - removes 2 of the 6 added covariates above
  f2 <- dengue_cases ~ loglag +
    f(ID.area, model='bym', graph=vnm.adj, 
      adjust.for.con.comp=FALSE, constr=TRUE, 
      scale.model=TRUE, 
      hyper = list(prec.unstruct=list(prior='pc.prec',param=c(3, 0.01)),
                   prec.spatial=list(prior='pc.prec', param=c(3, 0.01)))) +
    f(ID.year, model='iid', 
      hyper=list(prec = list(prior='pc.prec',param = c(3, 0.01))),
      group=ID.area2, 
      control.group=list(model='iid',hyper = list(
        prec = list(prior='pc.prec',param=c(3, 0.01))))) + 
    f(ID.month1, model='ar1', 
      hyper = list(prec=list(prior='pc.prec',param=c(3, 0.01)),
                   rho = list(prior='pc.cor1', param = c(0.5, 0.75))),
      group=ID.area1, 
      control.group = list(model='iid',
                           hyper=list(prec=list(prior='pc.prec',
                                                param=c(3, 0.01))))) + 
    # Remaing fixed effects
    periurban_landcover + urban_landcover + 
    shum02 + dtr02 
  
  #Model 2 needs loglag, ID.area, ID.area1, ID.area2, ID.year, ID.month1 and 4 covariates
  
  #Model 3 - uses four earlier covariates and adds a new one, tmin02
  f3 <- dengue_cases ~ loglag +
    f(ID.area, model='bym', graph=vnm.adj, 
      adjust.for.con.comp=FALSE, constr=TRUE, 
      scale.model=TRUE, 
      hyper = list(prec.unstruct=list(prior='pc.prec',param=c(3, 0.01)),
                   prec.spatial=list(prior='pc.prec', param=c(3, 0.01)))) +
    f(ID.year, model='iid', 
      hyper=list(prec = list(prior='pc.prec',param = c(3, 0.01))),
      group=ID.area2, 
      control.group=list(model='iid',hyper = list(
        prec = list(prior='pc.prec',param=c(3, 0.01))))) + 
    f(ID.month1, model='ar1', 
      hyper = list(prec=list(prior='pc.prec',param=c(3, 0.01)),
                   rho = list(prior='pc.cor1', param = c(0.5, 0.75))),
      group=ID.area1, 
      control.group = list(model='iid',
                           hyper=list(prec=list(prior='pc.prec',
                                                param=c(3, 0.01))))) + 
    # Remaing fixed effects
    periurban_landcover + urban_landcover + 
    tmin02 + dtr02 + nino3403
  
  #Model 3 needs loglag, ID.area, ID.area1, ID.area2, ID.year, ID.month1 and 5 covariates(1 new)
  
  #Model 4 - 3 from earlier and one new, tmax02
  f4 <- dengue_cases ~ loglag +
    f(ID.area, model='bym', graph=vnm.adj, 
      adjust.for.con.comp=FALSE, constr=TRUE, 
      scale.model=TRUE, 
      hyper = list(prec.unstruct=list(prior='pc.prec',param=c(3, 0.01)),
                   prec.spatial=list(prior='pc.prec', param=c(3, 0.01)))) +
    f(ID.year, model='iid', 
      hyper=list(prec = list(prior='pc.prec',param = c(3, 0.01))),
      group=ID.area2, 
      control.group=list(model='iid',hyper = list(
        prec = list(prior='pc.prec',param=c(3, 0.01))))) + 
    f(ID.month1, model='ar1', 
      hyper = list(prec=list(prior='pc.prec',param=c(3, 0.01)),
                   rho = list(prior='pc.cor1', param = c(0.5, 0.75))),
      group=ID.area1, 
      control.group = list(model='iid',
                           hyper=list(prec=list(prior='pc.prec',
                                                param=c(3, 0.01))))) + 
    # Remaing fixed effects
    periurban_landcover + urban_landcover + 
    tmin02 + tmax02
  
  #Model 4 needs loglag, ID.area, ID.area1, ID.area2, ID.year, ID.month1 and 4 covariates(1 new)
  
  #Model 5 - 3 earlier covariates
  f5 <- dengue_cases ~ loglag +
    f(ID.area, model='bym', graph=vnm.adj, 
      adjust.for.con.comp=FALSE, constr=TRUE, 
      scale.model=TRUE, 
      hyper = list(prec.unstruct=list(prior='pc.prec',param=c(3, 0.01)),
                   prec.spatial=list(prior='pc.prec', param=c(3, 0.01)))) +
    f(ID.year, model='iid', 
      hyper=list(prec = list(prior='pc.prec',param = c(3, 0.01))),
      group=ID.area2, 
      control.group=list(model='iid',hyper = list(
        prec = list(prior='pc.prec',param=c(3, 0.01))))) + 
    f(ID.month1, model='ar1', 
      hyper = list(prec=list(prior='pc.prec',param=c(3, 0.01)),
                   rho = list(prior='pc.cor1', param = c(0.5, 0.75))),
      group=ID.area1, 
      control.group = list(model='iid',
                           hyper=list(prec=list(prior='pc.prec',
                                                param=c(3, 0.01))))) + 
    # Remaing fixed effects
    periurban_landcover + urban_landcover + 
    wind_speed
  
  #Model 5 needs loglag, ID.area, ID.area1, ID.area2, ID.year, ID.month1 and 3 covariates
  
  #Fitting the models
    #It seems like they fit the model for the next timepoint, then add that do the dataset
    #and predict for the next one iteratively for each of the 6 models
    #the code is very repetative, maybe a nested for loop instead? Also very time consuming
    #Also not sure if they get samples
  
  #there are some ensemble stuff as well, some weighting with likelihoods and means etc
  #they also use the function fitmargBMA2 for this purpose, not user defined
  
  
  
  
  write.csv(df_preds, preds_fn, row.names = FALSE)
}

#code for running the file from the command line interface used by CHAP
args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  model_fn <- args[1]
  hist_fn <- args[2]
  future_fn <- args[3]
  preds_fn <- args[4]
  graph_fn <- args[5]
  
  predict_chap(model_fn, hist_fn, future_fn, preds_fn, graph_fn)
}



### An example from chap ewars of the standard structure in predict.R--------
library(INLA)
library(dlnm)
library(dplyr)

#for spatial effects
library(sf)
library(spdep)

predict_chap <- function(model_fn, hist_fn, future_fn, preds_fn, graph_fn){
  #model <- readRDS(file = model_fn) #would normally load a model here
  
  df <- read.csv(future_fn)
  df$Cases <- rep(NA, nrow(df))
  df$disease_cases <- rep(NA, nrow(df)) #so we can rowbind it with historic
  
  historic_df = read.csv(hist_fn)
  df <- rbind(historic_df, df)
  df <- mutate(df, ID_year = ID_year - min(df$ID_year) + 1)
  
  #adding a counting variable for the months like 1, ..., 12, 13, ...
  #could also do years*12 + months, but fails for weeks
  df <-group_by(df, location) |>
    mutate(month_num = row_number())
  
  basis_meantemperature <- crossbasis(df$meantemperature, lag=3,
                                      argvar = list(fun = "ns", knots = equalknots(df$meantemperature, 2)),
                                      arglag = list(fun = "ns", knots = 3/2), group = df$ID_spat)
  colnames(basis_meantemperature) = paste0("basis_meantemperature.", colnames(basis_meantemperature))
  
  basis_rainsum <- crossbasis(df$rainsum, lag=3,
                              argvar = list(fun = "ns", knots = equalknots(df$rainsum, 2)),
                              arglag = list(fun = "ns", knots = 3/2), group = df$ID_spat)
  colnames(basis_rainsum) = paste0("basis_rainsum.", colnames(basis_rainsum))
  
  df$ID_spat <- as.factor(df$ID_spat)
  df$ID_spat_num <- as.numeric(as.factor(df$ID_spat))
  
  df <- cbind(df, basis_meantemperature, basis_rainsum)
  
  # the formula without spatial smoothing and with some specified PC priors
  #lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid', hyper=list(prec = list(prior = "pc.prec",
  #  param = c(1, 0.01)))) + f(month_num, model = "rw1", scale.model = T,
  #  replicate = ID_spat_num, hyper=list(prec = list(prior = "pc.prec", param = c(1, 0.01)))) +
  #  f(month, model='rw1', cyclic=T, scale.model=T, hyper=list(prec = list(prior = "pc.prec",
  #  param = c(1, 0.01)))) + basis_meantemperature + basis_rainsum
  
  lagged_formula <- Cases ~ 1 + f(ID_spat, model='iid') + f(month_num, model = "rw1", scale.model = T,
                                                            replicate = ID_spat_num) +
    f(month, model='rw1', cyclic=T, scale.model=T) + basis_meantemperature + basis_rainsum
  
  
  model <- inla(formula = lagged_formula, data = df, family = "nbinomial", offset = log(E),
                control.inla = list(strategy = 'adaptive'),
                control.compute = list(config = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, prec.intercept = 0.1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE),
                verbose = F, safe=FALSE)
  
  casestopred <- df$Cases # response variable
  
  # Predict only for the cases where the response variable is missing
  idx.pred <- which(is.na(casestopred)) #this then also predicts for historic values that are NA, not ideal
  mpred <- length(idx.pred)
  s <- 1000
  y.pred <- matrix(NA, mpred, s)
  # Sample parameters of the model
  xx <- inla.posterior.sample(s, model)  # This samples parameters of the model
  xx.s <- inla.posterior.sample.eval(function(idx.pred) c(theta[1], Predictor[idx.pred]), xx, idx.pred = idx.pred) # This extracts the expected value and hyperparameters from the samples
  
  # Sample predictions
  for (s.idx in 1:s){
    xx.sample <- xx.s[, s.idx]
    y.pred[, s.idx] <- rnbinom(mpred,  mu = exp(xx.sample[-1]), size = xx.sample[1])
  }
  
  # make a dataframe where first column is the time points, second column is the location, rest is the samples
  # rest of columns should be called sample_0, sample_1, etc
  new.df = data.frame(time_period = df$time_period[idx.pred], location = df$location[idx.pred], y.pred)
  colnames(new.df) = c('time_period', 'location', paste0('sample_', 0:(s-1)))
  
  # Write new dataframe to file
  write.csv(new.df, preds_fn, row.names = FALSE)
  #saveRDS(model, file = model_fn) # to evaluate the model
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) >= 1) {
  model_fn <- args[1]
  hist_fn <- args[2]
  future_fn <- args[3]
  preds_fn <- args[4]
  graph_fn <- args[5]
  
  predict_chap(model_fn, hist_fn, future_fn, preds_fn, graph_fn)
}

