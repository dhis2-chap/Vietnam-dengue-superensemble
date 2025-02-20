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
  
  #04 Fit models
  temp    <- poly2nb(myMap, queen=FALSE) #myMap is defined earlier
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

