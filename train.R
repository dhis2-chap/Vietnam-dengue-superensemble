
options(warn=1)

train_chap <- function(train_fn, model_fn){
  #not used by this model
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2) {
  train_fn <- args[1]
  model_fn <- args[2]
  
  train_chap(train_fn, model_fn)
}