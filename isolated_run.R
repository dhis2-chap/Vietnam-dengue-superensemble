
source("train.R")
source("predict.R")

#test with random data
train_chap("example_data/training_data.csv", "model")
predict_chap("model", "example_data/historic_data.csv", "example_data/future_data.csv", "example_data/predictions.csv", "graph_fn")


#testing

