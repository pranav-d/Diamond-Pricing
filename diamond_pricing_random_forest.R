
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest

  

# for reproduciblity
# set.seed(1234)
# # default RF model
# rf_default_model <- randomForest(formula = price ~ carat + depth + table + cut2 + color2 + clarity2 + index2, data = train_main_diamond)
# rf_default_model
# plot(rf_default_model)

# drop the un-necessary variables
x_vars <- setdiff(names(train_main_diamond), c("price", "index1", "id"))

# randomForest speed - Check the time taken by default model
# system.time(
#     rfmod_randomForest <- randomForest(
#         formula =  price ~ carat + depth + table + cut2 + color2 + clarity2 + index2, 
#         data    = train_main_diamond, 
#         ntree   = 500,
#         mtry    = floor(length(x_vars) / 3)
#     )
# )
##    user  system elapsed 
##  55.371   0.590  57.364

# ranger speed - Time taken by faster implementation of random Forest using ranger 
# system.time(
#     rfmod_ranger <- ranger(
#         formula   = price ~ carat + depth + table + cut2 + color2 + clarity2 + index2, 
#         data      = train_main_diamond, 
#         num.trees = 500,
#         mtry      = floor(length(x_vars) / 3)
#     )
# )
# user  system elapsed 
# 12.913   0.442   4.328 

# create the grid for different tuning parameters for random forest
# mtry -  the number of variables to randomly sample as candidates at each split. 
# nodesize - minimum number of samples within the terminal nodes. Controls the complexity of the trees. Smaller node size allows for deeper, more complex trees and smaller node results in shallower trees
# sampsize - Determines the number of samples to train on.
hyper_grid <- expand.grid(
    mtry       = seq(2, 7, by = 1),
    node_size  = seq(3, 9, by = 2),
    samp_size = c(.55, .632, .70, .80),
    OOB_RMSE   = 0
)

# run the training model on grid in order to find out optimal tuning parameters
for(i in 1:nrow(hyper_grid)) {
    
    # train model
    model <- ranger(
        formula         = price ~ carat + depth + table + cut2 + color2 + clarity2 + index2, 
        data            = train_main_diamond, 
        num.trees       = 500,
        mtry            = hyper_grid$mtry[i],
        min.node.size   = hyper_grid$node_size[i],
        sample.fraction = hyper_grid$samp_size[i],
        seed            = 123
    )
    
    # add OOB error to grid correspinding to each tuning parameter
    hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

# the optimal tuning paramters
hyper_grid[hyper_grid$OOB_RMSE == min(hyper_grid$OOB_RMSE),]

# check the OOB RMSE - Repeat the model to get the better expectation of error rate
OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
    
    optimal_ranger <- ranger(
        formula         = price ~ carat + depth + table + cut2 + color2 + clarity2 + index2, 
        data            = train_main_diamond, 
        num.trees       = 500,
        mtry            = 5,
        min.node.size   = 5,
        sample.fraction = .55,
        importance      = 'impurity'
    )
    
    OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}

hist(OOB_RMSE, breaks = 20)
mean(OOB_RMSE)

# Highest to least important variables in the RF Model
optimal_ranger$variable.importance

# The optimal model
optimal_ranger <- ranger(
    formula         = price ~ carat + depth + table + cut2 + color2 + clarity2 + index2, 
    data            = train_main_diamond, 
    num.trees       = 500,
    mtry            = 5,
    min.node.size   = 5,
    sample.fraction = .55,
    importance      = 'impurity',
    seed = 123
)


# Random Forest ASE  - for validation data
tuned_rf_mse <- mean((predict(optimal_ranger, test_main_diamond)$predictions - test_main_diamond$price)^2)
tuned_rf_mse






