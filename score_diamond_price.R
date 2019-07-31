
## Load data.
# rm(list=ls())
score_diamond_in <- read.csv("/Users/pranavd/Documents/RWork/R_Projects/diamond_pricing/data/new_dia_fms_cleaned_eg_2.csv")


score_dia_int <- score_diamond_in[,!(names(score_diamond_in) %in% c("cut2", "color2", "ln_carat"))]


score_dia_int$cut2 <- factor(score_dia_int$cut, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"), ordered = TRUE)

score_dia_int$color2 <- ordered(score_dia_int$color, level = c("J","I","H","G","F","E","D"))



 

score_dia_int$clarity2 <- factor(with(score_dia_int, ifelse(clarity2 == 1, 'I1',
                                                    ifelse(clarity2 == 2, 'SI2',
                                                    ifelse(clarity2 == 3, 'SI1',
                                                    ifelse(clarity2 == 4, 'VS2',
                                                    ifelse(clarity2 == 5, 'VS1',
                                                    ifelse(clarity2 == 6, 'VVS2',
                                                    ifelse(clarity2 == 7, 'VVS1',
                                                    ifelse(clarity2 == 8, 'IF',
                                                    'NULL'))))))))), levels = c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1", "IF"), ordered = TRUE)


str(score_dia_int)

score_dia_final <- score_dia_int[,!(names(score_dia_int) %in% c("cut", "color"))]
str(score_dia_final)

str(train_main_diamond)
########################################################################################

library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest

hyper_grid <- expand.grid(
    mtry       = seq(2, 6, by = 1),
    node_size  = seq(3, 9, by = 2),
    samp_size = c(.55, .632, .70, .80),
    OOB_PredErr   = 0
)

# run the training model on grid in order to find out optimal tuning parameters
for(i in 1:nrow(hyper_grid)) {
    
    # train model
    model <- ranger(
        formula         = index2 ~ carat + depth + table + cut2 + color2 + clarity2, 
        data            = train_main_diamond, 
        num.trees       = 500,
        mtry            = hyper_grid$mtry[i],
        min.node.size   = hyper_grid$node_size[i],
        sample.fraction = hyper_grid$samp_size[i],
        seed            = 123
    )
    
    # add OOB error to grid correspinding to each tuning parameter
    hyper_grid$OOB_PredErr[i] <- sqrt(model$prediction.error)
}

# the optimal tuning paramters
hyper_grid[hyper_grid$OOB_PredErr == min(hyper_grid$OOB_PredErr),]

set.seed(1234)
# The optimal model
optimal_ranger_id2 <- ranger(
    formula         = index2 ~ carat + depth + table + cut2 + color2 + clarity2 , 
    data            = train_main_diamond, 
    num.trees       = 500,
    mtry            = 3,
    min.node.size   = 5,
    sample.fraction = .55,
    importance      = 'impurity',
    seed = 123
)
optimal_ranger_id2$prediction.error

table(predict(optimal_ranger_id2, test_main_diamond)$predictions , test_main_diamond$index2)
(120+118)/(7574+120+118+6517)
1-mean(predict(optimal_ranger_id2, test_main_diamond)$predictions == test_main_diamond$index2)


########################################################################################
# https://machinelearningmastery.com/difference-between-a-batch-and-an-epoch/
# https://heartbeat.fritz.ai/binary-classification-using-keras-in-r-ef3d42202aaa
library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)
data_train_id2 <- train_main_diamond[,!(names(train_main_diamond) %in% c("index1", "id", "index2", "price"))]
data_train_id2 %<>% mutate_if(is.factor, as.numeric)
# Build your own `normalize()` function
normalize <- function(x) {
    num <- x - min(x)
    denom <- max(x) - min(x)
    return (num/denom)
}
# Normalize the data
training_id2_norm <- as.data.frame((lapply(data_train_id2, normalize)))
training_id2 <- as.matrix(training_id2_norm)
# training_target_id2 <- as.matrix(as.numeric(as.character(train_main_diamond$index2)))


data_test_id2 <- test_main_diamond[,!(names(test_main_diamond) %in% c("index1", "id", "index2", "price"))]
data_test_id2 %<>% mutate_if(is.factor, as.numeric)

data_test_id2_norm <- as.data.frame((lapply(data_test_id2, normalize)))

test_id2 <- as.matrix(data_test_id2_norm)
# test_target_id2 <- as.matrix(as.numeric(as.character(test_main_diamond$index2)))

# fine tune the model

training_target_id2 <- to_categorical(as.numeric(as.character(train_main_diamond$index2)))
test_target_id2 <- to_categorical(as.numeric(as.character(test_main_diamond$index2)))


set.seed(1234)
# Create Model
model <- keras_model_sequential()
model %>% 
    layer_dense(units = 5, activation = 'relu', input_shape = c(6)) %>%
    layer_dense(units = 3, activation = 'relu') %>%
    layer_dense(units = 2, activation = 'sigmoid')
summary(model)
# Compile
model %>% compile(loss = 'binary_crossentropy',
                  optimizer = 'adam',
                  metrics = c('accuracy'))
set.seed(1234)
# Fit Model
mymodel <- model %>%
    fit(training_id2,
        training_target_id2,
        epochs = 100,
        batch_size = 32,
        validation_split = 0.3)

# sparse_categorical_crossentropy
# categorical_crossentropy
# binary_crossentropy


model %>% evaluate(test_id2, test_target_id2)

pred_id2 <- model %>% predict_classes(test_id2)


# misclassification table for neural net - prediction index2
table(pred_id2, test_main_diamond$index2)
1-mean(pred_id2 == test_main_diamond$index2)

# Missclassification table for Random forest - prediction index2
table(predict(optimal_ranger_id2, test_main_diamond)$predictions , test_main_diamond$index2)
1-mean(predict(optimal_ranger_id2, test_main_diamond)$predictions == test_main_diamond$index2)
# random forest model has lower test error. we will use random forest to predict index2
########################################################################################
# take the predictions for index2 from neural net to merge with scoring dataset
score_dia_final$index2 <- as.factor(as.character(predict(optimal_ranger_id2, score_dia_final)$predictions))

########################################################################################
# Model Selection
########################################################################################

# validation MSE - Random Forest
tuned_rf_mse
# validation MSE - Neural Network
tuned_nn_mse
# validation MSE - Decision Tree
pruned_dec_tree_mse
# validation MSE Linear Regression - No variable selection
lm_noselec_mse
# validation MSE Linear Regression - Forward selection
fwd_sel_mse
# validation MSE Linear Regression - Backward selection
bkwd_sel_mse
# validation MSE Linear Regression - Stepwise selection
stws_sel_mse
# validation MSE - Decision Tree
pruned_dec_tree_mse

# Model with least validation MSE is Random forest


########################################################################################
# Score the diamonds using Random Forst model 
########################################################################################

sigma <- sd(predict(optimal_ranger, score_dia_final)$predictions)

mean_dia <- mean(predict(optimal_ranger, score_dia_final)$predictions) 

#99% Lower CI for sum
(mean_dia - (qnorm(.99)*sigma/sqrt(nrow(score_dia_final))))*nrow(score_dia_final)
#99% upper CI for sum
(mean_dia + (qnorm(.99)*sigma/sqrt(nrow(score_dia_final))))*nrow(score_dia_final)
#Sum of diamond price
sum(predict(optimal_ranger, score_dia_final)$predictions)
########################################################################################




