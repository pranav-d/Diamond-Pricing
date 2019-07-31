# Libraries
library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)
# library(tensorflow)

# data <- train_main_diamond
# data %<>% mutate_if(is.factor, as.numeric)
# Neural Network Visualization
# n <- neuralnet(price ~ carat + depth + table + cut2 + color2 + clarity2 + index2,
#                data = data,
#                hidden = c(5,3),
#                linear.output = F,
#                lifesign = 'full',
#                rep=1)

# 
# 
# 
# plot(n,
#      col.hidden = 'darkgreen',
#      col.hidden.synapse = 'darkgreen',
#      show.weights = F,
#      information = F,
#      fill = 'lightblue')


data_train <- train_main_diamond
data_train %<>% mutate_if(is.factor, as.numeric)

training <- as.matrix(data_train[,!(names(data_train) %in% c("index1", "id", "price"))])
training_target <- as.matrix(train_main_diamond$price)

data_test <- test_main_diamond
data_test %<>% mutate_if(is.factor, as.numeric)
test <- as.matrix(data_test[,!(names(data_test) %in% c("index1", "id", "price"))])
test_target <- as.matrix(test_main_diamond$price)




# for reproduciblity
set.seed(1234)
# Create Model
model <- keras_model_sequential()
model %>% 
    layer_dense(units = 5, activation = 'relu', input_shape = c(7)) %>%
    layer_dense(units = 1)

# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')
set.seed(1234)
# Fit Model
mymodel <- model %>%
    fit(training,
        training_target,
        epochs = 100,
        batch_size = 32,
        validation_split = 0.2)


# Evaluate
model %>% evaluate(test, test_target)
pred <- model %>% predict(test)
mean((test_target-pred)^2)
plot(test_target, pred)
#################################################################
#Fine tune the model
#################################################################
# Normalize
m <- colMeans(training)
s <- apply(training, 2, sd)
training <- scale(training, center = m, scale = s)
test <- scale(test, center = m, scale = s)

set.seed(1234)
# Create Model
model <- keras_model_sequential()
model %>% 
    layer_dense(units = 5, activation = 'relu', input_shape = c(7)) %>%
    layer_dense(units = 3, activation = 'relu') %>%
    layer_dense(units = 1)
summary(model)
# Compile
model %>% compile(loss = 'mse',
                  optimizer = 'rmsprop',
                  metrics = 'mae')
set.seed(1234)
# Fit Model
mymodel <- model %>%
    fit(training,
        training_target,
        epochs = 100,
        batch_size = 32,
        validation_split = 0.2)


# Evaluate
model %>% evaluate(test, test_target)
pred <- model %>% predict(test)
#validation error
tuned_nn_mse <- mean((test_target-pred)^2)
plot(test_target, pred)
#training error
model %>% evaluate(training, training_target)


# reticulate::use_condaenv("keras-gpu")
# library(tensorflow)
# use_condaenv("r-tensorflow")
# sess <- tf$Session()
# hello <- tf$constant('Hello, TensorFlow!')
# sess$run(hello)
# install_tensorflow()
