## Load data.
rm(list=ls())
main_diamond <- read.csv("/Users/pranavd/Documents/RWork/R_Projects/diamond_pricing/data/main_diamond_1.csv")

## Extract a few offensive statistics (numerical variables).




test_main_diamond <- main_diamond
str(test_main_diamond)
#######################################################################
# Apply range to numeric variables
#########################################################################

#set x out of range to missing 
test_main_diamond$x[test_main_diamond$x < 3.9 | test_main_diamond$x > 8.6]
test_main_diamond$x[test_main_diamond$x < 3.9 | test_main_diamond$x > 8.6] <- NA

#validation check
length(main_diamond$x[main_diamond$x < 3.9 | main_diamond$x > 8.6])
length(test_main_diamond$x[is.na(test_main_diamond$x)])

#set y out of range to missing 
test_main_diamond$y[test_main_diamond$y < 3.9 | test_main_diamond$y > 8.5]
test_main_diamond$y[test_main_diamond$y < 3.9 | test_main_diamond$y > 8.5] <- NA

#validation check
length(main_diamond$y[main_diamond$y < 3.9 | main_diamond$y > 8.5])
length(test_main_diamond$y[is.na(test_main_diamond$y)])


#set z out of range to missing 
test_main_diamond$z[test_main_diamond$z < 2.4 | test_main_diamond$z > 5.3]
test_main_diamond$z[test_main_diamond$z < 2.4 | test_main_diamond$z > 5.3] <- NA

#validation check
length(main_diamond$z[main_diamond$z < 2.4 | main_diamond$z > 5.3])
length(test_main_diamond$z[is.na(test_main_diamond$z)])

#set Table out of range to missing 
test_main_diamond$table[test_main_diamond$table < 52 | test_main_diamond$table > 66]
test_main_diamond$table[test_main_diamond$table < 52 | test_main_diamond$table > 66] <- NA

#validation check
length(main_diamond$table[main_diamond$table < 52 | main_diamond$table > 66])
length(test_main_diamond$table[is.na(test_main_diamond$table)])


#set Depth out of range to missing 
test_main_diamond$depth[test_main_diamond$depth < 57 | test_main_diamond$depth > 67]
test_main_diamond$depth[test_main_diamond$depth < 57 | test_main_diamond$depth > 67] <- NA

#validation check
length(main_diamond$depth[main_diamond$depth < 57 | main_diamond$depth > 67])
length(test_main_diamond$depth[is.na(test_main_diamond$depth)])


#set carat out of range to missing 
test_main_diamond$carat[test_main_diamond$carat < 0.2 | test_main_diamond$carat > 4]
test_main_diamond$carat[test_main_diamond$carat < 0.2 | test_main_diamond$carat > 4] <- NA
#validation check
length(main_diamond$carat[main_diamond$carat < 0.2 | main_diamond$carat > 4])
length(test_main_diamond$carat[is.na(test_main_diamond$carat)])



#######################################################################
# create separate variables for ordered factorss
#########################################################################


test_main_diamond$cut2 <- factor(test_main_diamond$cut, levels = c("Fair", "Good", "Very Good", "Premium", "Ideal"), ordered = TRUE)

test_main_diamond$color2 <- ordered(test_main_diamond$color, level = c("J","I","H","G","F","E","D"))
test_main_diamond$clarity2 <- ordered(test_main_diamond$clarity, level = c("I1","SI2","SI1","VS2","VS1","VVS2","VVS1", "IF"))

str(test_main_diamond)
summary(test_main_diamond)

#######################################################################
#remove rows with missing values in R
#########################################################################
# remove the rows with any missing variables
clean_main_diamond <- na.omit(test_main_diamond)
str(clean_main_diamond)

# Validation between raw data missing values and cleaned data non missing values
sapply(test_main_diamond, function(x) sum( is.na(x) ))
sapply(clean_main_diamond, function(x) sum( is.na(x) ))

# frequency for ordinal variables
table(clean_main_diamond$color2)
table(clean_main_diamond$cut2)
table(clean_main_diamond$clarity2)
summary(clean_main_diamond)

#############################################################################
# create index variable so as to identfy the correct modelling population
# The plot below shows that the diamond sample provided may belong to different populations.
#we create indicator varaibles index1 for index2 to model 2 poulations separately
plot(clean_main_diamond$id, clean_main_diamond$carat)

clean_main_diamond$index2 <- factor(ifelse(clean_main_diamond$id < 27751 & clean_main_diamond$carat > 0.49, 1, 0))
clean_main_diamond$index1 <- factor(ifelse(clean_main_diamond$id < 27751, 1, 0))

table(clean_main_diamond$index1)
table(clean_main_diamond$index2)

#############################################################################
# check data again and keep modelling variables
str(clean_main_diamond)

model_main_diamond <- clean_main_diamond[c("carat", "depth", "table", "price", "cut2", "color2", "clarity2", "index2", "index1", "id")]
str(model_main_diamond)
#############################################################################

#create traning and test data split    
set.seed(1983) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 70% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(model_main_diamond), size = floor(0.7*nrow(model_main_diamond)), replace = F)
train_main_diamond <- model_main_diamond[sample, ]
test_main_diamond  <- model_main_diamond[-sample, ]
#############################################################################
# Models
#############################################################################

# Fit linear regression model with 2 factor interactions and without varaible selection

lm.fit =lm(price ~ carat + depth + table + cut2*color2 + cut2*clarity2 + cut2*index2 +
               color2*clarity2 + color2*index2 +  clarity2*index2
               ,data=train_main_diamond)
summary(lm.fit)


# ASE - for training data
mean((lm.fit$residuals)^2)

# ASE  - for validation data
lm_noselec_mse <- mean((predict(lm.fit, test_main_diamond) - test_main_diamond$price)^2)
lm_noselec_mse
#############################################################################


#############################################################################

# variable selection using step function
# step(lm.fit, scope = . ~ .^2, direction = 'forward', test = "P")



fwd.sel <- step(lm.fit, scope = price ~ carat + depth + table + cut2*color2 + cut2*clarity2 + cut2*index2 + color2*clarity2 + color2*index2 +  clarity2*index2, direction = 'forward', trace = TRUE)

# Train ASE for Forward selection model
mean((predict(fwd.sel, train_main_diamond) - train_main_diamond$price)^2)
# Test ASE for Forward selection model
fwd_sel_mse <- mean((predict(fwd.sel, test_main_diamond) - test_main_diamond$price)^2)
fwd_sel_mse


bkwd.sel <- step(lm.fit, scope = price ~ carat + depth + table + cut2*color2 + cut2*clarity2 + cut2*index2 + color2*clarity2 + color2*index2 +  clarity2*index2, direction = 'backward', trace = TRUE)
# Train ASE for Backward selection model
mean((predict(bkwd.sel, train_main_diamond) - train_main_diamond$price)^2)
# Test ASE for Backward selection model
bkwd_sel_mse <- mean((predict(bkwd.sel, test_main_diamond) - test_main_diamond$price)^2)
bkwd_sel_mse


stws.sel <- step(lm.fit, scope = price ~ carat + depth + table + cut2*color2 + cut2*clarity2 + cut2*index2 + color2*clarity2 + color2*index2 +  clarity2*index2, direction = 'both', trace = TRUE)
# Train ASE for Stepwise selection model
mean((predict(stws.sel, train_main_diamond) - train_main_diamond$price)^2)
# Test ASE for Stepwise selection model
stws_sel_mse <- mean((predict(stws.sel, test_main_diamond) - test_main_diamond$price)^2)
stws_sel_mse

#############################################################################

library(rpart)
library(rpart.plot)

# train the decision tree model - cp = 0 is used to allow the tree to grow, limiting the max depth tree to 6 levels (cue from SAS E Miner)
fit_decision_tree <- rpart(formula = price ~ carat + depth + table + cut2 + color2 + clarity2 + index2, data = train_main_diamond, method  = "anova",
                           control = list(minsplit = 5, maxdepth = 6, xval = 10), cp = 0)
# see the node-wise details
summary(fit_decision_tree)
# print the cp details
fit_decision_tree$cptable
printcp(fit_decision_tree)
# plotcp function provides cross validation error for each nsplit, it will be used for pruning the tree. the one with least cross-validated error (xerror) is the optimal value of CP given by the printcp() function
plotcp(fit_decision_tree)
# plot decsion tree
rpart.plot(fit_decision_tree)

# This is the optimal valus of CP
fit_decision_tree$cptable[which.min(fit_decision_tree$cptable[,"xerror"]),"CP"]

# Prune the decision tree based on optimal cp value chosen above
prune_decision_tree <- prune(fit_decision_tree, cp = fit_decision_tree$cptable[which.min(fit_decision_tree$cptable[,"xerror"]),"CP"])

# plot the pruned decsion tree
rpart.plot(prune_decision_tree)

# Train ASE for Decision tree model
mean((predict(prune_decision_tree, train_main_diamond) - train_main_diamond$price)^2)
# Test ASE for Decision tree model
pruned_dec_tree_mse <- mean((predict(prune_decision_tree, newdata = test_main_diamond) - test_main_diamond$price)^2)

#############################################################################
# https://medium.com/analytics-vidhya/a-guide-to-machine-learning-in-r-for-beginners-decision-trees-c24dfd490abb

# http://uc-r.github.io/regression_trees

# https://dzone.com/articles/decision-trees-and-pruning-in-r

# index2 = 
#     if id < 27751 & carat > 0.49 then 1
#     else 0

# index1 = 
#     if id < 27751 then 1
#     else 0


# xt <- loan_pred_train_cln[which(is.na(loan_pred_train_cln$LoanAmount)),]
# xt$LoanAmount = ifelse(is.na(xt$LoanAmount) & xt$Self_Employed == 'Yes' & xt$Education == 'Graduate', 157.5, ifelse(is.na(xt$LoanAmount) & xt$Self_Employed == 'Yes' & xt$Education == 'Not Graduate', 130, ifelse(is.na(xt$LoanAmount) & xt$Self_Employed == 'No' & xt$Education == 'Graduate', 130, ifelse(is.na(xt$LoanAmount) & xt$Self_Employed == 'No' & xt$Education == 'Not Graduate', 113, NA))))
# loan_pred_train_cln[which(is.na(loan_pred_train_cln$LoanAmount)),]$LoanAmount <- xt$LoanAmount
# summary(loan_pred_train_cln)



# 
# 
# 
# aggregate(DTA$price, by=list(DTA$color), FUN=mean)
# 
# data.frame(color=levels(factor(DTA$color)),mean=(aggregate(DTA$price, by=list(DTA$color), FUN=mean)$x),
#            
#            sd=(aggregate(DTA$price, by=list(DTA$color), FUN=sd)$x))
# 
# data.frame(clarity=levels(factor(DTA$clarity)),mean=(aggregate(DTA$price, by=list(DTA$clarity), FUN=mean)$x),
#            
#            sd=(aggregate(DTA$price, by=list(DTA$clarity), FUN=sd)$x))
# 
# data.frame(cut=levels(factor(DTA$cut)),mean=(aggregate(DTA$price, by=list(DTA$cut), FUN=mean)$x),
#            
#            sd=(aggregate(DTA$price, by=list(DTA$cut), FUN=sd)$x))
# 
# 
# 
# #carat
# plot(DTA$carat)
# plot(DTA$carat, DTA$price)
# 
# # depth
# plot(DTA$depth)
# plot(DTA$depth, DTA$price)
# 
# # table
# plot(DTA$table)
# plot(DTA$table, DTA$price)
# 
# # x
# plot(DTA$x)
# plot(DTA$x, DTA$price)
# 
# # y
# plot(DTA$y)
# plot(DTA$y, DTA$price)
# 
# # z
# plot(DTA$z)
# plot(DTA$z, DTA$price)
# 
# library(leaps)

# regfit.fwd=regsubsets (price ~ .^2, data=train_main_diamond, method ="forward")


# regfit.fwd=regsubsets(price ~ carat + depth + table + cut2*color2 + cut2*clarity2 + cut2*index2 + color2*clarity2 + color2*index2 +  clarity2*index2, data=train_main_diamond, method ="forward")
# summary(regfit.fwd )
# plot(summary(regfit.fwd)$rsq, type = "l", xlab = "Number of Variables", ylab = "RSquare")
# # Best Model using Adjusted R-square
# which.max(summary(regfit.fwd)$adjr2)
# plot(summary(regfit.fwd)$adjr2, type = "l", xlab = "Number of Variables", ylab = "Adjusted RSquare")
# points(8, summary(regfit.fwd)$adjr2[8], col = "red", cex = 2, pch = 20 )
# 
# 
# 
# #Best Model using BIC
# which.min(summary(regfit.fwd)$bic)
# plot(summary(regfit.fwd)$bic, type = "l", xlab = "Number of Variables", ylab = "BIC")
# points(9, summary(regfit.fwd)$bic[9], col = "red", cex = 2, pch = 20 )

#############################################################################
# 
# which.max(summary(regfit.fwd)$adjr2)
# 
# regfit.bkwd=regsubsets (price ~ carat + depth + table + cut2*color2 + cut2*clarity2 + cut2*index2 + color2*clarity2 + color2*index2 +  clarity2*index2, data=train_main_diamond, method ="backward")
# summary(regfit.bkwd)

# 
# printcp(fit.dec_tree) # display the results 
# plotcp(fit.dec_tree) # visualize cross-validation results 
# summary(fit.dec_tree) # detailed summary of splits
# 
# # create additional plots 
# par(mfrow=c(1,2)) # two plots on one page 
# rsq.rpart(fit.dec_tree) # visualize cross-validation results  	
# 
# fit.dec_tree <- rpart(price ~ carat + depth + table + cut2 + color2 + clarity2 + index2,
#                       method = "anova", data = train_main_diamond)
# # plot tree 
# plot(fit.dec_tree, uniform=TRUE, 
#      main="Regression Tree for diamond Price")
# text(fit.dec_tree, use.n=TRUE, all=TRUE, cex=.8)
# 
# prun_fit_dc <- prune(fit.dec_tree, 
#                      cp=fit.dec_tree$cptable[which.min(fit.dec_tree$cptable[,"xerror"]),"CP"]) # from cptable 
# 
# # plot the pruned tree 
# plot(prun_fit_dc, uniform=TRUE, 
#      main="Pruned Regression Tree for Diamond Pricing")
# text(prun_fit_dc, use.n=TRUE, all=TRUE, cex=.8)
# 
# # Train ASE for Decision tree model
# mean((predict(prun_fit_dc, train_main_diamond) - train_main_diamond$price)^2)
# # Test ASE for Decision tree model
# mean((predict(prun_fit_dc, test_main_diamond) - test_main_diamond$price)^2)