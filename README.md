# Diamond-Pricing

Predict the prize for diamond sample by developing regression, decision trees, neural networks and random forest models using R in order to enable the jeweler to correctly bid for an auction at diamond wholesaler.

### Code run Sequence
##### 1. diamond_pricing_cleaning.R
##### 2. neural_net_diamond_pricing.R
##### 3. diamond_pricing_random_forest.R
##### 4. score_diamond_price.R


#### Data cleaning

1.	Read the diamond price raw data file in R.
2.	The diamond raw dataset has the response variable as diamond Price in USD and the explanatory variables such as carat, cut, color, clarity, X (length), Y (width), Z (depth), table (width of top of diamond relative to widest point).
3.	Assign the out of range values for explanatory variables to missing and set the ordered factors for variables like cut, color and clarity.
4.	Drop the rows with missing values for input variables.
5.	Plot of price vs id shows that the raw diamond file may be sourced from two different sub populations. Hence, it’s decided to create a new index variable based on id and carat values.

#### Statistical analysis

1.	Split the clean data into 70/30 training and validation split.
2.	Fit linear regression model with price as response variable and carat, cut, color, clarity, X (length), Y (width), Z (depth), table and new index as explanatory variables on the training data set.
3.	Use forward selection, backward selection and stepwise selection to see if any of the input variables can be dropped from regression models.
4.	Fit decision tree models with maximum depth = 6 criteria and prune the decision tree based on optimal CP values. 
5.	Fit random forest model with optimal node size = 5 (Minimum size of terminal nodes. Setting this number smaller causes larger trees to be grown)
and sample fraction as 0.55 (Parameter specifies the fraction of observations to be used in each tree. Smaller fractions lead to greater diversity, and thus less correlated trees which often is desirable).
6.	Build neural network model using RELU as the activation function (determines the output a node will generate, based upon its input) with 1 hidden layer, 3 units and optimization algorithm as RMSPROP
7.	read the scoring file provided by diamond merchant. The task is to predict the price of each diamond in this file. The sum of predicted price of the diamonds will be used by the merchant to bid during auction.
8.	The new index variable that we created in the raw dataset for building models is not available in the scoring dataset. We used random forest and neural network models to predict index variable. 
9.	Use least MSE (mean squared error) on validation data as the criteria for model selection. 

#### Model Summary and prediction

1.	Out of several models such as Random Forest, Neural Network, Decision tree and regression models, we chose Random forest because it gave the least MSE on validation data.
2.	Create 99% CI for the sum of predicted price of diamonds which is ($19,450,456, $20,953,305).

