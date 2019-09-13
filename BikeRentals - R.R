setwd("~/Documents/Job Search/edWisor/MyProjects/2. Bike Rentals/Final Deliverables/R")
#setwd(dir = 'Path_to_folder_containing_data_files')
getwd()

# =========================================================================================#
#
#                                   1. Loading Data Files
#
# =========================================================================================#
training_data_full = read.csv('day.csv')

# =========================================================================================#
#
#                                   2. Exploring Data
#
# =========================================================================================#
str(training_data_full)
sapply(training_data_full, class)
sapply(training_data_full, typeof)
summary(training_data_full)

# =========================================================================================#
#
#                                   3. Preprocessing Data
#
# =========================================================================================#

# ============================== #
# 3.1 Missing Value Analysis
# ============================== #
show_missing_vals = function(data_frame){sapply(data_frame, function(feature) sum(is.na(feature)))}
show_missing_vals(training_data_full)

# ============================== #
# 3.2 Changing Data Types
# ============================== #
numerical_columns = c('temp','atemp','hum','windspeed','casual','registered','cnt')
categorical_columns = c('season', 'yr', 'mnth', 'holiday', 'weekday', 'workingday', 'weathersit')

training_data_full[categorical_columns] = lapply(training_data_full[categorical_columns], as.factor)
str(training_data_full)
clean_training_data = training_data_full
# ============================== #
# 3.3 Outlier Analysis & Distribution
# ============================== #
install.packages(c('tidyverse','hrbrthemes','viridis'))
library('tidyverse')
library('hrbrthemes')
library('viridis')

box_plot = function(numerical_column_name, categorical_column_name, dataframe=clean_training_data){
  dataframe %>% #Chaining
    ggplot(aes_string(x = categorical_column_name, y = numerical_column_name, fill = categorical_column_name)) +
    geom_boxplot() +
    geom_jitter(color='black', size=0.4, alpha=0.9) +
    theme_ipsum() +
    theme(
      legend.position = 'top',
      plot.title = element_text(size = 9)
    ) +
    ggtitle(paste("BoxPlot with", categorical_column_name, " & ",numerical_column_name))
}

box_plot('temp','season',clean_training_data)
box_plot('hum','season')
box_plot('windspeed','weathersit')

box_hist_plot = function(numerical_column_name, dataframe=clean_training_data){
  numerical_column = dataframe[,numerical_column_name]
  # Layout to split the screen
  layout(mat = matrix(c(1,2),nrow = 2, ncol = 1, byrow = TRUE), heights = c(1,8))
  # Draw the boxplot and the histogram 
  par(mar=c(0, 3.1, 1.1, 2.1)) #Margins
  boxplot(numerical_column , horizontal=TRUE , ylim=c(min(numerical_column),max(numerical_column)), xaxt="n" , col=rgb(0.8,0.8,0,0.5) , frame=F)
  par(mar=c(4, 3.1, 1.1, 2.1))
  hist(numerical_column , breaks=40 , col=rgb(0.2,0.8,0.5,0.5) , border=F , main="" , xlab=paste("value of the variable : ",numerical_column_name), xlim=c(min(numerical_column),max(numerical_column)))
}

box_hist_plot('temp')
box_hist_plot('atemp')
box_hist_plot('hum')
box_hist_plot('windspeed')
box_hist_plot('casual')
box_hist_plot('registered')
box_hist_plot('cnt')

bar_plot = function(x_col, y_col, fill_col){
  clean_training_data %>%
    ggplot(aes_string(x = x_col, y = y_col, fill = fill_col))+
    geom_bar(position='stack', stat = 'identity')+
    scale_fill_viridis(discrete = T)+
    ggtitle(paste("Bar Plot of",x_col,"on X-Axis,",y_col,"on Y-Axis &", fill_col,"stacked into the bars."))+
    theme_dark()+
    xlab("")
}

bar_plot('season','cnt','weathersit')
bar_plot('season','temp','weathersit')
bar_plot('season','hum','weathersit')
bar_plot('mnth','windspeed','weathersit')
bar_plot('weathersit','temp','season')

# =========================================================================================#
#
#                                   4. Feature Engineering
#
# =========================================================================================#

# ============================== #
# 4.1 Setting New Features 
# ============================== #
set_weekday = function(x){
  if (x == 0 || x == 6)
    return(0)
  else
    return(1)
}

unset_winter_month = function(x){
  x = as.numeric(x)
  if (x <= 3 || x == 12)
    return(0)
  else
    return(1)
}

clean_training_data$mnth = sapply(clean_training_data$mnth, unset_winter_month)
clean_training_data$mnth = sapply(clean_training_data$mnth, as.factor)
clean_training_data$weekday = sapply(clean_training_data$weekday, set_weekday)
clean_training_data$weekday = sapply(clean_training_data$weekday, as.factor)

# ============================== #
# 4.2 One-Hot Encoding 
# ============================== #
install.packages('caret')
library(caret)

dmy = dummyVars("~ season", data = clean_training_data, fullRank = T)
temp_df = data.frame(predict(dmy, newdata = clean_training_data))
clean_training_data = cbind(clean_training_data, temp_df)

dmy = dummyVars("~ weathersit", data = clean_training_data, fullRank = T)
temp_df_2 = data.frame(predict(dmy, newdata = clean_training_data))
clean_training_data = cbind(clean_training_data, temp_df_2)

# ============================== #
# 4.3 Correlation Analysis
# ============================== #
# correlation plot for numerical feature
library('corrgram')
correl_mat = cor(x = clean_training_data[,numerical_columns], method = c("pearson", "kendall", "spearman"))
corrgram(clean_training_data[,numerical_columns], order = FALSE,
         upper.panel = panel.cor, text.panel = panel.txt,
         lower.panel = panel.pie, main = "Correlation Plot")

# Scatterplot
install.packages('GGally')
library(GGally)
ggpairs(clean_training_data[,numerical_columns], title = 'Scatter Plot of Numerical Features')
# Scatterplot with categorical variables
ggpairs(clean_training_data, columns = numerical_columns, ggplot2::aes_string(colour='season'))
ggpairs(clean_training_data, columns = numerical_columns, ggplot2::aes_string(colour='weathersit'))

# ============================== #
# 4.4 VIF Analysis of Correlated Features
# ============================== #
#VIF of all numerical columns
install.packages('usdm')
library('usdm')
# With correlated column
vif(clean_training_data[,numerical_columns])
# Without correlated column
vif(clean_training_data[,c('temp','hum','windspeed','cnt')])

# ============================================================= #
# Numerical Columns to drop - 'atemp','casual','registered'
# ============================================================= #

# ============================== #
# 4.4 Chi-Square Test for Categorical Columns
# ============================== #
categorical_columns = c('season','yr','mnth','holiday','weekday','workingday','weathersit','season.2','season.3','season.4','weathersit.2','weathersit.3')
clean_training_data[categorical_columns] = lapply(clean_training_data[categorical_columns], as.factor)
# Chi-Square test for categorical features
# Assumption - p-value threshold 0.05
paired_features = combn(x = categorical_columns, m = 2, simplify = FALSE)
for(i in paired_features){
  feat1 = i[1]
  feat2 = i[2]
  chisq_res = chisq.test(table(clean_training_data[,feat1], clean_training_data[,feat2]))
  if(chisq_res$p.value < 0.05){
    print(i)
    print(chisq_res$p.value)
  }
}

# ============================================================================ #
# Categorical Columns to drop - 'season','holiday','weathersit'
# ============================================================================ #

# ============================== #
# 4.5 Final Touch-ups
# ============================== #
drop_columns = c('instant','dteday','season','holiday','weathersit','atemp','casual','registered')
train_data = clean_training_data[,!colnames(clean_training_data) %in% drop_columns]
final_columns = c("yr","mnth","weekday","workingday","temp","hum","windspeed","season.2","season.3","season.4","weathersit.2","weathersit.3")
final_numeric_cols = c('temp','hum','windspeed')
final_cat_cols = c("yr","mnth","weekday","workingday","season.2","season.3","season.4","weathersit.2","weathersit.3")
target_cols = c('cnt')
# Creating a dataset without IQR outliers
train_data_iqr = train_data
remove_outliers_iqr = function(dataframe, colnames_list){
  init_dim = dim(dataframe)[1]
  for (colname in colnames_list) {
    outlier_vals = boxplot(dataframe[,colname], plot=FALSE)$out
    dataframe = dataframe[which(!dataframe[,colname] %in% outlier_vals), ]
  }
  rownames(dataframe) = 1:nrow(dataframe)
  end_dim = dim(dataframe)[1]
  rows_rem = init_dim - end_dim
  print(paste("No. of rows removed = ",rows_rem))
  return(dataframe)
}
train_data_iqr = remove_outliers_iqr(dataframe = train_data_iqr, colnames_list = final_numeric_cols)
box_hist_plot('temp', dataframe = train_data_iqr)
box_hist_plot('hum', dataframe = train_data_iqr)
box_hist_plot('windspeed', dataframe = train_data_iqr)

ggpairs(train_data_iqr[,c(final_numeric_cols, target_cols)], title = 'Scatter Plot of Numerical Features, IQR Data')
# Scatterplot with categorical variables
ggpairs(train_data_iqr, columns = c(final_numeric_cols, target_cols), ggplot2::aes_string(colour='workingday'))+
  ggtitle('Numerical Features Scatter Plot along with workingday categorial Feature')
ggpairs(train_data_iqr, columns =  c(final_numeric_cols, target_cols), ggplot2::aes_string(colour='mnth'))+
  ggtitle('Numerical Features Scatter Plot along with mnth categorial Feature')
ggpairs(train_data_iqr, columns =  c(final_numeric_cols, target_cols), ggplot2::aes_string(colour='yr'))+
  ggtitle('Numerical Features Scatter Plot along with yr categorial Feature')

# =========================================================================================#
#
#           5. Making Models & Hyperparameter Tuning for each Model individually
#
# =========================================================================================#
# Train-test split
require(caTools)
set.seed(27)
sampling = sample.split(train_data$cnt, SplitRatio = 0.8)
training_set = subset(train_data, sampling==TRUE)
testing_set = subset(train_data, sampling==FALSE)

sampling = sample.split(train_data_iqr$cnt, SplitRatio = 0.8)
training_set_iqr = subset(train_data_iqr, sampling==TRUE)
testing_set_iqr = subset(train_data_iqr, sampling==FALSE)

# Shuffling the rows in the split datasets
#training_set = training_set[sample(nrow(training_set)), ]
#testing_set = testing_set[sample(nrow(testing_set)), ]
#training_set_iqr = training_set_iqr[sample(nrow(training_set_iqr)), ]
#testing_set_iqr = testing_set_iqr[sample(nrow(testing_set_iqr)), ]

# Making features dataframe and target dataframe
#X_train = training_set[, c(final_numeric_cols, final_cat_cols)]
#y_train = training_set[, c(target_cols)]
X_test = testing_set[, c(final_numeric_cols, final_cat_cols)]
y_test = testing_set[, c(target_cols)]

#X_train_iqr = training_set_iqr[, c(final_numeric_cols, final_cat_cols)]
#y_train_iqr = training_set_iqr[, c(target_cols)]
X_test_iqr = testing_set_iqr[, c(final_numeric_cols, final_cat_cols)]
y_test_iqr = testing_set_iqr[, c(target_cols)]

# ================================================================ #
#                Function to perform training
# ================================================================ #
library(readxl)
library(ggplot2)

set.seed(527)
fitControl <- trainControl(method = 'repeatedcv',
                           number = 10,
                           repeats = 2)

# ============================================================== #
# Linear Regression - Boosted Linear Model & Simple Linear Model
# ============================================================== #
install.packages('bst')
library(bst)
BstLmGrid <- expand.grid(mstop = c(600, 1000),
                      #nu = c(0.1,0.3,1,3)
                      nu = 1)
lr_model = train(cnt ~ ., data = training_set,
                 method = 'BstLm',
                 trControl = fitControl,
                 tuneGrid = BstLmGrid)
lr_model_iqr = train(cnt ~ ., data = training_set_iqr,
                 method = 'BstLm',
                 trControl = fitControl,
                 tuneGrid = BstLmGrid)
lr_model_2 = train(cnt ~ ., data = training_set,
                   method = 'lm',
                   trControl = fitControl)
lr_model_2_iqr = train(cnt ~ ., data = training_set_iqr,
                   method = 'lm',
                   trControl = fitControl)
# Best Model - lr_model_2 (MAE - 601.28)
# ===================================================== #
# K-Nearest Neighbour
# ===================================================== #
KnnGrid <- expand.grid(k = c(1,2,3,5,8,10,40))
knn_model = train(cnt ~ ., data = training_set,
                  method = 'knn',
                  trControl = fitControl,
                  tuneGrid = KnnGrid)
knn_model_iqr = train(cnt ~ ., data = training_set_iqr,
                      method = 'knn',
                      trControl = fitControl,
                      tuneGrid = KnnGrid)
# Best Model - knn_model (K = 3, MAE = 557.68)
# ===================================================== #
# Decision Tree
# ===================================================== #
dtGrid = expand.grid(cp = (1:100)*0.0001)
dt_model = train(cnt ~ ., data = training_set,
                 method = 'rpart',
                 trControl = fitControl,
                 tuneGrid = dtGrid) # Best Model @ cp = 0.0005 (MAE = 634.45)
dt_model_iqr = train(cnt ~ ., data = training_set_iqr,
                 method = 'rpart',
                 trControl = fitControl,
                 tuneGrid = dtGrid) # Best Model @ cp = 0.0003 (MAE = 625.05)
dtGrid2 = expand.grid() # None tuning parameters
dt_model_2 = train(cnt ~ ., data = training_set,
                   method = 'rpart1SE',
                   trControl = fitControl) # MAE = 698.12
dtGrid3 = expand.grid(maxdepth = c(1:20))
dt_model_3 = train(cnt ~ ., data = training_set,
                   method = 'rpart2',
                   trControl = fitControl,
                   tuneGrid = dtGrid3) # Best Model @ maxdepth = 10 (MAE = 708.60)
# ===================================================== #
# Random Forest
# ===================================================== #
rfGrid = expand.grid(mtry = c(1:10))
rf_model = train(cnt ~ ., data = training_set,
                 method = 'qrf', #Quantile RF
                 trControl = fitControl,
                 tuneGrid = rfGrid) # Best model @ mtry = 4 (MAE = 492.40)
rf_model_2 = train(cnt ~ ., data = training_set,
                   method = 'parRF', #Parallel RF
                   trControl = fitControl,
                   tuneGrid = rfGrid) # Best model @ mtry = 5 (MAE = 491.66)
#rf_model_3 = train(cnt ~ ., data = training_set,
#                   method = 'cforest',
#                   trControl = fitControl,
#                  tuneGrid = rfGrid)
rf_model_4 = train(cnt ~ ., data = training_set,
                   method = 'rf',
                   trControl = fitControl,
                   tuneGrid = rfGrid) # Best model @ mtry = 5 (MAE = 482.44)

#rfGrid2 = expand.grid(mtry = c(),
#                      numRandomCuts = c())
#rf_model_5 = train(cnt ~ ., data = training_set,
#                   method = 'extraTrees', #EXTRA Trees
#                   trControl = fitControl)

#fitControl2 <- trainControl(method = 'repeatedcv',
#                           number = 5,
#                           repeats = 2)
#rfGrid3 = expand.grid(mtry = c(),
#                      maxdepth = c())
#rf_model_6 = train(cnt ~ ., data = training_set,
#                   method = 'rfRules', #RF Rules
#                   trControl = fitControl2)

#rfGrid4 = expand.grid(mtry = c(),
#                      coefReg = c(),
#                      coefImp = c())
#rf_model_7 = train(cnt ~ ., data = training_set,
#                   method = 'RRF') #Regulaized Random Forest

# ===================================================== #
# XGBoost
# ===================================================== #
# Can add as many more values to fine tune it even more. But because it is time consuming, stopping it at less tuning.
xgbGrid = expand.grid(nrounds = c(150,300,500),
                      max_depth = c(3),
                      eta = c(0.3),
                      gamma = c(0,0.01,0.03),
                      subsample = c(0.8),
                      colsample_bytree = c(0.6,0.7,0.8),
                      rate_drop = c(0.01,0.001,0.003),
                      skip_drop = c(0.01),
                      min_child_weight = c(0.3,1,3))
xgb_model = train(cnt ~ ., data = training_set,
                  method = 'xgbDART',
                  tuneGrid = xgbGrid)
# Storing the index of the best run.
best_res = rownames(xgb_model$bestTune)
# Printing the stats of best run
xgb_model$results[row.names(xgb_model$results) == best_res,]

xgb_model_2 = train(cnt ~ ., data = training_set,
                    method = 'xgbLinear')
xgb_model_2$results[row.names(xgb_model_2$results) == rownames(xgb_model_2$bestTune),]

xgb_model_3 = train(cnt ~ ., data = training_set,
                    method = 'xgbTree')
xgb_model_3$results[row.names(xgb_model_3$results) == rownames(xgb_model_3$bestTune),]

# Best Performance out of all Gradient Boosting Models
# Stochastic Gradient Boosting
xgbGrid2 = expand.grid(n.trees = c(150,300,500,800,1000),
                       interaction.depth = c(3,5,7),
                       shrinkage = c(0.1,0.3,0.03),
                       n.minobsinnode = c(10,20,5))
xgb_model_4 = train(cnt ~ ., data = training_set,
                    method = 'gbm',
                    tuneGrid = xgbGrid2)
xgb_model_4$results[row.names(xgb_model_4$results) == rownames(xgb_model_4$bestTune),]
# Best Model - xgb_model_4 (MAE - 507.07) (can be optimised even more)
# ===================================================== #
# Deep Neural Network - Not optimised yet. Just a raw model.
# ===================================================== #
install.packages('neuralnet')
library(neuralnet)
m <- model.matrix(~ cnt + yr + mnth + weekday + workingday + season.2 + season.3 + season.4 + weathersit.2 + weathersit.3 +
                    temp + hum + windspeed, data = training_set)
nn_model <- neuralnet(cnt ~ yr1 + mnth1 + weekday1 + workingday1 + season.21 + season.31 + season.41 + weathersit.21 + weathersit.31 +
                temp + hum + windspeed, data = m, hidden = c(48,36,24,6), threshold = 0.01, rep = 10, algorithm = 'rprop+',
                err.fct = 'sse', act.fct = 'tanh')

# =========================================================================================#
#                 6. Choosing the best model to predict test cases 
#                           & Saving it for further use
#     Best Model - rf_model_4 - MAE = 482.44, R-Squared Score - 88.37%, RMSE = 675.14
# =========================================================================================#
fitControl3 <- trainControl(method = 'repeatedcv',
                           number = 10,
                           repeats = 5)
rfGrid_best_model = expand.grid(mtry = c(1:10))
rf_model_4_iqr = train(cnt ~ ., training_set_iqr,
                       method = 'rf',
                       trControl = fitControl3,
                       tuneGrid = rfGrid_best_model)
rf_model_4_tuned = train(cnt ~ ., training_set,
                         method = 'rf',
                         trControl = fitControl3,
                         tuneGrid = rfGrid_best_model)
y_pred_1 = predict(rf_model_4_tuned, X_test)
mae_1 = MAE(y_pred_1, y_test)
y_pred_2 = predict(rf_model_4_tuned, X_test_iqr)
mae_2 = MAE(y_pred_2, y_test_iqr)
y_pred_3 = predict(rf_model_4_iqr, X_test)
mae_3 = MAE(y_pred_3, y_test)
y_pred_4 = predict(rf_model_4_iqr, X_test_iqr)
mae_4 = MAE(y_pred_4, y_test_iqr)

print(paste("Achieved Lowest (BEST PERFORMANCE) Mean Absolute Error of :", min(mae_1, mae_2, mae_3, mae_4)))

# =================================== #
# Plotting the predicted values
# =================================== #
# Storing in DataFrame
y_pred_best = data.frame(y_pred_2)
# COnverting predicted values to integer type
y_pred_best$y_pred_2 = as.integer(y_pred_best$y_pred_2)
y_test_iqr_df = data.frame(y_test_iqr)
# Concatenating the 2 dataframes
model_output = cbind(y_pred_best, y_test_iqr_df)
# basic scatter plot
ggplot(model_output, aes_string(x='y_test_iqr', y='y_pred_2'))+
  geom_point(color = '#69b3a2')+
  theme_ipsum()+
  ggtitle("Scatter plot of predicted values on Y-Axis vs. real values on X-Axis")
# with linear trend
ggplot(y_test_iqr_df, aes_string(x='y_test_iqr', y='y_pred_2')) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum()+
  ggtitle("Scatter plot over Line Plot of predicted values on Y-Axis vs. real values on X-Axis")
# linear trend + confidence interval
ggplot(model_output, aes_string(x='y_test_iqr', y='y_pred_2')) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()+
  ggtitle("Scatter plot over Line Plot with Confidence Interval of predicted values on Y-Axis vs. real values on X-Axis")

# ====================================== #
# Saving the best performing model
# ====================================== #
# save the model to disk
saveRDS(rf_model_4_tuned, "./final_tuned_model.rds")

# ================================================ #
# Reloading the model to predict newer unseen data
# ================================================ #
best_model = readRDS("./final_tuned_model.rds")

# ==================================== #
# Predicting Output for new Values
# ==================================== #
# NOTE: This CSV file is already in format required by the model. If it is not in that format, we can load it & preprocess with all the steps
# performed above. But ignoring that for now.
unseen_data = read.csv("sample_input.csv")
unseen_data[final_cat_cols] = lapply(unseen_data[final_cat_cols], as.factor)
y_unseen_pred = predict(best_model, unseen_data)
y_unseen_pred = as.integer(y_unseen_pred)
print(y_unseen_pred)
