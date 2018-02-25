## Functions to check if library is installed or not, if installed then it calls library function
## to load the packages
source('load_lib.R')
load_lib(readr)
load_lib(chron) 
load_lib(digest)
load_lib(dplyr)
load_lib(data.table)
load_lib(ggplot2)
load_lib(e1071)
load_lib(caret)
load_lib(factoextra)
load_lib(randomForest)

############################## Dataset Preparation ##############################
## Read data
pos <- read_csv('./sales_granular.csv')

## Transpose data
pos_t <- t(pos)
pos_t <- as.data.frame(pos_t)
names(pos_t) <- as.matrix(pos_t[1, ])
pos_t <- pos_t[-1, ]
pos_t[] <- lapply(pos_t, function(x) type.convert(as.character(x)))
pos_t <- cbind(Row.Names = rownames(pos_t), pos_t)
colnames(pos_t)[1] <- 'date'

## Convert date into four separate columns: Year, Month, Day and Time
xc <- chron(sub(" .*", "", pos_t$date), sub(".* (.*)", "\\1:00", pos_t$date)) 
ymdtime <- with(month.day.year(xc), data.frame(year, month, day, time = xc - dates(xc))) 

## Combine the ymdtime with transposed dataframe
pos_t <- cbind(ymdtime, pos_t)

## Digest duplicate columns
pos_tdigest <- pos_t[!duplicated(lapply(pos_t, digest))]

## Almost all stores are open between 9 to 19 (except few). In order to make the amount of
## product sales comparable between stores, the time window of 9 to 19 is selected.
pos_tdigest %>%
  mutate(time = as.numeric(gsub(":00:00", "" , time, perl=TRUE))) %>%
  filter(time >= 9 & time <= 19) -> pos_tdigest

## Sum number of product sold per each month
pos_monthly <- pos_tdigest %>%
  group_by(year, month) %>%
  select(-year, -month, -day, -time, -date) %>%
  summarise_all(funs(sum(., na.rm = TRUE)))

## Create year month variable
pos_monthly$yearmonth <- paste0(pos_monthly$year, '_', pos_monthly$month)

## Transform the data in order to plot it
pos_monthly_melt <- melt(pos_monthly,id.vars="yearmonth")
plot <- ggplot(pos_monthly_melt, aes(x = yearmonth, y = value, color = variable)) + 
  geom_line(aes(group = variable)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = 'Per each month', y = 'Sum of product sold') + 
  theme(legend.position="none") +
  ggtitle(paste0('Sum of product volumes in each store sold per month per year')) 
print(plot)

## Select store_code where the product was sold all months, this has a limitation as the 
## number of store_codes decrease considerably and smaller dataset cannot perform well 
## with maching learning algorithm.
## Select store_codes that has sales record from October 2010 to create comparable sales
pos_monthly <- as.data.frame(pos_monthly)
pos_monthly_10_2015 <- pos_monthly[ , pos_monthly[1, ] != 0]

## plot those store_codes without ZEROs
pos_monthly_10_2015_t <- melt(pos_monthly_10_2015, id.vars="yearmonth")
plot2 <- ggplot(pos_monthly_10_2015_t, aes(x = yearmonth, y = value, color = variable)) + 
  geom_line(aes(group = variable)) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = 'Per each month', y = 'Sum of product sold') + 
  theme(legend.position="none") +
  ggtitle(paste0('Sum of product volumes in each comparable store sold per month per year')) 
print(plot2)

## There are only 23 stores that has sales record from October 2010
## 23 records are too small to run any machine learning model. Either model performs bad 
## or will not generalize.

## Majority of the stores have sales record in 2017. It is good to take only the last six
## months because it can create a comparable sales record between stores.
## Removing the store_codes with ZERO values
## 12% (107 of 903) of the store_codes starts with ZERO product sold. I assume these stores
## did not offer the product sale for the first month of 2017 as a result were discards from 
## further analysis.
## The remaining 796 store_codes that had product sold monthly for 2017 were summed as the 
## response feature (product_number_sold)
pos_2017 <- subset(pos_monthly, year == 2017)
pos_2017 <- pos_2017[ , pos_2017[1, ] != 0]
pos_2017 %>%
  select(-month, -yearmonth) %>%
  summarise_all(funs(sum(., na.rm = TRUE))) -> pos_2017
pos_2017 <- melt(pos_2017, id='year')
colnames(pos_2017) <- c('year', 'store_code', 'sales_volume')
pos_2017 <- pos_2017[,-1]

## Read the surroundings.csv saved from surroundings.R file
## This file is already loaded, if it is not loaded, you can load it here.
surroundings <- read_csv('./new_surroundings.csv')

## Merge the pos_2017 with surroundings.csv
## Store_code present in both datasets will be matched (inner join)
store_surrounding <- merge(pos_2017, surroundings)

########################### Machine Learning Modeling ###########################
########################### Model One: Regression Model ##########################
## Now, we have two features that are continuous. Regression model is one of the machine
## learning model that can be tried.
## Set seed for reproducibility. In practice, I will be running a k-fold Cross Validation
## Then take average of these 10 random seeds to meaasure the model performance 
set.seed(893)
## Subset test and train datasets
sample = sample(seq_len(nrow(store_surrounding)), size = floor(nrow(store_surrounding)*.8))
train = store_surrounding[sample, ]
test = store_surrounding[-sample, ]

## Train the model based on the train dataset
model <- lm(sales_volume ~ amenities_no, data = train)
summary(model)

## Predict the test dataset based on the trained model
test$predicted <- predict(model, test)

## I created a function that can calculate the R quare and Root Mean Square Error (RMSE)
r2_rmse <- function (df, sales_volume, predicted_sales_volume) {
  r_square <- (cor(df[ , sales_volume], df[ , predicted_sales_volume]))^2
  rms_error <- sqrt(mean((df[, predicted_sales_volume] - df[, sales_volume])^2))
  metrics <- data_frame(
    r2 = r_square, 
    rmse = rms_error
  )
}

## Model performance can be measured using R square and Root Mean Square Error (RMSE)
## Estimate model performance
metrics <- r2_rmse(test, 'sales_volume', 'predicted')
metrics 

## Plot the predicted sales volume against actual sales volume with the model equation
coeff=coefficients(model)
reg_model = paste0("y = ", round(coeff[1],1), " + ", round(coeff[2],1), "*x")
plot3 <- ggplot(test, aes(x = sales_volume, y = predicted)) + 
  geom_point() +
  geom_abline(intercept = round(coeff[1],1), slope = round(coeff[2],1)) +
  ggtitle(reg_model) +
  theme_classic() +
  labs(x = 'Actual Sales Volume', y = 'Predicted Sales Volume')
print(plot3)

## Result: it is clear that the model trained on train subset performs not good when tested
## on the test subset: R square = 17.6% and RMSE = 9909. The higher R square and lower RMSE
## indicates that model performance is better. In this case, these values incdicate that the
## regression model can only explain 17.6% variablity in the sales volume, which is too low.

## Verdict: Regression Model performance accuracy is very low. We can not use this model
## to predict sales volume in a store based on number of amenities surroundings it.

################## Model Two: Clustering and RandomForset Model #################
## Prepare the dataset
#store_surrounding$store_code <- as.numeric(as.character(store_surrounding$store_code))
df_clustering <- store_surrounding[, -1]

## The features are on different scale. They have to be standardized in order to be on 
## similar scale
df_clustering_scaled <- scale(df_clustering)

## Determine the number of clusters. K-means clustering can be used to create clusters and
## subsequently, these clusters will be used in the randomforest modelling to predict the 
## product sales volume. factoextra package will be used to visualise possible number of 
## clusters
fviz_nbclust(df_clustering_scaled, kmeans, method = 'wss') +
  geom_vline(xintercept = 3, linetype = 2)

#3 K-means clustering calculations
# set seed for reproducibility
set.seed(32423)

## Perform k-means clustering with kmeans, followed by number of determined clusters
## and nstart of 50 (it tries 50 different random starting assignments and  selects
## the best model that has lowest within cluster variation.
df_clustering_kmeans <- kmeans(df_clustering_scaled, 3, nstart = 50)

## Print the results
print(df_clustering_kmeans)

## Combine the classification from kmeans clustering model into the original data 
df_clustering <- cbind(df_clustering, cluster = df_clustering_kmeans$cluster) 

## We can check the size of each cluster, i.e. how many sales volume are in each category
df_clustering_kmeans$centers

## fviz_cluster function takes kmeans result, original data, plot it, with drawing 
## concentration ellipse around each cluster.
fviz_cluster(df_clustering_kmeans, data = df_clustering,
             ellipse.type = "euclid", # Concentration ellipse 
             star.plot = TRUE, # Add segments from centroids to items 
             #repel = FALSE, # Avoid label overplotting (slow) 
             ggtheme = theme_minimal() 
)

## Combine the clusters with store_surroudning dataset
df_random_forest <- cbind(store_surrounding, df_clustering)
df_random_forest <- df_random_forest[,-(4:5)]

## Rearragen the columns and make clusters as a categorical feature
df_random_forest <- df_random_forest[c("cluster", "amenities_no", "store_code", "sales_volume")]
df_random_forest$cluster = as.factor(df_random_forest$cluster)
df_random_forest$store_code = as.numeric(as.character(df_random_forest$store_code))

## Reverse back the scaled dataframe to see centers of sales volume and amenities number for each cluster
df_random_forest %>% 
  select(-store_code) %>%
  group_by(cluster) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))

## Set seed for reproducibility of dataset splitting
set.seed(879)

## Subset test and train datasets (I always run k-fold cross valudation but for simplicity, I run only once)
sample = sample(seq_len(nrow(df_random_forest)), size = floor(nrow(df_random_forest)*.8))
rf_train = df_random_forest[sample, ]
rf_test = df_random_forest[-sample, ]

## Use amenities number to predict the clusters using randomForest with 500 trees
rf_model <-randomForest(cluster ~ amenities_no, data = rf_train, ntree=500)
plot(rf_model)

## Random Forest algorithm was used to built a 500 decision trees or a forest. We can plot the 
## error rate across decision trees. There is no significant reduction in error rate approximately
## after 50 decision trees as shown in the above figure
## Note: the model generated might be different than those I generated because it is a random process
rf_model <-randomForest(cluster ~ amenities_no, data = rf_train, ntree=50) 
plot(rf_model)

## Predicting cluster feature in the train dataset
rf_train$predicted_cluster <- predict(rf_model ,rf_train)

# Predicting cluster feature derived from sales volume in the test dataset
rf_test$predicted_cluster <- predict(rf_model ,rf_test)

############################## Confusion Matrix ############################## 
## confusionMatrix function from caret package can be used for creating confusion matrix based
## on actual response variable and predicted value.
# Create Confusion Matrix and measure  the accuracy of cluster prediction in the train dataset
confusionMatrix(data = rf_train$predicted_cluster,
                reference = rf_train$cluster,
                positive = 'yes')

# Create Confusion Matrix and measure accuracy of the model performance
confusionMatrix(data = rf_test$predicted_cluster,
                reference = rf_test$cluster,
                positive = 'yes')
