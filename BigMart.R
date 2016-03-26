##The data scientists at BigMart have collected 2013 sales data for 1559 products across 10 stores in 
##different cities. Also, certain attributes of each product and store have been defined. 
##The aim is to build a predictive model and find out the sales of each product at a particular store.
##Using this model, BigMart will try to understand the properties of products and stores which play a key
##role in increasing sales.Please note that the data may have missing values as some stores might not 
##report all the data due to technical glitches. Hence, it will be required to treat them accordingly.

library(dplyr)
library(ggplot2)
library(lubridate)
library(plyr)

#Loading the Training Dataset
data<- read.csv("Bigmart.csv",sep = ",")
View(data)
str(data)

#Loading the Testing dataset
datatest<- read.csv("Bigmarttest.csv",sep = ",")

# Cleaning of Data

## Combaining the both.
### Reason for Combaining is
#### . The Products weights are missing and Inorder to find them, We need to convert the Item_Identifier into 
#### Numeric and assign variables accordingly to wieghts. When we do the process for different datasets.
#### We still left-out with different NA's . Some products in train which have missing values are there in 
#### test data and vice-versa.

## Adding the prediction (sales column) to test data.
datatest <- mutate(datatest, Item_Outlet_Sales = 0)
data_clean <- rbind(data,datatest)

## Missing values of Item_Identifier
rtest<- data_clean[0,]
data_clean$Item_Identifier <- as.numeric(levels(data_clean$Item_Identifier))
for (i in 1:1559){
  m1<- filter(data_clean, Item_Identifier == i)
  m2 <- filter(m1, is.na(Item_Weight)== T)
  m3 <- filter(m1, is.na(Item_Weight)== F)
  f1 <- nrow(m2)
  f2 <- nrow(m3)
  if (f1>=1 & f2 >= 1){
    m2$Item_Weight[1:f1] <- mean(m3$Item_Weight)
  } else{
    m2 <- m2
    m3 <- m3
  }
  rtest <- rbind(rtest,m2,m3)
  m2 <- Null
  m3 <- Null
}
data_clean <- rtest


## Chaning LF and low to Low Fat and reg to Regular
data_clean$Item_Fat_Content <- revalue(data_clean$Item_Fat_Content, c("LF"="Low Fat", "low fat"="Low Fat", "reg"="Regular"))

# Dividing the data according to Outlet_size in Grocery segment.
## The Visibility index is same for both the outlets and so all the Grocery stores can be assigned to small
## stores

#Plots:


## Plotting all the graphs we get,
data_clean$Outlet_Size[data_clean$Outlet_Type == "Grocery Store" & data_clean$Outlet_Establishment_Year == 1998] <- "Small"
data_clean$Outlet_Size[data_clean$Outlet_Type == "Supermarket Type1" & data_clean$Outlet_Establishment_Year == 2002] <- "High"
data_clean$Outlet_Size[data_clean$Outlet_Type == "Supermarket Type1" & data_clean$Outlet_Establishment_Year == 2007] <- "High"
summary(data_clean)

#Dividing the data_clean again into datatest and data(train). 
data1<- filter(data_clean, Item_Outlet_Sales != 0)
datatest1 <- filter(data_clean, Item_Outlet_Sales == 0)
summary(data1)
summary(datatest1)
str(data1)

## Correlation Matrix.
library(corrplot)
corr_matrix = cor(data1[,c(1,2,4,6,8,12)])
corrplot(corr_matrix,order ="hclust",method="number")
## Only Item_MRP is significant with r = 0.57

## Feature Scaling 
data2 <- mutate_each_(data1,funs(scale),vars=c("Item_MRP","Item_Visibility","Item_Weight"))

## Now checking the correlation Matrix
library(corrplot)
corr_matrix = cor(data2[,c(2,4,6,12)])
corrplot(corr_matrix,order ="hclust",method="number")

## Plotting Item_Outlet_Sales and Item_MRP
ggplot(data2,aes(x=Item_MRP, y = log(Item_Outlet_Sales)))+
  geom_point()+
  facet_wrap(~Outlet_Identifier)

## Linear Regression equation 
equation1 <- lm(log(Item_Outlet_Sales) ~ Item_MRP+Outlet_Identifier,data2)
summary(equation1)

##R2 and Adjusted R2 - Both are coming around and 72% 
## checking for RMSE

## Test-Data
data1test <- mutate_each_(datatest1,funs(scale),vars=c("Item_MRP","Item_Visibility","Item_Weight"))

predicted_y = predict(equation1,data1test)
predicted_y = exp(predicted_y)
datatest2 = mutate(data1test , Item_Outlet_Sales = predicted_y)
datatest2=datatest2[,c(1,7,12)]

rawdata<- datatest[order(datatest$Item_Identifier),]
rawdata_estimation <- datatest2[order(datatest2$Item_Identifier),]
rawdata_estimation$Item_Identifier <- rawdata$Item_Identifier

## Writing the final dataset to the directory.
write.csv(rawdata_estimation, "Bigmartpredictionsfinal.csv")

