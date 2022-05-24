install.packages("dplyr")
install.packages("DataExplorer")
install.packages("xgboost")
install.packages("data.table")
install.packages("mltools")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("GGally")
library(GGally)
install.packages("reshape2")
library(reshape2)
library("DataExplorer")
library("xgboost")
library("data.table")
library("mltools")
library("glmnet")
library("ggplot2")
library('ggcorrplot')
library(dplyr)

train <- read.csv(file.choose())
test <- read.csv(file.choose())

dim(train)
dim(test)
names(train)
housedf<-rbind(train,test)
dim(housedf)
colSums(is.na(housedf))
unique(train$YrSold)
housedf_na <- housedf[ ,colSums(is.na(housedf)) > 0]

plot_missing(housedf_na)

# the variables, where NAs > 40% of the number of records in these variables)

housedf_na80 <- train[ ,colSums(is.na(housedf)) > 0.8 * nrow(housedf)]
names(housedf_na80)

#checking again we now have a lot of missing values in LotFrontage, Alley, 
# MasVnrType, MasVnrArea, BsmtQual, BsmtCond, BsmtExposure, BsmtFinType1,
# BsmtFinType2, Electrical, FireplaceQu, GarageType, GarageYrBlt, GarageFinish,
# GarageQual, GarageCond, PoolQC, Fence, and MiscFeature. 

#-----------------Preprocessing---------------------------------------------
# Assumptions: The bsmt values re 0 bc these house might not have 
# the basement
table(housedf$Street)
table(housedf$Alley)
table(housedf$MSZoning)
table(housedf$Electrical)
table(housedf$Utilities)
table(housedf$LandSlope)
table(housedf$Condition1)
table(housedf$Condition2)
table(housedf$RoofMatl)
table(housedf$RoofStyle)
table(housedf$Heating)
table(housedf$CentralAir)
table(housedf$LowQualFinSF)
table(housedf$KitchenAbvGr)
table(housedf$Functional)
table(housedf$PavedDrive)
table(housedf$GarageCond)
table(housedf$MiscVal)
table(housedf$MiscFeature)
table(housedf$LandContour)
table(housedf$LotConfig)
table(housedf$Exterior1st)
table(housedf$Exterior2nd)
table(housedf$SaleType)
housedf[which(is.na(housedf$BsmtFullBath)), c("BsmtFullBath", "BsmtQual")]
housedf1[which(is.na(housedf1$GarageCars)), c("GarageCars", "GarageQual")]
housedf1[which(is.na(housedf1$KitchenQual)), c("KitchenQual", "KitchenAbvGr")]
table(housedf1$KitchenQual)
housedf1 <- housedf %>% 
  mutate(BsmtQual = coalesce(BsmtQual, "None"),
         BsmtCond = coalesce(BsmtCond, "None"),
         BsmtExposure = coalesce(BsmtExposure, "None"),
         BsmtFinType1 = coalesce(BsmtFinType1, "None"),
         BsmtFinType2 = coalesce(BsmtFinType2, "None"),
         FireplaceQu = coalesce(FireplaceQu, "None"),
         GarageType = coalesce(GarageType, "None"),
         GarageFinish = coalesce(GarageFinish, "None"),
         GarageQual = coalesce(GarageQual, "None"),
         GarageCond = coalesce(GarageCond, "None"),
         MasVnrType = coalesce(MasVnrType, "None"),
         MasVnrArea = coalesce(MasVnrArea, 0),
         GarageYrBlt = coalesce(GarageYrBlt, 0),
         Electrical = coalesce(Electrical, "SBrkr"),
         PoolQC = coalesce(PoolQC, "None"),
         MSZoning = coalesce(Electrical, "RL"),
         Functional = coalesce(Functional, "Typ"),
         Utilities = coalesce(Utilities, "AllPub"),
         GarageArea = coalesce(GarageArea, 0),
         BsmtUnfSF = coalesce(BsmtUnfSF, 0),
         BsmtFinSF2 = coalesce(BsmtFinSF2, 0),
         BsmtFinSF1 = coalesce(BsmtFinSF1, 0),
         TotalBsmtSF = coalesce(TotalBsmtSF, 0),
         Exterior1st = coalesce(Exterior1st, "VinylSd"),
         Exterior2nd = coalesce(Exterior2nd, "VinylSd" ),
         BsmtFullBath = coalesce(BsmtFullBath, 0),
         BsmtHalfBath = coalesce(BsmtHalfBath, 0),
         SaleType = coalesce(SaleType, "WD"),
         GarageCars = coalesce(GarageCars, 0),
         GarageCars = coalesce(GarageCars, 0),
         KitchenQual = coalesce(KitchenQual, "TA"))


housedf1 = subset(housedf1, select=-c(MiscFeature, Alley, Fence))


housedf1 <- as.data.frame(housedf1 %>% group_by(LotShape) %>% 
                         mutate(LotFrontage=ifelse(is.na(LotFrontage), 
                                                   mean(LotFrontage, na.rm=TRUE), LotFrontage)))

colSums(is.na(housedf1))


#-------------------EDA-----------------------------
train <- housedf1[1:1460, ]
test <- housedf1[1461:2919, ]
dim(train)
dim(test)

library(scales)
ggplot(train, aes(x=SalePrice)) + 
  geom_histogram(binwidth = 15000 ,color="red",fill="blue")+ 
  scale_x_continuous(labels = comma)  

train %>% count(SaleCondition)
train1<-train[!(train$SaleCondition=="Abnorml" | train$SaleCondition=="AdjLand"|
               train$SaleCondition=="Alloca" | train$SaleCondition=="Family"|
               train$SaleCondition=="Partial"),]
test <- test[!(test$SaleCondition=="Abnorml" | test$SaleCondition=="AdjLand"|
                 test$SaleCondition=="Alloca" | test$SaleCondition=="Family"|
                 test$SaleCondition=="Partial"),]

# For explaination:

mean_value_SCondition <- train1 %>% 
  group_by(SaleCondition) %>% 
  summarise(average=mean(SalePrice))
mean_value_SCondition
mean(train$SalePrice)
train <- train1
# Explain why exclude based on saleprice mean, and the 
# commercial reasons behind the action.
#---1
train1 <- train1 %>% 
  arrange(YrSold)
train1$YrSold

suburbs <- train1 %>% 
  select(Neighborhood,YrSold, SalePrice) %>% 
  group_by(Neighborhood, YrSold) %>% 
  summarise(mean=mean(SalePrice))

ggplot(suburbs, aes(x=YrSold, y=mean, col=Neighborhood))+
         geom_line() +
         xlab("Year of Sales") +
         ggtitle("Number of sales in each neighborhood by year") +
         theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
               axis.title.x = element_text(size = 12, hjust = 0.5),
               axis.text.x = element_text(angle = 45, vjust = 0.5),
               axis.title.y = element_text(size = 12, hjust = 0.5),
               legend.position = "right")+
  scale_y_continuous(name = "Average Saleprices", labels = scales::comma)
         
top10_2006 <- suburbs %>% 
  filter(YrSold == 2006) %>% 
  arrange(desc(mean)) %>% 
  head(top10_2006, n=10)

top10_2007 <- suburbs %>% 
  filter(YrSold == 2007) %>% 
  arrange(desc(mean)) %>% 
  head(top10_2007, n=10)

top10_2008 <- suburbs %>% 
  filter(YrSold == 2008) %>% 
  arrange(desc(mean)) %>% 
  head(top10_2008, n=10)

top10_2009 <- suburbs %>% 
  filter(YrSold == 2009) %>% 
  arrange(desc(mean)) %>% 
  head(top10_2009, n=10)

top10_2010 <- suburbs %>% 
  filter(YrSold == 2010) %>% 
  arrange(desc(mean)) %>% 
  head(top10_2010, n=10)

#--2 Lot Frontage 

ggplot(train1, aes(y = SalePrice, x = LotFrontage, col = LotFrontage)) +
  geom_point() +
  ggtitle("Lotfrontage - sales relationship") +
  ylab("Sale Prices") +
  xlab("Lot Frontage (feet)") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, vjust = 1, size = 8, hjust = 1))+
  scale_y_continuous(name = "Sale Prices", labels = scales::comma)


# Some suburbs lack the statistics in some years
# Overall quality
density <- train1 %>% 
  group_by(OverallQual) %>% 
  summarise(average=mean(SalePrice))
  
ggplot(density, aes(y=average, x=OverallQual, col=OverallQual, fill=OverallQual))+
  geom_bar(stat = "identity")+
  scale_y_continuous(name = "Sale Prices", labels = scales::comma)
# housestyle
table(train1$HouseStyle)

HStyle <- train1 %>% 
  select(HouseStyle, SalePrice, YrSold) %>% 
  group_by(HouseStyle, YrSold) %>% 
  summarise(mean_HS=mean(SalePrice))

ggplot(HStyle, aes(x=YrSold, y=mean_HS, col=HouseStyle))+
  geom_line() +
  xlab("Year of Sales") +
  ggtitle("Average sales price for each house styles by year") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "right")+
  scale_y_continuous(name = "Average Saleprices", labels = scales::comma)

#--4 kitchen
table(train1$KitchenQual)

KitchenQ <- train1 %>% 
  group_by(KitchenQual, YrSold) %>% 
  summarise(mean_kitchen=mean(SalePrice))

ggplot(KitchenQ, aes(x=YrSold, y=mean_kitchen, col=KitchenQual))+
  geom_line() +
  xlab("Year of Sales") +
  ggtitle("Average sales for kitchen quality by year") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "right")+
  scale_y_continuous(name = "Average Saleprices", labels = scales::comma)

#--5 basement quality
# Is that different between the house with or without basement?
Meanbsmtqual <- train1 %>%  
  group_by(BsmtQual, YrSold) %>% 
  summarise(meanbsmt=mean(SalePrice), numbsmtq=n())
# Conclusion: The saleprice is higher with basement
ggplot(Meanbsmtqual, aes(x=YrSold, y=meanbsmt, col=BsmtQual))+
  geom_bar(stat='identity', fill='white')+
  xlab("Year of Sales") +
  ylab("Average Sales price") +
  ggtitle("Number of sales in basement quality by year") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "right")
 
# Basement is not so relevant to the sale prices 

dim(test)
dim(train)
#-- feature engineering
housedf1 <- rbind(train,test)
colSums(is.na(housedf1))
dim(housedf1)
house <- housedf1
housedf1 <- house
table(housedf1$BsmtQual)
housedf1$ExterQual<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1)[housedf1$ExterQual]
housedf1$ExterCond<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1)[housedf1$ExterCond]
housedf1$BsmtQual<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1,None=0)[housedf1$BsmtQual]
housedf1$BsmtCond<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$BsmtCond]
housedf1$BsmtExposure<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$BsmtExposure]
housedf1$BsmtFinType1<-c(GLQ=6,ALQ=5,BLQ=4,Rec=3,LwQ=2,Unf=1)[housedf1$BsmtFinType1]
housedf1$BsmtFinType2<-c(GLQ=6,ALQ=5,BLQ=4,Rec=3,LwQ=2,Unf=1)[housedf1$BsmtFinType2]
housedf1$HeatingQC<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$HeatingQC]
housedf1$KitchenQual<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$KitchenQual]
housedf1$FireplaceQu<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$FireplaceQu]
housedf1$GarageQual<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$GarageQual]
housedf1$GarageCond<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$GarageCond]
housedf1$PoolQC<-c(Ex=5,Gd=4,TA=3,Fa=2,Po=1, None=0)[housedf1$PoolQC]
housedf1$Functional<-c(Typ=7,Min1=6,Min2=5,Mod=4,Maj1=3,Maj2=2,Sev=1)[housedf1$Functional]

# Handling features by combining
housedf1$TotalArea<-housedf1$X1stFlrSF+housedf1$X2ndFlrSF+housedf1$TotalBsmtSF
housedf1$TotalFinSF<-housedf1$BsmtFinSF1+housedf1$BsmtFinSF2+housedf1$X1stFlrSF+housedf1$X2ndFlrSF
housedf1$TotBath<-housedf1$BsmtFullBath+(0.5*housedf1$BsmtHalfBath)+housedf1$FullBath+(0.5*housedf1$HalfBath)
housedf1$TotPorch<-housedf1$OpenPorchSF+housedf1$EnclosedPorch+housedf1$X3SsnPorch+housedf1$ScreenPorch
housedf1$OverallQC <- housedf1$OverallCond * housedf1$OverallQual
housedf1$GarageQC <- housedf1$GarageCond * housedf1$GarageQual
housedf1$ExterQC <- housedf1$ExterQual * housedf1$ExterCond
housedf1$KitchenQ <- housedf1$KitchenAbvGr * housedf1$KitchenQual
housedf1$FireplaceQ <- housedf1$Fireplaces * housedf1$FireplaceQu
housedf1$ExterQC <- housedf1$ExterQual * housedf1$ExterCond
housedf1$Age <- housedf1$YrSold - housedf1$YearBuilt
str(housedf1$YearBuilt)
colSums(is.na(housedf1))

housedf1$location = housedf1$OverallQual/ mean(housedf1$OverallQual) +
  housedf1$OverallCond/ mean(housedf1$OverallCond)+
  housedf1$ExterQual/ mean(housedf1$ExterQual)+
  housedf1$ExterCond/ mean(housedf1$ExterCond)+
  housedf1$Functional/ mean(housedf1$Functional)
   
mean(housedf1$location) 
as.factor(housedf1$location)
train <- housedf1[1:1198, ]
test <- housedf1[1199:2402, ]
dim(train)
dim(test)

# For location in train set
ScoreTrain<-train %>% group_by(Neighborhood) %>% 
  summarise(neighmean=mean(location)) %>% 
  arrange(neighmean)
ScoreTrain
Bottom<- c('MeadowV', 'Edwards', 'IDOTRR', 'Sawyer', 'BrDale',"SWISU")
upperlow <- c('ClearCr', 'BrkSide', 'Mitchel', "NAmes","NPkVill","OldTown")
MidHigh<- c("Gilbert", "SawyerW", "NWAmes", "Timber", "CollgCr","Crawfor")
High <- c("Blueste", "Somerst", "Blmngtn", "StoneBr", "NoRidge", "Veenker", "NridgHt")
ScoreTrain$Neighborhood
train[(train$Neighborhood %in% Bottom),88] = 1
train[(train$Neighborhood %in% upperlow),88 ] = 2
train[(train$Neighborhood %in% MidHigh),88 ] = 3
train[(train$Neighborhood %in% High),88 ] = 4
locationscore <- train %>% group_by(location) %>% 
  summarise(mean=mean(SalePrice))  
ggplot(locationscore, aes(x=location, y=mean, col=location, fill=location))+
  geom_bar(stat='identity')

#For location in test set
test[(test$Neighborhood %in% Bottom),88] = 1
test[(test$Neighborhood %in% upperlow),88 ] = 2
test[(test$Neighborhood %in% MidHigh),88 ] = 3
test[(test$Neighborhood %in% High),88 ] = 4

train1 <- subset(train, select = -c(X1stFlrSF, X2ndFlrSF, TotalBsmtSF, 
                                     BsmtFinSF1, BsmtFinSF2, X1stFlrSF, X2ndFlrSF,
                                     Street, MSZoning, Electrical, Utilities,
                                     LandSlope, Condition1, Condition2, RoofMatl,
                                     RoofStyle, Heating, CentralAir, LowQualFinSF,
                                     KitchenAbvGr, Functional, PavedDrive, GarageCond,
                                     MiscVal, LandContour, OpenPorchSF, EnclosedPorch,
                                     X3SsnPorch, ScreenPorch, SaleType, SaleCondition,
                                     GarageFinish, GarageType, Foundation, MasVnrType,
                                     Exterior2nd, Exterior1st, Id, SaleCondition, LotShape, LotConfig,
                                     Neighborhood, BldgType, HouseStyle))
train1$location <- as.numeric(train1$location)
train1$HouseStyle <- NULL
correlations <- cor(train1)
correlations <- as.data.frame(correlations)
names <- row.names(correlations)
priceCorrelation <- correlations$SalePrice
correlations <- cbind(names, priceCorrelation)
correlations <- correlations[order(-priceCorrelation),]
correlations[1:13, 1:2]

train2 <- subset(train1, select=c(SalePrice, TotalArea, TotalFinSF,
                                  OverallQual, GrLivArea, 
                                  ExterQual, TotBath, KitchenQual,
                                  GarageCars, Age, location, GarageArea))
ggpairs(train2)
ggcorr(train2, name="Correlation", label=T)
ggpairs()

ggplot(train1,aes(y=SalePrice,x=GrLivArea))+
  geom_point()+
  stat_smooth()
ggplot(train1,aes(y=SalePrice,x=Age))+
  geom_point()
ggplot(train1,aes(y=SalePrice,x=TotalArea))+
  geom_point()+
  stat_smooth()
ggplot(train1,aes(y=SalePrice,x=TotalFinSF))+
  geom_point()+
  stat_smooth()
ggplot(train1,aes(y=SalePrice,x=OverallQual))+
  geom_point()
ggplot(train1,aes(y=SalePrice,x=GarageCars))+
  geom_point()
ggplot(train1,aes(y=SalePrice,x=ExterQual))+
  geom_point()
ggplot(train1,aes(y=SalePrice,x=TotBath))+
  geom_point()
ggplot(train1,aes(y=SalePrice,x=KitchenQual))+
  geom_point()

ggplot(train1,aes(y=SalePrice,x=location))+
  geom_point()
ggplot(train1,aes(y=SalePrice,x=GarageArea))+
  geom_point()+
  stat_smooth()
ggplot(train1,aes(y=SalePrice,x=TotBath))+
  geom_point()
train1[which(train1$GrLivArea > 4000), c("GrLivArea", "SalePrice")]
train <- train[-c(4316),]

#-----------------Modelling--------------------------
library(modelr)
library(broom)
housedf1<-rbind(train,test)
house <- housedf1
dim(housedf1)
trainset<-house[1:1197,]
testset<-house[1198:2401,]
testset$SalePrice

testset$labelT= log(testset$SalePrice)
label= log(trainset$SalePrice)

highvar_model <- lm(label ~ TotalArea, data = trainset)
print(highvar_model)

summary(highvar_model)
plot(highvar_model, 1)
plot(highvar_model, 2)
plot(highvar_model, 3)
plot(highvar_model, 4)


testset$predicted <- predict(highvar_model, testset)

actuals <- testset$labelT
predictions <- testset$predicted

sqrt(mean((predictions-actuals)^2))

plot(highvar_model)

rmse(highvar_model, data=trainset)

outlierTest(highvar_model)

# Test the second model on the testing data and evaluate its performance using RMSE metrics

multi_model <- lm(label ~ OverallQual + TotalArea
                  + TotalFinSF + GrLivArea + Age +
                    ExterQual + KitchenQual
                  + GarageCars + location + TotBath + GarageArea, data = trainset)
print(multi_model)

summary(multi_model)
plot(multi_model, 1)
plot(multi_model, 2)
plot(multi_model, 3)
plot(multi_model, 4)

testset$predicted2 <- predict(multi_model, testset)

actuals <- testset$labelT
predictions <- testset$predicted2

sqrt(mean((predictions-actuals)^2))

plot(multi_model)

rmse(multi_model, data=trainset)

# Residual values

lmpred <- predict(highvar_model, newdata = testset)
lmdata <- testset %>% mutate(y = log(SalePrice)) %>% 
  mutate(ybar = lmpred) %>% mutate(diff = abs(y - ybar))

badlmdata <- lmdata %>% filter(diff > 0.5) %>% arrange(desc(diff))

lmresiduals <- ggplot(lmdata, aes(x = y, y = y-ybar), col = diff) +
  geom_point(aes(x = y, y = y-ybar, color = diff)) +
  geom_point(data = badlmdata, colour="red") +
  scale_color_gradient(name = "|y - ybar|", limits = c(0, 1.5)) +
  geom_abline(slope = 1, intercept = 0) +
  xlab("y") +
  ylab("y-ybar") +
  ggtitle("Linear model residuals") +
  theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(size = 12, hjust = 0.5),
        axis.title.y = element_text(size = 12, hjust = 0.5),
        legend.position = "right",
        legend.spacing.y = unit(0.5, 'cm'))

lmresiduals
