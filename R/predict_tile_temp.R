
#Basic idea here is to work out whether tile temp differentials can be predicted using RandomForest/gams,
#based on observed meteorological variables.

library(randomForest)
library(dplyr)
library(lubridate)

load("prepped_data.Rdata")

Tiledat<-DelmaFiltered %>%
	   select(AirTemp, SoilTemp, HumidA, Time, Sun, Cloud, yeardayfrac) %>%
	   mutate(TDiff=SoilTemp-AirTemp) %>%
	   mutate(Time =hour(Time)+ minute(Time)/60) %>%  #convert time to hour fraction
	   select(TDiff, everything()) %>%
	   filter(SoilTemp<55) %>%    #filter out implausibe soil temperatures
	   select(-SoilTemp, -Sun) %>%
	   filter(complete.cases(.)) %>%
	   filter(TDiff>-10, Time>6)  #remove outliers

samp<-sample(nrow(Tiledat), nrow(Tiledat)*0.8) #80% training partition
train<-Tiledat[samp,] #training data
test<-Tiledat[-samp,] #test data

## Random forest model

mod2<-randomForest(TDiff~AirTemp+HumidA+Cloud+Time+yeardayfrac, data=train, 
									 mtry=3, ntree=1000, importance=TRUE)

mod2_test<-predict(mod2, newdata=test)
#RMSE on the test data
mean(sqrt((mod2_test-test$TDiff)^2))


plot(mod2)

barplot(varUsed(mod2))

library(mgcv)

## GAM model

gammod<-gam(TDiff~s(AirTemp)+
							    s(HumidA)+
							    Cloud+
							    s(Time)+
							    s(yeardayfrac)
							    , data=train, select=TRUE)
plot(gammod)

gammod_test<-predict(gammod, newdata=test)

mean(sqrt((gammod_test-test$TDiff)^2))

plot(mod2_test~test$TDiff, pch=16, cex=0.3); abline(0, 1)
points(y=gammod_test, x=test$TDiff, pch=16, col="red", cex=0.3)

#XGboost model (current winner)

library(xgboost)

train_dat<-as.matrix(train[,-1])
train_lab<-as.numeric(train$TDiff)

xgb<-xgboost(data=train_dat, label=train_lab, nrounds=100, eta=0.05, gamma=0.75, max_depth=8,nthread=4,subsample = 0.75,
						 colsample_bytree = 1, min_child_weight=0.5, eval_metric="rmse")

xgb_test<-predict(xgb, newdata=as.matrix(test[,-1]))				 
mean(sqrt((xgb_test-test$TDiff)^2)) #1.857

plot(xgb_test~test$TDiff, pch=16, cex=0.4)
points(y=gammod_test, x=test$TDiff, pch=16, cex=0.4, col="red")
points(y=mod2_test, x=test$TDiff, pch=16, cex=0.4, col="blue") ;abline(0, 1)

xgb.importance(xgb, feature_names = NULL)

### Grid search of XGBoost Hyperparameters. Takes 10 mins or so to run. A finer grid might help obtain better parameters.
library(caret)
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, 
												#summaryFunction = twoClassSummary,
												allowParallel=T)

xgb.grid <- expand.grid(nrounds = c(10, 50, 100),
												eta = c(0.01,0.05,0.1),
												max_depth = c(2,4,6,8),
												gamma=c(0.01, 0.25, 0.75, 1), 
												colsample_bytree=c(0.5, 0.75, 1), 
												min_child_weight=c(0.5, 0.75, 1), 
												subsample=c(0.5, 0.75, 1)
)
set.seed(45)
xgb_tune <-train(TDiff~AirTemp+HumidA+Cloud+Time+yeardayfrac,
								 data=train,
								 method="xgbTree",
								 trControl=cv.ctrl,
								 tuneGrid=xgb.grid,
								 verbose=T,
								 metric="RMSE",
								 nthread =3
)
