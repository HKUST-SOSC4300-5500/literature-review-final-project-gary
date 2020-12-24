library(ggplot2)
library(cowplot)
library(randomForest)
library(caret)
library(mlbench)

# To confirm that caret has been loaded, one can run the following code.
library(caret)

loaded_packages  <- library()$results[,1]

# confirm caret is loaded
"caret" %in% tolower(loaded_packages)

data <- read.csv('/Users/choyuching/Downloads/Corrupted amount modified_1.csv')
head(data)
str(data)

data$status_type<-as.factor(data$status_type)
data$year_born <-as.factor(data$year_born)
data$gender<-as.factor(data$gender)
data$native_province<-as.factor(data$native_province)
data$corruption_location<-as.factor(data$corruption_location)
data$local_official<-as.factor(data$local_official)
data$rankErin<-as.factor(data$rankErin)
data$sentence_details<-as.factor(data$sentence_details)
data$monetary_gain <-as.factor(data$monetary_gain)
data$corrupted_amount<-as.factor(data$corrupted_amount)
data$sector<-as.factor(data$sector)
data$connections_with_tigers<-as.factor(data$connections_with_tigers)
data$oversea<-as.factor(data$oversea)
data$head_vice<-as.factor(data$head_vice)


str(data)

set.seed(200)
data.imputed <- rfImpute(status_type ~ ., data = data, iter=6)

xtabs(~ monetary_gain + gender, data=data)
xtabs(~ monetary_gain + status_type, data=data)
xtabs(~ monetary_gain + year_born, data=data)
xtabs(~ monetary_gain + native_province, data=data)
xtabs(~ monetary_gain + corruption_location, data=data)
xtabs(~ monetary_gain + local_official, data=data)
xtabs(~ monetary_gain + rankErin, data=data)
xtabs(~ monetary_gain + sentence_details, data=data)
xtabs(~ monetary_gain + sector, data=data)
xtabs(~ monetary_gain + corrupted_amount, data=data)
xtabs(~ monetary_gain + connections_with_tigers, data=data)
xtabs(~ monetary_gain + head_vice, data=data)
xtabs(~ monetary_gain + oversea, data=data)


# let's start super simple and see if status_type (Tiger/Fly) is a good
## predictor...
## First, let's just look at the raw data...
xtabs(~ monetary_gain + status_type, data=data.imputed)

###########
fitControll <- trainControl(method ="cv", number = 10, savePredictions= T)
mod_fitcv1 <- train(monetary_gain ~ status_type, data = data.imputed, method = "glm" ,family="binomial",trControl = fitControll)


summary(mod_fitcv1)
mod_fitcv1

caret::confusionMatrix(table((mod_fitcv1$pred)$pred, (mod_fitcv1$pred)$obs))

#ggplot(mod_fitcv1)
# Error in ggplot.train(mod_fitcv1) : 
#There are no tuning parameters for this model.

## We can plot the data...
logistic <- glm(monetary_gain ~ status_type, data=data.imputed, family="binomial")
summary(logistic)

predicted.data <- data.frame(
  probability.of.monetary_gain=logistic$fitted.values,
  status_type=data.imputed$status_type)

ggplot(data=predicted.data, aes(x=status_type, y=probability.of.monetary_gain)) +
  geom_point(aes(color=status_type), size=5) +
  xlab("status_type") +
  ylab("Predicted probability of having monetary_gain")

ggsave("/Users/choyuching/Downloads/1224 0200_vs status_types.png")

## Since there are only two probabilities (one for tigers and one for flies),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.monetary_gain + status_type, data=predicted.data)



## Since there are only two probabilities (one for tigers and one for flies),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.monetary_gain + status_type, data=predicted.data)

#####################################
##
## Now we will use all of the data available to predict monetary_gain
##
#####################################

fitControll <- trainControl(method ="cv", number = 10, savePredictions= T)
mod_fitcv2 <- train(monetary_gain ~ .
                    , data = data.imputed, method = "glm" ,family="binomial",trControl = fitControll)


#summary(mod_fitcv2)
#mod_fitcv2

#caret::confusionMatrix(table((mod_fitcv2$pred)$pred, (mod_fitcv2$pred)$obs))

#ggplot(mod_fitcv2)
# Error in ggplot.train(mod_fitcv2) : 
#There are no tuning parameters for this model.


## now we can visualizing the data

logistic <- glm(monetary_gain ~ ., data=data.imputed, family="binomial")
summary(logistic)

predicted.data <- data.frame(
  probability.of.monetary_gain=logistic$fitted.values,
  monetary_gain=data.imputed$monetary_gain)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.monetary_gain, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)



## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.monetary_gain  )) +
  geom_point(aes(color=monetary_gain), alpha=1, shape=4, stroke=1) +
  xlab("Index of the observation") +
  ylab("Probability of having monetary_gain")
ggsave("/Users/choyuching/Downloads/Caret_1.png")


#Comparsion
mod_fitcv2
summary(mod_fitcv2)
caret::confusionMatrix(table((mod_fitcv2$pred)$pred, (mod_fitcv2$pred)$obs))


## now we can visualizing the data using proposed model

logistic2 <- glm(monetary_gain ~  native_province + corruption_location + local_official + connections_with_tigers + head_vice, data=data.imputed, family="binomial")
summary(logistic2)


predicted.data <- data.frame(
  probability.of.monetary_gain=logistic2$fitted.values,
  monetary_gain=data.imputed$monetary_gain)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.monetary_gain, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)

## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.monetary_gain  )) +
  geom_point(aes(color=monetary_gain), alpha=1, shape=4, stroke=1) +
  xlab("Index of the observation") +
  ylab("Probability of having monetary_gain")
ggsave("/Users/choyuching/Downloads/Caret_proposed.png")

#Stat for pruposed model using 6 predictors.
fitControll <- trainControl(method ="cv", number = 10, savePredictions= T)
mod_fitcv3 <- train(monetary_gain ~  native_province + corruption_location + local_official + connections_with_tigers + oversea + head_vice
                    , data = data.imputed, method = "glm" ,family="binomial",trControl = fitControll)

mod_fitcv3
summary(mod_fitcv3)
caret::confusionMatrix(table((mod_fitcv3$pred)$pred, (mod_fitcv3$pred)$obs))
