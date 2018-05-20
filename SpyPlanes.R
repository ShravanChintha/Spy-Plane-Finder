#Spy Plane finder

#Loading libraries for reading data from CSV files

library(readr)
library(dplyr)

library(Amelia)

#loading full registration data of the planes

registered= read.csv('faa_registration.csv',header=T,na.strings=c(""))

#plotting missing values plot

missmap(registered, main = "Missing values vs observed")

#loading features and training data

features = read.csv('planes_features.csv',header=T,na.strings=c(""))

training = read.csv('train.csv',header=T,na.strings=c(""))

#loading known federation planes data
feds <- read.csv("feds.csv")

features = features %>% mutate(type2=as.integer(as.factor(type)))

head(features)

training <- read.csv("train.csv") %>% inner_join(features, by="adshex")

head(training)

formula =as.factor(class) ~ duration1 + duration2 + duration3 + duration4 + duration5 + boxes1 + boxes2 + boxes3 + boxes4 + boxes5 + speed1 + speed2 + speed3 + speed4 + speed5 + altitude1 + altitude2 + altitude3 + altitude4 + altitude5 + steer1 + steer2 + steer3 + steer4 + steer5 + steer6 + steer7 + steer8 + flights + squawk_1 + observations + type2

#random forest model

library(randomForest)
set.seed(35)

rfmodel <- randomForest(formula,data=training,metric="ROC",importance=TRUE,ntree=1000)

rfmodel

#plotting variable importance plot
library(ggplot2)
varImpPlot(rfmodel, pch = 20, main = "Variable Importance", color = "blue", cex = 1)

#removing training planes and known federation planes data from features data
labeling <- anti_join(features, training) %>% anti_join(feds)

head(labeling)

#predicting classes for the labeling data based on training results
labelrf <- predict(rfmodel, labeling)

#storing the predicted labels in dataframe based on each plane using thier adshex code
labelrf_df <- data.frame(adshex = labeling$adshex, class = labelrf)

#printing the planes summary based on each type
typesrf <- labelrf_df %>% group_by(class) %>% summarize(count=n())

print(typesrf)

#getting prediction probabilities for the 2 classes
#based on probabilities, each type is classified into 
#surveil or other classes

rfprobs <- as.data.frame(predict(rfmodel, labeling, type = "prob"))

head(rfprobs)

#with the probabilities, labeling the class and storing in a dataframe
#in descending order of Surveil class data
rfprobs_df<- bind_cols(as.data.frame(labeling$adshex), rfprobs) %>% mutate(adshex = labeling$adshex) %>%
  select(2:4) %>% arrange(desc(surveil)) %>%
  inner_join(features) %>%  select(1:3,squawk_1)

#displaying resulting probabilities and storing
rfresults <- head(rfprobs_df, 1000)

head(rfresults)

#getting n_number, name and adshex from planes registration details based on adshex 
registered <- registered %>% select(1,7,34)

names(registered) <- c("n_number","name","adshex")

registered <- registered %>%  mutate(reg = paste0("N",n_number)) %>%  select(2:4)

#joining the results with planes registraion details through adshex
rfresults <- left_join(rfresults,registered, by="adshex")

#exporting the resulting data to a csv file
write.csv(rfresults, "rfresults.csv", na="")

#-------------
  
#CART

library(caret)

set.seed(71)

#applying CART model on the training data
cartmodel = train(formula,data=training, method="rpart")

cartmodel

plot(cartmodel)

#results of the model
summary(cartmodel)

#predicting the labels for remaining data using trained input
labelcart=predict(cartmodel, labeling)

head(labelcart)

#storing the predicted labels in dataframe based on each plane using thier adshex code
labelcart_df <- data.frame(adshex = labeling$adshex, class = labelcart)

#printing the planes summary based on each type
typescart <- labelcart_df %>% group_by(class) %>% summarize(count=n())

print(typescart)

#getting prediction probabilities for the 2 classes
#based on probabilities, each type is classified into 
#surveil or other classes

cartprobs <- as.data.frame(predict(cartmodel, labeling, type = "prob"))

head(cartprobs)

#with the probabilities, labeling the class and storing in a dataframe
#in descending order of Surveil class data
cartprobs_df<- bind_cols(as.data.frame(labeling$adshex), cartprobs) %>% mutate(adshex = labeling$adshex) %>%
  select(2:4) %>% arrange(desc(surveil)) %>%
  inner_join(features) %>%  select(1:3,squawk_1)

#displaying resulting probabilities and storing
cartresults <- head(cartprobs_df, 1000)

head(cartresults)

#joining the results with planes registraion details through adshex
cartresults <- left_join(cartresults,registered, by="adshex")

#exporting the resulting data to a csv file
write.csv(cartresults, "cartresults.csv", na="")

#-------------

#evtree

library(evtree)

set.seed(19)

#applying evtree model on the training data
evtreemodel = evtree(formula,data=training, method="class")

evtreemodel

#plotting the tree
plot(evtreemodel)

#predicting the labels for remaining data using trained input
labelevtree=predict(evtreemodel, labeling)

head(labelevtree)

#storing the predicted labels in dataframe based on each plane using thier adshex code
labelevtree_df <- data.frame(adshex = labeling$adshex, class = labelevtree)

#printing the planes summary based on each type
typesevtree <- labelevtree_df %>% group_by(class) %>% summarize(count=n())

print(typesevtree)

#getting prediction probabilities for the 2 classes
#based on probabilities, each type is classified into 
#surveil or other classes

evtreeprobs <- as.data.frame(predict(evtreemodel, labeling, type = "prob"))

head(evtreeprobs)

#with the probabilities, labeling the class and storing in a dataframe
#in descending order of Surveil class data
evtreeprobs_df<- bind_cols(as.data.frame(labeling$adshex), evtreeprobs) %>% mutate(adshex = labeling$adshex) %>%
  select(2:4) %>% arrange(desc(surveil)) %>%
  inner_join(features) %>%  select(1:3,squawk_1)

#displaying resulting probabilities and storing
evtreeresults <- head(evtreeprobs_df, 1000)

head(evtreeresults)

#joining the results with planes registraion details through adshex
evtreeresults <- left_join(evtreeresults,registered, by="adshex")

#exporting the resulting data to a csv file
write.csv(evtreeresults, "evtreeresults.csv", na="")
#-------------

#ctree

library(partykit)

set.seed(87)

#applying ctree model on the training data
ctreemodel = ctree(formula,data=training)

ctreemodel

#plotting the tree
plot(ctreemodel)

#predicting the labels for remaining data using trained input
labelctree=predict(ctreemodel, labeling)

head(labelctree)

#storing the predicted labels in dataframe based on each plane using thier adshex code
labelctree_df <- data.frame(adshex = labeling$adshex, class = labelctree)

#printing the planes summary based on each type
typesctree <- labelctree_df %>% group_by(class) %>% summarize(count=n())

print(typesctree)

#getting prediction probabilities for the 2 classes
#based on probabilities, each type is classified into 
#surveil or other classes

ctreeprobs <- as.data.frame(predict(ctreemodel, labeling, type = "prob"))

head(ctreeprobs)

#with the probabilities, labeling the class and storing in a dataframe
#in descending order of Surveil class data
ctreeprobs_df<- bind_cols(as.data.frame(labeling$adshex), ctreeprobs) %>% mutate(adshex = labeling$adshex) %>%
  select(2:4) %>% arrange(desc(surveil)) %>%
  inner_join(features) %>%  select(1:3,squawk_1)

#displaying resulting probabilities and storing
ctreeresults <- head(ctreeprobs_df, 1000)

head(ctreeresults)

#joining the results with planes registraion details through adshex
ctreeresults <- left_join(ctreeresults,registered, by="adshex")

#exporting the resulting data to a csv file
write.csv(ctreeresults, "ctreeresults.csv", na="")

#-------------

#C4.5

library(RWeka)

set.seed(57)

#applying c45 model on the training data
c45model = J48(formula,data=training)

c45model

plot(c45model)

#results of the model
summary(c45model)

#predicting the labels for remaining data using trained input
labelc45=predict(c45model, labeling)

head(labelc45)

#storing the predicted labels in dataframe based on each plane using thier adshex code
labelc45_df <- data.frame(adshex = labeling$adshex, class = labelc45)

#printing the planes summary based on each type
typesc45 <- labelc45_df %>% group_by(class) %>% summarize(count=n())

print(typesc45)

#getting prediction probabilities for the 2 classes
#based on probabilities, each type is classified into 
#surveil or other classes

c45probs <- as.data.frame(predict(c45model, labeling, type = "prob"))

head(c45probs)

#with the probabilities, labeling the class and storing in a dataframe
#in descending order of Surveil class data
c45probs_df<- bind_cols(as.data.frame(labeling$adshex), c45probs) %>% mutate(adshex = labeling$adshex) %>%
  select(2:4) %>% arrange(desc(surveil)) %>%
  inner_join(features) %>%  select(1:3,squawk_1)

#displaying resulting probabilities and storing
c45results <- head(c45probs_df, 1000)

head(c45results)

#joining the results with planes registraion details through adshex
c45results <- left_join(c45results,registered, by="adshex")

#exporting the resulting data to a csv file
write.csv(c45results, "c45results.csv", na="")

#-------------

#bagging

library(ipred)

set.seed(29)

#applying bagging model on the training data
baggingmodel = bagging(formula,data=training)

#results of the model
summary(baggingmodel)

#predicting the labels for remaining data using trained input
labelbagging=predict(baggingmodel, labeling)

head(labelbagging)

#storing the predicted labels in dataframe based on each plane using thier adshex code
labelbagging_df <- data.frame(adshex = labeling$adshex, class = labelbagging)

#printing the planes summary based on each type
typesbagging <- labelbagging_df %>% group_by(class) %>% summarize(count=n())

print(typesbagging)

#getting prediction probabilities for the 2 classes
#based on probabilities, each type is classified into 
#surveil or other classes

baggingprobs <- as.data.frame(predict(baggingmodel, labeling, type = "prob"))

head(baggingprobs)

#with the probabilities, labeling the class and storing in a dataframe
#in descending order of Surveil class data
baggingprobs_df<- bind_cols(as.data.frame(labeling$adshex), baggingprobs) %>% mutate(adshex = labeling$adshex) %>%
  select(2:4) %>% arrange(desc(surveil)) %>%
  inner_join(features) %>%  select(1:3,squawk_1)

#displaying resulting probabilities and storing
baggingresults <- head(baggingprobs_df, 1000)

head(baggingresults)

#joining the results with planes registraion details through adshex
baggingresults = left_join(baggingresults,registered, by="adshex")

#exporting the resulting data to a csv file
write.csv(baggingresults, "baggingresults.csv", na="")

#comparison of models:

#misclassification rate:
mc <- function(obj) 1 - mean(predict(obj) == training$class)
trees <- list("RF"=rfmodel,"CART"=cartmodel,"evtree" = evtreemodel, 
              "ctree" = ctreemodel, "C4.5"=c45model,"Bagging"=baggingmodel)

round(sapply(trees, function(obj) c("misclassification" = mc(obj))),digits = 3)



