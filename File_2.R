setwd(dir = "C:/Users/hess1/OneDrive/Documents/INSEA/S5/AGD/projet")

path2data=file.path("c:","Users","hess1","Onedrive","Documents","INSEA","S5","AGD","projet")
Data = fread(file.path(path2data,"new_Base_CDM_balanced_V2.csv"))
Data <- Data[-1,]

Data$X1 =as.numeric(Data$X1)
Data$X2 =as.numeric(Data$X2)
Data$X3 =as.numeric(Data$X3)
Data$X4 =as.numeric(Data$X4)
Data$X6 =as.numeric(Data$X6)

library(DescTools)

DM = Data[,c(2,3,4,5,6,7,8,1)]

DM$X1 = DescTools::Winsorize(Data$X1, probs = c(0.05, 0.95))
DM$X2 = DescTools::Winsorize(Data$X2, probs = c(0.05, 0.95))
DM$X3 = DescTools::Winsorize(Data$X3, probs = c(0.05, 0.95))
DM$X4 = DescTools::Winsorize(Data$X4, probs = c(0.05, 0.95))
DM$X6 = DescTools::Winsorize(Data$X6, probs = c(0.05, 0.95))

#Numérisation des données
# code X7 en 0 et 1
X7 = c()
for(i in 1:nrow(Data)){
  if(Data$X7[i] == "Feat"){
    X7[i] = 1
  }
  else{
    X7[i] = 0
  }
}

DM$X7 = X7

#transformer Y en 0 et 1
Y = c()
for(i in 1:nrow(Data)){
  if(Data$Y[i] == "Displ"){
    Y[i] = 1
  }
  else{
    Y[i] = 0
  }
}
DM$Y = as.factor(Y)


#test One way anova pour X7 : 

library(stats)
aov_result <- aov(DM$X7 ~DM$Y)
summary(aov_result)

#X3 n'est pas un bon explicatif de Y

#Deviser Data selon X5
library(rpart)
library(rpart.plot)

control = rpart.control(cp = 0)

arbre = rpart(Y~X5 , data = Data , control = control)
rpart.plot(arbre)

#On devise la data en 4 Data mart selon l'arbre
#Data mart 1 : DM1
DM1 = DM[DM$X5=="CASINO" | DM$X5=="ECOMARCHE" | DM$X5=="GEANT" ,]
#Data mart 2 : DM2
DMI1 = DM[DM$X5 !="CASINO" & DM$X5 !="ECOMARCHE" & DM$X5 !="GEANT" ,]
DM2 = DMI1[DMI1$X5 !="CARREFOUR" & DMI1$X5 !="CARREFOUR MARKET" & DMI1$X5 !="HYPER U" 
           & DMI1$X5 !="INTERMARCHE" & DMI1$X5 !="MONOPRIX" & DMI1$X5 !="PRISUNIC",]
#Data mart 3 :DM3
DMI2 = DMI1[DMI1$X5 =="CARREFOUR" | DMI1$X5 =="CARREFOUR MARKET" | DMI1$X5 =="HYPER U" 
           | DMI1$X5 =="INTERMARCHE" | DMI1$X5 =="MONOPRIX" | DMI1$X5 =="PRISUNIC",]

DM3 = DMI2[DMI2$X5 =="CARREFOUR" | DMI2$X5 =="HYPER U" | DMI2$X5 =="INTERMARCHE" |
             DMI2$X5 =="MONOPRIX" | DMI2$X5 =="PRISUNIC" ,]
#Data mart 4 : DM4
DM4 = DMI2[DMI2$X5 == "CARREFOUR MARKET" ,]
####################################################################
# Mouliner les modèles à chaque DM
#DM1 : X5 = CASINO ECOMARCHE GEANT

cont.data_1 = DM1[,-5]

aov_result <- aov(DM1$X1 ~DM1$Y)
summary(aov_result)

aov_result <- aov(DM1$X2 ~DM1$Y)
summary(aov_result)

aov_result <- aov(DM1$X3 ~DM1$Y)
summary(aov_result)

aov_result <- aov(DM1$X4 ~DM1$Y)
summary(aov_result)

aov_result <- aov(DM1$X6 ~DM1$Y)
summary(aov_result)

aov_result <- aov(DM1$X7 ~DM1$Y)
summary(aov_result)
## fonction d'évaluation
calculate_metrics <- function(true_labels, predicted_labels) {
  # Calculate the true positive rate (recall)
  tpr <- sum(true_labels == 1 & predicted_labels == 1) / sum(true_labels == 1)
  
  # Calculate the positive predictive value (precision)
  ppv <- sum(true_labels == 1 & predicted_labels == 1) / sum(predicted_labels == 1)
  
  #f1
  F1 <- 2 * (ppv * tpr) / (ppv + tpr)
  
  #Acuuracy
  acc <- (sum(true_labels == 1 & predicted_labels == 1) + sum(true_labels == 0 & predicted_labels == 0))/length(true_labels)
  
  # Return the metrics
  metrics = c(recall = tpr, precision = ppv , f1_score = F1 , Accuracy = acc)
  return(metrics)
}

#correlation entre les variables X
library(Hmisc)
DM_Matrix <- as.matrix(cont.data_1[,-7])
rcorr(DM_Matrix , type=c("pearson","spearman"))
#il ya une forte colinéarité entre les variables
set.seed(222)
#pour construire le modèles de regression logistique
# PCA
library(FactoMineR)
PCA.res= PCA(cont.data_1[,-7],
         ncp = Inf,
         scale.unit = TRUE,  # True for normalistion
         graph = FALSE,
         axes = c(1,2)
)

#on prend 3 PCA

DM.PCA = res[,c(8,9,10,7)]

aov_result <- aov(DM.PCA$Dim.1 ~DM.PCA$Y)
summary(aov_result)

aov_result <- aov(DM.PCA$Dim.2 ~DM.PCA$Y)
summary(aov_result)

aov_result <- aov(DM.PCA$Dim.3 ~DM.PCA$Y)
summary(aov_result)

ggplot(DM.PCA, mapping = aes(x = DM.PCA$Dim.1, y = DM.PCA$Y, fill = as.factor(DM.PCA$Y))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 
ggplot(DM.PCA, mapping = aes(x = DM.PCA$Dim.2, y = DM.PCA$Y, fill = as.factor(DM.PCA$Y))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 
ggplot(DM.PCA, mapping = aes(x = DM.PCA$Dim.3, y = DM.PCA$Y, fill = as.factor(DM.PCA$Y))) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 

#logistic regression pour tester la significativité des PCA
logistic_model_test <- glm(Y ~ Dim.1 +Dim.2 + Dim.3, 
                      data = train_set, 
                      family = "binomial")
summary(logistic_model)

#On prend soit 2 composantes
DM.PCA_1 = DM.PCA[,c(1,2,4)]

library(stats)
library(caret)
split_index_pca = createDataPartition(y = DM.PCA_1$Y, p = 0.75, list = FALSE)
train_set_pca = DM.PCA_1[split_index_pca,]
test_set_pca = DM.PCA_1[-split_index_pca,]

library(caTools)
library(ROCR)

#logistic regression
logistic_model <- glm(Y ~ Dim.1 +Dim.2, 
                      data = train_set_pca, 
                      family = "binomial")

summary(logistic_model)

#Predictions 

PCA.prediction1 = predict(logistic_model , newdata = test_set_pca[,1:2])
PCA.prediction1 = as.numeric(PCA.prediction1>0.5)
PCA.prediction1 = as.factor(PCA.prediction1)
#Decision tree
split_index = createDataPartition(y = cont.data_1$Y, p = 0.75, list = FALSE)
train_set = cont.data_1[split_index,]
test_set = cont.data_1[-split_index,]

tree_model <- rpart(Y ~ ., data = train_set, method = "class")
# Make predictions on the testing set using the tree model
arbre.prediction1 <- predict(tree_model,
          newdata = test_set[,-7], type = "class")
# Convert predictions and target to factors
arbre.prediction1 <- as.factor(arbre.prediction1)

#Random forest
library(randomForest)
forest_model <- randomForest(x = train_set[,1:6], 
                             y = train_set$Y, ntree = 500,
     
                                                     regression = TRUE)
forest.pred <- predict(forest_model, newdata = test_set[,1:6])

forest.pred <- as.factor(forest.pred)
#XGB
library(xgboost)
dtrain <- xgb.DMatrix(data = as.matrix(train_set[,-7]), label= as.matrix(train_set[,7]))
dtest <- xgb.DMatrix(data = as.matrix(test_set[,-7]), label= as.matrix(test_set[,7]))

xgb.model <- xgboost(data = dtrain, # the data   
                 nround = 500, # max number of boosting iterations
                 objective = "binary:logistic")  # the objective function

xgb.pred <- predict(xgb.model, dtest)
xgb.pred <-  as.numeric(xgb.pred>0.5)
#svm
svm.model <- svm(Y~. , data = train_set_pca)
svm.pred1 = predict(svm.model , newdata = test_set_pca[,1:3])

#evaluate
calculate_metrics(test_set_pca$Y , PCA.prediction1)
calculate_metrics(test_set$Y , arbre.prediction1)
calculate_metrics(test_set$Y , forest.pred)
calculate_metrics(test_set$Y , xgb.pred)
calculate_metrics(test_set_pca$Y , svm.pred1)


#############################################################################"
#DM3 : CARREFOUR INTERMARCHE MONOPRIX PRISUNIC HYPER U
cont.data_2 = DM3[,-5]

aov_result <- aov(cont.data_2$X1 ~cont.data_2$Y)
summary(aov_result)

aov_result <- aov(cont.data_2$X2 ~cont.data_2$Y)
summary(aov_result)

aov_result <- aov(cont.data_2$X3 ~cont.data_2$Y)
summary(aov_result)

aov_result <- aov(cont.data_2$X4 ~cont.data_2$Y)
summary(aov_result)

aov_result <- aov(cont.data_2$X6 ~cont.data_2$Y)
summary(aov_result)

aov_result <- aov(cont.data_2$X7 ~cont.data_2$Y)
summary(aov_result)

#split
split_index = createDataPartition(y = cont.data_2$Y, p = 0.8, list = FALSE)
train_set = cont.data_2[split_index,]
test_set = cont.data_2[-split_index,]

#PCA
PCA.res= PCA(cont.data_2[,-7],
             ncp = Inf,
             scale.unit = TRUE,  # True for normalistion
             graph = FALSE,
             axes = c(1,2)
)

res <- cont.data_2 %>% cbind(PCA.res$ind$coord)
DM_Matrix <- as.matrix(res)
rcorr(DM_Matrix , type=c("pearson","spearman"))

#3 composantes
#logistic regression
set.seed(123)
DM.PCA_2 = res[,c(8,9,10,7)]
split_index2 = createDataPartition(y = DM.PCA_2$Y, p = 0.8, list = FALSE)
train.PCA = DM.PCA_2[split_index2,]
test.PCA = DM.PCA_2[-split_index2,]

logistic_model <- glm(Y ~ Dim.1 +Dim.2 + Dim.3, 
                      data = train.PCA, 
                      family = "binomial")
Pca.prediction2 = predict(logistic_model , newdata = test.PCA[,1:3])
Pca.prediction2 = as.numeric(Pca.prediction2>0.5)

#Decision Tree
tree_model <- rpart(Y ~ ., data = train_set, method = "class")
arbre.pred2 <- predict(tree_model,
                       newdata = test_set[,-7], type = "class")
arbre.pred2 <- as.factor(arbre.pred2)
#Random Forest
forest_model <- randomForest(x = train_set[,1:6], 
                             y = train_set$Y, ntree = 500,regression = TRUE)

forest.pred2 <- predict(forest_model, newdata = test_set[,1:6])
forest.pred2 <- as.factor(forest.pred2)
#XGboost
dtrain <- xgb.DMatrix(data = as.matrix(train_set[,-7]), label= as.matrix(train_set[,7]))
dtest <- xgb.DMatrix(data = as.matrix(test_set[,-7]), label= as.matrix(test_set[,7]))

xgb.model <- xgboost(data = dtrain, # the data   
                     nround = 500, # max number of boosting iterations
                     objective = "binary:logistic")  # the objective function

xgb.pred2 <- predict(xgb.model, dtest)
xgb.pred2 <-  as.numeric(xgb.pred2>0.5)
#SVM
library(e1071)
svm.model <- svm(Y~. , data = train.PCA)
svm.pred2 = predict(svm.model , newdata = test.PCA[,1:3])
#evaluate

calculate_metrics( test.PCA$Y,Pca.prediction1)
calculate_metrics(test_set$Y , arbre.pred2)
calculate_metrics(test_set$Y , forest.pred2)
calculate_metrics(test_set$Y , xgb.pred2)
calculate_metrics( test.PCA$Y,svm.pred2)

##########################################################################
#DM4 : Carrefour Market

cont.data_3 = DM4[,-5]

aov_result <- aov(cont.data_3$X1 ~cont.data_3$Y)
summary(aov_result)

aov_result <- aov(cont.data_3$X2 ~cont.data_3$Y)
summary(aov_result)

aov_result <- aov(cont.data_3$X3 ~cont.data_3$Y)
summary(aov_result)

aov_result <- aov(cont.data_3$X4 ~cont.data_3$Y)
summary(aov_result)

aov_result <- aov(cont.data_3$X6 ~cont.data_3$Y)
summary(aov_result)

aov_result <- aov(cont.data_3$X7 ~cont.data_3$Y)
summary(aov_result)

#PCA
PCA.res= PCA(cont.data_3[,-7],
             ncp = Inf,
             scale.unit = TRUE,  # True for normalistion
             graph = FALSE,
             axes = c(1,2)
)

res <- cont.data_3 %>% cbind(PCA.res$ind$coord)
DM_Matrix <- as.matrix(res)
rcorr(DM_Matrix , type=c("pearson","spearman"))

#logistic regression
set.seed(123)
DM.PCA_3 = res[,c(8,9,10,7)]
split_index3 = createDataPartition(y = DM.PCA_3$Y, p = 0.8, list = FALSE)
train.PCA = DM.PCA_3[split_index3,]
test.PCA = DM.PCA_3[-split_index3,]

logistic_model <- glm(Y ~ Dim.1 +Dim.2 + Dim.3, 
                      data = train.PCA, 
                      family = "binomial")
Pca.prediction3 = predict(logistic_model , newdata = test.PCA[,1:3])
Pca.prediction3 = as.numeric(Pca.prediction3>0.5)

#SVM
library(e1071)
svm.model <- svm(Y~. , data = train.PCA)
svm.pred3 = predict(svm.model , newdata = test.PCA[,1:3])

# train test split
split_index = createDataPartition(y = cont.data_3$Y, p = 0.7, list = FALSE)
train_set = cont.data_3[split_index,]
test_set = cont.data_3[-split_index,]

#Decision Tree
tree_model <- rpart(Y ~ ., data = train_set, method = "class")
arbre.pred3 <- predict(tree_model,
                        newdata = test_set[,-7], type = "class")
arbre.pred3 <- as.factor(arbre.pred3)
#Random Forest
forest_model <- randomForest(x = train_set[,1:6], 
                             y = train_set$Y, ntree = 500,regression = TRUE)

forest.pred3 <- predict(forest_model, newdata = test_set[,1:6])
forest.pred3 <- as.factor(forest.pred3)
#XGboost
dtrain <- xgb.DMatrix(data = as.matrix(train_set[,-7]), label= as.matrix(train_set[,7]))
dtest <- xgb.DMatrix(data = as.matrix(test_set[,-7]), label= as.matrix(test_set[,7]))

xgb.model <- xgboost(data = dtrain, # the data   
                     nround = 500, # max number of boosting iterations
                     objective = "binary:logistic")  # the objective function

xgb.pred3 <- predict(xgb.model, dtest)
xgb.pred3 <-  as.numeric(xgb.pred3>0.5)
#evaluate
calculate_metrics(test.PCA$Y,Pca.prediction3)
calculate_metrics(test_set$Y , arbre.pred3)
calculate_metrics(test_set$Y , forest.pred3)
calculate_metrics(test_set$Y , xgb.pred3)
calculate_metrics( test.PCA$Y,svm.pred3)

#####################################################################
# DM2 : 
chisq.test(DM2$X5 , DM2$Y)
cont.data_4 = DM2[,-5]

aov_result <- aov(cont.data_4$X1 ~cont.data_4$Y)
summary(aov_result)

aov_result <- aov(cont.data_4$X2 ~cont.data_4$Y)
summary(aov_result)

aov_result <- aov(cont.data_4$X3 ~cont.data_4$Y)
summary(aov_result)

aov_result <- aov(cont.data_4$X4 ~cont.data_4$Y)
summary(aov_result)

aov_result <- aov(cont.data_4$X6 ~cont.data_4$Y)
summary(aov_result)

aov_result <- aov(cont.data_4$X7 ~cont.data_4$Y)
summary(aov_result)


#PCA
PCA.res= PCA(cont.data_4[,-7],
             ncp = Inf,
             scale.unit = TRUE,  # True for normalistion
             graph = FALSE,
             axes = c(1,2)
)

res <- cont.data_4 %>% cbind(PCA.res$ind$coord)
DM_Matrix <- as.matrix(res)
rcorr(DM_Matrix , type=c("pearson","spearman"))

#logistic regression
library(caret)
set.seed(123)
DM.PCA_4 = res[,c(8,9,10,7)]
split_index4 = createDataPartition(y = DM.PCA_4$Y, p = 0.8, list = FALSE)
train.PCA = DM.PCA_4[split_index4,]
test.PCA = DM.PCA_4[-split_index4,]

logistic_model <- glm(Y ~ Dim.1 +Dim.2 , 
                      data = train.PCA, 
                      family = "binomial")
PCA.pred4 = predict(logistic_model , newdata = test.PCA[,1:3] , type = "response")
PCA.pred4 = as.numeric(PCA.pred4>0.5)

#SVM
library(e1071)
svm.model <- svm(Y~. , data = train.PCA)
svm.pred4 = predict(svm.model , newdata = test.PCA[,1:3])

# train test split
split_index = createDataPartition(y = cont.data_4$Y, p = 0.8, list = FALSE)
train_set = cont.data_4[split_index,]
test_set = cont.data_4[-split_index,]

#Decision Tree
tree_model <- rpart(Y ~ ., data = train_set, method = "class")
arbre.pred4 <- predict(tree_model,
                        newdata = test_set[,-7], type = "class")
arbre.pred4 <- as.factor(arbre.pred4)
#Random Forest
forest_model <- randomForest(x = train_set[,1:6], 
                             y = train_set$Y, ntree = 500,regression = TRUE)

forest.pred4 <- predict(forest_model, newdata = test_set[,1:6])
forest.pred4 <- as.factor(forest.pred4)
#XGboost
dtrain <- xgb.DMatrix(data = as.matrix(train_set[,-7]), label= as.matrix(train_set[,7]))
dtest <- xgb.DMatrix(data = as.matrix(test_set[,-7]), label= as.matrix(test_set[,7]))

xgb.model <- xgboost(data = dtrain, # the data   
                     nround = 500, # max number of boosting iterations
                     objective = "binary:logistic")  # the objective function

xgb.pred4 <- predict(xgb.model, dtest)
xgb.pred4 <-  as.numeric(xgb.pred4>0.5)


#evaluate
calculate_metrics(test.PCA$Y,PCA.pred4)
calculate_metrics(test_set$Y , arbre.pred4)
calculate_metrics(test_set$Y , forest.pred4)
calculate_metrics(test_set$Y , xgb.pred4)
calculate_metrics( test.PCA$Y,svm.pred4)