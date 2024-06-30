library(data.table)
library(dplyr)
library(FactoMineR)
library(ggplot2)
library(factoextra)
library(plotly)


setwd(dir = "C:/Users/hess1/OneDrive/Documents/INSEA/S5/AGD/projet")

#lire la base des données
path2data=file.path("c:","Users","hess1","Onedrive","Documents","INSEA","S5","AGD","projet")
Data = fread(file.path(path2data,"new_Base_CDM_balanced_V2.csv"))
Data <- Data[-1,]

#analyse univarié
#estimation de la distribution des variables quantitatives

x1 =as.numeric(Data$X1)
x2 =as.numeric(Data$X2)
x3 =as.numeric(Data$X3)
x4 =as.numeric(Data$X4)
x6 =as.numeric(Data$X6)

plot(density(x1),main="Density estimate of x1" , xlim = c(0,100))
plot(density(x2),main="Density estimate of x2" , xlim = c(0,2000))
plot(density(x6),main="Density estimate of x6" , xlim = c(0,5000) )

#X1 X2 X6 ont presque la meme distribution avec une différence au niveau des paramétres

plot(density(x3),main="Density estimate of x3")

plot(density(x4),main="Density estimate of x4")

#boxplot des X1 X2 X3 X4 X6 

boxplot(x1)
summary(x1)
boxplot(x2)
summary(x2)
boxplot(x3)
summary(x3)
boxplot(x4)
summary(x4)
boxplot(x6)
summary(x6)

#Correction des variables quantitatives
library(DescTools)

x1.corr = DescTools::Winsorize(x1, probs = c(0.05, 0.95))
x2.corr = DescTools::Winsorize(x2, probs = c(0.05, 0.95))
x3.corr = DescTools::Winsorize(x3, probs = c(0.05, 0.95))
x4.corr = DescTools::Winsorize(x4, probs = c(0.05, 0.95))
x6.corr = DescTools::Winsorize(x6, probs = c(0.05, 0.95))

#Variables qualitatives
#X5
contingency_table_X5 <- table(Data$X5)
frequencies_X5 <- table(Data$X5)
percentages_X5 <- prop.table(frequencies_X5)
frequencies_X5 <- sort(frequencies_X5, decreasing = TRUE)
barplot(frequencies_X5)

#X7
contingency_table_X7 <- table(Data$X7)
frequencies_X7 <- table(Data$X7)
percentages_X7 <- prop.table(frequencies_X7)
frequencies_X7 <- sort(frequencies_X7, decreasing = TRUE)
barplot(frequencies_X7)

#Y
contingency_table_Y <- table(Data$Y)
frequencies_Y <- table(Data$Y)
percentages_Y <- prop.table(frequencies_Y)
frequencies_Y <- sort(frequencies_Y, decreasing = TRUE)
barplot(frequencies_Y)

#rassembler les nouvelles variables corrigés

DM_1 = data.frame(cbind(x1.corr,x2.corr,x3.corr,x4.corr,Data$X5,x6.corr,Data$X7,Data$Y))
colnames(DM_1) = c("X1.corr","X2.corr","X3.corr","X4.corr","X5","X6.corr","X7","Y")

DM_1$X1.corr = as.numeric(DM_1$X1.corr)
DM_1$X2.corr = as.numeric(DM_1$X2.corr)
DM_1$X3.corr = as.numeric(DM_1$X3.corr)
DM_1$X4.corr = as.numeric(DM_1$X4.corr)
DM_1$X6.corr = as.numeric(DM_1$X6.corr)


#analyse multivarié
#Variables quantitatives
library(Hmisc)
cont.data = data.frame(subset(DM_1,select = -c(X5,X7)))
DM_Matrix <- as.matrix(cont.data[,1:5])
rcorr(DM_Matrix , type=c("pearson","spearman"))

#variables qualitatives
chisq.test(Data$X5 , Data$X7)
#dependance entre X5 QUANTITATIVES ET X5
aov_result <- aov(cont.data$X1.corr ~ Data$X5)
summary(aov_result)
aov_result <- aov(cont.data$X2.corr ~ Data$X5)
summary(aov_result)
aov_result <- aov(cont.data$X3.corr ~ Data$X5)
summary(aov_result)
aov_result <- aov(cont.data$X4.corr ~ Data$X5)
summary(aov_result)
aov_result <- aov(cont.data$X6.corr ~ Data$X5)
summary(aov_result)

#dependance entre X7 QUANTITATIVES ET X7
aov_result <- aov(cont.data$X1.corr ~ Data$X7)
summary(aov_result)
aov_result <- aov(cont.data$X2.corr ~ Data$X7)
summary(aov_result)
aov_result <- aov(cont.data$X3.corr ~ Data$X7)
summary(aov_result)
aov_result <- aov(cont.data$X4.corr ~ Data$X7)
summary(aov_result)
aov_result <- aov(cont.data$X6.corr ~ Data$X7)
summary(aov_result)
#Dependance avec Y
#X5 et X7 
chisq.test(Data$X5 , Data$Y)
chisq.test(Data$X7 , Data$Y)
#X1 X2 X3 X4 X6
aov_result <- aov(cont.data$X1.corr ~ Data$Y)
summary(aov_result)
aov_result <- aov(cont.data$X2.corr ~ Data$Y)
summary(aov_result)
aov_result <- aov(cont.data$X3.corr ~ Data$Y)
summary(aov_result)
aov_result <- aov(cont.data$X4.corr ~ Data$Y)
summary(aov_result)
aov_result <- aov(cont.data$X6.corr ~ Data$Y)
summary(aov_result)


#boxplot of Chaque variable quantitatives en fonction Y
ggplot(Data, mapping = aes(x = as.numeric(DM_1$X1.corr), y = DM_1$Y, fill = DM_1$Y)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 

ggplot(Data, mapping = aes(x = as.numeric(DM_1$X2.corr), y = DM_1$Y, fill = DM_1$Y)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 

ggplot(Data, mapping = aes(x = as.numeric(DM_1$X3.corr), y = DM_1$Y, fill = DM_1$Y)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 

ggplot(Data, mapping = aes(x = as.numeric(DM_1$X4.corr), y = DM_1$Y, fill = DM_1$Y)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 

ggplot(Data, mapping = aes(x = as.numeric(DM_1$X6.corr), y = DM_1$Y, fill = DM_1$Y)) + 
  geom_boxplot(outlier.colour = "red", outlier.shape = 5, outlier.size = 4) 

#X3 et X4 ne sont pas bien significatifs pour expliquer Y

#transformer tous les variables en categorielles

#Discretize by MDLPC supervisé par Y
library(arc)

mdlp2.res = mdlp2(cont.data,cl_index = 6) # 6 : indice de Y 
DM_categ = cbind(mdlp2.res$Disc.data,DM_1$X5,DM_1$X7)

DM_categ$X1_cat = as.factor(DM_categ$X1_cat)
DM_categ$X2_cat = as.factor(DM_categ$X2_cat)
DM_categ$X3_cat = as.factor(DM_categ$X3_cat)
DM_categ$X4_cat = as.factor(DM_categ$X4_cat)
DM_categ$X6_cat = as.factor(DM_categ$X6_cat)

DM_categ = DM_categ[,-c(1:5)]
colnames(DM_categ)[2:3] = c("X5","X7")

#Analyse des nouvelles variables
#X1_Cat
contingency_table_X1 <- table(DM_categ$X1_cat)
frequencies_X1 <- table(DM_categ$X1_cat)
frequencies_X1 <- sort(frequencies_X1, decreasing = TRUE)
barplot(frequencies_X1)
#X2_Cat
contingency_table_X2 <- table(DM_categ$X2_cat)
frequencies_X2 <- table(DM_categ$X2_cat)
frequencies_X2 <- sort(frequencies_X2, decreasing = TRUE)
barplot(frequencies_X2)
#X3_Cat
contingency_table_X3 <- table(DM_categ$X3_cat)
frequencies_X3 <- table(DM_categ$X3_cat)
frequencies_X3 <- sort(frequencies_X3, decreasing = TRUE)
barplot(frequencies_X3)
#X4_Cat
contingency_table_X4 <- table(DM_categ$X4_cat)
frequencies_X4 <- table(DM_categ$X4_cat)
frequencies_X4 <- sort(frequencies_X4, decreasing = TRUE)
barplot(frequencies_X4)
#X6_categ
contingency_table_X6 <- table(DM_categ$X6_cat)
frequencies_X6 <- table(DM_categ$X6_cat)
frequencies_X6 <- sort(frequencies_X6, decreasing = TRUE)
barplot(frequencies_X6)

#tableau de contingence et test chi 2
# X1 CAT
ta =  table(DM_categ[,c(1,3)])
chisq.test(ta)

# X2 CAT
ta =  table(DM_categ[,c(1,4)])

chisq.test(ta)
# X3 CAT
ta =  table(DM_categ[,c(1,5)])

chisq.test(ta)
# X4 CAT
ta =  table(DM_categ[,c(1,6)])

chisq.test(ta)

# X6 CAT
ta =  table(DM_categ[,c(1,7)])

chisq.test(ta)


#train test split :
#train = 75% 
library(caret)
split_index = createDataPartition(y = DM_categ$Y, p = 0.75, list = FALSE)
train = DM_categ[split_index,]
test = DM_categ[-split_index,]

X_test = test[,2:8]
Y_test = test[,1]
#code the Y on 0 et 1 
Y = c()
for(i in 1:nrow(train)){
  if(train$Y[i] == "Displ"){
    Y[i] = 1
  }
  else{
    Y[i] = 0
  }
}
train$Y = as.factor(Y)

Yt = c()
for(i in 1:nrow(test)){
  if(test$Y[i] == "Displ"){
    Yt[i] = 1
  }
  else{
    Yt[i] = 0
  }
}
test$Y = as.factor(Yt)


#tester les modèles 
#decision tree

library(rpart)

# Perform k-fold cross-validation
k <- 10
results <- trainControl(method = "cv", number = k)
cv <- train(Y ~ ., data = train, method = "rpart", trControl = results)

# Print the results
print(cv)
#utiliser la meilleur valeur de hyperparametre cp
arbre.model <- rpart(Y~., data=train, method="class" , control = rpart.control(cp = 0.02451236))
arbre.pred <- predict(arbre.model, X_test, type="class")
#random forest
library(randomForest)

# CV to find best ntree of Random Forest
cv <- train(Y ~ ., data = train, method = "rf", trControl = results)

#Fit the model
forest.model <- randomForest(Y~. , data = train , ntree = 450)

forest.pred <- predict(forest.model, newdata = test[,2:8])
forest.pred = as.factor(forest.pred)
#naive bayes
library(e1071)

NB.model <- naiveBayes(Y ~ ., data = train)
NB.pred = predict(NB.model, test[2:8])


#Evaluation des modeles

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

calculate_metrics(test$Y , arbre.pred)
calculate_metrics(test$Y , forest.pred)
calculate_metrics(test$Y , NB.pred)
