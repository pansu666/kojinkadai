library(tidyverse)
library(dlookr)
rm(list=ls())

#1.train data読み込み
a=read_csv("ti_train.csv", col_names =T, na = c("")) 

#1-2.family size
familysize=a$SibSp+a$Parch+1
a=cbind(a,familysize)

#1-3.name grouping
Name_group=a$Name
Name_group=gsub("^.*, (.*?)\\..*$", "\\1", Name_group)
unique(Name_group)
a <- a %>%
  mutate(Name_group = ifelse(Name_group %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", Name_group),
         Name_group = ifelse(Name_group == "Mme", "Mrs", Name_group),
         Name_group = ifelse(Name_group %in% c("Capt", "Col", "Major", "Dr", "Rev", "Don",
                                               "Sir", "the Countess", "Jonkheer"), "Officer", Name_group),
         Name_group = factor(Name_group))

#1-4.Embarked
table(a$Pclass,a$Embarked)
a[is.na(a$Embarked),]
a$Embarked=replace(a$Embarked,is.na(a$Embarked),"S")
a[is.na(a$Embarked),]

#1-5.factor化
a$Survived=as.factor(a$Survived)
a$Pclass=as.factor(a$Pclass)
a$Sex=as.factor(a$Sex)
a$Embarked=as.factor(a$Embarked)
a$SibSp=replace(a$SibSp,a$SibSp>=1,1)
a$Parch=replace(a$Parch,a$Parch>=1,1)
a$SibSp=as.factor(a$SibSp)
a$Parch=as.factor(a$Parch)

#1-6.Age回帰分析（欠損値の予測）
a1=a%>%filter(!is.na(a$Age))
a2=a%>%filter(is.na(a$Age))
lm_age=lm(Age~Pclass+Sex+Fare+familysize,a)
a.step<- step(lm_age, direction="both")
summary(a.step)
a2_age=round(predict(a.step,newdata = a2),2)
a2$Age=a2_age
a2_age
titanic_train=rbind(a1,a2)
titanic_train=titanic_train[,-c(11,9)]

#1-7.Age grouping
Age_group=case_when(titanic_train$Age<=10.5 ~ "1",
                    titanic_train$Age>10.5 & titanic_train$Age<=20.25 ~ "2",
                    titanic_train$Age>20.25 & titanic_train$Age<=25.37 ~ "3",
                    titanic_train$Age>25.37 & titanic_train$Age<=29.5 ~ "4",
                    titanic_train$Age>29.5 & titanic_train$Age<=32.25 ~ "5",
                    titanic_train$Age>32.25 & titanic_train$Age<=35.69 ~ "6",
                    titanic_train$Age>35.69 & titanic_train$Age<=40.75 ~ "7",
                    titanic_train$Age>40.75 & titanic_train$Age<=46.5 ~ "8",
                    titanic_train$Age>46.5 & titanic_train$Age<=54.5 ~ "9",
                    titanic_train$Age>54.5 & titanic_train$Age<=64.5 ~ "10",
                    titanic_train$Age>64.5 ~ "11")

#1-8. Train data set生成
titanic_train=cbind(titanic_train,Age_group)
titanic_train$Age_group=as.factor(titanic_train$Age_group)
titanic_train=titanic_train[,-6]
str(titanic_train)


#2.test data
b=read_csv("ti_test.csv", col_names =T, na = c("")) 
b$Pclass=as.factor(b$Pclass)
b$Sex=as.factor(b$Sex)
b$Embarked=as.factor(b$Embarked)
sum(is.na(b$Fare))
mean(b$Fare,na.rm=T)
b[153,10]=mean(b$Fare,na.rm = T)
sum(is.na(b$Fare))

familysize1=b$SibSp+b$Parch+1
b=cbind(b,familysize1)
names(b)[13]=c("familysize")
b$SibSp=replace(b$SibSp,b$SibSp>=1,1)
b$Parch=replace(b$Parch,b$Parch>=1,1)
b$SibSp=as.factor(b$SibSp)
b$Parch=as.factor(b$Parch)


#2-1.name grouping
Name_group1=b$Name
Name_group1=gsub("^.*, (.*?)\\..*$", "\\1", Name_group1)
unique(Name_group1)
b <- b %>%
  mutate(Name_group1 = ifelse(Name_group1 %in% c("Mlle", "Ms", "Lady", "Dona"), "Miss", Name_group1),
         Name_group1 = ifelse(Name_group1 == "Mme", "Mrs", Name_group1),
         Name_group1 = ifelse(Name_group1 %in% c("Capt", "Col", "Major", "Dr", "Rev", "Don",
                                                 "Sir", "the Countess", "Jonkheer"), "Officer", Name_group1),
         Name_group1 = factor(Name_group1))
names(b)[14]="Name_group"
str(b)

#2-2.age predict and age group makes
b1=b%>%filter(!is.na(b$Age))
b2=b%>%filter(is.na(b$Age))

b2_age=round(predict(a.step,newdata = b2),2)
b2$Age=b2_age
b2_age
titanic_test=rbind(b1,b2)
titanic_test=titanic_test[,-c(11,9)]
str(titanic_test)

#2-3.Age grouping
Age_group2=case_when(titanic_test$Age<=10.5 ~ "1",
                     titanic_test$Age>10.5 & titanic_test$Age<=20.25 ~ "2",
                     titanic_test$Age>20.25 & titanic_test$Age<=25.37 ~ "3",
                     titanic_test$Age>25.37 & titanic_test$Age<=29.5 ~ "4",
                     titanic_test$Age>29.5 & titanic_test$Age<=32.25 ~ "5",
                     titanic_test$Age>32.25 & titanic_test$Age<=35.69 ~ "6",
                     titanic_test$Age>35.69 & titanic_test$Age<=40.75 ~ "7",
                     titanic_test$Age>40.75 & titanic_test$Age<=46.5 ~ "8",
                     titanic_test$Age>46.5 & titanic_test$Age<=54.5 ~ "9",
                     titanic_test$Age>54.5 & titanic_test$Age<=64.5 ~ "10",
                     titanic_test$Age>64.5 ~ "11")

#2-4. Train data set
titanic_test=cbind(titanic_test,Age_group2)
titanic_test$Age_group2=as.factor(titanic_test$Age_group2)
titanic_test=titanic_test[,-6]
names(titanic_test)[12]=c("Age_group")

#3.最終機械学習データセット
titanic_c=rbind(titanic_train,titanic_test)
titanic_c=titanic_c[,-c(4)]
order(titanic_c$PassengerId)
as.data.frame(titanic_c)
str(titanic_c)
titanic_real_train=titanic_c[1:891,]
titanic_real_test=titanic_c[892:1309,]
str(titanic_real_train)


###############################################################################
#4-1.train_set machine learning, random forest
library(randomForest)
library(caret)

fitc <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
rf_fit1 <- train(Survived ~Pclass+Sex+Fare+familysize+Age_group+Name_group,
                 data = titanic_real_train, method = "rf", ntree=1000,
                 trControl = fitc, verbose = T)
#4-1-2.data predict
rf_fit1 #used test algorithm
test_rf=predict(rf_fit1, newdata =titanic_real_train )
confusionMatrix(test_rf, titanic_real_train$Survived)
rf_real1=predict(rf_fit1, newdata =titanic_real_test )
rf_real1
submission1=cbind(titanic_real_test,rf_real1)
submission1=submission1[,-c(2,3,4,5,6,7,8,9,10,11)]
names(submission1)[2]="Survived"
str(submission1)
write_csv(submission1,"submission4.9_kimpansu.csv")


#4-2.train_set machine learning, logistic regression
fitc2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
glm_fit <- train(Survived ~ Pclass+Sex+Fare+familysize+Age_group+Name_group,
                 data = titanic_real_train, method = "glmboost", 
                 trControl = fitc2, preProc="scale")


#4-2-2. data predict
glm_fit #used test algorithm3
glm_real=predict(glm_fit, newdata =titanic_real_test )
glm_real
submission2=cbind(titanic_real_test,glm_real)
submission2=submission2[,-c(2,3,4,5,6,7,8,9,10,11)]
names(submission2)[2]="Survived"
str(submission2)
write_csv(submission2,"submission4.10_kimpansu.csv")
