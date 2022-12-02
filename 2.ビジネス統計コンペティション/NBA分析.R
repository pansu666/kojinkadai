library(dplyr)
library(caret)
library(MASS)
#data load
a1=read.csv("rookie1.csv")
a2=read.csv("rookie.csv")

#主成分分析
a1.pca <- princomp(a1,cor=T)
summary(a1.pca)
plot(a1.pca,type="l")	
biplot(a1.pca)
a1.pca$loadings
a1.pca$scores
#c1=balance, c2=3point , c3=assist
a1.score=a1.pca$scores[,1:3]
a1.score
a3=cbind(a1.score,a2$target)
a3=as.data.frame(a3)
names(a3)=c("balance","3point","Assist","target")
a3$target=as.factor(a3$target)
screeplot(a1.pca)
str(a3)
View(a3)
qgraph(cor(a1), edge.labels = T, minimum = .2, edge.color = "black")

#logistic regression
a.glm=glm(target~.,data=a3, family = binomial("logit"))
summary(a.glm)
a.step<- step(a.glm, direction="both")
summary(a.step)

probability=predict(a.step,newdata=a3,type="response")
PREDICTED_C = ifelse(probability > 0.5 , 1 , 0)
PREDICTED_C = as.factor(PREDICTED_C)
confusionMatrix(a3$target,PREDICTED_C)

#オッズ比table
ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result
}
ORtable(a.step)

#lda判別分析
a.lda <- lda(target ~ ., data = a3)	
a.lda
apply(a.lda$means%*%a.lda$scaling,2,mean)
probability1=predict(a.lda, newdata=a3)
PREDICTED_C = ifelse(probability1$posterior > 0.5 , 1 , 0)
PREDICTED_C = as.factor(PREDICTED_C)
confusionMatrix(a3$target,predict(a.lda, newdata=a3)$class)

######################################################################


#探索的因子分析
library( psych )
library( GPArotation )
library(tidyverse)
library(readsdr)
a3=read.csv("rookie1.csv") 
a2=read.csv("rookie.csv")
cortest.bartlett(R=cor(a3),n=nrow(a3))
a3  %>% cor(use = "complete.obs") %>% eigen()
a3  %>% VSS.scree()
a3  %>% fa.parallel(fm="ml", fa="both", n.iter = 100)
a3  %>% vss(use="pairwise")
a3  %>% fa(nfactors = 3, fm = "ml",rotate = "promax") %>% print(sort=TRUE,digits = 3) 
a3  %>% fa(nfactors = 3, fm = "ml",rotate = "promax") %>% fa.diagram() 
#.5 up

a3<-mutate(a3, balance=(FGM+PTS+FGA+FTM+FTA+TOV+MIN+AST+STL+EFF) / 10)
a3<-mutate(a3, def=(REB + DREB + OREB + BLK) / 4)
a3<-mutate(a3, shoot=(X3P.Made + X3PA+ X3P.) / 3)
a4=cbind(a3,a2$target)
a4=as.data.frame(a4)
names(a4)[24]=c("target")
a4$target=as.factor(a4$target)
a5=subset(a4,select = c(balance,def,shoot,target))
View(a5)


#logistic regression
a.glm=glm(target~balance+def+shoot,data=a5, family = binomial)
summary(a.glm)
a.step<- step(a.glm, direction="backward")
summary(a.step)

probability=predict(a.step,newdata=a5,type="response")
PREDICTED_C = ifelse(probability > 0.5 , 1 , 0)
PREDICTED_C = as.factor(PREDICTED_C)
confusionMatrix(a5$target,PREDICTED_C)

#オッズ比table
ORtable=function(x,digits=2){
  suppressMessages(a<-confint(x))
  result=data.frame(exp(coef(x)),exp(a))
  result=round(result,digits)
  result=cbind(result,round(summary(x)$coefficient[,4],3))
  colnames(result)=c("OR","2.5%","97.5%","p")
  result}
ORtable(a.step)

#lda判別分析
a.lda <- lda(target ~ ., data = a5)	
a.lda
apply(a.lda$means%*%a.lda$scaling,2,mean)
probability1=predict(a.lda, newdata=a5)
PREDICTED_C = ifelse(probability1$posterior > 0.5 , 1 , 0)
PREDICTED_C = as.factor(PREDICTED_C)
confusionMatrix(a5$target,predict(a.lda, newdata=a5)$class)
