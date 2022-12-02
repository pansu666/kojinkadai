library(tidyverse)
library(psych)
library(caret)
setwd("C:/r_analysis/3.20221120/")
getwd()
rm(list=ls())
all=read_csv("allfdataoralpha.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
all=all[,-1]
all=all %>% filter(date!=2021&date!=2020) #年度指定
#test start
#コミットメントに対するalpha
CM1=all %>% select(CM01_01:CM01_07) %>%  as.data.frame()
CM2=all %>% select(CM02_01:CM02_06,CM02_08)%>% as.data.frame()
CM3=all %>% select(CM03_01:CM03_05,CM03_07,CM03_08)%>% as.data.frame()

aff=alpha(round(CM1*10),max=100,check.keys=TRUE)$total
nor=alpha(round(CM3*10),max=100,check.keys=TRUE)$total
con=alpha(round(CM2*10),max=100,check.keys=TRUE)$total

name=c("Affective","Normative","continuance")
aa=rbind(aff,nor,con) %>%as.data.frame() %>% select(raw_alpha,mean,sd)
aa=cbind(name,aa)
write_excel_csv(aa, "./9.output/1.commit_alpha15-19.csv")

#組織文化に対する alpha
OCI1_1=all %>%  select(OCI_H_1:OCI_H_10)%>% as.data.frame()
OCI1_2=all %>%  select(OCI_AF_1:OCI_AF_10)%>% as.data.frame()
OCI1_3=all %>%  select(OCI_AC_1:OCI_AC_10)%>% as.data.frame()
OCI1_4=all %>%  select(OCI_SA_1:OCI_SA_9)%>% as.data.frame()

OCI1_1_alpha=alpha(round(OCI1_1*10),max=100,check.keys=TRUE)$total
OCI1_2_alpha=alpha(round(OCI1_2*10),max=100,check.keys=TRUE)$total
OCI1_3_alpha=alpha(round(OCI1_3*10),max=100,check.keys=TRUE)$total
OCI1_4_alpha=alpha(round(OCI1_4*10),max=100,check.keys=TRUE)$total

Entrepreneurial=all %>%  select(DP01_02,DP02_02,DP03_02,DP04_02)%>% as.data.frame()
Entrepreneurial_alpha=alpha(round(Entrepreneurial),max=100,check.keys=TRUE)$total

name=c("Humanistic","Affiliative","Achievement","Self-actualization")
aa=rbind(OCI1_1_alpha,OCI1_2_alpha,OCI1_3_alpha,OCI1_4_alpha) %>%
  as.data.frame() %>% select(raw_alpha,mean,sd)
aa=cbind(name,aa)
write_excel_csv(aa, "./9.output/1.OCI_alpha15-19.csv")

name=c("Entrepreneurial")
aa=rbind(Entrepreneurial_alpha) %>%
  as.data.frame() %>% select(raw_alpha,mean,sd)
aa=cbind(name,aa)
write_excel_csv(aa, "./9.output/1.entre15-19.csv")


#alpha計算シミュレーション1万回

list=seq(1,10000,1)
alpha_list=data.frame()
all=read_csv("allfdataoralpha1116.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
all=all[,-1]
all=all %>% filter(name!="b0175")
set.seed(5882)

for (i in list){
  idx = createDataPartition(all$sex, list=F, p=0.01)
  all1=all[ idx,]
  CM1=all1 %>% select(CM1_1_rk:CM1_7_rk) %>%  as.data.frame()
  CM2=all1 %>% select(CM2_1_rk:CM2_6_rk,CM2_8_rk)%>% as.data.frame()
  CM3=all1 %>% select(CM3_1_rk:CM3_5_rk,CM3_7_rk,CM3_8_rk)%>% as.data.frame()
  OCI1_1=all1 %>%  select(OCI_H_1_rk:OCI_H_10_rk)%>% as.data.frame()
  OCI1_2=all1 %>%  select(OCI_AF_1_rk:OCI_AF_10_rk)%>% as.data.frame()
  OCI1_3=all1 %>%  select(OCI_AC_1_rk:OCI_AC_10_rk)%>% as.data.frame()
  OCI1_4=all1 %>%  select(OCI_SA_1_rk:OCI_SA_9_rk)%>% as.data.frame()
  aff=alpha(CM1,check.keys=TRUE)$total$raw_alpha
  nor=alpha(CM3,check.keys=TRUE)$total$raw_alpha
  con=alpha(CM2,check.keys=TRUE)$total$raw_alpha
  humanistic=alpha(OCI1_1,check.keys=TRUE)$total$raw_alpha
  affiliavive=alpha(OCI1_2,check.keys=TRUE)$total$raw_alpha
  achievement=alpha(OCI1_3,check.keys=TRUE)$total$raw_alpha
  self_actualizing=alpha(OCI1_4,check.keys=TRUE)$total$raw_alpha
  
  b=cbind(aff,nor,con,humanistic,affiliavive,achievement,self_actualizing)
  alpha_list=rbind(alpha_list,b)
}
summary(alpha_list)

#save data
write_excel_csv(alpha_list, "./9.output/alpha_simulation.csv")





#級内相関係数

library(misty)
rm(list=ls())
seicho=read_csv("allfdataoralpha.csv",col_names =T, na = c(""),locale = locale(encoding = "CP932"))
seicho=seicho[,-1]
seicho1=seicho
seicho$name=as.factor(seicho$name)
seicho$date=as.factor(seicho$date)
seicho$group=as.factor(seicho$group)

#ICC1計算式定義
icc1 <- function(X, id){ 
  aov1 <- aov(X ~ factor(id))
  v.btwn <- var(coef(aov1))
  v.wthn <- sum(resid(aov1)^2)/aov1$df.residual
  return((v.btwn)/( v.btwn + v.wthn))
} 

#ICC1計算
t=data.frame()
seicho1=seicho
seicho1=seicho %>% filter(date != 2020 & date != 2021 )　#年度指定
Affective=icc1(seicho1$AFFECTIVE,seicho1$name)
Normative=icc1(seicho1$NORMATIVE,seicho1$name)
Continuance=icc1(seicho1$CONTINUANCE,seicho1$name)
Constructive=icc1(seicho1$OCI_CONSTRUCTIVE,seicho1$name)
Humanistic=icc1(seicho1$OCI_humanistic,seicho1$name)
Affiliavtive=icc1(seicho1$OCI_affiliative,seicho1$name)
Achievement=icc1(seicho1$OCI_achievement,seicho1$name)
Self_actualization=icc1(seicho1$OCI_self,seicho1$name)
Entrepreneurial=icc1(seicho1$OCI_self,seicho1$name)
ICC1=rbind(Affective,Normative,Continuance,Humanistic,
           Affiliavtive,Achievement,Self_actualization,Entrepreneurial)
colnames(ICC1)=c("2015-2019")
summary(ICC1)
t=rbind(t,ICC1)

#save data
write.csv(t,"./ICC1.csv", fileEncoding = "cp932")


#ICC2計算

seicho8=seicho %>% filter(date !=2020 & date!=2021)
a8=seicho8 %>% select(AFFECTIVE,NORMATIVE,CONTINUANCE,OCI_CONSTRUCTIVE,OCI_humanistic,
                      OCI_affiliative,OCI_achievement,OCI_self,DP_ENTREPRENEURIAL) %>%  as.data.frame()


seicho9=seicho %>% filter(date ==2020 | date==2021 &name!="b0175")
a9=seicho9 %>% select(AFFECTIVE,NORMATIVE,CONTINUANCE,OCI_CONSTRUCTIVE,OCI_humanistic,
                      OCI_affiliative,OCI_achievement,OCI_self,DP_ENTREPRENEURIAL) %>%  as.data.frame()
library(multilevel)

icc2_1519=multilevel.icc(a8,seicho8$name,type=2)
icc2_1519=as.data.frame(icc2_1519)
icc2_2021=mult.icc(a9,seicho9$name,type=2)
icc2_2021=as.data.frame(icc2_2021)
y=cbind(icc2_1519,icc2_2021)
colnames(y)=c("2015-2019", "2020-2021")
y=round(y,2)

#save data
write.csv(y,"./ICC2.csv", fileEncoding = "cp932")



#合意指標計算
#ファイル読み込み
setwd("C:/r_analysis/3.20221120/")
getwd()
rm(list=ls())
seicho=read_csv("allfdataoralpha.csv",col_names =T, na = c(""),locale = locale(encoding = "CP932"))
seicho=seicho[,-1]
seicho8=seicho %>% filter(date !=2020 & date!=2021)　#2015-2019
seicho9=seicho %>% filter(date ==2020 | date==2021)  #2020-2021

#変数分割
a1=seicho8 %>% select(CM01_01:CM01_07) %>%  as.data.frame()
a2=seicho8 %>% select(CM02_01:CM02_08) %>%  as.data.frame()
a3=seicho8 %>% select(CM03_01:CM03_08) %>%  as.data.frame()
a4=seicho8 %>% select(OCI_H_1:OCI_H_10) %>%  as.data.frame()
a5=seicho8 %>% select(OCI_AF_1:OCI_AF_10) %>%  as.data.frame()
a6=seicho8 %>% select(OCI_AC_1:OCI_AC_10) %>%  as.data.frame()
a7=seicho8 %>% select(OCI_SA_1:OCI_SA_10) %>%  as.data.frame()
a8=seicho8 %>% select(DP01_02,DP02_02,DP03_02,DP04_02) %>%  as.data.frame()

b1=seicho9 %>% select(CM01_01:CM01_07) %>%  as.data.frame()
b2=seicho9 %>% select(CM02_01:CM02_08) %>%  as.data.frame()
b3=seicho9 %>% select(CM03_01:CM03_08) %>%  as.data.frame()
b4=seicho9 %>% select(OCI_H_1:OCI_H_10) %>%  as.data.frame()
b5=seicho9 %>% select(OCI_AF_1:OCI_AF_10) %>%  as.data.frame()
b6=seicho9 %>% select(OCI_AC_1:OCI_AC_10) %>%  as.data.frame()
b7=seicho9 %>% select(OCI_SA_1:OCI_SA_10) %>%  as.data.frame()
b8=seicho9 %>% select(DP01_02,DP02_02,DP03_02,DP04_02) %>%  as.data.frame()


#合意指標抽出
Affective=rwg.j(round(a1*10), seicho8$name,ranvar=5)$rwg.j
Normative=rwg.j(round(a2*10), seicho8$name, ranvar=5)$rwg.j
Continuance=rwg.j(round(a3*10), seicho8$name, ranvar=5)$rwg.j

Humanistic=rwg.j(round(a4*10), seicho8$name,ranvar=5)$rwg.j
Affiliative=rwg.j(round(a5*10), seicho8$name,ranvar=5)$rwg.j
Achievement=rwg.j(round(a6*10), seicho8$name,ranvar=5)$rwg.j
Self_actualization=rwg.j(round(a7*10), seicho8$name,ranvar=5)$rwg.j
Entrepreneurial=rwg.j(round(a8), seicho8$name,ranvar=5)$rwg.j

total=rbind(mean(Affective),
            mean(Normative),
            mean(Continuance),
            mean(Humanistic),
            mean(Affiliative),
            mean(Achievement),
            mean(Self_actualization),
            mean(Entrepreneurial))
total=as.data.frame(total)

Affective=rwg.j(round(b1*10), seicho9$name,ranvar=5)$rwg.j
Normative=rwg.j(round(b2*10), seicho9$name, ranvar=5)$rwg.j
Continuance=rwg.j(round(b3*10), seicho9$name, ranvar=5)$rwg.j

Humanistic=rwg.j(round(b4*10), seicho9$name,ranvar=5)$rwg.j
Affiliative=rwg.j(round(b5*10), seicho9$name,ranvar=5)$rwg.j
Achievement=rwg.j(round(b6*10), seicho9$name,ranvar=5)$rwg.j
Self_actualization=rwg.j(round(b7*10), seicho9$name,ranvar=5)$rwg.j
Entrepreneurial=rwg.j(round(b8), seicho9$name,ranvar=5)$rwg.j


total1=rbind(mean(Affective),
             mean(Normative),
             mean(Continuance),
             mean(Humanistic),
             mean(Affiliative),
             mean(Achievement),
             mean(Self_actualization),
             mean(Entrepreneurial))
total=cbind(total,total1)
total=as.data.frame(total)

#save data
write_excel_csv(total, "./9.output/4.rwg.csv")