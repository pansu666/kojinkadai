#ファイル読み込み
library(tidyverse)
library(psych)
setwd("C:/r_analysis/3.20221120/")
getwd()
rm(list=ls())
seicho=read_csv("allforalpha1120.csv",col_names =T, na = c(""),locale = locale(encoding = "CP932"))
seicho=seicho[,-1]

#年度指定
seicho=seicho %>% filter(date!=2021 & date!=2020) 

#会社番号、データフレイム作成
z=paste0("b000",1:9)
z1=paste0("b00",10:99)
z3=paste0("b0",100:179)
z4=c(z,z1,z3)
score=data.frame()
koumoku=data.frame()
colnames(seicho)


#最終変数抽出　loop

for (i in z4){
 
  hensu=seicho %>% filter(name==i)
  name=hensu$name[1]
  
  
  rikert_mean_CM=hensu %>% select(CM1_1_rk:CM3_8_rk) %>% apply(2,mean) %>% t() %>% as.data.frame()
  rikert_mean_OCI=hensu %>% select(OCI_H_1_rk:OCI_SA_10_rk) %>% apply(2,mean) %>% t() %>% as.data.frame()
  
  CM_sim_inventory=hensu %>% select(CM01_01:CM03_08) %>% apply(2,mean) %>% t() %>% as.data.frame()
  OCI_sim_inventory=hensu %>% select(OCI_H_1:OCI_SA_10) %>% apply(2,mean) %>% t() %>% as.data.frame()
  

  AFFECTIVE=mean(hensu$AFFECTIVE)
  CONTINUANCE=mean(hensu$CONTINUANCE)
  NORMATIVE=mean(hensu$NORMATIVE)
  CONSTRUCTIVE = mean(hensu$OCI_CONSTRUCTIVE)
  ENTREPRENEURIAL = mean(hensu$DP_ENTREPRENEURIAL)
  
  AFFECTIVE_RK=mean(hensu$AFFECTIVE_rk)
  CONTINUANCE_RK=mean(hensu$CONTINUANCE_rk)
  NORMATIVE_RK=mean(hensu$NORMATIVE_rk)
  CONSTRUCTIVE_RK = mean(hensu$OCI_CONSTRUCTIVE_rk)
  ENTREPRENEURIAL_RK = mean(hensu$DP_ENTREPRENEURIAL_rk)
  

  c=hensu %>% select(OCI_humanistic,OCI_affiliative,OCI_achievement,OCI_self)
  c1=c %>% apply(2,mean) %>% t() %>% as.data.frame()
  
  d=hensu %>% select(OCI_humanistic_rk,OCI_affiliative_rk,OCI_achievement_rk,OCI_self_rk)
  d1=d %>% apply(2,mean) %>% t() %>% as.data.frame()
  
  samples=nrow(hensu)
  samples=as.data.frame(samples)
  
  
  a=cbind(name,samples,
          AFFECTIVE,NORMATIVE,CONTINUANCE,
          AFFECTIVE_RK,NORMATIVE_RK,CONTINUANCE_RK,
          CONSTRUCTIVE,
          CONSTRUCTIVE_RK,
          ENTREPRENEURIAL,ENTREPRENEURIAL_RK, 
          c1,d1)
  
  b=cbind(name,samples,CM_sim_inventory,OCI_sim_inventory,
          rikert_mean_CM,rikert_mean_OCI)
  
  score=rbind(score,a)
  koumoku=rbind(koumoku,b)
  
  print(paste0("kaisha:",i, " ///  NA of a : FALSE= ",table(is.na(a)),"  ///  ","NA of b : FALSE= ",table(is.na(b))))
  print(paste0("kaisha:",i," ///  number of items :  ",nrow(c),",",nrow(d),",",nrow(hensu)))
  
  
}

#欠損値確認
table(is.na(score))
table(is.na(koumoku))

#save data
write.csv(score,"total1519.csv", fileEncoding = "cp932")
write.csv(koumoku,"koumoku1519.csv", fileEncoding = "cp932")