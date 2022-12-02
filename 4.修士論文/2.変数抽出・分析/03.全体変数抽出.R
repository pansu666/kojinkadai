
#ファイル読み込み
library(tidyverse)
library(psych)
setwd("C:/r_analysis/1.20221101/")
getwd()
rm(list=ls())
seicho=read_csv("20221103analysis.csv",col_names =T, na = c(""),locale = locale(encoding = "CP932"))
seicho=seicho[,-1]

#1.類似度点数の合計変数
#1-1.Commitment
CM1=seicho %>% select(CM01_01:CM01_07) %>% as.data.frame()
CM2=seicho %>% select(CM02_01:CM02_06,CM02_08)%>% as.data.frame()
CM3=seicho%>% select(CM03_01:CM03_05,CM03_07,CM03_08)%>% as.data.frame()

#1-2.OCI_Organizational Culture
OCI1_1=seicho %>% select(OCI_H_1:OCI_H_10)
OCI1_2=seicho %>% select(OCI_AF_1:OCI_AF_10)
OCI1_3=seicho %>% select(OCI_AC_1:OCI_AC_10)
OCI1_4=seicho %>% select(OCI_SA_1:OCI_SA_10)

OCI2_1=seicho %>% select(OCI_AP_1:OCI_AP_10)
OCI2_2=seicho %>% select(OCI_CO_1:OCI_CO_10)
OCI2_3=seicho %>% select(OCI_D_1:OCI_D_10)
OCI2_4=seicho %>% select(OCI_AV_1:OCI_AV_10)

OCI3_1=seicho %>% select(OCI_O_1:OCI_O_10)
OCI3_2=seicho %>% select(OCI_P_1:OCI_P_10)
OCI3_3=seicho %>% select(OCI_CP_1:OCI_CP_10)
OCI3_4=seicho %>% select(OCI_PF_1:OCI_PF_10)
OCI_a=cbind(OCI1_1,OCI1_2,OCI1_3,OCI1_4)
OCI_b=cbind(OCI2_1,OCI2_2,OCI2_3,OCI2_4)
OCI_c=cbind(OCI3_1,OCI3_2,OCI3_3,OCI3_4)


#deshpande's organizational culture

DP1=seicho %>% select(DP01_01:DP01_04) %>% +1
DP2=seicho %>% select(DP02_01:DP02_04) %>% +1
DP3=seicho %>% select(DP03_01:DP03_04) %>% +1
DP4=seicho %>% select(DP04_01:DP04_04) %>% +1

DP1_sum=DP1 %>% apply(1,sum)
DP1_sum=as.data.frame(DP1_sum)
DP2_sum=DP2 %>% apply(1,sum)
DP2_sum=as.data.frame(DP2_sum)
DP3_sum=DP3 %>% apply(1,sum)
DP3_sum=as.data.frame(DP3_sum)
DP4_sum=DP4 %>% apply(1,sum)
DP4_sum=as.data.frame(DP4_sum)

DP1_1=(DP1$DP01_01)*100/(DP1_sum)
DP1_2=(DP1$DP01_02)*100/(DP1_sum)
DP1_3=(DP1$DP01_03)*100/(DP1_sum)
DP1_4=(DP1$DP01_04)*100/(DP1_sum)

DP2_1=(DP2$DP02_01)*100/(DP2_sum)
DP2_2=(DP2$DP02_02)*100/(DP2_sum)
DP2_3=(DP2$DP02_03)*100/(DP2_sum)
DP2_4=(DP2$DP02_04)*100/(DP2_sum)

DP3_1=(DP3$DP03_01)*100/(DP3_sum)
DP3_2=(DP3$DP03_02)*100/(DP3_sum)
DP3_3=(DP3$DP03_03)*100/(DP3_sum)
DP3_4=(DP3$DP03_04)*100/(DP3_sum)

DP4_1=(DP4$DP04_01)*100/(DP4_sum)
DP4_2=(DP4$DP04_02)*100/(DP4_sum)
DP4_3=(DP4$DP04_03)*100/(DP4_sum)
DP4_4=(DP4$DP04_04)*100/(DP4_sum)

DP_con=cbind(DP1_1,DP2_1,DP3_1,DP4_1)
DP_entre=cbind(DP1_2,DP2_2,DP3_2,DP4_2)
DP_bureau=cbind(DP1_3,DP2_3,DP3_3,DP4_3)
DP_compe=cbind(DP1_4,DP2_4,DP3_4,DP4_4)


#2.Rikert尺度

transfer=function(x){
  ifelse(x>=quantile(x,0.8),5,
         ifelse(x>=quantile(x,0.6),4,
                ifelse(x>=quantile(x,0.4),3,
                       ifelse(x>=quantile(x,0.2),2,1))))
}


#2-1. Commitmnet
CM1_1_rk=transfer(seicho$CM01_01)
CM1_2_rk=transfer(seicho$CM01_02)
CM1_3_rk=transfer(seicho$CM01_03)
CM1_4_rk=transfer(seicho$CM01_04)
CM1_5_rk=transfer(seicho$CM01_05)
CM1_6_rk=transfer(seicho$CM01_06)
CM1_7_rk=transfer(seicho$CM01_07)

CM2_1_rk=transfer(seicho$CM02_01)
CM2_2_rk=transfer(seicho$CM02_02)
CM2_3_rk=transfer(seicho$CM02_03)
CM2_4_rk=transfer(seicho$CM02_04)
CM2_5_rk=transfer(seicho$CM02_05)
CM2_6_rk=transfer(seicho$CM02_06)
CM2_8_rk=transfer(seicho$CM02_08)

CM3_1_rk=transfer(seicho$CM03_01)
CM3_2_rk=transfer(seicho$CM03_02)
CM3_3_rk=transfer(seicho$CM03_03)
CM3_4_rk=transfer(seicho$CM03_04)
CM3_5_rk=transfer(seicho$CM03_05)
CM3_7_rk=transfer(seicho$CM03_07)
CM3_8_rk=transfer(seicho$CM03_08)

w1=cbind(CM1_1_rk,CM1_2_rk,CM1_3_rk,CM1_4_rk,CM1_5_rk,CM1_6_rk,CM1_7_rk)
w1=as.data.frame(w1)
w2=cbind(CM2_1_rk,CM2_2_rk,CM2_3_rk,CM2_4_rk,CM2_5_rk,CM2_6_rk,CM2_8_rk)
w2=as.data.frame(w2)
w3=cbind(CM3_1_rk,CM3_2_rk,CM3_3_rk,CM3_4_rk,CM3_5_rk,CM3_7_rk,CM3_8_rk)
w3=as.data.frame(w3)


#2-2.OCI_Organizational culture
OCI_H_1_rk=transfer(seicho$OCI_H_1)
OCI_H_2_rk=transfer(seicho$OCI_H_2)
OCI_H_3_rk=transfer(seicho$OCI_H_3)
OCI_H_4_rk=transfer(seicho$OCI_H_4)
OCI_H_5_rk=transfer(seicho$OCI_H_5)
OCI_H_6_rk=transfer(seicho$OCI_H_6)
OCI_H_7_rk=transfer(seicho$OCI_H_7)
OCI_H_8_rk=transfer(seicho$OCI_H_8)
OCI_H_9_rk=transfer(seicho$OCI_H_9)
OCI_H_10_rk=transfer(seicho$OCI_H_10)
r1=cbind(OCI_H_1_rk,OCI_H_2_rk,OCI_H_3_rk,OCI_H_4_rk,
         OCI_H_5_rk,OCI_H_6_rk,OCI_H_7_rk,OCI_H_8_rk,
         OCI_H_9_rk,OCI_H_10_rk)

OCI_AF_1_rk=transfer(seicho$OCI_AF_1)
OCI_AF_2_rk=transfer(seicho$OCI_AF_2)
OCI_AF_3_rk=transfer(seicho$OCI_AF_3)
OCI_AF_4_rk=transfer(seicho$OCI_AF_4)
OCI_AF_5_rk=transfer(seicho$OCI_AF_5)
OCI_AF_6_rk=transfer(seicho$OCI_AF_6)
OCI_AF_7_rk=transfer(seicho$OCI_AF_7)
OCI_AF_8_rk=transfer(seicho$OCI_AF_8)
OCI_AF_9_rk=transfer(seicho$OCI_AF_9)
OCI_AF_10_rk=transfer(seicho$OCI_AF_10)
r2=cbind(OCI_AF_1_rk,OCI_AF_2_rk,OCI_AF_3_rk,OCI_AF_4_rk,
         OCI_AF_5_rk,OCI_AF_6_rk,OCI_AF_7_rk,OCI_AF_8_rk,
         OCI_AF_9_rk,OCI_AF_10_rk)

OCI_AC_1_rk=transfer(seicho$OCI_AC_1)
OCI_AC_2_rk=transfer(seicho$OCI_AC_2)
OCI_AC_3_rk=transfer(seicho$OCI_AC_3)
OCI_AC_4_rk=transfer(seicho$OCI_AC_4)
OCI_AC_5_rk=transfer(seicho$OCI_AC_5)
OCI_AC_6_rk=transfer(seicho$OCI_AC_6)
OCI_AC_7_rk=transfer(seicho$OCI_AC_7)
OCI_AC_8_rk=transfer(seicho$OCI_AC_8)
OCI_AC_9_rk=transfer(seicho$OCI_AC_9)
OCI_AC_10_rk=transfer(seicho$OCI_AC_10)
r3=cbind(OCI_AC_1_rk,OCI_AC_2_rk,OCI_AC_3_rk,OCI_AC_4_rk,
         OCI_AC_5_rk,OCI_AC_6_rk,OCI_AC_7_rk,OCI_AC_8_rk,
         OCI_AC_9_rk,OCI_AC_10_rk)

OCI_SA_1_rk=transfer(seicho$OCI_SA_1)
OCI_SA_2_rk=transfer(seicho$OCI_SA_2)
OCI_SA_3_rk=transfer(seicho$OCI_SA_3)
OCI_SA_4_rk=transfer(seicho$OCI_SA_4)
OCI_SA_5_rk=transfer(seicho$OCI_SA_5)
OCI_SA_6_rk=transfer(seicho$OCI_SA_6)
OCI_SA_7_rk=transfer(seicho$OCI_SA_7)
OCI_SA_8_rk=transfer(seicho$OCI_SA_8)
OCI_SA_9_rk=transfer(seicho$OCI_SA_9)
OCI_SA_10_rk=transfer(seicho$OCI_SA_10)
r4=cbind(OCI_SA_1_rk,OCI_SA_2_rk,OCI_SA_3_rk,OCI_SA_4_rk,
         OCI_SA_5_rk,OCI_SA_6_rk,OCI_SA_7_rk,OCI_SA_8_rk,
         OCI_SA_9_rk,OCI_SA_10_rk)

#2-3.deshpande's Organizational Culture
DP1_1_rk=transfer(DP1_1$DP1_sum)
DP1_2_rk=transfer(DP1_2$DP1_sum)
DP1_3_rk=transfer(DP1_3$DP1_sum)
DP1_4_rk=transfer(DP1_4$DP1_sum)

DP2_1_rk=transfer(DP2_1$DP2_sum)
DP2_2_rk=transfer(DP2_2$DP2_sum)
DP2_3_rk=transfer(DP2_3$DP2_sum)
DP2_4_rk=transfer(DP2_4$DP2_sum)

DP3_1_rk=transfer(DP3_1$DP3_sum)
DP3_2_rk=transfer(DP3_2$DP3_sum)
DP3_3_rk=transfer(DP3_3$DP3_sum)
DP3_4_rk=transfer(DP3_4$DP3_sum)

DP4_1_rk=transfer(DP4_1$DP4_sum)
DP4_2_rk=transfer(DP4_2$DP4_sum)
DP4_3_rk=transfer(DP4_3$DP4_sum)
DP4_4_rk=transfer(DP4_4$DP4_sum)

q1=cbind(DP1_1_rk,DP2_1_rk,DP3_1_rk,DP4_1_rk)
q1=as.data.frame(q1)
q2=cbind(DP1_2_rk,DP2_2_rk,DP3_2_rk,DP4_2_rk)
q2=as.data.frame(q2)
q3=cbind(DP1_3_rk,DP2_3_rk,DP3_3_rk,DP4_3_rk)
q3=as.data.frame(q3)
q4=cbind(DP1_4_rk,DP2_4_rk,DP3_4_rk,DP4_4_rk)
q4=as.data.frame(q4)


#Rikert尺度集計
r1_1=cbind(r1,r2,r3,r4)
r2_1=cbind(r5,r6,r7,r8)
r3_1=cbind(r9,r10,r11,r12)
rikert_cul=cbind(w1,w2,w3,r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,q1,q2,q3,q4)
rikert_cul=as.data.frame(rikert_cul)


#3.変数追加
#3-1.類似度変数
seicho<-mutate(seicho, AFFECTIVE=apply(CM1,1,sum))
seicho<-mutate(seicho, CONTINUANCE=apply(CM2,1,sum))
seicho<-mutate(seicho, NORMATIVE=apply(CM3,1,sum))

seicho<-mutate(seicho, OCI_CONSTRUCTIVE=apply(OCI_a,1,sum))
seicho<-mutate(seicho, OCI_PASSIVE=apply(OCI_b,1,sum))
seicho<-mutate(seicho, OCI_AGRESSIVE=apply(OCI_c,1,sum))

seicho<-mutate(seicho, OCI_humanistic=apply(OCI1_1,1,sum))
seicho<-mutate(seicho, OCI_affiliative=apply(OCI1_2,1,sum))
seicho<-mutate(seicho, OCI_achievement=apply(OCI1_3,1,sum))
seicho<-mutate(seicho, OCI_self=apply(OCI1_4,1,sum))
seicho<-mutate(seicho, OCI_approval=apply(OCI2_1,1,sum))
seicho<-mutate(seicho, OCI_conventional=apply(OCI2_2,1,sum))
seicho<-mutate(seicho, OCI_dependent=apply(OCI2_3,1,sum))
seicho<-mutate(seicho, OCI_avoidance=apply(OCI2_4,1,sum))
seicho<-mutate(seicho, OCI_oppositional=apply(OCI3_1,1,sum))
seicho<-mutate(seicho, OCI_power=apply(OCI3_2,1,sum))
seicho<-mutate(seicho, OCI_competitive=apply(OCI3_3,1,sum))
seicho<-mutate(seicho, OCI_perfection=apply(OCI3_4,1,sum))

seicho<-mutate(seicho, DP_ENTREPRENEURIAL=apply(DP_entre,1,sum))


#3-2.Rikert尺度変数
seicho=mutate(seicho, AFFECTIVE_rk=apply(w1,1,sum))
seicho=mutate(seicho, CONTINUANCE_rk=apply(w2,1,sum))
seicho=mutate(seicho, CONTINUANCE_PS_rk=apply(w_ps,1,sum))
seicho=mutate(seicho, CONTINUANCE_PA_rk=apply(w_pa,1,sum))
seicho=mutate(seicho, NORMATIVE_rk=apply(w3,1,sum))

seicho<-mutate(seicho, OCI_CONSTRUCTIVE_rk=apply(r1_1,1,sum))
seicho<-mutate(seicho, OCI_PASSIVE_rk=apply(r2_1,1,sum))
seicho<-mutate(seicho, OCI_AGRESSIVE_rk=apply(r3_1,1,sum))

seicho<-mutate(seicho, OCI_humanistic_rk=apply(r1,1,sum))
seicho<-mutate(seicho, OCI_affiliative_rk=apply(r2,1,sum))
seicho<-mutate(seicho, OCI_achievement_rk=apply(r3,1,sum))
seicho<-mutate(seicho, OCI_self_rk=apply(r4,1,sum))
seicho<-mutate(seicho, OCI_approval_rk=apply(r5,1,sum))
seicho<-mutate(seicho, OCI_conventional_rk=apply(r6,1,sum))
seicho<-mutate(seicho, OCI_dependent_rk=apply(r7,1,sum))
seicho<-mutate(seicho, OCI_avoidance_rk=apply(r8,1,sum))
seicho<-mutate(seicho, OCI_oppositional_rk=apply(r9,1,sum))
seicho<-mutate(seicho, OCI_power_rk=apply(r10,1,sum))
seicho<-mutate(seicho, OCI_competitive_rk=apply(r11,1,sum))
seicho<-mutate(seicho, OCI_perfection_rk=apply(r12,1,sum))

seicho<-mutate(seicho, DP_CONSENSUAL_rk=apply(q1,1,sum))
seicho<-mutate(seicho, DP_ENTREPRENEURIAL_rk=apply(q2,1,sum))
seicho<-mutate(seicho, DP_BUREAUCRATIC_rk=apply(q3,1,sum))
seicho<-mutate(seicho, DP_COMPETITIVE_rk=apply(q4,1,sum))

colnames(DP1_1)=c("DP01_01")
colnames(DP1_2)=c("DP01_02")
colnames(DP1_3)=c("DP01_03")
colnames(DP1_4)=c("DP01_04")
colnames(DP2_1)=c("DP02_01")
colnames(DP2_2)=c("DP02_02")
colnames(DP2_3)=c("DP02_03")
colnames(DP2_4)=c("DP02_04")
colnames(DP3_1)=c("DP03_01")
colnames(DP3_2)=c("DP03_02")
colnames(DP3_3)=c("DP03_03")
colnames(DP3_4)=c("DP03_04")
colnames(DP4_1)=c("DP04_01")
colnames(DP4_2)=c("DP04_02")
colnames(DP4_3)=c("DP04_03")
colnames(DP4_4)=c("DP04_04")

seicho=seicho %>% select(-DP01_01,-DP01_02,-DP01_03,-DP01_04,-DP02_01,-DP02_02,-DP02_03,-DP02_04,-DP03_01,-DP03_02,-DP03_03,-DP03_04,-DP04_01,-DP04_02,-DP04_03,-DP04_04)
seicho=cbind(seicho,DP1_1,DP1_2,DP1_3,DP1_4,DP2_1,DP2_2,DP2_3,DP2_4,DP3_1,DP3_2,DP3_3,DP3_4,DP4_1,DP4_2,DP4_3,DP4_4)
seicho=seicho %>% select(no:CM03_08,DP01_01:DP04_04,OCI_H_1:DP_COMPETITIVE_rk)
seicho=cbind(seicho,rikert_cul)

#save data
write.csv(seicho,"./alldataforalpha.csv", fileEncoding = "cp932")

