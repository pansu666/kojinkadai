library(tidyverse)
library(psych)
library(caret)
library(MVN)
library(chemometrics)
setwd("C:/r_analysis/3.20221120/")
getwd()
rm(list=ls())

#1519ファイル読み込み＋変数指定
total=read_csv("total15-19.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
zaimu=read_csv("zaimu1124.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
zaimu=zaimu[,-1]
zaimu=zaimu[,-1]
total=total[,-1]

total$name=as.factor(total$name)
total$AFFECTIVE=as.numeric(total$AFFECTIVE)
total$NORMATIVE=as.numeric(total$NORMATIVE)
total$CONTINUANCE=as.numeric(total$CONTINUANCE)
total$CONSTRUCTIVE=as.numeric(total$CONSTRUCTIVE)
total$OCI_humanistic=as.numeric(total$OCI_humanistic)
total$OCI_affiliative=as.numeric(total$OCI_affiliative)
total$OCI_achievement=as.numeric(total$OCI_achievement)
total$OCI_self=as.numeric(total$OCI_self)
total$ENTREPRENEURIAL=as.numeric(total$ENTREPRENEURIAL)
zaimu=zaimu %>% select(ebitda5)
rikert=total %>% select(name,AFFECTIVE,NORMATIVE,CONTINUANCE,CONSTRUCTIVE,
                        OCI_humanistic,OCI_affiliative,OCI_achievement,OCI_self,
                        ENTREPRENEURIAL)
a=cbind(rikert,zaimu)
a1=a %>% select(ebitda5,OCI_humanistic,OCI_affiliative,
                OCI_achievement,OCI_self,AFFECTIVE,
                CONTINUANCE,NORMATIVE,ENTREPRENEURIAL)


#2020-2021ファイル読み込み＋変数指定

total1=read_csv("total20-21.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
zaimu1=read_csv("zaimu1120.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
zaimu1=zaimu1[,-1]
total1=total1[,-1]
total1$AFFECTIVE=as.numeric(total1$AFFECTIVE)
total1$NORMATIVE=as.numeric(total1$NORMATIVE)
total1$CONTINUANCE=as.numeric(total1$CONTINUANCE)
total1$CONSTRUCTIVE=as.numeric(total1$CONSTRUCTIVE)
total1$OCI_humanistic=as.numeric(total1$OCI_humanistic)
total1$OCI_affiliative=as.numeric(total1$OCI_affiliative)
total1$OCI_achievement=as.numeric(total1$OCI_achievement)
total1$OCI_self=as.numeric(total1$OCI_self)
zaimu1=zaimu1 %>% select(ebitda2)
total1$ENTREPRENEURIAL=as.numeric(total1$ENTREPRENEURIAL)
rikert1=total1 %>% select(AFFECTIVE,NORMATIVE,CONTINUANCE,CONSTRUCTIVE,
                          OCI_humanistic,OCI_affiliative,OCI_achievement,OCI_self,
                          ENTREPRENEURIAL)
a2=cbind(rikert1,zaimu1)
a2=a2 %>% select(ebitda2,OCI_humanistic,OCI_affiliative,
                OCI_achievement,OCI_self,AFFECTIVE,
                CONTINUANCE,NORMATIVE,ENTREPRENEURIAL)


#外れ値除去（mahalanovis）
examScor_outlier_m=Moutlier(a1,quantile = 0.99)
examScor_outlier_md <- examScor_outlier_m$md
examScor_outlier_md <- examScor_outlier_md %>% as.vector()
examScor_outlier_md <- examScor_outlier_md %>% unlist()
examScor_outlier_md <- examScor_outlier_md %>% as.numeric()   
examScor_outlier_cut <- examScor_outlier_m$cutoff
examScor_outlier_cut <- examScor_outlier_cut %>% as.vector()
examScor_outlier_cut <- examScor_outlier_cut %>% unlist()
examScor_outlier_cut <- examScor_outlier_cut %>% as.numeric()

outlier_md <- c()   #外れ値を書くdata frame

#loopで外れ値検出
for(i in 1:length(examScor_outlier_md)){ 
  if (examScor_outlier_md[i] >= examScor_outlier_cut) {      # cut-off value보다 크면 outlier_md에 저장합니다
    outlier_cmi <- print(i) 
    outlier_md <- append(outlier_md, outlier_cmi)} 
}

#1519外れ値除去
a1=a1 %>% filter(name!="b0063" &
                  name!="b0098" &
                  name!="b0139" &
                  name!="b0144" & 
                  name!="b0175" &
                  name!="b0121" )

#2021
a2=a2 %>% filter(name!="b0045" & name!="b0048" & name!="b0055" & name!="b0059" &
                  name!="b0062" & name!="b0063" & name!="b0078" & name!="b0083" &
                  name!="b0097" & name!="b0136" & name!="b0139" & name!="b0144"&
                  name!="b0167" & name!="b0171" & name!="b0175" & name!="b0177")



#多変量正規性検定
b1=a1 %>% select(AFFECTIVE,OCI_humanistic)
b2=a1 %>% select(AFFECTIVE,OCI_affiliative)
b3=a1 %>% select(AFFECTIVE,OCI_achievement)
b4=a1 %>% select(AFFECTIVE,OCI_self)

b5=a1 %>% select(NORMATIVE,OCI_humanistic)
b6=a1 %>% select(NORMATIVE,OCI_affiliative)
b7=a1 %>% select(NORMATIVE,OCI_achievement)
b8=a1 %>% select(NORMATIVE,OCI_self)

b9=a1 %>% select(CONTINUANCE,OCI_humanistic)
b10=a1 %>% select(CONTINUANCE,OCI_affiliative)
b11=a1 %>% select(CONTINUANCE,OCI_achievement)
b12=a1 %>% select(CONTINUANCE,OCI_self)

b13=a1 %>% select(AFFECTIVE,ebitda5)
b14=a1 %>% select(NORMATIVE,ebitda5)
b15=a1 %>% select(CONTINUANCE,ebitda5)

b16=a1 %>% select(OCI_humanistic,ebitda5)
b17=a1 %>% select(OCI_affiliative,ebitda5)
b18=a1 %>% select(OCI_achievement,ebitda5)
b19=a1 %>% select(OCI_self,ebitda5)
b20=a1 %>% select(ENTREPRENEURIAL,ebitda5)


b1=a2 %>% select(AFFECTIVE,OCI_humanistic)
b2=a2 %>% select(AFFECTIVE,OCI_affiliative)
b3=a2 %>% select(AFFECTIVE,OCI_achievement)
b4=a2 %>% select(AFFECTIVE,OCI_self)

b5=a2 %>% select(NORMATIVE,OCI_humanistic)
b6=a2 %>% select(NORMATIVE,OCI_affiliative)
b7=a2 %>% select(NORMATIVE,OCI_achievement)
b8=a2 %>% select(NORMATIVE,OCI_self)

b9=a2 %>% select(CONTINUANCE,OCI_humanistic)
b10=a2 %>% select(CONTINUANCE,OCI_affiliative)
b11=a2 %>% select(CONTINUANCE,OCI_achievement)
b12=a2 %>% select(CONTINUANCE,OCI_self)

b13=a2 %>% select(AFFECTIVE,ebitda2)
b14=a2 %>% select(NORMATIVE,ebitda2)
b15=a2 %>% select(CONTINUANCE,ebitda2)


b16=a2 %>% select(OCI_humanistic,ebitda2)
b17=a2 %>% select(OCI_affiliative,ebitda2)
b18=a2 %>% select(OCI_achievement,ebitda2)
b19=a2 %>% select(OCI_self,ebitda2)
b20=a2 %>% select(ENTREPRENEURIAL,ebitda2)

mvn(b1,multivariatePlot = "qq")
mvn(b2,multivariatePlot = "qq")
mvn(b3,multivariatePlot = "qq")
mvn(b4,multivariatePlot = "qq")
mvn(b5,multivariatePlot = "qq")
mvn(b6,multivariatePlot = "qq")
mvn(b7,multivariatePlot = "qq")
mvn(b8,multivariatePlot = "qq")
mvn(b9,multivariatePlot = "qq")
mvn(b10,multivariatePlot = "qq")
mvn(b11,multivariatePlot = "qq")
mvn(b12,multivariatePlot = "qq")
mvn(b13,multivariatePlot = "qq")
mvn(b14,multivariatePlot = "qq")
mvn(b15,multivariatePlot = "qq")
mvn(b16,multivariatePlot = "qq")
mvn(b17,multivariatePlot = "qq")
mvn(b18,multivariatePlot = "qq")
mvn(b19,multivariatePlot = "qq")
mvn(b20,multivariatePlot = "qq")




#相関分析
x=round(corr.test(a1)$r,3)
x=as.data.frame(x)
y=round(corr.test(a1)$p,10)
y=as.data.frame(y)
x1=round(corr.test(a2)$r,3)
x1=as.data.frame(x1)
y1=round(corr.test(a2)$p,10)
y1=as.data.frame(y1)

#save data
write_excel_csv(x, "./9.output/1519r.csv")
write_excel_csv(y, "./9.output/1519p.csv")
write_excel_csv(x1, "./9.output/2021r.csv")
write_excel_csv(y1, "./9.output/2021p.csv")
