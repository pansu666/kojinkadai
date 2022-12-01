library(tidyverse)
#라벨만들기
rm(list=ls())
setwd("C:/r_analysis/3.20221120/")
getwd()
seicho1=read_csv("data_complete_34814.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 

CM1=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/CM_01.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
CM2=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/CM_02.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
CM3=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/CM_03.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 

OCI1_1=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/OCI_01_01.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
OCI1_2=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/OCI_01_02.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
OCI1_3=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/OCI_01_03.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
OCI1_4=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/OCI_01_04.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 

DP1=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/DP01.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
DP2=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/DP02.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
DP3=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/DP03.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 
DP4=read_csv("C:/r_analysis/3.20221120/6.similarity/colab_version/DP04.csv", col_names =T, na = c(""),locale = locale(encoding = "CP932")) 



seicho_f=cbind(seicho1,
        CM1,CM2,CM3,
        OCI1_1,OCI1_2,OCI1_3,OCI1_4,DP1,DP2,DP3,DP4)

#회사 그룹지정
seicho_f$group=case_when(seicho_f$name=="b0001"~"鉄鋼",
                         seicho_f$name=="b0002"~"情報業",
                         seicho_f$name=="b0003"~"輸送用機器",
                         seicho_f$name=="b0004"~"輸送用機器",
                         seicho_f$name=="b0005"~"卸売業",
                         seicho_f$name=="b0006"~"情報業",
                         seicho_f$name=="b0007"~"輸送用機器",
                         seicho_f$name=="b0008"~"サービス業",
                         seicho_f$name=="b0009"~"保険業",
                         seicho_f$name=="b0010"~"電気機器",
                         seicho_f$name=="b0011"~"機械",
                         seicho_f$name=="b0012"~"機械",
                         seicho_f$name=="b0013"~"電気機器",
                         seicho_f$name=="b0014"~"輸送用機器",
                         seicho_f$name=="b0015"~"電気機器",
                         seicho_f$name=="b0016"~"情報業",
                         seicho_f$name=="b0017"~"機械",
                         seicho_f$name=="b0018"~"電気・ガス業",
                         seicho_f$name=="b0019"~"輸送用機器",
                         seicho_f$name=="b0020"~"輸送用機器",
                         seicho_f$name=="b0021"~"電気機器",
                         seicho_f$name=="b0022"~"小売業",
                         seicho_f$name=="b0023"~"ゴム製品",
                         seicho_f$name=="b0024"~"輸送用機器",
                         seicho_f$name=="b0025"~"輸送用機器",
                         seicho_f$name=="b0026"~"サービス業",
                         seicho_f$name=="b0027"~"サービス業",
                         seicho_f$name=="b0028"~"電気機器",
                         seicho_f$name=="b0029"~"電気・ガス業",
                         seicho_f$name=="b0030"~"建設業",
                         seicho_f$name=="b0031"~"建設業",
                         seicho_f$name=="b0032"~"繊維製品",
                         seicho_f$name=="b0033"~"陸運業",
                         seicho_f$name=="b0034"~"電気機器",
                         seicho_f$name=="b0035"~"医薬品",
                         seicho_f$name=="b0036"~"輸送用機器",
                         seicho_f$name=="b0037"~"電気機器",
                         seicho_f$name=="b0038"~"輸送用機器",
                         seicho_f$name=="b0039"~"電気機器",
                         seicho_f$name=="b0040"~"機械",
                         seicho_f$name=="b0041"~"不動産業",
                         seicho_f$name=="b0042"~"鉄鋼",
                         seicho_f$name=="b0043"~"化学",
                         seicho_f$name=="b0044"~"電気機器",
                         seicho_f$name=="b0045"~"食料品",
                         seicho_f$name=="b0046"~"サービス業",
                         seicho_f$name=="b0047"~"電気機器",
                         seicho_f$name=="b0048"~"情報業",
                         seicho_f$name=="b0049"~"鉄鋼",
                         seicho_f$name=="b0050"~"輸送用機器",
                         seicho_f$name=="b0051"~"輸送用機器",
                         seicho_f$name=="b0052"~"建設業",
                         seicho_f$name=="b0053"~"電気機器",
                         seicho_f$name=="b0054"~"輸送用機器",
                         seicho_f$name=="b0055"~"精密機器",
                         seicho_f$name=="b0056"~"サービス業",
                         seicho_f$name=="b0057"~"精密機器",
                         seicho_f$name=="b0058"~"サービス業",
                         seicho_f$name=="b0059"~"電気機器",
                         seicho_f$name=="b0060"~"情報業",
                         seicho_f$name=="b0061"~"保険業",
                         seicho_f$name=="b0062"~"サービス業",
                         seicho_f$name=="b0063"~"電気機器",
                         seicho_f$name=="b0064"~"卸売業",
                         seicho_f$name=="b0065"~"小売業",
                         seicho_f$name=="b0066"~"情報業",
                         seicho_f$name=="b0067"~"化学",
                         seicho_f$name=="b0068"~"電気機器",
                         seicho_f$name=="b0069"~"化学",
                         seicho_f$name=="b0070"~"サービス業",
                         seicho_f$name=="b0071"~"メーカー",
                         seicho_f$name=="b0072"~"輸送用機器",
                         seicho_f$name=="b0073"~"輸送用機器",
                         seicho_f$name=="b0074"~"金属製品",
                         seicho_f$name=="b0075"~"メーカー",
                         seicho_f$name=="b0076"~"機械",
                         seicho_f$name=="b0077"~"電気機器",
                         seicho_f$name=="b0078"~"情報業",
                         seicho_f$name=="b0079"~"小売業",
                         seicho_f$name=="b0080"~"小売業",
                         seicho_f$name=="b0081"~"小売業",
                         seicho_f$name=="b0082"~"小売業",
                         seicho_f$name=="b0083"~"小売業",
                         seicho_f$name=="b0084"~"証券",
                         seicho_f$name=="b0085"~"小売業",
                         seicho_f$name=="b0086"~"食料品",
                         seicho_f$name=="b0087"~"小売業",
                         seicho_f$name=="b0088"~"小売業",
                         seicho_f$name=="b0089"~"建設業",
                         seicho_f$name=="b0090"~"小売業",
                         seicho_f$name=="b0091"~"小売業",
                         seicho_f$name=="b0092"~"食料品",
                         seicho_f$name=="b0093"~"不動産業",
                         seicho_f$name=="b0094"~"情報業",
                         seicho_f$name=="b0095"~"小売業",
                         seicho_f$name=="b0096"~"サービス業",
                         seicho_f$name=="b0097"~"水産・農林業",
                         seicho_f$name=="b0098"~"機械",
                         seicho_f$name=="b0099"~"小売業",
                         seicho_f$name=="b0100"~"小売業",
                         seicho_f$name=="b0101"~"情報業",
                         seicho_f$name=="b0102"~"サービス業",
                         seicho_f$name=="b0103"~"小売業",
                         seicho_f$name=="b0104"~"情報業",
                         seicho_f$name=="b0105"~"不動産業",
                         seicho_f$name=="b0106"~"金融サービス",
                         seicho_f$name=="b0107"~"保険業",
                         seicho_f$name=="b0108"~"小売業",
                         seicho_f$name=="b0109"~"サービス業",
                         seicho_f$name=="b0110"~"陸運業",
                         seicho_f$name=="b0111"~"小売業",
                         seicho_f$name=="b0112"~"小売業",
                         seicho_f$name=="b0113"~"不動産業",
                         seicho_f$name=="b0114"~"金融サービス",
                         seicho_f$name=="b0115"~"保険業",
                         seicho_f$name=="b0116"~"建設業",
                         seicho_f$name=="b0117"~"小売業",
                         seicho_f$name=="b0118"~"電気機器",
                         seicho_f$name=="b0119"~"小売業",
                         seicho_f$name=="b0120"~"サービス業",
                         seicho_f$name=="b0121"~"不動産業",
                         seicho_f$name=="b0122"~"サービス業",
                         seicho_f$name=="b0123"~"サービス業",
                         seicho_f$name=="b0124"~"小売業",
                         seicho_f$name=="b0125"~"小売業",
                         seicho_f$name=="b0126"~"銀行業",
                         seicho_f$name=="b0127"~"小売業",
                         seicho_f$name=="b0128"~"小売業",
                         seicho_f$name=="b0129"~"金融サービス",
                         seicho_f$name=="b0130"~"情報業",
                         seicho_f$name=="b0131"~"情報業",
                         seicho_f$name=="b0132"~"サービス業",
                         seicho_f$name=="b0133"~"サービス業",
                         seicho_f$name=="b0134"~"情報業",
                         seicho_f$name=="b0135"~"サービス業",
                         seicho_f$name=="b0136"~"サービス業",
                         seicho_f$name=="b0137"~"小売業",
                         seicho_f$name=="b0138"~"小売業",
                         seicho_f$name=="b0139"~"サービス業",
                         seicho_f$name=="b0140"~"情報業",
                         seicho_f$name=="b0141"~"サービス業",
                         seicho_f$name=="b0142"~"小売業",
                         seicho_f$name=="b0143"~"化学",
                         seicho_f$name=="b0144"~"情報業",
                         seicho_f$name=="b0145"~"情報業",
                         seicho_f$name=="b0146"~"化学",
                         seicho_f$name=="b0147"~"小売業",
                         seicho_f$name=="b0148"~"小売業",
                         seicho_f$name=="b0149"~"メーカー",
                         seicho_f$name=="b0150"~"小売業",
                         seicho_f$name=="b0151"~"小売業",
                         seicho_f$name=="b0152"~"メーカー",
                         seicho_f$name=="b0153"~"電気機器",
                         seicho_f$name=="b0154"~"食料品",
                         seicho_f$name=="b0155"~"金融サービス",
                         seicho_f$name=="b0156"~"小売業",
                         seicho_f$name=="b0157"~"小売業",
                         seicho_f$name=="b0158"~"サービス業",
                         seicho_f$name=="b0159"~"サービス業",
                         seicho_f$name=="b0160"~"小売業",
                         seicho_f$name=="b0161"~"電気機器",
                         seicho_f$name=="b0162"~"卸売業",
                         seicho_f$name=="b0163"~"小売業",
                         seicho_f$name=="b0164"~"小売業",
                         seicho_f$name=="b0165"~"サービス業",
                         seicho_f$name=="b0166"~"陸運業",
                         seicho_f$name=="b0167"~"サービス業",
                         seicho_f$name=="b0168"~"サービス業",
                         seicho_f$name=="b0169"~"陸運業",
                         seicho_f$name=="b0170"~"小売業",
                         seicho_f$name=="b0171"~"小売業",
                         seicho_f$name=="b0172"~"建設業",
                         seicho_f$name=="b0173"~"機械",
                         seicho_f$name=="b0174"~"メーカー",
                         seicho_f$name=="b0175"~"機械",
                         seicho_f$name=="b0176"~"サービス業",
                         seicho_f$name=="b0177"~"サービス業",
                         seicho_f$name=="b0178"~"化学",
                         seicho_f$name=="b0179"~"建設業")

#0:제조 , 1:코우리 , 2: 서비스 , 3:금융업, 4:건설 5:육운, 6:부동산, 7:수산, 8:전기, 9:정보
seicho_f$group=case_when(
  seicho_f$group=="ゴム製品" ~ 0,seicho_f$group=="メーカー" ~ 0,seicho_f$group=="機械" ~ 0,
  seicho_f$group=="繊維製品" ~ 0,seicho_f$group=="輸送用機器" ~ 0,seicho_f$group=="食料品" ~ 0,
  seicho_f$group=="医薬品" ~ 0,seicho_f$group=="電気機器" ~ 0,seicho_f$group=="鉄鋼" ~ 0,
  seicho_f$group=="化学" ~ 0,seicho_f$group=="金属製品" ~ 0,seicho_f$group=="精密機器" ~ 0,
  seicho_f$group=="小売業" ~ 1,seicho_f$group=="卸売業" ~ 1,
  seicho_f$group=="サービス業" ~ 2,
  seicho_f$group=="金融サービス" ~ 3,seicho_f$group=="保険業" ~ 3,seicho_f$group=="証券" ~ 3,
  seicho_f$group=="銀行業" ~ 3,
  seicho_f$group=="建設業" ~ 4,seicho_f$group=="陸運業" ~ 4,seicho_f$group=="不動産業" ~ 4,
  seicho_f$group=="水産・農林業" ~ 4,seicho_f$group=="電気・ガス業" ~ 4,seicho_f$group=="情報業" ~ 4)

seicho_f$group=as.factor(seicho_f$group)

colnames(seicho_f)
seicho_f=seicho_f %>% select(no,name,review_cleansing,date,sex,retire,type,group,CM01_01:DP04_04)

seicho_f$name=as.factor(seicho_f$name)
seicho_f$date=as.factor(seicho_f$date)
seicho_f$sex=as.factor(seicho_f$sex)
seicho_f$retire=as.factor(seicho_f$retire)
seicho_f$type=as.factor(seicho_f$type)

write.csv(seicho_f, "./analysis1120.csv", fileEncoding = "cp932")





