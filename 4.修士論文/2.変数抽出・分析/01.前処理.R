
#ファイル読み込み＋集計
library(tidyverse)
rm(list=ls())
dir='C:/r_analysis/1.20221101/0.newdata/'
file_list=list.files(dir)
file_list
length(file_list)
file_list=as.data.frame(file_list)

a=data.frame()

for (file in file_list) {
  temp=read_csv(paste(dir,file,sep="/"), col_names =T, na = c(""))
  a=rbind(a,temp)
}

a=a[,-1]

#sex
#男:0 , 女:1, 非公開:2
mypattern=grepl('男性',a$stat)    
mypattern1=grepl('女性',a$stat)    
a$sex=ifelse(mypattern==T,0,ifelse(mypattern1==T,1,2))

#retire
#退職:0 , 在職:1
mypattern2=grepl('退職済み',a$stat)  
a$retire=ifelse(mypattern2==T,0,1)

#type
#正社員:0, 非正規雇用:1, その他:2
mypattern3=grepl('(正社員)',a$stat)  
mypattern4=grepl('(非正社員)',a$stat)  
a$type=ifelse(mypattern4==T,1,ifelse(mypattern3==T,0,2))

#date
datepattern=regexpr('[0-9]{4}',a$date)
a$date=regmatches(a$date,datepattern)

#star2
a$star=gsub("NA",NA,a$star)
a$star=as.numeric(a$star)
c=mean(a$star,na.rm=T)
a$star2=ifelse(is.na(a$star),mean(a$star,na.rm=T),a$star)

#label
b1=quantile(a$star,0.77,na.rm=T)
c1=quantile(a$star,0.22,na.rm=T)
a$label_k=ifelse(a$star2>=b1,1,ifelse(a$star2<=c1,0,2))

#変数集計
a=a%>%select(name,date,sex,retire,type,star,star2,label_k,review)


#review cleansing
a$review=gsub("成長・キャリア開発：|成長・キャリア開発:|働きがい:|働きがい：|教育・研修：|教育・研修:|\\【.*?\\】|\n|\\!\\?\\~|\\(.*?\\)|\n\n|\\?|\\!|\\~|\\？|\\！|\\～","",a$review)

review=a$review
review=gsub("\\、","",review)
review=gsub("\\。","",review)
review=gsub("\\→","",review)
review=gsub("\\、","",review)
review=gsub("\\，","",review)
review=gsub("\\,","",review)
review=gsub("\\「|\\」","",review)
review=gsub("\\【|\\】","",review)
review=gsub("\\、","",review)
review=gsub("\\，|\\,|\\。|\\．|\\.","",review)
review=gsub("\\(|\\)","",review)
review=gsub("\\（|\\）","",review)
review=gsub("\\:|\\：","",review)
review=gsub("\\・","",review)
review=gsub("\\?|\\？","",review)
review=gsub("\\%|\\％","",review)
review=gsub("[[:punct:]]", "", review)
review=gsub("[[:space:]]","",review)
review=gsub("[[:blank:]]","",review)
review=toupper(review)
a$review=review

#短いレビューデータ除去
h=nchar(a$review)
a=a %>% filter(h>=15)

#factorize
a$star=as.numeric(a$star)
a$star2=as.numeric(a$star2)
a$name=as.factor(a$name)
a$date=as.numeric(a$date)
a$sex=as.factor(a$sex)
a$retire=as.factor(a$retire)
a$type=as.factor(a$type)
a$label_k=as.factor(a$label_k)
a=as.data.frame(a)

write.csv(a, "C:/r_analysis/1.20221101/data_complete_34814.csv", fileEncoding = "cp932", row.names = FALSE)



################################################################
################################################################
################################################################
#in a : U+7E6B: U+4E2B:  // in b :U+5653: U+31F2: U+5699: