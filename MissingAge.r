> train=read.csv('train.csv',header=T)
> train1=train
> train1$Pclass=as.factor(train1$Pclass)
> train1$Survived=as.factor(train1$Survived)

> test=read.csv('test.csv',header=T)
> Survived=rep('None',nrow(test))
> test1=data.frame(test,Survived)
> test2=test1
> test2$Pclass=as.factor(test2$Pclass)

#Write a function to extrac titles
> extractTitle=function(name){
+ if (length(grep('Miss',name))>0){return ('Miss')}
+ if (length(grep('Master',name))>0){return ('Master')}
+ if (length(grep('Mrs',name))>0){return ('Mrs')}
+ if (length(grep('Mr',name))>0){return ('Mr')}
+ else {return ('other')}
+ }

#Distribution of titles in those with missing age
> titles=NULL
> trainNA=train1[is.na(train1$Age),]
> for (i in 1:nrow(trainNA)){
+ titles=c(titles,extractTitle(trainNA[i,'Name']))
+ }
> table(titles)
titles
Master   Miss     Mr    Mrs  other 
     4     36    119     17      1 
> trainNA1=data.frame(trainNA,titles)

#Extract title from "train2" and find median age for each title class
> train2=train1[complete.cases(train1$Age),]
> titles2=NULL
> for (i in 1:nrow(train2)){
+ titles2=c(titles,extractTitle(train2[i,'Name']))
+ } 
> table(titles2)
titles2
Master   Miss     Mr    Mrs  other 
    36    146    399    110     23
> train2.title=data.frame(train2,titles2)

> summary(train2.title[train2.title$titles2=='Miss',]$Age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   0.75   14.12   21.00   21.77   30.00   63.00 
> summary(train2.title[train2.title$titles2=='Master',]$Age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  0.420   1.000   3.500   4.574   8.000  12.000 
> summary(train2.title[train2.title$titles2=='Mrs',]$Age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  14.00   27.25   35.00   35.90   44.00   63.00 
> summary(train2.title[train2.title$titles2=='Mr',]$Age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  11.00   23.00   30.00   32.41   39.00   80.00 
> summary(train2.title[train2.title$titles2=='other',]$Age)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  23.00   30.00   44.00   42.65   53.00   70.00 

#Use the mean age for each title group to replace missing age in “train1”
>titles=NULL
> for (i in 1:nrow(train1)){
+ titles=c(titles,extractTitle(train1[i,'Name']))
+ }
> table(titles)
titles
Master   Miss     Mr    Mrs  other 
    40    182    518    127     24
> train1.title=data.frame(train1,titles) 

> train1.na=train1.title[is.na(train1.title$Age),]
> train1.na[train1.na$titles=='Miss',]$Age=21.77
> train1.na[train1.na$titles=='Master',]$Age=4.57
> train1.na[train1.na$titles=='Mrs',]$Age=35.90
> train1.na[train1.na$titles=='Mr',]$Age=32.41
> train1.na[train1.na$titles=='other',]$Age=42.65

> names(train2.title)[13]='titles'
> train3=rbind(train2.title,train1.na)
> train4=train3[,c(2,3,5,6,7,8,10,12)]


#Replace missing age in “test2”
> titles=NULL
> for (i in 1:nrow(test2)){
+ titles=c(titles,extractTitle(test2[i,'Name']))
+ }
> table(titles)
titles
Master   Miss     Mr    Mrs  other 
    21     78    240     72      7 

> test2.title=data.frame(test2,titles)

> test2.na=test2.title[is.na(test2.title$Age),]
> test2.na[test2.na$titles=='Miss',]$Age=21.77
> test2.na[test2.na$titles=='Master',]$Age=4.57
> test2.na[test2.na$titles=='Mrs',]$Age=35.90
> test2.na[test2.na$titles=='Mr',]$Age=32.41
> test2.na[test2.na$titles=='other',]$Age=42.65

> test2.a=test2.title[complete.cases(test2.title$Age),]
> test3=rbind(test2.a,test2.na)

> test4=test3[,c(12,2,4,5,6,7,9,11)]
> levels(test4$Embarked)=c("","C","Q","S")





