#Look at data for relevant features and missing data

train=read.csv('train.csv',header=T)
dim(train)
[1] 891  12
names(train)
[1] "PassengerId" "Survived"    "Pclass"      "Name"        "Sex"        
 [6] "Age"         "SibSp"       "Parch"       "Ticket"      "Fare"       
[11] "Cabin"       "Embarked" 
str(train)

#Copy original data and change Pclass and Surivived to factor
train1=train
train1$Pclass=as.factor(train1$Pclass)
train1$Survived=as.factor(train1$Survived)

#Some data in Age is missing
sum(is.na(train1$Age))
[1] 177
train2=train1[complete.cases(train1$Age),]
dim(train2)
[1] 714  12
dim(train1)
[1] 891  12

#Plot Age vs surivived,in barplot and density plot
#Conclude Age is relevant in predicting survival
library(ggplot2)
ggplot(train2,aes(x=Age,fill=Survived))+geom_bar(binwidth=0.5,position='dodge')
ggplot(train2,aes(x=Age,fill=Survived))+geom_density(alpha=0.2)

#Plot Pclass vs survived
#Conclude Pclass is relevant in predicting survival.Pclass=1 or 2 survived more than Pclass=3.
sum(is.na(train1$Pclass))
[1] 0
ggplot(train1,aes(x=Pclass,fill=Survived))+geom_bar(binwidth=0.5,position='dodge')

#Plot Sex vs Survived
#Conclude Sex is relevant in predicting survival.Female survived more than male.
sum(is.na(train1$Sex))
[1] 0
ggplot(train1,aes(x=Sex,fill=Survived))+geom_bar(binwidth=0.5,position='dodge')

#Explore SibSp vs Survived with ggplot,table,and barplot
#Conclude SibSp is relevant in predicting survival.Having 1 or 2 SibSp improved survival. Having >2SibSp, especially>4SibSp, decreased chance of survival.
sum(is.na(train1$SibSp))
[1] 0
ggplot(train1,aes(x=SibSp,fill=Survived))+geom_bar(binwidth=0.5)
#Low end of the barplot has few data points. Use table and barplot to have a better look.
table(train1$SibSp,train1$Survived)
   
      0   1
  0 398 210
  1  97 112
  2  15  13
  3  12   4
  4  15   3
  5   5   0
  8   7   0
barplot(prop.table(table(train1$Survived,train1$SibSp),2))

#Do the same for Parch
#Conclude Parch is relevant for predicting survial,in a similar trend as SibSp.Having 1 or 2 Parch improved survival. Having >3Parch decreased chance of survival.
sum(is.na(train1$Parch))
[1] 0
ggplot(train1,aes(x=Parch,fill=Survived))+geom_bar(binwidth=0.5)
table(train1$Parch,train1$Survived)
   
      0   1
  0 445 233
  1  53  65
  2  40  40
  3   2   3
  4   4   0
  5   4   1
  6   1   0
barplot(prop.table(table(train1$Survived,train1$Parch),2))

#Plot Fare vs Survived
#Conclude that Fare is relevant to survival.Higher fare improved survival.
sum(is.na(train1$Fare))
[1] 0
ggplot(train1,aes(x=Fare,fill=Survived))+geom_bar()
ggplot(train1,aes(x=Fare,fill=Survived))+geom_density(alpha=0.5)
#To have a better look at fare<100, where most data points concentrate
ggplot(train1[train1$Fare<100,],aes(x=Fare,fill=Survived))+geom_bar()
ggplot(train1[train1$Fare<100,],aes(x=Fare,fill=Survived))+ geom_density(alpha=0.5)

#Both "Ticket" and "Cabin" have too many levels. I didn't include them as relevant features.

#Use table and plot to look at "Embark".Conclude it is relevant.
#Notice there are four levels of factors, with one empty level.
sum(is.na(train1$Embark))
[1] 0
table(train1$Embark,train1$Survived)
   
      0   1
      0   2
  C  75  93
  Q  47  30
  S 427 217
barplot(prop.table(table(train1$Survived,train1$Embark),2))

