#Build models with missing data. Then fill in missing data points.

train=read.csv('train.csv',header=T)
train1=train
train1$Pclass=as.factor(train1$Pclass)
train1$Survived=as.factor(train1$Survived)

#Remove missing Age
train2=train1[complete.cases(train1$Age),]

#Keep relevant features(PClass,Age,Sex,SibSp,Parch,Fare,Embarked)
train3=train2[,c(2,3,5,6,7,8,10,12)]
names(train3)
[1] "Survived" "Pclass"   "Sex"      "Age"      "SibSp"    "Parch"   
[7] "Fare"     "Embarked"

#Get test data
test=read.csv('test.csv',header=T)
dim(test)
[1] 418  11
names(test)
 [1] "PassengerId" "Pclass"      "Name"        "Sex"         "Age"        
 [6] "SibSp"       "Parch"       "Ticket"      "Fare"        "Cabin"      
[11] "Embarked"   

#Change Pclass to factor. "test" has missing data in Age and Fare.
test1=test
test1$Pclass=as.factor(test1$Pclass)
sum(is.na(test1$Age))
[1] 86
sum(is.na(test1$Fare))
[1] 1

#Add a column "Survived" as "test2"
Survived=rep('None',nrow(test1))
test2=data.frame(test1,Survived)
dim(test2)
[1] 418  12

#Take the revelant features as "test3"
test3=test2[,c(12,2,4,5,6,7,9,11)]
names(test3)
[1] "Survived" "Pclass"   "Sex"      "Age"      "SibSp"    "Parch"   
[7] "Fare"     "Embarked"

#"test" has three levels for "Embarked".Change that to four levels.
levels(test3$Embarked)
[1] "C" "Q" "S"
levels(test3$Embarked)=c("","C","Q","S")

#Build randomForest model with "train3" and predict on "test3"
library(randomForest)
rf1=randomForest(Survived~.,data=train3,mtyr=2,importance=TRUE)
pred1=predict(rf1,test3)

#Combine "pred1" with "PassengerId"
final=data.frame(test2$PassengerId,pred1)
dim(final)
[1] 418   2
sum(is.na(final))
[1] 87

#But records with missing data cannot be predicted

#Fill in missing fare 
test2[is.na(test$Fare),]
    PassengerId Pclass               Name  Sex  Age SibSp Parch Ticket
153        1044      3 Storey, Mr. Thomas male 60.5     0     0   3701
    Fare Cabin Embarked Survived
153   NA              S     None
#This passenger is in Pclass=3.Fill in missing Fare with median price from train$Fare where Pclass=3.
summary(train1[train1$Pclass==3,]$Fare)

test2[153,]$Fare=8.05

#To fill in missing Age, use Title from name
extractTitle=function(name){
+ if (length(grep('Miss',name))>0){return ('Miss')}
+ if (length(grep('Master',name))>0){return ('Master')}
+ if (length(grep('Mrs',name))>0){return ('Mrs')}
+ if (length(grep('Mr',name))>0){return ('Mr')}
+ else {return ('other')}
+ }

#Use median age to fill in missingAge in "train1" to get "trainComplete"
Title=NULL
for (i in 1:nrow(train1)){
+ Title=c(Title,extractTitle(train1[i,'Name']))
+ }
table(Title)
Title
Master   Miss     Mr    Mrs  other 
    40    182    518    127     24 
train1.title=data.frame(train1,Title)
trainNA=train1.title[is.na(train1.title$Age),]
summary(train1.title[train1.title$Title=='Miss',]$Age)
trainNA[trainNA$Title=='Miss',]$Age=21.00
summary(train1.title[train1.title$Title=='Master',]$Age)
trainNA[trainNA$Title=='Master',]$Age=3.500
summary(train1.title[train1.title$Title=='Mrs',]$Age)
trainNA[trainNA$Title=='Mrs',]$Age=35.00
summary(train1.title[train1.title$Title=='Mr',]$Age)
trainNA[trainNA$Title=='Mr',]$Age=30.00
summary(train1.title[train1.title$Title=='other',]$Age)
trainNA[trainNA$Title=='other',]$Age=44.00
trainA=train1.title[complete.cases(train1.title$Age),]
trainComplete=rbind(trainA,trainNA)
sum(is.na(trainComplete$Age))
[1] 0
