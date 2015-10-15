#Fill in missing Age for "train" and "test"
#Use randomForest for modeling and prediction

train=read.csv('train.csv',header=T)
train1=train
train1$Pclass=as.factor(train1$Pclass)
train1$Survived=as.factor(train1$Survived)

#Fill in missing age in training data
extractTitle=function(name){
+ if (length(grep('Miss',name))>0){return ('Miss')}
+ if (length(grep('Master',name))>0){return ('Master')}
+ if (length(grep('Mrs',name))>0){return ('Mrs')}
+ if (length(grep('Mr',name))>0){return ('Mr')}
+ else {return ('other')}
+ }
Title=NULL
for (i in 1:nrow(train1)){
+ Title=c(Title,extractTitle(train1[i,'Name']))
+ }
train1.title=data.frame(train1,Title)
trainNA=train1.title[is.na(train1.title$Age),]
trainNA[trainNA$Title=='Miss',]$Age=21.00
trainNA[trainNA$Title=='Master',]$Age=3.500
trainNA[trainNA$Title=='Mrs',]$Age=35.00
trainNA[trainNA$Title=='Mr',]$Age=30.00
trainNA[trainNA$Title=='other',]$Age=44.00
trainA=train1.title[complete.cases(train1.title$Age),]
trainComplete=rbind(trainA,trainNA)
trainFinal=trainComplete[,c(2,3,5,6,7,8,10,12)]

#Fill in missing Age and Fare in test data
test=read.csv('test.csv',header=T)
test1=test
test1$Pclass=as.factor(test1$Pclass)
Survived=rep('None',nrow(test1))
test2=data.frame(test1,Survived)

test2[153,]$Fare=8.05

Title=NULL
> for (i in 1:nrow(test2)){
+ Title=c(Title,extractTitle(test2[i,'Name']))
+ }
test2.title=data.frame(test2,Title)
test2.na=test2.title[is.na(test2.title$Age),]
test2.na[test2.na$Title=='Miss',]$Age=21.00
test2.na[test2.na$Title=='Master',]$Age=3.500
test2.na[test2.na$Title=='Mrs',]$Age=35.00
test2.na[test2.na$Title=='Mr',]$Age=30.00
test2.na[test2.na$Title=='other',]$Age=44.00
test2.a=test2.title[complete.cases(test2.title$Age),]
test2.complete=rbind(test2.a,test2.na)
levels(test2.complete$Embarked)=c("","C","Q","S")
testFinal=test2.complete[,c(12,2,4,5,6,7,9,11)]

#Final model with "trainFinal" and "testFinal"
library(randomForest)
rf=randomForest(Survived~.,data=trainFinal,mtyr=2,importance=TRUE)
pred=predict(rf,testFinal)
final=data.frame(test2.complete$PassengerId,pred)
final1=final[order(final$test2.complete.PassengerId),]
names(final1)=c('PassengerId','Survived')
write.csv(final1,file='submission.csv',row.names=FALSE)



