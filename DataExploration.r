> train=read.csv('train.csv',header=T)
> train1=train
> train1$Pclass=as.factor(train1$Pclass)
> train1$Survived=as.factor(train1$Survived)

#Plot age vs survived with ggplot
> library(ggplot2)
> train2=train1[complete.cases(train1$Age),]
> ggplot(train2,aes(x=Age,fill=Survived))+geom_bar(binwidth=0.5,position='dodge')
> (train2,aes(x=Age,fill=Survived))+geom_density(alpha=0.2)

#Plot Pclass vs survived
> ggplot(train1,aes(x=Pclass,fill=Survived))+geom_bar(binwidth=0.5,position='dodge')

#Plot sex vs survived
> ggplot(train1,aes(x=Sex,fill=Survived))+geom_bar(binwidth=0.5,position='dodge')

#Use ggplot and table to explore SibSp and Parch
> ggplot(train1,aes(x=SibSp,fill=Survived))+geom_bar(binwidth=0.5)
> train1.table1=table(train1$Survived,train1$SibSp)
> barplot(prop.table(train1.table1,2))
> train1.table2=table(train1$Survived,train1$Parch)
> barplot(prop.table(train1.table2,2))

#Plot fare vs survived
> p1=ggplot(train1,aes(x=Fare,fill=Survived))+geom_bar()
> p2=ggplot(train1,aes(x=Fare,fill=Survived))+geom_density(alpha=0.5)
> multiplot(p1,p2,cols=2)

#First attempt in making models; remove NA from age in both train; 
use Pclass, Sex, Age, Sibsp, Parch, Fare, Embarked as features
> train3=train2[,c(2,3,5,6,7,8,10,12)]

#Clean up test data; remove NA from Age and Fare; change levels in Embarked
> test=read.csv('test.csv',header=T)
> Survived=rep('None',nrow(test))
> test1=data.frame(test,Survived)
> test2=test1
> test2$Pclass=as.factor(test2$Pclass)
> test3=test2[complete.cases(test2$Age),]
> test4=test3[complete.cases(test3$Fare),]
> test5=test4[,c(12,2,4,5,6,7,9,11)]
> levels(test5$Embarked)=c("","C","Q","S")

#Use randomforest to build models and make predictions
> library(randomForest)
> rf1=randomForest(Survived~.,data=train3,mtyr=2,importance=TRUE)
> pred1=predict(rf1,test5)
> final=data.frame(test4$PassengerId,pred1)
> final1=final[order(final$test3.PassengerId),]
> names(final1)=c('PassengerId','Survived')
> write.csv(final1, file = "submission1.csv", row.names = FALSE)

#For passenger with NA, the model cannot make a prediciton
> sum(is.na(pred2)) 

#Find one NA in test$Fare; fill with median fare price
> test2[is.na(test$Fare),] 
> summary(train3[train3$Pclass==3,]$Fare)
> test2[153,]$Fare=13.23

#With missing age, extract title from name and look for median age of each class of title






