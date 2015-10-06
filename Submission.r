
#Use train4 and test4 for modeling, and refer to previous files
Use Pclass, Sex, Age, SibSp, Parch, Fare, and Embarked as features for prediction
For missing Age in both train and test, extract title from name and use the medium
age from each title class to fill in the missing age



> library(randomForest)
> rf1=randomForest(Survived~.,data=train4,mtyr=2,importance=TRUE)
> pred=predict(rf1,test4)
> final=data.frame(test3$PassengerId,pred)
> final1=final[order(final$test3.PassengerId),]
> names(final1)=c('PassengerId','Survived')
> write.csv(final1, file = "TitanicSubmission.csv", row.names = FALSE)
