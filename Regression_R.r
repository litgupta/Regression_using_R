#Step 1: reading data
df=read.csv('~/Desktop/coding/learning_R/new_data/insurance.csv')
head(df)

str(df)

#Step 2: cleaning data
#Step 2.1: removing NA values and replacing categorical data with continuous data
clean<-function(df){
  columns=colnames(df)
  for( col in columns){
    if(any(is.na(df[,col]))){
      if(class(df[,col]) %in% c('numeric','integer')){
        df[,col]=ifelse(is.na(df[,col],mean(df[,col],na.rm=TRUE),df[,col]))
      }else{
        df[,col]=iflse(is.na(df[,col]),'none',df[,col])
      }
    }else if(class(df[,col])=='factor'){
      df[,col]=factor(df[,col],levels=unique(df[,col]),labels=1:nlevels(df[,col]))
      df[,col]=as.integer(df[,col])
    }
  }
  return(df)
}

df=clean(df)
head(df)

#Step 2.2 taking only the age and smoking status in consideration
df=df[,c('age','smoker','expenses')]

#splitting the data into training and testing set
library(caTools)
result=sample.split(df,SplitRatio = 0.8)
train_set=df[result==TRUE,]
test_set=df[result==FALSE,]

#making the regressor

#linear regressor to test the covariance
lin_regressor=lm(formula=expenses ~ . ,data=train_set)
summary(lin_regressor)
y_pred=predict(lin_regressor,newdata = test_set)

#Support vector regressor
library(e1071)
svm_regressor=svm(formula=expenses ~ .,data=train_set,type='eps-regression')
svm_pred=predict(svm_regressor,newdata=test_set)

#decision tree regressor
library(rpart)
decision_regressor=rpart(formula=expenses ~ ., data=train_set,control=rpart.control(minsplit = 100))
dt_pred=predict(decision_regressor,newdata=test_set)

#RandomForest
library(randomForest)
random_forest_regressor=randomForest(formula=expenses~.,data=train_set,ntree=100)
random_forest_pred=predict(random_forest_regressor,newdata=test_set)

#visualization
library(ggplot2)

#linear_regressor
ggplot()+
  geom_point(aes(x=train_set$age,y=train_set$expenses),data=train_set,color='red')+
  geom_line(aes(x=train_set$age,y=predict(lin_regressor,train_set)),data=train_set)

#svm plotting
ggplot()+
  geom_point(aes(x=train_set$age,y=train_set$expenses),data=train_set,color='red')+
  geom_line(aes(x=train_set$age,y=predict(svm_regressor,train_set)),data=train_set,color='blue')

#decison tree plotting
ggplot()+
  geom_point(aes(x=train_set$age,y=train_set$expenses),data=train_set,color='red')+
  geom_line(aes(x=train_set$age,y=predict(decision_regressor,train_set)),data=train_set)

#Random Forest plotting
ggplot()+
  geom_point(aes(x=train_set$age,y=train_set$expenses),data=train_set,color='red')+
  geom_line(aes(x=train_set$age,y=predict(random_forest_regressor,train_set)),data=train_set,color='green')

#age v/s expenses
ggplot(df, aes(age, fill = expenses)) + 
  geom_histogram(bins=30,color='red') + 
  xlab("Age") +
  scale_fill_discrete(name = "expenses") + 
  ggtitle("Age vs expenses")

#prediction for manual data 
predict(svm_regressor,newdata = data.frame(age=c(18,20,22,24,30),smoker=c(1,0,1,1,0)))

