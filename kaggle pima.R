diabetes = read.csv(file.choose())
head(diabetes)
summary(diabetes)
str(diabetes)
#Removing zero values from the data 
clean_data = diabetes[!(apply(diabetes[2:6],1,function(row) any(row==0))),]
str(clean_data)
names(clean_data)
#converting everything to lower case
names(clean_data)=tolower(names(clean_data))
names(clean_data)
#renaming DiabetesPedigreeFunction to dpf for easy identification
names(clean_data)[7]="dpf"
#renaming "outcome" column to "diabetes?"
names(clean_data)[9]="diabetes?"
#checking the new names
names(clean_data)
# now of rows present in intial diabetes data and clean data
NROW(diabetes)
NROW(clean_data)
#histogram for better understanding
par(mfrow=c(2,2))
hist(clean_data$pregnancies)
hist(clean_data$glucose)
hist(clean_data$bloodpressure)
hist(clean_data$bmi)
hist(clean_data$insulin)
hist(clean_data$skinthickness)
hist(clean_data$dpf)
hist(clean_data$age)
hist(clean_data$'diabetes?')
#ploting relation to find the significance of variable
cor(clean_data)
#svn model
library(caret)
library(e1071)
set.seed(2)
split=sample(nrow(clean_data),nrow(clean_data)*0.85)
train=clean_data[split,]
test=clean_data[-split,]
set.seed(7)
fit=svm(factor(train$`diabetes?`) ~ .,data=train,probability=T)
pre= predict(fit,test,decision.values = T,probability = T)
confusionMatrix(factor(test$`diabetes?`),factor(pre))

#naive bayes classifier
set.seed(5)
train$`diabetes?`=as.factor(train$`diabetes?`)
naive_model=naiveBayes(train$`diabetes?` ~ .,data = train)
naive_pre=predict(naive_model , test)
confusionMatrix(test$`diabetes?`,naive_pre)

#neural network
library('neuralnet')
split <- sample(nrow(clean_data), nrow(clean_data)*0.85)
train <- clean_data[split,]
test <- clean_data[-split,]
neural_pre = neuralnet(train$'diabetes?' ~pregnancies+glucose+bloodpressure+skinthickness+insulin+bmi+dpf+age,train,hidden=50,lifesign="minimal",linear.output=FALSE,threshold = 0.01)
plot(neural_pre,rep = "best")
predict_set <- neuralnet::compute(neural_pre,test_temp)