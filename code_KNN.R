library(class) ## a library with lots of classification tools
library(kknn) ## knn library
setwd("C:/Ananya/Masters/Summer/Predictive Modelling/Predictive-Model-R-master")

data.raw <- read.csv("cleanedData.csv", header=T)
str(data.raw)
data.raw[sapply(data.raw, is.factor)] <- data.matrix(data.raw[sapply(data.raw, is.factor)])
str(data.raw)
attach(data.raw)
sum(data.raw$stroke == 0)
which.max(data.raw$age)
data.raw$age[85]
##########################################################################################
# all
train = data.frame(data.raw)
test =  data.frame(data.raw)

# k-fold cross validation  
n = nrow(train)
kcv = 10
n0 = round(n/kcv,0)
precision = matrix(0,kcv,7)
recall = matrix(0,kcv,7)
misclass = matrix(0,kcv,7)
set = 1:n
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  kk = c(100, 150, 200,250,300,400,505)
  for(i in 1:7){
    val00 = 0
    val01 = 0
    val10 = 0
    val11 = 0
    
    near <- kknn(train_i$stroke~., train_i, test_i, k=kk[i], kernel = "rectangular")
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] > 0.03){
        near$fitted[k] <- 1
      } 
      else {
        near$fitted[k] <- 0
      }
    }
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 0){
        val00 <- val00 + 1
      }
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 1){
        val11 <- val11 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 1){
        val10 <- val10 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 0){
        val01 <- val01 + 1
      }
      
    }
    p = val11 / (val11 + val01)
    r = val11 / (val11 + val10)
    m = (val10 + val01) / (nrow(test_i))
    precision[j,i] = p
    recall[j,i] = r
    misclass[j,i] = m
    
  }
}
cat ("all features \n")
cat("optimum k: ", kk[floor(which.max(recall) / 10) + 1], "\n")
cat("best recall: ", recall[which.max(recall)], "\n")
cat("best precision for best recall : ", precision[which.max(recall)], "\n")
cat("misclassification error: ", misclass[which.max(recall)], "\n")

##########################################################################################
# age and gender
train = data.frame(stroke, age, gender)
test =  data.frame(stroke, age, gender)

# k-fold cross validation  
n = nrow(train)
kcv = 10
n0 = round(n/kcv,0)
out_MSE = matrix(0,kcv,7)
precision = matrix(0,kcv,7)
recall = matrix(0,kcv,7)
misclass = matrix(0,kcv,7)
set = 1:n
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  kk = c(100, 150, 200,250,300,400,505)
  for(i in 1:7){
    val00 = 0
    val01 = 0
    val10 = 0
    val11 = 0
    
    near <- kknn(train_i$stroke~., train_i, test_i, k=kk[i], kernel = "rectangular")
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] > 0.03){
        near$fitted[k] <- 1
      } 
      else {
        near$fitted[k] <- 0
      }
    }
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 0){
        val00 <- val00 + 1
      }
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 1){
        val11 <- val11 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 1){
        val10 <- val10 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 0){
        val01 <- val01 + 1
      }
      
    }
    p = val11 / (val11 + val01)
    r = val11 / (val11 + val10)
    m = (val10 + val01) / (nrow(test_i))
    precision[j,i] = p
    recall[j,i] = r
    misclass[j,i] = m
    
  }
}
cat ("only age and gender \n")
cat("optimum k: ", kk[floor(which.max(recall) / 10) + 1], "\n")
cat("best recall: ", recall[which.max(recall)], "\n")
cat("best precision for best recall : ", precision[which.max(recall)], "\n")
cat("misclassification error: ", misclass[which.max(recall)], "\n")

######################################################################################################
# all medical
MSE_all_medical = NULL
kk = c(100, 150, 200,250,300,400,505)
train = data.frame(stroke, age, gender, hypertension, heart_disease, avg_glucose_level, bmi, smoking_status)
test =  data.frame(stroke, age, gender, hypertension, heart_disease, avg_glucose_level, bmi, smoking_status)

# k-fold cross validation  
n = nrow(train)
kcv = 10
n0 = round(n/kcv,0)
out_MSE = matrix(0,kcv,7)
precision = matrix(0,kcv,7)
recall = matrix(0,kcv,7)
misclass = matrix(0,kcv,7)
set = 1:n
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  kk = c(100, 150, 200,250,300,400,505)
  for(i in 1:7){
    val00 = 0
    val01 = 0
    val10 = 0
    val11 = 0
    
    near <- kknn(train_i$stroke~., train_i, test_i, k=kk[i], kernel = "rectangular")
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] > 0.03){
        near$fitted[k] <- 1
      } 
      else {
        near$fitted[k] <- 0
      }
    }
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 0){
        val00 <- val00 + 1
      }
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 1){
        val11 <- val11 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 1){
        val10 <- val10 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 0){
        val01 <- val01 + 1
      }
      
    }
    p = val11 / (val11 + val01)
    r = val11 / (val11 + val10)
    m = (val10 + val01) / (nrow(test_i))
    precision[j,i] = p
    recall[j,i] = r
    misclass[j,i] = m
    
  }
}
cat ("all medicalfeatures \n")
cat("optimum k: ", kk[floor(which.max(recall) / 10) + 1], "\n")
cat("best recall: ", recall[which.max(recall)], "\n")
cat("best precision for best recall : ", precision[which.max(recall)], "\n")
cat("misclassification error: ", misclass[which.max(recall)], "\n")


######################################################################################################
# all non - medical + bmi + smoking_status
MSE_all_medical = NULL
kk = c(100, 150, 200,250,300,400,505)
train = data.frame(stroke, ever_married, work_type, Residence_type, bmi, smoking_status)
test =  data.frame(stroke, ever_married, work_type, Residence_type, bmi, smoking_status)
set.seed(123)
# k-fold cross validation  
n = nrow(train)
kcv = 10
n0 = round(n/kcv,0)
out_MSE = matrix(0,kcv,7)
precision = matrix(0,kcv,7)
recall = matrix(0,kcv,7)
misclass = matrix(0,kcv,7)
set = 1:n
for(j in 1:kcv){
  
  if(n0<length(set)){val = sample(set,n0)}
  if(n0>=length(set)){val=set}
  
  train_i = train[-val,]
  test_i = test[val,]
  kk = c(100, 150, 200,250,300,400,505)
  for(i in 1:7){
    val00 = 0
    val01 = 0
    val10 = 0
    val11 = 0
    
    near <- kknn(train_i$stroke~., train_i, test_i, k=kk[i], kernel = "rectangular")
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] > 0.03){
        near$fitted[k] <- 1
      } 
      else {
        near$fitted[k] <- 0
      }
    }
    for(k in 1:nrow(test_i)){
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 0){
        val00 <- val00 + 1
      }
      if(near$fitted[k] == near$CL[k] &  test_i$stroke[k] == 1){
        val11 <- val11 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 1){
        val10 <- val10 + 1
      }
      if(near$fitted[k] != near$CL[k] &  test_i$stroke[k] == 0){
        val01 <- val01 + 1
      }
      
    }
    cat(val00, val11, val10, val01, "\n")
    p = val11 / (val11 + val01)
    r = val11 / (val11 + val10)
    m = (val10 + val01) / (nrow(test_i))
    precision[j,i] = p
    recall[j,i] = r
    misclass[j,i] = m
    
  }
}
cat ("all lifestyle features \n")
cat("optimum k: ", kk[floor(which.max(recall) / 10) + 1], "\n")
cat("best recall: ", recall[which.max(recall)], "\n")
cat("best precision for best recall : ", precision[which.max(recall)], "\n")
cat("misclassification error: ", misclass[which.max(recall)], "\n")

