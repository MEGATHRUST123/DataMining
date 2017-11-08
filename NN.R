# Preparing the data
setwd("C:/Users/kell0021/Desktop")
s=read.csv("final.csv",header=T)
s=s[,-1]

# Neural Netork Packages
install.packages("nnet")
# install.packages("ggplot2")
library(nnet)


# Neural Network code

nn.rep <- function(rep, ...) { 
v.min <- Inf # initialize v.min
for (r in 1:rep) { # repeat nnet
nn.temp <- nnet(...) # fit the first nnet
v.temp <- nn.temp$value # store the cost
if (v.temp < v.min) { # choose better weights
v.min <- v.temp
nn.min <- nn.temp
}
}
return(nn.min)
}

# Deterimine the ideal parameters for neural network
set.seed(4510)
training <- sample(nrow(s), 0.9 * nrow(s))

# Randomise the data 
shuffle<-sample(nrow(s))
s<-s[shuffle,]

s.train=s[training,]
s.test=s[-training,]
index=which(colnames(s.train)=="price")

# create 10 folds 
K<-10
folds<- cut(seq(1,nrow(s.train)),breaks=K,labels=FALSE)

# Matrices to store training and validation errors
costl=matrix(0,5,10)
trainl=matrix(0,5,10) 
x=seq(0,5)


for(k in 1:K){

test <- which(folds==k)

# Standardise the training data
s.mean=apply(s.train[-test,-index],2,mean)
s.sd=apply(s.train[-test,-index],2,sd)
s.scaled=data.frame(scale(x =s.train[-index], 
                    center =s.mean,
                    scale=s.sd),
                    price =s.train[index])

sydney.train=s.scaled[-test,]
sydney.test=s.scaled[test,]

####################### Hidden layer 1
sydney.nn.repl <- nn.rep(rep = 200, price~., data = sydney.train,
size = 1, linout = TRUE, trace = FALSE)

# Validation Error
sydney.nn.pred_testl <- predict(sydney.nn.repl, sydney.test)
cost_long[1,k]=sum((sydney.nn.pred_testl - sydney.test$price) ^ 2)/nrow(sydney.train)

# Training Error
sydney.nn.pred_trainl <- predict(sydney.nn.repl, sydney.train)
train_long[1,k]=sum((sydney.nn.pred_trainl - sydney.train$price) ^ 2)/nrow(sydney.train)
print(1)

########################## Hidden layer 2 
sydney.nn.repl <- nn.rep(rep = 200, price~., data = sydney.train,
size = 2, linout = TRUE, trace = FALSE)

# Validation Error
sydney.nn.pred_testl <- predict(sydney.nn.repl, sydney.test)
cost_long[2,k]=sum((sydney.nn.pred_testl - sydney.test$price) ^ 2)/nrow(sydney.train)

# Training Error
sydney.nn.pred_trainl <- predict(sydney.nn.repl, sydney.train)
train_long[2,k]=sum((sydney.nn.pred_trainl - sydney.train$price) ^ 2)/nrow(sydney.train)

print(2)
########################## Hidden layer 3 
sydney.nn.repl <- nn.rep(rep = 200, price~., data = sydney.train,
size = 3, linout = TRUE, trace = FALSE)

# Validation Error
sydney.nn.pred_testl <- predict(sydney.nn.repl, sydney.test)
cost_long[3,k]=sum((sydney.nn.pred_testl - sydney.test$price) ^ 2)/nrow(sydney.train)

# Training Error
sydney.nn.pred_trainl <- predict(sydney.nn.repl, sydney.train)
train_long[3,k]=sum((sydney.nn.pred_trainl - sydney.train$price) ^ 2)/nrow(sydney.train)

print(3)
########################## Hidden layer 4 
sydney.nn.repl <- nn.rep(rep = 200, price~., data = sydney.train,
size = 4, linout = TRUE, trace = FALSE)

# Validation Error
sydney.nn.pred_testl <- predict(sydney.nn.repl, sydney.test)
cost_long[4,k]=sum((sydney.nn.pred_testl - sydney.test$price) ^ 2)/nrow(sydney.train)

# Training Error
sydney.nn.pred_trainl <- predict(sydney.nn.repl, sydney.train)
train_long[4,k]=sum((sydney.nn.pred_trainl - sydney.train$price) ^ 2)/nrow(sydney.train)
print(4)
########################## Hidden layer 5 
sydney.nn.repl <- nn.rep(rep = 200, price~., data = sydney.train,
size = 5, linout = TRUE, trace = FALSE)

# Validation Error
sydney.nn.pred_testl <- predict(sydney.nn.repl, sydney.test)
cost_long[5,k]=sum((sydney.nn.pred_testl - sydney.test$price) ^ 2)/nrow(sydney.train)

# Training Error
sydney.nn.pred_trainl <- predict(sydney.nn.repl, sydney.train)
train_long[5,k]=sum((sydney.nn.pred_trainl - sydney.train$price) ^ 2)/nrow(sydney.train)

print(5)


}


# Plot training and MSE
MSE_l=apply(cost_long,1,mean)
train_l=apply(train_long,1,mean)

# MSE vs Training error for short model
x=seq(1,4)
plot(x,MSE_l,type="b",col="blue",title="Validation Error versus Training Error",ylab="MSE",xlab="Hidden Layers")
par(new=T)
plot(x,train_l,type="b",col="red",axes=FALSE,ylab=" ",xlab=" ")
legend("topright", inset=.01, title="Errors",
  	c("Validation","Training"), fill=c("blue","red"), horiz=TRUE)

# Therefore our model will use two hidden layers 

