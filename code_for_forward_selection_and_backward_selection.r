
data<-clean.data[2:(length(clean.data)-1)]

#forward selection
set.seed(1)
train=sample(c(TRUE,FALSE),nrow(data),rep=TRUE)
test = (!train)

library(leaps)

regfit.best=regsubsets(new.cases.per.day~., data=data[train,], nvmax=50, method="forward")
test.mat = model.matrix(new.cases.per.day~., data=data[test,])



val.errors = rep(NA, 50)
for (i in 1:50){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((data$new.cases.per.day[test]-pred)^2)
}


val.errors
which.min(val.errors)

coef(regfit.best,6)



coefi = coef(regfit.best, id = 6)
pred = test.mat[,names(coefi)]%*%coefi
RMSE=val.errors[6]=sqrt(mean((10^(data$new.cases.per.day[test])-10^pred)^2))
cat(RMSE)

MAE=val.errors[6]=mean(abs(10^(data$new.cases.per.day[test])-10^pred))
cat(MAE)

MAPE=val.errors[6]=mean(abs(10^pred/10^(data$new.cases.per.day[test])-1))
cat(MAPE)

cor(10^(data$new.cases.per.day[test]),10^pred,use='everything',method='pearson')

#backward selection

set.seed(2)
train=sample(c(TRUE,FALSE),nrow(data),rep=TRUE)
test = (!train)

library(leaps)


regfit.best=regsubsets(new.cases.per.day~., data=data[train,], nvmax=50, method="backward")
test.mat = model.matrix(new.cases.per.day~., data=data[test,])

val.errors.backward = rep(NA, 50)
for (i in 1:50){
  coefi = coef(regfit.best, id = i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors.backward[i]=mean((data$new.cases.per.day[test]-pred)^2)
}


val.errors.backward
which.min(val.errors.backward)

coef(regfit.best,9)

coefi = coef(regfit.best, id = 9)
pred = test.mat[,names(coefi)]%*%coefi
RMSE=val.errors.backward[9]=sqrt(mean((10^(data$new.cases.per.day[test])-10^pred)^2))
cat(RMSE)

MAE=val.errors[9]=mean(abs(10^(data$new.cases.per.day[test])-10^pred))
cat(MAE)

MAPE=val.errors[9]=mean(abs(10^pred/10^(data$new.cases.per.day[test])-1))
cat(MAPE)


cor(10^(data$new.cases.per.day[test]),10^pred,use='everything',method='pearson')


