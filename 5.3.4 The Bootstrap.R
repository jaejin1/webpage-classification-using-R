library(ISLR)

alpha.fn = function(data, index){
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X) + var(Y)-2*cov(X,Y)))
  # Bootstrap 공식 리턴하는 거임 
  # return a^
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(100,100, replace=T))

boot(Portfolio, alpha.fn, R=1000) # alpha.fn란 함수를 1000번 반복한다.


boot.fn = function(data, index)
  return(coef(lm(mpg~horsepower, data = data, subset=index)))

boot.fn(Auto, 1:392)

set.seed(1)

boot.fn(Auto, sample(392,392, replace=T))

boot.fn(Auto, sample(392,392, replace=T))

boot(Auto , boot.fn,1000)

summary(lm(mpg~horsepower , data=Auto))$coef  #coef만 보여주기 

boot.fn = function(data,index)
  coefficients(lm(mpg~horsepower + I(horsepower^2), data=data, subset = index))  
set.seed(1)
boot(Auto, boot.fn,1000)

summary(lm(mpg~horsepower + I(horsepower^2) , data=Auto))$coef
