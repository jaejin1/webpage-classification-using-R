library(ISLR)
set.seed(1)
train = sample(392,196)  # 392까지의 수중에 196개를 추출
train

lm.fit = lm(mpg~horsepower, data=Auto, subset=train)
# ~ 는 예측 변수   mpg를 horsepower가지고 예측한다. subset은 부분집합만사용함
#coef(lm.fit)
# lm.fit 에서는 train 196개 추출한거 사용하고 그 밑에는 -train해서 뺀것들만 사용 
attach(Auto)
mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train)
# horsepower 의 2차 다항식에대해서 
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[-train]^2)
# seed 1일때 -----------------

set.seed(2)
train = sample(392,196)

lm.fit = lm(mpg~horsepower, subset = train)

mean((mpg-predict(lm.fit, Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2), data=Auto, subset=train)
# horsepower 의 2차 다항식에대해서 
mean((mpg-predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower,3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[-train]^2)
# seed 2일때 ----------------- 2차식이 결과가 좋음.
