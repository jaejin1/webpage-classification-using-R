# LOOCV

library(ISLR)
glm.fit = glm(mpg~horsepower, data=Auto)
coef(glm.fit)

library(boot) # cv.glm을 사용하려고 
glm.fit = glm(mpg~horsepower, data=Auto)

cv.err = cv.glm(Auto,glm.fit) #LOOCV실행 
cv.err$delta # delta는 cross-validation 결과들담고있다.

cv.error = rep(0,5)
for(i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i), data=Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

plot(1:5,cv.error, type="b")
