rm(list=ls()) # 데이터 지우기 

#install.packages("glmnet")
#install.packages("Matrix")
#install.packages("foreach")

############################## library
library(xml2)
library(rvest)
library(MASS)
library(ISLR) 
library(mlbench)
library(gplots) # ROC가져오기위해 그래프 
library(ROCR)   # ROC
library(class)  # KNN
library(scales) # 그래프 
library(foreach)
library(Matrix) # glmnet 가져올려면 필요함
library(glmnet) # LASSO
library(e1071)  # SVM

############################## 데이터 입력때 쓰일 아스키코드 변환 함수

asc <- function(x) { strtoi(charToRaw(x),16L) }   # 아스키코드 함수 
chr <- function(n) { rawToChar(as.raw(n)) }

############################## 초기화

data(Sonar)
y_test = rep(1, nrow(tag))
y = rep(1, nrow(tag))

a1 <- read.table("/Users/jaejin/dev/R_pratice/practice/WebpageDataset/A0001.txt", sep = "=", header =F, nrows=6)
# transpose : t()  row, colum 바꿔줌 
header = as.data.frame(test_set(a1))
names(header) <- c("ID", "URL", "SIZE", "DATE", "TIME", "DATASET")

header
#remove col1
header <- header[c(2),]  # 첫번째 row 없애버림 

rownames(header) <- NULL  # rowname 바꿈
header

# 데이터 추가 할꺼 있음 추가하기 
tag_class <- data.frame(class = c(1))
tag1 <- data.frame(b = c(1))  # b태그 
tag2 <- data.frame(img = c(2)) # img태그 
tag3 <- data.frame(a = c(1))  # a태그
tag4 <- data.frame(option = c(1))  # option태그
tag5 <- data.frame(form = c(1))  # form태그
tag6 <- data.frame(p = c(1))  # p태그
tag7 <- data.frame(i = c(1))  # i태그
tag8 <- data.frame(table = c(1))  # table태그
tag9 <- data.frame(td = c(1))  # td태그
tag10 <- data.frame(tr = c(1))  # tr태그
tag <- cbind(tag_class ,tag1, tag2, tag3, tag4, tag5, tag6, tag7, tag8, tag9, tag10)

############################## 초기화 끝



addzero <- function(a){
  if(a < 10){
    b <- paste("000",a, sep="")
    return(b)
  }else if(a < 100){
    b <- paste("00", a, sep="")
    return(b)
  }else if(a < 1000){
    b <- paste("0", a, sep="")
    return(b)
  }else{
    b <- a
    return(b)
  }
}

a_str <- "A"
a_str2 <- "X" 
a_str3 <- "A"


for(i in 1:(asc(a_str2) - asc(a_str) + 1)){
  if(asc(a_str3) >= 75 && asc(a_str3) < 88){  # K 부터 W까지 데이터가 없으므로 
    a_str3 <- chr((asc(a_str3)+1))  # 다음 알파벳 으로 바꾸기
  }else{
    for(i in 1:1000){
      #print(paste(a_str3,addzero(i),sep="")) # paste(a_str3,addzero2(i),sep="")이걸 파일 명으로 주면됨 .
      # 여기에 작업하면 될듯. 모든데이터 
      #data <- read_html(paste("/Users/jaejin/dev/R_pratice/practice/WebpageDataset/",paste(a_str3,addzero(i),sep=""),".txt", sep=""), encoding = "UTF-8")
      a1 <- read.table(paste("/Users/jaejin/dev/R_pratice/practice/WebpageDataset/",paste(a_str3,addzero(i),sep=""),".txt", sep="") , sep="=", header=F, nrows=6, fill = T)  
      a1_t = as.data.frame(t(a1))
      names(a1_t) <- c("ID", "URL", "SIZE", "DATE", "TIME", "DATASET")
      #remove col1
      
      a1_t <- a1_t[c(2),]  # 첫번째 row 없애버림 
      rownames(a1_t) <- NULL  # rowname 바꿈
      
      header <- rbind(header, a1_t)
      
      ########## 추가 데이터
      frozen <- read_html(paste("/Users/jaejin/dev/R_pratice/practice/WebpageDataset/",paste(a_str3,addzero(i),sep=""),".txt", sep=""), options = "HUGE")
      
      
      #태그 class
      class <- c("class")
      tag_class_data <- data.frame(test = c(a_str3))
      names(tag_class_data) <- c(class)   # 이름 바꾸기
      
      #b태그
      tag_b <- html_nodes(frozen, "b")
      tag_b_count <- length(tag_b)
      data1 <- c("b")
      tag_b_data <- data.frame(test = c(tag_b_count))
      names(tag_b_data) <- c(data1)   # 이름 바꾸기 
      
      #img태그 
      tag_img <- html_nodes(frozen, "img")
      tag_img_count <- length(tag_img)
      data2 <- c("img")
      tag_img_data <- data.frame(test = c(tag_img_count))
      names(tag_img_data) <- c(data2)   # 이름 바꾸기 
      
      #a태그
      tag_a <- html_nodes(frozen, "a")
      tag_a_count <- length(tag_a)
      data1 <- c("a")
      tag_a_data <- data.frame(test = c(tag_a_count))
      names(tag_a_data) <- c(data1)   # 이름 바꾸기 
      
      #option태그
      tag_option <- html_nodes(frozen, "option")
      tag_option_count <- length(tag_option)
      data1 <- c("option")
      tag_option_data <- data.frame(test = c(tag_option_count))
      names(tag_option_data) <- c(data1)   # 이름 바꾸기 
      
      #form태그
      tag_form <- html_nodes(frozen, "form")
      tag_form_count <- length(tag_form)
      data1 <- c("form")
      tag_form_data <- data.frame(test = c(tag_form_count))
      names(tag_form_data) <- c(data1)   # 이름 바꾸기 
      
      #p태그
      tag_p <- html_nodes(frozen, "p")
      tag_p_count <- length(tag_p)
      data1 <- c("p")
      tag_p_data <- data.frame(test = c(tag_p_count))
      names(tag_p_data) <- c(data1)   # 이름 바꾸기 
      
      #i태그
      tag_i <- html_nodes(frozen, "i")
      tag_i_count <- length(tag_i)
      data1 <- c("i")
      tag_i_data <- data.frame(test = c(tag_i_count))
      names(tag_i_data) <- c(data1)   # 이름 바꾸기 
      
      #table태그
      tag_table <- html_nodes(frozen, "table")
      tag_table_count <- length(tag_table)
      data1 <- c("table")
      tag_table_data <- data.frame(test = c(tag_table_count))
      names(tag_table_data) <- c(data1)   # 이름 바꾸기 
      
      #td태그
      tag_td <- html_nodes(frozen, "td")
      tag_td_count <- length(tag_td)
      data1 <- c("td")
      tag_td_data <- data.frame(test = c(tag_td_count))
      names(tag_td_data) <- c(data1)   # 이름 바꾸기 
      
      #tr태그
      tag_tr <- html_nodes(frozen, "tr")
      tag_tr_count <- length(tag_tr)
      data1 <- c("tr")
      tag_tr_data <- data.frame(test = c(tag_tr_count))
      names(tag_tr_data) <- c(data1)   # 이름 바꾸기 
      
      table <- cbind(tag_class_data, tag_b_data, tag_img_data, tag_a_data,tag_option_data,tag_form_data,tag_p_data,tag_i_data,tag_table_data,tag_td_data,tag_tr_data)
      tag <- rbind(tag, table)
      
      ##########
      
      if(i == 1000){
        a_str3 <- chr((asc(a_str3)+1))  # 다음 알파벳 으로 바꾸기 
        
      }
    }
  }
  #if((asc(a_str2) - asc(a_str) + 1)){
  #  a12_t <- a12_t[c(2),]  # 첫번째 row 없애버림 
  #  a12_test <- a12_test[c(2),]  # 첫번째 row 없애버림 
  #}
}


header
tag

### 초기화 했던 데이터 제거 
header <- header[-1,]
rownames(header) <- NULL
tag <- tag[-1,]
rownames(tag) <- NULL  

# train, valid, test data 나누기 
idx <- sample(x = c("train", "valid", "test"),
              size = nrow(tag),
              replace = TRUE,
              prob = c(3,1,1))
train_set <- tag[idx == "train",]
valid_set <- tag[idx == "valid",]
test_set <- tag[idx == "test",]

# LASSO 데이터 구할때 쓰려고 시간이 오래걸려서 subset만듬
idx2 <- sample(x = c("train_sub", "test_sub", "aa"),
               size = nrow(train_set),
               replace = TRUE,
               prob = c(3,1,4))

train_sub <- train_set[idx2 == "train_sub",]
test_sub <- train_set[idx2 == "test_sub",]

train_sub_x <- train_sub[,-1]
test_sub_x <- test_sub[,-1]

train_sub_y <- train_sub[,1]
test_sub_y <- test_sub[,1]

# train valid test 데이터들  x와 y로 나누기 
train_set_x <- train_set[,-1]
valid_set_x <- valid_set[,-1]
test_set_x <- test_set[,-1]

train_set_y <- train_set[,1]
valid_set_y <- valid_set[,1]
test_set_y <- test_set[,1]

# 숫자로 나타낸 데이터가 필요하므로
y[which(train_set_y == "A")] = 0 # A
y[which(train_set_y == "B")] = 1 # B
y[which(train_set_y == "C")] = 2 # C
y[which(train_set_y == "D")] = 3 # D
y[which(train_set_y == "E")] = 4 # E
y[which(train_set_y == "F")] = 5 # F
y[which(train_set_y == "G")] = 6 # G
y[which(train_set_y == "H")] = 7 # H
y[which(train_set_y == "I")] = 8 # I
y[which(train_set_y == "J")] = 9 # J
y[which(train_set_y == "X")] = 10 # x

y_test[which(test_set_y == "A")] = 0 
y_test[which(test_set_y == "B")] = 1 
y_test[which(test_set_y == "C")] = 2 
y_test[which(test_set_y == "D")] = 3 
y_test[which(test_set_y == "E")] = 4 
y_test[which(test_set_y == "F")] = 5 
y_test[which(test_set_y == "G")] = 6
y_test[which(test_set_y == "H")] = 7 
y_test[which(test_set_y == "I")] = 8 
y_test[which(test_set_y == "J")] = 9 
y_test[which(test_set_y == "X")] = 10 

################################################################################################데이터 완료  

##################################################### LDA
set.seed(1)

lda.fit = lda(class~b+img+a+option+form+p+i+table+td+tr, data=train_set)

Direction = test_set$class
Direction

lda.pred = predict(lda.fit, test_set)
names(lda.pred)
## pred 한거 확인 
lda.pred$class

ld <- lda.pred$x
cls <- as.numeric(tag[,5])
plot(ld, asp = 1, col = cls[train], xlab="LD 1", ylab = "LD 2") #그래프
# check quality of prediction

lda.class = lda.pred$class
lda.class

df <- data.frame(lda.class, Direction)
df

tab <- table(df)  
tab
diag(tab) # 맞는 것만 가져옴  nrow(df) 2018 개수 가져옴 

1 - sum(diag(tab)) / nrow(df) # 0.7916364  error rate  79%임.. ㅠㅜ
# 맞는 확률 21% ㅠㅜ

mean(lda.class == Direction)  # 맞는 확률  0.2083636

###################################################### Logistic Regression
Logistic <- data.frame(train_set_x,y)

glm.fit = glm(y~b+img+a+option+form+p+i+table+td+tr, data=Logistic)


summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
glm.probs = predict(glm.fit, type="response")
glm.probs[1:10]
Direction_factors <- as.factor(Direction)
mode(Direction_factors)

contrasts(Direction_factors)
names(glm.probs)

# binary Data로 만들어주기위해 
y_test[which(test_set == "A")] = 0
y_test[which(test_set == "B")] = 1
y_test[which(test_set == "C")] = 0
y_test[which(test_set == "D")] = 0
y_test[which(test_set == "E")] = 0
y_test[which(test_set == "F")] = 0
y_test[which(test_set == "G")] = 0
y_test[which(test_set == "H")] = 0
y_test[which(test_set == "I")] = 0
y_test[which(test_set == "J")] = 0
y_test[which(test_set == "X")] = 0

y_test
p <- predict(glm.fit, newdata=test_set, type = "response")

p
pr <- prediction(p, y_test)

prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf) # ROC 그래프 그리기 


############################################################# QDA

qda.fit = qda(class~b+img+a+option+form+p+i+table+td+tr, data=train_set)
qda.fit

qda.class = predict(qda.fit, test_set)$class
table(qda.class, test_set)
qda_df <- data.frame(qda.class, Direction)
qda_tab = table(qda_df)
qda_tab
diag(qda_tab)

mean(qda.class == Direction)  # 16.8% ...

#  img, option, form, table, tr  Lasso 한것으로 

qda.fit2 = qda(class~img+option+form+table+tr, data=train_set)
qda.fit2
qda.class = predict(qda.fit2, test_set)$class
#table(qda.class, test_set)
qda_df <- data.frame(qda.class, Direction)
qda_tab = table(qda_df)
qda_tab
diag(qda_tab)


1 - sum(diag(qda_tab)) / nrow(qda_df) # 0.808  error rate  84.5%임.. ㅠㅜ

mean(qda.class == Direction)   # 14% .....



############################################################# KNN
set.seed(1)

# K = 1일때 
knn_1 <- knn(train = train_set_x,
             test = valid_set_x,
             cl = train_set_y,
             k = 1) 
# train 산점도 그리기  knn_1을 쓴이유는 어차피 분류하는건 같아서 

plot(formula = a~tr,  # a태그와 tr 태그를 이용함.  나중에 최적 찾아서 추가하기 
     data = train_set,
     col = alpha(c("purple", "orange", "green", "blue", "yellow", "pink", "red", "brown", "black", "grey"),0.7)[knn_1],
     main = "KNN(k=1)",
     xlim= c(0,400),
     ylim = c(0,400))

# valid 결과 추가하기 
points(formula = a~tr,
       data = valid_set,
       pch = 17,
       cex = 0.7 ,
       col = alpha(c("purple", "orange", "green", "blue", "yellow", "pink", "red", "brown", "black", "grey"),0.7)[knn_1])

# 범례 그리기 
legend("topright",
       c("A","B","C","D","E","F","G","H","I","J","X"),
       pch = c(rep(1,11), rep(17,11)),
       col = c(rep(alpha(c("purple", "orange", "green", "blue", "yellow", "pink", "red", "brown", "black", "grey"),0.7),2)),
       cex = 0.9)

# KNN_1 분류 정확도 계산하기
accuracy_1 <- sum (knn_1 == valid_set_y) / length(valid_set_y) ; accuracy_1 # KNN_1 0.6411609

accuracy_k <- NULL

for(kk in c(1:499)){ # R 에서(컴터문제일수도 ㅠ) 최대 499개 까지밖에안되서 
  set.seed(1)
  knn_k <- knn(train = train_set_x,
               test = valid_set_x,
               cl = train_set_y,
               k = kk)
  
  accuracy_k <- c(accuracy_k, sum(knn_k == valid_set_y)/ length(valid_set_y))
} 

valid_k <- data.frame(k = c(1:499), accuracy = accuracy_k)

# validation 에 가장 잘맞는 K를 고르는데 데이터가 너무 나눠지지않아서 가장 복잡한 K=1일때가 죄척의 K이다.. ㅠ
plot(fomula = accuracy ~ k,
     data = valid_k,
     type = "o",
     pch = 20,
     main = "validation - optimal k")

# 분류 정확도가 가장 높으면서 가장 작은 k 는 ? 역시나 1
min(valid_k[valid_k$accuracy %in% max(accuracy_k), "k"])

knn_1_test <- knn(train = train_set_x, # 따라서 KNN_1 로 다시 모델 셋팅
                  test = test_set_x,
                  cl = train_set_y,
                  k = 1)

# 최종 정확도 계산하기 
sum(knn_1_test == test_set_y) / length(test_set_y)
# 맞는 확률 0.6649289 지금까지 제일 좋다.
# Lasso로 구한 table, form으로 하니까 겁나 쪼금 올랐다. 0.6658768


############################################################# LASSO

x = as.matrix(train_set_x)

grid=10^seq(10,-2,length=100) # 최적의 람다 찾음
lasso.mod = glmnet(x,y,alpha = 1, lambda = grid)

plot(lasso.mod)

set.seed(1)
cv.out = cv.glmnet(x,y,alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred=predict(lasso.mod, s=bestlam, newx = x)
mean((lasso.pred - y_test)^2) # MSE 

out = glmnet(x,y,alpha=1, lambda=grid)
lasso.coef=predict(out, type="coefficients",s=bestlam)[1:11,]
lasso.coef

fit.lass = glmnet(x,y, alpha = 1)
lasso.coef[lasso.coef!=0]

# b = -0.0001
# img = 0.0043
# a = 0.0017
# option = 0.0046
# form 0.0778
# p = 0.0016
# i = 0.0020
# table = 0.0056
# td = 0.0018
# tr = 0.0042

# form이 그나마... 연관이 가장 높고 나머지는 거의 비슷비슷 하다 
# form 태그는 작성하는 폼이 있어서 주제나눌때 연관이 높은거 같다.

# img, option, form, table, tr  가지고만 사용해도 될꺼 같다. 하지만 predictor가 적기 때문에 몇개 있나 없나 성능 비슷함.


############################################################# SVM


mode(train_set_x)
y = factor(train_set_y)
y_test = factor(test_set_y)
mode(y)

#model <- svm(train_set_x, y , kernel = "linear", cost=10, scale=FALSE) # kernel 등 줄수이씀
#model
#pred <- predict(model, test_set_x)

#table(pred, Direction)


#plot(cmdscale(dist(test_set_x)), col=y_test,
#     pch = c("o","+")[1:150 %in% model$index +1],
#     xlim= c(-1000,200),
#     ylim = c(-200,250))
# support vector 는 +모양으로 아닌것은 O로 나타냄

y_sub = factor(train_sub_y)

# kernel에 따른 조정인자 튜닝
result <- tune.svm(train_sub_x,y_sub,gamma=2^(-5:0), cost=2^(0:4), kernel="radial")
result1 <- tune.svm(train_sub_x, y_sub, cost=2^(0:4), kernel="linear")
result2 <- tune.svm(train_sub_x, y_sub, cost=2^(0:4), degree=2:4, kernel="polynomial")
# svm 이 너무 오래 걸려서 sub data를 사용했다.
# best model만을 얻는 것이기 때문에 

# 적정 cost, gamma값 확인
result$best.parameters # gamma 1 cost 16
result1$best.parameters # cost 16
result2$best.parameters # degree 2 cost 16

german_svm <- svm(train_set_x, y, kernel="radial", gamma = 1, cost=16)
german_svm1 <- svm(train_set_x, y, kernel="linear", cost=16)
german_svm2 <- svm(train_set_x,y, kernel = "polynomial", cost = 16, degree = 2)

#결과
summary(german_svm)
summary(german_svm1)
summary(german_svm2)

# 세경우의 서포트벡터 확인
german_svm$index
german_svm1$index
german_svm2$index

# test로 정확도 측정 
german_svm_predict <- predict(german_svm, test_set_x)
german_svm_predict1 <- predict(german_svm1, test_set_x)
german_svm_predict2 <- predict(german_svm2, test_set_x)

# 표로 확인
table(german_svm_predict, Direction) # 제일 좋음 
table(german_svm_predict1, Direction)
table(german_svm_predict2, Direction)


# + 가 서포트벡터 o 이 데이터 나타낸다. 
plot(cmdscale(dist(test_set_x)), col=y_test,
     pch = c("o","+")[1:150 %in% german_svm$index +1],
     xlim= c(-1000,200),
     ylim = c(-200,250))

plot(cmdscale(dist(test_set_x)), col=y_test,
     pch = c("o","+")[1:150 %in% german_svm1$index +1],
     xlim= c(-1000,200),
     ylim = c(-200,250))

plot(cmdscale(dist(test_set_x)), col=y_test,
     pch = c("o","+")[1:150 %in% german_svm2$index +1],
     xlim= c(-1000,200),
     ylim = c(-200,250))



sum(as.character(german_svm_predict) == Direction) / length(Direction)  # 57 % 정도
# 0.5696682
sum(as.character(german_svm_predict1) == Direction) / length(Direction)  # 26 % 정도
# 0.2630332
sum(as.character(german_svm_predict2) == Direction) / length(Direction)  # 30 % 정도
# 0.2995261


