rm(list=ls()) # 데이터 지우기 
asc <- function(x) { strtoi(charToRaw(x),16L) }   # 아스키코드 함수 
chr <- function(n) { rawToChar(as.raw(n)) }

############################## 초기화
a1 <- read.table("/Users/jaejin/dev/R_pratice/practice/WebpageDataset/A0001.txt", sep = "=", header =F, nrows=6)
# transpose : t()  row, colum 바꿔줌 
header = as.data.frame(t(a1))
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

tag
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


################################################################################################데이터 완료  

set.seed(1)
train = sample(1:dim(tag)[1],as.integer(dim(tag)[1]*(3/4)), replace = FALSE)  # data_length에서 3/4데이터를 추출
train_set <- tag[train,]
test_set <- tag[-train,] 



library(MASS)
lda.fit = lda(class~b+img+a+option+form+p+i+table+td+tr, data=train_set)
lda.fit 
plot(lda.fit)

Direction = test_set$class
Direction

lda.pred = predict(lda.fit, test_set)
names(lda.pred)
## pred 한거 확인 
lda.pred$class

##
ld <- lda.pred$x
cls <- as.numeric(tag[,5])
plot(ld, asp = 1, col = cls[train], xlab="LD 1", ylab = "LD 2") #그래프
# check quality of prediction


##

lda.class = lda.pred$class
lda.class

df <- data.frame(lda.class, Direction)

tab <- table(df)  
tab
diag(tab) # 맞는 것만 가져옴  nrow(df) 2018 개수 가져옴 

1 - sum(diag(tab)) / nrow(df) # 0.7916364  error rate  79%임.. ㅠㅜ
# 맞는 확률 21% ㅠㅜ

mean(lda.class == Direction)  # 맞는 확률  0.2083636

##################################################### LDA