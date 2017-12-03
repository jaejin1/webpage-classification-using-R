rm(list=ls()) # 데이터 지우기 
asc <- function(x) { strtoi(charToRaw(x),16L) }
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
tag <- cbind(tag_class ,tag1, tag2)

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
  
      table <- cbind(tag_class_data, tag_b_data, tag_img_data)
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

dim(a12_t)[1]  # 개수 
as.integer(dim(a12_t)[1]*(3/4))

set.seed(1)
train = sample(dim(a12_t)[1],as.integer(dim(a12_t)[1]*(3/4)))  # data_length에서 3/4데이터를 추출


fix(a12_t) # 데이터 잘들어온다. 


##################################################################태그 데이터 가져오기 

testext <- read_html("/Users/jaejin/dev/R_pratice/practice/WebpageDataset/K0001.txt", options = "HUGE")
casttext <- html_nodes(frozen, "title")
casttext
html_text(casttext)

cast2 <- html_nodes(frozen, "b")
cast2
html_text(cast2)

cast_img <- html_nodes(frozen, "img")
cast_img
b <- length(cast_img)

length(cast2) # 개수 가져오기 
a <- length(cast2)
a
b
##################### table 만들기 
data1 <- c("b")
data2 <- c("img")

table_test <- data.frame(test = c(a))
table_test
names(table_test) <- c(data1)   # 이름 바꾸기 

table_test2 <- data.frame(test = c(b))
names(table_test) <- c(data2)   # 이름 바꾸기 

table_test
table_test2

table <- cbind(table_test, table_test2)
table2 <- table


#####################
a_str <- "A"
a_str2 <- "X" 
a_str3 <- "A"

for(i in 1:(asc(a_str2) - asc(a_str) + 1)){
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
    
    a12_t <- rbind(a12_t, a1_t)
    
    if(i == 1000){
      a_str3 <- chr((asc(a_str3)+1))  # 다음 알파벳 으로 바꾸기 
      
    }
  }
}
