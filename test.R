a1 <- read.table("A0001.html", sep = "=", header =F, nrows=6)

# transpose : t()  row, colum 바꿔줌 
a1_t = as.data.frame(t(a1))
names(a1_t) <- c("ID", "URL", "SIZE", "DATE", "TIME", "DATASET")

#remove col1

a1_t <- a1_t[c(2),]  # 첫번째 row 없애버림 
rownames(a1_t) <- NULL  # rowname 바꿈

a2 <- read.table("A0002.html", sep="=", header=F, nrows=6, fill = T)  # url 내의 다른= 가 있을때는 fill씀
a2 <- a2[,c(1:2)]
a2_t <- as.data.frame(t(a2))
names(a2_t) <- c("ID","URL", "SIZE", "DATE", "TIME", "DATASET")
a2_t <- a2_t[c(2),]
a2_t

rownames(a2_t) <- NULL
a2_t

a12_t <- rbind(a1_t, a2_t)   # merge a1 , a2
a12_t   

a3 <- read.table("A0003.html", sep="=" , header=F, nrows=6, fill=T)
a3
a3_t <- as.data.frame(t(a3))
a3_t
names(a3_t) <- c("ID","URL", "SIZE", "DATE", "TIME", "DATASET")
a3_t <- a3_t[c(2),]
a3_t

rownames(a3_t) <- NULL

a123_t <- rbind(a12_t, a3_t)
a123_t


test <- "aa"
test
test2 <- 3
test2 
test3 <- sum(test,test2)

as.integer(123)
as.numeric(test2)
test3 <- test + test
test3 <- paste(test,test)
test3
test3 <- paste(test,test , sep="")  # sep값이 원래는 " " 이라서 지정안해주면 띄어쓰기 생김
test3

test4 <- paste(test,as.numeric(test2))
test4
test4 <- paste(test,as.numeric(test2), sep="")    # integer to String
test4
