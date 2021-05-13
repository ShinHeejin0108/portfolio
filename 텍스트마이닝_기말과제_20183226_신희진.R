library(httr)
library(rvest)
#크리스마스 선물
url0 <- "https://www.coupang.com/np/search?q=%ED%81%AC%EB%A6%AC%EC%8A%A4%EB%A7%88%EC%8A%A4+%EC%84%A0%EB%AC%BC&channel=user&component=&eventCategory=SRP&trcid=&traid=&sorter=saleCountDesc&minPrice=&maxPrice=&priceRange=&filterType=&listSize=36&filter=&isPriceRange=false&brand=&offerCondition=&rating=0&page="
url1 <- "&rocketAll=false&searchIndexingToken=&backgroundColor="

df.product<-NULL               

for (pageno in  1:10) {                       
  url2<-paste(url0,pageno,url1,sep="")
  print(url2)
  
  doc<-read_html(url2,encoding="UTF-8")
  
  namenodes <-html_nodes(doc, ".name")  
  name <- html_text(namenodes)
  name<-gsub('\n','',name)
  
  pricenodes <- html_nodes(doc,".price-value")  
  price <- html_text(pricenodes)
  price<-gsub(',','',price)
  price1<-as.numeric(price)
  df<-data.frame(name,price1)
  df.product<-rbind(df.product,df)
}

df.product$name<-format(df.product$name,justify="left")
data1<-df.product
head(data1)
dim(data1)

x <- data1$price1

mean(x)

par(mfrow=c(1,2))
plot(x,main="Christmas-gift",pch=21,xlab='popular',ylab = 'price',ylim=c(0,90000)) 
a <- mean(x);abline(h=a,col='red')

data1 <- gsub("[0-9]","",data1)
data1 <- gsub(",", "", data1)
data1 <- gsub("크리스마스\\S*", "크리스마스", data1)
data1 <- gsub("\"\\s*", "", data1)
data1 <- gsub("개\\s*", "", data1)
data1 <- gsub("[A-Za-z]","",data1)
library(tm)
library(qdapRegex)
library(wordcloud)
library(RColorBrewer)

myCorpus <- Corpus(VectorSource(data1))
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
findFreqTerms(myDtm, lowfreq = 7)

palette <- brewer.pal(8,"Dark2") 
set.seed(1234)

wordcloud(words = data1,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = palette)

#입학 선물
url3 <-"https://www.coupang.com/np/search?q=%EC%9E%85%ED%95%99%EC%84%A0%EB%AC%BC&channel=user&component=&eventCategory=SRP&trcid=&traid=&sorter=saleCountDesc&minPrice=&maxPrice=&priceRange=&filterType=&listSize=36&filter=&isPriceRange=false&brand=&offerCondition=&rating=0&page="
url4 <- "&rocketAll=false&searchIndexingToken=&backgroundColor="

df.product1 <-NULL               

for (i in  1:10) {                       
  url5<-paste(url3,i,url4,sep="")
  print(url5)
  
  doc1<-read_html(url5,encoding="UTF-8")
  
  namenodes1 <-html_nodes(doc1, ".name")  
  name1 <- html_text(namenodes1)
  name1 <-gsub('\n','',name1)
  
  pricenodes1 <- html_nodes(doc1,".price-value")  
  price2 <- html_text(pricenodes1)
  price2 <-gsub(',','',price2)
  price3 <-as.numeric(price2)
  df1 <-data.frame(name1,price3)
  df.product1 <-rbind(df.product1 ,df1)
}

df.product1$name1<-format(df.product1$name1,justify="left")
data2<-df.product1
head(data2)
dim(data2)

x1 <- data2$price3

mean(x1)

par(mfrow=c(1,2))
plot(x1,main='Entrance gift',pch=21,xlab="popular",ylab = 'price',ylim=c(0,80000)) 
a1 <- mean(x1);abline(h=a1,col='red')


data2 <- gsub("[0-9]","",data2)
data2 <- gsub("개", "", data2)
data2 <- gsub("\"\\S*", "", data2)
data2 <- gsub("[A-Za-z]","",data2)
data2 <- gsub("남\\s*","",data2)
data2 <- gsub("\\s*,","",data2)

myCorpus <- Corpus(VectorSource(data2))
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
findFreqTerms(myDtm, lowfreq = 7)

palette1 <- brewer.pal(8,"Set1") 
set.seed(2234)

wordcloud(words = data2,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = palette1)

#졸업 선물
url6 <- "https://www.coupang.com/np/search?q=%EC%A1%B8%EC%97%85%EC%84%A0%EB%AC%BC&channel=user&component=&eventCategory=SRP&trcid=&traid=&sorter=saleCountDesc&minPrice=&maxPrice=&priceRange=&filterType=&listSize=36&filter=&isPriceRange=false&brand=&offerCondition=&rating=0&page="
url7 <- "&rocketAll=false&searchIndexingToken=&backgroundColor="
  
df.product2<-NULL

for (i in  1:10) {                       
  url8<-paste(url6,i,url7,sep="")
  print(url8)
  
  doc2<-read_html(url8,encoding="UTF-8")
  
  namenodes2 <-html_nodes(doc2, ".name")  
  name2 <- html_text(namenodes2)
  name2 <- gsub('\n','',name2)
  
  pricenodes2 <- html_nodes(doc2,".price-value")  
  price4 <- html_text(pricenodes2)
  price4<-gsub(',','',price4)
  price5<-as.numeric(price4)
  df2 <-data.frame(name2 ,price5)
  df.product2 <- rbind(df.product2,df2)
}

df.product2$name2 <- format(df.product2$name2,justify="left")
data3 <- df.product2
head(data3)
dim(data3)

x2 <- data3$price5

mean(x2)

par(mfrow=c(1,2))
plot(x2,main='Graduation gift',pch=21,xlab="popular",ylab = 'price',ylim=c(0,80000)) 
a2 <- mean(x2);abline(h=a2,col='red')


data3 <- gsub("[0-9]","",data3)
data2 <- gsub("개", "", data2)
data3 <- gsub("\"\\S*", "", data3)
data3 <- gsub("[A-Za-z]","",data3)
data3 <- gsub("개\\s*","",data3)
data3 <- gsub("꽃다발\\s*","꽃다발",data3)
data3 <- gsub("남\\s*","",data3)

myCorpus <- Corpus(VectorSource(data3))
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
findFreqTerms(myDtm, lowfreq = 7)

palette2 <- brewer.pal(8,"YlGn") 
set.seed(1232)

wordcloud(words = data3,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = palette)

#생일 선물
url9 = "https://www.coupang.com/np/search?q=%EC%83%9D%EC%9D%BC%EC%84%A0%EB%AC%BC%EC%B6%94%EC%B2%9C&channel=user&component=&eventCategory=SRP&trcid=&traid=&sorter=saleCountDesc&minPrice=&maxPrice=&priceRange=&filterType=&listSize=36&filter=&isPriceRange=false&brand=&offerCondition=&rating="
url10 = "&rocketAll=false&searchIndexingToken=&backgroundColor="

df.product3<-NULL               

for (p in  1:10) {                       
  url11<-paste(url9,p,url10,sep="")
  print(url11)
  
  doc3<-read_html(url11,encoding="UTF-8")
  
  namenodes3 <-html_nodes(doc3, ".name")  
  name3 <- html_text(namenodes3)
  name3 <- gsub('\n','',name3)
  
  pricenodes3 <- html_nodes(doc3,".price-value")  
  price6 <- html_text(pricenodes3)
  price6 <-gsub(',','',price6)
  price7 <- as.numeric(price6)
  df3 <- data.frame(name3 ,price7)
  df.product3 <- rbind(df.product3,df3)
}

df.product3$name3<-format(df.product3$name3,justify="left")
data4 <- df.product3
head(data4)
dim(data4)

x3 <- price7

mean(price7)

par(mfrow=c(1,2))
plot(x3,main='birthday present',pch=21,xlab="popular",ylab = 'price',ylim=c(0,80000)) 
a3 <-mean(x3);abline(h=a3,col='red')

data4 <- gsub("[0-9]","",data4)
data2 <- gsub("개", "", data2)
data4 <- gsub("\"\\S*", "", data4)
data4 <- gsub("[A-Za-z]","",data4)
data4 <- gsub("개\\s*","",data4)
data4 <- gsub("혼합색상\\s*","",data4)
data4 <- gsub("무드등\\s*","무드등",data4)

myCorpus <- Corpus(VectorSource(data4))
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 3))
findFreqTerms(myDtm, lowfreq = 5)

palette3 <- brewer.pal(7,"Paired") 
set.seed(1232)

wordcloud(words = data4,
          min.freq = 2,
          max.words = 200,
          random.order = F,
          rot.per = .1,
          scale = c(4, 0.3),
          colors = palette.colors(8))

#가격 별 비교
a_1 <- c(a,a1,a2,a3)
a_2 <- round(a_1, digits = 1)
a_3 <- paste(a_2,"원")

barplot(a_2, names = c("크리스마스", "입학", "졸업", "생일"),
        col = rainbow(9),
        main = "기념일 선물 별 가격 비교",
        xlab = "기념일",
        ylab = "가격",
        ylim = c(0,30000))
text(bar.text, a_2+10, labels = a_3)
