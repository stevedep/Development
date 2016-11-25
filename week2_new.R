library(data.table,  lib.loc="C:/TFS/Rlib/")
library(ngram,  lib.loc="C:/TFS/Rlib/")
library(reshape,  lib.loc="C:/TFS/Rlib/")
library(NLP,  lib.loc="C:/TFS/Rlib/")
library(tm,  lib.loc="C:/TFS/Rlib/")
#install.packages("lazyeval", lib="C:/TFS/Rlib/",dependencies=TRUE)
#install.packages("labeling", lib="C:/TFS/Rlib/",dependencies=TRUE)
library(labeling,  lib.loc="C:/TFS/Rlib/")
install.packages("cldr", lib="C:/TFS/Rlib/",dependencies=TRUE)

#install.packages("ggplot2", lib="C:/TFS/Rlib/",dependencies=TRUE)

library(ggplot2,  lib.loc="C:/TFS/Rlib/")


con <- file("files/final/en_US/en_US.twitter.txt", "r") 
txt = readLines(con) 
df_txt = data.frame(txt, stringsAsFactors = F) #string as factor = F!!!!

splitter<-function(x) {
  dfr = data.frame(strsplit(x, "\\."), stringsAsFactors = F)
  colnames(dfr) = "zin"
  zinnen = cbind(words = apply(dfr, 1, wordcount), dfr)
  return(zinnen)
}

create_ngram_table = function(n, zinnen) {
  r = 1
  #create result data frame
  df <- data.frame(freq = integer(1000), w1 = character(1000),w2 = character(1000),
                   w3 = character(1000),w4 = character(1000),w5 = character(1000), stringsAsFactors = FALSE)
  #use n words sentences only
  nwords <- data.frame(zinnen[zinnen$words>n-1,2], stringsAsFactors = F)
  if (length(nwords[,]) > 0 ) {
    #loop trough sentences
    for (z in 1:length(nwords[,1])) { #for each sentence
      #get ngrams for sentence
      pt = data.frame(get.phrasetable(ngram(nwords[z,], n=n, sep=" ")),stringsAsFactors = F)
      #split in columns
      y = data.frame(transform(pt, pt = colsplit(ngrams, split = " ", names = rep('w', n))), stringsAsFactors = F)
      #convert new columns to character
      for (a in 1:n) {
        y[,a+3] = as.character(y[,a+3])  
      }
      
      #add each ngram to result data frame
      k = n+3
      l = n+1
      for (i in 1:length(y[,1])) {
        df[r,1] <- y[i,2]
        df[r,2:l] <- y[i,4:k]
        r <- r + 1
      }
      
    }
  }
  return(df)
}

loop_doc = function(a,d) {
    r = 1
    result <- data.frame(n = integer(2000000), w1 = character(2000000), w2 = character(2000000),
                         w3 = character(2000000), w4 = character(2000000), w5 = character(2000000),
                         stringsAsFactors = FALSE)
  #loop door de documenten op te bouwen
    for (i in 1:d) {
    str = dataframe[i,]
    zinnen = splitter(str)
    xgram =create_ngram_table(a,zinnen)
    xgram = xgram[!xgram$w1 == "",]
    if (length(xgram[,1]) > 0 ) {
      result[r:(r+length(xgram[,1])-1),] = xgram[,1:(a+1)]
      r = r + length(xgram[,1])  
      }
    
    print(i)
  }
  
  result = result[!result$w1 == "",]
  result_dt = data.table(result)
  dtw = result_dt[, sum(n), by=c(names(result_dt)[2:(a+1)])]
  return(dtw)
}

#load data
dfCorpus = Corpus(VectorSource(df_txt[1:10000,]))
lower_case = tm_map(dfCorpus, content_transformer(tolower))

dataframe<-data.frame(text=unlist(sapply(lower_case, `[`, "content")), 
                      stringsAsFactors=F)

#Some words are more frequent than others - what are the distributions of word frequencies?
q1 = loop_doc(1,500)
y = head(sort(q1$V1,decreasing=TRUE), n = 50)
top50 = q1[q1$V1 %in% y,]
top50 = top50[order(-V1),] 

x = c(1:length(top50$w1))
z = data.frame(cbind(x,top50))
p = ggplot(z, aes(x, V1, label = w1)) + geom_text()
p

#What are the frequencies of 2-grams
q2 = loop_doc(2,500)
y = head(sort(q2$V1,decreasing=TRUE), n = 50)
top50n2 = q2[q2$V1 %in% y,]
top50n2 = top50n2[order(-V1),] 

x = c(1:length(top50n2$w1))
z = data.frame(cbind(x,top50n2))

q = p + geom_line(data=z, aes(x=x, y=V1), color='red')
q
#What are the frequencies of 3-grams

q3 = loop_doc(3,500)
y = head(sort(q3$V1,decreasing=TRUE), n = 2)
top50n3 = q3[q3$V1 %in% y,]
top50n3 = top50n3[order(-V1),] 

x = c(1:length(top50n3$w1))
z = data.frame(cbind(x,top50n3))

r = q + geom_line(data=z, aes(x=x, y=V1), color='purple')
r

#How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
q1 = loop_doc(1,500)
q1 = q1[order(-V1),]

q1n = cbind(nr = c(1:length(q1$w1)), q1)
rt = sapply(1:length(q1$w1), function(x) sum(q1n[1:x]$V1))
q1n2 = cbind(q1n, rt)
t = sum(q1n$V1)
prc = rt / t * 100
prc = data.frame(pr = prc)
length(prc[prc$pr < 50.1,]) / length(prc[,]) * 100
