install.packages("doParallel",dependencies=TRUE)
install.packages("ngram",dependencies=TRUE)
install.packages("reshape",dependencies=TRUE)
install.packages("doMC",dependencies=TRUE)
install.packages("data.table",dependencies=TRUE)

library(data.table)
library(doParallel)
library(ngram)
library(reshape)
library(foreach)
library(doMC)

#test

#download file
url = "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip" 
destfile = "Coursera-SwiftKey.zip"
download.file(url, destfile)

#unzip
unzip(destfile)

#functions
splitter<-function(x) {
  dfr = data.frame(strsplit(x, "\\. |\\? "), stringsAsFactors = F)
  colnames(dfr) = "zin"
  dfr$zin = tolower(dfr$zin)
  dfr$zin = gsub("[^[:alnum:]| ]", "",dfr$zin)
  dfr$zin = gsub("\\s+", " ",dfr$zin)
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

f_create_ngram = function (nl, ds,n, parts) {#number of lines, dataset, # ngram 
  nl = length(ds[,1])  
  intm_result = parLapply(cl, ds[,],
                          function(x) {
                            library(ngram)
                            library(reshape)
                            zinnen = splitter(x)
                            xgram =create_ngram_table(n,zinnen)
                            xgram = xgram[!xgram$w1 == "",]
                            xgram
                          }
  )
  
  result = foreach(j=seq(1,nl, by=(nl/parts))) %dopar% {
    t = 0
    for (i in j:(j+(nl/parts-1))) {
      t = t + length(intm_result[[i]]$w1)
    }
    
    result <- data.frame(n = integer(t), w1 = character(t), w2 = character(t),
                         w3 = character(t), w4 = character(t), w5 = character(t),
                         stringsAsFactors = FALSE)
    r=1
    for (i in j:(j+(nl/parts-1))) {
      if (length(intm_result[[i]][,1]) > 0 ) {
        result[r:(r+length(intm_result[[i]][,1])-1),] = intm_result[[i]][,]
        r = r + length(intm_result[[i]][,1])  
      }
    }
    result
  }  
  do.call("rbind", result)
}

count_lines<-function(x) {
  dfr = data.frame(strsplit(x, "\\. |\\? "), stringsAsFactors = F)
  colnames(dfr) = "zin"
  zinnen = cbind(words = apply(dfr, 1, wordcount), dfr)
  length(zinnen$zin)
}

count_words<-function(x) {
  dfr = data.frame(strsplit(x, "\\. |\\? "), stringsAsFactors = F)
  colnames(dfr) = "zin"
  zinnen = cbind(words = apply(dfr, 1, wordcount), dfr)
  sum(zinnen$words)
}

#top percentage by frequency
topnperc =  function(df, p,a) { #data frame, percentage, #ngram
  dt = data.table(df)
  dt_a = dt[, sum(n), by=c(names(dt)[2:(a+1)])]
  dt_as =  dt_a[order(-V1),] 
  q1n = cbind(nr = c(1:length(dt_as$w1)), dt_as)
  q1nv = q1n$V1
  #clusterExport(cl=cl, varlist=c("q1nv"), envir=environment())
  #rt = parSapply(cl,1:length(q1nv), function(x) sum(q1nv[1:x]))
  rt = cumsum(q1nv)
  q1n = cbind(q1n, rt)                        
  t = sum(q1n$V1, na.rm=TRUE)
  prc = rt / t * 100
  lprc = (prc < p)
  l = table(lprc)["TRUE"]
  q1n[1:l,]
}


##Start processing singlegram
#load twitter
nl = 10000

#Twitter
con <- file("../final/en_US/en_US.twitter.txt", "r") 
txt = readLines(con, n=nl) 
close(con)
df_txt_twitter = data.frame(txt, stringsAsFactors = F) 

#l = sum(apply(df_txt_twitter,1,count_lines))

con <- file("../final/en_US/en_US.news.txt", "r") 
txt = readLines(con, n=nl) 
close(con)
df_txt_news = data.frame(txt, stringsAsFactors = F) 

con <- file("../final/en_US/en_US.blogs.txt", "r") 
txt = readLines(con, n=nl) 
close(con)
df_txt_blog = data.frame(txt, stringsAsFactors = F) 
#l = sum(apply(df_txt_blog,1,count_lines))

df_txt_twitter = rbind(df_txt_twitter, df_txt_blog)
df_txt_twitter = rbind(df_txt_twitter, df_txt_news)


#prep parralel processing
# Calculate the number of cores
no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
registerDoMC(no_cores) 
clusterExport(cl=cl, varlist=c("splitter", "create_ngram_table"))


singlegram_twitter = f_create_ngram(nl,ds = df_txt_twitter,n = 1, parts = 63)
twogram_twitter = f_create_ngram(nl,ds = df_txt_twitter,n = 2, parts = 63)
threegram_twitter = f_create_ngram(nl,ds = df_txt_twitter,n = 3, parts = 63)
fourgram_twitter = f_create_ngram(nl,ds = df_txt_twitter,n = 4, parts = 63)
fivegram_twitter = f_create_ngram(nl,ds = df_txt_twitter,n = 5, parts = 63)
#system.time(f_create_ngram(nl,ds = df_txt_twitter,n = 1, parts = 63))

top50p_singlegram = topnperc(singlegram_twitter,70,1)
top50p_twogram = topnperc(twogram_twitter,30,2)
top50p_threegram = topnperc(threegram_twitter,30,3)
top50p_fourgram = topnperc(fourgram_twitter,30,4)
top50p_fivegram = topnperc(fivegram_twitter,30,5)

#head(top50p_threegram)

#add probabilities
st = sum(singlegram_twitter$n, na.rm = T) 
top50p_singlegram = cbind(top50p_singlegram, prob = top50p_singlegram$V1 / st)

tt = sum(twogram_twitter$n, na.rm = T) 
top50p_twogram = cbind(top50p_twogram, prob = top50p_twogram$V1 / st)

trt = sum(threegram_twitter$n, na.rm = T) 
top50p_threegram = cbind(top50p_threegram, prob = top50p_threegram$V1 / st)

frt = sum(fourgram_twitter$n, na.rm = T) 
top50p_fourgram = cbind(top50p_fourgram, prob = top50p_fourgram$V1 / st)

frt = sum(fivegram_twitter$n, na.rm = T) 
top50p_fivegram = cbind(top50p_fivegram, prob = top50p_fivegram$V1 / st)

setkey(top50p_twogram,w2)
setkey(top50p_singlegram,w1)

top50p_twogram_new = top50p_singlegram[top50p_twogram]
top50p_twogram_new_labels = top50p_twogram_new[,c("i.nr","i.w1","w1","i.V1", "i.rt", "i.prob", "V1", "prob")]
colnames(top50p_twogram_new_labels) = c("nr", "w1", "w2", "V1", "rt", "prob", "V11", "prob1")

setkey(top50p_threegram,w2,w3)
setkey(top50p_twogram_new_labels,w1,w2)

top50p_threegram_new = top50p_twogram_new_labels[top50p_threegram]
head(top50p_threegram_new)
top50p_threegram_new_labels = top50p_threegram_new[,c("i.nr","i.w1","w1", "w2","i.V1", "i.rt", "i.prob", "V1", "prob", "V11", "prob1")]
#head(top50p_threegram_new_labels)
colnames(top50p_threegram_new_labels) = c("nr", "w1", "w2", "w3", "V1", "rt", "prob", "V12", "prob2", "V11","prob1")

setkey(top50p_fourgram,w2,w3,w4)
setkey(top50p_threegram_new_labels,w1,w2,w3)

top50p_fourgram_new = top50p_threegram_new_labels[top50p_fourgram] #alles met i. aangevuld met V1, prob en het 2e en 3e woord
top50p_fourgram_new_labels = top50p_fourgram_new[,c("i.nr","i.w1","w1", "w2", "w3", "i.V1", "i.rt", "i.prob", "V1", "prob", "V12", "prob2", "V11", "prob1")]
colnames(top50p_fourgram_new_labels) = c("nr", "w1", "w2", "w3", "w4", "V1", "rt", "prob", "V13", "prob3", "V12", "prob2", "V11", "prob1")
top50p_fourgram_new_labels[, ][is.na(top50p_fourgram_new_labels[, ])] <- 0

setkey(top50p_fivegram,w2, w3,w4,w5)
setkey(top50p_fourgram_new_labels,w1,w2,w3,w4)

top50p_fivegram_new = top50p_fourgram_new_labels[top50p_fivegram] #alles met i. aangevuld met V1, prob en 
#het 2e en 3e woord
top50p_fivegram_new_labels = top50p_fivegram_new[,c("i.nr","i.w1","w1", "w2", "w3", "w4", "i.V1", "i.rt", "i.prob", "V1", "prob", "V13", "prob3","V12", "prob2", "V11", "prob1")]
colnames(top50p_fivegram_new_labels) = c("nr", "w1", "w2", "w3", "w4", "w5", "V1", "rt", "prob","V14", "prob4", "V13", "prob3", "V12", "prob2", "V11", "prob1")
top50p_fivegram_new_labels[, ][is.na(top50p_fivegram_new_labels[, ])] <- 0

#total prob uitrekennen
w2 = c(0.9,0.1)
tt = mapply("*",top50p_twogram_new_labels[,c("prob", "prob1")],w2)
tt = data.frame(tt)
tt$new = tt$prob + tt$prob1
top50p_twogram_new_labels = cbind(top50p_twogram_new_labels,tprob = tt$new)

#head(top50p_twogram_new_labels[order(-prob),])
#head(top50p_twogram_new_labels[order(-tprob),])

w3 = c(0.9,0.05, 0.05)
tt = mapply("*",top50p_threegram_new_labels[,c("prob","prob2", "prob1")],w3)
tt = data.frame(tt)
tt$new = tt$prob + tt$prob2 + tt$prob1
top50p_threegram_new_labels = cbind(top50p_threegram_new_labels,tprob = tt$new)

#head(top50p_threegram_new_labels[order(-prob),])
#head(top50p_threegram_new_labels[order(-tprob),])

w4 = c(0.8,0.1,0.005,0.005)
tt = mapply("*",top50p_fourgram_new_labels[,c("prob", "prob3", "prob2", "prob1")],w4)
tt = data.frame(tt)
tt$new = tt$prob + tt$prob3 + tt$prob2 + tt$prob1
top50p_fourgram_new_labels = cbind(top50p_fourgram_new_labels,tprob = tt$new)

#head(top50p_fourgram_new_labels[order(-prob),])
#head(top50p_fourgram_new_labels[order(-tprob),])

w5 = c(0.7,0.1, 0.1,0.005,0.005)
tt = mapply("*",top50p_fivegram_new_labels[,c("prob", "prob4", "prob3", "prob2", "prob1")],w5)
tt = data.frame(tt)
tt$new = tt$prob + tt$prob4  +tt$prob3 + tt$prob2 + tt$prob1
top50p_fivegram_new_labels = cbind(top50p_fivegram_new_labels,tprob = tt$new)

#head(top50p_fivegram_new_labels[order(-prob),])
#head(top50p_fivegram_new_labels[order(-tprob),])

#stopCluster(cl)

#five labels hebben we nog niet gemaakt. 
nword4 = function(words) {
  temp = top50p_fivegram_new_labels[top50p_fivegram_new_labels$w1==words[1] &
                                      top50p_fivegram_new_labels$w2==words[2]
                                    & top50p_fivegram_new_labels$w3==words[3]
                                    & top50p_fivegram_new_labels$w4==words[4]
                                    ,]
  if (length(temp$nr) > 1) 
  {  head(temp[order(-tprob), c("w5", "tprob")],3) }
  else {
    temp = top50p_fourgram_new_labels[top50p_fourgram_new_labels$w1==words[2] &
                                        top50p_fourgram_new_labels$w2==words[3]
                                      & top50p_fourgram_new_labels$w3==words[4],]
    if (length(temp$nr) > 1) 
    {  head(temp[order(-tprob), c("w4", "tprob")],3) }
    else {
      temp = top50p_threegram_new_labels[top50p_threegram_new_labels$w1==words[3] &
                                           top50p_threegram_new_labels$w2==words[4]
                                         ,]
      if (length(temp$nr) > 1) 
      {  head(temp[order(-tprob), c("w3", "tprob")],3) }
      else {
        temp = top50p_twogram_new_labels[top50p_twogram_new_labels$w1==words[4] 
                                         ,]
        if (length(temp$nr) > 1) 
        {  head(temp[order(-tprob), c("w2", "tprob")],3) }
        else {data.frame(c("the"))}
      }      
    }
  }
}

nword3 = function(words) {
  temp = top50p_fourgram_new_labels[top50p_fourgram_new_labels$w1==words[1] &
                                      top50p_fourgram_new_labels$w2==words[2]
                                    & top50p_fourgram_new_labels$w3==words[3],]
  if (length(temp$nr) > 1) 
  {  head(temp[order(-tprob), c("w4", "tprob")],3) }
  else {
    temp = top50p_threegram_new_labels[top50p_threegram_new_labels$w1==words[2] &
                                         top50p_threegram_new_labels$w2==words[3]
                                       ,]
    if (length(temp$nr) > 1) 
    {  head(temp[order(-tprob), c("w3", "tprob")],3) }
    else {
      temp = top50p_twogram_new_labels[top50p_twogram_new_labels$w1==words[3] 
                                       ,]
      if (length(temp$nr) > 1) 
      {  head(temp[order(-tprob), c("w2", "tprob")],3) }
      else {data.frame(c("the"))}
    }      
  }
}

nword2 = function(words) {
  temp = top50p_threegram_new_labels[top50p_threegram_new_labels$w1==words[1] &
                                       top50p_threegram_new_labels$w2==words[2]
                                     ,]
  if (length(temp$nr) > 1) 
  {  head(temp[order(-tprob), c("w3", "tprob")],3) }
  else {
    temp = top50p_twogram_new_labels[top50p_twogram_new_labels$w1==words[2] 
                                     ,]
    if (length(temp$nr) > 1) 
    {  head(temp[order(-tprob), c("w2", "tprob")],3) }
    else {data.frame(c("the"))}
  }      
}

next_word = function(s) {
  words = splitwords(s)
  wc = length(words[])
  if (wc > 4) {wc = 4}
  
  if (wc == 4) {
    nword4(words)
  }
  else if (wc == 3) {nword3(words)}
  else if (wc == 2) {nword2(words)}
}

splitwords<-function(x) {
  dfr = data.frame(strsplit(x, " "), stringsAsFactors = F)
  colnames(dfr) = "word"
  dfr$word = tolower(dfr$word)
  dfr$word = gsub("[^[:alnum:]| ]", "",dfr$word)
  dfr$word = gsub("\\s+", " ",dfr$word)
  l = length(dfr$word)
  b = if(l-3 < 1) {1} else {l-3}
  return(dfr$word[b:l])
}

splitwords(s)
s = "hi mi for the"
s= "hello my what thanks for... the"
s= " a bouquet, and a case of"

next_word(s)

