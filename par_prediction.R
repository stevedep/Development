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
nl = 100000

#Twitter
con <- file("final/en_US/en_US.twitter.txt", "r") 
txt = readLines(con, n=nl) 
close(con)
df_txt_twitter = data.frame(txt, stringsAsFactors = F) 

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
top50p_threegram = topnperc(threegram_twitter,20,3)
top50p_fourgram = topnperc(fourgram_twitter,20,4)

st = sum(singlegram_twitter$n, na.rm = T) 
top50p_singlegram = cbind(top50p_singlegram, prob = top50p_singlegram$V1 / st)

tt = sum(twogram_twitter$n, na.rm = T) 
top50p_twogram = cbind(top50p_twogram, prob = top50p_twogram$V1 / st)

trt = sum(threegram_twitter$n, na.rm = T) 
top50p_threegram = cbind(top50p_threegram, prob = top50p_threegram$V1 / st)

frt = sum(fourgram_twitter$n, na.rm = T) 
top50p_fourgram = cbind(top50p_fourgram, prob = top50p_fourgram$V1 / st)

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
head(top50p_threegram_new_labels)
colnames(top50p_threegram_new_labels) = c("nr", "w1", "w2", "w3", "V1", "rt", "prob", "V12", "prob2", "V11","prob1")


setkey(top50p_fourgram,w2,w3,w4)
setkey(top50p_threegram_new_labels,w1,w2,w3)

top50p_fourgram_new = top50p_threegram_new_labels[top50p_fourgram] #alles met i. aangevuld met V1, prob en het 2e en 3e woord
top50p_fourgram_new_labels = top50p_fourgram_new[,c("i.nr","i.w1","w1", "w2", "w3", "i.V1", "i.rt", "i.prob", "V1", "prob", "V12", "prob2", "V11", "prob1")]
colnames(top50p_fourgram_new_labels) = c("nr", "w1", "w2", "w3", "w4", "V1", "rt", "prob", "V13", "prob3", "V12", "prob2", "V11", "prob1")
remove(top50p_fourgram_new_labels)
w4 = c(0.6,0.3,0.05,0.05)
tt = w4 * top50p_fourgram_new_labels[,c("prob", "prob3", "prob2", "prob1")]
tt[, ][is.na(tt[, ])] <- 0
t = tt[, new := prob + prob3 + prob2 + prob1 ]
top50p_fourgram_new_labels = cbind(top50p_fourgram_new_labels,tprob = t$new)

t = head(top50p_fourgram_new_labels[order(-V1),],5)

tolower(t$w1)
#stopCluster(cl)


nword4 = function(words) {
  temp = top50p_fivegram_new_labels[top50p_fivegram_new_labels$w1==words[1] &
                                      top50p_fivegram_new_labels$w2==words[2]
                                    & top50p_fivegram_new_labels$w3==words[3]
                                    & top50p_fivegram_new_labels$w3==words[4]
                                    ,]
  head(temp[order(-tprob), c("w5", "tprob")],3)
}

nword3 = function(words) {
  temp = top50p_fourgram_new_labels[top50p_fourgram_new_labels$w1==words[1] &
                                      top50p_fourgram_new_labels$w2==words[2]
                                    & top50p_fourgram_new_labels$w3==words[3]
                                    ,]
  head(temp[order(-tprob), c("w4", "tprob")],3)
}

nword2 = function(words) {
  temp = top50p_threegram_new_labels[top50p_threegram_new_labels$w1==words[1] &
                                       top50p_threegram_new_labels$w2==words[2]
                                    ,]
  head(temp[order(-tprob), c("w3", "tprob")],3)
}


next_word = function(s) {
  words = splitwords(s)
  wc = length(words[])
  if (wc == 4) {
      result = nword4(words)
      if (length(result[,1] > 0 )) {print("resultaten")} else {print("geen resultaten")}
    }
}


splitwords<-function(x) {
  dfr = data.frame(strsplit(x, " "), stringsAsFactors = F)
  colnames(dfr) = "word"
  dfr$word = tolower(dfr$word)
  l = length(dfr$word)
  b = if(l-3 < 1) {1} else {l-3}
  return(dfr$word[b:l])
}

splitwords(s)
s = "my dog is"
s= "hello my what thanks for the"
next_word(s)
