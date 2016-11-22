library(data.table,  lib.loc="C:/TFS/Rlib/")
library(ngram,  lib.loc="C:/TFS/Rlib/")
library(reshape,  lib.loc="C:/TFS/Rlib/")

con <- file("files/final/en_US/en_US.twitter.txt", "r") 
txt = readLines(con) 
df_txt = data.frame(txt, stringsAsFactors = F) #string as factor = F!!!!


###############################################################################
###############################################################################
###############################################################################

splitter<-function(x) {
  dfr = data.frame(strsplit(x, "\\."), stringsAsFactors = F)
  colnames(dfr) = "zin"
  zinnen = cbind(words = apply(dfr, 1, wordcount), dfr)
  return(zinnen)
}

#nu met loop
#alles met minimaal 3 woorden
create_ngram_table = function(n, zinnen) {
  r = 1
  #use n words sentences only
  nwords = data.frame(zinnen[zinnen$words>n-1,2], stringsAsFactors = F)
  #create result data frame
  df <- data.frame(freq = integer(1000), w1 = character(1000),w2 = character(1000),w3 = character(1000),w4 = character(1000),w5 = character(1000), stringsAsFactors = FALSE)
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
    for (i in 1:length(y[,1])) {
      k = n+3
      l = n+1
      df[r,1] <- y[i,2]
      df[r,2:l] <- y[i,4:k]
      r <- r + 1
    }
  }
  #dt=data.table(df[,])
  #dtw = dt[, .N, by=c(names(dt)[2:n+1])]
  #dtw[, count := .N, by=c(names(dt)[1:n-1])][,prob:=1/count]
  #return(data.frame(dtw, stringsAsFactors = F))
  return(df)
}

dt = create_ngram_table(1,zinnen[2,])
head(dt)

#dit is goed! LET OP SPATIES RONDOM PUNT, NOG BETER AFHANDELEN
str <- "gaan we.gaan we lopen.waar gaan we shoppen.waar gaan we heen.we shoppen ons gek."
zinnen = splitter(str)
singlegram =create_ngram_table(1,zinnen)
singlegram = singlegram[!singlegram$w1 == "",]

dfCorpus = Corpus(VectorSource(df_txt[1:10,]))
lower_case = tm_map(dfCorpus, content_transformer(tolower))

dataframe<-data.frame(text=unlist(sapply(lower_case, `[`, "content")), 
                      stringsAsFactors=F)

r = 1
result <- data.frame(w1 = character(1000), n = integer(1000), stringsAsFactors = FALSE)
#loop door de documenten op te bouwen
for (i in 1:100) {
  str = dataframe[i,]
  zinnen = splitter(str)
  singlegram =create_ngram_table(1,zinnen)
  singlegram = singlegram[!singlegram$w1 == "",]
  result[r:(r+length(singlegram[,1])-1),] = singlegram[,1:2]
  r = r + length(singlegram[,1])  
}


dataframe[1,]
zinnen = splitter(dataframe[1,])
dt = create_ngram_table(1,zinnen[2,])
dtw = dt[, .N, by=c(names(dt)[1:n])]
nwords = data.frame(zinnen[zinnen$words>1,2], stringsAsFactors = F)
pt = data.frame(get.phrasetable(ngram(nwords[2,], n=n, sep=" ")),stringsAsFactors = F)
y = data.frame(transform(pt, pt = colsplit(ngrams, split = " ", names = rep('w', n))), stringsAsFactors = F)

head(result)
dt = data.table(result)
n=1
dtw = dt[, sum(n), by=c(names(dt)[1:n])]

View(dtw)

###############################################################################
###############################################################################
###############################################################################

driegram = create_ngram_table(3,zinnen)
driegram = driegram[!driegram$w1 == "",]
tweegram = create_ngram_table(2,zinnen)
tweegram = tweegram[!tweegram$w1 == "",]
singlegram =create_ngram_table(1,zinnen)
singlegram = singlegram[!singlegram$w1 == "",]
singlegram[, count2 := sum(N)][, prob2:=N/count2]

