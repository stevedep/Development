load("top50p_fivegram_new_labels.Rda")
load("top50p_fourgram_new_labels.Rda")
load("top50p_threegram_new_labels.Rda")
load("top50p_twogram_new_labels.Rda")


nword3 = function(words) {
  temp = top50p_fourgram_new_labels[top50p_fourgram_new_labels$w1==words[1] &
                                      top50p_fourgram_new_labels$w2==words[2]
                                    & top50p_fourgram_new_labels$w3==words[3],]
  if (length(temp$nr) > 1) 
  {  head(temp[order(-temp$tprob), c("w4", "tprob")],3) }
  else {
    temp = top50p_threegram_new_labels[top50p_threegram_new_labels$w1==words[2] &
                                         top50p_threegram_new_labels$w2==words[3]
                                       ,]
    if (length(temp$nr) > 1) 
    {  head(temp[order(-temp$tprob), c("w3", "tprob")],3) }
    else {
      temp = top50p_twogram_new_labels[top50p_twogram_new_labels$w1==words[3] 
                                       ,]
      if (length(temp$nr) > 1) 
      {  head(temp[order(-temp$tprob), c("w2", "tprob")],3) }
      else {data.frame(c("the"))}
    }      
  }
}

nword2 = function(words) {
  temp = top50p_threegram_new_labels[top50p_threegram_new_labels$w1==words[1] &
                                       top50p_threegram_new_labels$w2==words[2]
                                     ,]
  if (length(temp$nr) >= 1) 
  {  head(temp[order(-temp$tprob), c("w3", "tprob")],3) }
  else {
    temp = top50p_twogram_new_labels[top50p_twogram_new_labels$w1==words[2] 
                                     ,]
    if (length(temp$nr) >= 1) 
    {  head(temp[order(-temp$tprob), c("w2", "tprob")],3) }
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

nword4 = function(words) {
  temp = top50p_fivegram_new_labels[top50p_fivegram_new_labels$w1==words[1] &
                                      top50p_fivegram_new_labels$w2==words[2]
                                    & top50p_fivegram_new_labels$w3==words[3]
                                    & top50p_fivegram_new_labels$w4==words[4]
                                    ,]
  if (length(temp$nr) >= 1) 
  {  head(temp[order(-temp$tprob), c("w5", "tprob")],3) }
  else {
    temp = top50p_fourgram_new_labels[top50p_fourgram_new_labels$w1==words[2] &
                                        top50p_fourgram_new_labels$w2==words[3]
                                      & top50p_fourgram_new_labels$w3==words[4],]
    if (length(temp$nr) >= 1) 
    {  head(temp[order(-temp$tprob), c("w4", "tprob")],3) }
    else {
      temp = top50p_threegram_new_labels[top50p_threegram_new_labels$w1==words[3] &
                                           top50p_threegram_new_labels$w2==words[4]
                                         ,]
      if (length(temp$nr) >= 1) 
      {  head(temp[order(-temp$tprob), c("w3", "tprob")],3) }
      else {
        temp = top50p_twogram_new_labels[top50p_twogram_new_labels$w1==words[4] 
                                         ,]
        if (length(temp$nr) >= 1) 
        {  head(temp[order(-temp$tprob), c("w2", "tprob")],3) }
        else {data.frame(c("the"))}
      }      
    }
  }
}


shinyServer(function(input, output) {

  predictionResp<-reactive(
    {
      if(input$userInput==""){return("")}
      else   
      {
        return(next_word(input$userInput))
      }})
  
  output$Prediction <- renderText(predictionResp()[1,1])
  output$PredictionTable = renderDataTable({
    predictionResp()
  }, options = list(orderClasses = F, searching = FALSE, paging = FALSE))
  
}
)
