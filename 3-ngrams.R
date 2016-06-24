#------------------------------------------------------------------------------#
#                                                                              #
#                           Coursera Capstone Project                          #
#                                                                              #
#------------------------------------------------------------------------------#
# Author list (Alphabetical Order):
#    Name                       Email
#
#    Mathieu Wauters            wauters.mathieu@gmail.com
#------------------------------------------------------------------------------#
#                           NGrams.R                                           #
#------------------------------------------------------------------------------#

source("0-general.R")

dt<-readRDS(file = paste(my_directory,"cleanwords.Rds",sep=""))

unigram<-create_unigram(dt)
saveRDS(unigram, file = paste(my_directory,"unigram.Rds",sep=""))
unigram<-NULL
bigram<-create_bigram(dt)
saveRDS(bigram, file = paste(my_directory,"bigram.Rds",sep=""))
bigram<-NULL
trigram<-create_trigram(dt)
saveRDS(trigram, file = paste(my_directory,"trigram.Rds",sep=""))
trigram<-NULL
fourgram<-create_fourgram(dt)
saveRDS(fourgram, file = paste(my_directory,"fourgram.Rds",sep=""))
fourgram<-NULL

rotate<-function(my_vector){
  return(my_vector[(1:(length(my_vector)) %% length(my_vector))+1])
}

#------------------------------------------------------------------------------#
#                           Unigram functions                                  #
#------------------------------------------------------------------------------#
create_unigram<-function(my_dt){
  setkey(my_dt, W1)
  my_dt = unique(my_dt)
  my_dt = my_dt[W1 != start_sentence_string_filtered]
  nbr1Gram = sum(my_dt$cW1)
  my_dt[ , pW1:= cW1/nbr1Gram]
  return(my_dt)
}

#------------------------------------------------------------------------------#
#                           Bigram functions                                   #
#------------------------------------------------------------------------------#
create_bigram<-function(my_dt){
  my_dt = data.table(
    W1 = my_dt$W1, 
    cW1 = my_dt$cW1,
    W2= rotate(my_dt$W1),
    cW2= rotate(my_dt$cW1)
  )
  setkey(my_dt, W1, W2)
  my_dt = my_dt[W2 != start_sentence_string_filtered]
  my_dt[, cW1W2:= .N, by = .(W1, W2)]
  my_dt = unique(my_dt)
  nbr2Gram = as.numeric(sum(my_dt$cW1W2))           #we have 443.336 bigrams
  
  #perform Good Turing and Katz backoff
  gt = goodTuring(my_dt$cW1W2) #this yields a NaN
  sum(gt$proportion * gt$n) + gt$P0
  setkey(my_dt, cW1W2)
  
  gtFreq = rep(gt$proportion, gt$n)
  
  my_dt[ , dcW1W2:= gtFreq * nbr2Gram]
  
  unigram<-readRDS(file = paste(my_directory,"unigram.Rds",sep=""))
  setkey(unigram, W1)
  
  setkey(my_dt, W1)
  my_dt[ , pW1:= unigram[W1]$pW1]
  setkey(my_dt, W2)
  my_dt[ , pW2:= unigram[W2]$pW1]
  
  setkey(my_dt, W1)
  my_dt[ , pW1W2:= dcW1W2/cW1]
  
  setkey(my_dt, W1, W2)
  my_dt[ , beta:= 1 - sum(pW1W2), by=W1]
  
  my_dt[ , alpha:= beta/(1 - sum(pW2)), by=W1] #fix small error here
  
  setkey(my_dt, W1, W2)
  my_dt[ , code:= 1:nrow(my_dt)]
  return(my_dt)
}
convert_bigram<-function(my_bigram,row_nbr){
  #return words corresponding with row
  return(unlist(my_bigram[which(my_bigram$code==row_nbr), .(W1, W2)]))
}
convert_bigram<-function(my_bigram,my_W1,my_W2){
  #return code corresponding with 2 words, namely my_w1 and my_W2
  my_list<-list(W1=my_W1,W2=my_W2)
  return(my_bigram[my_list]$code)
}

#------------------------------------------------------------------------------#
#                           Trigram functions                                  #
#------------------------------------------------------------------------------#
create_trigram<-function(my_dt){
  my_dt = data.table(
    W1 = my_dt$W1, 
    cW1 = my_dt$cW1,
    W2= rotate(my_dt$W1),
    cW2= rotate(my_dt$cW1),
    W3= rotate(rotate(my_dt$W1)),
    cW3= rotate(rotate(my_dt$cW1))
  )
  setkey(my_dt, W1, W2, W3)
  my_dt[, cW1W2W3:= .N, by = .(W1, W2, W3)]
  my_dt = my_dt[W2 != start_sentence_string_filtered] 
  my_dt = my_dt[W3 != start_sentence_string_filtered]
  my_dt = unique(my_dt)
  nbr3Gram = as.numeric(sum(my_dt$cW1W2W3))
  
  #perform Good Turing and Katz backoff
  gt = goodTuring(my_dt$cW1W2W3) #this yields a NaN
  setkey(my_dt, cW1W2W3)
  
  gtFreq = rep(gt$proportion, gt$n)
  
  my_dt[ , dcW1W2W3:= gtFreq * nbr3Gram]
  
  bigram<-readRDS(file = paste(my_directory,"bigram.Rds",sep=""))
  
  setkey(my_dt, W1, W2)
  setkey(bigram, W1, W2)
  
  my_dt[ , W1W2Code:= convert_bigram(bigram,W1, W2)]
  
  setkey(my_dt, W2, W3)
  setkey(bigram, W1, W2)
  
  my_dt[ , W2W3Code:= convert_bigram(bigram,W2, W3)]
  
  setkey(bigram, code)
  setkey(my_dt, W1, W2)
  
  my_dt[ , cW1W2:=bigram[.(W1W2Code)]$cW1W2]
  setkey(my_dt, W2, W3)
  my_dt[ , pW2W3:= bigram[.(W2W3Code)]$pW1W2]
  
  setkey(my_dt, W1,W2,W3)
  my_dt[ , pW1W2W3:= dcW1W2W3/cW1W2]
  my_dt[ , beta:= 1 - sum(pW1W2W3), by=W1W2Code]
  my_dt[ , alpha:= beta/(1 - sum(pW2W3)), by=W1W2Code]
  my_dt[ , code:= 1:nrow(my_dt)]
  return(my_dt)
}
convert_trigram<-function(my_trigram,row_nbr){
  #return words corresponding with row
  return(unlist(my_trigram[which(my_trigram$code==row_nbr), .(W1, W2,W3)]))
}
convert_trigram<-function(my_trigram,my_W1,my_W2,my_W3){
  #return code corresponding with 2 words, namely my_w1, my_W2 and my_W3
  my_list<-list(W1=my_W1,W2=my_W2,W3=my_W3)
  return(my_trigram[my_list]$code)
}
#------------------------------------------------------------------------------#
#                           Fourgram functions                                 #
#------------------------------------------------------------------------------#
create_fourgram<-function(my_dt){
  my_dt = data.table(
    W1 = my_dt$W1, 
    cW1 = my_dt$cW1,
    W2= rotate(my_dt$W1),
    cW2= rotate(my_dt$cW1),
    W3= rotate(rotate(my_dt$W1)),
    cW3= rotate(rotate(my_dt$cW1)),
    W4=rotate(rotate(rotate(my_dt$W1))),
    cW4=rotate(rotate(rotate(my_dt$cW1)))
  )
  setkey(my_dt, W1, W2, W3,W4)
  my_dt[, cW1W2W3W4:= .N, by = .(W1, W2, W3,W4)]
  my_dt = my_dt[W2 != start_sentence_string_filtered] 
  my_dt = my_dt[W3 != start_sentence_string_filtered]
  my_dt = my_dt[W4 != start_sentence_string_filtered]
  my_dt = unique(my_dt)
  nbr4Gram = as.numeric(sum(my_dt$cW1W2W3W4))
  
  #perform Good Turing and Katz backoff
  gt = goodTuring(my_dt$cW1W2W3W4) 
  setkey(my_dt, cW1W2W3W4)
  
  gtFreq = rep(gt$proportion, gt$n)
  
  my_dt[ , dcW1W2W3W4:= gtFreq * nbr4Gram]
  
  trigram<-readRDS(file = paste(my_directory,"trigram.Rds",sep=""))
  setkey(my_dt, W1, W2,W3)
  setkey(trigram, W1, W2, W3)
  my_dt[ , W1W2W3Code:= convert_trigram(trigram,W1,W2,W3)]
  setkey(my_dt, W2, W3, W4)
  my_dt[ , W2W3W4Code:= convert_trigram(trigram,W2, W3, W4)]
  
  setkey(trigram, code)
  setkey(my_dt, W1, W2, W3)
  my_dt[ , cW1W2W3:=trigram[.(W1W2W3Code)]$cW1W2W3]
  setkey(my_dt, W2, W3, W4)
  my_dt[ , pW2W3W4:= trigram[.(W2W3W4Code)]$pW1W2W3]
  
  setkey(my_dt, W1,W2,W3)
  my_dt[ , pW1W2W3W4:= dcW1W2W3W4/cW1W2W3]
  my_dt[ , beta:= 1 - sum(pW1W2W3W4), by=W1W2W3Code]
  my_dt[ , alpha:= beta/(1 - sum(pW2W3W4)), by=W1W2W3Code]
  my_dt[ , code:= 1:nrow(my_dt)]
  return(my_dt)
}
