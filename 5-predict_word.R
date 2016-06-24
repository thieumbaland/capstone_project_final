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
#                           predict_word.R                                    #
#------------------------------------------------------------------------------#

source("0-general.R")

unigram<-readRDS(file = paste(my_directory,"unigram_shiny.Rds",sep=""))
bigram<-readRDS(file = paste(my_directory,"bigram_shiny.Rds",sep=""))
trigram<-readRDS(file = paste(my_directory,"trigram_shiny.Rds",sep=""))
fourgram<-readRDS(file = paste(my_directory,"fourgram_shiny.Rds",sep=""))


predict_unigram<-function(previous_words="",nbr_predicted_words=default_predicted_words){
  unigram_filter<-unigram[!(W1 %in% previous_words),]
  unigram_filter<-unigram_filter[order(pW1,decreasing=T)]
  
  words<-unigram_filter$W1[1:min(nrow(unigram_filter),nbr_predicted_words)]
  probabilities<-unigram_filter$pW1[1:min(nrow(unigram_filter),nbr_predicted_words)]
  
  my_list<-list(words,probabilities)
  return(my_list)
}

predict_bigram<-function(my_words,previous_words="",nbr_predicted_words=default_predicted_words){
  
  bigram_filter<-bigram[!(W2 %in% previous_words)]
  bigram_filter<-bigram_filter[.(my_words)]
  bigram_filter<-bigram_filter[!is.na(bigram_filter$pW1W2),]
  
  if(nrow(bigram_filter)>0){
    
    bigram_filter<-bigram_filter[order(pW1W2,decreasing=T)]
    words<-bigram_filter$W2[1:min(nrow(bigram_filter),nbr_predicted_words)]
    probabilities<-bigram_filter$pW1W2[1:min(nrow(bigram_filter),nbr_predicted_words)]
    
    unigram_list = predict_unigram(c(previous_words, words), nbr_predicted_words)
    words = c(words, unigram_list$words)
    probabilities = c(probabilities, bigram_filter$alpha[1] * unigram_list[[2]])  
    
  } else {
    
    # my_words is an unseen word
    unigram_list<-predict_unigram(previous_words, nbr_predicted_words)
    words<-unigram_list[[1]]
    probabilities<-unigram_list[[2]]
    
  }
  
  my_list<-list(words,probabilities)
  return(my_list)
}

predict_trigram<-function(my_words,previous_words="",nbr_predicted_words=default_predicted_words){
  
  my_W1<-my_words[1]
  my_W2<-my_words[2]
  
  trigram_filter<-trigram[!(W3 %in% previous_words)]
  trigram_filter<-trigram_filter[.(my_W1,my_W2)]
  trigram_filter<-trigram_filter[!is.na(trigram_filter$pW1W2W3),]
  
  if(nrow(trigram_filter)>0){
    
    trigram_filter<-trigram_filter[order(pW1W2W3,decreasing=T)]
    words<-trigram_filter$W3[1:min(nrow(trigram_filter),nbr_predicted_words)]
    probabilities<-trigram_filter$pW1W2W3[1:min(nrow(trigram_filter),nbr_predicted_words)]
    
    bigram_list = predict_bigram(my_W2,c(previous_words, words), nbr_predicted_words)
    words = c(words, bigram_list[[1]])
    probabilities = c(probabilities, trigram_filter$alpha[1] * bigram_list[[2]])  
    
  } else {
    
    # my_words is an unseen word
    bigram_list<-predict_bigram(my_W2,previous_words, nbr_predicted_words)
    words<-bigram_list[[1]]
    probabilities<-bigram_list[[2]]
    
  }
  
  my_list<-list(words,probabilities)
  return(my_list)
}

predict_fourgram<-function(my_words,previous_words="",nbr_predicted_words=default_predicted_words){
  
  my_W1<-my_words[1]
  my_W2<-my_words[2]
  my_W3<-my_words[3]
  
  fourgram_filter<-fourgram[!(W4 %in% previous_words)]
  fourgram_filter<-fourgram_filter[.(my_W1,my_W2,my_W3)]
  fourgram_filter<-fourgram_filter[!is.na(fourgram_filter$pW1W2W3W4),]
  
  if(nrow(fourgram_filter)>0){
    
    fourgram_filter<-fourgram_filter[order(pW1W2W3W4,decreasing=T)]
    words<-fourgram_filter$W3[1:min(nrow(fourgram_filter),nbr_predicted_words)]
    probabilities<-fourgram_filter$pW1W2W3W4[1:min(nrow(fourgram_filter),nbr_predicted_words)]
    
    trigram_list = predict_trigram(c(my_W2,my_W3),c(previous_words, words), nbr_predicted_words)
    words = c(words, trigram_list[[1]])
    probabilities = c(probabilities, fourgram_filter$alpha[1] * trigram_list[[2]])  
    
  } else {
    
    # my_words is an unseen word
    trigram_list<-predict_trigram(c(my_W2,my_W3),previous_words, nbr_predicted_words)
    words<-trigram_list[[1]]
    probabilities<-trigram_list[[2]]
    
  }
  
  my_list<-list(words,probabilities)
  return(my_list)
}

profanity_filter<-function(my_vector){
  badwords<-readLines("data/bad_words.txt")
  return(which(my_vector %in% badwords))
}
profanity_filter_words<-function(my_vector){
  #returns the actual words to remove, not the index of the elements
  badwords<-readLines("data/bad_words.txt")
  return(my_vector[(my_vector %in% badwords)==F])
}

main<-function(my_string,nbr_predicted_words=default_predicted_words){
  my_words<-clean_string(my_string)
  nbr_words<-as.numeric(length(my_words))
  
  if(nbr_words>=3){
    my_words<-my_words[(nbr_words-2):nbr_words]
    tmp<-predict_fourgram(my_words=my_words,nbr_predicted_words=nbr_predicted_words)
  }else{
    if(nbr_words==2){
      #predict with trigram
      my_words<-my_words[(nbr_words-1):nbr_words]
      tmp<-predict_trigram(my_words=my_words,nbr_predicted_words=nbr_predicted_words)
    }else{
      if(nbr_words==1){
        #predict with bigram
        tmp<-predict_bigram(my_words=my_words,nbr_predicted_words=nbr_predicted_words) 
      }else{
        #predict with unigram
        tmp<-predict_unigram(nbr_predicted_words = nbr_predicted_words)
      }
    }
  }
  
  #perform profanity filtering on predicted words
  words_to_remove<-profanity_filter(tmp[[1]])
  if(length(words_to_remove)!=0){
    tmp[[1]]<-tmp[[1]][!words_to_remove]
    tmp[[2]]<-tmp[[2]][!words_to_remove]
  }
  tmp[[1]]<-tmp[[1]][1:min(length(tmp[[1]]),nbr_predicted_words)]
  tmp[[2]]<-tmp[[2]][1:min(length(tmp[[2]]),nbr_predicted_words)]
  #now we have the predicted words and probabilities!
  return(tmp)
}

clean_string<-function(my_string){
  my_string = tolower(my_string)
  my_string = paste(start_sentence_string, my_string)
  my_words = unlist(stri_extract_all_words(my_string))
  
  #remove years
  cleanwords<-stri_replace_all(my_words, replacement = "", regex = "^[12]{1}[0-9]{3}$")
  
  #remove digits
  cleanwords<-stri_replace_all(cleanwords, replacement = "", regex = "^[[:digit:]]+(,[[:digit:]]*)*\\.*[[:digit:]]*$")
  
  #remove punctuation
  cleanwords<-stri_replace_all(cleanwords, replacement = "", regex = "[\\[\\]\\$\\*\\+\\.\\?\\^\\{\\}\\|\\(\\)\\#%&~_/<=>✬!,:;❵@]")
  
  #fix apostrophe
  cleanwords<-stri_replace_all(cleanwords, replacement = "'", fixed = "’")
  
  #remove non-English characters
  cleanwords<-stri_replace_all(cleanwords, replacement = "", regex = "[^A-Za-z'-[:space:]]")
  
  #remove profanity words
  cleanwords<-profanity_filter_words(cleanwords)
  
  #remove empty words
  empty = (cleanwords == "")|(cleanwords=="'")
  
  cleanwords = cleanwords[!empty]
  return(cleanwords)
}