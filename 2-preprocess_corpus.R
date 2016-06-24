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
#                           preprocess_corpus.R                                #
#------------------------------------------------------------------------------#

source("0-general.R")

corpus<-readRDS(file = paste(my_directory,"sample_corpus.Rds",sep=""))

words<-extract_words(corpus)
cleanwords<-clean_corpus(words)

dt = data.table(W1 = cleanwords)
dt[ , cW1:=.N, by=W1]

saveRDS(dt, file = paste(my_directory,"cleanwords.Rds",sep=""))


extract_words=function(my_df){
  sentences<-unlist(stri_split_boundaries(my_df, type="sentence"))
  names(sentences)<-NULL
  
  sentences<-tolower(sentences)
  sentences<-paste(start_sentence_string, sentences)
  words<-unlist(stri_extract_all_words(sentences))
  return(words)
}

clean_corpus=function(my_words){
  #remove years
  #cleanwords = stri_replace_all(my_words, replacement = "YR", regex = "^[12]{1}[0-9]{3}$")
  cleanwords<-stri_replace_all(my_words, replacement = "", regex = "^[12]{1}[0-9]{3}$")
  #remove digits
  #cleanwords = stri_replace_all(cleanwords, replacement = "NBR", regex = "^[[:digit:]]+(,[[:digit:]]*)*\\.*[[:digit:]]*$")
  cleanwords<-stri_replace_all(cleanwords, replacement = "", regex = "^[[:digit:]]+(,[[:digit:]]*)*\\.*[[:digit:]]*$")
  #remove punctuation
  cleanwords<-stri_replace_all(cleanwords, replacement = "", regex = "[\\[\\]\\$\\*\\+\\.\\?\\^\\{\\}\\|\\(\\)\\#%&~_/<=>✬!,:;❵@]")
  #fix apostrophe
  cleanwords<-stri_replace_all(cleanwords, replacement = "'", fixed = "’")
  #remove non-English characters
  cleanwords<-stri_replace_all(cleanwords, replacement = "", regex = "[^A-Za-z'-[:space:]]")
  #perform profanity filtering
  #badwords<-readLines("data/bad_words.txt")
  #for(i in 1:length(badwords)){
    #cleanwords<-gsub(badwords[i],"",cleanwords,perl = T)
  #}
  
  #remove empty words
  empty<-((cleanwords == "")|(cleanwords=="'"))
  cleanwords<-cleanwords[!empty]
  return(cleanwords)
}