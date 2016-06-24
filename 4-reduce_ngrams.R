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
#                           reduce_ngrams.R                                    #
#------------------------------------------------------------------------------#

source("0-general.R")

#------------------------------------------------------------------------------#
#                           Unigram                                            #
#------------------------------------------------------------------------------#

unigram<-readRDS(file = paste(my_directory,"unigram.Rds",sep=""))
unigram<-unigram[cW1>=cutoff_count,]
saveRDS(unigram,file=paste(my_directory,"unigram_shiny.Rds",sep=""))
unigram<-NULL

#------------------------------------------------------------------------------#
#                           Bigram                                             #
#------------------------------------------------------------------------------#

bigram<-readRDS(file = paste(my_directory,"bigram.Rds",sep=""))
bigram[ , c("cW1", "cW2", "dcW1W2", "pW1", "pW2","beta") := NULL]
bigram<-bigram[cW1W2>=cutoff_count,]
saveRDS(bigram,file=paste(my_directory,"bigram_shiny.Rds",sep=""))
bigram<-NULL

#------------------------------------------------------------------------------#
#                           Trigram                                            #
#------------------------------------------------------------------------------#

trigram<-readRDS(file = paste(my_directory,"trigram.Rds",sep=""))
trigram[ , c("cW1", "cW2", "cW3", "dcW1W2W3", "W1W2Code", "cW1W2", "W2W3Code", "pW2W3","beta") := NULL]
trigram<-trigram[cW1W2W3>=cutoff_count,]
saveRDS(trigram,file=paste(my_directory,"trigram_shiny.Rds",sep=""))
trigram<-NULL

#------------------------------------------------------------------------------#
#                           Fourgram                                           #
#------------------------------------------------------------------------------#

fourgram<-readRDS(file = paste(my_directory,"fourgram.Rds",sep=""))
fourgram[ , c("cW1", "cW2", "cW3", "cW4","dcW1W2W3W4", "W1W2W3Code", "W2W3W4Code","cW1W2W3","pW2W3W4","beta") := NULL]
fourgram<-fourgram[cW1W2W3W4>=cutoff_count,]
saveRDS(fourgram,file=paste(my_directory,"fourgram_shiny.Rds",sep=""))
fourgram<-NULL