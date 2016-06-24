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
#                           general.R                                          #
#------------------------------------------------------------------------------#

#general.R

#libraries
options(java.parameters = "-Xmx12000m")
library(rJava)
library(data.table)
library(stringi)
library(edgeR)

#parameters
myseed<-1234
sample_size<-0.8
cutoff_count<-3
lang<-"en_US"
my_directory<-paste("data/",lang,"/",sep="")
start_sentence_string<-" BEGINOFSENTENCE "
start_sentence_string_filtered<-gsub(pattern=" ",replacement="",start_sentence_string)
default_predicted_words<-7

