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
#                           sample_corpus.R                                    #
#------------------------------------------------------------------------------#

source("0-general.R")

blogs = paste(my_directory,lang,".blogs.txt",sep="")
news = paste(my_directory,lang,".news.txt",sep="")
twitter = paste(my_directory,lang,".twitter.txt",sep="")

blogs = readLines(blogs, skipNul = TRUE)
news = readLines(news, skipNul = TRUE)
twitter = readLines(twitter, skipNul = TRUE)
corpus=c(blogs,news,twitter)

nbrlines = round(length(corpus) * sample_size, 0)
samplelines = sort(sample(1:length(corpus), nbrlines, replace = FALSE))

sample_corpus = corpus[samplelines]

saveRDS(sample_corpus, file = paste(my_directory,"sample_corpus.Rds",sep=""))


        