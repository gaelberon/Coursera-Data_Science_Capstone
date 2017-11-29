# Data Science Capstone project source code file
library(shiny)
#library(ggplot2)
library(shinydashboard)
#library(car)
#library(DT)
#library(ggExtra)
library(R.utils)
library(stringi)
library(knitr)
library(tokenizers)
library(dplyr)
library(caret)
#library(tm)
#library(qdap)
library(ngram)
#library(wordcloud)
#library(ggplot2)
#library(gridExtra)
#library(gridGraphics)
#library(gridBase)

####################################
## Set global settings and variables
####################################

## Input variables from ui
nb_words_to_predict <- NULL
captured_text <- NULL

# Set seed, working directory and references to data files
set.seed(123456)
proc_time_per_treatment <- data.frame(matrix(ncol = 4, nrow = 0))
names(proc_time_per_treatment) <- c("treatment", "user", "system", "elapsed")

setwd("/Users/gaelberon/Documents/Coursera/Data_Science_Capstone")
fileNameList <- c("en_US.blogs.txt",
                  "en_US.twitter.txt",
                  "en_US.news.txt")
fileList <- c(
        paste0(getwd(), "/final/en_US/", fileNameList[1]),
        paste0(getwd(), "/final/en_US/", fileNameList[2]),
        paste0(getwd(), "/final/en_US/", fileNameList[3])
)

# Data frame containing the data input files details
#file_details <- data.frame(matrix(ncol = 9, nrow = 0))

# Initialize the sampling factor for study data set
sampling_factor <- .1
# Initialize the raw data set
original_raw_data <- NULL
raw_data <- NULL
# Out of the random sample, split into the training, testing and validation data sets
training_factor <- .6
testing_factor <- .3
validation_factor <- .1
# Initialize the training, testing and validation data sets
training <- NULL
testing <- NULL
validation <- NULL

##########################
## Define global functions
##########################

# Data frame containing the training, testing and validation data sets details
#datasets_details <- data.frame(matrix(ncol = 5, nrow = 0))

## set_proc_time_per_treatment: Updates data frame 'proc_time_per_treatment' with
## elapsed time between given parameters 'start_time' and 'stop_time'. Linked
## this data to the treatment's name passed as a characters' string parameter
## proc_time_per_treatment = input data frame to be updated
## treatment_name = input characters string with given name for the processed treatment
## start_time = input recorded time at the begining of the treatment as 'proc.time' format
## stop_time = input recorded time at the end of the treatment as 'proc.time' format
## proc_time_per_treatment = output updated data frame

#set_proc_time_per_treatment <- function(df_to_update,
#                                        treatment_name,
#                                        start_time,
#                                        stop_time) {
#        # calculate the elapsed time between start and stop
#        elapsed_time <- stop_time - start_time
#        
#        # update the data frame 'proc_time_per_treatment' with
#        # given name for the processed treatment, and elapsed
#        # time between start and stop
#        updated_df <- rbind(df_to_update,
#                            data.frame(treatment = treatment_name,
#                                       user = elapsed_time[[1]],
#                                       system = elapsed_time[[2]],
#                                       elapsed = elapsed_time[[3]]))
#        
#        # return the updated data frame 'proc_time_per_treatment'
#        return(updated_df)
#        
#}

## read_txt_file: Reads a text file using function 'readLines' of package 'R.utils'
## and loading its output into a vector whose elements are lines in the file.
##
## fileName = input text fileName in the working directory
## warn = FALSE, skip warnings of missing EOF
## encoding = "UTF-8", encoding type for the file to read
## skipNul = TRUE, skip warnings of embedded nul
## textVector = output a vector of text
##
read_txt_file <- function(fileName, warn = FALSE, encoding = "UTF-8", skipNul = TRUE) {
        textFile <- file(fileName, open = "r")
        textVector <- readLines(textFile,
                                warn = warn,
                                encoding = encoding,
                                skipNul = skipNul)
        close(textFile)
        return(textVector)
}

## clean_dataset: Removes English stopwords from a vector of charater strings using packages 'tm'.
## 
## dataset = input vector of character strings to be cleand
## dataset = output vector cleaned from all unexpected data
##
remove_stop_words <- function(dataset) {
        ## Remove English stopwords if needed
        dataset <- tm_map(dataset, removeWords, stopwords("en"))
        
        return(dataset)
}

## clean_dataset: Cleans a vector of charater strings using packages 'tm' and 'qdap'.
## For each character strings:
## - Convert to plain text document
## - Convert to lower case
## - Replace contractions with their full forms
## - Remove profanities
## - Remove numbers and punctuation
## - Strip white spaces
##
## dataset = input vector of character strings to be cleand
## dataset = output vector cleaned from all unexpected data
##
clean_dataset <- function(dataset) {
        ## Convert to plain text document
        dataset <- tm_map(dataset, PlainTextDocument)
        
        ## Convert to lower case
        dataset <- tm_map(dataset, content_transformer(tolower))
        
        ## Replace contractions with their full forms
        dataset <- tm_map(dataset, content_transformer(replace_contraction))
        
        ## Remove profanities
        
        ## 1. Read Google naughty word list
        ## from https://gist.github.com/ryanlewis/a37739d710ccdb4b406d
        banned_words <- read_txt_file(paste0(getwd(),"/google_twunter_lol.txt"))
        ## 2. Remove Google banned words from our dataset
        dataset <- tm_map(dataset, removeWords, banned_words)
        
        ## Remove numbers, puntuation and strip white space
        
        ## Replace non alphabetic characters with spaces
        ## toSpace from onepager.togaware.com/TextMiningO.pdf
        to_space <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
        dataset <- tm_map(dataset, to_space, "[^a-zA-Z|\'s]")
        
        ### LIMITATIONS :
        ##    - do not separate sentences
        ##    - [OK now] do not manage the " 's " cases, for instance
        ##    - 
        ##    - 
        
        ## Strip white space
        dataset <- tm_map(dataset, stripWhitespace)
        
        return(dataset)
}

# Read the input data files from variable fileList and build:
# file_details: a data frame containing global analysis
# raw_data: a vector that contains a random sample of each input data file mixed together
for (i in 1:length(fileList)) {
        # Iterate on files in fileList
        file <- fileList[i]
        file_lines <- read_txt_file(fileName = file)
#        nb_lines <- length(file_lines)
        #print(paste0("Nb of lines in the file ", fileNameList[i], " -> ", nb_lines))
#        min_length <- min(nchar(file_lines))
#        max_length <- max(nchar(file_lines))
#        nb_words_lines <- stri_count_words(file_lines)
#        nb_words <- sum(nb_words_lines)
        #print(paste0("Nb of words in the file ", fileNameList[i], " -> ", nb_words))
#        min_nb_words <- min(nb_words_lines)
#        max_nb_words <- max(nb_words_lines)
        
        # Update the files details dataframe
#        file_details <- rbind(file_details,
#                              data.frame(file = fileNameList[i],
#                                         file_size_Mb = round(file.info(file)$size/1024/1024,2),
#                                         nb_lines = nb_lines,
#                                         nb_words = nb_words,
#                                         words_per_line = round(nb_words/nb_lines,2),
#                                         shortest_line = min_length,
#                                         min_nb_words = min_nb_words,
#                                         longest_line = max_length,
#                                         max_nb_words = max_nb_words))
        
        # Sample the current read file and split the sample into training,
        # testing and validation data sets
        file_sample <- sample(file_lines,
                              size = length(file_lines)*sampling_factor,
                              replace = FALSE)
        # Combine samples of all the 3 data files (blogs, twitter and news) into
        # the vector 'raw_data'
        original_raw_data <- c(original_raw_data, file_sample)
}
#file_details <- format(file_details, nsmall = 1, big.mark = ",")

# Permutate lines into the 'raw_data' vector containing the combined samples
raw_data <- sample(original_raw_data, size = length(original_raw_data), replace = FALSE)

#kable(file_details, format = "markdown")

# Partitioning the 'raw_data' vector into 'testing', and then 'training' and 'validation' data sets
#start_time <- proc.time()
raw_data_non_testing <- NULL
if (testing_factor != 0) {
        in_testing <- createDataPartition(seq_len(NROW(raw_data)),
                                          p = testing_factor,
                                          list = FALSE)
        testing <- raw_data[in_testing]
        raw_data_non_testing <- raw_data[-in_testing]
} else {
        raw_data_non_testing <- raw_data
}

#in_training <- raw_data == raw_data
in_training <- createDataPartition(seq_len(NROW(raw_data_non_testing)),
                                   p = training_factor/(training_factor+validation_factor),
                                   list = FALSE)
training <- raw_data_non_testing[in_training]
validation <- raw_data_non_testing[-in_training]

# Calculate ratios for number of lines in the 'training', 'testing' and 'validation'
# datasets out of the total number of lines
ratio_training <- length(training)/(length(training)+length(testing)+length(validation))
ratio_testing <- length(testing)/(length(training)+length(testing)+length(validation))
ratio_validation <- length(validation)/(length(training)+length(testing)+length(validation))
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Create training, testing and validation data sets",
#        start_time,
#        stop_time)

# Save data to files and remove temporary data
#save(original_raw_data, file_details, file = "original_data.rda")
#save(training, testing, validation, datasets_details, file = "sample_data.rda")
rm(in_testing, raw_data_non_testing, in_training, original_raw_data, raw_data)

# Convert training dataset to corpus
trainCorpus <- SimpleCorpus(VectorSource(training))
# Inspect a few lines
#control_sample <- 3
#control_lines <- sample(1:length(trainCorpus), control_sample)
#inspect(trainCorpus[control_lines])

# Call clean_dataset function to proceed with cleaning actions
#start_time <- proc.time()
trainCorpus <- clean_dataset(trainCorpus)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to clean_dataset function",
#        start_time,
#        stop_time)
# Inspect the same few lines again, after cleaning
#inspect(trainCorpus[control_lines])

# Call remove_stop_words function to remove English stop words
#start_time <- proc.time()
trainCorpusNoStopWords <- remove_stop_words(trainCorpus)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to remove_stop_words function",
#        start_time,
#        stop_time)
# Inspect the same few lines again, after removing the English stop words
#inspect(trainCorpus[control_lines])

# Convert testing dataset to corpus
testCorpus <- SimpleCorpus(VectorSource(testing))
# Call clean_dataset function to proceed with cleaning actions
#start_time <- proc.time()
testCorpus <- clean_dataset(testCorpus)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Testing data set - Call to clean_dataset function",
#        start_time,
#        stop_time)
# Call remove_stop_words function to remove English stop words
#start_time <- proc.time()
testCorpusNoStopWords <- remove_stop_words(testCorpus)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Testing data set - Call to remove_stop_words function",
#        start_time,
#        stop_time)

# Convert validation dataset to corpus
validationCorpus <- SimpleCorpus(VectorSource(validation))
# Call clean_dataset function to proceed with cleaning actions
#start_time <- proc.time()
validationCorpus <- clean_dataset(validationCorpus)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Validation data set - Call to clean_dataset function",
#        start_time,
#        stop_time)
# Call remove_stop_words function to remove English stop words
#start_time <- proc.time()
validationCorpusNoStopWords <- remove_stop_words(validationCorpus)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Validation data set - Call to remove_stop_words function",
#        start_time,
#        stop_time)

#datasets_details <- data.frame(
#        dataset = c("training", "testing", "validation"),
#        ratio_total_nb_lines = c(
#                paste0(round(ratio_training, 1)*100,"%"),
#                paste0(round(ratio_testing, 1)*100,"%"),
#                paste0(round(ratio_validation, 1)*100,"%")),
#        nb_lines = c(length(training),
#                     length(testing),
#                     length(validation)),
#        nb_words = c(sum(stri_count_words(training)),
#                     sum(stri_count_words(testing)),
#                     sum(stri_count_words(validation))),
#        words_per_line = c(
#                round(sum(stri_count_words(training))/length(training), 2),
#                round(sum(stri_count_words(testing))/length(testing), 2),
#                round(sum(stri_count_words(validation))/length(validation), 2)),
#        nb_words_after_cleaning = c(sum(stri_count_words(concatenate(trainCorpus$content))),
#                                    sum(stri_count_words(concatenate(testCorpus$content))),
#                                    sum(stri_count_words(concatenate(validationCorpus$content)))),
#        nb_words_no_stop_words = c(sum(stri_count_words(concatenate(trainCorpusNoStopWords$content))),
#                                   sum(stri_count_words(concatenate(testCorpusNoStopWords$content))),
#                                   sum(stri_count_words(concatenate(validationCorpusNoStopWords$content)))))

#datasets_details$nb_lines <- as.numeric(datasets_details$nb_lines)
#datasets_details$nb_words <- as.numeric(datasets_details$nb_words)
#datasets_details$words_per_line <- as.numeric(datasets_details$words_per_line)
#datasets_details$nb_words_after_cleaning <- as.numeric(datasets_details$nb_words_after_cleaning)
#datasets_details$nb_words_no_stop_words <- as.numeric(datasets_details$nb_words_no_stop_words)

#kable(datasets_details, format = "markdown")

## getNgram: extracts ngrams with the ngram library
## corpus = input, a SimpleCorpus
## n = input, number of word in ngrams
## ng = output, a dataframe of ngram, freq, and probability
##
getNgram <- function(corpus, n = 2) {
        ## Convert corpus to a string
        str <- concatenate(corpus$content)
        ngrams <- ngram(str = str, n = n)
        return(get.phrasetable(ngrams))
}

## getNGramSummary
getNGramSummary <- function(ngram) {
        # Initialize the output 'nGramSummary' data set
        nGramSummary <- data.frame()
        
        # Calculate the total number of words into training data set and
        # total number of unique words into ngram data set
        nb_words_total <- datasets_details[1, 'nb_words']
        nb_unique_words_total <- dim(ngram)[1]
        #print("#################################################")
        #print(dim(ngram))
        #print(head(ngram))
        #print(paste0("##### TOTAL NB of WORDS: ", nb_words_total, " ######"))
        #print(paste0("##### TOTAL NB of UNIQUE WORDS: ", nb_unique_words_total, " ######"))
        
        loop <- TRUE
        iter <- 1
        nb_words <- 0
        cumul_freq <- 0
        curr_ratio <- .1
        
        # 
        while (loop) {
                nb_words <- nb_words + 1
                word_freq <- ngram[iter, 'freq']
                cumul_freq <- cumul_freq + word_freq
                ratio_freq <- cumul_freq / nb_words_total
                ratio_nb_w_oo_nb_unique_w <- round(nb_words / nb_unique_words_total, 4)
                
                #print("#################################################")
                #print(paste0("##### NB of WORDS: ", nb_words, " ######"))
                #print(paste0("##### WORD FREQUENCY: ", word_freq, " ######"))
                #print(paste0("##### FREQUENCY of the WORD OUT OF GRAND TOTAL: ", round(ratio_freq, 2), " ######"))
                
                if (ratio_freq > 0 & ratio_freq >= curr_ratio) {
                        nGramSummary <- rbind(
                                nGramSummary,
                                data.frame(
                                        "nb_words" = nb_words,
                                        "frequency_words / nb_words_total" = 
                                                paste0(round(ratio_freq,2)*100, "%"),
                                        "cumulated_frequency" = cumul_freq,
                                        "nb_words / nb_unique_words" = 
                                                paste0(ratio_nb_w_oo_nb_unique_w*100, "%")))
                        curr_ratio <- curr_ratio + .1
                        if (curr_ratio == 1 | nb_words > 4999) loop <- FALSE
                }
                
                # increment the iterator
                iter <- iter + 1
                #if (iter == 101) loop <- FALSE
        }
        
        # return the built dataframe with NGram summary
        return(nGramSummary)
}

# PROCEEDING WITH GETTING NGRAM (1) WITH ENGLISH STOP WORDS
#start_time <- proc.time()
train1Gram <- getNgram(trainCorpus, n = 1)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNgram function for 1 gram with English stop words",
#        start_time,
#        stop_time)

# PROCEEDING WITH GETTING NGRAM (1) WITHOUT ENGLISH STOP WORDS
#start_time <- proc.time()
train1GramNoStopWords <- getNgram(trainCorpusNoStopWords, n = 1)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNgram function for 1 gram without English stop words",
#        start_time,
#        stop_time)

# Analyse the frequency of words into train1Gram data set
#start_time <- proc.time()
#train1GramSummary <- getNGramSummary(train1Gram)
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNGramSummary function for 1 gram with English stop words",
#        start_time,
#        stop_time)

# Analyse the frequency of words into train1GramNoStopWords data set
#start_time <- proc.time()
#train1GramNoStopWordsSummary <- getNGramSummary(train1GramNoStopWords)
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNGramSummary function for 1 gram without English stop words",
#        start_time,
#        stop_time)

#kable(train1GramSummary, format = "markdown")

#kable(train1GramNoStopWordsSummary, format = "markdown")

#par(mfrow=c(1, 2),  mai = c(.1, .1, .1, .1))

#wordcloud(train1Gram$ngram,
#          train1Gram$freq,
#          scale = c(5, 1), 
#          max.words = 20,
#          random.order = FALSE,use.r.layout = TRUE)

#plot.new()

#histTrain1Gram <- train1Gram[1:20,]
#hist_train1Gram <- ggplot(data = histTrain1Gram,
#                          aes(x = ngrams, y = freq)) + #, fill=rainbow(20))) +
#        geom_bar(stat = "identity") +
#        scale_x_discrete(limits = histTrain1Gram$ngrams) +
#        theme_bw() +
#        theme(panel.grid.major.y = element_line(colour = "black", linetype = 3, size = .5),
#              panel.background = element_blank(),
#              axis.title.x = element_text(size=12),
#              axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1),
#              axis.title.y = element_text(size=12, angle = 90),
#              axis.text.y = element_text(size=10),
#              strip.background = element_rect(color="white", fill="white"),
#              strip.text = element_text(size=12))

#vps <- baseViewports()
#pushViewport(vps$inner, vps$figure, vps$plot)
#grid.text(names(histTrain1Gram$ngrams),
#          x = unit(histTrain1Gram$ngrams, "native"),
#          y=unit(-1, "lines"),
#          just="right", rot=50)
#popViewport(3)

#print(vp = vpStack(vps$figure, vps$plot), hist_train1Gram)

#par(mfrow=c(1, 2),  mai = c(.1, .1, .1, .1))

#wordcloud(train1GramNoStopWords$ngram,
#          train1GramNoStopWords$freq,
#          scale = c(5, 1), 
#          max.words = 20,
#          random.order = FALSE,use.r.layout = TRUE)

#plot.new()
#vps <- baseViewports()

#histTrain1GramNoStopWords <- train1GramNoStopWords[1:20,]
#hist_train1GramNoStopWords <- ggplot(data = histTrain1GramNoStopWords,
#                                     aes(x = ngrams, y = freq)) +
#        geom_bar(stat = "identity") +
#        scale_x_discrete(limits = histTrain1GramNoStopWords$ngrams) +
#        theme_bw() +
#        theme(panel.grid.major.y = element_line(colour = "black", linetype = 3, size = .5),
#              panel.background = element_blank(),
#              axis.title.x = element_text(size=12),
#              axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1),
#              axis.title.y = element_text(size=12, angle = 90),
#              axis.text.y = element_text(size=10),
#              strip.background = element_rect(color="white", fill="white"),
#              strip.text = element_text(size=12))

#print(vp = vpStack(vps$figure, vps$plot), hist_train1GramNoStopWords)

# PROCEEDING WITH GETTING NGRAM (2) WITH ENGLISH STOP WORDS
#start_time <- proc.time()
train2Gram <- getNgram(trainCorpus, n = 2)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNgram function for 2 gram with English stop words",
#        start_time,
#        stop_time)

# PROCEEDING WITH GETTING NGRAM (2) WITHOUT ENGLISH STOP WORDS
#start_time <- proc.time()
train2GramNoStopWords <- getNgram(trainCorpusNoStopWords, n = 2)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNgram function for 2 gram without English stop words",
#        start_time,
#        stop_time)

#par(mfrow=c(1, 2),  mai = c(.1, .1, .1, .1))

#wordcloud(train2Gram$ngram,
#          train2Gram$freq,
#          scale = c(5, 1), 
#          max.words = 20,
#          random.order = FALSE,use.r.layout = TRUE)

#plot.new()

#histTrain2Gram <- train2Gram[1:20,]
#hist_train2Gram <- ggplot(data = histTrain2Gram,
#                          aes(x = ngrams, y = freq)) +
#        geom_bar(stat = "identity") +
#        scale_x_discrete(limits = histTrain2Gram$ngrams) +
#        theme_bw() +
#        theme(panel.grid.major.y = element_line(colour = "black", linetype = 3, size = .5),
#              panel.background = element_blank(),
#              axis.title.x = element_text(size=12),
#              axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1),
#              axis.title.y = element_text(size=12, angle = 90),
#              axis.text.y = element_text(size=10),
#              strip.background = element_rect(color="white", fill="white"),
#              strip.text = element_text(size=12))

#vps <- baseViewports()
#pushViewport(vps$inner, vps$figure, vps$plot)
#grid.text(names(histTrain2Gram$ngrams),
#          x = unit(histTrain2Gram$ngrams, "native"),
#          y=unit(-1, "lines"),
#          just="right", rot=50)
#popViewport(3)

#print(vp = vpStack(vps$figure, vps$plot), hist_train2Gram)

#par(mfrow=c(1, 2),  mai = c(.1, .1, .1, .1))

#wordcloud(train2GramNoStopWords$ngram,
#          train2GramNoStopWords$freq,
#          scale = c(5, 1), 
#          max.words = 20,
#          random.order = FALSE,use.r.layout = TRUE)

#plot.new()
#vps <- baseViewports()

#histTrain2GramNoStopWords <- train2GramNoStopWords[1:20,]
#hist_train2GramNoStopWords <- ggplot(data = histTrain2GramNoStopWords,
#                                     aes(x = ngrams, y = freq)) +
#        geom_bar(stat = "identity") +
#        scale_x_discrete(limits = histTrain2GramNoStopWords$ngrams) +
#        theme_bw() +
#        theme(panel.grid.major.y = element_line(colour = "black", linetype = 3, size = .5),
#              panel.background = element_blank(),
#              axis.title.x = element_text(size=12),
#              axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1),
#              axis.title.y = element_text(size=12, angle = 90),
#              axis.text.y = element_text(size=10),
#              strip.background = element_rect(color="white", fill="white"),
#              strip.text = element_text(size=12))

#print(vp = vpStack(vps$figure, vps$plot), hist_train2GramNoStopWords)

# PROCEEDING WITH GETTING NGRAM (3) WITH ENGLISH STOP WORDS
#start_time <- proc.time()
train3Gram <- getNgram(trainCorpus, n = 3)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNgram function for 3 gram with English stop words",
#        start_time,
#        stop_time)

# PROCEEDING WITH GETTING NGRAM (3) WITHOUT ENGLISH STOP WORDS
#start_time <- proc.time()
train3GramNoStopWords <- getNgram(trainCorpusNoStopWords, n = 3)
#stop_time <- proc.time()
#proc_time_per_treatment <- set_proc_time_per_treatment(
#        proc_time_per_treatment,
#        "Training data set - Call to getNgram function for 3 gram without English stop words",
#        start_time,
#        stop_time)

#par(mfrow=c(1, 2),  mai = c(.1, .1, .1, .1))

#wordcloud(train3Gram$ngram,
#          train3Gram$freq,
#          scale = c(5, 1), 
#          max.words = 20,
#          random.order = FALSE,use.r.layout = TRUE)

#plot.new()

#histTrain3Gram <- train3Gram[1:20,]
#hist_train3Gram <- ggplot(data = histTrain3Gram,
#                          aes(x = ngrams, y = freq)) +
#        geom_bar(stat = "identity") +
#        scale_x_discrete(limits = histTrain3Gram$ngrams) +
#        theme_bw() +
#        theme(panel.grid.major.y = element_line(colour = "black", linetype = 3, size = .5),
#              panel.background = element_blank(),
#              axis.title.x = element_text(size=12),
#              axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1),
#              axis.title.y = element_text(size=12, angle = 90),
#              axis.text.y = element_text(size=10),
#              strip.background = element_rect(color="white", fill="white"),
#              strip.text = element_text(size=12))

#vps <- baseViewports()
#pushViewport(vps$inner, vps$figure, vps$plot)
#grid.text(names(histTrain3Gram$ngrams),
#          x = unit(histTrain3Gram$ngrams, "native"),
#          y=unit(-1, "lines"),
#          just="right", rot=50)
#popViewport(3)

#print(vp = vpStack(vps$figure, vps$plot), hist_train3Gram)

#par(mfrow=c(1, 2),  mai = c(.1, .1, .1, .1))

#wordcloud(train3GramNoStopWords$ngram,
#          train3GramNoStopWords$freq,
#          scale = c(5, 1), 
#          max.words = 20,
#          random.order = FALSE,use.r.layout = TRUE)

#plot.new()
#vps <- baseViewports()

#histTrain3GramNoStopWords <- train3GramNoStopWords[1:20,]
#hist_train3GramNoStopWords <- ggplot(data = histTrain3GramNoStopWords,
#                                     aes(x = ngrams, y = freq)) +
#        geom_bar(stat = "identity") +
#        scale_x_discrete(limits = histTrain3GramNoStopWords$ngrams) +
#        theme_bw() +
#        theme(panel.grid.major.y = element_line(colour = "black", linetype = 3, size = .5),
#              panel.background = element_blank(),
#              axis.title.x = element_text(size=12),
#              axis.text.x = element_text(size=10, angle=45, hjust=1, vjust=1),
#              axis.title.y = element_text(size=12, angle = 90),
#              axis.text.y = element_text(size=10),
#              strip.background = element_rect(color="white", fill="white"),
#              strip.text = element_text(size=12))

#print(vp = vpStack(vps$figure, vps$plot), hist_train3GramNoStopWords)

#kable(proc_time_per_treatment, format = "markdown")










#############################
## Manage Shiny App interface
#############################

server <- function(input, output) {

        # Load corpus files
        
        # Refreshing data
        # Return the selected 'nb_words_to_predict' variable
        get_nb_words_to_predict <- reactive({
                # get the nb_words_to_predict selected in the sliderInput
                nb_words_to_predict <- input$nb_words_to_predict
                print(nb_words_to_predict)
                return(nb_words_to_predict)
        })
        
        # Refreshing data
        # Return the captured 'text' variable
        get_captured_text <- reactive({
                # get the text captured in the textInput
                captured_text <- input$text
                return(captured_text)
        })
        
        # Refreshing data
        live_data <- reactive({
                nb_words_to_predict <- get_nb_words_to_predict()
                captured_text <- get_captured_text()
        })
        
        # Output captured text
        output$captured_text <- renderText({
                paste0("Here after the ",
                       input$nb_words_to_predict,
                       " best predicted words to follow: '",
                       input$text,
                       "'")
        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
        output$predicted_words_print <- renderPrint({
                "blabla"
        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
        output$predicted_words_table <- renderTable({
                head(train3Gram)
        })
}