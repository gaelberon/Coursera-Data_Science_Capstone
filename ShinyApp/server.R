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
library(tm)
library(qdap)
library(ngram)
#library(wordcloud)
#library(ggplot2)
#library(gridExtra)
#library(gridGraphics)
#library(gridBase)

####################################
## Set global settings and variables
####################################



##########################
## Define global functions
##########################

## remove_stop_words: Removes English stopwords from a vector of charater strings using packages 'tm'.
## 
## dataset = input vector of character strings to be cleand
## dataset = output vector cleaned from all unexpected data
##
remove_stop_words <- function(dataset) {
        ## Remove English stopwords if needed
        dataset <- tm_map(dataset, removeWords, stopwords("en"))
        
        return(dataset)
}

## 
## 
get_n_last_words_from_captured_text <- function(captured_text, nb_words, no_stop_words = FALSE) {
        list_captured_words <- NULL
        if (no_stop_words) {
                list_captured_words <- tokenize_words(x = captured_text, lowercase = TRUE,
                                                      stopwords = stopwords("english"))
        } else {
                list_captured_words <- tokenize_words(x = captured_text, lowercase = TRUE)
        }
#        print("List of captured word(s):")
#        print(list_captured_words)
        
        nb_captured_words <- length(list_captured_words[[1]])
#        print(nb_captured_words)
        
        n_last_words <- NULL
        if (is.null(nb_words) | is.na(nb_words)) {
                n_last_words <- NULL
        } else if (nb_words == 1) {
                n_last_words <- list_captured_words[[1]][nb_captured_words]
        } else {
                for (i in 1:nb_words) {
                        n_last_words <- paste(list_captured_words[[1]][nb_captured_words - i + 1],
                                              n_last_words,
                                              collapse = " ")
                }
                n_last_words <- substring(text = n_last_words, first = 1, last = nchar(n_last_words) - 1)
#                print(n_last_words)
        }
#        print(paste0("the ", nb_words, " last word(s) captured is (are): ", n_last_words))
        return(n_last_words)
}


## look_for_ngrams_from_captured_text: 
## 
look_for_ngrams_from_captured_text <- function(captured_text, from_ngrams, no_stop_words = FALSE) {
        #print(head(from_ngrams))
        #pattern <- paste0("^", tolower(captured_text), " ")
        pattern <- paste0("^", captured_text, " ")
        result_idx <- grep(pattern, from_ngrams$ngrams, value = FALSE)
#        print(head(result_idx))
        result <- from_ngrams[result_idx, ]
#        print("Result including stop words:")
#        print(head(result))
        
        # REMOVE ENGLISH STOP WORDS FROM BEST RESULTS
        if (no_stop_words) {
                result <- result[!(result[, 2] %in% stopwords("en")), ]
#                print("Result excluding stop words:")
#                print(head(result))
        }
        
        #result_tbl <- table(result)
        #print(result_tbl)
        
        # return next words and frequencies
        return(result)
#        return(result[,2:3])
}

## 
## 
get_probabilities <- function(df, col_name) {
        #        df[, "ngrams"]
        #        df[, "next_word"]
        #        df[, "freq"]
        
#        print(colnames(df))
#        print(head(df))
        if (!is.null(df) & nrow(df) > 0) {
                sum_freq <- sum(df[, "freq"])
                df[, col_name] <- df[, "freq"] / sum_freq
                
                max_prob <- df[df[, col_name] == max(df[, col_name]), ]
#                print(paste0("for ngram '",
#                             max_prob[, "ngrams"],
#                             "', the word '",
#                             max_prob[, "next_word"],
#                             "' has the highest probability of '",
#                             max_prob[, "prop"], "'"))
        }
        
        return(df)
}

## get_result:
## 
get_result <- function(captured_text) {
#        print("###########################################")
#        print("1. Try using trigrams excluding English stop words")
        n_last_words <- get_n_last_words_from_captured_text(
                captured_text = captured_text,
                nb_words = 2,
                no_stop_words = TRUE)
        result <- look_for_ngrams_from_captured_text(n_last_words, trigrams)
        
        if (!is.null(result) & nrow(result) > 0) {
                result <- get_probabilities(result)
        } else {
#                print("2. Try using trigrams including English stop words")
                n_last_words <- get_n_last_words_from_captured_text(
                        captured_text = captured_text,
                        nb_words = 2,
                        no_stop_words = FALSE)
                result <- look_for_ngrams_from_captured_text(n_last_words, trigrams)
        }
        if (!is.null(result) & nrow(result) > 0) {
                result <- get_probabilities(result)
        } else {
#                print("3. Try using bigrams excluding English stop words")
                n_last_words <- get_n_last_words_from_captured_text(
                        captured_text = captured_text,
                        nb_words = 1,
                        no_stop_words = TRUE)
                result <- look_for_ngrams_from_captured_text(n_last_words, bigrams)
        }
        if (!is.null(result) & nrow(result) > 0) {
                result <- get_probabilities(result)
        } else {
#                print("4. Try using bigrams including English stop words")
                n_last_words <- get_n_last_words_from_captured_text(
                        captured_text = captured_text,
                        nb_words = 1,
                        no_stop_words = FALSE)
                result <- look_for_ngrams_from_captured_text(n_last_words, bigrams)
                result <- get_probabilities(result)
        }
        
        return(result)
}


## search_ngrams: 
## 
search_ngrams <- function(pattern, from_ngrams, no_stop_words = FALSE) {
        #print(head(from_ngrams))
        # [1] "Ngrams head:"
        #   ngrams  freq        prop next_word n
        # 1     of 26030 0.004237771       the 2
        # 2     in 23920 0.003894256       the 2
        # 3     it 13565 0.002208427        is 2
        # 4     to 12733 0.002072975       the 2
        # 5      I 12205 0.001987015        am 2
        # 6    for 11886 0.001935081       the 2
#        print(paste0("pattern to search in ngrams last words: ", pattern))
        result_idx <- grep(pattern, from_ngrams$ngrams, value = FALSE)
#        print("head(result_idx):")
#        print(head(result_idx))
        result <- from_ngrams[result_idx, ]
#        print("Result including stop words:")
#        print(head(result))
        
        # REMOVE ENGLISH STOP WORDS FROM BEST RESULTS
        if (no_stop_words) {
                result <- result[!(result[, "next_word"] %in% stopwords("en")), ]
                #                print("Result excluding stop words:")
                #                print(head(result))
        }
        
        #result_tbl <- table(result)
        #print(result_tbl)
        
        # return next words and frequencies
        return(result)
        #        return(result[,2:3])
}


## search_1gram_from_last_words: 
## 
search_1gram_from_last_words <- function(word, from_1gram, no_stop_words = FALSE) {
        pattern <- paste0("(^", word, "$| ", word,"$)")
        search_ngrams(pattern, from_1gram, no_stop_words)
}

## search_2grams_from_last_words: 
## 
search_2grams_from_last_words <- function(words, from_ngrams, no_stop_words = FALSE) {
        pattern <- paste0("(^", words, "$| ", words,"$)")
        search_ngrams(pattern, from_ngrams, no_stop_words)
}

## search_3grams_from_last_words: 
## 
search_3grams_from_last_words <- function(words, from_ngrams, no_stop_words = FALSE) {
        pattern <- paste0("^", words, "$")
        search_ngrams(pattern, from_ngrams, no_stop_words)
}

## get_prediction_from_captured_text:
## 
get_prediction_from_captured_text <- function(ngrams, captured_text) {
#        print("##################################################################################")
#        print("##### Beginning of function get_prediction_from_captured_text(captured_text) #####")
#        print("##################################################################################")
        
        ## Initialize the output data frame 'prediction'
        prediction <- data.frame()
        
        ## Test if captured_text is neither null nor empty
        if (is.null(captured_text) | length(captured_text) == 0) {
                return(NULL)
        }
        
#        print(paste0("Captured text to search: '", captured_text, "'"))
        
        ## Replace contractions with their full forms
        myCorpus <- Corpus(VectorSource(captured_text))
        myCorpus <- tm_map(myCorpus , tolower)
        
        myCorpus <- tm_map(myCorpus, content_transformer(replace_contraction))
        
        captured_text <- as.character(myCorpus[[1]])
        
        predict_bigrams <- NULL
        predict_trigrams <- NULL
        predict_quadrigrams <- NULL
        
        tokens <- tokenize_words(x = captured_text)
        length_tokens <- length(tokens[[1]])
#        print(paste0("Length of captured_text: ", length_tokens))
        
        ## 1. Predict from bigrams
#        print("1. Predict from bigrams")
        last_word <- tokens[[1]][length(tokens[[1]])]
#        print(paste0("Word to search: '", last_word, "'"))
#        print(paste0("Last word of captured text: ", last_word))
        temp_ngrams <- search_1gram_from_last_words(last_word,
                                                    ngrams,
                                                    no_stop_words = FALSE)
#        print("Table of temp_ngrams per n:")
#        print(table(temp_ngrams$n))
        
        # Store the bigrams into data frame 'predict_bigrams'
        predict_bigrams <- temp_ngrams[temp_ngrams[, "n"] == 2, ]
#        print("Head of predict_bigrams:")
#        print(head(predict_bigrams))
        
        # Store the trigrams and quadrigrams into data frame 'temp_ngrams'
        temp_ngrams <- temp_ngrams[temp_ngrams[, "n"] != 2, ]
#        print("temp_ngrams after removing bigrams:")
#        print(head(temp_ngrams))
        
        if (length_tokens > 1) {
                
                ## 2. Predict from trigrams
#                print("2. Predict from trigrams")
                last_2_words <- paste(tokens[[1]][length(tokens[[1]]) - 1],
                                      last_word,
                                      sep = " ")
#                print(paste0("Words to search: '", last_2_words, "'"))
                # Look for trigrams and quadrigrams and store into data frame 'predict_ngrams'
                predict_ngrams <- search_2grams_from_last_words(last_2_words,
                                                             temp_ngrams,
                                                             no_stop_words = FALSE)
                # Store the trigrams and quadrigrams into data frame 'temp_ngrams'
                predict_trigrams <- predict_ngrams[predict_ngrams[, "n"] == 3, ]
                if (nrow(predict_trigrams) > 0) {
                        # Store the quadrigrams into data frame 'temp_ngrams'
                        temp_ngrams <- predict_ngrams[predict_ngrams[, "n"] != 3, ]
                }
#                print("Head of predict_trigrams:")
#                print(head(predict_trigrams))
                
        }
        
        if (length_tokens > 2) {
                
                ## 3. Predict from quadrigrams
#                print("3. Predict from quadrigrams")
                last_3_words <- paste(tokens[[1]][length(tokens[[1]]) - 2],
                                      last_2_words,
                                      sep = " ")
#                print(paste0("Words to search: '", last_3_words, "'"))
                predict_quadrigrams <- search_3grams_from_last_words(last_3_words,
                                                                     temp_ngrams,
                                                                     no_stop_words = FALSE)
        }
        
        ## Concatenate the 'bigrams', 'trigrams' and 'quadrigrams' data frames
        prediction <- rbind(predict_bigrams,
                            predict_trigrams,
                            predict_quadrigrams)
        
#        print("Table of predictions per n:")
#        print(table(prediction$n))
        
        prediction <- prediction[with(prediction, order(-n, -freq)), ]
        
        ## Add the probabilities (OR BEFORE CONCATENATION ???)
        if (nrow(prediction) > 0) {
                prediction[, "probn"] <- 0
                prediction[, "prob"] <- 0
                prediction[prediction[, "n"] == 1, ] <- get_probabilities(
                        prediction[prediction[, "n"] == 1, ],
                        "probn")
                prediction[prediction[, "n"] == 2, ] <- get_probabilities(
                        prediction[prediction[, "n"] == 2, ],
                        "probn")
                prediction[prediction[, "n"] == 3, ] <- get_probabilities(
                        prediction[prediction[, "n"] == 3, ],
                        "probn")
                prediction[prediction[, "n"] == 4, ] <- get_probabilities(
                        prediction[prediction[, "n"] == 4, ],
                        "probn")
                
                prediction <- get_probabilities(prediction, "prob")
                
                prediction_no_stopwords <- prediction[!(prediction[, "next_word"] %in% stopwords("en")), ]
                if (nrow(prediction_no_stopwords) > 0)
                        prediction <- prediction_no_stopwords
                
                prediction <- prediction[with(prediction, order(-probn, -prob, -n, -freq)), ]
                
                prediction <- prediction[!duplicated(prediction["next_word"]), ]
        }
        
#        print("Table of predictions per n:")
#        print(table(prediction$n))
        
        return(prediction)
}

###########################
## Load the ngrams data set
###########################

#print("#################################################")
#print("################## LOADING ... ##################")
#print("#################################################")

#load(file = "/Users/gaelberon/Documents/Coursera/Data_Science_Capstone/bigrams.rda")
#print(paste0("Bigrams size: ", nrow(bigrams)))
#load(file = "/Users/gaelberon/Documents/Coursera/Data_Science_Capstone/trigrams.rda")
#print(paste0("Trigrams size: ", nrow(trigrams)))
#load(file = "/Users/gaelberon/Documents/Coursera/Data_Science_Capstone/quadrigrams.rda")
#print(paste0("Quadrigrams size: ", nrow(quadrigrams)))
load(file = "ngrams.rda")

## Format of the ngrams dataframe
## 
# [1] "Ngrams head:"
#   ngrams  freq        prop next_word n
# 1     of 26030 0.004237771       the 2
# 2     in 23920 0.003894256       the 2
# 3     it 13565 0.002208427        is 2
# 4     to 12733 0.002072975       the 2
# 5      I 12205 0.001987015        am 2
# 6    for 11886 0.001935081       the 2

#print(paste0("Ngrams size: ", nrow(ngrams)))
#print("Table of ngrams per n:")
#print(table(ngrams$n))
#print("Ngrams head:")
#print(head(ngrams))
#print(paste0("size of ngrams where freq = 1: ", nrow(ngrams[ngrams[, "freq"] == 1, ])))

# Convert freq into integer class format
#ngrams[, "freq"] <- as.integer(ngrams[, "freq"])
#ngrams[, "freq"] <- as.integer(ngrams[, "n"])




#print("TEST TEST TEST TEST TEST TEST")
#temp_ngrams <- search_ngrams_from_last_words("a test",
#                                             ngrams,
#                                             no_stop_words = FALSE)
#print(head(temp_ngrams))
#print(table(temp_ngrams$n))



#############################
## Manage Shiny App interface
#############################

server <- function(input, output) {

        # Load corpus files
        
        # Refreshing data
        # Return the selected 'nb_words_to_predict' variable
        Nb_words_to_predict <- reactive({
                # get the nb_words_to_predict selected in the sliderInput
                Nb_words_to_predict <- input$nb_words_to_predict
        })
        
        # Refreshing data
        # Return the captured 'text' variable
        Captured_text <- reactive({
                # get the text captured in the textInput
                Captured_text <- input$text
        })
        
        # Refreshing data
        Prediction <- reactive({
                #nb_words_to_predict <- get_nb_words_to_predict()
                captured_text <- Captured_text()
                Prediction <- get_prediction_from_captured_text(ngrams, captured_text)
        })
        
        # Output captured text
#        output$captured_text <- renderText({
#                paste0("Here after the ",
#                       Nb_words_to_predict(),
#                       " best predicted words to follow: '",
#                       Captured_text(),
#                       "'")
#        })
        
        # Output captured text
        output$best_word <- renderText({
                Prediction()[1, "next_word"]
        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
        output$predicted_words_table <- renderTable({
#        output$predicted_words_table <- renderTable({
                #captured_text <- Captured_text()
                nb_words_to_predict <- Nb_words_to_predict()
                #print(paste0("Ngrams size: ", nrow(ngrams)))
                #prediction <- get_prediction_from_captured_text(ngrams, captured_text)
                Prediction()[2:nb_words_to_predict, "next_word"]
        })
}