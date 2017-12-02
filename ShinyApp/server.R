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
        pattern <- paste0("^", tolower(captured_text), " ")
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
get_probabilities <- function(df) {
        #        df[, "ngrams"]
        #        df[, "next_word"]
        #        df[, "freq"]
        
        print(colnames(df))
        print(head(df))
        if (!is.null(df) & nrow(df) > 0) {
                sum_freq <- sum(df[, "freq"])
                df[, "prob"] <- df[, "freq"] / sum_freq
                
                max_prob <- df[df[, "prob"] == max(df[, "prob"]), ]
                print(paste0("for ngram ",
                             max_prob[, "ngrams"],
                             ", the word '",
                             max_prob[, "next_word"],
                             "' has the highest probability of '",
                             max_prob[, "prob"], "'"))
        }
        
        return(df)
}

## get_result:
## 
get_result <- function(captured_text) {
        print("###########################################")
        print("1. Try using trigrams excluding English stop words")
        n_last_words <- get_n_last_words_from_captured_text(
                captured_text = captured_text,
                nb_words = 2,
                no_stop_words = TRUE)
        result <- look_for_ngrams_from_captured_text(n_last_words, trigrams)
        
        if (!is.null(result) & nrow(result) > 0) {
                result <- get_probabilities(result)
        } else {
                print("2. Try using trigrams including English stop words")
                n_last_words <- get_n_last_words_from_captured_text(
                        captured_text = captured_text,
                        nb_words = 2,
                        no_stop_words = FALSE)
                result <- look_for_ngrams_from_captured_text(n_last_words, trigrams)
        }
        if (!is.null(result) & nrow(result) > 0) {
                result <- get_probabilities(result)
        } else {
                print("3. Try using bigrams excluding English stop words")
                n_last_words <- get_n_last_words_from_captured_text(
                        captured_text = captured_text,
                        nb_words = 1,
                        no_stop_words = TRUE)
                result <- look_for_ngrams_from_captured_text(n_last_words, bigrams)
        }
        if (!is.null(result) & nrow(result) > 0) {
                result <- get_probabilities(result)
        } else {
                print("4. Try using bigrams including English stop words")
                n_last_words <- get_n_last_words_from_captured_text(
                        captured_text = captured_text,
                        nb_words = 1,
                        no_stop_words = FALSE)
                result <- look_for_ngrams_from_captured_text(n_last_words, bigrams)
                result <- get_probabilities(result)
        }

        return(result)
}

###########################
## Load the ngrams data set
###########################

print("#################################################")
print("################## LOADING ... ##################")
print("#################################################")

load(file = "/Users/gaelberon/Documents/Coursera/Data_Science_Capstone/bigrams.rda")
print(paste0("Bigrams size: ", nrow(bigrams)))
load(file = "/Users/gaelberon/Documents/Coursera/Data_Science_Capstone/trigrams.rda")
print(paste0("Trigrams size: ", nrow(trigrams)))
#load(file = "/Users/gaelberon/Documents/Coursera/Data_Science_Capstone/quadrigrams.rda")
#print(paste0("Quadrigrams size: ", nrow(quadrigrams)))


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
#        live_data <- reactive({
#                nb_words_to_predict <- get_nb_words_to_predict()
#                captured_text <- get_captured_text()
#        })
        
        # Output captured text
        output$captured_text <- renderText({
                paste0("Here after the ",
                       Nb_words_to_predict(),
                       " best predicted words to follow: '",
                       Captured_text(),
                       "'")
        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
#        output$predicted_words_print <- renderPrint({
#                "blablabla"
#        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
        output$predicted_words_table <- renderTable({
                #                head(trigrams, Nb_words_to_predict())
                captured_text <- Captured_text()
                nb_words_to_predict <- Nb_words_to_predict()
                result <- get_result(captured_text)
                head(result, nb_words_to_predict)
        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
#        output$predicted_words_from_bigrams_table <- renderTable({
#                #                head(trigrams, Nb_words_to_predict())
#                captured_text <- Captured_text()
#                nb_words_to_predict <- Nb_words_to_predict()
#                n_last_words <- get_n_last_words_from_captured_text(
#                        captured_text = captured_text,
#                        nb_words = 1,
#                        no_stop_words = FALSE)
#                result <- look_for_ngrams_from_captured_text(n_last_words, bigrams)
#                head(result, nb_words_to_predict)
#        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
#        output$predicted_words_from_trigrams_table <- renderTable({
#                #                head(trigrams, Nb_words_to_predict())
#                captured_text <- Captured_text()
#                nb_words_to_predict <- Nb_words_to_predict()
#                n_last_words <- get_n_last_words_from_captured_text(
#                        captured_text = captured_text,
#                        nb_words = 2,
#                        no_stop_words = FALSE)
#                result <- look_for_ngrams_from_captured_text(n_last_words, trigrams)
#                head(result, nb_words_to_predict)
#        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
#        output$predicted_words_from_quadrigrams_table <- renderTable({
#                #                head(quadrigrams, Nb_words_to_predict())
#                captured_text <- Captured_text()
#                nb_words_to_predict <- Nb_words_to_predict()
#                n_last_words <- get_n_last_words_from_captured_text(
#                        captured_text = captured_text,
#                        nb_words = 3,
#                        no_stop_words = FALSE)
#                result <- look_for_ngrams_from_captured_text(n_last_words, quadrigrams)
#                head(result, nb_words_to_predict)
#        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
#        output$predicted_words_from_bigrams_no_stop_words_table <- renderTable({
#                #                head(trigrams, Nb_words_to_predict())
#                captured_text <- Captured_text()
#                nb_words_to_predict <- Nb_words_to_predict()
#                n_last_words <- get_n_last_words_from_captured_text(
#                        captured_text = captured_text,
#                        nb_words = 1,
#                        no_stop_words = FALSE)
#                result <- look_for_ngrams_from_captured_text(n_last_words, bigrams,
#                                                             no_stop_words = TRUE)
#                head(result, nb_words_to_predict)
#        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
#        output$predicted_words_from_trigrams_no_stop_words_table <- renderTable({
#                #                head(trigrams, Nb_words_to_predict())
#                captured_text <- Captured_text()
#                nb_words_to_predict <- Nb_words_to_predict()
#                n_last_words <- get_n_last_words_from_captured_text(
#                        captured_text = captured_text,
#                        nb_words = 2,
#                        no_stop_words = FALSE)
#                result <- look_for_ngrams_from_captured_text(n_last_words, trigrams,
#                                                             no_stop_words = TRUE)
#                head(result, nb_words_to_predict)
#        })
        
        # Show the first "n" words predicted
        # The output$predicted_text table depends on both the text captured and
        # the number of words to predict
#        output$predicted_words_from_quadrigrams_no_stop_words_table <- renderTable({
#                #                head(quadrigrams, Nb_words_to_predict())
#                captured_text <- Captured_text()
#                nb_words_to_predict <- Nb_words_to_predict()
#                n_last_words <- get_n_last_words_from_captured_text(
#                        captured_text = captured_text,
#                        nb_words = 3,
#                        no_stop_words = FALSE)
#                result <- look_for_ngrams_from_captured_text(n_last_words, quadrigrams,
#                                                             no_stop_words = TRUE)
#                head(result, nb_words_to_predict)
#        })
}