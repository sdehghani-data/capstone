library(shiny)
library(tm)
library(data.table)
options(show.error.messages = FALSE)

# Loading data
load("Unigrams_dic.RData")
load("Unigrams.RData")
load("Bigrams.RData")
load("Trigrams.RData")
load("Fourgrams.RData")

# Creating a dictionary and finding  word's index in it

dictionary <- 1:nrow(Unigrams_dic)
names(dictionary) <- Unigrams_dic$Unigrams
dic_length <- length(dictionary)

lookup <- function(x){
    for(i in 1:dic_length){
        if (names(dictionary[i]) == x){
            return(i)
        }
    }
    return(NULL)
}

# Refining input phrase

refining <- function(input){
    input <- tolower(input)
    input <- removePunctuation(input)
    input <- removeNumbers(input)
    input <- stripWhitespace(input)
    input_splitted <- unlist(strsplit(input, " "))
    i_length <- length(input_splitted)
    if (i_length == 0){
        stop("Please, input the first part of phrase/sentense")
    }
    if(i_length> 3){ #if more than 3 words in input, keep only last three
        input<- paste0(input_splitted[i_length-2], " ", 
                       input_splitted[i_length-1], " ", 
                       input_splitted[i_length], sep = "")
    }
    return(input)
}


# Creating prediction function

prediction <-function(input, max = 5){
    
    input <- refining(input)
    input_splitted <- unlist(strsplit(input, ' '))
    i_length <- length(input_splitted)
    
    # one word in input Phrase
    if(i_length == 1){
        
        index_w1 <- lookup(input) # input not found in dictionary
        if (is.null(index_w1)){
            result <- head(Unigrams, max)$w1
        }
        else {
            result <- head(Bigrams[w1 == index_w1], max)$w2
        }
    }
    
    # two words in input Phrase
    if(i_length == 2){
        index_w1<- lookup(input_splitted[1])
        index_w2<- lookup(input_splitted[2])
        
        subTrigrams <- Trigrams[w1 == index_w1 & w2 == index_w2]
        
        if(nrow(subTrigrams) == 0){ #if w1 and w2 not found together in Trigram -> backoff to Bigram
            
            subBigrams <- Bigrams[w1 == index_w2]
            
            if (nrow(subBigrams) == 0){
                result <- head(Unigrams, max)$w1
            }
            
            else {
                result <- head(subBigrams, max)$w2
            }
        }
        
        else {
            result<- head(subTrigrams, max)$w3
        }
    }
    
    # three words in input phrase
    if(i_length == 3){
        index_w1<- lookup(input_splitted[1])
        index_w2<- lookup(input_splitted[2])
        index_w3<- lookup(input_splitted[3])
        
        subFourgrams <- Fourgrams[w1 == index_w1 & w2 == index_w2 & w3 == index_w3]
        
        if(nrow(subFourgrams) == 0){ #if w1 and w2 and w3 not found together in Trigram -> backoff to Trigram
            
            subTrigrams <- Trigrams[w1 == index_w2 & w2 == index_w3]
            
            if(nrow(subTrigrams) == 0){ #if w2 and w3 not found in Trigram -> backoff to Bigram
                
                subBigrams <- Bigrams[w1 == index_w3]
                
                if (nrow(subBigrams) == 0){
                    result <- head(Unigrams, max)$w1
                }
                
                else {
                    result <- head(subBigrams, max)$w2
                    
                }
            }
            else {
                result<- head(subTrigrams, max)$w3
            }
            
        }
        
        else {
            result<- head(subFourgrams, max)$w4
        }
        
    }        
    
    next_Words<- names(dictionary[result])
    next_Words
}


# Main SERVER part
# Input variable: input$Phrase
# To be provided: Output$word_predicted

shinyServer(function(input, output) {
    
    Phrase <- reactive({
        if (length(input$Phrase) > 0){
            input$Phrase
        }
    })
    predict_result <- reactive(prediction(Phrase()))
    
    output$word_predicted_1 <- renderText({
        word_predicted_1 <- predict_result()[1]
    })
    output$word_predicted_2 <- renderText({
        word_predicted_2 <- predict_result()[2]
    })
    output$word_predicted_3 <- renderText({
        word_predicted_3 <- predict_result()[3]
    })
    output$word_predicted_4 <- renderText({
        word_predicted_4 <- predict_result()[4]
    })
    output$word_predicted_5 <- renderText({
        word_predicted_5 <- predict_result()[5]
    })
}
)