###################################################################################
#Importing Libraries
###################################################################################
#Importing library to read text files
library(textreadr)

#Importing tidy libraries and dplyr to use piping
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)

#Importing ggplot and plotly to create graphs
library(ggplot2)
library(plotly)

#Importing shiny 
library(shiny)

#Importing libraries for bigram
library(igraph)
library(ggraph)

library(topicmodels)
library(reshape2)
library(tm)
library(wordcloud)
library(tidyverse)

###################################################################################
#Creating Shiny Logic
###################################################################################

shinyServer(function(input, output) {
    
    ###############################################################################
    #Reading Survey Resuls
    ###############################################################################
    
    #Importing file
    survey_file <- read_document(file="Team3_Interviews_Mauricio.txt")
    
    #Converting to dataframe
    survey_df = data.frame(survey_file)
    
    #Changing column name
    names(survey_df)[names(survey_df) == "survey_file"] <- "Answer"
    
    #Changing data type to character
    survey_df$Answer <- as.character(survey_df$Answer)
    
    ###############################################################################
    #Adding and Modifying Columns
    ###############################################################################
    
    #Creating Gender column
    survey_df$Gender = 'None'
    
    #Filling Gender column
    for (i in 1:nrow(survey_df)){
        if (i%%8 == 1){
            gender <- survey_df[i,1]
        }
        survey_df[i,2] <- gender
    }
    
    #Deleting the rows with the gender in Answer
    index_del = c()
    
    for (i in 1:nrow(survey_df)){
        if (i%%8 == 1){
            index_del <- c(index_del, i)
        }
    }
    
    survey_df <- survey_df[-index_del,]
    
    #Fixing the index after deleting rows
    rownames(survey_df) <- 1:nrow(survey_df)
    
    #Adding Question_ID
    survey_df$Question_ID <- seq(1,7)
    
    #Adding Person_ID
    survey_df$Person_ID <- 0
    
    person_id = 0
    
    for (i in 1:nrow(survey_df)){
        if (i%%7 == 1){
            person_id = person_id + 1
        }
        survey_df[i,4] <- person_id
    }
    
    ###############################################################################
    #Tokenizing
    ###############################################################################
    survey_tokenized <- survey_df %>%
        unnest_tokens(word,Answer)
    
    ###############################################################################
    #Removing Stop Words
    ###############################################################################
    
    #Creating Custom Stop Words
    cust_stop_q1 <- data_frame(word=c('watch','movies'),
                               lexicon=rep('cust', each = 1))
    
    cust_stop_q1 <- rbind(stop_words, cust_stop_q1)
    
    cust_stop_q2 <- data_frame(word=c('free','time'),
                               lexicon=rep('cust', each = 1))
    
    cust_stop_q2 <- rbind(stop_words, cust_stop_q2)
    
    cust_stop_q3 <- data_frame(word=c('sport','sports'),
                               lexicon=rep('cust', each = 1))
    
    cust_stop_q3 <- rbind(stop_words, cust_stop_q3)
    
    cust_stop_q4 <- data_frame(word=c('favorite','food'),
                               lexicon=rep('cust', each = 1))
    
    cust_stop_q4 <- rbind(stop_words, cust_stop_q4)
    
    cust_stop_q5 <- data_frame(word=c('spirit','animal'),
                               lexicon=rep('cust', each = 1))
    
    cust_stop_q5 <- rbind(stop_words, cust_stop_q5)
    
    cust_stop_q6 <- data_frame(word=c('cockroach'),
                               lexicon=rep('cust', each = 1))
    
    cust_stop_q6 <- rbind(stop_words, cust_stop_q6)
    
    cust_stop_q7 <- data_frame(word=c('pet','name'),
                               lexicon=rep('cust', each = 1))
    
    cust_stop_q7 <- rbind(stop_words, cust_stop_q7)
    
    #Removing Stop Words
    survey_q1 <- survey_tokenized %>%
        filter(Question_ID == 1) %>%
        anti_join(cust_stop_q1)
    
    survey_q2 <- survey_tokenized %>%
        filter(Question_ID == 2) %>%
        anti_join(cust_stop_q2)
    
    survey_q3 <- survey_tokenized %>%
        filter(Question_ID == 3) %>%
        anti_join(cust_stop_q3)
    
    survey_q4 <- survey_tokenized %>%
        filter(Question_ID == 4) %>%
        anti_join(cust_stop_q4)
    
    survey_q5 <- survey_tokenized %>%
        filter(Question_ID == 5) %>%
        anti_join(cust_stop_q5)
    
    survey_q6 <- survey_tokenized %>%
        filter(Question_ID == 6) %>%
        anti_join(cust_stop_q6)
    
    survey_q7 <- survey_tokenized %>%
        filter(Question_ID == 7) %>%
        anti_join(cust_stop_q7)
    
    survey_tokenized_clean <- rbind(survey_q1,
                                    survey_q2,
                                    survey_q3,
                                    survey_q4,
                                    survey_q5,
                                    survey_q6,
                                    survey_q7)
    
    ###############################################################################
    #TF-IDF
    ###############################################################################
    output$outtf_idf <- renderPlot({
        
        if(input$t_question == 0){
            
            survey_tokenized_clean %>%
                count(Gender, word,sort=T)%>%
                bind_tf_idf(word, Gender, n) %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(Gender) %>%
                top_n(input$tf_idf) %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=Gender))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="TF-IDF")+
                facet_wrap(~Gender, ncol=2, scales="free")+
                theme(text = element_text(size=20))+
                coord_flip()
            
        }else{
            survey_tokenized_clean %>%
                filter(Question_ID == input$t_question) %>%
                count(Gender, word,sort=T)%>%
                bind_tf_idf(word, Gender, n) %>%
                arrange(desc(tf_idf)) %>%
                mutate(word=factor(word, levels=rev(unique(word)))) %>%
                group_by(Gender) %>%
                top_n(input$tf_idf) %>%
                ungroup %>%
                ggplot(aes(word, tf_idf, fill=Gender))+
                geom_col(show.legend=FALSE)+
                labs(x=NULL, y="TF-IDF")+
                facet_wrap(~Gender, ncol=2, scales="free")+
                theme(text = element_text(size=20))+
                coord_flip()
        }
        
    }) 
    
    ###############################################################################
    #Sentiment Analysis
    ###############################################################################  
    
    output$sentiment_question <- renderPlot({
        
        survey_freq <- survey_tokenized_clean
        
        if(input$s_question > 0){
            survey_freq <- survey_freq %>%
                filter(Question_ID == input$s_question)
        }
        
        if(input$s_gender > 0){
            if(input$s_gender == 1){
                survey_freq <- survey_freq %>%
                    filter(Gender == 'Man')
            }else{
                survey_freq <- survey_freq %>%
                    filter(Gender == 'Woman')
            }
        }
        
        survey_freq %>%
            inner_join(get_sentiments("nrc")) %>%
            count(word, sentiment, sort=TRUE) %>%
            acast(word ~sentiment, value.var="n", fill=0) %>%
            comparison.cloud(colors = c("blue", "cyan2","chocolate1","coral2","darkgoldenrod1","brown2","chartreuse4", "black"),
                             max.words=100,
                             scale =c(4,1),
                             fixed.asp=TRUE, 
                             title.size = 1
            )
    })
    
    ###############################################################################
    #Wordcount
    ###############################################################################
    output$words_hist <- renderPlot({
        
        survey_freq <- survey_tokenized_clean
        
        if(input$h_question > 0){
            survey_freq <- survey_freq %>%
                filter(Question_ID == input$h_question)
        }
        
        if(input$h_gender > 0){
            if(input$h_gender == 1){
                survey_freq <- survey_freq %>%
                    filter(Gender == 'Man')
            }else{
                survey_freq <- survey_freq %>%
                    filter(Gender == 'Woman')
            }
        }
        
        frequencies_tokens_nostop1 <- survey_freq %>%
            count(word, sort=TRUE)
        
        frequencies_tokens_nostop1 %>%
            top_n(input$hist_n)  %>%
            mutate(word=reorder(word,n)) %>%
            ggplot(aes(word, n))+
            geom_col()+
            xlab(NULL)+
            ylab('Frequency')+
            theme(text = element_text(size=20))+
            coord_flip()
        
    })
    
    ###############################################################################
    #Prediction Model
    ###############################################################################
    
    observeEvent(input$action,{
        output$outModel <- renderPrint({
            answer_pre_corpus <- paste(input$answer1,'',
                                       input$answer2,'',
                                       input$answer3,'',
                                       input$answer4,'',
                                       input$answer5,'',
                                       input$answer6,'',
                                       input$answer7)
            
            answer_pre_corpus <- "soccer"
            survey_pre_corpus <- c()
            
            for(i in seq(1,nrow(survey_df),by=7)){
                text <- ''
                for(j in 1:7){
                    text <- paste(text,survey_df[i+j-1,1],' ')
                }
                survey_pre_corpus <- c(survey_pre_corpus,text)
            }

            survey_pre_corpus <- c(survey_pre_corpus,answer_pre_corpus)

            survey_corpus <- corpus(survey_pre_corpus)
            
            #Creating DFM
            msg.dfm <- dfm(survey_corpus, tolower = TRUE)
            msg.dfm <- dfm_trim(msg.dfm, min_docfreq = 0)
            msg.dfm <- dfm_weight(msg.dfm)

            #Prepating answer set for model
            answer_set <- c()

            for(i in 1:nrow(survey_df)){
                if (i%%7 == 1){
                    answer_set <- c(answer_set,survey_tokenized_clean[i,1])
                }
            }
            
            for(i in 1:length(answer_set)){
                if (answer_set[i] == 'Woman'){
                    answer_set[i] <- 1
                }else{
                    answer_set[i] <- 0
                }
            }
            
            answer_set <- as.integer(answer_set)
            
            #Training Model
            NB_classifier <- textmodel_nb(msg.dfm[-length(survey_pre_corpus)], answer_set)

            #Testing with user's answer

            pred <- predict(NB_classifier, msg.dfm[length(survey_pre_corpus),])

            if(as.integer(pred[1])==1){
                print('You are a woman!')
            }else if (as.integer(pred[1])==0){
                print('You are a man')
            }
        })
    })
    
    ###############################################################################
    #Bigram
    ###############################################################################
    
    output$outbigram <- renderPlot({
        
        survey_bigrams <- survey_df %>%
            unnest_tokens(bigram, Answer, token = "ngrams", n=2)
        
        bigrams_separated <- survey_bigrams %>%
            separate(bigram, c("word1", "word2"), sep = " ") 
        
        #Removing Stop Words
        bigrams_filtered_q1 <- bigrams_separated %>%
            filter(Question_ID == 1) %>%
            filter(!word1 %in% cust_stop_q1$word) %>%
            filter(!word2 %in% cust_stop_q1$word)
        
        bigrams_filtered_q2 <- bigrams_separated %>%
            filter(Question_ID == 2) %>%
            filter(!word1 %in% cust_stop_q2$word) %>%
            filter(!word2 %in% cust_stop_q2$word)
        
        bigrams_filtered_q3 <- bigrams_separated %>%
            filter(Question_ID == 3) %>%
            filter(!word1 %in% cust_stop_q3$word) %>%
            filter(!word2 %in% cust_stop_q3$word)
        
        bigrams_filtered_q4 <- bigrams_separated %>%
            filter(Question_ID == 4) %>%
            filter(!word1 %in% cust_stop_q4$word) %>%
            filter(!word2 %in% cust_stop_q4$word)
        
        bigrams_filtered_q5 <- bigrams_separated %>%
            filter(Question_ID == 5) %>%
            filter(!word1 %in% cust_stop_q5$word) %>%
            filter(!word2 %in% cust_stop_q5$word)
        
        bigrams_filtered_q6 <- bigrams_separated %>%
            filter(Question_ID == 6) %>%
            filter(!word1 %in% cust_stop_q6$word) %>%
            filter(!word2 %in% cust_stop_q6$word)
        
        bigrams_filtered_q7 <- bigrams_separated %>%
            filter(Question_ID == 7) %>%
            filter(!word1 %in% cust_stop_q7$word) %>%
            filter(!word2 %in% cust_stop_q7$word)
        
        #Combining questiosn together
        
        bigrams_filtered <- rbind(bigrams_filtered_q1,
                                  bigrams_filtered_q2,
                                  bigrams_filtered_q3,
                                  bigrams_filtered_q4,
                                  bigrams_filtered_q5,
                                  bigrams_filtered_q6,
                                  bigrams_filtered_q7)
        
        bigram_united <- bigrams_filtered%>%
            unite(bigram, word1, word2, sep=" ")
        
        survey_freq <- bigrams_filtered
        
        if(input$b_question > 0){
            survey_freq <- survey_freq %>%
                filter(Question_ID == input$b_question)
        }
        
        if(input$b_gender > 0){
            if(input$b_gender == 1){
                survey_freq <- survey_freq %>%
                    filter(Gender == 'Man')
            }else{
                survey_freq <- survey_freq %>%
                    filter(Gender == 'Woman')
            }
        }
        
        #Spider plot
        bigram_graph <- survey_freq[,c(4,5)] %>%
            count(word1,word2,sort=TRUE) %>%
            filter(n>=1) %>%
            graph_from_data_frame()  
        
        ggraph(bigram_graph, layout = "fr") +
            geom_edge_link()+
            geom_node_point()+
            geom_node_text(aes(label=name), vjust =1, hjust=1)
        
    })
    
})
