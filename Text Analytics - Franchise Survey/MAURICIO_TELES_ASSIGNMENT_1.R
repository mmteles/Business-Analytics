#######################  C A S E  #######################
# I owned a franchisee of  a brain training program. 
# In 2017, I was member of the franchisee council and to
# bring insights to the franchisor, I launched 2 surveys
# One in May and another in November. The questions were
# divided by departments (Sales, Marketing, Operations,
# and Education) and more than a rating score, the questions
# asked for feedback and suggestions of improvement.
# The data is structured in a excel spreadsheet, but the 
# text answers.
# The text is in Brazilian portuguese and I used a Google
# Docs function to translate it.


###########  L I B R A R I E S ###########
library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(tidytext)
library(ggplot2)
library(stringr)
library(textdata)
library(reshape2)
library(wordcloud)
library(igraph)
library(ggraph)

# open the data frame
supera_may <- read_excel("Survey Supera.xlsx", sheet = "2017may", col_names = TRUE)
supera_nov <- read_excel("Survey Supera.xlsx", sheet = "2017nov", col_names = TRUE)

###########  F U N C T I O N  ###########
#  split_answers
#  organize the data to include "location"
split_answers <- function(df_in, df_out, survey, dept, type, avg_score){
  df_out <- data.frame()
  for(i in 1:nrow(df_in)){
    if (!is.na(df_in[i, paste(dept,type,sep="_")])){
      df_out[i, "survey"] <- survey
      df_out[i, "dept"]   <- dept
      df_out[i, "type"]   <- type
      df_out[i, "text"]   <- supera_may[i, paste(dept,type,sep="_")]
      if (is.na(df_in[i, paste(dept,"score",sep="_")])) {
        df_out[i, "score"] <- "neutral" 
      }else{
        if (df_in[i, paste(dept,"score",sep="_")] >= avg_score) {
          df_out[i, "score"] <- "positive" 
        }else{
          df_out[i, "score"] <- "negative"
        }
      }
    }
  }
  df_out <- na.omit(df_out)
  return(df_out)
}#closing the negated_words_plot function



####  D A T A   P R E P A R A T I O N  #### 
may_sales_avg <- mean(supera_may$sales_score, na.rm = TRUE)
may_education_avg <- mean(supera_may$education_score, na.rm = TRUE)
may_marketing_avg <- mean(supera_may$marketing_score, na.rm = TRUE)
may_operations_avg <- mean(supera_may$operations_score, na.rm = TRUE)

MSF <- split_answers(supera_may, SF, "may2017", "sales", "feedback", may_sales_avg)
MSI <- split_answers(supera_may, SF, "may2017", "sales", "improve", may_sales_avg)
MEF <- split_answers(supera_may, SF, "may2017", "education", "feedback", may_education_avg)
MEI <- split_answers(supera_may, SF, "may2017", "education", "improve", may_education_avg)
MMF <- split_answers(supera_may, SF, "may2017", "marketing", "feedback", may_marketing_avg)
MMI <- split_answers(supera_may, SF, "may2017", "marketing", "improve", may_marketing_avg)
MOF <- split_answers(supera_may, SF, "may2017", "operations", "feedback", may_operations_avg)
MOI <- split_answers(supera_may, SF, "may2017", "operations", "improve", may_operations_avg)

nov_sales_avg <- mean(supera_nov$sales_score, na.rm = TRUE)
nov_education_avg <- mean(supera_nov$education_score, na.rm = TRUE)
nov_marketing_avg <- mean(supera_nov$marketing_score, na.rm = TRUE)
nov_operations_avg <- mean(supera_nov$operations_score, na.rm = TRUE)

NSF <- split_answers(supera_nov, SF, "nov2017", "sales", "feedback", nov_sales_avg)
NSI <- split_answers(supera_nov, SF, "nov2017", "sales", "improve", nov_sales_avg)
NEF <- split_answers(supera_nov, SF, "nov2017", "education", "feedback", nov_education_avg)
NEI <- split_answers(supera_nov, SF, "nov2017", "education", "improve", nov_education_avg)
NMF <- split_answers(supera_nov, SF, "nov2017", "marketing", "feedback", nov_marketing_avg)
NMI <- split_answers(supera_nov, SF, "nov2017", "marketing", "improve", nov_marketing_avg)
NOF <- split_answers(supera_nov, SF, "nov2017", "operations", "feedback", nov_operations_avg)
NOI <- split_answers(supera_nov, SF, "nov2017", "operations", "improve", nov_operations_avg)

all_surveys <- bind_rows(MSF, MSI, MEF, MEI, MMF, MMI, MOF, MOI,
                         NSF, NSI, NEF, NEI, NMF, NMI, NOF, NOI)  %>%
               mutate(index=row_number())


####  T O K E N I Z A T I O N  ####
all_tokens <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(word, sort=TRUE)

dept_tokens <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(dept, word, sort=TRUE)

type_tokens <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(type, word, sort=TRUE)

survey_tokens <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(survey, word, sort=TRUE)

score_tokens <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(score, word, sort=TRUE)

dept_score_tokens <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(dept, score, word, sort=TRUE)

survey_score_tokens <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  count(survey, score, word, sort=TRUE)

####  H I S T O G R A M S  ####
freq_hist <- all_tokens %>%
  filter(n > 50) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

dept_tokens %>%
  group_by(dept) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=dept)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~dept, scales = "free_y")+
  labs(y="Department impact", x=NULL)+
  coord_flip()

type_tokens %>%
  group_by(type) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free_y")+
  labs(y="Kind of contribution", x=NULL)+
  coord_flip()

survey_tokens %>%
  group_by(survey) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=survey)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~survey, scales = "free_y")+
  labs(y="When the survey was done", x=NULL)+
  coord_flip()

score_tokens %>%
  group_by(score) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=score)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~score, scales = "free_y")+
  labs(y="Based on average rating", x=NULL)+
  coord_flip()

dept_score_tokens %>%
  group_by(dept) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=dept)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~dept + score, scales = "free_y")+
  labs(y="Department impact", x=NULL)+
  coord_flip()

survey_score_tokens %>%
  group_by(survey) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=survey)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~survey + score, scales = "free_y")+
  labs(y="Department impact", x=NULL)+
  coord_flip()

####  W O R D C L O U D  ####
all_tokens %>%
  with(wordcloud(word, n, max.words = 100))

score_tokens %>%
  count(word, score, sort=TRUE) %>%
  acast(word ~score, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=200,
                   scale=c(0.5,0.5),
                   fixed.asp=TRUE,
                   title.size=1
  )


#####  D T M   A NA L Y S I S  #####
surveys_dtm <- all_surveys %>%
  unnest_tokens(word, text) %>%
  count(survey, word) %>%
  cast_dtm(survey, word, n)

dim(surveys_dtm)
surveys_dtm

dept_dtm <- all_surveys %>%
  unnest_tokens(word, text) %>%
  count(dept, word) %>%
  cast_dtm(dept, word, n)

dim(dept_dtm)
dept_dtm


####  S E N T I M E N T S  ####
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
unique(nrc$sentiment)
bing <- get_sentiments("bing")

sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon="nrc"),
                        mutate(bing, lexicon="bing")
)

afinn <- all_tokens %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(word) %>% #using integer division to define larger sections of text
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  all_tokens%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  all_tokens %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, word, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(word, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

####  N R C  ####
nrc_data <- subset(sentiments, lexicon == "nrc")

tokens_sentiment <- all_tokens %>%
  inner_join(nrc_data) 

tokens_sentiment_count <- tokens_sentiment %>%
  count(sentiment, sort=T)

nrc_hist <- tokens_sentiment_count %>%
  mutate(sentiment=reorder(sentiment,n)) %>%
  ggplot(aes(sentiment, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(nrc_hist)

all_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20","grey40", "grey60", "gray80"),
                   max.words=500,
                   scale=c(0.5,0.5),
                   fixed.asp=TRUE,
                   title.size=1
  )


nrc_counts <- all_tokens %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

nrc_counts %>%
  group_by(sentiment) %>%
  top_n(1) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

####   B I N G   ####
bing_data <- subset(sentiments, lexicon == "bing")

tokens_sentiment <- all_tokens %>%
  inner_join(bing_data) 

tokens_sentiment_count <- tokens_sentiment %>%
  count(sentiment, sort=T)

bing_hist <- tokens_sentiment_count %>%
  mutate(sentiment=reorder(sentiment,n)) %>%
  ggplot(aes(sentiment, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(bing_hist)

bing_counts <- all_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts %>%
  group_by(sentiment) %>%
  top_n(1) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

all_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=500,
                   scale=c(0.5,0.5),
                   fixed.asp=TRUE,
                   title.size=1
  )

####  Z I P F  ####
dept_words <- all_surveys %>%
  unnest_tokens(word, text) %>%
  count(dept, word, sort=TRUE) %>%
  group_by(dept) %>%
  summarize(total=sum(n))

dept_words <- left_join(dept_tokens, dept_words)

ggplot(dept_words, aes(n/total, fill = dept))+
  geom_histogram(show.legend=FALSE)+
  xlim(NA, 0.001) +
  facet_wrap(~dept, ncol=2, scales="free_y")

dept_rank <- dept_words %>%
  group_by(dept) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

dept_rank %>%
  ggplot(aes(rank, `term frequency`, color=dept))+
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

dept_words <- dept_words %>%
  bind_tf_idf(word, dept, n)

dept_words %>%
  arrange(desc(tf_idf))

dept_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(dept) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=dept))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~dept, ncol=2, scales="free")+
  coord_flip()

####  N - G R A M S  ####
supera_bigrams <- all_surveys %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE)

counted_bigrams <- supera_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE)

####  C O N S I D E R I N G   N E G A T I V E   W O R D S  ####
negation_tokens <- c("no", "never", "without", "not")#what negation tokens do you want to use?

negative_bigrams <- supera_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

negated_words <- negative_bigrams %>%
  filter(word1 %in% negation_tokens) %>%
  inner_join(get_sentiments("afinn"), by=c(word2="word")) %>%
  count(word1, word2, value, sort=TRUE) %>%
  ungroup()

negated_words_plot <- function(x){
  negated_words %>%
    filter(word1 == x) %>%
    mutate(contribution = n * value) %>%
    arrange(desc(abs(contribution))) %>%
    head(20) %>%
    mutate(word2 = reorder(word2, contribution)) %>%
    ggplot(aes(word2, n*value, fill = n*value >0))+
    geom_col(show.legend = FALSE)+
    xlab(paste("Words preceded by", x))+
    ylab("Sentiment value* number of occurences")+
    coord_flip()
}#closing the negated_words_plot function

negated_words_plot(x="not") #this is your first negation word
negated_words_plot(x="no") #this is your second negation word
negated_words_plot(x="never") #this is your third negation word

####  B I G R A M S   N E T W O R K  ####

bigram_graph <- counted_bigrams %>%
  #filter(n>1) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


####  F R E Q U E N C Y   A N A L Y S I S  ####
dept_frequency <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(dept, word) %>%
  group_by(dept) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(dept, proportion) %>%
  gather(dept, proportion, `sales`, `marketing`, `operations`)

dept_frequency <- dept_frequency %>% filter(dept != 'marketing')
ggplot(dept_frequency, aes(x=proportion, y=`education`, 
                      color = abs(`education`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=0.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~dept, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "education", x=NULL)

cor.test(data=dept_frequency[dept_frequency$dept == "operations",],
         ~proportion + `education`)

cor.test(data=dept_frequency[dept_frequency$dept == "sales",],
         ~proportion + `education`)

cor.test(data=dept_frequency[dept_frequency$dept == "operations",],
         ~proportion + `sales`)

#################################

score_frequency <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(score, word) %>%
  group_by(score) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(score, proportion) %>%
  gather(score, proportion, `negative`, `positive`)

ggplot(score_frequency, aes(x=proportion, y=`neutral`, 
                                       color = abs(`neutral`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=0.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~score, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "neutral", x=NULL)

cor.test(data=score_frequency[score_frequency$score == "positive",],
         ~proportion + `neutral`)

cor.test(data=score_frequency[score_frequency$score == "negative",],
         ~proportion + `neutral`)


#################################
survey_frequency <- all_surveys %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(survey, word) %>%
  group_by(survey) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(survey, proportion) %>%
  gather(survey, proportion, `nov2017`)

ggplot(survey_frequency, aes(x=proportion, y=`may2017`, 
                             color = abs(`may2017`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=0.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~survey, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "may2017", x=NULL)

cor.test(data=survey_frequency[survey_frequency$survey == "nov2017",],
         ~proportion + `may2017`)


