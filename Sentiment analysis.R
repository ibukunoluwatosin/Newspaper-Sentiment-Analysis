install.packages("topicmodels")
install.packages("sentimentr")
install.packages("ggalluvial")
install.packages("tidyquant")

#Import packages
library(tidytext) #for tokenizing and sentiment anlysis
library(data.table) #for importing data and data handling
library(dplyr) #Easy data wrangling
library(magrittr) #piping operations
library(stringr) #subsetting of strings
library(ggplot2) #Visualizations
library(tidyr) #Data wrangling
library(lubridate) #Handles the date
library(ggalluvial) #Visualization - Sankey chart
library(sentimentr)
library(tidyquant) #wrapper for theme_tq() for the plots

#Set working directory
setwd("C:/Users/nc169de/Desktop/ASD/1. Sentiment analysis project")

#A. Import Data-----------------
guardian <- fread("Guardian complete.csv")
guardian$news_outlet <- "Guardian"

punch <- fread("Punch complete.csv")
punch$news_outlet <- "Punch"

vanguard <- fread("Vanguard complete.csv")
vanguard$news_outlet <- "Vanguard"

this_day <- fread("This day complete.csv")
this_day$news_outlet <- "This day"

sahara <- fread("Sahara.csv")
sahara$news_outlet <- "Sahara"


#B. Data Preparation------------
#1. Change all headline words into lowercase for standardization - cos R is case sensitive
guardian$Headlines %<>% tolower()
punch$Headlines %<>% tolower()
vanguard$Headlines %<>% tolower()
this_day$Headlines %<>% tolower()
sahara$Headlines %<>% tolower()

#2. Format the date to POSIXct
guardian$NewDate <- as.POSIXct(guardian$Date, format='%A, %B %d, %Y')
punch$NewDate <- as.POSIXct(punch$Date, format='%A, %B %d, %Y')
vanguard$NewDate <- as.POSIXct(vanguard$Date, format='%A, %B %d, %Y')
this_day$NewDate <- as.POSIXct(this_day$Date, format='%A, %B %d, %Y')
sahara$NewDate <- as.POSIXct(sahara$Date, format='%A, %B %d, %Y')

guardian$Date <- NULL
punch$Date <- NULL
vanguard$Date <- NULL
this_day$Date <- NULL
sahara$Date <- NULL

#3. Filter out 2010 data
guardian <- guardian[year(guardian$NewDate) > 2010]
punch <- punch[year(punch$NewDate) > 2010]
vanguard <- vanguard[year(vanguard$NewDate) > 2010]
this_day <- this_day[year(this_day$NewDate) > 2010]
sahara <- sahara[year(sahara$NewDate) > 2010]

#4. Split into 2011-2015, 2015-2018
add_tenure <- function(outlet) {
  temp <- bind_rows(outlet[year(outlet$NewDate) %in% 2011:2014],
                    outlet[year(outlet$NewDate) %in% 2015 & month(outlet$NewDate) %in% 1:5])
  temp$tenure <- 1
  temp2 <- outlet[not(outlet$Headlines %in% temp$Headlines)]
  temp2$tenure <- 2
  x <- bind_rows(temp,temp2)
  return(x)
  }

guardian <- add_tenure(guardian)
punch <- add_tenure(punch)
vanguard <- add_tenure(vanguard)
this_day <- add_tenure(this_day)
sahara <- add_tenure(sahara)

punch$tenure %<>% as.character() 
sahara$tenure %<>% as.character()
this_day$tenure %<>% as.character()
vanguard$tenure %<>% as.character()

#5. Remove out of scope headlines
dim(guardian) %>% comcat()
dim(punch)
dim(this_day)
dim(vanguard)
dim(sahara)

out_of_scope <- c("cup","athletic","eagles","fifa","nff","football","soccer","basketball","tennis",
                  "messi","ronaldo","liverpool","barca","arsenal","champions league", "world cup",
                  "premier league", "trump", "pakistan","obama","clinton","isis","mutual fund","gotv boxing",
                  "libya","syria","china","egypt","korea","s'africa", "south africa", "north korea",
                  "erection", "s3x", "viagra", "doctor damages", "prince charles","lagos ibadan")

for(i in out_of_scope)  {
  guardian <- guardian[!grep(i, guardian$Headlines),]
  punch <- punch[!grep(i, punch$Headlines),]
  sahara <- sahara[!grep(i, sahara$Headlines),]
  vanguard <- vanguard[!grep(i, vanguard$Headlines),]
  this_day <- this_day[!grep(i, this_day$Headlines),]
}

this_day %>%
  filter(str_detect(Headlines, "gotv boxing"))

#C. Exploratory analysis-----------
data("stop_words")
#1. Topics and number of news items on them monthly.
#Viz - number of news monthly in line chart and wordcloud for headlines associated with topic
#fraud,bribe,corruption,nnpc,cbn,missing,dss,security,violence,technology,labour,subsidy

#report trend of headlines since 2011 till date
#Words:buhari,jonathan,security,bribe,corruption,technology,trade,idp,explosive,militant,ipob
plot_trend <- function(topic) {
  bind_rows(sahara,punch,vanguard,this_day) %>%
  filter(str_detect(Headlines, topic)) %>%
  group_by(month= floor_date(NewDate,"1 months")) %>%
  count(Headlines) %>%
  summarise(n_of_news =sum(n)) %>%
  ggplot(aes(month,n_of_news)) +
  geom_line() +
  geom_smooth() +
  labs(title=paste0("Report trend of '",topic, "' in the headlines since 2011 till date"),
       x=NULL,
       y="Number of Headlines monthly") +
    theme(title = element_text(size=15))+
    theme(axis.text.y = element_text(size=15))+
    theme_tq()}
plot_trend("killing")

#install.packages("wordcloud")
library(wordcloud)

#fraud,bribe,corruption,nnpc,cbn,missing,dss,security,violence,technology,labour,subsidy

#Compare tenures with word clouds
library(reshape2)
#fraud,bribe,corruption,nnpc,cbn,missing,dss,security,violence,technology
#labour,subsidy,trade,business,breaking
plot_wordcloud <- function(topic) {
  bind_rows(sahara,punch,vanguard,this_day) %>%
    filter(str_detect(Headlines,topic )) %>%
    group_by(tenure) %>%
    unnest_tokens(word,Headlines) %>%
    anti_join(stop_words) %>%
    filter(!word %in% topic) %>%
    count(tenure, word, sort = TRUE) %>%
    acast(word ~ tenure, value.var = "n", fill = 0) %>%
    comparison.cloud(colors = c("orange", "gray20"),
                     max.words = 200, random.order=FALSE)}
plot_wordcloud("violence")

#D. Data analysis-----------
#Insights
All <- bind_rows(sahara,punch,vanguard,this_day)

#1. General sentiment of alll headlines per year combining all outlets
plot_sentiment<- function(outlet){
  outlet %>%
    unnest_tokens(word,Headlines) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(year = year(NewDate),sentiment) %>%
    arrange(desc(n)) %>%
    mutate(sentiment = factor(sentiment, levels = rev(unique(sentiment)))) %>% 
    ggplot(aes(sentiment,n, fill = year)) +
    geom_col(show.legend = FALSE) +
    labs(title=paste0("Trend of NRC lexicon sentiments over the years for headlines from Sahara reporters"),
         x=NULL,y=NULL) +
    facet_wrap(~year, nrow = 2, scales = "free_x")+
    theme(title = element_text(size=15, colour="Red"))+
    theme(axis.text.y = element_text(size=15),
          axis.text.x = element_blank()) +
    theme_tq()+
    coord_flip()  }

plot_sentiment(All)
plot_sentiment(punch) #negative througout
plot_sentiment(sahara) #negative througout
plot_sentiment(vanguard)
plot_sentiment(this_day)

#2. Investigate buzzwords in the newsoutlets - sentiment for words
#security
#jonathan,buhari,osinbajo,sambo,police,court,apc,pdp,nnpc,dss
bind_rows(sahara,punch,vanguard,this_day) %>%
  group_by(news_outlet) %>%
  filter(str_detect(Headlines,"jonathan")) %>%
  unnest_tokens(word,Headlines) %>%
  inner_join(get_sentiments("bing")) %>%
  count(d=lubridate::floor_date(NewDate,"2 months"), sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot() +
  geom_line(aes(d, sentiment),color="red") +
  geom_line(aes(d,0)) +
  facet_wrap(~news_outlet,ncol = 1)+
  labs(title="Trend of sentiment for 'Jonathan' over 2011-2018",
       x=NULL,y="Sentiment score")+
  theme(title = element_text(size=15, colour="Red")) +
  theme(axis.text.y = element_text(size=15))+
  tidyquant::theme_tq()


#3. Trend analysis of each day for the sentiment of different news outlets - for GEJ and Buhari, etc
#Box plot depicting sentiment - per candidate, GEJ related headlines 
#Bigram - analyse monthly basis

#Sentiment comparison - Buhari/Jonathan
get_subject_sentiment <- function(outlet,subject)
  {
  outlet %>%
    filter(str_detect(Headlines,subject)) %>%
    mutate(timeline=floor_date(NewDate,"2 months")) %>%
    get_sentences()%$%
    sentiment_by(., by =c('news_outlet','timeline')) %>%
    select(news_outlet,timeline,ave_sentiment)
  }

jonathan <- get_subject_sentiment(bind_rows(sahara,punch,vanguard,this_day), "jonathan")
buhari <- get_subject_sentiment(bind_rows(sahara,punch,vanguard,this_day), "buhari")

buhari %>%
  ggplot() +
  geom_line(aes(timeline,ave_sentiment,color=news_outlet),show.legend = F) +
  geom_line(aes(timeline,0)) +
  facet_wrap(~news_outlet,ncol = 1)+
  labs(title="Trend of sentiment for 'Buhari' over 2011-2018",
       subtitle="Package: Sentimentr",
       x=NULL,y="Sentiment score")+
  theme(title = element_text(size=15, colour="Red")) +
  theme(axis.text.y = element_text(size=15))+
  tidyquant::theme_tq()

#Facet by tenure or facet by news_outlet
bind_rows(mutate(jonathan, tenure="GEJ"),
                    mutate(buhari, tenure="PMB")) %>%
  ggplot(aes(news_outlet,ave_sentiment))+
  geom_boxplot(outlier.colour="red")+
  geom_jitter(alpha=0.2)+
  labs(x="News Outlets",
       y="Average sentiment",
       title="Average sentiment per news outlet comparing Presidents Jonathan and Buhari")+
  facet_wrap(~tenure,ncol = 2)+
  theme(title = element_text(size=15, colour="Red")) +
  theme(axis.text.y = element_text(size=15))+
  tidyquant::theme_tq()

#Buzzword per tenure
#Facet by tenure or facet by news_outlet
#words: economy,senator,election,health
get_buzz <- function(outlet,buzz)
{ outlet %>%
    filter(str_detect(Headlines,buzz)) %>%
    mutate(timeseries=floor_date(NewDate,"1 months")) %>%
    get_sentences()%$%
    sentiment_by(., by =c('tenure','news_outlet','timeseries')) %>%
    select(-sd) %>%
    ggplot(aes(timeseries,ave_sentiment)) +
    geom_point(aes(size=word_count,color=news_outlet)) +
    geom_line(aes(timeseries,0))+
    geom_smooth()+
    labs(x=NULL,
         y="Average sentiment",title=paste0("Sentiment trend of the word '",buzz,"' over the years"))+
    facet_wrap(~news_outlet,scales = "free_x")+
    theme(title = element_text(size=12, colour="Red")) +
    theme(axis.text.y = element_text(size=12))+
    tidyquant::theme_tq()}

get_buzz(bind_rows(sahara,punch,vanguard,this_day),"economy")

get_boxplot <- function(outlet,buzz)
{ outlet %>%
    filter(str_detect(Headlines,buzz)) %>%
    mutate(timeseries=floor_date(NewDate,"1 months")) %>%
    get_sentences()%$%
    sentiment_by(., by =c('tenure','news_outlet','timeseries')) %>%
    select(-sd) %>%
    ggplot(aes(news_outlet,ave_sentiment)) +
    geom_boxplot(outlier.colour = "red") +
    labs(title=paste0("Boxplot for the word '",buzz,"' in the news."),
         subtitle="Outliers are in red. 1 - GEJ Admin, 2- Buhari Admin",
         x="News outlet",
         y="Average sentiment")+
    facet_wrap(~tenure,scales = "free_x")+
    theme(title = element_text(size=15, colour="Red")) +
    theme(axis.text.y = element_text(size=15))+
    tidyquant::theme_tq()}
get_boxplot(bind_rows(sahara,punch,vanguard,this_day),"economy")

#Bigram analysis
bigram <- bind_rows(sahara,punch,vanguard,this_day) %>%
  group_by(tenure,news_outlet) %>%
  unnest_tokens(bigram,Headlines,token = "ngrams",n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram,sort=T)

bigram %>% 
  filter(!bigram=="president jonathan" & !bigram=="president buhari" &!bigram=="premium times" &
           !bigram=="efcc press"&!bigram=="press release" & !bigram=="pm news" ) %>%
  group_by(tenure,news_outlet) %>% 
  top_n(5) %>% 
  ungroup() %>%
  mutate(bigram = reorder(bigram, n)) %>%
  ggplot(aes(bigram, n, fill = news_outlet)) +
  geom_col(show.legend = FALSE) +
  facet_grid(news_outlet~tenure,scales = "free") +
  coord_flip()+
  labs(title=paste0("Top word pairs in the news per news outlet during the GEJ & PMB administrations"),
       subtitle="1-GEJ Admin, 2-PMB Admin",
       x="Bigram",
       y="Word count")+
  theme(title = element_text(size=15, colour="Red")) +
  theme(axis.text.y = element_text(size=5))+
  tidyquant::theme_tq()

bigram %>%
   filter(!bigram=="president jonathan" &!bigram=="premium times" &
            !bigram=="efcc press"&!bigram=="press release" & !bigram=="pm news" ,
          tenure==1)%>%
  top_n(3) %>%
  ggplot(aes(y = n, axis1 = news_outlet, axis2 = bigram)) +
  geom_alluvium(aes(fill = news_outlet), width = 1/10) +
  geom_stratum(width = 1/10,  fill = "white", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("News outlet", "Words"))+
  labs(y=NULL,title="Top three wordpairs per news outlet for GEJ administration")+
  theme(title = element_text(size=15, colour="#182C61")) +
  theme(axis.text.y = element_text(size=15))+
  theme(panel.background = element_blank())+
  theme_tq()

bigram %>%
  filter(!bigram=="president buhari", tenure==2) %>%
  top_n(3) %>%
  ggplot(aes(y = n, axis1 = news_outlet, axis2 = bigram)) +
  geom_alluvium(aes(fill = news_outlet), width = 1/10) +
  geom_stratum(width = 1/10,  fill = "white", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:2, labels = c("News outlet", "Words"))+
  labs(y=NULL,title="Top three wordpairs per news outlet for PMB administration")+
  theme(title = element_text(size=15, colour="#182C61")) +
  theme(axis.text.y = element_text(size=15))+
  theme(panel.background = element_blank())+
  theme_tq()

#Most associated words bigrams
presidents <- c("buhari", "jonathan")
president <- bind_rows(sahara,punch,vanguard,this_day) %>%
  group_by(tenure,news_outlet) %>%
  unnest_tokens(bigram,Headlines,token = "ngrams",n=2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(word1 %in% presidents) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  count(tenure,word1, word2, score, sort = TRUE) %>%
  ungroup()

president %>%
  group_by(news_outlet) %>%
  mutate(Sentiment = ifelse(score>0,"positive","negative"),
         contribution = n * score) %>%
  top_n(10,abs(contribution)) %>% 
  ungroup() %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2,abs(contribution), fill = Sentiment)) +
  geom_col() +
  labs(x = NULL, y = "contribution") +
  facet_wrap(word1~news_outlet,nrow=2, scales = "free") +
  coord_flip()+
  labs(y=NULL,
       title="Top words associated with the names of Presidents Buhari & Jonathan as reported by news outlets")+
  theme(title = element_text(size=10, colour="Red")) +
  scale_fill_manual(values = c("#FD7272","#55E6C1")) +
  theme(axis.text.y = element_text(size=15))+
  tidyquant::theme_tq()

president %>%
  mutate(contribution=n*abs(score)) %>%
  group_by(tenure) %>% 
  mutate(Sentiment = ifelse(score>0,"positive","negative")) %>%
  top_n(10,contribution) %>%
  ungroup() %>%
  ggplot(aes(y = contribution,
             axis2 = word1, axis1 = word2)) +
  geom_alluvium(aes(fill = Sentiment), width = 1/10) +
  geom_stratum(width = 1/10, fill = "white", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c("Word","President"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  scale_fill_manual(values = c("#FD7272","#55E6C1")) +
  labs(title="Top words associated with the names of Presidents Buhari & Jonathan as reported by news outlets",
       subtitle="1-GEJ Administration. \t 2- PMB Administration") +
  theme(title = element_text(size=20, colour="Red")) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15)) +
  tidyquant::theme_tq()

#Trigram - check for oil,power,works,etc
trigram <- bind_rows(sahara,punch,vanguard,this_day) %>%
  group_by(tenure,news_outlet) %>%
  unnest_tokens(trigram, Headlines, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  unite(trigram, word1, word2, word3, sep = " ") %>%
  count(trigram, sort = TRUE) %>%
  ungroup()

trigram %>%
  filter(!trigram =="pm news lagos" & !trigram =="efcc press release" &
           !trigram =="press release efcc" ) %>%
  group_by(tenure,news_outlet) %>% 
  top_n(5) %>% 
  ungroup() %>%
  mutate(trigram = reorder(trigram, n)) %>%
  ggplot(aes(trigram, n, fill = news_outlet)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "Occurrence") +
  facet_grid(news_outlet~tenure, scales = "free") +
  coord_flip()+
  labs(y=NULL,
       title="Top five consecutive words per news outlet pairs by administration",
       subtitle="1-GEJ Administration.\t \t \t \t 2- PMB Administration") +
  theme(title = element_text(size=20, colour="Red")) +
  theme(axis.text.x = element_text(size=10)) +
  theme(axis.text.y = element_text(size=10))+
  theme_tq()

trigram %>%
  filter(!trigram =="pm news lagos" & !trigram =="efcc press release" &
           !trigram =="press release efcc" & !trigram =="president goodluck jonathan") %>%
  group_by(tenure,news_outlet) %>%
  top_n(3) %>%
  ggplot(aes(y = n, axis2 = news_outlet, axis1 = trigram)) +
  geom_alluvium(aes(fill = tenure), width = 1/10) +
  geom_stratum(width = 1/8, fill = "white", color = "black") +
  geom_label(stat = "stratum", label.strata = TRUE) +
  scale_x_continuous(breaks = 1:2, labels = c("News outlet", "Words"))+
  scale_fill_manual(values = c("#E69F00","#56B4E9")) +
  labs(y=NULL,legend="sentiment",
       title="Top three consecutive words appearing in the news per news outlet and tenure",
       subtitle="1-GEJ Administration.\t \t \t \t 2- PMB Administration") +
  theme(title = element_text(size=15, colour="Red")) +
  theme(axis.text.x = element_text(size=15)) +
  theme(axis.text.y = element_text(size=15))+
  theme(panel.background = element_blank())+
  theme_tq()
