library(HadoopStreaming)
library(rJava)
library(SparkR)
library(bigdatadist)
library(tidyverse) #  data manipulation and graphs
library(stringr) #  string manipulation
library(lubridate) #  date manipulation
library('wordcloud') #  wordcloud
library(tidytext) # tidy implementation of NLP methods
library(DT)       # table format display of data
library(leaflet) # maps

library(igraph) #  graphs
library(ggraph) #  graphs

library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming

library(textcat)

library(textdata)

##Read the data

rm(list=ls())

fillColor = "#FFA07A"
fillColor2 = "#F1C40F"

reviews <- read_csv('E:/major projectr/yelp_review.csv')
business <- read_csv("E:/major projectr/yelp_business.csv")






#Business data

datatable(head(business), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

#Reviews data


glimpse(reviews)



#Detecting the language of the reviews


textcat(reviews[1:10,]$text)



#Most Popular Categories

categories = str_split(business$categories,";")
categories = as.data.frame(unlist(categories))
colnames(categories) = c("Name")

categories %>%
  group_by(Name) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(Name = reorder(Name,Count)) %>%
  head(10) %>%
  
  
  ggplot(aes(x = Name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor2) +
  geom_text(aes(x = Name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of Category', y = 'Count', 
       title = 'Top 10 Categories of Business') +
  coord_flip() + 
  theme_bw()

 


#Top Ten Cities with the most Business parties mentioned in Yelp


business %>%
  group_by(city) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(City = reorder(city,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = City,y = Count)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = City, y = 1, label = paste0("(",round(Count/1e3)," K )",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'City', y = 'Count of Reviews', 
       title = 'Top Ten Cities with the most Business parties in Yelp') +
  coord_flip() + 
  theme_bw()


 

#Map of the business parties in Las vegas
LasvegasCoords = business %>% filter(city == "Las Vegas")

center_lon = median(LasvegasCoords$longitude,na.rm = TRUE)
center_lat = median(LasvegasCoords$latitude,na.rm = TRUE)

leaflet(LasvegasCoords) %>% addProviderTiles("Esri.NatGeoWorldMap") %>%
  addCircles(lng = ~longitude, lat = ~latitude,radius = ~sqrt(review_count))  %>%
  
  # controls
  setView(lng=center_lon, lat=center_lat,zoom = 13)

 



#Business with most Five Star Reviews from Users

most5StarsReviews = reviews %>%
  filter(stars == 5) %>%
  group_by(business_id) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(BusinessID = reorder(business_id,Count)) %>%
  head(10)

most5StarsReviews = inner_join(most5StarsReviews,business)

most5StarsReviews %>%
  mutate(name = reorder(name,Count)) %>%
  ggplot(aes(x = name,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = name, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Name of the Business', 
       y = 'Count', 
       title = 'Name of the Business and Count') +
  coord_flip() +
  theme_bw()

 


#**"Mon Ami Gabi"**
mon_ami_gabi = business %>% filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw") %>%
  select(name,neighborhood,city,state,postal_code,categories)

datatable(head(mon_ami_gabi), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

 

##Useful,funny,cool reviews
mon_ami_gabi_reviews = reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw")

mon_ami_gabi_reviews %>%
  group_by(useful) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(useful = reorder(useful,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = useful,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = useful, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Useful Reviews', 
       y = 'Count', 
       title = 'Useful Reviews and Count') +
  coord_flip() +
  theme_bw()


mon_ami_gabi_reviews %>%
  group_by(funny) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(funny = reorder(funny,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = funny,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = funny, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Funny Reviews', 
       y = 'Count', 
       title = 'Funny Reviews and Count') +
  coord_flip() +
  theme_bw()


mon_ami_gabi_reviews %>%
  group_by(cool) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(cool = reorder(cool,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = cool,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = fillColor) +
  geom_text(aes(x = cool, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Cool Reviews', 
       y = 'Count', 
       title = 'Cool Reviews and Count') +
  coord_flip() +
  theme_bw()


 

## Word Cloud of Mon Ami Gabi

createWordCloud = function(train)
{
  train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(30) %>%
    
    with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}

createWordCloud(reviews %>%
                  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw"))

 

##Top Ten most common Words of the business **"Mon Ami Gabi"**

reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

 

## Sentiment Analysis - Postive and Not So Postive Words of **"Mon Ami Gabi"**
positiveWordsBarGraph <- function(SC) {
  contributions <- SC %>%
    unnest_tokens(word, text) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(score))
  
  contributions %>%
    top_n(20, abs(contribution)) %>%
    mutate(word = reorder(word, contribution)) %>%
    head(20) %>%
    ggplot(aes(word, contribution, fill = contribution > 0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw()
}

positiveWordsBarGraph(reviews %>%
                        filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw"))


 

## Calculate Sentiment for the reviews

calculate_sentiment <- function(review_text)
{
  sentiment_lines  =  review_text %>%
    filter(textcat(text) == "english") %>%  # considering only English text
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(review_id) %>%
    summarize(sentiment = mean(score),words = n()) %>%
    ungroup() %>%
    filter(words >= 5) 
  
  return(sentiment_lines)
  
}


sentiment_lines = calculate_sentiment(mon_ami_gabi_reviews)

head(sentiment_lines)

 



## Negative Reviews

display_neg_sentiments <- function(sentiment_lines,review_text)
{
  neg_sentiment_lines = sentiment_lines %>%
    arrange(desc(sentiment))  %>%
    top_n(-10, sentiment) %>%
    inner_join(review_text, by = "review_id") %>%
    select(date,sentiment,text) 
  
  datatable(neg_sentiment_lines, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
}

display_neg_sentiments(sentiment_lines,mon_ami_gabi_reviews)

 

## Positive  Reviews

display_pos_sentiments <- function(sentiment_lines,review_text)
{
  pos_sentiment_lines = sentiment_lines %>%
    arrange(desc(sentiment))  %>%
    top_n(10, sentiment) %>%
    inner_join(review_text, by = "review_id") %>%
    select(date,sentiment,text) 
  
  datatable(pos_sentiment_lines, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))
  
}

display_pos_sentiments(sentiment_lines,mon_ami_gabi_reviews)


 




## Most Common Bigrams of **"Mon Ami Gabi"**


count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}


visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

visualize_bigrams_individual <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  select(bigram,review_id) %>%
  head(10)


reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw") %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  filter(!word1 %in% c("mon","ami")) %>%
  filter(!word2 %in% c("gabi")) %>%
  unite(bigramWord, word1, word2, sep = " ") %>%
  group_by(bigramWord) %>%
  tally() %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(bigramWord = reorder(bigramWord,n)) %>%
  head(10) %>%
  
  ggplot(aes(x = bigramWord,y = n)) +
  geom_bar(stat='identity',colour="white", fill = fillColor2) +
  geom_text(aes(x = bigramWord, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Bigram', 
       y = 'Count', 
       title = 'Bigram and Count') +
  coord_flip() + 
  theme_bw()

 


##Relationship among words

bigramsMonAmiGabi <- reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw") %>%
  count_bigrams()

bigramsMonAmiGabi %>%
  filter(n > 50) %>%
  visualize_bigrams()

 

###Relationship of words with **steak**
bigramsMonAmiGabi %>%
  filter(word1 == "steak" | word2 == "steak") %>%
  filter(n > 30) %>%
  visualize_bigrams()

 

###Relationship of words with **french**

bigramsMonAmiGabi %>%
  filter(word1 == "french" | word2 == "french" ) %>%
  filter(n > 30) %>%
  visualize_bigrams()

 

# **Bacchanal Buffet**

bacchanal = business %>% filter(business_id == "RESDUcs7fIiihp38-d6_6g") %>%
  select(name,neighborhood,city,state,postal_code,categories)

datatable(head(bacchanal), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

 

## Word Cloud of **Bacchanal Buffet**
bacchanal = reviews %>% filter(business_id == "RESDUcs7fIiihp38-d6_6g")

createWordCloud(bacchanal)

 


##Top Ten most common Words of the business **"Bacchanal Buffet"**

bacchanal %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

 

## Sentiment Analysis - Postive and Not So Postive Words of **Bacchanal Buffet**

positiveWordsBarGraph(bacchanal)


 

## Calculate Sentiment for the reviews

sentiment_lines = calculate_sentiment(bacchanal)

head(sentiment_lines)

 



## Negative Reviews

display_neg_sentiments(sentiment_lines,bacchanal)

 

## Positive  Reviews

display_pos_sentiments(sentiment_lines,bacchanal)


 



##Relationship among words in Bacchanal Buffet

bigrams_bacchanal <- bacchanal %>%
  count_bigrams()

bigrams_bacchanal %>%
  filter(n > 100) %>%
  visualize_bigrams()

 

###Relationship of words with **crab**

bigramsMonAmiGabi %>%
  filter(word1 == "crab" | word2 == "crab" ) %>%
  visualize_bigrams()

 

###Relationship of words with **food**

bigramsMonAmiGabi %>%
  filter(word1 == "food" | word2 == "food" ) %>%
  filter(n > 10) %>%
  visualize_bigrams()

 


#Top Ten Business in Toronto

toronto_biz = business %>%
  filter(city == "Toronto") %>%
  arrange(desc(review_count,stars)) %>%
  select(name,neighborhood,address,review_count,stars) %>%
  head(10)

datatable(toronto_biz, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


 



# **Pai Northern Thai Kitchen**

##Word Cloud of business **Pai Northern Thai Kitchen**

#r_BrIgzYcwo1NAuG9dLbpg

createWordCloud(reviews %>%
                  filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg"))

 


##Ten most common words used in reviews of business **Pai Northern Thai Kitchen**

reviews %>%
  filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

 


## Sentiment Analysis - Postive and Not So Postive Words of **Pai Northern Thai Kitchen**

positiveWordsBarGraph(reviews %>%
                        filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg"))


 


## Calculate Sentiment for the reviews


pai_thai = reviews %>%
  filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg")

sentiment_lines = calculate_sentiment(pai_thai)

head(sentiment_lines)

 



## Negative Reviews
display_neg_sentiments(sentiment_lines,pai_thai)

 

## Positive  Reviews

display_pos_sentiments(sentiment_lines,pai_thai)


 


##Relationship among words in **Pai Northern Thai Kitchen**


bigrams_thai <- reviews %>%
  filter(business_id == "r_BrIgzYcwo1NAuG9dLbpg") %>%
  count_bigrams()

bigrams_thai %>%
  filter(n > 50) %>%
  visualize_bigrams()

 

###Relationship of words with **thai**
bigrams_thai %>%
  filter(word1 == "thai" | word2 == "thai" ) %>%
  filter(n > 5) %>%
  visualize_bigrams()

 


# **Chipotle business**

chipotle_biz = business %>%
  filter(str_detect(name,"Chipotle") )%>%
  arrange(desc(review_count,stars))

datatable(head(chipotle_biz), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


 


# Chipotle Business in Yonge Street Toronto
##Word Cloud of business **Chipotle Business in Yonge Street Toronto**

#gOBxVkHpqtjRRxHBIrpnMA

chioptle_yonge = reviews %>%
  filter(business_id == "gOBxVkHpqtjRRxHBIrpnMA")

createWordCloud(chioptle_yonge)

 


##Top Ten most common Words of the business **"Chipotle Business in Yonge Street Toronto"**

chioptle_yonge %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

 

## Sentiment Analysis - Postive and Not So Postive Words of **Chipotle Business in Yonge Street Toronto**

positiveWordsBarGraph(chioptle_yonge)


 

## Calculate Sentiment for the reviews


sentiment_lines = calculate_sentiment(chioptle_yonge)

head(sentiment_lines)

 



## Negative Reviews

display_neg_sentiments(sentiment_lines,chioptle_yonge)

 

## Positive  Reviews

display_pos_sentiments(sentiment_lines,chioptle_yonge)


 


##Relationship among words in Chipotle Business in Yonge Street Toronto
bigrams_chioptle_yonge <- chioptle_yonge %>%
  count_bigrams()

bigrams_chioptle_yonge %>%
  filter(n > 5) %>%
  visualize_bigrams()

 



#Topic Modelling   


##LDA Function
unique_indexes <- unique(DTM$i) # get the index of each unique value
DTM <- DTM[unique_indexes,] # get a subset of only those indexes

# preform LDA & get the words/topic in a tidy text format
lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
topics <- tidy(lda, matrix = "beta")

# get the top ten terms for each topic
top_terms <- topics  %>% # take the topics data frame and..
  group_by(topic) %>% # treat each topic as a different group
  top_n(10, beta) %>% # get the top 10 most informative words
  ungroup() %>% # ungroup
  arrange(topic, -beta) # arrange words in descending informativeness

# if the user asks for a plot (TRUE by default)
if(plot == T){
  # plot the top ten terms for each topic in order
  top_terms %>% # take the top terms
    mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
    ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
    geom_col(show.legend = FALSE) + # as a bar plot
    facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
    labs(x = NULL, y = "Beta") + # no x label, change y label 
    coord_flip() # turn bars sideways
}else{ 
  # if the user does not request a plot
  # return a list of sorted terms instead
  return(top_terms)
}
}


 

##Topic Modelling for **Mon Ami Gabi**  

create_LDA_topics <- function(business_text,custom_stop_words)
{
  # create a document term matrix to clean
  reviewsCorpus <- Corpus(VectorSource(business_text$text)) 
  reviewsDTM <- DocumentTermMatrix(reviewsCorpus)
  
  # convert the document term matrix to a tidytext corpus
  reviewsDTM_tidy <- tidy(reviewsDTM)
  
  # remove stopwords
  reviewsDTM_tidy_cleaned <- reviewsDTM_tidy %>% # take our tidy dtm and...
    anti_join(stop_words, by = c("term" = "word")) %>% # remove English stopwords and...
    anti_join(custom_stop_words, by = c("term" = "word")) # remove my custom stopwords
  
  top_terms_by_topic_LDA(reviewsDTM_tidy_cleaned$term, number_of_topics = 4)
  
}

monamigabi = reviews %>%
  filter(business_id == "4JNXUYY8wbaaDmk3BPzlWw")

custom_stop_words <- tibble(word = c("mon","ami","gabi","restaurant","food","vegas"))

create_LDA_topics(monamigabi,custom_stop_words)


 


##Topic Modelling for **Bacchanal Buffet**

custom_stop_words <- tibble(word = c("restaurant","food"))

create_LDA_topics(bacchanal,custom_stop_words)

 


##Topic Modelling for **Pai Northern Thai Kitchen**

custom_stop_words <- tibble(word = c("thai","restaurant","food"))

create_LDA_topics(pai_thai,custom_stop_words)

city_biz = business %>%
  filter(city == "Phoenix") %>%
  arrange(desc(review_count,stars)) %>%
  select(name,neighborhood,address,review_count,stars) %>%
  head(10)

datatable(city_biz, style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))


 



## Topic Modelling for **Phoenix City**

CityCoords = business %>%
  filter(city == "Phoenix")

city_words = inner_join(CityCoords,reviews) %>% select(date,text,review_id) %>% sample_n(10000)

custom_stop_words <- tibble(word = c("restaurant","food"))

create_LDA_topics(city_words,custom_stop_words)

 




## Word Cloud of **Phoenix City**

createWordCloud(city_words)

 


##Top Ten most common Words of the business **Phoenix City**

city_words %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

 





## Sentiment Analysis - Postive and Not So Postive Words of **Phoenix City**

positiveWordsBarGraph(city_words)


## Calculate Sentiment for the reviews

sentiment_lines = calculate_sentiment(city_words)

head(sentiment_lines)

 

## Negative Reviews

display_neg_sentiments(sentiment_lines,city_words)

 

## Positive  Reviews
display_pos_sentiments(sentiment_lines,city_words)


 


#**Bobby Q**

##Word Cloud of business **Bobby Q**

#VyVIneSU7XAWgMBllI6LnQ

bobby_q = reviews %>%
  filter(business_id == "VyVIneSU7XAWgMBllI6LnQ")

createWordCloud(bobby_q)


 


##Ten most common words used in reviews of business **Bobby Q**

reviews %>%
  filter(business_id == "VyVIneSU7XAWgMBllI6LnQ") %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% c('food','restaurant')) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  head(10) %>%
  
  ggplot(aes(x = word,y = n)) +
  geom_bar(stat='identity',colour="white", fill =fillColor) +
  geom_text(aes(x = word, y = 1, label = paste0("(",n,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count', 
       title = 'Word Count') +
  coord_flip() + 
  theme_bw()

 


## Sentiment Analysis - Postive and Not So Postive Words of **Bobby Q**

positiveWordsBarGraph(reviews %>%
                        filter(business_id == "VyVIneSU7XAWgMBllI6LnQ"))


 


## Calculate Sentiment for the reviews


bobbyQ = reviews %>%
  filter(business_id == "VyVIneSU7XAWgMBllI6LnQ")

sentiment_lines = calculate_sentiment(bobbyQ)

head(sentiment_lines)

 



## Negative Reviews


display_neg_sentiments(sentiment_lines,bobbyQ)

 

## Positive  Reviews


display_pos_sentiments(sentiment_lines,bobbyQ)


 


##Relationship among words in **Bobby Q**

bigrams_restaurant <- reviews %>%
  filter(business_id == "VyVIneSU7XAWgMBllI6LnQ") %>%
  count_bigrams()

bigrams_restaurant %>%
  filter(n > 50) %>%
  visualize_bigrams()

