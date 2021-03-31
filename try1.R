# hi I have literally no idea what's going on
library(tidyverse)
install.packages("tidytext")
library(tidytext)
library(ggwordcloud)
library(ggwordcloud)
install.packages("gutenbergr") 
library(gutenbergr)
#install.packages('textdata')
library(textdata)


#PHL inquirer-- 'data science'
PHLInquirer<- read_lines("Files (25).txt")
PHLInquirer<- tibble(PHLInquirer) 
PHLInquirer$PHLInquirer<- as.character(PHLInquirer$PHLInquirer) #into large character

PHLInquirer <- PHLInquirer %>%
  unnest_tokens(word, PHLInquirer)%>%
  anti_join(stop_words)%>% 
  count(word, sort=TRUE)

#sentiment analysis: 

PHLInqAFFIN<- PHLInquirer %>%
  inner_join(get_sentiments("afinn"))

PHLInqNRC<- PHLInquirer %>%
  inner_join(get_sentiments("nrc"))

PHLInqBING<- PHLInquirer %>%
  inner_join(get_sentiments("bing"))

table(PHLInqBING$sentiment)
table(PHLInqNRC$sentiment)


ggplot(data = PHLInqAFFIN, 
       aes(x=value)
)+
  geom_histogram()+
  ggtitle("PHL Affin Sentiment Range")+
  theme_minimal()


# NYT 

NYT<- read_lines("NYT 40.txt")
NYT<- tibble(NYT) 
NYT$NYT<- as.character(NYT$NYT) #into large character

NYT <- NYT %>%
  unnest_tokens(word, NYT)%>%
  anti_join(stop_words)%>% 
  count(word, sort=TRUE)

# NYT sentiment analysis: 

NYTaffin<- NYT %>%
  inner_join(get_sentiments("afinn"))

NYTnrc<- NYT %>%
  inner_join(get_sentiments("nrc"))

NYTbing<- NYT %>%
  inner_join(get_sentiments("bing"))

table(NYTbing$sentiment)
table(NYTnrc$sentiment)


ggplot(data = NYTaffin, 
       aes(x=value)
)+
  geom_histogram()+
  ggtitle("NYT Affin Sentiment Range")+
  theme_minimal()


#chicago daily herald
chicago<- read_lines("chicago 40.txt")
chicago<- tibble(chicago) 
chicago$chicago<- as.character(chicago$chicago) #into large character

chicago <- chicago %>%
  unnest_tokens(word, chicago)%>%
  anti_join(stop_words)%>% 
  count(word, sort=TRUE)

# Chicago sentiment analysis: 

chicagoAFINN<- chicago %>%
  inner_join(get_sentiments("afinn"))

chicagoNRC<- chicago %>%
  inner_join(get_sentiments("nrc"))

chicagoBING<- chicago %>%
  inner_join(get_sentiments("bing"))

table(chicagoBING$sentiment)
table(chicagoNRC$sentiment)


ggplot(data = chicagoAFINN, 
       aes(x=value)
)+
  geom_histogram()+
  ggtitle("Chicago Affin Sentiment Range")+
  theme_minimal()

#Dayton, Ohio
dayton<- read_lines("dayton 50.txt")
dayton<- tibble(dayton) 
dayton$dayton<- as.character(dayton$dayton) #into large character

dayton <- dayton %>%
  unnest_tokens(word, dayton)%>%
  anti_join(stop_words)%>% 
  count(word, sort=TRUE)

# Dayton sentiment analysis: 

daytonAFINN<- dayton %>%
  inner_join(get_sentiments("afinn"))

daytonNRC<- dayton %>%
  inner_join(get_sentiments("nrc"))

daytonBING<- dayton %>%
  inner_join(get_sentiments("bing"))

table(daytonBING$sentiment)
table(daytonNRC$sentiment)


ggplot(data = daytonAFINN, 
       aes(x=value)
)+
  geom_histogram()+
  ggtitle("Dayton Affin Sentiment Range")+
  theme_minimal()

## LA Times

la_times <- read_lines("la_times7.txt")

la_times <- tibble(la_times)


la_times <- la_times %>%
  unnest_tokens(word, la_times)%>%
  anti_join(stop_words)%>% 
  count(word, sort=TRUE)

# LA Sentiment Analysis:

la_sentiment_affin <- la_times %>%
  inner_join(get_sentiments("afinn"))#using a inner join to match words and add the sentiment variable

la_sentiment_nrc <- la_times %>%
  inner_join(get_sentiments("nrc"))

la_sentiment_bing <- la_times %>%
  inner_join(get_sentiments("bing"))

table(la_sentiment_bing$sentiment)

table(la_sentiment_nrc$sentiment)

ggplot(data = la_sentiment_affin, 
       aes(x=value)
        )+
  geom_histogram()+
  ggtitle("LA Times Sentiment Range")+
  theme_minimal()
# using ggwordcloud package
set.seed(42)
ggplot(la_times[1:50,], aes(label = word, size = n)
       ) +
  geom_text_wordcloud() +
  theme_minimal()


### tf-idf-- look at frequency comapred with the whole corpus
#treat each newspaper as a document in a corpus
PHLraw<- as_tibble(read_lines("Files (25).txt"))
NYTraw<- as_tibble(read_lines("NYT 40.txt"))                   
Chicagoraw<- as_tibble(read_lines("chicago 40.txt"))
Daytonraw<- as_tibble(read_lines("dayton 50.txt"))
la_times_raw <- as_tibble(read_lines("la_times.txt"))


data_prep <- function(x,y,z){
  i <- as_tibble(t(x))
  ii <- unite(i,"text",y:z,remove = TRUE,sep = "")
}

PHLprep<- data_prep(PHLraw,'V1','V1738')
NYTprep<- data_prep(NYTraw, 'V1', 'V3085')
Chicagoprep<- data_prep(Chicagoraw, 'V1','V1775')
Daytonprep<- data_prep(Daytonraw, 'V1', 'V2913')
LAprep <- data_prep(la_times_raw, 'V1', 'V307')
cities <- c("Philadelphia","NYC","Chicago", "Dayton", "Los Angeles")


tf_idf_text <- tibble(cities,text=t(tibble(PHLprep,NYTprep,Chicagoprep,Daytonprep, LAprep,.name_repair = "universal")))

class(tf_idf_text)

word_count <- tf_idf_text %>%
  unnest_tokens(word, text) %>%
  count(cities, word, sort = TRUE)

total_words <- word_count %>% 
  group_by(cities) %>% 
  summarize(total = sum(n))

news_words <- left_join(word_count, total_words)

View(news_words)

news_words2 <- news_words %>%
  bind_tf_idf(word, cities, n)

View(news_words2)

