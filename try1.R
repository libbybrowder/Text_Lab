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

### tf-idf
PHLraw<- as_tibble(read_lines("Files (25).txt"))
NYTraw<- as_tibble(read_lines("NYT 40.txt"))                   
Chicagoraw<- as_tibble("chicago 40.txt")

data_prep <- function(x,y,z){
  i <- as_tibble(t(x))
  ii <- unite(i,"text",y:z,remove = TRUE,sep = "")
}


