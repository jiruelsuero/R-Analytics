#GROUP1
#SUERO
#HUERVANA
#AHUMADA
#GALLENERO

library(rvest)
library(readr)
library(tm)
library(NLP)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)


scrape_reviews <- function(url) {
  webpage <- read_html(url)
  reviews <- webpage %>% html_nodes(".text.show-more__control") %>% html_text()
  reviews <- gsub("[\r\n]", "", reviews)
  reviews <- gsub("\\s+", " ", reviews)
  return(reviews)
}


next_page <- function(url) {
  webpage <- read_html(url)
  next_page_url <- webpage %>% html_node(".load-more-data") %>% html_attr("data-key")
  return(next_page_url)
}


url <- "https://www.imdb.com/title/tt0898266/reviews/?ref_=tt_ov_rt"


reviews_list <- c()
reviews_count <- 0

while (reviews_count < 300) {
  reviews <- scrape_reviews(url)
  reviews_list <- c(reviews_list, reviews)
  reviews_count <- reviews_count + length(reviews)
  
  next_key <- next_page(url)
  if (is.na(next_key)) {
    break
  } else {
    url <- paste0("https://www.imdb.com/title/tt0898266/reviews/_ajax?paginationKey=", next_key)
  }
}


reviews_list <- head(reviews_list, 300)


sentiment <- ifelse(nchar(reviews_list) > 100, "Positive", "Negative")


reviews_with_sentiment <- data.frame(Review = reviews_list, Sentiment = sentiment)
reviews_with_sentiment

sentiment_counts <- table(reviews_with_sentiment$Sentiment)


sentiment_data <- data.frame(Sentiment = names(sentiment_counts), Count = as.numeric(sentiment_counts))


ggplot(sentiment_data, aes(x = Sentiment, y = Count, fill = Sentiment)) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  labs(title = "Distribution of Sentiment in Reviews",
       x = "Sentiment",
       y = "Count") +
  theme_minimal()


TheBigBangTheory_Corpus <- Corpus(VectorSource(reviews_with_sentiment$Review))
TheBigBangTheory_Corpus <- tm_map(TheBigBangTheory_Corpus, content_transformer(tolower))
TheBigBangTheory_Corpus <- tm_map(TheBigBangTheory_Corpus, removeWords, stopwords("en"))
TheBigBangTheory_DTM <- DocumentTermMatrix(TheBigBangTheory_Corpus)


TheBigBangTheory_wordcloud <- wordcloud(words = names(sort(colSums(as.matrix(TheBigBangTheory_DTM)))), 
                                        freq = sort(colSums(as.matrix(TheBigBangTheory_DTM))),
                                        min.freq = 1,
                                        scale = c(3, 0.5),
                                        max.words = 200,
                                        random.order = FALSE, 
                                        colors = brewer.pal(8, "Dark2"))


TheBigBangTheory_wordcloud