#GROUP1
#SUERO
#GALLENERO
#AHUMADA
#HUERVANA
install.packages("tidyverse")
install.packages("dplyr")
install.packages("rvest")
install.packages("httr")
install.packages("polite")
install.packages("ggplot2")
library(rvest)
library(dplyr)
library(polite)
library(httr)
library(ggplot2)
library(tidyverse)
#1.

link <- "https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250"
session <- bow(link, user_agent= "Educational Purposes")
session

#1.1. Each group needs to extract the top 50 tv shows in Imdb.com. It will include the rank, the title of the tv show, tv rating, the number of people who voted, the number of episodes, the year it was released.

link <- "https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250"
session <- bow(link, user_agent= "Educational Purposes")
session

rank <- scrape(session) %>% 
  html_nodes('h3.ipc-title__text') %>% 
  html_text()
rank[1:51]

titles <- scrape(session) %>%
  html_nodes("h3.ipc-title__text") %>%
  html_text()
titles50 <- titles[2:51]

ratings <- scrape(session) %>%
  html_nodes("span.ipc-rating-star.ipc-rating-star--base.ipc-rating-star--imdb.ratingGroup--imdb-rating") %>%
  html_text()

cleaned_ratings <- substr(ratings, 1, 3)
ratings50 <- cleaned_ratings[1:50]

people_vote <- scrape(session) %>%
  html_nodes("span.ipc-rating-star--voteCount") %>%
  html_text()

people_vote50 <- people_vote[1:50]
cleaned_vote <- gsub("\\(|\\)", "", people_vote50)


episodes <- scrape(session) %>%
  html_nodes("span.sc-43986a27-8:nth-of-type(2)") %>%
  html_text()
episodes50 <- episodes[1:50]


year <- scrape(session) %>%
  html_nodes("span.sc-43986a27-8:nth-of-type(1)") %>%
  html_text()
year50 <- year[1:50]


topTv_df <- data.frame(
  Ranks = rank[1:50],  # Correct variable name
  Titles = titles50,
  Ratings = ratings50,
  People_Votes = cleaned_vote,
  Episodes = episodes50,
  Year = year50
)
topTv_df


#2. Reviewers Data (Name, Date, Rating, Title of the Tv show, and the comment of each reviewers)
#1st link


pad_with_na <- function(vec, target_length) {
  if (length(vec) < target_length) {
    return(c(vec, rep(NA, target_length - length(vec))))
  } else {
    return(vec)
  }
}


TWD_Link <- "https://www.imdb.com/title/tt1520211/reviews?ref_=tt_urv"
session2 <- bow(TWD_Link, user_agent = "Educational Purposes")


TWD_Title <- scrape(session2) %>%
  html_nodes("a[itemprop='url']") %>%
  html_text()

TWD_RevName <- scrape(session2) %>%
  html_nodes(".display-name-link a") %>%
  html_text()

TWD_RevDate <- scrape(session2) %>%
  html_nodes("span.review-date") %>%
  html_text()

TWD_UserRating <- scrape(session2) %>%
  html_nodes("span.rating-other-user-rating") %>%
  html_text() %>% str_trim()

TWD_TitleRev <- scrape(session2) %>%
  html_nodes("a.title") %>%
  html_text() %>%
  gsub("\n ", "", .)

TWD_TextRev <- scrape(session2) %>%
  html_nodes("div.text") %>%
  html_text()


max_length <- max(
  length(TWD_Title),
  length(TWD_RevName),
  length(TWD_RevDate),
  length(TWD_UserRating),
  length(TWD_TitleRev),
  length(TWD_TextRev)
)

TWD_Title <- pad_with_na(TWD_Title, max_length)
TWD_RevName <- pad_with_na(TWD_RevName, max_length)
TWD_RevDate <- pad_with_na(TWD_RevDate, max_length)
TWD_UserRating <- pad_with_na(TWD_UserRating, max_length)
TWD_TitleRev <- pad_with_na(TWD_TitleRev, max_length)
TWD_TextRev <- pad_with_na(TWD_TextRev, max_length)


TWD_Df <- data.frame(
  Tv_Shows = TWD_Title,
  Reviewer_Name = TWD_RevName,
  Reviewer_Date = TWD_RevDate,
  Reviewer_Rating = TWD_UserRating,
  Title_Review = TWD_TitleRev,
  Text_Review = TWD_TextRev
)

head(TWD_Df)

#2nd Link


pad_with_na <- function(vec, target_length) {
  if (length(vec) < target_length) {
    return(c(vec, rep(NA, target_length - length(vec))))
  } else {
    return(vec)
  }
}


JJK_Link <- "https://www.imdb.com/title/tt12343534/reviews?ref_=tt_urv"
session2 <- bow(JJK_Link, user_agent = "Educational Purposes")


JJK_Title <- scrape(session2) %>%
  html_nodes(".parent a") %>%
  html_text()

JJK_RevName <- scrape(session2) %>%
  html_nodes(".display-name-link") %>%
  html_text()

JJK_RevDate <- scrape(session2) %>%
  html_nodes("span.review-date") %>%
  html_text()

JJK_UserRating <- scrape(session2) %>%
  html_nodes("span.rating-other-user-rating") %>%
  html_text() %>% str_trim()

JJK_TitleRev <- scrape(session2) %>%
  html_nodes("a.title") %>%
  html_text() %>%
  gsub("\n ", "", .)

JJK_TextRev <- scrape(session2) %>%
  html_nodes("div.text") %>%
  html_text()


max_length <- max(
  length(JJK_Title),
  length(JJK_RevName),
  length(JJK_RevDate),
  length(JJK_UserRating),
  length(JJK_TitleRev),
  length(JJK_TextRev)
)

JJK_Title <- pad_with_na(JJK_Title, max_length)
JJK_RevName <- pad_with_na(JJK_RevName, max_length)
JJK_RevDate <- pad_with_na(JJK_RevDate, max_length)
JJK_UserRating <- pad_with_na(JJK_UserRating, max_length)
JJK_TitleRev <- pad_with_na(JJK_TitleRev, max_length)
JJK_TextRev <- pad_with_na(JJK_TextRev, max_length)


JJK_Df <- data.frame(
  Tv_Shows = JJK_Title,
  Reviewer_Name = JJK_RevName,
  Reviewer_Date = JJK_RevDate,
  Reviewer_Rating = JJK_UserRating,
  Title_Review = JJK_TitleRev,
  Text_Review = JJK_TextRev
)

head(JJK_Df)

#3rd Link


pad_with_na <- function(vec, target_length) {
  if (length(vec) < target_length) {
    return(c(vec, rep(NA, target_length - length(vec))))
  } else {
    return(vec)
  }
}

CE_Link <- "https://www.imdb.com/title/tt12590266/reviews?ref_=tt_urv"
session2 <- bow(CE_Link, user_agent = "Educational Purposes")


CE_Title <- scrape(session2) %>%
  html_nodes(".parent a") %>%
  html_text()

CE_RevName <- scrape(session2) %>%
  html_nodes(".display-name-link") %>%
  html_text()
CE_RevDate <- scrape(session2) %>%
  html_nodes("span.review-date") %>%
  html_text()

CE_UserRating <- scrape(session2) %>%
  html_nodes("span.rating-other-user-rating") %>%
  html_text() %>% str_trim()

CE_TitleRev <- scrape(session2) %>%
  html_nodes("a.title") %>%
  html_text() %>%
  gsub("\n ", "", .)

CE_TextRev <- scrape(session2) %>%
  html_nodes("div.text") %>%
  html_text()


max_length <- max(
  length(CE_Title),
  length(CE_RevName),
  length(CE_RevDate),
  length(CE_UserRating),
  length(CE_TitleRev),
  length(CE_TextRev)
)

CE_Title <- pad_with_na(CE_Title, max_length)
CE_RevName <- pad_with_na(CE_RevName, max_length)
CE_RevDate <- pad_with_na(CE_RevDate, max_length)
CE_UserRating <- pad_with_na(CE_UserRating, max_length)
CE_TitleRev <- pad_with_na(CE_TitleRev, max_length)
CE_TextRev <- pad_with_na(CE_TextRev, max_length)


CE_Df <- data.frame(
  Tv_Shows = CE_Title,
  Reviewer_Name = CE_RevName,
  Reviewer_Date = CE_RevDate,
  Reviewer_Rating = CE_UserRating,
  Title_Review = CE_TitleRev,
  Text_Review = CE_TextRev
)

head(CE_Df)



#4th Link

pad_with_na <- function(vec, target_length) {
  if (length(vec) < target_length) {
    return(c(vec, rep(NA, target_length - length(vec))))
  } else {
    return(vec)
  }
}

G_Link <- "https://www.imdb.com/title/tt0988818/reviews?ref_=tt_urv"
session2 <- bow(G_Link, user_agent = "Educational Purposes")


G_Title <- scrape(session2) %>%
  html_nodes(".parent a") %>%
  html_text()

G_RevName <- scrape(session2) %>%
  html_nodes(".display-name-link") %>%
  html_text()

G_RevDate <- scrape(session2) %>%
  html_nodes("span.review-date") %>%
  html_text()

G_UserRating <- scrape(session2) %>%
  html_nodes("span.rating-other-user-rating") %>%
  html_text() %>% str_trim()

G_TitleRev <- scrape(session2) %>%
  html_nodes("a.title") %>%
  html_text() %>%
  gsub("\n ", "", .)

G_TextRev <- scrape(session2) %>%
  html_nodes("div.text") %>%
  html_text()


max_length <- max(
  length(G_Title),
  length(G_RevName),
  length(G_RevDate),
  length(G_UserRating),
  length(G_TitleRev),
  length(G_TextRev)
)

G_Title <- pad_with_na(G_Title, max_length)
G_RevName <- pad_with_na(G_RevName, max_length)
G_RevDate <- pad_with_na(G_RevDate, max_length)
G_UserRating <- pad_with_na(G_UserRating, max_length)
G_TitleRev <- pad_with_na(G_TitleRev, max_length)
G_TextRev <- pad_with_na(G_TextRev, max_length)


G_Df <- data.frame(
  Tv_Shows = G_Title,
  Reviewer_Name = G_RevName,
  Reviewer_Date = G_RevDate,
  Reviewer_Rating = G_UserRating,
  Title_Review = G_TitleRev,
  Text_Review = G_TextRev
)

head(G_Df)


#5th Link

pad_with_na <- function(vec, target_length) {
  if (length(vec) < target_length) {
    return(c(vec, rep(NA, target_length - length(vec))))
  } else {
    return(vec)
  }
}

TBBT_Link <- "https://www.imdb.com/title/tt0898266/reviews?ref_=tt_urv"
session2 <- bow(TBBT_Link, user_agent = "Educational Purposes")


TBBT_Title <- scrape(session2) %>%
  html_nodes(".parent a") %>%
  html_text()

TBBT_RevName <- scrape(session2) %>%
  html_nodes(".display-name-link") %>%
  html_text()
TBBT_RevDate <- scrape(session2) %>%
  html_nodes("span.review-date") %>%
  html_text()

TBBT_UserRating <- scrape(session2) %>%
  html_nodes("span.rating-other-user-rating") %>%
  html_text() %>% str_trim()

TBBT_TitleRev <- scrape(session2) %>%
  html_nodes("a.title") %>%
  html_text() %>%
  gsub("\n ", "", .)

TBBT_TextRev <- scrape(session2) %>%
  html_nodes("div.text") %>%
  html_text()


max_length <- max(
  length(TBBT_Title),
  length(TBBT_RevName),
  length(TBBT_RevDate),
  length(TBBT_UserRating),
  length(TBBT_TitleRev),
  length(TBBT_TextRev)
)

TBBT_Title <- pad_with_na(CE_Title, max_length)
TBBT_RevName <- pad_with_na(TBBT_RevName, max_length)
TBBT_RevDate <- pad_with_na(TBBT_RevDate, max_length)
TBBT_UserRating <- pad_with_na(TBBT_UserRating, max_length)
TBBT_TitleRev <- pad_with_na(TBBT_TitleRev, max_length)
TBBT_TextRev <- pad_with_na(TBBT_TextRev, max_length)


TBBT_Df <- data.frame(
  Tv_Shows = TBBT_Title,
  Reviewer_Name = TBBT_RevName,
  Reviewer_Date = TBBT_RevDate,
  Reviewer_Rating = TBBT_UserRating,
  Title_Review = TBBT_TitleRev,
  Text_Review = TBBT_TextRev
)

head(TBBT_Df)



# 3.
library(ggplot2)
library(dplyr)

ggplot(topTv_df, aes(x = Year, fill = Year)) +
  geom_bar() +
  labs(title = "Number of TV Shows Released by Year",
       x = "Year",
       y = "Number of TV Shows") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 44, hjust = 1))



cat("The year with the most number of tv shows is", topTv_df$Year[1])

#4. Select 3 products from Amazon of the same category. Extract the price, description, ratings and reviews of each product.

install.packages("rvest")
install.packages("dplyr")
install.packages("polite")
install.packages("tidyverse")
#FIRST ITEM


library(rvest)
library(dplyr)
library(polite)
library(tidyverse)
item1 <- "www.amazon.com/dp/B09726KT4R/ref=sspa_dk_detail_4?pd_rd_i=B09726KT4R&pd_rd_w=yWjR0&content-id=amzn1.sym.eb7c1ac5-7c51-4df5-ba34-ca810f1f119a&pf_rd_p=eb7c1ac5-7c51-4df5-ba34-ca810f1f119a&pf_rd_r=6JKFM9VEXMM9286HNFHD&pd_rd_wg=FCM7S&pd_rd_r=4086727a-6e83-40bb-97ad-008259923fd8&s=videogames&sp_csd=d2lkZ2V0TmFtZT1zcF9kZXRhaWw&th=1"
session1 <- html_session(item1, user_agent = "Educational Purposes")


price1 <- session1 %>%
  html_nodes('span.a-offscreen') %>%
  html_text() %>%
  trimws()
price1[1]



description1 <- session1 %>%
  html_nodes('span.a-size-large.product-title-word-break') %>%
  html_text() %>%
  trimws()
description1


prodrating1 <- session1 %>%
  html_nodes('.a-icon-star .a-icon-alt') %>%
  html_text() %>%
  trimws()
prodrating1[1]



prodreview1 <- session1 %>%
  html_nodes('#acrCustomerReviewText') %>%
  html_text() %>%
  trimws()
prodreview1[1]

# SECOND ITEM

item2 <- "www.amazon.com/dp/B0BWS5SC27/ref=sspa_dk_detail_3?psc=1&pd_rd_i=B0BWS5SC27&pd_rd_w=TSdv4&content-id=amzn1.sym.eb7c1ac5-7c51-4df5-ba34-ca810f1f119a&pf_rd_p=eb7c1ac5-7c51-4df5-ba34-ca810f1f119a&pf_rd_r=M2BD287YW6NV4HFPQ06Q&pd_rd_wg=PZGn8&pd_rd_r=0dc44319-86e9-4883-ba51-5b3e50819318&s=videogames&sp_csd=d2lkZ2V0TmFtZT1zcF9kZXRhaWw"
session2 <- html_session(item2, user_agent = "Educational Purposes")

price2 <- session2 %>%
  html_nodes('span.a-offscreen') %>%
  html_text() %>%
  trimws()
price2[1]

description2 <- session2 %>%
  html_nodes('span.a-size-large.product-title-word-break') %>%
  html_text() %>%
  trimws()
description2



prodrating2 <- session2 %>%
  html_nodes('.a-icon-star .a-icon-alt') %>%
  html_text() %>%
  trimws()
prodrating2[1]

prodreview2 <- session2 %>%
  html_nodes('#acrCustomerReviewText') %>%
  html_text() %>%
  trimws()
prodreview2[1]

# THIRD ITEM

item3 <- "www.amazon.com/ASUS-ROG-Qi-Micro-Textured-Pass-Through/dp/B07P9GHDQ3/ref=pd_bxgy_img_d_sccl_2/139-9642401-5032164?pd_rd_w=1TyH6&content-id=amzn1.sym.839d7715-b862-4989-8f65-c6f9502d15f9&pf_rd_p=839d7715-b862-4989-8f65-c6f9502d15f9&pf_rd_r=KFX6Y6YBRKAQ68R4VDWM&pd_rd_wg=nhO8W&pd_rd_r=63566a43-6820-4810-a9ea-cde7cca74d97&pd_rd_i=B07P9GHDQ3&psc=1"
session3 <- html_session(item3, user_agent = "Educational Purposes")

price3 <- session3 %>%
  html_nodes('span.a-offscreen') %>%
  html_text() %>%
  trimws()
price3[1]


description3 <- session3 %>%
  html_nodes('span.a-size-large.product-title-word-break') %>%
  html_text() %>%
  trimws()
description3


prodrating3 <- session3 %>%
  html_nodes('.a-icon-star .a-icon-alt') %>%
  html_text() %>%
  trimws()
prodrating3[1]


prodreview3 <- session3 %>%
  html_nodes('#acrCustomerReviewText') %>%
  html_text() %>%
  trimws()
prodreview3[1]

