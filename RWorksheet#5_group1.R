#GROUP1
#SUERO
#GALLENERO
#AHUMADA
#HUERVANA

library(rvest)
library(dplyr)
library(polite)
library(httr)


install.packages("dplyr")
install.packages("rvest")
install.packages("httr")
install.packages("polite")
install.packages("ggplot2")

#1.
```{r}
library(polite)
library(rvest)
library(httr)
library(dplyr)
link <- "https://www.imdb.com/chart/toptv/?ref_=nv_tvv_250"
session <- bow(link, user_agent= "Educational Purposes")
session

#rank of the Top 50 Tv Shows
rank <- scrape(session)  %>% html_nodes('h3.ipc-title__text') %>% html_text()
rank [1:51]

#Titles of the Top 50 Tv Shows
title <- scrape(session) %>%
  html_nodes('h3.ipc-title__text') %>%
  html_text()
title [1:51]

#Rating of the Top 50 Tv Shows
rating <- scrape(session) %>%
  html_nodes('span.ipc-rating-star--imdb') %>%
  html_text()
rating [1:51]

#Numbers Of Voters of the Top 50 Tv Shows
numberofvoters <- scrape(session) %>%
  html_nodes('span.ipc-rating-star--imdb') %>%
  html_text
num_voters50 <-numberofvoters [1:51]
num_voters50

#This shows on how many Episodes does the Top 50 Tv Shows have
episodes <- scrape(session) %>%
  html_nodes('span.ipc-rating-star--voteCount') %>%
  html_text%>%
  gsub("\\D","",.)
num_episodes <-episodes [1:51]
num_episodes

#The Year it was released (The Top 50 Tv Shows)
year <- scrape(session) %>%
  html_nodes('span.sc-479faa3c-8:nth-of-type(1)') %>%
  html_text()
year [1:51]
min_length <- min(length(rank), length(title), length(rating), length(numberofvoters), length(episodes), length(year))
min_length

data <- data.frame(
  Rank = seq(1, min_length),
  Title = title[1:min_length],
  Rating = rating[1:min_length],
  NumberOfVoters = numberofvoters[1:min_length],
  Episodes = episodes[1:min_length],
  Year = (sapply(year[1:min_length], function(x) if (grepl("^\\d+$", x)) as.integer(x) else NA))
)
View(data)


```

#2.
#Reviewers Data (Name, Date, Rating, Title of the Tv show, and the comment of each reviewers)

firstrevlink <- "https://www.imdb.com/title/tt1520211/reviews?ref_=tt_urv"
session2 <- bow(firstrevlink, user_agent= "Educational Purposes")
session2

ReviewersNametv1 <- scrape(session2) %>%
  html_nodes(' .display-name-link a') %>%
  html_text()
ReviewersNametv1

ReviewsDatetv1 <- scrape(session2) %>%
  html_nodes('.review-date') %>%
  html_text()
ReviewsDatetv1

UserRatingtv1 <- scrape(session2) %>%
  html_nodes('.rating-other-user-rating') %>%
  html_text()
UserRatingtv1

RevTitletv1 <-scrape(session2) %>%
  html_nodes('.parent a') %>%
  html_text()
RevTitletv1

TextRevtv1 <-scrape(session2) %>%
  html_nodes('.collapsable div.text') %>%
  html_text()
TextRevtv1

#Second Data
secondrevlink <- "https://www.imdb.com/title/tt12343534/reviews?ref_=tt_urv"
session3 <- bow(secondrevlink, user_agent= "Educational Purposes")
session3

ReviewersNametv2 <- scrape(session3) %>%
  html_nodes(' .display-name-link a') %>%
  html_text()
ReviewersNametv2

ReviewsDatetv2 <- scrape(session3) %>%
  html_nodes('.review-date') %>%
  html_text()
ReviewsDatetv2

UserRatingtv2 <- scrape(session3) %>%
  html_nodes('.rating-other-user-rating') %>%
  html_text()
UserRatingtv2

RevTitletv2 <-scrape(session3) %>%
  html_nodes('.parent a') %>%
  html_text()
RevTitletv2

TextRevtv2 <-scrape(session3) %>%
  html_nodes('.collapsable div.text') %>%
  html_text()
TextRevtv2


#third
thirdrevlink <- "https://www.imdb.com/title/tt12590266/?ref_=nv_sr_srsg_0_tt_7_nm_1_q_cyberpunk"
session4 <- bow(secondrevlink, user_agent= "Educational Purposes")
session4

ReviewersNametv3 <- scrape(session4) %>%
  html_nodes(' .display-name-link a') %>%
  html_text()
ReviewersNametv3

ReviewsDatetv3 <- scrape(session4) %>%
  html_nodes('.review-date') %>%
  html_text()
ReviewsDatetv3

UserRatingtv3 <- scrape(session4) %>%
  html_nodes('.rating-other-user-rating') %>%
  html_text()
UserRatingtv3

RevTitletv3 <-scrape(session4) %>%
  html_nodes('.parent a') %>%
  html_text()
RevTitletv3

TextRevtv3 <-scrape(session4) %>%
  html_nodes('.collapsable div.text') %>%
  html_text()
TextRevtv3

#fourth
fourthrevlink <- "https://www.imdb.com/title/tt7366338/reviews?ref_=tt_urv"
session5 <- bow(fourthrevlink, user_agent= "Educational Purposes")
session5

ReviewersNametv4 <- scrape(session5) %>%
  html_nodes('.display-name-link a') %>%
  html_text()
ReviewersNametv4

ReviewsDatetv4 <- scrape(session5) %>%
  html_nodes('.review-date') %>%
  html_text()
ReviewsDatetv4

UserRatingtv4 <- scrape(session5) %>%
  html_nodes('.rating-other-user-rating') %>%
  html_text()
UserRatingtv4

RevTitletv4 <-scrape(session5) %>%
  html_nodes('.parent a') %>%
  html_text()
RevTitletv4

TextRevtv4 <-scrape(session5) %>%
  html_nodes('.collapsable div.text') %>%
  html_text()
TextRevtv4

#fifth
fifthrevlink <- "https://www.imdb.com/title/tt2395695/reviews?ref_=tt_urv"
session5 <- bow(fifthrevlink, user_agent= "Educational Purposes")
session5

ReviewersNametv5 <- scrape(session5) %>%
  html_nodes('.display-name-link a') %>%
  html_text()
ReviewersNametv5

ReviewsDatetv5 <- scrape(session5) %>%
  html_nodes('.review-date') %>%
  html_text()
ReviewsDatetv5

UserRatingtv5 <- scrape(session5) %>%
  html_nodes('.rating-other-user-rating') %>%
  html_text()
UserRatingtv5

RevTitletv5 <-scrape(session5) %>%
  html_nodes('.parent a') %>%
  html_text()
RevTitletv5

TextRevtv5 <-scrape(session5) %>%
  html_nodes('.collapsable div.text') %>%
  html_text()
TextRevtv5


```
#3.

```{r}
library(ggplot2)
library(dplyr)

ggplot(data, aes(x = Year, fill = factor(Year))) +
  geom_bar() +
  labs(title = "Number of TV Shows Released by Year",
       x = "Year",
       y = "Number of TV Shows") +
  theme_minimal()

# Find the year with the most number of TV shows released
most_shows_year <- data %>%
  group_by(Year) %>%
  summarize(NumShows = n()) %>%
  arrange(desc(NumShows)) %>%
  slice(1)

cat("Year with the most TV shows released:", most_shows_year$Year, "\n")
cat("Number of TV shows released:", most_shows_year$NumShows, "\n")

```



#4.

```{r}
#1stproduct
amazonlink <- "https://www.amazon.com/Google-Pixel-Unlocked-Smartphone-Advanced/dp/B0CGTNPFZD/ref=sr_1_1_sspa?crid=2ASB0MTR2NB61&keywords=phones&qid=1701305956&sprefix=phon%2Caps%2C460&sr=8-1-spons&sp_csd=d2lkZ2V0TmFtZT1zcF9hdGY&th=1"
session6 <- bow(amazonlink, user_agent= "Educational Purposes")

price <-scrape(session6) %>%
  html_nodes('#corePriceDisplay_desktop_feature_div .a-price-whole') %>%
  html_text()
price

description <-scrape(session6) %>%
  html_nodes('#feature-bullets .a-list-item') %>%
  html_text()
description

prodrating <-scrape(session6) %>%
  html_nodes('#acrPopover') %>%
  html_text()
prodrating

prodreview <- scrape(session6) %>%
  html_nodes('#acrPopover') %>%
  html_text()
prodreview

#2rdproduct

amazonlink2nd <- "https://www.amazon.com/dp/B09S5MJLF6/ref=sspa_dk_detail_2?psc=1&pd_rd_i=B09S5MJLF6&pd_rd_w=tkc0I&content-id=amzn1.sym.386c274b-4bfe-4421-9052-a1a56db557ab&pf_rd_p=386c274b-4bfe-4421-9052-a1a56db557ab&pf_rd_r=AFCCC2P8F4VENZZF4VRZ&pd_rd_wg=GH9kV&pd_rd_r=585deed2-ce5f-497b-b3f1-dbc0ea621f62&s=wireless&sp_csd=d2lkZ2V0TmFtZT1zcF9kZXRhaWxfdGhlbWF0aWM&spLa=ZW5jcnlwdGVkUXVhbGlmaWVyPUEyQzFIRDc2SUVHRlU5JmVuY3J5cHRlZElkPUEwMzMxNzg1MlBaVzRHV042M0hSNyZlbmNyeXB0ZWRBZElkPUEwODMzMDgyMUhUNlhSRVNSTlM3RCZ3aWRnZXROYW1lPXNwX2RldGFpbF90aGVtYXRpYyZhY3Rpb249Y2xpY2tSZWRpcmVjdCZkb05vdExvZ0NsaWNrPXRydWU="

session7 <- bow(amazonlink2nd, user_agent= "Educational Purposes")



price2 <-scrape(session7) %>%
  html_nodes('#corePriceDisplay_desktop_feature_div .a-price-whole') %>%
  html_text()
price2

description2 <-scrape(session7) %>%
  html_nodes('#feature-bullets .a-list-item') %>%
  html_text()
description2

prodrating2 <-scrape(session7) %>%
  html_nodes('#acrPopover') %>%
  html_text()
prodrating2

prodreview2 <- scrape(session7) %>%
  html_nodes('#acrPopover') %>%
  html_text()
prodreview2

#3rdproduct

amazonlink3rd <- "https://www.amazon.com/OUKITEL-WP21-Smartphone-Unlocked-Waterproof/dp/B0BLZ7CRSC/ref=sr_1_2_sspa?crid=2ASB0MTR2NB61&keywords=phones&qid=1701307426&sprefix=phon%2Caps%2C460&sr=8-2-spons&sp_csd=d2lkZ2V0TmFtZT1zcF9hdGY&psc=1"

session8 <- bow(amazonlink3rd, user_agent= "Educational Purposes")



price3 <- scrape(session8) %>%
  html_nodes('.reinventPricePriceToPayMargin span.a-price-whole') %>%
  html_text()
price3

description3 <-scrape(session8) %>%
  html_nodes('#feature-bullets .a-list-item') %>%
  html_text()
description3

prodrating3 <-scrape(session8) %>%
  html_nodes('.a-size-base span.a-size-medium.a-color-base') %>%
  html_text()
prodrating3

prodreview3 <- scrape(session8) %>%
  html_nodes('#acrPopover') %>%
  html_text()
prodreview3

```



