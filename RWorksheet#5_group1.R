library(rvest)
library(dplyr)
library(polite)
library(httr)

polite::use_manners(save_as = 'polite_scrape.R')


url <- 'https://www.imdb.com/chart/tvmeter/?ref_=nv_tvv_mptv'

session <- bow(url, user_agent = "Educational")

session

rank_list <- scrape(session) %>%
  html_nodes('div.sc-94da5b1b-0.soBIM.meter-const-ranking.sc-479faa3c-6.glWBvR.cli-meter-title-header') %>%
  html_text
rank_list [1:50]

title_list <- scrape (session) %>%
  html_nodes('h3.ipc-title__text')%>%
  html_text
title_list[1:50]


rating_list <- scrape (session) %>%
  html_nodes('span.ipc-rating-star.ipc-rating-star--base.ipc-rating-star--imdb.ratingGroup--imdb-rating')%>%
  html_text
rating_list[1:50]

vote_list <- scrape (session) %>%
  html_nodes('span.ipc-rating-star--voteCount')%>%
  html_text
vote_list[1:50]

split_df <- strsplit(as.character(rating_list$vote_list),".",fixed = TRUE)
split_df <- data.frame(do.call(rbind,split_df))

episode_list <- scrape (session) %>%
  html_nodes('span.sc-479faa3c-8.bNrEFi.cli-title-metadata-item')%>%
  html_text
episode_list

year_list <- scrape (session) %>%
  html_nodes('span.sc-479faa3c-8.bNrEFi.cli-title-metadata-item')%>%
  html_text
year_list





