#Tweet Stripper
require(tidyverse)
require(tidytext)
require(httr)

APIkey <- "N6t47mWc75m3omWuuOK2efWR0"
APIsecret <- "e6fiNAFrpuuGakbtInoE8v9tdzDilE5zrXgl1tw0ukZxj1gHRx"
bearer_token <- "AAAAAAAAAAAAAAAAAAAAAMS6XwEAAAAA1D8a4lJWJZP2yVJLEn7r%2Fzsk5m0%3D3n1w2DqpF2HWN9HHHlEGrq56gFx2NHfQsOFlQ2yRXjAyhbBgIp"
AccessToken <- "128872936-RJp6XvOZNcFQNWAH3TqFfeVEJ0UpUUz6fZOsnKX7"
AccessSecret <- "LXWfbLPC1OQzY2meoMQERwPioMDIzWJmOhaTiAOxJFLfJ"
ClientID <- "YWdoVWRlYlpWcTlNNFBfRHFOR086MTpjaQ"
ClientSecret <- "R481m4n2uw_LIwwoJoF04eTs_FElx-lo4H2eRfoD2rx975Fe-5"

set_bearer_token <- function(x){bearer_token = x}

searchTwitterTextAndTimestamp <- function(search_terms,n_results){
  headers = c(
    `Authorization` = sprintf('Bearer %s', bearer_token)
  )
  
  params = list(
    `query` = search_terms,
    `max_results` = n_results,
    `tweet.fields` = 'created_at'
  )
  
  
  response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', httr::add_headers(.headers=headers), query = params)
  
  
  recent_search_body <-
    content(
      response,
      as = 'parsed',
      type = 'application/json',
      simplifyDataFrame = TRUE
    )
  
  return(recent_search_body$data)
}

afinn <- get_sentiments("afinn")

text_amalgamator <- function(data){
scorp_words <- data$text
scorp <- str_replace_all(scorp_words, "[^[:alnum:]]", " ")
scorp <- paste(scorp, collapse = " ")
scorp <- str_split(scorp," ") %>% unlist() %>% as.data.frame()
colnames(scorp) <- c("word")
scorp$word <- tolower(scorp$word)
bad <- c("RT", " ", "t", "co", "", "https","and","are","the","with","be","like","love","cancer")
scorp <- scorp %>% filter(., !(word %in% bad))
scorp_count <- scorp %>% count(word)
scorp_count

scorp_count <- merge.data.frame(scorp_count,afinn)
scorp_count <- mutate(scorp_count, score = n * value)
return(scorp_count) }

twitter_sentiment <- function(query){
  tweets <- searchTwitterTextAndTimestamp(query,100)
  sentis <- text_amalgamator(tweets)
  return(sentis)
}

aries <- twitter_sentiment("aries men")
View(aries)