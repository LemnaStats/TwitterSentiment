#Tweet Stripper
require(tidyverse)
require(tidytext)
require(httr)


bearer_token <- "sike"



searchTwitterTextAndTimestamp <- function(search_terms,n_results){
  headers = c(
    `Authorization` = sprintf('Bearer %s', bearer_token)
  )
  
  params = list(
    `query` = search_terms,
    `max_results` = n_results,
    `tweet.fields` = 'created_at'
  )
  
  
  response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', 
                        httr::add_headers(.headers=headers), query = params)
  
  
  recent_search_body <-
    content(
      response,
      as = 'parsed',
      type = 'application/json',
      simplifyDataFrame = TRUE
    )
  
  return(recent_search_body$data)
}

text_amalgamator <- function(data,bad){
scorp_words <- data$text
afinn <- get_sentiments("afinn")
scorp <- str_replace_all(scorp_words, "[^[:alnum:]]", " ")
scorp <- paste(scorp, collapse = " ")
scorp <- str_split(scorp," ") %>% unlist() %>% as.data.frame()
colnames(scorp) <- c("word")
scorp$word <- tolower(scorp$word)
scorp <- scorp %>% filter(., !(word %in% bad))
scorp_count <- scorp %>% count(word)
scorp_count <- merge.data.frame(scorp_count,afinn)
scorp_count <- mutate(scorp_count, score = n * value)
scorp_count <- tibble(scorp_count)
return(scorp_count) }

twitter_sentiment <- function(query,filtered){
  tweets <- searchTwitterTextAndTimestamp(query,100)
  sentis <- text_amalgamator(tweets,filtered)
  return(sentis)
}

zodiac_signs <- tibble(c("aries","taurus","gemini","cancer","leo","virgo","libra","scorpio","sagittarius","capricorn","aquarius","pisces"))
colnames(zodiac_signs) <- "signs"

zodiac_men_list <- mutate(zodiac_signs, sign_men = str_glue("{signs} men"))
zodiac_men_list <- zodiac_men_list$sign_men 

count_tester <- function(input, yes_no){
  testvar <- filter(input, word == yes_no)$n 
  if (length(testvar) == 0) {
    testvar <- 0
  }
  return(testvar)
}

conduct_sentiment <- function(query,filtered_words){
  gem_tibble <- tibble(query)
  colnames(gem_tibble) <- "query"
  gem_sent <- twitter_sentiment(query,filtered_words)
  gem_tibble$word_count <- sum(gem_sent$n)
  gem_tibble$yes_count <- count_tester(gem_sent, "yes")
  gem_tibble$no_count <- count_tester(gem_sent,"no")
  gem_tibble$score <- sum(gem_sent$score)
  gem_sent <- arrange(gem_sent,desc(n))
  gem_tibble$common_words <- filter(gem_sent, n > 1)$word %>% paste(.,collapse = ", ")
  return(gem_tibble)
}

analyse_sentiment <- function(query,sent_table){
  gem_tibble <- tibble(query)
  colnames(gem_tibble) <- "query"
  gem_tibble$word_count <- sum(sent_table$n)
  gem_tibble$yes_count <- count_tester(sent_table, "yes")
  gem_tibble$no_count <- count_tester(sent_table,"no")
  gem_tibble$score <- sum(sent_table$score)
  sent_table <- arrange(sent_table,desc(n))
  gem_tibble$common_words <- filter(sent_table, n > 1)$word %>% paste(.,collapse = ", ")
  return(gem_tibble)
}

many_sentiments <- function(query_list,filtered_words){
  results_tibble <- tibble(query=character(),word_count=integer(),
                           yes_count=integer(),no_count=integer(),
                           score=integer(),common_words=character())
  tweet_table_list <- list(results_tibble)
  names(tweet_table_list) <- c(query_list[1])
  sentiment_table_list <- list(results_tibble)
  names(sentiment_table_list) <- c(query_list[1])
  for (item in query_list){
    tweets <- searchTwitterTextAndTimestamp(item,100)
    sentis <- text_amalgamator(tweets,filtered_words)
    new_row <- analyse_sentiment(item,sentis)
    results_tibble <- rbind(results_tibble,new_row)
    tweet_table_list[[item]] <- tweets
    sentiment_table_list[[item]] <- sentis
  }
  master_list <- list(results_tibble,tweet_table_list,sentiment_table_list)
  names(master_list) <- c("sentiments","tweets","word lists")
  return(master_list)
}

taylor_albums <- tibble(c("self-titled","fearless","speak now","red","1989","reputation","lover","folklore","evermore","long pond"))
colnames(taylor_albums) <- "albums"
taylor_filters <- c("swift","fearless")

taylor_results <- many_sentiments(taylor_albums,taylor_filters)
