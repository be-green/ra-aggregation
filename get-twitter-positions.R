
suppressWarnings({
  
library(rtweet)
library(googlesheets)
library(data.table)
library(stringr)
  
})

suppressMessages({
suppressWarnings({
    
  client <- readLines(file("client.scrt"))
  
  secret <- readLines(file("secret.scrt"))
  
  acc_token <- readLines(file("access-token.scrt"))
  acc_secret <- readLines(file("access-secret.scrt"))
  
})
  
  token <- 
    rtweet::create_token(app = "ra-aggregate", 
                       consumer_key = client,
                       consumer_secret = secret,
                       access_token = acc_token,
                       access_secret = acc_secret)
  
  # econ_ra <- rtweet::lookup_users("econ_ra", token = token)
  
  uid <- "1042199173527941121"
  
  timeline <- get_timeline("1042199173527941121", n = 100)
  
  get_urls <- function(url_list) {
    lapply(url_list, function(x) paste(x, collapse = ", ")) %>% 
      unlist
}

timeline <- as.data.table(timeline) %>% 
  .[,urls_expanded_url := get_urls(urls_expanded_url)] %>% 
  .[,urls_t.co := get_urls(urls_t.co)] %>% 
  .[,urls_url := get_urls(urls_url)]

positions <- timeline[!is.na(retweet_text)]
positions[urls_expanded_url == "NA", urls_expanded_url := NA]

url_regex <- "(https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|www\\.[a-zA-Z0-9][a-zA-Z0-9-]+[a-zA-Z0-9]\\.[^\\s]{2,}|https?:\\/\\/(?:www\\.|(?!www))[a-zA-Z0-9]+\\.[^\\s]{2,}|www\\.[a-zA-Z0-9]+\\.[^\\s]{2,})"

coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}

posted_positions <- 
  positions[,.(Date = created_at, RawText = text, 
               JobLink = coalesce(urls_expanded_url, str_extract(text, url_regex)),
               PosterName = retweet_name,
               MentionedNames = lapply(positions$mentions_screen_name, function(x) paste(x, collapse = ", ")) %>% 
                 unlist,
               UserDescription = retweet_description
             )]

setorder(posted_positions, -Date)

posted_positions <- posted_positions[,Date := as.character(Date)]

ss <- googlesheets::gs_title("ra-positions")

known_positions <- gs_read(ss)
known_positions <- as.data.table(known_positions)

new_tweets <- setdiff(posted_positions$RawText, known_positions$RawText)

new_positions <- posted_positions[RawText %in% new_tweets]

setorder(new_positions, -Date)

all_positions <- rbind(new_positions, known_positions)
})


if(nrow(new_positions) > 0){
  message(paste0("Found ", nrow(new_positions), " new positions."))
  googlesheets::gs_edit_cells(ss, ws = "twitter", input = all_positions)
} else {
  message("No new positions today.")
}

