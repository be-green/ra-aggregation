# get nber positions

suppressWarnings({
  
library(xml2)
library(rvest)
library(stringr)
library(magrittr)
library(data.table)
library(googlesheets)

})
  
url <- "https://www.nber.org/jobs/nonnberjobs.html"

job_page <- read_html(url)

jobs <- rvest::html_nodes(job_page, "p")

get_job <- function(node) {
  list(job_text = html_text(node),
       job_link = html_nodes(node, "a") %>% 
         html_attr("href"))
}

make_df_titles <- function(titles) {
  tolower(str_extract(titles, "[A-Za-z]*(?=(\\:$))")) %>% 
    quanteda::char_wordstem(.)
}

parse_job_text <- function(job_text) {
  
  alltext <- str_split(job_text, pattern = "\n") %>% 
    unlist
  
  
  alltext <- subset(alltext, str_detect(alltext, "[A-Za-z]")) %>% 
    subset(., !tolower(.) %like% "link|*.pdf|*.html")
  
  
  categories <- str_extract_all(alltext[2:length(alltext)],"^(.*?)\\:") %>% 
    unlist %>% 
    make_df_titles
  
  alltext[2:length(alltext)] <- 
    str_replace_all(alltext[2:length(alltext)], 
                    "^(.*?)\\:","") %>% 
    str_trim
  list(alltext, categories)
}

parse_job_posting <- function(job){
  
  parsed_text <- parse_job_text(job$job_text)
  alltext <- parsed_text[[1]]
  categories <- parsed_text[[2]]
  link <- job$job_link
  
  for(i in 1:length(link)) {
    if(!link[i] %like% "www|http|\\.com"){
      link[i] <- paste0("https://www.nber.org/jobs/", link[i])
    } 
  }
  
  if(length(link) > 1) {
    link <- paste0(link, collapse = ", ")
  }
  
  tbl <- as.data.table(t(alltext))
  
  # print(tbl$V1)
  
  setnames(tbl, colnames(tbl), c("title", categories))
  
  
  tbl[,link := link]
  
  tbl[]  
}

nber_jobs <- lapply(jobs, get_job) %>% 
  Filter(function(x) !x$job_link %like% "/jobs/employment_opp.html",
         .) %>% 
  head(50) %>% 
  lapply(parse_job_posting) %>% 
  rbindlist(fill = T, use.names = T)

coalesce <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}

colnames(nber_jobs) <- make.unique(colnames(nber_jobs))

posted_jobs <- 
  nber_jobs[,.(Job = title, Institution = institut,
             Researchers = research, Field = coalesce(field, V1, research.1),
             Link = link)] %>% 
  .[,lapply(.SD, function(x) if(is.character(x)) str_trim(x) else x)]

suppressMessages({
  
ss <- googlesheets::gs_title("ra-positions")

known_nber_positions <- gs_read(ss, ws = "nber") %>% 
  as.data.table

})
if (nrow(known_nber_positions) > 0 ) {
  
  new_nber_positions <- fsetdiff(posted_jobs, known_nber_positions)
  
  all_nber_positions <- rbind(new_nber_positions, known_nber_positions)
  
  if(nrow(new_nber_positions) > 0){
    message(paste0("Found ", nrow(new_positions), " new positions."))
    
    supressMessages({
      
      googlesheets::gs_edit_cells(ss, ws = "nber", input = all_nber_ositions)
      
    })
  } else {
    message("No new positions today.")
  }
} else {
  googlesheets::gs_edit_cells(ss, ws = "nber", input = posted_jobs)
}




