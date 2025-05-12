suppressPackageStartupMessages({
library(sentimentr)
library(tidytext)
library(lubridate)
library(dplyr)
library(tidyr)
library(argparse)
library(ggpubr)
})

load_data<-function(filename) {
    # Read data
    data <- read.csv(filename) %>%
      filter(language == "en") # Only take sources in English
    data$content <- str_remove_all(data$content, "<[^>]+>") # Remove HTML from content
    
    # Convert created_at to date time
    data$created_at <- data$created_at %>% str_remove(".") %>%
      as.POSIXct(format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    data$id <- as.character(data$id) # Ensure "id" is character class
    return(data)
}

word_analysis<-function(toot_data, emotion) {
    # Load lexicon
    nrc_lex <- get_sentiments("nrc")
    
    # Tokenize toots and join with lexicon
    word_data <- toot_data %>%
      select(id, created_at, content) %>%
      unnest_tokens(word, content) %>%
      inner_join(nrc_lex, by = "word") %>%
      filter(sentiment == "emotion") %>%
      group_by(word) %>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>%
      top_n(10, n)
    
    # Print dataframe
    print(word_data)
      
    return(word_data)
}

sentiment_analysis<-function(toot_data) {

    return()

}

main <- function(args) {

}


if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sentiment Analysis",
                    description="Analyse toots for word and sentence sentiments"
                    )
  parser$add_argument("filename",
                    help="the file to read the toots from")
  parser$add_argument("--emotion",
                      default="anger",
                      help="which emotion to search for")
  parser$add_argument('-v', '--verbose',
                    action='store_true',
                    help="Print progress")
  parser$add_argument('-p', '--plot',
                    help="Plot something. Give the filename")
  
  args = parser$parse_args()  
  main(args)
}

# Copyright © 2025 Mason Rodrigue
# All rights reserved.