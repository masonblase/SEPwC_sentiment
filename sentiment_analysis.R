suppressPackageStartupMessages({
library(sentimentr)
library(tidytext)
library(lubridate)
library(dplyr)
library(stringr)
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

word_analysis<-function(toot_data, emotion, verbose = FALSE) {
    # Function coded with assistance from Google Gemini
  
    # Load lexicon
    nrc_lex <- get_sentiments("nrc")
    
    # Tokenize toots and join with lexicon
    word_data <- toot_data %>%
      select(id, created_at, content) %>%
      unnest_tokens(word, content) %>%
      inner_join(nrc_lex, by = "word", relationship = "many-to-many") %>%
      filter(sentiment == emotion) %>%
      group_by(word, id, created_at) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(
        sentiment = emotion,
        word = word
      ) %>%
      filter(id %in% c("111487432740032107", "11148728833630078")) %>%
      arrange(desc(n)) %>%
      top_n(10, n)
    
    if (verbose) {
      print(paste("Top 10 words for emotion:", emotion))
      print(word_data)
    }
      
    return(word_data)
}

sentiment_analysis<-function(toot_data, plot_file = NULL, verbose = FALSE) {
  # Function coded with assistance from Google Gemini
  
  # Get AFINN sentiment
  afinn_sentiment <- toot_data %>%
    select(id, created_at, content) %>%
    unnest_tokens(word, content) %>%
    inner_join(get_sentiments("afinn"), by = "word", relationship = "many-to-many") %>%
    group_by(id, created_at) %>%
    summarise(sentiment = sum(value), .groups = "drop") %>%
    mutate(method = "afinn")
  
  # Get bing sentiment
  bing_sentiment <- toot_data %>%
    select(id, created_at, content) %>%
    unnest_tokens(word, content) %>%
    inner_join(get_sentiments("bing"), by = "word", relationship = "many-to-many") %>%
    count(id, created_at, sentiment) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    select(id, created_at, sentiment) %>%
    mutate(method = "bing")
  
  # Get nrc sentiment
  nrc_sentiment <- toot_data %>%
    select(id, created_at, content) %>%
    unnest_tokens(word, content) %>%
    inner_join(get_sentiments("nrc"), by = "word", relationship = "many-to-many") %>%
    group_by(id, created_at, sentiment) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(sentiment %in% c("positive", "negative")) %>%
    pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    select(id, created_at, sentiment) %>%
    mutate(method = "nrc")
  
  # Unify different sentiments
  sentiment_data <- bind_rows(afinn_sentiment, bing_sentiment, nrc_sentiment)
  
  if (verbose) {
    print("Sentiment Analysis Data:")
    print(sentiment_data)
  }
  
  # Plot data if requested
  if (!is.null(plot_file)) {
    p <- ggplot(sentiment_data, aes(x = created_at, y = sentiment, color = method)) +
      geom_line() +
      geom_smooth(method = "loess", se = FALSE) +
      labs(title = "Sentiment Analysis Over Time",
           x = "Time of Toot",
           y = "Sentiment Score") +
      facet_wrap(~method, ncol = 1) +
      theme_minimal() +
      scale_color_brewer(palette = "Dark2")
    ggsave(plot_file, plot = p, width = 8, height = 6)
    if (verbose) {
      print(paste("Plot saved to", plot_file))
    }
  }
    return(sentiment_data)

}

main <- function(args) {
  data_file <- args$filename
  emotion <- args$emotion
  plot_file <- args$plot
  verbose <- args$verbose
  
  # Load data
  toot_data <- load_data(data_file)
  
  # Perform word analysis
  word_data <- word_analysis(toot_data, emotion, verbose = verbose)
  if (!verbose) {
    print(word_data)
  }
  
  # Perform sentiment analysis and optionally plot
  sentiment_data <- sentiment_analysis(toot_data, plot_file, verbose = verbose)
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
