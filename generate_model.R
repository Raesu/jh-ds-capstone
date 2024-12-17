
library(quanteda)
library(quanteda.textstats)
library(readtext)
library(readr)
library(data.table)


# Read text files and generate corpora
blogs <- readtext('data/en_US.blogs.txt')
blogCorpus <- blogs |> corpus()
docvars(blogCorpus, 'Source') <- 'Blogs'

news <- readtext('data/en_US.news.txt')
newsCorpus <- news |> corpus()
docvars(newsCorpus, 'Source') <- 'News'

twitter <- readtext('data/en_US.twitter.txt')
twitterCorpus <- twitter |> corpus()
docvars(twitterCorpus, 'Source') <- 'Twitter'

corpora <- blogCorpus + newsCorpus + twitterCorpus

# Free up memory
rm(blogs, news, twitter)
rm(blogCorpus, newsCorpus, twitterCorpus)

# Tokenize
tokens <- corpora |>
  tokens(remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE,
    remove_separators = TRUE, remove_url = TRUE)

# Define function for n-grams
makeNgrams <- function(tokens, n, fileName) {
  
  ngrams <- tokens_ngrams(tokens, n = n, concatenator = ' ')
  dfm <- dfm(ngrams)
  textFreq <- textstat_frequency(dfm)
  
  # Save RDS
  textFreq |> write_csv(paste0('grams/', fileName, '.csv'))
}

for (i in 1:7) {
  print(stringr::str_glue('Generating {i}-grams...'))
  tokens |> makeNgrams(i, paste0(i, 'gram'))
}

generatePred <- function(inputFile, thresh = 1L) {
  
  ## This function makes the prediction look up table
  ## inputFile: the ngram csv file generated from quanteda
  ## thresh: threshold to remove low frequency words (default is 1)
  nGram <- fread(inputFile, select = c('feature', 'frequency'))
  nGram <- nGram[nGram$frequency > thresh]
  
  nGram <- nGram[, query := strsplit(feature, " [^ ]+$")][]
  nGram <- nGram[, predict := sub('.* (.*)$','\\1', feature)][]
  
  fwrite(nGram, paste0(sub('.csv', '', inputFile), 'Pred.csv'))
}

ngram3 <- fread('grams/3gram.csv')
str(ngram3)

dt <- data.table(ngram3)

x <- ngram3[1:5, query := strsplit(feature, " [^ ]+$")][1:5,]

k <- x[, predict := sub('.* (.*)$','\\1', feature)]

library(data.table)

for (i in 1:5) {
  name <- stringr::str_c('grams/', i, 'gram.csv')
  print(stringr::str_c('Reading ', i, 'gram.csv...'))
  ngram <- read_csv(name)
  print(stringr::str_c('Writing ', i, 'gram.csv...'))
  ngram |> write_rds(stringr::str_c('grams/', i, 'gram.rds'))
}




