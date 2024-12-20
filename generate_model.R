
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

ngram3 <- fread('JHU-Swiftkey-Capstone/3gramOver1.csv')
ngram3[, .(count = .N), by = frequency > 1][]


ngram3[1:3]


nngram3 <- ngram3[, query := strsplit(feature, " [^ ]+$")]
ngram3 <- nngram3[, predict := sub('.* (.*)$','\\1', feature)]

ngram3[,.(query, predict, frequency)] |>
  fwrite('JHU-SwiftKey-Capstone/3gramOver1.csv')

dt <- data.table(ngram3)

dt2 <- ngram3[, query := strsplit(feature, " [^ ]+$")]

dt2[1:5,]

dt3 <- dt2[, predict := sub('.* (.*)$','\\1', feature)]

dt3[1:5,]

dt3[query == 'mean the',]

dt3[query == 'me the',][1:30,]

dt3[query == 'but the' & predict %in% c('players','referees','defense','crowd')]

dt3[query == 'at the' & predict %in% c('mall','movies','grocery','beach')]

dt3[query == 'on my' & predict %in% c('way','phone','horse','motorcycle')]

dt3[query == 'quite some' & predict %in% c('years','thing','time','weeks')]

dt3[query == 'his little' & predict %in% c('ears','toes','fingers','eyes')]

dt3[query == 'during the' & predict %in% c('sad','hard','bad','worse')]

dt3[query == 'must be' & predict %in% c('insane','asleep','insensitive','callous')]

dt3[query == "and i'd" & predict %in% c('sleep','give','die','eat')][1:30]

dt3[query == 'me about',]

dt3[query == 'monkeys this']

dt3[query == 'reduce your']

dt3[query == 'take a' & predict %in% c('picture','look','walk','minute')]

dt3[query == 'settle the']

dt3[query == 'in each' & predict %in% c('toe','hand','arm','finger')]

dt3[query == 'to the' & predict %in% c('side','top','center','middle')]

dt3[query == 'from playing']

dt3[query == "adam sandler's"]

h <- 'query'

h |> stringr::str_extract('[^ ]* [^ ]*$')

ngram3[query == 'that is', predict][1]


