####################################################################
##
## analysis.R
##
## (c) Nicholas Van Horn 
##
## 1.0.0  2017-06-22 NVH: Initial version

####################################################################
## Topic Modeling
####################################################################

## Assumption is that you've manually set the current working
## directory to where the data files are located

library(tidytext)
library(tm)
library(topicmodels)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

## ## Emergent data --------------------------------------------------

## ## Emergent data (1 example file)
## d = read.csv("url-versions-2015-06-14-clean-train-fold-1.csv", stringsAsFactors = F)
## ## Convert to simple corpus
## myCorpus = Corpus(VectorSource(d$articleHeadline))

## Fake News Challenge data ---------------------------------------- 
## Fake news challenge data
d = read.csv("train_bodies.csv", stringsAsFactors = F)
## Convert to simple corpus
myCorpus = Corpus(VectorSource(removePunctuation(d$articleBody)))

## Common code to all data sets follows ---------------------------

## Downcase everything
myCorpus = tm_map(myCorpus, tolower)
## Remove stopwords
myCorpus = tm_map(myCorpus, removeWords, stopwords('english'))

## Convert to docucment term matrix
dtm = DocumentTermMatrix(myCorpus)

## Fit an LDA model
fn_lda = LDA(dtm, k = 2, control = list(seed = 1234))

## Extract per-topic-per-word probabilities (beta)
fn_topics <- tidy(fn_lda, matrix = "beta")

## Plot the 10 most common words within each topic
fn_top_terms <- fn_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

fn_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

## Alternatively, we can visualize the words that had the greatest
## difference in beta (probabilities) between topic 1 and topic 2
beta_spread <- fn_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))

## beta_spread # Inspect manually

topic1 = beta_spread %>% arrange(desc(log_ratio)) %>% head(10)
topic2 = beta_spread %>% arrange(log_ratio) %>% head(10)

## Plot everything
par(mai=c(1,2,1,1))
barplot(c(topic1$log_ratio, topic2$log_ratio),
        names.arg = c(topic1$term, topic2$term),
        horiz = T, las = 1)
grid()
box()

## Document-topic probabilities. With LDA, each document is also
## modeled as a mixture of topics. We can inspect the
## per-document-per-topic probabilities (known as gamma)
fn_documents <- tidy(fn_lda, matrix = "gamma")
fn_documents

## It seems that for the Fake News Challenge data, document 3 draws
## almost none of its words from topic 1. Let's inspect the most common
## words from that document
tidy(dtm) %>%
    filter(document == 3) %>%
    arrange(desc(count))
