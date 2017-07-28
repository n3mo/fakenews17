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

####################################################################
## Snope Data Comparison
####################################################################

## Snopes data -----------------------------------------------------
fake <- Corpus(DirSource("./fake_articles"))
satire <- Corpus(DirSource("./satire_articles"))

fake <- tm_map(fake, removeNumbers)
fake <- tm_map(fake, removePunctuation)
fake <- tm_map(fake , stripWhitespace)
fake <- tm_map(fake, tolower)
fake <- tm_map(fake, removeWords, stopwords("english")) 
## Document term matrix created. We weight by term frequence (the
## default) as this is needed for topic modeling, which expects
## integer counts of words/tokens
fake_dtm <- DocumentTermMatrix(fake)
## Although tf is needed for topic modeling, tf-idf is often preferred
## for other analyses. We create a dtm weighted this way as well
fake_dtm_tfidf <-DocumentTermMatrix(fake, 
                                    control = 
                                        list(weighting = 
                                                 function(x) weightTfIdf(x, normalize = TRUE)))
## fake_dtm <- removeSparseTerms(fake_dtm, 0.75)

satire <- tm_map(satire, removeNumbers)
satire <- tm_map(satire, removePunctuation)
satire <- tm_map(satire , stripWhitespace)
satire <- tm_map(satire, tolower)
satire <- tm_map(satire, removeWords, stopwords("english")) 
## Document term matrix created. We weight by term frequence (the
## default) as this is needed for topic modeling, which expects
## integer counts of words/tokens
satire_dtm <- DocumentTermMatrix(satire)
## Although tf is needed for topic modeling, tf-idf is often preferred
## for other analyses. We create a dtm weighted this way as well
satire_dtm_tfidf <-DocumentTermMatrix(satire,
                                      control = 
                                          list(weighting = 
                                                   function(x) weightTfIdf(x, normalize = TRUE))) 
## satire_dtm <- removeSparseTerms(satire_dtm, 0.75)

## Term frequency analysis for fake news
freqf <- colSums(as.matrix(fake_dtm_tfidf))
wff = data.frame(term=names(freqf),occurrences=freqf)
wff$term = factor(wff$term, levels = wff$term[order(-wff$occurrences)])
f <- ggplot(subset(wff, wff$occurrences > 0.8), aes(term, occurrences))
f <- f + geom_bar(stat="identity")
f <- f + theme(axis.text.x=element_text(angle=45, hjust=1))
f

## Term frequency analysis for satire
freqs <- colSums(as.matrix(satire_dtm_tfidf))
wfs = data.frame(term=names(freqs),occurrences=freqs)
wfs$term = factor(wfs$term, levels = wfs$term[order(-wfs$occurrences)])
s <- ggplot(subset(wfs, freqs > 0.8), aes(term, occurrences))
s <- s + geom_bar(stat="identity")
s <- s + theme(axis.text.x=element_text(angle=45, hjust=1))
s

## Alternatively, we can plot the top-n
ordf = order(freqf, decreasing = T)
f <- ggplot(head(wff[ordf,], 10), aes(term, occurrences))
f <- f + geom_bar(stat="identity")
f <- f + theme(axis.text.x=element_text(angle=45, hjust=1))
f <- f + ylim(0, 2.5)
f

ords = order(freqs, decreasing = T)
s <- ggplot(head(wfs[ords,], 10), aes(term, occurrences))
s <- s + geom_bar(stat="identity")
s <- s + theme(axis.text.x=element_text(angle=45, hjust=1))
s <- s + ylim(0, 2.5)
s

## Fit an LDA model to both fake news and satire (separately)
fake_lda = LDA(fake_dtm, k = 4, control = list(seed = 1234))
satire_lda = LDA(satire_dtm, k = 4, control = list(seed = 1234))

## Extract per-topic-per-word probabilities (beta)
fake_topics <- tidy(fake_lda, matrix = "beta")
satire_topics <- tidy(satire_lda, matrix = "beta")

## Plot the 10 most common words within each topic
fake_top_terms <- fake_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
satire_top_terms <- satire_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

fake_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()
satire_top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip()

## Alternatively, we can visualize the words that had the greatest
## difference in beta (probabilities) between topic 1 and topic 2
## beta_spread <- fake_topics %>%
##     mutate(topic = paste0("topic", topic)) %>%
##     spread(topic, beta) %>%
##     filter(topic1 > .001 | topic2 > .001) %>%
##     mutate(log_ratio = log2(topic2 / topic1))

## ## beta_spread # Inspect manually

## topic1 = beta_spread %>% arrange(desc(log_ratio)) %>% head(10)
## topic2 = beta_spread %>% arrange(log_ratio) %>% head(10)

## ## Plot everything
## par(mai=c(1,2,1,1))
## barplot(c(topic1$log_ratio, topic2$log_ratio),
##         names.arg = c(topic1$term, topic2$term),
##         horiz = T, las = 1)
## grid()
## box()
