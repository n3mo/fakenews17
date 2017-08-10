e####################################################################
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
library(slam)
library(cluster)

####################################################################
## Snope Data Comparison
####################################################################

## Snopes data -----------------------------------------------------
## fake <- Corpus(DirSource("./fake_articles"))
fake <- Corpus(DirSource("./all_fake_articles"))
real <- Corpus(DirSource("./all_real_articles"))
satire <- Corpus(DirSource("./satire_articles"))
## combined <- Corpus(DirSource("./combined"))
combined <- Corpus(DirSource("./fake_and_real_articles"))

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

real <- tm_map(real, removeNumbers)
real <- tm_map(real, removePunctuation)
real <- tm_map(real , stripWhitespace)
real <- tm_map(real, tolower)
real <- tm_map(real, removeWords, stopwords("english")) 
## Document term matrix created. We weight by term frequence (the
## default) as this is needed for topic modeling, which expects
## integer counts of words/tokens
real_dtm <- DocumentTermMatrix(real)
## Although tf is needed for topic modeling, tf-idf is often preferred
## for other analyses. We create a dtm weighted this way as well
real_dtm_tfidf <-DocumentTermMatrix(real, 
                                    control = 
                                        list(weighting = 
                                                 function(x) weightTfIdf(x, normalize = TRUE)))
## real_dtm <- removeSparseTerms(real_dtm, 0.75)


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

combined <- tm_map(combined, removeNumbers)
combined <- tm_map(combined, removePunctuation)
combined <- tm_map(combined , stripWhitespace)
combined <- tm_map(combined, tolower)
combined <- tm_map(combined, removeWords, stopwords("english")) 
## Document term matrix created. We weight by term frequence (the
## default) as this is needed for topic modeling, which expects
## integer counts of words/tokens
combined_dtm <- DocumentTermMatrix(combined)
## Although tf is needed for topic modeling, tf-idf is often preferred
## for other analyses. We create a TDM weighted this way as well
combined_tdm_tfidf <-TermDocumentMatrix(combined, 
                                        control = 
                                            list(weighting = 
                                                     function(x) weightTfIdf(x, normalize = TRUE)))
## combined_dtm_tfidf <-DocumentTermMatrix(combined, 
##                                         control = 
##                                             list(weighting = 
##                                                      function(x) weightTfIdf(x, normalize = TRUE)))


## Term frequency analysis for fake news
freqf <- colSums(as.matrix(fake_dtm_tfidf))
wff = data.frame(term=names(freqf),tfidf=freqf)
wff$term = factor(wff$term, levels = wff$term[order(-wff$tfidf)])
f <- ggplot(subset(wff, wff$tfidf > 15), aes(term, tfidf))
f <- f + geom_bar(stat="identity")
f <- f + theme(axis.text.x=element_text(angle=45, hjust=1))
f

## Term frequency analysis for real news
freqr <- colSums(as.matrix(real_dtm_tfidf))
wfr = data.frame(term=names(freqr),tfidf=freqr)
wfr$term = factor(wfr$term, levels = wfr$term[order(-wfr$tfidf)])
r <- ggplot(subset(wfr, wfr$tfidf > 19.03138), aes(term, tfidf))
r <- r + geom_bar(stat="identity")
r <- r + theme(axis.text.x=element_text(angle=45, hjust=1))
r

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
f <- ggplot(head(wff[ordf,], 10), aes(term, tfidf))
f <- f + geom_bar(stat="identity")
f <- f + theme(axis.text.x=element_text(angle=45, hjust=1))
;;f <- f + ylim(0, 2.5)
f

ords = order(freqs, decreasing = T)
s <- ggplot(head(wfs[ords,], 10), aes(term, occurrences))
s <- s + geom_bar(stat="identity")
s <- s + theme(axis.text.x=element_text(angle=45, hjust=1))
s <- s + ylim(0, 2.5)
s

## Let's inspect what terms correlate (that is, co-occur) with our top
## n most influential terms from the fake and satire corpora. Keep in
## mind that the correlated words that show up do so based on their
## co-occurrence, and do not show up because they are (necessarily)
## frequent in our data set.
top_terms_fake = names(head(freqf[ordf]))
top_terms_satire = names(head(freqs[ords]))
## Our correlation cutoff (between 0 and 1 in this case)
cor_cutoff = 0.50
## Correlations to the top-10 fake news terms
findAssocs(fake_dtm_tfidf, top_terms_fake, cor_cutoff)
## Correlations to the top-10 satire news terms
findAssocs(satire_dtm_tfidf, top_terms_satire, cor_cutoff)

## Document similarity (cosine)
cosine_dist_mat <-
    crossprod_simple_triplet_matrix(combined_tdm_tfidf) / 
    (sqrt(col_sums(combined_tdm_tfidf^2) %*% t(col_sums(combined_tdm_tfidf^2))))
## k-means clustering for n clusters
num_clusters = 2
## kfit <- kmeans(cosine_dist_mat, num_clusters, iter.max = 100, nstart=100)
kfit <- kmeans(cosine_dist_mat, num_clusters, nstart=100)
## Plot
clusplot(cosine_dist_mat, kfit$cluster, color=T, shade=T, labels=2,
         lines=0)

## ------------------------------------------------------------
## Fake vs. real news correlation comparison
## ------------------------------------------------------------

## Find terms common to both datasets
tmp = intersect(names(freqf), names(freqr))
idx = names(freqf) %in% tmp
df.f = data.frame(word = names(freqf[idx]), 
                  tfidf = as.vector(freqf[idx]),
                  source = rep("fake", length(freqf[idx])),
                  stringsAsFactors = FALSE)
ord = order(df.f$word)
df.f = df.f[ord, ]

idx = names(freqr) %in% tmp
df.r = data.frame(word = names(freqr[idx]), 
                  tfidf = as.vector(freqr[idx]),
                  source = rep("real", length(freqr[idx])),
                  stringsAsFactors = FALSE)
ord = order(df.r$word)
df.r = df.r[ord, ]

## Final combined dataset
d = data.frame(word = df.f$word, fake = df.f$tfidf, real = df.r$tfidf,
               stringsAsFactors = FALSE)

ggplot(d, aes(fake, real, color = abs(fake - real))) +
    geom_abline(color = "gray40", lty = 2) +    
    geom_point() +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5, size = 5) +
    scale_x_continuous() +
    scale_y_continuous() + 
    scale_color_gradient(limits = c(0, 3), low = "darkslategray4", high = "gray75") +
    theme(legend.position="none") + 
    labs(y = "Real News (tf-idf)", x = "Fake News (tf-idf)")



## For labeling the most influential terms
## tmp = abs(d$fake - d$real)
## idx = d$fake > 15 | d$real > 19.03138
## idx = tmp > 3
## idx = d$fake > 3 | d$real > 3
## plot(d$fake[!idx], d$real[!idx],
##      xlab = "Fake News Terms",
##      ylab = "Real News Terms",
##      col = "gray75",
##      ylim = c(0, 30),
##      xlim = c(0, 25))
## par(new = TRUE)
## plot(d$fake[idx], d$real[idx],
##      xlab = "Fake News Terms",
##      ylab = "Real News Terms",
##      col = "darkslategray4",
##      ylim = c(0, 30),
##      xlim = c(0, 25))
## text(d$fake[idx], d$real[idx], labels = d$word[idx], pos = 1)

####################################################################
## Topic Modeling of SNOPES data
####################################################################


## Fit an LDA model to both fake news and satire (separately)
fake_lda = LDA(fake_dtm, k = 4, control = list(seed = 1234))
real_lda = LDA(real_dtm, k = 4, control = list(seed = 1234))
satire_lda = LDA(satire_dtm, k = 4, control = list(seed = 1234))

## Extract per-topic-per-word probabilities (beta)
fake_topics <- tidy(fake_lda, matrix = "beta")
real_topics <- tidy(real_lda, matrix = "beta")
satire_topics <- tidy(satire_lda, matrix = "beta")

## Plot the 10 most common words within each topic
fake_top_terms <- fake_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
real_top_terms <- real_topics %>%
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
real_top_terms %>%
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

####################################################################
## Emergent Data
####################################################################

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
