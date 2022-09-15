# Packages 

install.packages("quanteda")
install.packages("readtext")
install.packages("spacyr")
install.packages("newsmap")
install.packages("seededlda")
install.packages("tm")

library(quanteda)
library(readtext)
library(spacyr)
library(newsmap)
library(seededlda)
library(tm)

# Corpus 

corp_immig <- corpus(data_char_ukimmig2010, 
                     docvars = data.frame(party = names(data_char_ukimmig2010)))
print(corp_immig)

summary(corp_immig)


# set path
path_data <- system.file("extdata/", package = "readtext")

# import csv file
dat_inaug <- read.csv(paste0(path_data, "/csv/inaugCorpus.csv"))
names(dat_inaug)

corp_inaug <- corpus(dat_inaug, text_field = "texts")
print(corp_inaug)

summary(corp_inaug, 5)

docid <- paste(dat_inaug$Year, 
               dat_inaug$FirstName, 
               dat_inaug$President, sep = " ")
docnames(corp_inaug) <- docid
print(corp_inaug)

corp_tm <- tm::VCorpus(tm::VectorSource(data_char_ukimmig2010))
corp_quanteda <- corpus(corp_tm)

corp <- data_corpus_inaugural
head(docvars(corp))

docvars(corp, field = "Year")
corp$Year

docvars(corp, field = "Century") <- floor(docvars(corp, field = "Year") / 100) + 1
head(docvars(corp))


corp <- data_corpus_inaugural
ndoc(corp)

head(docvars(corp))

corp_recent <- corpus_subset(corp, Year >= 1990)
ndoc(corp_recent)

corp_dem <- corpus_subset(corp, President %in% c("Obama", "Clinton", "Carter"))
ndoc(corp_dem)


corp <- corpus(data_char_ukimmig2010)
print(corp)

ndoc(corp)

corp_sent <- corpus_reshape(corp, to = "sentences")
print(corp_sent)
ndoc(corp_sent)


corp_doc <- corpus_reshape(corp_sent, to = "documents")
print(corp_doc)

ndoc(corp_doc)


corp_sent_long <- corpus_subset(corp_sent, ntoken(corp_sent) >= 10)
ndoc(corp_sent_long)


corp_doc_long <- corpus_reshape(corp_sent_long, to = "documents")
ndoc(corp_doc_long)



corp_tagged <- corpus(c("##INTRO This is the introduction.
                         ##DOC1 This is the first document.  Second sentence in Doc 1.
                         ##DOC3 Third document starts here.  End of third document.",
                        "##INTRO Document ##NUMBER Two starts before ##NUMBER Three."))
corp_sect <- corpus_segment(corp_tagged, pattern = "##*")

cbind(docvars(corp_sect), text = as.character(corp_sect))



corp_speeches <- corpus("Mr. Smith: Text.
                        Mrs. Jones: More text.
                        Mr. Smith: I'm speaking, again.")
corp_speakers <- corpus_segment(corp_speeches, pattern = "\\b[A-Z].+\\s[A-Z][a-z]+:", valuetype = "regex")
cbind(docvars(corp_speakers), text = as.character(corp_speakers))


corp <- corpus(c(d1 = "This, is a sentence?  You: come here.", 
                 d2 = "Yes, yes okay."))
corp_sent <- corpus_segment(corp, pattern = "\\p{P}", valuetype = "regex", 
                            extract_pattern = FALSE, pattern_position = "after")
print(corp_sent)




# Tokens

corp_immig <- corpus(data_char_ukimmig2010)
toks_immig <- tokens(corp_immig)
print(toks_immig)


toks_nopunct <- tokens(data_char_ukimmig2010, remove_punct = TRUE)
print(toks_nopunct)


toks <- tokens(data_char_ukimmig2010)

kw_immig <- kwic(toks, pattern =  "immig*")
head(kw_immig, 10)

kw_immig2 <- kwic(toks, pattern = c("immig*", "migra*"))
head(kw_immig2, 10)

kw_immig3 <- kwic(toks, pattern = c("immig*", "migra*"), window = 7)
head(kw_immig3, 10)

kw_asylum <- kwic(toks, pattern = phrase("asylum seeker*"))
head(kw_asylum)


View(kw_asylum)



toks <- tokens(data_char_ukimmig2010)

toks_nostop <- tokens_select(toks, pattern = stopwords("en"), selection = "remove")
print(toks_nostop)

toks_nostop2 <- tokens_remove(toks, pattern = stopwords("en"))
print(toks_nostop2)


toks_nostop_pad <- tokens_remove(toks, pattern = stopwords("en"), padding = TRUE)
print(toks_nostop_pad)


toks_immig <- tokens_select(toks, pattern = c("immig*", "migra*"), padding = TRUE)
print(toks_immig)


toks_immig_window <- tokens_select(toks, pattern = c("immig*", "migra*"), padding = TRUE, window = 5)
print(toks_immig_window)


toks <- tokens(data_char_ukimmig2010)


kw_multiword <- kwic(toks, pattern = phrase(c("asylum seeker*", "british citizen*")))
head(kw_multiword, 10)


toks_comp <- tokens_compound(toks, pattern = phrase(c("asylum seeker*", "british citizen*")))
kw_comp <- kwic(toks_comp, pattern = c("asylum_seeker*", "british_citizen*"))
head(kw_comp, 10)


toks <- tokens(data_char_ukimmig2010)

dict_newsmap <- dictionary(file = "../../dictionary/newsmap.yml")



toks <- tokens(data_char_ukimmig2010, remove_punct = TRUE)

toks_ngram <- tokens_ngrams(toks, n = 2:4)
head(toks_ngram[[1]], 30)

tail(toks_ngram[[1]], 30)

toks_skip <- tokens_ngrams(toks, n = 2, skip = 1:2)
head(toks_skip[[1]], 30)

toks_neg_bigram <- tokens_compound(toks, pattern = phrase("not *"))
toks_neg_bigram_select <- tokens_select(toks_neg_bigram, pattern = phrase("not_*"))
head(toks_neg_bigram_select[[1]], 30)



# DFM

toks_inaug <- tokens(data_corpus_inaugural, remove_punct = TRUE)
dfmat_inaug <- dfm(toks_inaug)
print(dfmat_inaug)

ndoc(dfmat_inaug)
nfeat(dfmat_inaug)
head(docnames(dfmat_inaug), 20)
head(featnames(dfmat_inaug), 20)
head(rowSums(dfmat_inaug), 10)
head(colSums(dfmat_inaug), 10)
topfeatures(dfmat_inaug, 10)


dfmat_inaug_prop <- dfm_weight(dfmat_inaug, scheme  = "prop")
print(dfmat_inaug_prop)


dfmat_inaug_tfidf <- dfm_tfidf(dfmat_inaug)
print(dfmat_inaug_tfidf)


toks_inaug <- tokens(data_corpus_inaugural, remove_punct = TRUE)
dfmat_inaug <- dfm(toks_inaug)
print(dfmat_inaug)

dfmat_inaug_nostop <- dfm_select(dfmat_inaug, pattern = stopwords("en"), selection = "remove")
print(dfmat_inaug_nostop)


dfmat_inaug_nostop <- dfm_remove(dfmat_inaug, pattern = stopwords("en"))
print(dfmat_inaug_nostop)


dfmat_inaug_long <- dfm_keep(dfmat_inaug, min_nchar = 5)
print(dfmat_inaug_long)


topfeatures(dfmat_inaug_long, 10)

dfmat_inaug_freq <- dfm_trim(dfmat_inaug, min_termfreq = 10)
print(dfmat_inaug_freq)

dfmat_inaug_docfreq <- dfm_trim(dfmat_inaug, max_docfreq = 0.1, docfreq_type = "prop")
print(dfmat_inaug_docfreq)