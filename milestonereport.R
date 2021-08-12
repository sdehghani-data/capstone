require(tm)
require(wordcloud)
require(stringi)
require(RWeka)
require(ggplot2)
options(java.parameters = "-Xmx7300m")
set.seed(100)

# Loading data in R, sampling and gathering statistics
fileName <- "en_US.blogs.txt"
con <- file(fileName,open="r")
lineBlogs_data <- readLines(con, skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
lineBlogs <- VCorpus(VectorSource(sample(lineBlogs_data, 1000)))
words_in_Blogs <- round(sum(stri_count_words(lineBlogs_data)),1)/1000000
lines_in_Blogs <- length(lineBlogs_data)
Mb_in_Blogs <- file.info("en_US.blogs.txt")$size/1024/1024
close(con)

fileName <- "en_US.news.txt"
con <- file(fileName,open="rb")
lineNews_data <- readLines(con, skipNul = TRUE, warn = FALSE, encoding = "UTF-8") 
lineNews <- VCorpus(VectorSource(sample(lineNews_data, 1000)))
words_in_News <- round(sum(stri_count_words(lineNews_data)),1)/1000000
lines_in_News <- length(lineNews_data)
Mb_in_News <- file.info("en_US.news.txt")$size/1024/1024
close(con)

fileName <- "en_US.twitter.txt"
con <- file(fileName,open="r")
lineTwitter_data <- readLines(con, skipNul = TRUE, warn = FALSE, encoding = "latin1")
lineTwitter <- VCorpus(VectorSource(sample(lineTwitter_data, 1000)))
words_in_Twitter <- round(sum(stri_count_words(lineTwitter_data)),1)/1000000
lines_in_Twitter <- length(lineTwitter_data)
Mb_in_Twitter <- file.info("en_US.twitter.txt")$size/1024/1024
close(con)

remove(lineBlogs_data, lineNews_data, lineTwitter_data)

# Basic summary

Summary <- data.frame(file = c("Blogs", "News", "Twitter"),
                      Millions_of_words <- c(words_in_Blogs, words_in_News, words_in_Twitter),
                      Number_of_lines <- c(lines_in_Blogs, lines_in_News, lines_in_Twitter),
                      File_size_Mb <- c(Mb_in_Blogs, Mb_in_News, Mb_in_Twitter))
colnames(Summary) <- c("file", "Millions_of_words", "Number_of_lines", "File_size_Mb")
print(Summary)

# Concatination of three datasets
txt <- c(lineBlogs, lineNews, lineTwitter)
remove(lineBlogs, lineNews, lineTwitter)
gc()

#Tidying data
txt <- tm_map(txt, removeNumbers)
txt <- tm_map(txt, removePunctuation, preserve_intra_word_dashes = TRUE)
txt <- tm_map(txt, stripWhitespace)
txt <- tm_map(txt, content_transformer(tolower))
txt <- tm_map(txt, removeWords, stopwords("en"))
profanity_words <- VectorSource(readLines("Badwords.txt"))
txt <- tm_map(txt, removeWords, profanity_words)

# Tokenization
UniToken<-function(x)NGramTokenizer(x, Weka_control(min = 1, max = 1))
BiToken<-function(x)NGramTokenizer(x, Weka_control(min = 2, max = 2))
TriToken<-function(x)NGramTokenizer(x, Weka_control(min = 3, max = 3))

unigrams<-function(x){
  tdm <- TermDocumentMatrix(x, control = list(tokenize = UniToken))
  fm <- rowSums(as.matrix(tdm))
  ngram<-data.frame(ngram=names(fm),freq=fm)
  ngram<-ngram[order(-ngram$freq),]
}
bigrams<-function(x){
  tdm <- TermDocumentMatrix(x, control = list(tokenize = BiToken))
  fm <- rowSums(as.matrix(tdm))
  ngram<-data.frame(ngram=names(fm),freq=fm)
  ngram<-ngram[order(-ngram$freq),]
}
trigrams<-function(x){
  tdm <- TermDocumentMatrix(x, control = list(tokenize = TriToken))
  fm <- rowSums(as.matrix(tdm))
  ngram<-data.frame(ngram=names(fm),freq=fm)
  ngram<-ngram[order(-ngram$freq),]
}

gc()

Unigrams <- unigrams(txt)
Bigrams <- bigrams(txt)
Trigrams <- trigrams(txt)

#Some words are more frequent than others - what are the distributions of word frequencies?
wordcloud(Unigrams$ngram, Unigrams$freq, scale=c(5,0.5), max.words=100, random.order=FALSE, 
          rot.per=0.5, use.r.layout=FALSE, colors=brewer.pal(6,"Dark2"))

#What are the frequencies of 2-grams and 3-grams in the dataset?
par(mar=c(5,7,1,1))
barplot(Bigrams[1:20,2],col="orange",
        names.arg = Bigrams$ngram[1:20],horiz = TRUE,
        space=0.1, xlim=c(0,20),las=2)

par(mar=c(5,15,1,1)) 
barplot(Trigrams[1:20,2],col="orange",
        names.arg = Trigrams$ngram[1:20], horiz = TRUE,
        space=0.1, xlim=c(0,3),las=2)

#How many unique words do we need in a frequency sorted dictionary to cover 50%
# of all word instances in the language? 90%?

sumCover <- 0
for(i in 1:length(Unigrams$freq)) {
  sumCover <- sumCover + Unigrams$freq[i]
  if(sumCover >= 0.5*sum(Unigrams$freq)){break}
}
print(i)

sumCover <- 0
for(i in 1:length(Unigrams$freq)) {
  sumCover <- sumCover + Unigrams$freq[i]
  if(sumCover >= 0.9*sum(Unigrams$freq)){break}
}
print(i)

