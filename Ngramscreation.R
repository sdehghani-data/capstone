require(quanteda)
require(tm)
require(dplyr)
require(data.table)
profanity_words <- readLines("Badwords.txt", encoding = "UTF-8")

# BLOGS - Loading data

fileName <- "en_US.blogs.txt"
con <- file(fileName,open="r")
lineBlogs <- readLines(con, skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
close(con)
lineBlogs <- lineBlogs[as.logical(rbinom(length(lineBlogs),1,prob=0.5))]

txt <- corpus(lineBlogs)
remove(lineBlogs)
gc()

# BLOGS - Tokenization, cleaning, tidying and saving tokenized result

txt <- tokens(txt, remove_symbols = TRUE,
              remove_numbers = TRUE, remove_punct = TRUE, 
              remove_twitter = TRUE)
Unigrams_blogs <- dfm(txt, ngrams = 1L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Unigrams_blogs <- tf(x = Unigrams_blogs, scheme = "prop")
Unigrams_blogs <- t(rbind(names(Unigrams_blogs), colSums(Unigrams_blogs)))
Unigrams_blogs <- as.data.frame(Unigrams_blogs)
Unigrams_blogs[,1] <- as.numeric(Unigrams_blogs[,1])
Unigrams_blogs <- cbind(rownames(Unigrams_blogs), Unigrams_blogs)
Unigrams_blogs <- Unigrams_blogs[order(Unigrams_blogs[,2], decreasing = TRUE),]

save(Unigrams_blogs, file="Unigrams_blogs.RData")
remove(Unigrams_blogs)

Bigrams_blogs <- dfm(txt, ngrams = 2L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Bigrams_blogs <- tf(x = Bigrams_blogs, scheme = "prop")
Bigrams_blogs <- t(rbind(names(Bigrams_blogs), colSums(Bigrams_blogs)))
Bigrams_blogs <- as.data.frame(Bigrams_blogs)
Bigrams_blogs[,1] <- as.numeric(Bigrams_blogs[,1])
Bigrams_blogs <- cbind(rownames(Bigrams_blogs), Bigrams_blogs)
Bigrams_blogs <- Bigrams_blogs[order(Bigrams_blogs[,2], decreasing = TRUE),]

save(Bigrams_blogs, file="Bigrams_blogs.RData")
remove(Bigrams_blogs)

Trigrams_blogs <- dfm(txt, ngrams = 3L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Trigrams_blogs <- tf(x = Trigrams_blogs, scheme = "prop")
Trigrams_blogs <- t(rbind(names(Trigrams_blogs), colSums(Trigrams_blogs)))
Trigrams_blogs <- as.data.frame(Trigrams_blogs)
Trigrams_blogs[,1] <- as.numeric(Trigrams_blogs[,1])
Trigrams_blogs <- cbind(rownames(Trigrams_blogs), Trigrams_blogs)
Trigrams_blogs <- Trigrams_blogs[order(Trigrams_blogs[,2], decreasing = TRUE),]

save(Trigrams_blogs, file="Trigrams_blogs.RData")
remove(Trigrams_blogs)

Fourgrams_blogs <- dfm(txt, ngrams = 4L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Fourgrams_blogs <- tf(x = Fourgrams_blogs, scheme = "prop")
Fourgrams_blogs <- t(rbind(names(Fourgrams_blogs), colSums(Fourgrams_blogs)))
Fourgrams_blogs <- as.data.frame(Fourgrams_blogs)
Fourgrams_blogs[,1] <- as.numeric(Fourgrams_blogs[,1])
Fourgrams_blogs <- cbind(rownames(Fourgrams_blogs), Fourgrams_blogs)
Fourgrams_blogs <- Fourgrams_blogs[order(Fourgrams_blogs[,2], decreasing = TRUE),]

save(Fourgrams_blogs, file="Fourgrams_blogs.RData")
remove(Fourgrams_blogs)

remove(txt)
gc()

# TWITTER - Loading data

fileName <- "en_US.twitter.txt"
con <- file(fileName,open="r")
lineTwitter <- readLines(con, skipNul = TRUE, warn = FALSE, encoding = "latin1")
close(con)
lineTwitter <- lineTwitter[as.logical(rbinom(length(lineTwitter),1,prob=0.25))]

txt <- corpus(lineTwitter)
remove(lineTwitter)
gc()

# TWITTER - Tokenization, cleaning, tidying and saving tokenized result

txt <- tokens(txt, remove_symbols = TRUE,
              remove_numbers = TRUE, remove_punct = TRUE, 
              remove_twitter = TRUE)
Unigrams_twitter <- dfm(txt, ngrams = 1L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Unigrams_twitter <- tf(x = Unigrams_twitter, scheme = "prop")
Unigrams_twitter <- t(rbind(names(Unigrams_twitter), colSums(Unigrams_twitter)))
Unigrams_twitter <- as.data.frame(Unigrams_twitter)
Unigrams_twitter[,1] <- as.numeric(Unigrams_twitter[,1])
Unigrams_twitter <- cbind(rownames(Unigrams_twitter), Unigrams_twitter)
Unigrams_twitter <- Unigrams_twitter[order(Unigrams_twitter[,2], decreasing = TRUE),]

save(Unigrams_twitter, file="Unigrams_twitter.RData")
remove(Unigrams_twitter)

Bigrams_twitter <- dfm(txt, ngrams = 2L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Bigrams_twitter <- tf(x = Bigrams_twitter, scheme = "prop")
Bigrams_twitter <- t(rbind(names(Bigrams_twitter), colSums(Bigrams_twitter)))
Bigrams_twitter <- as.data.frame(Bigrams_twitter)
Bigrams_twitter[,1] <- as.numeric(Bigrams_twitter[,1])
Bigrams_twitter <- cbind(rownames(Bigrams_twitter), Bigrams_twitter)
Bigrams_twitter <- Bigrams_twitter[order(Bigrams_twitter[,2], decreasing = TRUE),]

save(Bigrams_twitter, file="Bigrams_twitter.RData")
remove(Bigrams_twitter)

Trigrams_twitter <- dfm(txt, ngrams = 3L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Trigrams_twitter <- tf(x = Trigrams_twitter, scheme = "prop")
Trigrams_twitter <- t(rbind(names(Trigrams_twitter), colSums(Trigrams_twitter)))
Trigrams_twitter <- as.data.frame(Trigrams_twitter)
Trigrams_twitter[,1] <- as.numeric(Trigrams_twitter[,1])
Trigrams_twitter <- cbind(rownames(Trigrams_twitter), Trigrams_twitter)
Trigrams_twitter <- Trigrams_twitter[order(Trigrams_twitter[,2], decreasing = TRUE),]

save(Trigrams_twitter, file="Trigrams_twitter.RData")
remove(Trigrams_twitter)

Fourgrams_twitter <- dfm(txt, ngrams = 4L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Fourgrams_twitter <- tf(x = Fourgrams_twitter, scheme = "prop")
Fourgrams_twitter <- t(rbind(names(Fourgrams_twitter), colSums(Fourgrams_twitter)))
Fourgrams_twitter <- as.data.frame(Fourgrams_twitter)
Fourgrams_twitter[,1] <- as.numeric(Fourgrams_twitter[,1])
Fourgrams_twitter <- cbind(rownames(Fourgrams_twitter), Fourgrams_twitter)
Fourgrams_twitter <- Fourgrams_twitter[order(Fourgrams_twitter[,2], decreasing = TRUE),]

save(Fourgrams_twitter, file="Fourgrams_twitter.RData")
remove(Fourgrams_twitter)

remove(txt)
gc()

# NEWS - Loading data

fileName <- "en_US.news.txt"
con <- file(fileName,open="rb")
lineNews <- readLines(con, skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
close(con)

txt <- corpus(lineNews)
remove(lineNews)
gc()

# NEWS - Tokenization, cleaning, tidying and saving tokenized result

txt <- tokens(txt, remove_symbols = TRUE,
              remove_numbers = TRUE, remove_punct = TRUE, 
              remove_twitter = TRUE)
Unigrams_news <- dfm(txt, ngrams = 1L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Unigrams_news <- tf(x = Unigrams_news, scheme = "prop")
Unigrams_news <- t(rbind(names(Unigrams_news), colSums(Unigrams_news)))
Unigrams_news <- as.data.frame(Unigrams_news)
Unigrams_news[,1] <- as.numeric(Unigrams_news[,1])
Unigrams_news <- cbind(rownames(Unigrams_news), Unigrams_news)
Unigrams_news <- Unigrams_news[order(Unigrams_news[,2], decreasing = TRUE),]

save(Unigrams_news, file="Unigrams_news.RData")
remove(Unigrams_news)

Bigrams_news <- dfm(txt, ngrams = 2L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Bigrams_news <- tf(x = Bigrams_news, scheme = "prop")
Bigrams_news <- t(rbind(names(Bigrams_news), colSums(Bigrams_news)))
Bigrams_news <- as.data.frame(Bigrams_news)
Bigrams_news[,1] <- as.numeric(Bigrams_news[,1])
Bigrams_news <- cbind(rownames(Bigrams_news), Bigrams_news)
Bigrams_news <- Bigrams_news[order(Bigrams_news[,2], decreasing = TRUE),]

save(Bigrams_news, file="Bigrams_news.RData")
remove(Bigrams_news)

Trigrams_news <- dfm(txt, ngrams = 3L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Trigrams_news <- tf(x = Trigrams_news, scheme = "prop")
Trigrams_news <- t(rbind(names(Trigrams_news), colSums(Trigrams_news)))
Trigrams_news <- as.data.frame(Trigrams_news)
Trigrams_news[,1] <- as.numeric(Trigrams_news[,1])
Trigrams_news <- cbind(rownames(Trigrams_news), Trigrams_news)
Trigrams_news <- Trigrams_news[order(Trigrams_news[,2], decreasing = TRUE),]

save(Trigrams_news, file="Trigrams_news.RData")
remove(Trigrams_news)

Fourgrams_news <- dfm(txt, ngrams = 4L, remove = profanity_words, tolower = TRUE, stem = FALSE)
Fourgrams_news <- tf(x = Fourgrams_news, scheme = "prop")
Fourgrams_news <- t(rbind(names(Fourgrams_news), colSums(Fourgrams_news)))
Fourgrams_news <- as.data.frame(Fourgrams_news)
Fourgrams_news[,1] <- as.numeric(Fourgrams_news[,1])
Fourgrams_news <- cbind(rownames(Fourgrams_news), Fourgrams_news)
Fourgrams_news <- Fourgrams_news[order(Fourgrams_news[,2], decreasing = TRUE),]

save(Fourgrams_news, file="Fourgrams_news.RData")
remove(Fourgrams_news)

remove(txt)
gc()


# Concatination of all UNIGRAMS to a usable format

load("Unigrams_blogs.RData")
colnames(Unigrams_blogs) <- c("Unigrams", "Count_prop")
Unigrams_blogs$Unigrams <- as.character(Unigrams_blogs$Unigrams)
load("Unigrams_twitter.RData")
colnames(Unigrams_twitter) <- c("Unigrams", "Count_prop")
Unigrams_twitter$Unigrams <- as.character(Unigrams_twitter$Unigrams)
load("Unigrams_news.RData")
colnames(Unigrams_news) <- c("Unigrams", "Count_prop")
Unigrams_news$Unigrams <- as.character(Unigrams_news$Unigrams)
Unigrams <- bind_rows(Unigrams_blogs, Unigrams_twitter, Unigrams_news)
Unigrams <- na.omit(Unigrams, cols = "Unigrams")

Unigrams_dic <- Unigrams
save(Unigrams_dic, file = "Unigrams_dic.RData")

# Creating dictionary and function for mapping N-grams

dictionary <- 1:nrow(Unigrams_dic)
names(dictionary) <- Unigrams_dic$Unigrams

mapping_words <- function(v, dictionary) {
  # split the names of a vector into w1, w2, w3...
  splitted_names <- sapply(names(v), function(x) strsplit(x, '_'))
  
  # get the number of columns (1 for unigrams, 2 for bigrams, etc)
  cols <- length(splitted_names[[1]])
  
  # initialize the output matrix
  output_matrix <- matrix(0, length(v), cols)
  
  # map the 1st column with keys of w1, 2nd with keys of w2, etc
  for(i in 1:cols){ 
    output_matrix[,i] <- dictionary[sapply(splitted_names, function(x) x[[i]])]
  }
  
  # combine new columns with counts column
  unname(cbind(output_matrix, v))
}


Uni_vector<- Unigrams$Count_prop
names(Uni_vector)<- Unigrams$Unigrams
Unigrams<- mapping_words(Uni_vector, dictionary)
colnames(Unigrams)<- c("w1", "Count_prop")

Unigrams <- as.data.table(Unigrams)
save(Unigrams, file = "Unigrams.RData")
remove(Unigrams_blogs, Unigrams_twitter, Unigrams_news, Unigrams, Uni_vector)


# Concatination and mapping of all BIGRAMS to a usable format

load("Bigrams_blogs.RData")
colnames(Bigrams_blogs) <- c("Bigrams", "Count_prop")
Bigrams_blogs$Bigrams <- as.character(Bigrams_blogs$Bigrams)
load("Bigrams_twitter.RData")
colnames(Bigrams_twitter) <- c("Bigrams", "Count_prop")
Bigrams_twitter$Bigrams <- as.character(Bigrams_twitter$Bigrams)
load("Bigrams_news.RData")
colnames(Bigrams_news) <- c("Bigrams", "Count_prop")
Bigrams_news$Bigrams <- as.character(Bigrams_news$Bigrams)
Bigrams <- bind_rows(Bigrams_blogs, Bigrams_twitter, Bigrams_news)
Bigrams <- na.omit(Bigrams, cols = "Bigrams")

Bi_vector<- Bigrams$Count_prop
names(Bi_vector)<- Bigrams$Bigrams
Bigrams<- mapping_words(Bi_vector, dictionary)
colnames(Bigrams)<- c("w1", "w2", "Count_prop")

Bigrams <- as.data.table(Bigrams)
save(Bigrams, file = "Bigrams.RData")
remove(Bigrams_blogs, Bigrams_twitter, Bigrams_news, Bigrams, Bi_vector)

# Concatination and mapping of all TRIGRAMS to a usable format

load("Trigrams_blogs.RData")
colnames(Trigrams_blogs) <- c("Trigrams", "Count_prop")
Trigrams_blogs$Trigrams <- as.character(Trigrams_blogs$Trigrams)
load("Trigrams_twitter.RData")
colnames(Trigrams_twitter) <- c("Trigrams", "Count_prop")
Trigrams_twitter$Trigrams <- as.character(Trigrams_twitter$Trigrams)
load("Trigrams_news.RData")
colnames(Trigrams_news) <- c("Trigrams", "Count_prop")
Trigrams_news$Trigrams <- as.character(Trigrams_news$Trigrams)
Trigrams <- bind_rows(Trigrams_blogs, Trigrams_twitter, Trigrams_news)
Trigrams <- na.omit(Trigrams, cols = "Trigrams")

Tri_vector<- Trigrams$Count_prop
names(Tri_vector)<- Trigrams$Trigrams
Trigrams<- mapping_words(Tri_vector, dictionary)
colnames(Trigrams)<- c("w1", "w2", "w3", "Count_prop")

Trigrams <- as.data.table(Trigrams)
save(Trigrams, file = "Trigrams.RData")
remove(Trigrams_blogs, Trigrams_twitter, Trigrams_news, Trigrams, Tri_vector)

# Concatination and mapping of all FOURGRAMS to a usable format

load("Fourgrams_blogs.RData")
colnames(Fourgrams_blogs) <- c("Fourgrams", "Count_prop")
Fourgrams_blogs$Fourgrams <- as.character(Fourgrams_blogs$Fourgrams)
load("Fourgrams_twitter.RData")
colnames(Fourgrams_twitter) <- c("Fourgrams", "Count_prop")
Fourgrams_twitter$Fourgrams <- as.character(Fourgrams_twitter$Fourgrams)
load("Fourgrams_news.RData")
colnames(Fourgrams_news) <- c("Fourgrams", "Count_prop")
Fourgrams_news$Fourgrams <- as.character(Fourgrams_news$Fourgrams)
Fourgrams <- bind_rows(Fourgrams_blogs, Fourgrams_twitter, Fourgrams_news)
Fourgrams <- na.omit(Fourgrams, cols = "Fourgrams")

# Due to RAM limit it is better to split Fourgrams to 3 parts and process them consecutively

partition_size <- round(nrow(Fourgrams)/3)
Fourgrams_1 <- Fourgrams[1:partition_size,]
Fourgrams_2 <- Fourgrams[(partition_size+1):(partition_size*2),]
Fourgrams_3 <- Fourgrams[((partition_size*2)+1):nrow(Fourgrams),]
remove(Fourgrams_blogs, Fourgrams_twitter, Fourgrams_news, Fourgrams)

# 1st part
Four_vector_1<- Fourgrams_1$Count_prop
names(Four_vector_1)<- Fourgrams_1$Fourgrams
Fourgrams_1<- mapping_words(Four_vector_1, dictionary)
colnames(Fourgrams_1)<- c("w1", "w2", "w3", "w4", "Count_prop")

save(Fourgrams_1, file = "Fourgrams_1.RData")
remove(Fourgrams_1, Four_vector_1)
gc()

# 2st part
Four_vector_2<- Fourgrams_2$Count_prop
names(Four_vector_2)<- Fourgrams_2$Fourgrams
Fourgrams_2<- mapping_words(Four_vector_2, dictionary)
colnames(Fourgrams_2)<- c("w1", "w2", "w3", "w4", "Count_prop")

save(Fourgrams_2, file = "Fourgrams_2.RData")
remove(Fourgrams_2, Four_vector_2)
gc()

# 3st part
Four_vector_3<- Fourgrams_3$Count_prop
names(Four_vector_3)<- Fourgrams_3$Fourgrams
Fourgrams_3<- mapping_words(Four_vector_3, dictionary)
colnames(Fourgrams_3)<- c("w1", "w2", "w3", "w4", "Count_prop")

save(Fourgrams_3, file = "Fourgrams_3.RData")
remove(Fourgrams_3, Four_vector_3)
gc()

# creating joint file for Fourgrams
load("Fourgrams_1.RData")
load("Fourgrams_2.RData")
load("Fourgrams_3.RData")
Fourgrams <- rbind(Fourgrams_1, Fourgrams_2, Fourgrams_3)
Fourgrams <- as.data.table(Fourgrams)
save(Fourgrams, file = "Fourgrams.RData")
remove(Fourgrams_1, Fourgrams_2, Fourgrams_3, Fourgrams)
gc()