# Number 1
install.packages("tm")
install.packages("topicmodels")
install.packages("wordcloud")
install.packages("SnowballC")
library(tm)
library(topicmodels)
library(wordcloud)
library(SnowballC)

# Number 2
textdata <- read.csv("News.csv")
corp <- Corpus(DataframeSource(textdata))

# Number 3
processedCorp <- tm_map(corp, stripWhitespace)
processedCorp <- tm_map(processedCorp, removePunctuation)
processedCorp <- tm_map(processedCorp, removeNumbers)
processedCorp <- tm_map(processedCorp, removeWords, stopwords("english"))
processedCorp <- tm_map(processedCorp, stemDocument)

# Number 4
DTM <- DocumentTermMatrix(processedCorp, control = list(bounds = list(global = c(3, Inf))))
dim(DTM)
nTerms(DTM)
nDocs(DTM)
DTM$dimnames$Terms[1:50]

# Number 5
set.seed(1000)
tm <- LDA(DTM, 20, method = "Gibbs", control = list(iter = 1000, verbose = 50))

# Number 6
tm.res <- posterior(tm)

beta <- tm.res$terms
dim(beta)
beta[,1:5]
rowSums(beta)

theta <- tm.res$topics
dim(theta)
theta[1:5, ]
rowSums(theta)[1:10]

# Number 7
terms(tm, 10)

# Number 8
as.character(corp[1082]$content)
barplot(theta[1082,])

top.term.prob <- sort(beta[4,], decreasing = TRUE)[1:50]
wordcloud(names(top.term.prob), top.term.prob, random.order = FALSE)
