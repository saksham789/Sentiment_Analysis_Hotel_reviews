
library(tm)
library(dplyr)
df<-read.csv("train.csv",stringsAsFactors = FALSE)
df$Is_Response<-as.factor(df$Is_Response)
set.seed(2)
df<-df[sample(nrow(df)),]
corpus<-Corpus(VectorSource(df$Description))
inspect(corpus[1:2])
corpus.clean <- corpus %>%
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords(kind="en")) %>%
  tm_map(stripWhitespace)
inspect(corpus.clean[1:2])
dtm<-DocumentTermMatrix(corpus.clean)
samp<-sample(nrow(df),nrow(df)*.75)
df.train<-df[samp,]
df.test<-df[-samp,]
dtm.train<-dtm[samp,]
dtm.test<-dtm[-samp,]
corpus.clean.train<-corpus.clean[samp]
corpus.clean.test<-corpus.clean[-samp]
fivefreq<-findFreqTerms(dtm.train,5)
length(fivefreq)
dtm.train.nb<-DocumentTermMatrix(corpus.clean.train,control = list(dictionary=fivefreq))
dtm.test.nb<-DocumentTermMatrix(corpus.clean.test,control=list(dictionary=fivefreq))
convert_count <- function(x) {
  y <- ifelse(x > 0, 1,0)
  y <- factor(y, levels=c(0,1), labels=c("No", "Yes"))
  y
}
trainNB <- apply(dtm.train.nb, 2, convert_count)
trainNb<-apply(dtm.train.nb,2,FUN = function(x){y<-ifelse(x>0,"yes","no");y<-as.factor(y);y})



