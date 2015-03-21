library(twitteR)
library(tm)
library(wordcloud)

APIkey <- 'w1K1Sl7CC0jAQsxSU0rcyXZYJ'
APIsecret <- '4IMoJZtP5Dwh2aF5TQoNIYe5BUl9ESHO0ibjrpQ1LKYdWDZz6I'
Accesstoken <-  '55613153-mJLYUOwu34l1BSJeFpweeAeGtsn6u56mrKyxPjplq'
Accesssecret <- 'JuIw1fMPrU3DfL6Ba6mjfogsO5ifDY9hlFGkbplmgqXeq'

#setup_twitter_oauth(APIkey,APIsecret)

origop <- options("httr_oauth_cache")
options(httr_oauth_cache=TRUE)
setup_twitter_oauth(APIkey,APIsecret, Accesstoken, Accesssecret)
options(httr_oauth_cache=origop)



tweetGrab <- function(string, n = 1000, min=1){

	tweets <- searchTwitter(as.character(string), n = n, lang = 'en', resultType="recent")
	tweets_text <- sapply(tweets, function(x) x$getText())
	tweets_text_corpus <- Corpus(VectorSource(tweets_text))
 	tweets_text_corpus <- tm_map(tweets_text_corpus,
    	content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
        mc.cores=1
    )
	tweets_text_corpus <- tm_map(tweets_text_corpus, content_transformer(tolower), mc.cores=1)
	tweets_text_corpus <- tm_map(tweets_text_corpus, removePunctuation, mc.cores=1)
	tweets_text_corpus <- tm_map(tweets_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
	tdm = TermDocumentMatrix(tweets_text_corpus, control=list(tolower = TRUE))
	m = as.matrix(tdm)
	word_freqs = sort(rowSums(m), decreasing=TRUE) 
	dm = data.frame(word=names(word_freqs), freq=word_freqs)
	dm <- dm[-1,]
	wordcloud(dm$word, dm$freq, min.freq= min, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
	dm

}

r1 <- tweetGrab('crossfit', n=1000, min = 5)

