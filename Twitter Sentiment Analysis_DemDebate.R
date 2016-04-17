# install packages
install.packages(c('twitteR','ROAuth','ggplot2','devtools','rjson','bit64','httr'))
library(twitteR,ROAuth,ggplot2,devtools,rjson,bit64,httr)
install.packages(c('plyr','stringr'))
library(plyr,stringr)
install.packages(c("sqldf","RSQLite"))
library(sqldf)
library(RSQLite)

#Sets up the OAuth credentials for a twitteR session
api_key<-XXXX
api_secret<-XXXX
access_token<-XXXX
access_token_secret<-XXXX
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#Fetching tweets including hashtag #DemDebate and keyword 'Clinton', 'Sanders'
#Stores fetched tweets in local database /Users/Mengying/Desktop/DemDebate
#table Clinton contains tweets that have keyword 'Clinton'
#table Sanders contains tweets that have keyword 'Sanders'
register_sqlite_backend("/Users/Mengying/Desktop/DemDebate")
search_twitter_and_store("Clinton", "Clinton_tweets")
search_twitter_and_store("Sanders", "Sanders_tweets")

#load stored data
C<-load_tweets_db(as.data.frame = T, table_name = "Clinton_tweets")
S<-load_tweets_db(as.data.frame = T, table_name = "Sanders_tweets")

# Modify tables
# table C_s only contains tweets including "Clinton" but not "Sanders"
# table S_c only contains tweets including "Sanders" but not "Clinton"
C_s<-sqldf("select * from C where text NOT LIKE '%Sanders%'")
S_c<-sqldf("select * from S where text NOT LIKE '%Clinton%'")
    #sqldf('select count(*) from C_s')  3857
    #sqldf('select count(*) from S_c')  3921

#Sentiment analysis
#define function ToWords(), split sentence to words
ToWords<-function(sentence)
{
	  sentence<-gsub('[[:punct:]]','',sentence)
	  sentence<-gsub('[[:cntrl:]]','',sentence)
	  sentence<-gsub('\\d+','',sentence)
	  sentence<-iconv(sentence, 'UTF-8', 'ASCII')  #remove emojis
	  sentence<-tolower(sentence)
	  word.ls=str_split(sentence,'\\s+')
	  words<-unlist(word.ls)
  	return(words)
}

#define function CalScores(), calculate score of each tweet according to negative/positive-words matches. 
######About negative/positive-words
######Cite
#   Minqing Hu and Bing Liu. "Mining and Summarizing Customer Reviews." 
#       Proceedings of the ACM SIGKDD International Conference on Knowledge 
#       Discovery and Data Mining (KDD-2004), Aug 22-25, 2004, Seattle, 
#       Washington, USA, 
#   Bing Liu, Minqing Hu and Junsheng Cheng. "Opinion Observer: Analyzing 
#       and Comparing Opinions on the Web." Proceedings of the 14th 
#       International World Wide Web conference (WWW-2005), May 10-14, 
#       2005, Chiba, Japan.

pos.words<-scan('/Users/Mengying/Desktop/positive-words.txt',what='character')
neg.words<-scan('/Users/Mengying/Desktop/negative-words.txt',what='character')

CalScores<-function(sentence)
{
	  words<-ToWords(sentence)
	  pos.matches<-match(words, pos.words)
	  neg.matches<-match(words, neg.words)
			
	  pos.ma<-!is.na(pos.matches)
	  neg.ma<-!is.na(neg.matches)
	
	#the score of one tweet= [#(positive words)-#(negative words)]/#(total words)
	#the score range is [-1,1]
	#if score<0, attitude is negative; if score>0, attitude is positive
	  score<-(sum(pos.ma)-sum(neg.ma))/length(words)
	  return(score)
}

#define function score.sentiment,result is data frame
score.sentiment<-function(sentences,.progress='none'){
	  require(plyr)
	  require(stringr)
	
	  scores<-laply(sentences,function(s) CalScores(s),.progress=.progress)
	  scores.df<-data.frame(score=scores, text=sentences)
	  return(scores.df)
}

#plug data in
C.scores=score.sentiment(C_s$text,.progress='text')
S.scores=score.sentiment(S_c$text,.progress='text')

C.scores$Name<-'Hillary Clinton'
S.scores$Name<-'Bernie Sanders'

#combine
all.scores=rbind(C.scores,S.scores)

#make table
table(all.scores$score, all.scores$Name)

#make plots
p1<-ggplot(data=all.scores,aes(score))+geom_histogram(aes(fill=Name),binwidth=0.02)+facet_grid(Name~.)+theme_bw()+scale_fill_brewer()

data_vlines<-data.frame(Name=levels(as.factor(all.scores$Name)),mean=c(mean(S.scores$score),mean(C.Dscores$score)))

p2<-p1+geom_vline(aes(xintercept=mean), data=data_vlines,colour='red',linetype='dashed')

p3<-ggplot(all.scores, aes(score, fill = Name)) + geom_density(alpha=0.3)

p4<-ggplot(all.scores, aes(x=score,  fill=Name))+geom_histogram(position=position_dodge(), binwidth=0.02)


##perform test, compare two groups' mean
t.test(S.scores$score,C.scores$score,'great')
