library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("TrudeauMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



Trudeau <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time(Trudeau)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
Trudeau
#r <- ggplotly(p)
#r

library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("HarperMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



Harper <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time(Harper)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
Harper
#r <- ggplotly(p)
#r
library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("MulcairMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



Mulcair <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time (Mulcair)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
Mulcair
#r <- ggplotly(p)
#r

library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("ElizabethMayMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



ElizabethMay <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time (Elizabeth May)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
ElizabethMay
#r <- ggplotly(p)
#r

library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("LiberalMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



Liberal <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time (Liberal)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
Liberal
#r <- ggplotly(p)
#r

library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("ConservativeMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



Conservative <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time (Conservative)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
Conservative
#r <- ggplotly(p)
#r

library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("NDPMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



NDP <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time (NDP)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
NDP
#r <- ggplotly(p)
#r

library(stringi)
library(dplyr)
library(SciencesPo)
tweets <-read.csv("GreenPartyMain.csv")
tweets$summer <- 1
filter=c("niqab")
filter2=c("marijuana")
filter3=c("refugees")
filter4=c("tax")
filter5=c("oil")
filter6=c("climate")
filter7=c("budget")
filter8=c("tpp")

tweets$niqab <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter)),1L)
tweets$marijuana <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter2)),1L)
tweets$refugee <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter3)),1L)
tweets$tax <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter4)),1L)
tweets$oil <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter5)),1L)
tweets$climate <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter6)),1L)
tweets$budget <- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter7)),1L)
tweets$tpp<- vapply(tweets$text, function(x) sum(stri_count_fixed(x,filter8)),1L)


tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(niqab)) %>%ungroup() -> tweetsniqab
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(marijuana)) %>%ungroup() -> tweetsmarijuana
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(refugee)) %>%ungroup() -> tweetsrefugee
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tax)) %>%ungroup() -> tweetstax
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(oil)) %>%ungroup() -> tweetsoil
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(climate)) %>%ungroup() -> tweetsclimate
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(budget)) %>%ungroup() -> tweetsbudget
tweets %>% group_by(Date=format(as.Date(created),format="%Y-%m-%d")) %>% 
  summarize(freq =sum(tpp)) %>%ungroup() -> tweetstpp

tweets$Date<- as.Date(tweets$created)
tweetssummary <- tweets %>% group_by(Date) %>% summarise_each(funs(mean), 
                        -text, -replyToSN, -id, -retweetCount, -latitude, -longitude, 
                        -replyToUID, -isRetweet, -favorited, -truncated, - statusSource, -retweeted, 
                        -favoriteCount, -replyToSID, -screenName, -longitude, -replyToUID, 
                        -niqab, -created, -marijuana, -tax, -oil, -climate, -budget, -tpp, -refugee, -x)

tweetsniqab$Date <- as.Date(tweetsniqab$Date)
tweetsmarijuana$Date <- as.Date(tweetsmarijuana$Date)
tweetsrefugee$Date <- as.Date(tweetsrefugee$Date)
tweetstax$Date <- as.Date(tweetstax$Date)
tweetsoil$Date <- as.Date(tweetsoil$Date)
tweetsclimate$Date <- as.Date(tweetsclimate$Date)
tweetsbudget$Date <- as.Date(tweetsbudget$Date)
tweetstpp$Date <- as.Date(tweetstpp$Date)

tweetsniqab <- merge.data.frame(tweetssummary, tweetsniqab, by = "Date")
tweetsmarijuana <- merge.data.frame(tweetssummary, tweetsmarijuana, by= "Date")
tweetsrefugee <- merge.data.frame(tweetssummary, tweetsrefugee, by= "Date")
tweetstax <- merge.data.frame(tweetssummary, tweetstax, by = "Date")
tweetsoil <- merge.data.frame(tweetssummary, tweetsoil, by= "Date")
tweetsclimate <- merge.data.frame(tweetssummary, tweetsclimate, by= "Date")
tweetsbudget <- merge.data.frame(tweetssummary, tweetsbudget, by= "Date")
tweetstpp <- merge.data.frame(tweetssummary, tweetstpp, by= "Date")


tweetsniqab$normnumber <- (tweetsniqab$freq/tweetsniqab$X)*100
tweetsmarijuana$normnumber <- (tweetsmarijuana$freq/tweetsmarijuana$X)*100
tweetsrefugee$normnumber <- (tweetsrefugee$freq/tweetsrefugee$X)*100
tweetstax$normnumber <- (tweetstax$freq/tweetstax$X)*100
tweetsoil$normnumber <- (tweetsoil$freq/tweetsoil$X)*100
tweetsclimate$normnumber <- (tweetsclimate$freq/tweetsclimate$X)*100
tweetsbudget$normnumber <- (tweetsbudget$freq/tweetsbudget$X)*100
tweetstpp$normnumber <- (tweetstpp$freq/tweetstpp$X)*100



GreenParty <- ggplot()+
  geom_line(data=tweetsniqab, aes(x=Date, y=normnumber, color="Niqab"))+
  geom_line(data=tweetsmarijuana, aes(x=Date, y=normnumber, color="Marijuana"))+
  geom_line(data=tweetsrefugee, aes(x=Date, y=normnumber, color="Refugees"))+
  geom_line(data=tweetstax, aes(x=Date, y=normnumber, color="Tax"))+
  geom_line(data=tweetsoil, aes(x=Date, y=normnumber, color="Oil"))+
  geom_line(data=tweetsclimate, aes(x=Date, y=normnumber, color="Climate"))+
  geom_line(data=tweetsbudget, aes(x=Date, y=normnumber, color="Budget"))+
  geom_line(data=tweetstpp, aes(x=Date, y=normnumber, color="Tpp"))+
  xlab('Date')+
  ylab('Term Frequency (percent)')+
  labs(color="Terms")+
  ggtitle("Terms in Tweets over Time (Green Party)")+
  scale_x_date(date_breaks = "3 day",date_labels = "%m-%d")+
  scale_y_continuous(breaks= seq(from=0, to=100, by=2))
GreenParty
#r <- ggplotly(p)
#r


