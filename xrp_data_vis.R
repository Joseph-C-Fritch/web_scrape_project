
library(dplyr)
library(ggplot2)
library(wordcloud2)
library(tm)
library(knitr)
library(Hmisc)
library(corrplot)

df3 <- readr::read_csv("./df3_wsentiment.csv")
df4 <- readr::read_csv("./df4.csv")
df5 = left_join(df3,df4, by = 'week')

df30 = df5%>%
  mutate(daily_percent_change = (daily_price_change/open)*100)%>%
  select(., date, number_of_posts, daily_percent_change, weekly_percent_change, polarity, 
         subjectivity, Volume, week)%>%
  group_by(., date)%>%
  summarise(.,
            count=n(), 
            percent_change = mean(weekly_percent_change),
            volume = mean(Volume), polarity = mean(polarity),
            subjectivity = mean(subjectivity))%>%
  mutate(., yesterday_posts = lag(count))%>%
  mutate(., yesterday_polarity = lag(polarity))%>%
  mutate(., yesterday_subject = lag(subjectivity))%>%
  rename(., today_post = count, 
         today_polarity = polarity, 
         today_volume = volume,
         today_perc_change = percent_change,
         today_subjectivity = subjectivity)%>%
  select(., -date)%>%
  na.omit()

df31 = df5%>%
  mutate(daily_percent_change = (daily_price_change/open)*100)%>%
  select(., date, number_of_posts, daily_percent_change, weekly_percent_change, polarity, 
         subjectivity, Volume, week)%>%
  group_by(., week)%>%
  summarise(.,
            count=n(), 
            percent_change = mean(weekly_percent_change),
            volume = mean(Volume), polarity = mean(polarity),
            subjectivity = mean(subjectivity))%>%
  mutate(., last_week_posts = lag(count))%>%
  mutate(., last_week_polarity = lag(polarity))%>%
  mutate(., last_week_subject = lag(subjectivity))%>%
  rename(., this_week_posts = count, 
         this_week_volume = volume,
         this_week_perc_change = percent_change,
         this_week_polarity = polarity, 
         this_week_subjectivity = subjectivity)%>%
  select(., -week)%>%
  na.omit()



#Plot posts and price as function of time
#df7 = df5%>%
  #filter(., weekly_price_change > 0)
#df7

#write.csv(df7,file = 'df7.csv',fileEncoding = 'UTF-8')
#########################################################################
#Add percentage change column and compare perious time period
#Week Prior
df20 = df5%>%
  group_by(., week)%>%
  summarise(.,count=n(), percentage_change = mean(weekly_price_change))%>%
  mutate(., prev_count = lag(count))%>%
  select(., percentage_change, prev_count)%>%
  na.omit()


#Day Prior
df21 = df5%>%
  rename(., day = date)%>%
  group_by(., day)%>%
  summarise(.,count=n(), percentage_change = mean(((close-open)/open)*100))%>%
  mutate(., prev_count = lag(count))%>%
  select(., percentage_change, prev_count)%>%
  na.omit()


#Plot
ggplot(data = df20, aes(x = prev_count, y = percentage_change))+
  geom_point() +
  labs(y = "Price Change, (%)",
       x = "Number of Posts, (n)",
       colour = "Legend")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.8, 0.9))+
  ggtitle('Price Change vs Number of Posts')
#########################################################################
#Sentiment vs Price Change
df22 = df5%>%
  group_by(., week)%>%
  summarise(.,polarity = mean(polarity), percentage_change = mean(weekly_price_change))%>%
  mutate(., prev_polarity = lag(polarity))%>%
  select(., percentage_change, prev_polarity)%>%
  na.omit()

#Day Prior
df23 = df5%>%
  rename(., day = date)%>%
  group_by(., day)%>%
  summarise(.,polarity = mean(polarity), percentage_change = mean(((close-open)/open)*100))%>%
  mutate(., prev_polarity = lag(polarity))%>%
  select(., percentage_change, prev_polarity)%>%
  na.omit()

#Plot
ggplot(data = df23, aes(x = prev_polarity, y = percentage_change))+
  geom_point() +
  labs(y = "Price Change, (%)",
       x = "Average Post Sentiment Polarity",
       colour = "Legend")+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = c(0.8, 0.9))+
  ggtitle('Price Change vs Average Post Sentiment')
#########################################################################





#mydata.cor = cor(df8, method = c("spearman"))

mydata.rcorr = rcorr(as.matrix(df31))
rcx = mydata.rcorr
df.rcx.r=round(data.frame(rcx$r),2)
df.rcx.p=round(data.frame(rcx$P),2)

write.csv(df.rcx.r,file = 'df.rcx.r2.csv',fileEncoding = 'UTF-8')
write.csv(df.rcx.p,file = 'df.rcx.p2.csv',fileEncoding = 'UTF-8')


mydata.cor = cor(df31, method = c("pearson"))

corrplot(mydata.cor)

mydata.rcorr = rcorr(as.matrix(df30))



mydata.cor = cor(df31, method = c("pearson"))

corrplot(mydata.cor)



#ggplot(df8, aes(x=date, y=count, fill = count)) +
  #geom_histogram(stat="identity", position = 'dodge')
  #geom_line(data=df8, aes(x=date, y=percentage_change*20), colour="red")+
  #scale_y_continuous(sec.axis = sec_axis(~./20, name = "Price Change, (%)")) 
  #labs(y = "Number of Posts, (n)",
  #            x = "Date",
  #            colour = "Legend")+
  #theme(plot.title = element_text(hjust = 0.5)) +
  #theme(legend.position = c(0.8, 0.9))+
  #ggtitle('Post Count & Percentage Change Over Time')




#docs <- Corpus(VectorSource(df7$text)) 
#tdm <- TermDocumentMatrix(docs)
#m <- as.matrix(tdm)
#v <- sort(rowSums(m),decreasing=TRUE)
#d <- data.frame(word = names(v),freq=v)

##### 		from frequency counts 	#####
#docs <- Corpus(VectorSource(df3$text)) 
#tdm <- TermDocumentMatrix(docs)
#m <- as.matrix(tdm)
#v <- sort(rowSums(m),decreasing=TRUE)
#d <- data.frame(word = names(v),freq=v)

#set.seed(1234)
#wordcloud2(data = d[0:400, ], size = .30, backgroundColor = 'black',color = 'white',
#          figPath = "./xrp_logo2.jpg")

#Plot posts and price as function of time
#df6 = df3%>%
#  group_by(., date)%>%
#  summarise(.,count=n(), price = mean(close))
#df6


#ggplot(df6, aes(x = date)) +
#  geom_line(aes(y = count, colour = "posts")) +
#  geom_line(aes(y = price*800, colour = "price")) +
#  scale_y_continuous(sec.axis = sec_axis(~./800, name = "Price, ($)")) +
#  scale_colour_manual(values = c("blue", "red")) +
#  labs(y = "Number of Posts, (n)",
#              x = "Date",
#              colour = "Legend")+
#  theme(plot.title = element_text(hjust = 0.5)) +
#  theme(legend.position = c(0.8, 0.9))+
#  ggtitle('Number of Posts & Price Over Time')




#Filter words that appear the day before price increase
#Day Prior
df40 = df5%>%
  #mutate(., prev_count = lag(count))%>%
  filter(., daily_price_change>0)%>%
  select(., date)%>%
  unique()
df41 = df40
df41$date = df40$date-1

df42 = df5%>%
  filter(., date %in% df41$date)%>%
  select(., text)

write.csv(df42,file = 'df42.csv',fileEncoding = 'UTF-8')

df43 = df5%>%
  #mutate(., prev_count = lag(count))%>%
  filter(., weekly_price_change>0)%>%
  select(., week)%>%
  unique()
df44 = df43
df44$week = df43$week-1

df45 = df5%>%
  filter(., week %in% df44$week)%>%
  select(., text)

write.csv(df45,file = 'df45.csv',fileEncoding = 'UTF-8')

#Filter words that appear the day before price decresase
#Day Prior
df46 = df5%>%
  #mutate(., prev_count = lag(count))%>%
  filter(., daily_price_change<0)%>%
  select(., date)%>%
  unique()
df47 = df46
df47$date = df46$date-1

df48 = df5%>%
  filter(., date %in% df41$date)%>%
  select(., text)

write.csv(df48,file = 'df48.csv',fileEncoding = 'UTF-8')

#Filter words that appear the day before price increase
#Day Prior

df49 = df5%>%
  #mutate(., prev_count = lag(count))%>%
  filter(., weekly_price_change<0)%>%
  select(., week)%>%
  unique()
df50 = df49
df50$week = df49$week-1

df51 = df5%>%
  filter(., week %in% df44$week)%>%
  select(., text)

write.csv(df51,file = 'df51.csv',fileEncoding = 'UTF-8')

