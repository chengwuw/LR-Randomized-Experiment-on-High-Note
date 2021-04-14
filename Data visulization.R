
############################################### High Note ###############################################
# Chengwu Weng
# 02/25/2021
## https://rstudio-pubs-static.s3.amazonaws.com/594181_27503022bbc040dba00a68bd0ccd36f4.html
## https://rpubs.com/sebastk1/hnpsm
## https://github.com/chavisingal/High-Note-Case-Study
## https://nijanthanand.github.io/analytics/projects/proj-2.html

# load packages needed
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(psych)
library(MatchIt) 

# set working direction and load data
data <- read.csv("HighNote Data Midterm.csv", header = T)
head(data)

# remove the column: ID
data <- data %>%
  select( c(2:16) ) 


#################################################### Q2 ###################################################
# Data Visualization: Generate a set of charts (e.g., scatter plots, box plots, etc) 
# to help visualize how adopters and non-adopters (of the premium subscription service) 
# differ from each other in terms of (i) demographics, (ii) peer influence, 
# and (iii) user engagement. What can you conclude from your charts? 

# create a new data for visualization
data_q2 <- data
data_q2$subscribe <- ifelse(data_q2$adopter==1,'Adopter','Non-adopter')

## overview distribution
par(mar=c(2,2,2,2))
par(mfrow=c(4, 4))
colname1 <- dimnames(data_q2)[[2]]
for (i in 1:15) {
  hist(data_q2[,i], main=colname1[i], probability=TRUE, col="#f48fb1", border="white",xlab=NA )
  lines(density(data_q2[,i]))
}
par(mfrow=c(1, 1))

## Demographics

label_demo <- paste("Country Type:", c("Good", "Bad"))
label_demo2 <- paste(c('Adopter','Non-adopter'))
data_q2 %>% mutate(good_country = ifelse(good_country == 1, label_demo[1], label_demo[2]))%>%
  mutate(adopter = ifelse(adopter == 1, label_demo2[1], label_demo2[2]))%>%
  ggplot(aes(x=age, y=friend_cnt, shape=adopter, color=adopter)) + geom_point(size=2)+
  facet_wrap(~good_country)+ylim(0,2500)

# Age Quartile by Adopter
plt_d1 <- ggplot(data_q2, aes(x=subscribe, y=age)) + 
  geom_boxplot(aes(fill=subscribe),outlier.colour="red", outlier.shape=8,outlier.size=1)+ 
  ggtitle("11(a) Age Quartile by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ 
  scale_fill_manual(values=c("#f48fb1", "#ffe082")) +theme(legend.position="none")+
  theme(axis.title.x=element_blank())

# Age Distribution by Adopter
plt_d2 <- data_q2 %>%
  ggplot(aes(x=age, fill = subscribe) ) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#f06292", "#ffb74d")) + #c( "#9fa8da","#69b3a2")
  labs(fill="") + ggtitle("11(b) Age Distribution by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Age Distribution by Gender
plt_d3 <- ggplot(data_q2,aes(x = age,fill = as.factor(male))) + 
  geom_bar(position = "fill") + ggtitle("11(c) Age Distribution by Gender") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))+ 
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))

# Gender by Adopter
plt_d4 <- ggplot(data_q2, aes(subscribe))+ 
  geom_bar(aes(fill = as.factor(male)))+ 
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(axis.title.x=element_blank())+ ggtitle("11(d) Gender by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Good Country by Adopter
plt_d5 <- ggplot(data_q2, aes(subscribe))+ 
  geom_bar(aes(fill = as.factor(good_country)))+ 
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(axis.title.x=element_blank())+ ggtitle("11(e) Good Country by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

plot_grid(plt_d1,plt_d2,plt_d3,plt_d4,plt_d5, ncol=2, 
          rel_heights = c(2, 1, 1))

## Peer influence

label_peer <- paste("Gender:", c("Male", "Female"))
label_peer2 <- paste(c("Adopter", "Non_Adopter"))
data_q2 %>% mutate(male = ifelse(male == 1, label_peer[1], label_peer[2]))%>%
  mutate(adopter = ifelse(adopter == 1, label_peer2[1], label_peer2[2]))%>%
  ggplot(aes(x=friend_cnt, y=subscriber_friend_cnt, shape=male, color=male)) + geom_point(size=2)+
  facet_wrap(~adopter)+ylim(0,100)

# Friend Count by Adopter
plt_p1 <- ggplot(data_q2, aes(x=subscribe, y=friend_cnt)) + 
  geom_boxplot(aes(fill=subscribe),outlier.color="#d1495b", outlier.shape=8,outlier.size=1)+ 
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$friend_cnt, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+
  ggtitle("12(a) Friend Count by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Friend Age by Adopter
plt_p2 <- ggplot(data_q2, aes(x=subscribe, y=avg_friend_age)) + 
  geom_boxplot(aes(fill=subscribe),outlier.color="#d1495b", outlier.shape=8,outlier.size=1)+ 
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$avg_friend_age, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+ 
  ggtitle("12(b) Friend Age by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Friend Male by Adopter
plt_p3 <- ggplot(data_q2, aes(x=subscribe, y=avg_friend_male)) + 
  geom_boxplot(aes(fill=subscribe),outlier.color="#d1495b", outlier.shape=8,outlier.size=1)+
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$avg_friend_male, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+ 
  ggtitle("12(d) Friend Male by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Friend Country Count by Adopter
plt_p4 <- ggplot(data_q2, aes(x=subscribe, y=friend_country_cnt)) + 
  geom_boxplot(aes(fill=subscribe),outlier.color="#d1495b", outlier.shape=8,outlier.size=1)+
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$friend_country_cnt, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+ 
  ggtitle("12(c) Friend Country Count by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Subscriber Friend Count by Adopter
avg_subs_friend_cnt <- data_q2 %>%
  group_by(adopter)%>%
  summarise(subscriber_friend_cnt=mean(subscriber_friend_cnt)) %>%
  mutate(subscribe = ifelse(adopter==1,'Adopter','Non-adopter'))

plt_p5<-ggplot(avg_subs_friend_cnt, aes(x=subscribe, y=subscriber_friend_cnt)) + 
  geom_bar(aes(fill = subscribe), position = "dodge", stat="identity") +
  geom_text(aes(label=round(subscriber_friend_cnt,2)), vjust=0) + 
  scale_fill_manual(values=c("#f48fb1", "#ffe082")) +
  labs(y="Average Subscriber Friend Count")+ 
  theme(axis.title.x=element_blank())+ 
  ggtitle("12(e) Subscriber Friend Count by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

grid.arrange(plt_p1,plt_p2,plt_p4,plt_p3,plt_p5, ncol=2)


## User Engagement 

label_engage <- paste("Gender:", c("Male", "Female"))
labs_engage2 <- paste(c("Adopter", "Non_Adopter"))
data_q2 %>% mutate(male = ifelse(male == 1, label_engage[1], label_engage[2]))%>%
  mutate(adopter = ifelse(adopter == 1, labs_engage2[1], labs_engage2[2]))%>%
  ggplot(aes(x=songsListened, y=lovedTracks,shape=adopter, color=adopter)) + geom_point(size=2)+
  facet_wrap(~male)

# Song Listened by Adopter
plt_engage1 <- ggplot(data_q2, aes(x=subscribe, y=songsListened)) + 
  geom_boxplot(aes(fill=subscribe),outlier.colour="red", outlier.shape=8,outlier.size=1)+
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$songsListened, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+ 
  ggtitle("13(a) Song Listened by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Loved Tracks by Adopter
plt_engage2 <- ggplot(data_q2, aes(x=subscribe, y=lovedTracks)) + 
  geom_boxplot(aes(fill=subscribe),outlier.colour="red", outlier.shape=8,outlier.size=1)+
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$lovedTracks, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+ 
  ggtitle("13(b) Loved Tracks by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Posts by Adopter
avg_post <- data_q2 %>%
  group_by(adopter)%>%
  summarise(posts=mean(posts)) %>%
  mutate(subscribe = ifelse(adopter==1,'Adopter','Non-adopter'))

plt_engage3<-ggplot(avg_post, aes(x=subscribe, y=posts)) + 
  geom_bar(aes(fill = subscribe), position = "dodge", stat="identity") +
  geom_text(aes(label=round(posts,2))) + 
  scale_fill_manual(values=c("#f48fb1", "#ffe082")) +
  labs(y="Average Posts")+
  theme(axis.title.x=element_blank())+ 
  ggtitle("13(c) Posts by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Playlists by Adopter
avg_playlists <- data_q2 %>%
  group_by(adopter)%>%
  summarise(playlists=mean(playlists)) %>%
  mutate(subscribe = ifelse(adopter==1,'Adopter','Non-adopter'))

plt_engage4<-ggplot(avg_playlists, aes(x=subscribe, y=playlists)) + 
  geom_bar(aes(fill = subscribe), position = "dodge", stat="identity") +
  geom_text(aes(label=round(playlists,2))) + 
  scale_fill_manual(values=c("#f48fb1", "#ffe082")) +
  labs(y="Average Playlists")+
  theme(axis.title.x=element_blank())+ 
  ggtitle("13(d) Playlists by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Shouts by Adopter
plt_engage5 <- ggplot(data_q2, aes(x=subscribe, y=shouts)) + 
  geom_boxplot(aes(fill=subscribe),outlier.colour="red", outlier.shape=8,outlier.size=1)+
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$shouts, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+ 
  ggtitle("13(e) Shouts by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

# Tenure by Adopter
plt_engage6 <- ggplot(data_q2, aes(x=subscribe, y=tenure)) + 
  geom_boxplot(aes(fill=subscribe),outlier.colour="red", outlier.shape=8,outlier.size=1)+
  scale_fill_manual(values=c("#f48fb1", "#ffe082"))+
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(data_q2$tenure, c(0.1, 0.9)))+
  theme(axis.title.x=element_blank())+ 
  ggtitle("13(f) Tenure by Adopter") + 
  theme(plot.title = element_text(color= "dodgerblue4", size = 14,face="bold"))

grid.arrange(plt_engage1,plt_engage2,plt_engage3,plt_engage4,plt_engage5,plt_engage6, ncol=2)


