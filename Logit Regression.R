
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

#################################################### Q3 ###################################################

# 3.	Propensity Score Matching (PSM): 
# You will use PSM to test whether having subscriber friends affects the likelihood of becoming an adopter (i.e., fee customer).
# For this purpose, the "treatment" group will be users that have one or more subscriber friends (subscriber_friend_cnt >= 1),
# while the "control" group will include users with zero subscriber friends. 
# Use PSM to first create matched treatment and control samples, 
# then test whether there is a significant average treatment effect. Provide an interpretation of your results.

# Taking log for the highly skewed terms, abs(skew value)>=2
data$ln_friend_cnt <- log(data$friend_cnt)
data$ln_friend_country_cnt <- log(data$friend_country_cnt +1)
data$ln_subscriber_friend_cnt <- log(data$subscriber_friend_cnt +1)
data$ln_songsListened <- log(data$songsListened +1)
data$ln_lovedTracks <- log(data$lovedTracks +1)
data$ln_posts <- log(data$posts +1)
data$ln_playlists <- log(data$playlists +1)
data$ln_shouts <- log(data$shouts +1)

data <- data %>%
  mutate(treatment = ifelse(subscriber_friend_cnt >= 1,1,0))

# calculate the mean for each covariates by treatment status

summary_treat <- data %>%
  group_by(treatment) %>%
  select(c('age','male','ln_friend_cnt','avg_friend_age',
           'avg_friend_male','ln_friend_country_cnt','ln_songsListened',
           'ln_lovedTracks','ln_posts','ln_playlists','ln_shouts','tenure',
           'good_country')) %>%
  summarise_all(funs(mean(., na.rm = T)))
htmlTable::htmlTable(round(summary_treat,2))

cov1 <- c('age','male','ln_friend_cnt','avg_friend_age',
          'avg_friend_male','ln_friend_country_cnt',
          'ln_lovedTracks','ln_posts','ln_playlists',
          'ln_shouts','tenure','good_country','ln_songsListened')

lapply(cov1, function(a) {
  t.test(data[, a] ~ data$treatment)
})

with(data, t.test(adopter ~ treatment))

### Propensity score estimation #####

model_ps <- glm(treatment ~  age + male + ln_friend_cnt + avg_friend_age + avg_friend_male
                + ln_friend_country_cnt + ln_songsListened + ln_lovedTracks + ln_posts
                + ln_playlists + ln_shouts + tenure + good_country,
                family = binomial(), data = data) # no subscriber
summary(model_ps)

# propensity score for each user
propensity_score <- data.frame(pr_score = predict(model_ps, type = "response"),
                               treatment = model_ps$model$treatment )

# histgram
label <- paste("Treatment Type:", c("0 Subscriber friends", "1 or more Subscriber friends"))

propensity_score %>%
  mutate(treatment = ifelse(treatment == 0, label[1], label[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white",bins=55) +
  facet_wrap( ~ treatment) +
  xlab("Probability of having subscriber friends") +
  theme_bw() + xlim(0,1.01)

cov2 <- c('age','male','ln_friend_cnt','avg_friend_age',
          'avg_friend_male','ln_friend_country_cnt','ln_songsListened',
          'ln_lovedTracks','ln_posts','ln_playlists','ln_shouts','tenure','good_country',
          'adopter','ln_subscriber_friend_cnt')

data_nomiss <- data %>%  # MatchIt does not allow missing values
  select(subscriber_friend_cnt, treatment, one_of(cov2)) %>%
  na.omit()

mod_match <- matchit(formula = treatment ~ age + male + ln_friend_cnt + avg_friend_age + avg_friend_male
                     + ln_friend_country_cnt + ln_songsListened + ln_lovedTracks + ln_posts
                     + ln_playlists + ln_shouts + tenure + good_country, 
                     data = data_nomiss, caliper = 0.02, method = "nearest")
summary(mod_match)

plot(mod_match)

## create new data
data_matched <- match.data(mod_match,drop.unmatched = T)
dim(data_matched) #Dimensions of the matched data frame
table(data_matched$treatment) #Treatment distribution in the matched data frame

data_matched$treat <- as.factor(data_matched$treatment)
plt1 <- ggplot(data_matched, aes(y=distance,x=treat)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=1) +
  theme(legend.position="none") 
plt1
data_matched$treat <- NULL

summary_matched <- data_matched %>%
  group_by(treatment) %>% 
  select(c('age','male','ln_friend_cnt','avg_friend_age',
           'avg_friend_male','ln_friend_country_cnt','ln_songsListened',
           'ln_lovedTracks','ln_posts','ln_playlists','ln_shouts','tenure',
           'good_country')) %>%
  summarise_all(funs(mean(., na.rm = T)))
htmlTable::htmlTable(round(summary_matched,2))

# t test after matching 
cov3 <- c('age','male','ln_friend_cnt','avg_friend_age',
          'avg_friend_male','ln_friend_country_cnt','ln_songsListened',
          'ln_lovedTracks','ln_posts','ln_playlists','ln_shouts','tenure','good_country')

lapply(cov3, function(v) {
  t.test(data_matched[, v] ~ data_matched$treatment)
})

with(data_matched, t.test(adopter~ treatment))


#################################################### Q4 ###################################################
# Regression Analyses: use a logistic regression approach to test which variables (including subscriber friends) 
# are significant for explaining the likelihood of becoming an adopter. 
# Use your judgment and visualization results to decide which variables to include in the regression. 
# Estimate the odds ratios for the key variables. What can you conclude from your results?

# model 1 with all variables of interest
glm_treat1 <- glm(adopter ~ age + male + ln_friend_cnt + avg_friend_age + avg_friend_male
                  + ln_friend_country_cnt + ln_songsListened + ln_lovedTracks + ln_posts
                  + ln_playlists + ln_shouts + tenure + good_country + ln_subscriber_friend_cnt,
                  data = data_matched, family='binomial')
summary(glm_treat1)
exp(coef(glm_treat1))

#ln_friend_cnt, avg_friend_male, ln_friend_country_cnt

# model 2 removing insignificant variables
glm_treat2 <- glm(adopter ~  age + male + avg_friend_age
                  + ln_songsListened + ln_lovedTracks + ln_posts
                  + ln_playlists + ln_shouts + tenure + good_country+ ln_subscriber_friend_cnt ,
                  data = data_matched, family='binomial')
summary(glm_treat2)
exp(coef(glm_treat2))

