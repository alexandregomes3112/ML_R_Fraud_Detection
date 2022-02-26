#------------------------------------------------------------------------------#
# Data Science Academy Project                                                 #
#                                                                              #
# Business Problem: TalkingData AdTracking Fraud Detection Challenge           #
#                                                                              #
# Created By: Alexandre Gomes - Data Scientist                                 #
#                                                                              #
# Data Source:                                                                 #
# https://www.kaggle.com/c/talkingdata-adtracking-fraud-detection/             #
#                                                                              #
#------------------------------------------------------------------------------#

# 1.  Exploratory data analysis

# 1.1 Defining Working Directory
setwd("c:/Users/BIJ/Dropbox/EPSON/RH/Programação/DSA/Big Data Analytics com R e Microsoft Azure Machine Learning/Análise Fraudes Cliques App Mobile/")
getwd()

# 1.2 Loading Packages
library(data.table)
library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(C50)
library(ROCR)

# 1.3 Exploring Sample
df_sample <- fread("train_sample.csv", stringsAsFactors = F, sep = ",", header =T)

# 1.4 Checking Dataset
dim(df_sample)
View(df_sample)
str(df_sample)

# 1.5 Replacing NA Values
df_sample$attributed_time <- replace_na(df_sample$attributed_time, as.Date("2017-11-06 00:00:01"))

dim(df_sample)
View(df_sample)
str(df_sample)

#df_sample$diff_time <- as.numeric(df_sample$click_time - df_sample$attributed_time, units = "mins")
# NOTE: Experiment to see if changing this variable would result in any 
#       significant improvement or insight

# 1.6 Plots to understand the relation between variables.

#ggplot(df_sample, aes(df_sample$attributed_time, df_sample$ip)) +
#  geom_line()

#ggplot(df_sample, aes(df_sample$attributed_time, df_sample$app)) +
#  geom_line()

#ggplot(df_sample, aes(df_sample$attributed_time, df_sample$device)) +
#  geom_line()

#ggplot(df_sample, aes(df_sample$attributed_time, df_sample$os)) +
#  geom_line()

#ggplot(df_sample, aes(df_sample$attributed_time, df_sample$channel)) +
#  geom_line()

#ggplot(df_sample, aes(df_sample$attributed_time, df_sample$is_attributed)) +
#  geom_line()

# Note: These plots below confirm the obvious, the relation between 
#       attributed_time and click_time, but we can find a pattern
ggplot(df_sample, aes(df_sample$attributed_time, df_sample$click_time, colour = df_sample$is_attributed)) +
  geom_line()

ggplot(df_sample, aes(df_sample$click_time, df_sample$attributed_time, colour = df_sample$is_attributed)) +
  geom_point()

# NOTE: As we can see it´s a straight, so, probably our result will be almost 
# perfect

# 1.7 Analyzing White List
# 1.7.1 Filtering the data that was clicked on effectively
white_click <- subset(df_sample, df_sample$attributed_time > as.POSIXct("2017-11-07 00:00:00"))

#white_click <- subset(df_sample, !is.na(df_sample$attributed_time))
# NOTE: I did some tests trying to use the data without any action, keeping the 
#       "NA" values, but I got better results replacing "NA" values with a date 
#       before the first record

# 1.7.2 Filtering all ip clicked
white_click <- as.vector(white_click$ip)

# 1.7.3 Creating a white list
white_list <- subset(df_sample, df_sample$ip %in% white_click)
View(white_list)

# 1.7.4 Analisis white list to identify how many clicks to download
wl_ip <- white_list %>% count(ip, sort = TRUE)
wl_ip$n <- as.numeric(wl_ip$n)

dim(wl_ip)
View(wl_ip)
hist(wl_ip$n)
summary(wl_ip$n)

# NOTE: It´s clear that we have only 6 ip´s with more than 19 clicks to download.

# 1.7.5 Excluding outliers
wl_ip <- subset(wl_ip, wl_ip$n < 19)
View(wl_ip)

wl_ip <- as.vector(wl_ip$ip)
white_list_rev <- subset(df_sample, df_sample$ip %in% wl_ip)

View(white_list_rev)
str(white_list_rev)
summary(white_list_rev)

# 1.7.6 Converting click_time and attributed_time as numeric 
white_list_rev$click_time <- as.numeric(white_list_rev$click_time)
white_list_rev$attributed_time <- as.numeric(white_list_rev$attributed_time)

# 1.7.7 Checking the proportion between effective clicked and clicked w/o download
prop.table(table(white_list_rev$is_attributed))

# 1.7.8 Correlation
cor(white_list_rev)
cor = cor(white_list_rev)
corrplot(cor, is.corr = FALSE, method = "square")

# 1.8 Analising Fake List
fake_click <- filter(df_sample, df_sample$is_attributed == 0 & !(df_sample$ip %in% white_list))
fake_click <- as.vector(fake_click$ip)
fake_list <- subset(df_sample[,c(1,2,3,4,5,6)], df_sample$ip %in% fake_click)

# 1.8.1 Converting click_time and attributed_time as numeric 
fake_list$click_time <- as.numeric(fake_list$click_time)

View(fake_list)
str(fake_list)

# 1.8.2 Analysing   
fk_ip <- fake_list %>% count(ip, sort = TRUE)

View(fk_ip)
ggplot(fk_ip, aes(fk_ip$ip, fk_ip$n)) +
  geom_line()

# NOTE: we can observe a small number of IP's that generate a great number of 
#       clicks

# 1.8.3 Checking the correlation between the variables from Fake List
cor(fake_list)
cor = cor(fake_list)
corrplot(cor, is.corr = FALSE, method = "square")

# NOTE: See that there is an obvious correlation between the device and the OS, 
#       probably given by Apple/iOS and Android. Furthermore, we see a strong 
#       and obvious correlation between the assigned time and whether or not it 
#       was assigned.

# 1.8.4 Converting click_time and attributed_time as numeric 
df_sample$click_time <- as.integer(df_sample$click_time)
df_sample$attributed_time <- as.integer(df_sample$attributed_time)

# 1.8.5  Checking the Data Frame
str(df_sample)

# 1.8.6 Checking distribution
hist(df_sample$ip)
# NOTE: Interesting to note that we have many ip's in a low range.

hist(df_sample$app)
# NOTE: We also observed that the applications are concentrated in a low 
#       numbering range

hist(df_sample$device)
# NOTE: We also have a concentration of devices in a low range of observations, 
#       making sense with market data, dominated by few manufacturers

hist(df_sample$os)
# NOTE: Following the same concept of concentration of manufacturers, 
#       the OS could not be different. Probably the concentration is between 
#       iOS and Android

hist(df_sample$channel)
# NOTE: Here we observe a very interesting point, as we find that there is a 
#       variation of significant ad publishers.

hist(df_sample$click_time)
# NOTE: We have an interesting pattern here, where the number of clicks is 
#       concentrated in a certain time range. This is a good indicator for 
#       scheduling ads at specific times.

df_sample_1 <- filter(df_sample, df_sample$attributed_time > 1509926400)
dim(df_sample_1)
# NOTE: We have very few actual click records compared to total clicks.

# 1.8.7 Checking Missing Values
sum(is.na(df_sample))

# 1.8.8 Checking Dataset
dim(df_sample)
class(df_sample)
View(df_sample)
str(df_sample)
summary(df_sample)

# 1.8.9 Checking the Correlation between the variables
cor(df_sample)
cor = cor(df_sample)
corrplot(cor, is.corr = FALSE, method = "square")

# 1.8.10  Checking data balance
prop.table(table(df_sample$is_attributed))

# NOTE: As we have not too much white list, I decided to use 100% white list 
#       and random fake list as the same proportion

# 1.8.11  Adjusting data balance for the model

fake_click <- filter(df_sample, df_sample$is_attributed == 0 & !(df_sample$ip %in% white_list_rev))
fake_click <- as.vector(fake_click$ip)
fake_list <- subset(df_sample, df_sample$ip %in% fake_click)

s_fake_list <- sample_n(fake_list, nrow(white_list_rev)/2, prop = 1, replace = FALSE)
nrow(white_list_rev)
nrow(s_fake_list)
dim(s_fake_list)
View(s_fake_list)
View(white_list_rev)


df_index <- rbind(s_fake_list, white_list_rev)
dim(df_index)
View(df_index)
prop.table(table(df_index$is_attributed))

# NOTE: Now we have a now we have a slightly more balanced proportion. 
#       Even with results with 28% of "1", we have a list of "0" that converted 
#       to "1" at some time

# 1.8.12 Splitting data into training and testing - 70:30 ratio
indexes <- sample(1:nrow(df_index), size = 0.7 * nrow(df_index))
df_train <- df_index[indexes,]
df_test <- df_index[-indexes,]


# 1.8.13 Checking data balance
prop.table(table(df_train$is_attributed))
prop.table(table(df_test$is_attributed))

# 1.8.14 Checking the data before submitting to the model
str(df_train$is_attributed)
str(df_test$is_attributed)
df_train$is_attributed <- as.factor(df_train$is_attributed)
df_test$is_attributed <- as.factor(df_test$is_attributed)
str(df_train$is_attributed)
str(df_test$is_attributed)

# 1.9 Creating Model
model_v1 <- C5.0(is_attributed ~ ., data = df_train)

# 1.10  Predicting with test data
predict_v1 <- predict(model_v1, df_test)

# 1.10.1  Checking acuracity
caret::confusionMatrix(df_test$is_attributed, predict_v1, positive = '1')

# 1.12  Predicting with other data
df_sample_2 <- sample_n(df_sample, 1000, prop = 1, replace = FALSE)
df_sample_2$is_attributed <- as.factor(df_sample_2$is_attributed)
predict_v2 <- predict(model_v1, df_sample_2)

# 1.12.1  Checking acuracity with random values
caret::confusionMatrix(df_sample_2$is_attributed, predict_v2, positive = '1')

# 1.13  Predicting with all whitelist
white_list_2 <- subset(df_sample, df_sample$attributed_time > 1509926400)
white_list_2$is_attributed <- as.factor((white_list_2$is_attributed))
View(white_list_2)
predict_v3 <- predict(model_v1, white_list_2)

# 1.13.1  Checking acuracity with white list 2
caret::confusionMatrix(white_list_2$is_attributed, predict_v3, positive = '1')

str(white_list_2)
str(predict_v3)

# NOTE: The model has a very high accuracy, but as we observe that the data 
# form a straight line, I decided to continue with this model, and observe the 
# results with the complete data set.

# END OF EXPLORATORY ANALYSIS

###############################################################################
###############################################################################

# 2 Working with entire data set

###############################################################################
###############################################################################

# NOTE: Some scripts may not work depending on your computer's configuration. 
#       I tried to optimize and find the best packages to run in any machine 
#       configuration. For information purposes, I ran it on a Mobile 
#       Workstation with a Core i7 processor, 64GB RAM and 2 x 1TB SSA Disks

# 2.1 Loading Data set as Data Frame
df <- fread("train.csv", stringsAsFactors = F, sep = ",", header =T)

dim(df)
head(df)
str(df)

#### --- WARNING ! The following line needs processing power and memory --- ####
summary(df)

# 2.2 Replacing NA Values
df$attributed_time <- replace_na(df$attributed_time, as.Date("2017-11-06 00:00:01"))

head(df)
dim(df)
str(df)

# 2.3 Converting click_time and attributed_time as integer
df$click_time <- as.integer(df$click_time)
df$attributed_time <- as.integer(df$attributed_time)

# 2.4 Checking Missing Values
sum(is.na(df))

# 2.5  Checking Data
head(df)
dim(df)
str(df)

white_list_prod <- subset(df, df$attributed_time > 1509926400)
dim(white_list_prod)

white_click <- as.vector(white_list_prod$ip)
white_list_prod <- subset(df, df$ip %in% white_click)

dim(white_list_prod)
str(white_list_prod)

wl_ip <- white_list_prod %>% count(ip, sort = TRUE)
wl_ip$n <- as.numeric(wl_ip$n)

dim(wl_ip)
View(wl_ip)
hist(wl_ip$n)
summary(wl_ip$n)

wl_ip <- subset(wl_ip, wl_ip$n <= 130)
View(wl_ip)

wl_ip <- as.vector(wl_ip$ip)
white_list_prod <- subset(df, df$ip %in% wl_ip)

View(white_list_prod)
str(white_list_prod)
summary(white_list_prod)

#### --- WARNING ! The following line needs processing power and memory --- ####
fake_click <- filter(df, df$is_attributed == 0 & !(df$ip %in% white_list_prod))

fake_click <- as.vector(fake_click$ip)
fake_list <- subset(df, df$ip %in% fake_click)
nrow(white_list_prod)
nrow(fake_list)

s_fake_list <- sample_n(fake_list, nrow(white_list_prod), prop = 1, replace = FALSE)
nrow(white_list_prod)
nrow(s_fake_list)
dim(s_fake_list)
View(s_fake_list)
View(white_list_rev)
prop.table(table(s_fake_list$is_attributed))
prop.table(table(white_list_prod$is_attributed))

df_index <- rbind(s_fake_list, white_list_prod)
dim(df_index)
View(df_index)
prop.table(table(df_index$is_attributed))

# 2.6 Splitting data into training and testing - 70:30 ratio, slicing to run in
#     my machine
indexes <- sample(1:nrow(df_index), size = 0.7 * nrow(df_index))
df_train <- df_index[indexes,]
df_test <- df_index[-indexes,]


# 2.7 Checking data balance
prop.table(table(df_train$is_attributed))
prop.table(table(df_test$is_attributed))

str(df_train$is_attributed)
str(df_test$is_attributed)
df_train$is_attributed <- as.factor(df_train$is_attributed)
df_test$is_attributed <- as.factor(df_test$is_attributed)
str(df_train$is_attributed)
str(df_test$is_attributed)
dim(df_train)
dim(df_test)

# 3 Creating Model
model_v1 <- C5.0(is_attributed ~ ., data = df_train)

# 4 Predicting with test data
predict_v1 <- predict(model_v1, df_test)

# 4.1 Checking acuracity
caret::confusionMatrix(df_test$is_attributed, predict_v1, positive = '1')

# 5 Predicting with df_index
predict_index <- predict(model_v1, df_index)
df_index$is_attributed <- as.factor(df_index$is_attributed)

# 5.1 Checking acuracity
caret::confusionMatrix(df_index$is_attributed, predict_index, positive = '1')

# 6 Creating a hypothetical data

df2 <- data.frame(ip = integer(),
                  app = integer(),
                  device = integer(),
                  os = integer(),
                  channel = integer(),
                  click_time = integer(),
                  attributed_time = integer(),
                  is_attributed = character(), 
                  stringsAsFactors = FALSE)
df2[1,] <- list(1,2,3,4,5,1509984059,1509984069,1)
df2[2,] <- list(2,3,4,5,7,1509984069,1509984079,0)
df2[3,] <- list(3,4,5,6,8,1509984079,1509984089,1)
df2[4,] <- list(4,5,6,7,9,1509984089,1509984099,0)
df2[5,] <- list(5,6,7,8,10,1509984099,1509984109,1)
df2[6,] <- list(6,7,8,9,11,1509984109,1509984119,0)
df2[7,] <- list(7,8,9,10,12,1509984119,1509984129,1)
df2[8,] <- list(8,9,10,11,13,1509984129,1509984139,0)

df2$is_attributed <- as.factor(df2$is_attributed)
str(df2)

# 6.1 Predicting with hypothetical data
predict_v2 <- predict(model_v1, df2 )

# 6.2 Checking acuracity
caret::confusionMatrix(df2$is_attributed, predict_v2, positive = '1')


################################################################################

# _____________                      CONCLUSION               __________________

# As it is my first ML model made independently and based on the knowledge 
# acquired until the moment I am training as a data scientist, I am still 
# disbelief about the result, probably is oversampling. However, I decided 
# to keep my line of reasoning and keep this result, as I observed that the data 
# actually behaves like a straight line, which theoretically makes it easier for 
# the ML model to hit all the variables that were presented.

################################################################################