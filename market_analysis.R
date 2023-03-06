#import the data sets
library(readr)

Subscription <- read_csv("Subscription.csv")

amusement <- read_csv("amusement.csv")

# PART1 - Working with Subscription data set
# QUESTION 1

# i. number of rows and columns
dim(Subscription)

# ii. data type of each variable

str(Subscription)

# iii. first and last 15 rows
head(Subscription, n = 15)

tail(Subscription, n = 15)


# iv. Check for any missing values
any(is.na(Subscription))

# v. converting character type variables to factor type
Subscription$gender <- as.factor(Subscription$gender)
Subscription$ownHome <- as.factor(Subscription$ownHome)
Subscription$subscribe <- as.factor(Subscription$subscribe)
Subscription$Segment <- as.factor(Subscription$Segment)

class(Subscription$gender)


# vi. Summary statistics on the data
summary(Subscription)

# QUESTION TWO
library(dplyr)

subscribed <- Subscription %>%
  filter(subscribe == "subYes")

head(subscribed)

summary(subscribed)



not_subscribed <- Subscription %>%
  filter(subscribe == "subNo")

head(not_subscribed)

summary(not_subscribed)


# QUESTION 3
# create cross-tables and proportion-based cross tables between

# i. subscription status and home ownership
home_owner_table <- table(Subscription$subscribe, Subscription$ownHome)
home_owner_table

home_owner_prop_table <- prop.table(home_owner_table, margin = 1)

home_owner_prop_table

home_owner_prop_table_2 <- prop.table(home_owner_table, margin = 2)
home_owner_prop_table_2 


# ii. subscription status and gender
gender_cross_table <- table(Subscription$subscribe, Subscription$gender)
gender_cross_table

gender_prop_table <- prop.table(gender_cross_table, margin = 1)
gender_prop_table
gender_prop_table_2 <- prop.table(gender_cross_table, margin = 2)
gender_prop_table_2

# iii. subscription status and segments
segment_cross_table <- table(Subscription$subscribe, Subscription$Segment)
segment_cross_table

segment_prop_table <- prop.table(segment_cross_table, margin = 1)
segment_prop_table
segment_prop_table_2 <- prop.table(segment_cross_table, margin = 2)
segment_prop_table_2

# QUESTION 4
levels(Subscription$Segment)

# a) what the average income, average number of kids, and average age is for each segment

# segment - Moving up
Moving_up_stats <- Subscription %>%
  filter(Segment == "Moving up") %>%
  summarize(average_income = mean(income), average_no_kids = mean(kids), 
            average_age = mean(age), ownHome)

Moving_up_stats

# count home owners
Moving_up_home <- Subscription %>%
  filter(Segment == "Moving up") %>%
  filter(ownHome == "ownYes")

summary(Moving_up_home$ownHome)
# Suburb mix segement
Sburb_mix_stats <- Subscription %>%
  filter(Segment == "Suburb mix") %>%
  summarize(average_income = mean(income), average_no_kids = mean(kids), 
            average_age = mean(age))

Sburb_mix_stats

# Sburb own home
Suburb_mix_home <- Subscription %>%
  filter(Segment == "Suburb mix") %>%
  filter(ownHome == "ownYes")

summary(Suburb_mix_home$ownHome)

# Travelers Segment
Travelers_stats <- Subscription %>%
  filter(Segment == "Travelers") %>%
  summarize(average_income = mean(income), average_no_kids = mean(kids), 
            average_age = mean(age))

Travelers_stats

Travelers_home <- Subscription %>%
  filter(Segment == "Travelers") %>%
  filter(ownHome == "ownYes")

summary(Travelers_home$ownHome)

# Urban hip segment
Urban_hip_stats <- Subscription %>%
  filter(Segment == "Urban hip") %>%
  summarize(average_income = mean(income), average_no_kids = mean(kids), 
            average_age = mean(age))

Urban_hip_stats

Urban_hip_home <- Subscription %>%
  filter(Segment == "Urban hip") %>%
  filter(ownHome == "ownYes")

summary(Urban_hip_home$ownHome)

Urban_hip_home <- Subscription %>%
  filter(Segment == "Urban hip") %>%
  filter(ownHome == "ownYes")

summary(Urban_hip_home$ownHome)

# different genders in the different segments
gender_table <- table(Subscription$gender,Subscription$Segment)

gender_table
# QUESTION FIVE

# i) a histogram for income and discuss its distribution
library(ggplot2)

ggplot(Subscription,aes(income)) + geom_histogram() + labs(
  title = "A histogram for income "
)

# ii) a histogram for age
ggplot(Subscription,aes(age)) + geom_histogram() + labs(
  title = "A histogram for age "
)


# iii) a boxplot for income vs. customer segments 
ggplot(Subscription, aes(x = income, y = Segment )) + geom_boxplot() +
  labs(title = "A Boxplot for Income vs Customer Segments")

# iv)  a boxplot for age vs. customer segments
ggplot(Subscription, aes(x = age, y = Segment )) + geom_boxplot() +
  labs(title = "A Boxplot for Age vs Customer Segments")


# PART 2 - Relationship between continuous variables
# Using the amusements.csv data set

# QUESTION 7

# i) the number of rows (respondents) and number of columns (variables)
dim(amusement)

# ii) the data types of each variable
str(amusement)

# iii) whether there is any missing value in the data set
any(is.na(amusement))

# iv) run summary statistics of the data set.
summary(amusement)

# QUESTION 8
library(car)
scatterplotMatrix(formula=~num.child+distance+rides+games+wait+clean+overall,data=amusement)

# transform variables which are not normaly distributed
amusement$num.child <- log(amusement$num.child)
amusement$distance <- log(amusement$distance)

hist(amusement$num.child, probability = TRUE)

hist(amusement$distance, probability = TRUE)


# QUESTION 9
# Plot  a scatter plot for overall satisfaction and rides satisfaction

plot(amusement$rides, amusement$overall, col = "blue", main = 'Scatterplot of rides and overall satisfaction'
     , xlab = 'rides', ylab = 'overall satisfaction')

abline(lm(amusement$overall~amusement$rides ))

# QUESTION 10 -  Run a correlation matrix with corrplot.mixed() 
library(corrplot)

cor_matrix <- cor(amusement[, 2:8],use = "complete.obs")
cor_matrix

corrplot(cor_matrix, method = "ellipse")


