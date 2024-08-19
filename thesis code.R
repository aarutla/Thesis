library(readxl)
library(ggplot2)
library(ggcorrplot)
library(likert)
library(srvyr)
library(plyr)
library(dplyr)
library(ggpie)
library(likert)
library(ggridges)
library(MASS)
library(robustbase)
library(lmtest)
library(car)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

numeric <- read_excel("Desktop/final numeric.xlsx")
View(numeric)

word_choices <- read_excel("Desktop/raw word.xlsx")
View(word_choices)

word_choices2 <- read_excel("Desktop/final word choice.xlsx")
View(word_choices2)

numeric1 <- read_excel("Desktop/raw numeric .xlsx")
View(numeric1)


my_data <- as.data.frame(numeric) 
my_data_words <- as.data.frame(word_choices)
my_data_words1 <- as.data.frame(word_choices2)
my_data1 <- as.data.frame(numeric1)


fit1 <- table(my_data_words1$international_vs_local_stocks)
prop.table(fit1)

summary(my_data_words)

class(my_data)
head(my_data)
tail(my_data)

ncol(my_data)

summary(my_data)
summary(my_data$education)

#changing code 


ggplot(my_data, aes(x = US_immigration, fill = potential_investments)) + 
  geom_bar(position = "stack")

library(scales)
library(ggpol)
library(scales)

#immigration year and debt aversion

ggplot(data_complete1, 
       aes(x = factor(US_immigration,
                      labels = c("Before 1980",
                                 "1980-1990",
                                 "1990-2000",
                                 "2000-2010",
                                 "2010+")), 
           y = avoid_debt, color = US_immigration)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3) +
  geom_jitter(alpha = 0.5, 
              width=.2) + 
  labs(title = "US Immigration and Debt Aversion", 
       subtitle = "On a scale from 0-10, how much do you avoid debt?",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

#immigration year and investment attitude 

ggplot(data_complete1, 
       aes(x = factor(US_immigration,
                      labels = c("Before 1980",
                                 "1980-1990",
                                 "1990-2000",
                                 "2000-2010",
                                 "2010+")), 
           y = best_describes_you, color = US_immigration)) +
  geom_boxplot(size=1,
               outlier.shape = 1,
               outlier.color = "black",
               outlier.size  = 3) +
  geom_jitter(alpha = 0.5, 
              width=.2) + 
  labs(title = "US Immigration and Investment Attitude",
       x = "",
       y = "") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_flip()

#complete data

data_complete1 <- my_data_words1[complete.cases(my_data_words1),]
data_complete_numeric <- my_data[complete.cases(my_data),]

#no 1960-1970

filtered_data <- subset(data_complete, US_immigration != "1960-1970")

#potential investments 
ggplot(data = subset(data_complete1, !is.na(potential_investments)), aes(x = US_immigration, fill = potential_investments)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

#potential investments 2
ggplot(data = subset(data_complete1, !is.na(potential_investments2)), aes(x = US_immigration, fill = potential_investments2)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")


data <- filter(data_complete, !is.na(avoid_debt))

#descriptive stats 
summary(my_data)

table(my_data$US_immigration)

table(my_data$people_household)

table(my_data$education)


#binary variables
my_data_words$debt_child_binary <- ifelse(my_data_words$debt_child== 'Yes', 1, 0)
my_data_words$debt_house_binary <- ifelse(my_data_words$debt_house == 'Yes', 1, 0)
my_data_words$debt_vacation_binary <- ifelse(my_data_words$debt_vacation== 'Yes', 1, 0)
my_data_words$debt_car_binary <- ifelse(my_data_words$debt_car == 'Yes', 1, 0)
my_data_words$debt_business_binary <- ifelse(my_data_words$debt_business== 'Yes', 1, 0)
my_data_words$debt_school_binary <- ifelse(my_data_words$debt_school == 'Yes', 1, 0)

view(my_data_words)

data_complete1 <- my_data_words[complete.cases(my_data_words),]
view (data_complete1)

#debt preference graph
library(likert)

items <- select(my_data_words, starts_with(c("debt_child_binary", "debt_house_binary", "debt_vacation_binary", "debt_car_binary", "debt_business_binary", "debt_school_binary")))

# Rename the items so that the question statement becomes the name
names(items) <- c(
  "debt_child" = "Would you take on debt for your child's education?",
  "debt_house" = "Would you take on debt to buy a house?",
  "debt_vacation" = "Would you take on debt to go on vacation or other luxury goods?",
  "debt_car" = "Would you take on debt to buy a car?",
  "debt_business" = "Would you take on debt to invest in a business?",
  "debt_school" = "Would you take on debt to attend a more expensive and highly regarded school?")

# A custom function to recode numerical responses into ordered factors
likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "No", "Yes"))
  
  y <- factor(y, levels = c("No", "Yes"))
  
  return(y)
}

items_likert <- items %>%
  mutate_all(likert_recode) %>%
  likert()

# Create a stacked bar chart
plot(items_likert, 
     # Group the items alphabetically
     group.order=names(items),
     # Plot the percentages for each response category
     plot.percents = TRUE,
     # Plot the total percentage for negative responses
     plot.percent.low = FALSE,
     # Plot the total percentage for positive responses
     plot.percent.high = FALSE,
     # Whether response categories should be centered
     # This is only helpful when there is a middle response
     # option such as "neutral" or "neither agree nor disagree"
     centered = FALSE,
     # Wrap label text for item labels
     wrap=30)

#on a scale from 1-10 graph

items <- select(my_data, starts_with(c("family", "retirement", "future_present", "avoid_debt","different_assets","debt_age","debt_country")))

# Rename the items so that the question statement becomes the name
names(items) <- c(
  "family" = "How important is family to you?",
  "retirement" = "How important is retirement to you?",
  "future_present" = "I focus more on the future than the present.",
  "avoid_debt" = "I try to avoid debt at any cost.",
  "different_assets" = "It's wise to put your money in multiple and different assets.",
  "debt_age" = "How much debt do you think others your age take on?",
  "debt_country" = "How much debt do you think others from your home country take on?")

# A custom function to recode numerical responses into ordered factors
likert_recode <- function(x) {
  y <- ifelse(is.na(x), NA,
              ifelse(x == 1, "1", 
                     ifelse (x == 2, "2",
                            ifelse(x == 3, "3",
                                   ifelse(x == 4, "4",
                                          ifelse(x == 5, "5",
                                                 ifelse(x == 6, "6",
                                                        ifelse(x == 7, "7",
                                                               ifelse(x == 8, "8",
                                                                      ifelse(x == 9, "9",
                                                                             ifelse(x == 10, "10", "0")))))))))))
  
  y <- factor(y, levels = c("0","1", "2", "3", "4", "5", "6", "7", "8", "9","10"))
  
  return(y)
}

items_likert <- items %>%
  mutate_all(likert_recode) %>%
  likert()

# Create a stacked bar chart
plot(items_likert, 
     group.order = names(items),
     plot.percents = TRUE,
     plot.percent.low = FALSE,
     plot.percent.high = FALSE,
     centered = FALSE,
     wrap = 30)


#regressions
debt_reg <- lm(avoid_debt ~ US_immigration, data = my_data_words, na.action = na.exclude)
summary(debt_reg)

debt_reg1 <- lm(avoid_debt ~ US_immigration + age_num, data = my_data_words, na.action = na.exclude)
summary(debt_reg1)

debt_reg2 <- lm(avoid_debt ~ US_immigration + income_num + people_household + age_num, data = my_data_words, na.action = na.exclude)
summary(debt_reg2)

investment_reg<- lm(attitude_investment_num ~ US_immigration, data = my_data_words, na.action = na.exclude)
summary(investment_reg)

investment_reg1<- lm(attitude_investment_num ~ US_immigration + age_num, data = my_data_words, na.action = na.exclude)
summary(investment_reg1)

investment_reg2<- lm(attitude_investment_num ~ US_immigration + income_num + people_household + age_num, data = my_data_words, na.action = na.exclude)
summary(investment_reg2)



