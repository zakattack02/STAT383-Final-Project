graphics.off() # clear all previous plots
rm(list=ls()) # clear the environment from previous codes
cat("\014") # clear the console 
# Install Packages if not already installed
if (!require(devtools)) install.packages("devtools")
library(devtools)
if (!require(ggpubr)) devtools::install_github("kassambara/ggpubr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(car))  install.packages("car")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(car)



library(readxl)
df <- read_excel("consolidated_data.xlsx") 
View(df)
head(df)

#Calculate Mean
trump_mean <- mean(df$vote=="Donald Trump")


# change a column name
colnames(df)[3] <- "different"
sum(df$vote=="Donald Trump")
sum(df$vote=="Kamala Harris")
sum(df$vote=="Other")
nrow(df)

# proportion of Donald Trump voters
print("Donald Trump Votes:")
print(sum(df$vote=="Donald Trump")/nrow(df))

# proportion of Kamala Harris voters
print("Kamala Harris Votes:")
print(sum(df$vote=="Kamala Harris")/nrow(df))

# proportion of Other voters
print("Other Votes:")
print(sum(df$vote=="Other")/nrow(df))

# Subsetting a dataset

df_males <- df[df$gender==1,]
df_females <- df[df$gender==0,]

# proportion of Donald Trump voters amongst male students
print("Male Donald Trump Votes:")
print(sum(df_males$vote=="Donald Trump")/nrow(df_males))
# proportion of Donald Trump voters amongst female students
print("Female Donald Trump Votes:")
print(sum(df_females$vote=="Donald Trump")/nrow(df_females))



# proportion of Kamala Harris voters amongst Male students
print("Male Kamala Harris Votes:")
print(sum(df_males$vote=="Kamala Harris")/nrow(df_males))
# proportion of Kamala Harris voters amongst Female students
print("Female Kamala Harris Votes:")
print(sum(df_females$vote=="Kamala Harris")/nrow(df_females))
cat('-----------------------------------',"\n")

# Z Test
# H0: Proportion of voters who support Trump = 0.50
# Ha: Proportion of voters who support Trump != 0.50

# Calculate sample proportion
  sample_proportion <- sum(df$vote == "Donald Trump") / nrow(df)
  p0 <- 0.50
  n <- nrow(df)

# Calculate standard error
  se <- sqrt(p0 * (1 - p0) / n)
# Calculate z-score
  z_score <- (sample_proportion - p0) / se
# Calculate p-value
  p_value <- 2 * (1 - pnorm(abs(z_score)))
# Print results
  print("Z Testing")
  print(paste("Z-score:", z_score))
  print(paste("P-value:", p_value))
  cat('-----------------------------------',"\n")
# Sample T Test
  x_sample <- df$vote[df$vote == "Donald Trump"]
  y_sample <- df$vote[df$vote == "Other"]

# Convert votes to numeric for t-test
  x_sample_numeric <- as.numeric(x_sample == "Donald Trump")
  y_sample_numeric <- as.numeric(y_sample == "Other")

# Perform two-sample t-test 
#sample_t_test <- t.test(x_sample_numeric, y_sample_numeric, alternative = "two.sided")
#sample_t_test <- t.test(x_sample_numeric,mean= trump_mean, alternative = "two.sided")
print(sample_t_test)
#cat('-----------------------------------',"\n")


#LINEAR REGRESSION
# Create a numeric version of the 'vote' column
  df$vote_numeric <- ifelse(df$vote == "Kamala Harris", 0, 
                          ifelse(df$vote == "Donald Trump", 1, 2))

# Check the new column
  head(df)
# Linear regression model to predict vote outcome based on gender, vote difference from parents, and minority status
  model <- lm(vote_numeric ~ gender + different + minority, data = df)

# View the summary of the regression model
  summary(model)

# Predicting the vote outcome based on the model
  df$predicted_vote <- predict(model)

# Plot the predicted values against actual values
  ggplot(df, aes(x = predicted_vote, y = vote_numeric)) +
  geom_point(aes(color = factor(vote_numeric)), alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression of Vote on Gender, Parental Influence, and Minority Status",
       x = "Predicted Vote",
       y = "Actual Vote") +
  scale_color_manual(values = c("red", "green", "blue"), labels = c("Kamala Harris", "Donald Trump", "Other")) +
  theme_minimal()

# Independence -> Chi-Square Goodness-of-Fit Test
  observed_votes <- table(df$vote)
  expected_votes <- rep(sum(observed_votes) / length(observed_votes), length(observed_votes))
  chisq_test <- chisq.test(observed_votes, p = expected_votes / sum(expected_votes))
  print(chisq_test)
#print Graphs 
# Density plot
  ggplot(df, aes(x = vote_numeric, fill = factor(vote))) +
  geom_density(alpha = 0.6) +  # alpha controls transparency
  labs(title = "Density Plot of Votes by Category", 
       x = "Vote Numeric (0 = Kamala, 1 = Trump, 2 = Other)", 
       y = "Density") +
  scale_fill_manual(values = c("blue", "red", "green"), 
                    labels = c("Kamala Harris", "Donald Trump", "Other")) +
  theme_minimal() +
  theme(legend.title = element_blank())  # Remove legend title for cleaner look
  
  

  
# Calculate mean and SD
  mean_votes <- mean(df$vote == "Donald Trump")
  sd_votes <- sqrt(mean(1 - mean_votes))

  
  
#Equal Variances Test
  df$vote_numeric <- ifelse(df$vote == "Kamala Harris", 0, 
                          ifelse(df$vote == "Donald Trump", 1, 2))

# vote_numeric is the dependent variable and gender is the grouping variable
  leveneTest(vote_numeric ~ factor(vote), data = df)

# Levene's Test for gender-based vote variance
  leveneTest(vote_numeric ~ factor(gender), data = df)

# Levene's Test for parental vote difference-based variance
  leveneTest(vote_numeric ~ factor(different), data = df)

# Levene's Test for minority-based vote variance
  leveneTest(vote_numeric ~ factor(minority), data = df)

# Boxplot with jitter added
  ggplot(df, aes(x = factor(vote), y = vote_numeric, fill = factor(vote))) +
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +  # Add jitter to show individual data points
  labs(title = "Vote Variance Across Categories", 
       x = "Vote Category", 
       y = "Vote Numeric (0 = Kamala, 1 = Trump, 2 = Other)") +
  scale_fill_manual(values = c("blue", "red", "green"), 
                    labels = c("Kamala Harris", "Donald Trump", "Other")) +
  theme_minimal()

# Boxplot for gender-based vote variance
  ggplot(df, aes(x = factor(gender), y = vote_numeric, fill = factor(gender))) +
  geom_boxplot() +
  labs(title = "Vote Variance Based on Gender", 
       x = "Gender", 
       y = "Vote Numeric (0 = Kamala, 1 = Trump, 2 = Other)") +
  scale_fill_manual(values = c("lightblue", "pink","plum4"), labels = c("Male", "Female")) +
  theme_minimal()

# Boxplot for minority-based vote variance
  ggplot(df, aes(x = factor(minority), y = vote_numeric, fill = factor(minority))) +
  geom_boxplot() +
  labs(title = "Vote Variance Based on Minority Status", 
       x = "Minority Status", 
       y = "Vote Numeric (0 = Kamala, 1 = Trump, 2 = Other)") +
  scale_fill_manual(values = c("lightgreen", "orange","deeppink3"), labels = c("Not Minority", "Minority")) +
  theme_minimal()

# Boxplot for parental vote difference-based variance
  ggplot(df, aes(x = factor(different), y = vote_numeric, fill = factor(different))) +
  geom_boxplot() +
  labs(title = "Vote Variance Based on Parental Vote Difference", 
       x = "Vote Different Than Parent", 
       y = "Vote Numeric (0 = Kamala, 1 = Trump, 2 = Other)") +
  scale_fill_manual(values = c("lightcoral", "yellowgreen","firebrick"), labels = c("Same", "Different")) +
  theme_minimal()



#Normality Test   
  # Shapiro-Wilk Test for normality
  shapiro_test <- shapiro.test(df$vote_numeric)
  print(shapiro_test)
  
  # Add small random noise to vote_numeric to break ties
  df$vote_numeric_no_ties <- df$vote_numeric + rnorm(length(df$vote_numeric), 0, 1e-6)
  
  # Kolmogorov-Smirnov Test with noise
  ks_test <- ks.test(df$vote_numeric_no_ties, "pnorm", mean(df$vote_numeric_no_ties), sd(df$vote_numeric_no_ties))
  
  print(ks_test)
  
  # Histogram for normality
  ggplot(df, aes(x = vote_numeric)) +
    geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Vote Numeric", x = "Vote Numeric", y = "Frequency") +
    theme_minimal()
  
  


