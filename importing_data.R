graphics.off() # clear all previous plots
rm(list=ls()) # clear the environment from previous codes
cat("\014") # clear the console 
# Install Packages if not already installed
if (!require(devtools)) install.packages("devtools")
if (!require(ggpubr)) devtools::install_github("kassambara/ggpubr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(car))  install.packages("car")
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(car)
library(devtools)


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
print('-----------------------------------')


# Z Test
# H0: Proportion of voters who support Trump = 0.50
# Ha: Proportion of voters who support Trump != 0.50

# Sample proportion
sample_proportion <- sum(df$vote == "Donald Trump") / nrow(df)
p0 <- 0.50  # Null hypothesis proportion
n <- nrow(df)

# Standard error and Z-score
se <- sqrt(p0 * (1 - p0) / n)
z_score <- (sample_proportion - p0) / se

# P-value (two-tailed test)
p_value <- 2 * (1 - pnorm(abs(z_score)))

# Create the plot for Z-Test
library(ggplot2)

# Generate a sequence of values for the normal distribution
x <- seq(-4, 4, length = 1000)
y <- dnorm(x)

print("Z-Test Results:")
print(paste("Sample Proportion of Trump Voters:", round(sample_proportion, 4)))
print(paste("Z-score:", round(z_score, 4)))
print(paste("P-value:", round(p_value, 4)))
print('-----------------------------------')
# Create the plot
ggplot(data.frame(x, y), aes(x, y)) +
  geom_line(color = "blue", size = 1) +  # Normal distribution curve
  geom_vline(xintercept = z_score, color = "red", linetype = "dashed", size = 1) +  # Z-score line
  geom_area(data = data.frame(x = x[x <= -abs(z_score)]), aes(x = x, y = dnorm(x)), fill = "red", alpha = 0.3) +  # Left tail area (p-value)
  geom_area(data = data.frame(x = x[x >= abs(z_score)]), aes(x = x, y = dnorm(x)), fill = "red", alpha = 0.3) +  # Right tail area (p-value)
  labs(title = "Z-Test Visualization",
       subtitle = paste("Z-score = ", round(z_score, 2), " | P-value = ", round(p_value, 4)),
       x = "Z-value",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



#LINEAR REGRESSION
# Convert 'vote' to numeric: Kamala Harris = 0, Donald Trump = 1, Other = 2
  df$vote_numeric <- ifelse(df$vote == "Kamala Harris", 0, 
                            ifelse(df$vote == "Donald Trump", 1, 2))
  colnames(df)
  colnames(df)[colnames(df) == "incorrect_column_name"] <- "vote_different_than_at_least_one_of_your_parent"
  
# Check the new 'vote_numeric' column
  head(df)
  
# Predict vote outcome based on gender, vote_diff, and minority status
  model <- lm(vote_numeric ~ gender + different + minority, data = df)
  
# View the summary of the regression model
  summary(model)
  
# Predict the vote outcome based on the model
  df$predicted_vote <- predict(model)
  
# Display the first few predicted values
  head(df$predicted_vote)
  
#Visualizing the predicted vote by gender with a scatterplot and best fit line
  ggplot(df, aes(x = factor(gender), y = predicted_vote)) +
    geom_point(aes(color = factor(gender)), size = 3) +  # Add points for predicted votes by gender
    geom_smooth(method = "lm", aes(group = 1), se = FALSE, color = "black", linetype = "dashed") +  # Best fit line
    labs(title = "Predicted Vote by Gender with Best Fit Line",
         x = "Gender",
         y = "Predicted Vote") +
    scale_color_manual(values = c("blue", "pink", "green"), 
                       labels = c("Female", "Male", "Other")) +
    theme_minimal() +
    theme(legend.title = element_blank())

  
#Create a plot for Parental Influence vs Predicted Vote with Legend Labels
  ggplot(df, aes(x = different, y = predicted_vote)) +
    geom_point(aes(color = factor(different)), size = 3) +  # Add points for each parental influence
    geom_smooth(method = "lm", aes(group = 1), se = FALSE, color = "black", linetype = "dashed") +  # Best fit line
    labs(title = "Predicted Vote by Parental Influence with Best Fit Line",
         x = "Parental Influence (Vote Difference)",
         y = "Predicted Vote") +
    scale_color_manual(values = c("0" = "blue", "1" = "pink", "2" = "green"), 
                       name = "Parental Influence", 
                       labels = c("No Influence", "Minor Influence", "Major Influence")) +  # Customize the legend
    theme_minimal() +
    theme(legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
          legend.text = element_text(size = 10))  # Customize legend text
  

  
#Create a plot for Minority Status vs Predicted Vote with a Line Plot
  ggplot(df, aes(x = factor(minority), y = predicted_vote)) +
    geom_point(aes(color = factor(minority)), size = 3) +  # Add points for each minority status
    geom_smooth(method = "lm", aes(group = 1), se = FALSE, color = "black", linetype = "dashed") +  # Best fit line
    labs(title = "Predicted Vote by Minority Status with Best Fit Line",
         x = "Minority Status",
         y = "Predicted Vote") +
    scale_color_manual(values = c("0" = "blue", "1" = "pink"),  # Color scale for Minority Status
                       name = "Minority Status",
                       labels = c("Non-Minority", "Minority")) +  # Custom labels for the legend
    theme_minimal() +
    theme(legend.title = element_text(size = 12, face = "bold"),  # Customize legend title
          legend.text = element_text(size = 10))  # Customize legend text
  
  


#Independence -> Chi-Square Goodness-of-Fit Test
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
  
  
#CALCULATIONS 
  
  

