graphics.off() # clear all previous plots
rm(list=ls()) # clear the environment from previous codes
cat("\014") # clear the console 
# Install Packages if not already installed
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")



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
cat("Z-score:", z_score, "\n")
cat("P-value:", p_value, "\n")























# Sample T Test
x_sample <- df$vote[df$vote == "Donald Trump"]
y_sample <- df$vote[df$vote == "Other"]

# Convert votes to numeric for t-test
x_sample_numeric <- as.numeric(x_sample == "Donald Trump")
y_sample_numeric <- as.numeric(y_sample == "Other")

# Perform two-sample t-test 
#sample_t_test <- t.test(x_sample_numeric, y_sample_numeric, alternative = "two.sided")
#sample_t_test <- t.test(x_sample_numeric,mean= trump_mean, alternative = "two.sided")
#print(sample_t_test)
