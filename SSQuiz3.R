# 1.
# a)
# Given parameters
mu <- 54.5
sigma <- 10.5

# Calculate the z-score for 67
z_a <- (67 - mu) / sigma

# Calculate the probability that a value is greater than 67
prob_a <- pnorm(z_a, lower.tail = FALSE)

# b)
# Calculate the z-score for 19
z_b <- (19 - mu) / sigma

# Calculate the probability that a value is less than 19
prob_b <- pnorm(z_b)

# c)
# Calculate the z-scores for 45 and 55
z_c1 <- (45 - mu) / sigma
z_c2 <- (55 - mu) / sigma

# Calculate the probability that a value is between 45 and 55
prob_c <- pnorm(z_c2) - pnorm(z_c1)

# Output the probabilities
cat("Part (a) Probability:", prob_a, "\n")
cat("Part (b) Probability:", prob_b, "\n")
cat("Part (c) Probability:", prob_c, "\n")

# 2.
# Read the dataset for chi-square test
data <- read.csv("C:/Users/navee/Documents/STAM 200/Quiz3_DataSet2_15.csv")

# Create a contingency table of Sex and Sugar consumption
table_data <- table(data$Sex, data$Sugar)

# Perform Chi-square test manually
# Calculate the row and column sums
row_sums <- rowSums(table_data)
col_sums <- colSums(table_data)
total <- sum(table_data)

# Calculate expected frequencies
expected <- outer(row_sums, col_sums) / total

# Calculate the Chi-square statistic
observed_chi_square <- sum((table_data - expected)^2 / expected)

# Degrees of freedom
df <- (nrow(table_data) - 1) * (ncol(table_data) - 1)

# Critical value for alpha = 0.05
alpha <- 0.05
critical_value <- qchisq(1 - alpha, df)

# p-value for the observed chi-square statistic
p_value <- 1 - pchisq(observed_chi_square, df)

# a)
cat("Critical chi-square score (alpha = 0.05):", critical_value, "\n")
# b)
cat("Observed chi-square score:", observed_chi_square, "\n")
# c)
cat("p-value:", p_value, "\n")

# d)
# Conclusion based on the p-value
if(p_value < alpha) {
  cat("Conclusion: Reject the null hypothesis\n")
} else {
  cat("Conclusion: Fail to reject the null hypothesis\n")
}

# 3.
# Read the dataset for two-sample t-test
data <- read.csv("C:/Users/navee/Documents/STAM 200/Quiz3_DataSet1_2.csv")

# Print the first few rows of the data to understand its structure
print(head(data))

# Split the data into two groups: Teenagers and Adults
teenagers <- subset(data, Generation == 'Teenager')$Sugar
adults <- subset(data, Generation == 'Adult')$Sugar

# Perform a two-sample t-test
t_test_result <- t.test(teenagers, adults)

# Extract the necessary information from the t-test result
# a)
mean_diff <- abs(mean(teenagers) - mean(adults))  # Mean difference
# b)
t_score <- t_test_result$statistic  # t-score
# c)
p_value <- t_test_result$p.value  # p-value
# d)
critical_t <- qt(0.975, df = t_test_result$parameter)  # Critical t-score for alpha = 0.05 (two-tailed)

# Print the results of the t-test
print(paste("Mean difference: ", mean_diff))
print(paste("Observed t-score: ", t_score))
print(paste("p-value: ", p_value))
print(paste("Critical t-score: ", critical_t))
