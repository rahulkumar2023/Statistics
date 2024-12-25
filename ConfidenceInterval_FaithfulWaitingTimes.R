# Load the dataset
data("faithful")

# Set the sample size and confidence level
sample_size <- 50
confidence_level <- 0.95

# Sample 50 values from the 'waiting' column
data_sample <- sample(faithful$waiting, sample_size)

# Calculate the mean of the sample
data_mean <- mean(data_sample)

# Calculate the standard deviation of the sample
sd_sample <- sd(data_sample)

# Calculate the size of the sample
size_sample <- length(data_sample)

# Calculate the standard error of the sample
error_sample <- sd_sample / sqrt(size_sample)

# Determine the critical t-score for a 95% confidence interval
critical_t_score <- qt(1 - (1 - confidence_level) / 2, df = size_sample - 1)

# Calculate the margin of error
error_margin <- critical_t_score * error_sample

# Calculate the lower and upper confidence intervals
lci <- data_mean - error_margin
uci <- data_mean + error_margin

# Output the results
cat(sprintf("Mean: %.2f\n", data_mean))
cat(sprintf("95%% CI: [%.2f, %.2f]\n", lci, uci))

# Calculate the population mean
population_mean <- mean(faithful$waiting)

# Print the population mean
cat(sprintf("Population Mean: %.2f\n", population_mean))

# Check if it falls within the 95% CI
falls_within_ci <- population_mean >= lci && population_mean <= uci
cat(sprintf("Population mean falls within 95%% CI: %s\n", ifelse(falls_within_ci, "Yes", "No")))
