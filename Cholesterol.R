# Set the working directory to where the file is located
setwd("C:/Users/navee/Documents/STAM 200")

# Load the data
cholesterol_data <- read.csv("Cholesterol.csv")

# Check for missing or non-finite values in the cholesterol column
cholesterol_data$cholesterol <- as.numeric(cholesterol_data$cholesterol)
cholesterol_data <- cholesterol_data[!is.na(cholesterol_data$cholesterol) & is.finite(cholesterol_data$cholesterol), ]

# 1. Create the boxplot
boxplot(cholesterol_data$cholesterol,
        main = "Boxplot of Total Cholesterol Levels in Community Members",
        ylab = "Total Cholesterol (mmol/L)",
        col = "skyblue")

# Add a dashed horizontal line at the threshold value of 5.2 mmol/L
abline(h = 5.2, lty = 2, col = "red")

# Adding figure caption
mtext("Figure 1. Blood-cholesterol levels for a study of 58 patients in Canada, combined to show average levels. The horizontal dashed line indicates the threshold of 5.2 mmol/L for having ‘high cholesterol’ as per the Canadian Health guidelines.", side = 1, line = 4, cex = 0.75)

# 5. Perform the one-sample t-test and statistical conclusion
t_test_result <- t.test(cholesterol_data$cholesterol, mu = 5.2, alternative = "greater")

# Extract t-value and p-value
t_value <- t_test_result$statistic
p_value <- t_test_result$p.value

# Extract critical t-value
alpha <- 0.05
critical_t <- qt(1 - alpha, df = t_test_result$parameter)

# Conclusion
if(p_value < alpha) {
  conclusion <- "reject"
} else {
  conclusion <- "fail to reject"
}

stat_conclusion <- paste("We", conclusion, "the null hypothesis because the observed t-score (", round(t_value, 2), ")",
                         ifelse(conclusion == "reject", "is greater than", "is not greater than"),
                         "the critical t-score (", round(critical_t, 2), ").")
print(stat_conclusion)

# 6. Scientific conclusion
sci_conclusion <- paste("The mean total cholesterol level of the community is significantly above the threshold value of 5.2 mmol/L (t =",
                        round(t_value, 2), ", df =", round(t_test_result$parameter, 2), ", P =", format(p_value, scientific = TRUE), ").")
print(sci_conclusion)

# 8. Sketching the t-distribution
x <- seq(-7, 7, length = 100)
y <- dt(x, df = t_test_result$parameter)

plot(x, y, type = "l", lwd = 2, col = "blue", ylab = "Density", xlab = "t-value", main = "t-Distribution with Observed and Critical t-scores")
abline(v = c(t_value, critical_t), col = c("red", "green"), lty = 2)

# Shading the areas
polygon(c(x[x >= critical_t], critical_t, x[1]), c(y[x >= critical_t], 0, 0), col = rgb(0, 1, 0, 0.5))
polygon(c(x[x >= t_value], t_value, x[1]), c(y[x >= t_value], 0, 0), col = rgb(1, 0, 0, 0.5))

legend("topright", legend = c("Observed t-score", "Critical t-score"), col = c("red", "green"), lty = 2)
