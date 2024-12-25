# Load the data
data <- read.csv('C:/Users/navee/Documents/STAM 200/PolticialParty.csv')

# 1.
contingency_table <- table(data$race, data$party)
contingency_table_prop <- prop.table(contingency_table)
contingency_table_prop <- round(contingency_table_prop, 3)

# Print the contingency table and proportions
print("Contingency Table:")
print(contingency_table)

# Compute marginal distributions for race/ethnicity
race_totals <- margin.table(contingency_table, 1)
total <- sum(contingency_table)
race_proportions <- race_totals / total

# Print marginal distributions
print("Marginal Distributions (Proportions) for Race/Ethnicity:")
print(round(race_proportions, 3))

# Compute marginal distributions for political affiliation
party_totals <- margin.table(contingency_table, 2)
party_proportions <- party_totals / total

# Print marginal distributions
print("Marginal Distributions (Proportions) for Political Affiliation:")
print(round(party_proportions, 3))

# 2.
print("The appropriate statistical test to determine whether political affiliation and race/ethnicity are independent is the Chi-square test of independence.")

# 3.
# a)
print("Null Hypothesis (H0): Political affiliation and race/ethnicity are independent.")
print("Alternative Hypothesis (H1): Political affiliation and race/ethnicity are not independent.")
# b)
print("This is a two-tailed test because we are checking for any kind of association between the variables, not just a specific direction.")

# 4.
chi_square_test <- chisq.test(contingency_table)
expected <- chi_square_test$expected

# Print the expected frequencies
print("Expected Frequencies:")
print(expected)

# Specific expected frequencies
# a)
expected_caucasian_republican <- expected["Caucasian", "Republican"]
# b)
expected_black_democrat <- expected["Black", "Democrat"]
# c)
expected_asian_american_independent <- expected["Asian American", "Independent"]
# d)
expected_hispanic_democrat <- expected["Hispanic", "Democrat"]

print(paste("Expected frequency for Caucasian Republicans:", round(expected_caucasian_republican, 2)))
print(paste("Expected frequency for Black Democrats:", round(expected_black_democrat, 2)))
print(paste("Expected frequency for Asian American Independents:", round(expected_asian_american_independent, 2)))
print(paste("Expected frequency for Hispanic Democrats:", round(expected_hispanic_democrat, 2)))

# 5.
df <- (nrow(contingency_table) - 1) * (ncol(contingency_table) - 1)
# a)
alpha <- 0.05
critical_value <- qchisq(1 - alpha, df)

# Print the critical chi-square statistic
print(paste("Critical chi-square value:", round(critical_value, 2)))

# b)
# Sketch the chi-square distribution
x <- seq(0, 30, by=0.1)
y <- dchisq(x, df)
plot(x, y, type="l", main="Chi-square Distribution", xlab="Chi-square value", ylab="Density")
abline(v=critical_value, col="red", lty=2)
text(critical_value + 0.5, max(y)/2, paste("Critical value =", round(critical_value, 2)), col="red")

# 6.
if (chi_square_test$p.value < alpha) {
  stat_conclusion <- "Reject the null hypothesis."
  reasoning <- "Based on the Chi-squared test for independence, with a p-value less than 2.26e-16, which is much smaller than the chosen significance level of 0.05, we reject the null hypothesis. This result provides strong evidence of a significant association between political affiliation and race/ethnicity."
} else {
  stat_conclusion <- "Fail to reject the null hypothesis."
  reasoning <- "The p-value is greater than the alpha level (0.05), indicating no statistically significant association between political affiliation and race/ethnicity."
}

print(stat_conclusion)
print(reasoning)

# 7.
if (chi_square_test$p.value < alpha) {
  sci_conclusion <- "There is a significant association between political affiliation and race/ethnicity."
  sci_reasoning <- "Based on the results of the chi-square test (X-squared = 1325.2, df = 9, p-value < 2.2e-16), we reject the null hypothesis and conclude that there is a significant association between political affiliation and race/ethnicity. This means that political affiliation varies significantly among different racial and ethnic groups. For example, there are significantly more Black Democrats (1032) than expected under the assumption of independence (561.27). This significant association indicates that race/ethnicity influences political affiliation, suggesting that different racial/ethnic groups have distinct political preferences."
} else {
  sci_conclusion <- "There is no significant association between political affiliation and race/ethnicity."
  sci_reasoning <- "This indicates that political affiliation does not vary by race/ethnicity."
}

print(sci_conclusion)
print(sci_reasoning)

# 8.
observed_black_democrat <- contingency_table["Black", "Democrat"]
comparison <- paste("Observed count for Black Democrats:", observed_black_democrat, "Expected count:", round(expected_black_democrat, 2))

print(comparison)

if (observed_black_democrat > expected_black_democrat) {
  impact <- paste("The observed count for Black Democrats is", observed_black_democrat, 
                  "which is substantially higher than the expected count of", round(expected_black_democrat, 2), 
                  ". This comparison indicates that Black individuals are more likely to affiliate with the Democratic Party than would be expected if political affiliation were independent of race/ethnicity. If the proportion of Black individuals in the US increases, the number of Black Democrats may also increase, potentially influencing voting results. This observed deviation suggests that demographic shifts, such as an increase in the Black population, could lead to a greater number of Democratic voters, thereby affecting future election outcomes.")
} else {
  impact <- "If the proportion of people identifying as Black in the US increases, it might not have a significant impact on the number of Black Democrats."
}

print(impact)
