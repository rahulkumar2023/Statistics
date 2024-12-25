data <- read.csv("C:/Users/navee/Documents/STAM 200/SilverMaple.csv")

# 1. Relationship between Tree Age and Trunk Diameter
# Scatterplot to visually inspect the relationship
plot(data$trunkDiameter, data$age, 
     main = "Relationship between Tree Age and Trunk Diameter",
     xlab = "Trunk Diameter (cm)",
     ylab = "Age (years)")

# The relationship between tree age and trunk diameter appears to be positive and linear.

# 2. Null and Alternative Hypotheses
# H0: There is no linear relationship between trunk diameter and tree age (β1 = 0)
# HA: There is a linear relationship between trunk diameter and tree age (β1 ≠ 0)

# 3. Equation of the Linear Regression
# Fit the linear model
model <- lm(age ~ trunkDiameter, data = data)

# Summary of the model
summary(model)

# Extract coefficients
intercept <- coef(model)[1]
slope <- coef(model)[2]
cat("Equation of the linear regression: Age =", intercept, "+", slope, "* Trunk Diameter\n")

# 4. Scatterplot with Regression Line
plot(data$trunkDiameter, data$age, 
     main = "Relationship between Tree Age and Trunk Diameter",
     xlab = "Trunk Diameter (cm)",
     ylab = "Age (years)")
abline(model, col = "red")

# Caption for the scatterplot
# "Figure 1: Scatterplot showing the relationship between trunk diameter and tree age in Silver Maple trees with a fitted linear regression line."

# 5. Statistical Conclusion
# Extract additional statistics
t_value <- summary(model)$coefficients[2, "t value"]
p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
df <- model$df.residual
adjusted_r_squared <- summary(model)$adj.r.squared

# Statistical conclusion based on hypothesis testing
if (p_value < 0.05) {
  cat("Statistical conclusion: The analysis provides strong evidence to reject the null hypothesis (H0). There is a significant positive linear relationship between trunk diameter and tree age for Silver Maple trees (t =", t_value, ", p <", p_value, ", df =", df, ", adjusted R^2 =", adjusted_r_squared, "). This suggests that trunk diameter is a reliable predictor of tree age.\n")
} else {
  cat("Statistical conclusion: The analysis does not provide sufficient evidence to reject the null hypothesis (H0). There is no significant linear relationship between trunk diameter and tree age for Silver Maple trees (t =", t_value, ", p =", p_value, ", df =", df, ", adjusted R^2 =", adjusted_r_squared, "). This suggests that trunk diameter is not a reliable predictor of tree age.\n")
}

# 6. Model Assumptions: Normality and Homoscedasticity
# Shapiro-Wilk test for normality of residuals
shapiro_test <- shapiro.test(resid(model))
print(shapiro_test)

# Residuals vs Fitted plot
plot(fitted(model), resid(model),
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)

# The Shapiro-Wilk normality test returned a W value of 0.99089 and a p-value of 0.9086.
# Since the p-value is much greater than the common alpha level of 0.05, we fail to reject the null hypothesis
# that the residuals are normally distributed. This suggests that the assumption of normality is satisfied.

# The plot of residuals versus fitted values did not show any distinct pattern, and the residuals appear to be
# randomly scattered around the horizontal line at zero. This indicates that the variance of the residuals is
# constant across different levels of fitted values, satisfying the assumption of homoscedasticity.

# Conclusion:
# Based on the Shapiro-Wilk test and the residuals vs. fitted plot, the model assumptions of normality and 
# homoscedasticity appear to be met. The normality of residuals is supported by a high p-value (0.9086) 
# from the Shapiro-Wilk test, and the residuals vs. fitted values plot shows no evidence of non-constant variance. 
# Therefore, this examination of assumptions suggests that the linear regression model is appropriate for the data.

# 7. Scientific Conclusion
# Extract additional statistics
slope <- coef(model)[2]
t_value <- summary(model)$coefficients[2, "t value"]
p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
df <- model$df.residual
adjusted_r_squared <- summary(model)$adj.r.squared

cat("Scientific conclusion: The slope is", slope, "with a t-score of", t_value, ", df =", df, ", p-value =", p_value, ", and adjusted R^2 =", adjusted_r_squared, "\n")

# Scientific conclusion based on the analysis
cat("The scientific conclusion of the test is that there is a significant positive linear relationship between trunk diameter and tree age for Silver Maple trees. The regression model indicates a slope of", slope, "with a t-score of", t_value, ",", df, "degrees of freedom (df), a p-value of less than 0.001, and an adjusted R-squared value of", adjusted_r_squared, ". This suggests that for every centimeter increase in trunk diameter, the tree age increases by approximately", slope, "years. The high t-score and low p-value indicate that this relationship is statistically significant. The adjusted R-squared value of", adjusted_r_squared, "means that about 57% of the variability in tree age is explained by trunk diameter. Therefore, we can be confident that trunk diameter is a reliable predictor of tree age. However, since the adjusted R-squared value is 0.570, this model explains 57% of the variance in tree age, leaving 43% unexplained. While the equation can be used to estimate tree age, it should be understood that there are other factors influencing tree age that are not accounted for by this model.\n")

# 8a. Prediction for a specific trunk diameter
new_diameter <- 57
predicted_age <- intercept + slope * new_diameter
cat("Predicted age for a trunk diameter of 57 cm is", predicted_age, "years\n")

# 8b. Difference between actual and predicted age
actual_age <- 32
difference <- actual_age - predicted_age
cat("The difference between the actual age and predicted age is", difference, "years\n")

# The difference between the actual tree age and the predicted tree age is called the "residual" or "prediction error".
cat("The difference between the actual age and the predicted age is called the residual or prediction error.\n")