# Set working directory
setwd("C:/Users/navee/Documents/STAM 200/Week 4")

# Read the data
MyData <- read.csv("fertility_data.csv")
head(MyData)

# Subset for quintile 1 (Poorest) - 1990-2005
quintile1olderperiod <- subset(MyData, quintile == "Q1 (Poorest)" & period == "1990-2005")

# Calculate statistics for quintile 1 - 1990-2005
quintile1_stats <- c("Quintile 1 (Poorest)",
                     round(mean(quintile1olderperiod$fertility), 2),    # Round to 2 decimal places
                     round(median(quintile1olderperiod$fertility), 2),  # Round to 2 decimal places
                     round(sd(quintile1olderperiod$fertility), 2))      # Round to 2 decimal places

quintile1_stats

# Subset for quintile 2 - 1990-2005
quintile2olderperiod <- subset(MyData, quintile == "Q2" & period == "1990-2005")

# Calculate statistics for quintile 2 - 1990-2005
quintile2_stats <- c("Quintile 2",
                     round(mean(quintile2olderperiod$fertility), 2),    # Round to 2 decimal places
                     round(median(quintile2olderperiod$fertility), 2),  # Round to 2 decimal places
                     round(sd(quintile2olderperiod$fertility), 2))      # Round to 2 decimal places

quintile2_stats

# Subset for quintile 3 - 1990-2005
quintile3olderperiod <- subset(MyData, quintile == "Q3" & period == "1990-2005")

# Calculate statistics for quintile 3 - 1990-2005
quintile3_stats <- c("Quintile 3",
                     round(mean(quintile3olderperiod$fertility), 2),    # Round to 2 decimal places
                     round(median(quintile3olderperiod$fertility), 2),  # Round to 2 decimal places
                     round(sd(quintile3olderperiod$fertility), 2))      # Round to 2 decimal places

quintile3_stats

# Subset for quintile 4 - 1990-2005
quintile4olderperiod <- subset(MyData, quintile == "Q4" & period == "1990-2005")

# Calculate statistics for quintile 4 - 1990-2005
quintile4_stats <- c("Quintile 4",
                     round(mean(quintile4olderperiod$fertility), 2),    # Round to 2 decimal places
                     round(median(quintile4olderperiod$fertility), 2),  # Round to 2 decimal places
                     round(sd(quintile4olderperiod$fertility), 2))      # Round to 2 decimal places

quintile4_stats

# Subset for quintile 5 (Richest) - 1990-2005
quintile5olderperiod <- subset(MyData, quintile == "Q5 (Richest)" & period == "1990-2005")

# Calculate statistics for quintile 5 - 1990-2005
quintile5_stats <- c("Quintile 5 (Richest)",
                     round(mean(quintile5olderperiod$fertility), 2),    # Round to 2 decimal places
                     round(median(quintile5olderperiod$fertility), 2),  # Round to 2 decimal places
                     round(sd(quintile5olderperiod$fertility), 2))      # Round to 2 decimal places

quintile5_stats

# Subset for quintile 1 (Poorest) - 2006-2019
quintile1newerperiod <- subset(MyData, quintile == "Q1 (Poorest)" & period == "2006-2019")

# Calculate statistics for quintile 1 - 2006-2019
quintile1_stats_new <- c("Quintile 1 (Poorest) - 2006-2019",
                         round(mean(quintile1newerperiod$fertility), 2),    # Round to 2 decimal places
                         round(median(quintile1newerperiod$fertility), 2),  # Round to 2 decimal places
                         round(sd(quintile1newerperiod$fertility), 2))      # Round to 2 decimal places

quintile1_stats_new

# Subset for quintile 2 - 2006-2019
quintile2newerperiod <- subset(MyData, quintile == "Q2" & period == "2006-2019")
                               
# Calculate statistics for quintile 2 - 2006-2019
quintile2_stats_new <- c("Quintile 2 - 2006-2019",
                         round(mean(quintile2newerperiod$fertility), 2),    # Round to 2 decimal places
                         round(median(quintile2newerperiod$fertility), 2),  # Round to 2 decimal places
                         round(sd(quintile2newerperiod$fertility), 2))      # Round to 2 decimal places

quintile2_stats_new

# Subset for quintile 3 - 2006-2019
quintile3newerperiod <- subset(MyData, quintile == "Q3" & period == "2006-2019")

# Calculate statistics for quintile 3 - 2006-2019
quintile3_stats_new <- c("Quintile 3 - 2006-2019",
                         round(mean(quintile3newerperiod$fertility), 2),    # Round to 2 decimal places
                         round(median(quintile3newerperiod$fertility), 2),  # Round to 2 decimal places
                         round(sd(quintile3newerperiod$fertility), 2))      # Round to 2 decimal places

quintile3_stats_new

# Subset for quintile 4 - 2006-2019
quintile4newerperiod <- subset(MyData, quintile == "Q4" & period == "2006-2019")

# Calculate statistics for quintile 4 - 2006-2019
quintile4_stats_new <- c("Quintile 4 - 2006-2019",
                         round(mean(quintile4newerperiod$fertility), 2),    # Round to 2 decimal places
                         round(median(quintile4newerperiod$fertility), 2),  # Round to 2 decimal places
                         round(sd(quintile4newerperiod$fertility), 2))      # Round to 2 decimal places

quintile4_stats_new

# Subset for quintile 5 (Richest) - 2006-2019
quintile5newerperiod <- subset(MyData, quintile == "Q5 (Richest)" & period == "2006-2019")

# Calculate statistics for quintile 5 - 2006-2019
quintile5_stats_new <- c("Quintile 5 (Richest) - 2006-2019",
                         round(mean(quintile5newerperiod$fertility), 2),    # Round to 2 decimal places
                         round(median(quintile5newerperiod$fertility), 2),  # Round to 2 decimal places
                         round(sd(quintile5newerperiod$fertility), 2))      # Round to 2 decimal places

quintile5_stats_new

# Calculate the absolute effect size for quintile 1
abs_effect_size_q1 <- abs(mean(quintile1newerperiod$fertility) - mean(quintile1olderperiod$fertility))

# Calculate the absolute effect size for quintile 2
abs_effect_size_q2 <- abs(mean(quintile2newerperiod$fertility) - mean(quintile2olderperiod$fertility))

# Calculate the absolute effect size for quintile 3
abs_effect_size_q3 <- abs(mean(quintile3newerperiod$fertility) - mean(quintile3olderperiod$fertility))

# Calculate the absolute effect size for quintile 4
abs_effect_size_q4 <- abs(mean(quintile4newerperiod$fertility) - mean(quintile4olderperiod$fertility))

# Calculate the absolute effect size for quintile 5
abs_effect_size_q5 <- abs(mean(quintile5newerperiod$fertility) - mean(quintile5olderperiod$fertility))

# Create a dataframe to store the absolute effect sizes for each quintile
abs_effect_sizes <- data.frame(
  Quintile = c("Q1", "Q2", "Q3", "Q4", "Q5"),
  Absolute_Effect_Size = c(round(abs_effect_size_q1, 2), round(abs_effect_size_q2, 2), round(abs_effect_size_q3, 2), round(abs_effect_size_q4, 2), round(abs_effect_size_q5, 2))
)

abs_effect_sizes
