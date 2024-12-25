# 1.
# Load data for winning times
winning_times <- read.csv("C:\\Users\\navee\\Documents\\STAM 200\\Week 7\\Winning_Times_Marathon.csv")
# Subset data for the year 1968
times_1968 <- subset(winning_times, year == 1968)
# Extract winning times for males and females in 1968
male_time_1968 <- times_1968$minutes[times_1968$sex == 'male']
female_time_1968 <- times_1968$minutes[times_1968$sex == 'female']
# Print the winning times
print(male_time_1968)
print(female_time_1968)

# 2.
# Load the marathon times data
marathon_times <- read.csv("C:\\Users\\navee\\Documents\\STAM 200\\Week 7\\marathon_times.csv", encoding = 'latin1')
# Subset data for men and women under 30 years old
men_u30 <- subset(marathon_times, gender == 'M' & age < 30)
women_u30 <- subset(marathon_times, gender == 'W' & age < 30)
# Ensure the 'officialTime_Minutes' column is numeric
men_u30$officialTime_Minutes <- as.numeric(men_u30$officialTime_Minutes)
women_u30$officialTime_Minutes <- as.numeric(women_u30$officialTime_Minutes)
# Remove any NA values
men_u30 <- men_u30[!is.na(men_u30$officialTime_Minutes), ]
women_u30 <- women_u30[!is.na(women_u30$officialTime_Minutes), ]
# Create histogram for women under 30
hist(women_u30$officialTime_Minutes, 
     main = "Histogram of Finishing Times for Women Under 30 (2019)", 
     xlab = "Official Time (Minutes)", 
     ylab = "Frequency", 
     col = "blue", 
     border = "black")
# Create histogram for men under 30
hist(men_u30$officialTime_Minutes, 
     main = "Histogram of Finishing Times for Men Under 30 (2019)", 
     xlab = "Official Time (Minutes)", 
     ylab = "Frequency", 
     col = "red", 
     border = "black")

# 3.
# Load the necessary datasets
winning_times <- read.csv("C:\\Users\\navee\\Documents\\STAM 200\\Week 7\\Winning_Times_Marathon.csv") 
marathon_times <- read.csv("C:\\Users\\navee\\Documents\\STAM 200\\Week 7\\marathon_times.csv", encoding = 'latin1')
# Extract winning times for 1968
times_1968 <- subset(winning_times, year == 1968)
male_time_1968 <- times_1968$minutes[times_1968$sex == 'male']
female_time_1968 <- times_1968$minutes[times_1968$sex == 'female']
# Subset data for men and women under 30 years old
men_u30 <- subset(marathon_times, gender == 'M' & age < 30)
women_u30 <- subset(marathon_times, gender == 'W' & age < 30)
# Ensure the 'officialTime_Minutes' column is numeric
men_u30$officialTime_Minutes <- as.numeric(men_u30$officialTime_Minutes)
women_u30$officialTime_Minutes <- as.numeric(women_u30$officialTime_Minutes)
# Remove any NA values that might cause issues
men_u30 <- men_u30[!is.na(men_u30$officialTime_Minutes), ]
women_u30 <- women_u30[!is.na(women_u30$officialTime_Minutes), ]
# Calculate percentages of racers beating 1968 winning times
men_beating_1968 <- subset(men_u30, officialTime_Minutes < male_time_1968)
women_beating_1968 <- subset(women_u30, officialTime_Minutes < female_time_1968)
men_percent <- (length(men_beating_1968$officialTime_Minutes) / length(men_u30$officialTime_Minutes)) * 100
women_percent <- (length(women_beating_1968$officialTime_Minutes) / length(women_u30$officialTime_Minutes)) * 100
# Print percentages
cat("Percent of men under 30 beating 1968 winning time:", men_percent, "%\n")
cat("Percent of women under 30 beating 1968 winning time:", women_percent, "%\n")

# 4.
# Calculate mean and standard deviation for each category
mean_men <- mean(men_u30$officialTime_Minutes)
mean_women <- mean(women_u30$officialTime_Minutes)
sd_men <- sd(men_u30$officialTime_Minutes)
sd_women <- sd(women_u30$officialTime_Minutes)
# Print mean and standard deviation
print(mean_men)
print(mean_women)
print(sd_men)
print(sd_women)

# 6.
# a)
# Calculate the probability of beating the 1968 winning time
prob_men <- pnorm(male_time_1968, mean_men, sd_men)
prob_women <- pnorm(female_time_1968, mean_women, sd_women)
# Calculate the race time for the fastest 10% of racers
fastest_men <- qnorm(0.10, mean_men, sd_men)
fastest_women <- qnorm(0.10, mean_women, sd_women)
# Print probabilities and race times
print(prob_men)
print(prob_women)
print(fastest_men)
print(fastest_women)
# b)
# Calculate actual percentages again for clarity
men_percent_actual <- (length(men_beating_1968$officialTime_Minutes) / length(men_u30$officialTime_Minutes)) * 100
women_percent_actual <- (length(women_beating_1968$officialTime_Minutes) / length(women_u30$officialTime_Minutes)) * 100
# Print actual percentages
print(men_percent_actual)
print(women_percent_actual)
