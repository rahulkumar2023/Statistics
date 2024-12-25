# 1. Calculate the interquartile range (IQR) for the Happiness column
setwd("C:/Users/navee/Documents/STAM 200")

# Read the CSV file
data <- read.csv("Quiz1_DataSet2_9.csv")

# Calculate the IQR for the Happiness column
happiness_IQR <- IQR(data$Happiness)

# Print the IQR value
happiness_IQR

# 2. Cube each entry in the original vector and calculate the median
# Step 1: Create the vector
original_vector <- c(7.3, 22.9, 14.7, 20, 7.9, 24.5)

# Step 2: Cube each entry in the original vector
cubed_vector <- original_vector^3

# Step 3: Calculate the median of the new vector
median_cubed <- median(cubed_vector)

# Print the median value
median_cubed

# 3. Find the median of the vector and create a new vector that has the median subtracted from each entry
# Step 1: Create the vector
original_vector <- c(20, 8.4, 17.5, 15.8, 14.6, 21.6, 24.5, 2.6, 19.2, 4.6, 1.6, 18.5)

# Step 2: Find the median of the vector
median_value <- median(original_vector)

# Step 3: Subtract the median from each entry in the original vector to create a new vector
new_vector <- original_vector - median_value

# Step 4: Determine the minimum value of the new vector
min_value <- min(new_vector)

# Print the median value and the minimum value of the new vector
median_value
min_value

# 4. Calculate the mean number of hours of sleep for all students and for those that drink only lattes
setwd("C:/Users/navee/Documents/STAM 200")

# Import the CSV file
data <- read.csv("Quiz1_DataSet1_1.csv")

# Calculate the mean number of hours of sleep for all students
mean_sleep_all <- mean(data$HoursSleep)

# Calculate the mean number of hours of sleep for the subset of students that drink only lattes
latte_drinkers <- subset(data, Type == "latte")
mean_sleep_latte <- mean(latte_drinkers$HoursSleep)

# Print the mean values
mean_sleep_all
mean_sleep_latte


