# Set the working directory to the specified path
setwd("C:/Users/navee/Documents/STAM 200")

# Read the CSV file into a data frame without specifying column classes
MyData = read.csv("week03_sample_data.csv")

# Display the first few rows of the data frame
head(MyData)

# Display the structure of the data frame
str(MyData)

# Read the CSV file again, this time specifying column classes
MyData = read.csv("week03_sample_data.csv", colClasses = c('factor', 'factor', 'numeric', 'numeric'))

# Display the structure of the data frame to confirm column classes
str(MyData)

# Create subsets of the data for each diet type
FirstDiet <- subset(MyData, diet == 1)
SecondDiet <- subset(MyData, diet == 2)
ThirdDiet <- subset(MyData, diet == 3)
FourthDiet <- subset(MyData, diet == 4)

# Calculate and display the mean weight for each diet type
firstmean = mean(FirstDiet$weight)
firstmean
secondmean = mean(SecondDiet$weight)
secondmean
thirdmean = mean(ThirdDiet$weight)
thirdmean
fourthmean = mean(FourthDiet$weight)
fourthmean

# Define the x and y variables for plotting (time and weight for the third diet)
x = ThirdDiet$time
y = ThirdDiet$weight

# Plot the data with appropriate labels for the axes
plot(x, y, xlab = 'Time (days)', ylab = 'Weight (grams)')

# Calculate the percentage change between the first and second diet mean weights
changebetween1and2 = 100 * (secondmean - firstmean) / firstmean
changebetween1and2

# Calculate the percentage change between the second and third diet mean weights
changebetween2and3 = 100 * (thirdmean - secondmean) / secondmean
changebetween2and3

# Calculate the percentage change between the third and fourth diet mean weights
changebetween3and4 = 100 * (fourthmean - thirdmean) / thirdmean
changebetween3and4
