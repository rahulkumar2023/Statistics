# 1. Loading and analyzing slang usage data

# Load the dataset
MyData <- read.csv("C:\\Users\\navee\\Documents\\STAM 200\\Slang_Usage_Data.csv")
print(MyData)

# a) Determine the most common political ideology among GenerationX
# Filter data for GenerationX
gen_x_data <- MyData[MyData$Generation == "GenerationX", ]
# Find the most common political ideology in GenerationX
most_common_ideology_gen_x <- sort(table(gen_x_data$PoliticalIdeology), decreasing = TRUE)[1]
most_common_ideology_gen_x_name <- names(most_common_ideology_gen_x)
print(most_common_ideology_gen_x_name)

# b) Calculate the proportion of GenerationX who use slang infrequently (≤ 3)
# Filter GenerationX data for those who use slang infrequently (≤ 3)
gen_x_slang_infrequent <- gen_x_data[gen_x_data$SlangUse <= 3, ]
# Calculate the proportion of GenerationX with infrequent slang usage
proportion_gen_x_infrequent <- nrow(gen_x_slang_infrequent) / nrow(gen_x_data)
print(proportion_gen_x_infrequent)

# c) Create a boxplot of slang usage by generation
# Generate the boxplot for slang usage by generation
boxplot(SlangUse ~ Generation, data = MyData, main = "Slang Usage by Generation",
        xlab = "Generation", ylab = "Slang Usage", las = 2)

# 2. Analyzing GPA data by socioeconomic status for Business students

# Load the dataset
MyData <- read.csv("C:\\Users\\navee\\Documents\\STAM 200\\Business_GPA_Socioeconomic_Data.csv")
# Filter data for Business students
business_data <- MyData[MyData$Program == "Business", ]
# Ensure SocioeconomicStatus is treated as a factor with specific levels
business_data$SocioeconomicStatus <- factor(business_data$SocioeconomicStatus, levels = c("low", "medium", "high"))
# Create a boxplot for Last Year GPA by Socioeconomic Status for Business students
boxplot(LastYearGPA ~ SocioeconomicStatus, data = business_data, 
        main = "Last Year GPA by Socioeconomic Status for Business Students",
        xlab = "Socioeconomic Status", ylab = "Last Year GPA",
        las = 2, ylim = c(1.0, 4.0))

# 3. Creating a boxplot of earnings by career type with sample data

# Create a sample dataset
set.seed(123)  # for reproducibility
MyData <- data.frame(
  Earnings = c(rnorm(50, mean = 300, sd = 50), rnorm(50, mean = 350, sd = 50), rnorm(50, mean = 400, sd = 50)),
  Career = rep(c("Engineer", "Doctor", "Artist"), each = 50),
  Age = sample(25:60, 150, replace = TRUE)
)
# Display the first few rows of the dataset
head(MyData)

# Create a boxplot of Earnings by Career
boxplot(MyData$Earnings ~ MyData$Career, ylab = 'Amount Earned', xlab = 'Career Type', ylim = c(0, 550), col = 'blue')

# 4. Creating a scatter plot with a vertical line at x = 12

# Generate example data
set.seed(123)
x <- rnorm(100, mean = 10, sd = 5)
y <- rnorm(100, mean = 50, sd = 10)

# Create scatter plot
plot(x, y, main = "Scatter Plot with Vertical Line at x = 12", 
     xlab = "X-axis", ylab = "Y-axis", pch = 19, col = "blue")

# Add a vertical orange line at x = 12
abline(v = 12, col = "orange")
