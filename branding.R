# Set working directory and read data
setwd("C:/Users/navee/Documents/STAM 200")
MyData <- read.csv("branding.csv")

# Subset data for the "Economy" issue
economy <- subset(MyData, issue == "Economy")

# Create contingency table for year and type
economyTable <- table(economy$year, economy$type)

# Print the contingency table
print(economyTable)

# Determine the year with the most "Economy" branding for each type
brandedelection <- which.max(economyTable[, "Press Release Election"])
brandednonelection <- which.max(economyTable[, "Press Release Nonelection"])
brandedplatform <- which.max(economyTable[, "Platform"])

year <- rownames(economyTable)
brandedelectionyear <- year[brandedelection]
brandednonelectionyear <- year[brandednonelection]
brandedplatformyear <- year[brandedplatform]

# Print the years with the most branding for each type
brandedelectionyear
brandednonelectionyear
brandedplatformyear

# Subset the data for the "Economy" issue
BrandData <- subset(MyData, issue == "Economy")

# First bar plot for counts of different types of branding
counts <- table(BrandData$type)
barplot(counts, main = "Counts of Different Types of Branding", xlab = "Brand Type", ylab = "Count", col = c("orange", "red", "green"), legend = c('Press Release NonElection', 'Platform', 'Press Release Election'))

# Second bar plot for counts of economy branding per type over the years
counts_per_year <- table(BrandData$type, BrandData$year)
barplot(counts_per_year, main = "Liberal Party Economy Branding", xlab = "Year", ylab = "Count of Economy Branding per type", col = c('orange', 'red', 'green'))
legend("topright", legend = c('Press Release NonElection', 'Platform', 'Press Release Election'), col = c('orange', 'red', 'green'), pch = 19)

# Subset the data for the "Platform" type
Platform <- subset(MyData, type == "Platform")

# Create contingency table for issue and year
Table_Issue <- table(Platform$issue, Platform$year)

# Print the contingency table
Table_Issue

# Bar plot for branding words for each election issue
barplot(Table_Issue, beside = TRUE, col = c('pink', 'red', 'yellow', 'blue', 'green'),
        ylim = c(0, 600),
        main = "Branding Words for Each Election Issue (2006 - 2015)",
        xlab = "Year", ylab = "Number of Branding Words")

# Add legend
legend("topright", c('Economy', 'Social Issues', 'Social Services', 
                     'Multiculturalism', 'Security and International Relations'),
       col = c('pink', 'red', 'yellow', 'blue', 'green'), pch = 19, cex = 0.75)


