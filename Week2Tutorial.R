# Group members: Rahul Kumar, Grace Lawford, Molly Roche, Niko Piller, Wenxin Yin

# Chart position of each of the 13 albums that the Tragically Hip released prior to 2000 and from 2000 - 2016
Before2000=c(13,1,1,1,1,1)
After2000=c(1,2,1,2,1,3,1)

# Lowest spot of a chart album of The Tragically Hip for an album released before 2000
max(Before2000)

# Difference between the average chart album value of The Tragically Hip before 2000 and the average chart album value of The Tragically Hip after 2000
meandifference = mean(Before2000) - mean(After2000)

# The following line allows the value of the difference to be printed in the console window
print(meandifference)

# Since the value of the variable of meandifference is positive, this means that the average chart album value of The Tragically Hip before 2000 was higher than that of the average chart album value for The Tragically Hip between 2000 - 2016. Therefore, The Tragically Hip was more popular after 2000 as it had on average, a lower spot on charts after 2000 in comparison to before 2000.