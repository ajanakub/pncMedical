### This script will contain basic R functionality, written by Bukola
###
### Ellyn Butler
### September 22, 2020

### Completed by:  Bukola Ajanaku
### Date: October 22, 2020

set.seed(30)

# Load the R library, ggplot2
library(ggplot2)
library("MASS")

# Create a dataframe with NAs in every cell, 100 rows, and columns with the
# following names: bblid,sex,age,cognition.

myDataFrame <- data.frame(matrix(ncol = 4, nrow = 100))
x <- c("bblid", "sex", "age", "cognition")
colnames(myDataFrame) <- x

# Generate 100 random integers, without replacement, between 10,000 and 20,000
# to serve as bblids. Put these integers into the bblid column.
myDataFrame$bblid <- sample(10000:20000, 100)

# Simulate 100 ages and 100 cognition scores from a bivariate normal distribution
# with rho=.7, and put these values into their respective columns in the dataframe
# thoughts:
rho <- 0.7
mu1 <- 1; s1 <- 8
mu2 <- 1; s2 <- 10
N <- 100
mu <- c(mu1,mu2)
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2), 2)
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma )

myDataFrame$age <- bvn1[,1]
myDataFrame$cognition <- bvn1[,2]

# Rescale the age values so mean=15 and variance=6.
myDataFrame$age <- myDataFrame$age / sd(myDataFrame$age)
myDataFrame$age <- myDataFrame$age * sqrt(6)
myDataFrame$age <- myDataFrame$age + 15 - mean(myDataFrame$age)

# Check if any age values are below zero. How many are there?
## Did not have any values below zero, funny enough.
sum(myDataFrame$age < 0)
## I got a sum of 0.

# For any age values that are below zero, replace that age with zero.
## would have used something like this if i did:
## myDataFrame$age <- replace(myDataFrame$age, myDataFrame$age < 0, 0)

# Create two ggplots: a histogram of ages, and a histogram of cognition. Use
# non-default themes and colors (explore!).
p1 <- ggplot(myDataFrame, aes(x=age)) + geom_histogram(binwidth=3, color = 'aquamarine4', fill = 'aquamarine')
p2 <- ggplot(myDataFrame, aes(x=cognition)) + geom_histogram(binwidth=2, color = 'red', fill = 'pink')
# Calculate the correlation between age and cognition. Does it look familiar?
cor(myDataFrame$age,myDataFrame$cognition)
## = 0.6525141

# Now assign the first 50 rows in your dataframe to be female, and the latter
# 50 to be male. Make sure your sex variable is coded as a factor.
myDataFrame[1:50,"sex"] <- 'female'
myDataFrame[51:100,"sex"] <- 'male'
myDataFrame$sex <- as.factor(myDataFrame$sex)

# Create a scatterplot with age on the x-axis, and cognition on the y-axis.
#use link to scatterplot using ggplot: http://www.sthda.com/english/wiki/ggplot2-scatter-plots-quick-start-guide-r-software-and-data-visualization
p3 <- ggplot(myDataFrame, aes(x=age, y=cognition)) + geom_point()

# Color your points by sex, and plot a regression line per sex.
p3 <- ggplot(myDataFrame, aes(x=age, y=cognition, color=sex)) + geom_point() + geom_smooth(method=lm, se=FALSE, fullrange = TRUE)

# Modifying the graphs, aesthetically.
p1 <- p1 + ggtitle("A Histogram of Ages") + xlab("Age") + ylab("Values") +
theme(plot.title = element_text(color="cyan4", size=14, face = "bold")) +
theme(plot.title = element_text(hjust = 0.5),
axis.title.x = element_text(color="cyan4", size= 12, face = "italic"),
axis.title.y = element_text(color="cyan4", size= 12, face = "italic")
)

p2 <- p2 + ggtitle("A Histogram of Cognition") + xlab("Cognition") + ylab("Values") +
theme(plot.title = element_text(color="hotpink4", size=14, face = "bold")) +
theme(plot.title = element_text(hjust = 0.5),
axis.title.x = element_text(color="hotpink3", size= 12, face = "italic"),
axis.title.y = element_text(color="hotpink3", size= 12, face = "italic")
)

p3 <- p3 + ggtitle("Cognition by Age") + xlab("Age") + ylab("Cognition") +
theme(plot.title = element_text(color="mediumorchid1", size= 16, face = "bold", hjust = 0.5),
axis.title.x = element_text(color="plum3", size= 12, face = "bold.italic"),
axis.title.y = element_text(color="plum3", size= 12, face = "bold.italic"))

# Export all of your plots to one pdf, with each plot on a different page.
pdf(file= "/Users/bukola/Documents/PennBBL/myDataFrameExercise.pdf",
    width = 4,
    height = 4) *make sure to check width and height*
p1
p2
p3
dev.off()

## for personal use: remember that gridExtra and ggpubr export onto one page
