##############################################################################################################

#set the working directory for file outputs
setwd("C:/Users/User/Documents/NCI_Sem3/Summer Camp")

getwd() # check we have moved

##################################################################
######################## R Introduction  #########################
##################################################################
print("Hello World")

print("Good morning, Welcome to the Data Science Summer Camp")

#####################################################
# Some basic data types in R - numbers and characters
#####################################################

a <- 4

b = 5

answer = "42"

pi <- 3.14159


#########################
# Some basic R operations
#########################
c = a * b

c = c + 10

e = pi*(5^2)

# Can check the data type for these variables
is.numeric(a)

is.character(b)

is.character(answer)

#################################
# Some other R data types - LISTS
# Create a list with 3 variables
#################################
n = c(12, 23, 35)

# print the contents of n, some, all or specific entry
n[1:3]
n
n[2]
n[2:3]

##################################################
# More data types.......................data frame
##################################################

name = c("John", "Mary", "Ed")
age = c(16, 22, 35)
gender = c("M", "F", "M")
grade = c(0,0,0)

cohort = data.frame(name, age, gender, grade)

cohort

cohort$grade

cohort$grade[3] = 100

cohort$grade[1] = 0


##################################################################
######################## Functions in R ##########################
##################################################################

# Define the set_grade function
set_grade <- function(index, raise){
  grade = cohort$age[index] * raise
  return(grade)
}

# Check the grades before calling set_grade
cohort$grade

# Call the set_grade function for John/1 and Mary/2
cohort$grade[1] = set_grade(1,3)
cohort$grade[2] = set_grade(2,3)

# Check the grades after calling set_grade
cohort$grade

for(i in 1:nrow(cohort)) {
  cohort$grade[i] = set_grade(i,3)
}

##################################################################
######################## Another function ########################
# Define the average function
average <- function(number1, number2){
  averageVal = (number1 + number2) / 2
  print(averageVal)
}

average(9,19)



##################################################################
############### Reading and Writing files in R ###################
##################################################################

write.csv(cohort, file="cohort.csv", row.names=F)

new_cohort <- read.csv(file="cohort.csv",head=TRUE,sep=",")

new_cohort




##################################################################
########################   Correlation   #########################
##################################################################
# Draw a simple Scatter Plot
#
x <- c( 1, 2, 3, 4, 5)
y <- c( 1, 2, 1.3, 3.75, 2.25)

# Simple Plot
plot( x, y)

# Plot with axes labelled.....
plot( x, y, main="Quick Example", xlab="X Variable", ylab="Y Variable", col="green" )
#
# Pearsons' Correllation Coefficient (r)
#
cor (x,y) # should get r = 0.6268327
#
#
##################################################################
####################### Building a model #########################
##################################################################

#correl_data <- read.csv(file="CorrelationData.csv",head=TRUE,sep=",")
#

Test_Score = c(17, 13, 12, 15, 16, 14, 16, 16, 18, 19)
Attitude = c(94, 73, 59, 80, 93, 85, 66, 79, 77, 91)
correl_data = data.frame(Test_Score, Attitude)

# Print the contents of the variable correl_data
correl_data   # Display data

# Is there a relationship between student attitude and test score?
cor(correl_data$Test_Score, correl_data$Attitude) # Calculate r = 0.5960948
cor(correl_data$Attitude, correl_data$Test_Score) # Calculate r = 0.5960948
#
#
plot(correl_data$Attitude, correl_data$Test_Score, main="Correlation ", xlab="Attitude ", ylab="Test Score ", pch=19)

# Build a linear model for the correl_data variables
model1 = lm(correl_data$Test_Score ~ correl_data$Attitude)

# Draw the line for the equation
abline(model1, col="red")

# Print a  summary of the model details
summary(model1)



# Example 2 ########################################################
# Number of Cricket Chirps and Temperature - is there a correlation?
####################################################################
chirp_nums = c( 18, 20, 21, 23, 27, 30, 34, 39)
temp_f = c( 57, 60, 64, 65, 68, 71, 74, 77)
plot(temp_f, chirp_nums)
cor(chirp_nums, temp_f)
cor(temp_f, chirp_nums)

#
fit <- lm( chirp_nums ~ temp_f)
plot( temp_f, chirp_nums)
abline(fit, col = "blue")
summary(fit)

##################################################################
################# FINALLY - Save A PLOT to FILE ##################
##################################################################
# Plot an image....Save the image to a file
#
jpeg("SampleChart.jpg")
plot (x) # variable x from above

#plot.default(correl_d$Attitude, correl_d$Test_Score, main="Salkind Data (Test Score vs Attitude)", xlab="Test Score", ylab="Attitude to Tests ")

dev.off()
















##################################################################
############### R has in-built data sets e.g. iris ###############
##################################################################
#
# Iris Dataset from R Library (datasets package - included with R)
#
library(datasets) # import datasets

# Tell the R program you are going to use the iris data set
data(iris)   # in Global Env.....iris variable <Promise>

# What are the dimensions of the iris data set?
dim(iris)

# What is the structure of the iris dataset.............
# ..shows the variable names and the type of data that they contain
str(iris)

# Print the top 6 rows of data
head(iris)
head(iris, 20)

# Any command to print the bottom 6 rows?????


# If we only want the data dsiplayed for one variable e.g. Sepal.Length data
# we use the $ sign.............
iris$Sepal.Length

#
#################################################################
#  Descriptives for Iris Sepal Length variable
#################################################################

mean(iris$Sepal.Length)                    # Mean
median(iris$Sepal.Length)                  # Median
max(iris$Sepal.Length)                     # Maximum
min(iris$Sepal.Length)                     # Minimum
summary(iris)


# Check Iris Sepal data for relationship
cor(iris$Sepal.Length, iris$Petal.Length)                 # r = 0.8717538

plot(iris$Sepal.Length, iris$Petal.Length)

plot(iris$Sepal.Length, iris$Petal.Length, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], main="Edgar Anderson's Iris Data")


plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], main="Edgar Anderson's Iris Data")

pairs(iris[1:4])

pairs(iris[1:4], main = "Edgar Anderson's Iris Data", pch = 22, bg = c("red", "green3", "blue")[unclass(iris$Species)])


