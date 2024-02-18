#########################################################################
############# 2nd lesson - RStudio basics ###############################
######## Adela Vrtkova, Martina Litschmannova ###########################
#########################################################################

#########################################################################
# Issue a simple command
2+4
5/2
3^7          # 3 to the power of 7

factorial(5)
choose(5,2)  # binomial coefficient

# Brackets - only round brackets () are used for counting, square brackets [] and braces {} are used in a different way
{[(10+2)*(340-33)]-2}/3  # won't work

(((10+2)*(340-33))-2)/3

#########################################################################
# Getting Help with RStudio

# Displaying the Help file for a specific function
?choose
help(choose)


########################################################################
# Revision - combinatorics -> exercises from http://homel.vsb.cz/~vrt0020/statistics/01_exercises_combinatorics.pdf

## Exercise 1 - how many "passwords" are possible for each padlock? The safest one is the one with most possible options.
choose(10,4)  # first padlock
choose(10,6)  # second padlock
10^4 #third padlock

# The third padlock is the safest.

## Exercise 2
choose(22,3)     # a) randomly, no condition, pick 3 students from 22 (does order matter? repetition allowed?)
choose(22,3)-(choose(12,3)*choose(10,0))  # b) at least 1 girl

## Exercise 3 - pick 3 items from 7 items (does order matter? repetition allowed?)
choose(7,3)

## Exercise 4
factorial(4)  # permutations
factorial(13)/(factorial(2)*factorial(2)*factorial(2))  # permutations wih repetition!

## Exercise 5
26^4  # only letters, no UPPER or lower case
52^4  # distinguish UPPER and lower case
62^4  # with numbers


###  Data types in R  ###############################################################
# vector
# factor (special type of vector - necessary to know for Exploratory Data Analysis)
# matrix 
# data.frame
# list
######################################################################################

# Vectors
a = c(1,2,5.3,6,-2,4) # numeric vector
a
b = c("A","B","C") # character vector
c = c(TRUE,TRUE,TRUE,FALSE,TRUE,FALSE) #logical vector

# How to find data type?
class(a)
class(b)
class(c)

# numbers as character - note the quotation marks
a
as.character(a)

#character as factor
b
as.factor(b)

# refer to an elements of a vector using square brackets
a[3]

# get part of vector according to any condition
a
a[a>3] # elements of a, which are greater than 3
a[a<3 & a>0]

# get the length of the vector
length(b)

# other possbile ways to define a vector
rep(2,6)
seq(1,10,2)
5:15

# TASK 1 ///////////////////////////////////////////////////////////////
# Define a vector that contains all even numbers from 2 to 100. 
# Print this vector.



# //////////////////////////////////////////////////////////////////////


##############################################################

# Matrices
values= 1:12
matrix(v,nrow = 3)
matrix(v,nrow = 3,byrow = T)

# naming of rows and columns of matrix
M = matrix(v,nrow = 3,byrow = T)
rownames(M)
rownames(M) = c("row 1","row 2","row 3")
colnames(M) = c("column 1","column 2","column 3","column 4")
M

# refer to an element of matrix, a row, several columns...
M[2,3]
M[2,]
M[,2:3]

# operations with matrices 
A = matrix(0:3,nrow = 2,byrow = T)
B = matrix(rep(1,4),nrow = 2)
A
B
A + B
A * B    # it is not product of A and B
A %*% B  # be careful if you want to calculate product of matrices

# TASK 2 ///////////////////////////////////////////////////////////////
# a) Build a matrix with numbers 1 to 4 in the first column 
#    and numbers 5 to 8 in the second column
# b) Build a matrix with numbers 1 to 4 in the first row 
#    and numbers 5 to 8 in the second row



# //////////////////////////////////////////////////////////////////////

################################################################
##### Data frame
# dataset can be imported - from the file on the internet, from the file in your laptops
# you can import e.g. xls(x), csv...
# or you can use some dataset which is saved in R

# importing from the internet (not recommended, usually you don't know the structure)
data=read.csv(file="http://homel.vsb.cz/~vrt0020/statistics/battles.csv")

# recommendation - save it to your laptops, check the structure and import it from the file
data=read.csv2("C:/Users/USER1/battles.csv",header=TRUE)

# or use function getwd() and setwd() to set your working directory 
# and import the dataset without specifying the path in read.csv2
getwd()
setwd("C:/Users/USER1")
data=read.csv2("battles.csv",header=TRUE)

# or use the button "Import dataset" in "Environment"

# or you can use some dataset which is saved in R (usually in some extra package)
install.packages("car") # downloading package from the internet to your laptop - only once
library(car) # activating the functions from this package - every time you re-open RStudio

iris  # can be used directly but you can't see it in the Environment
my_data=iris # now it shows up (recommended)

#################################################################################
# Let's work with the GoT dataset

data=read.csv(file="http://homel.vsb.cz/~vrt0020/statistics/battles.csv")

# Working with dataset
rownames(data)
colnames(data)
data$attacker_king
data$defender_size

data[data$attacker_king=="Joffrey/Tommen Baratheon",] # notice the double "=="
Joffrey=data[data$attacker_king=="Joffrey/Tommen Baratheon",]

data[data$attacker_king=="Robb Stark" & data$attacker_outcome=="win",]

data$name[data$attacker_king=="Robb Stark" & data$attacker_outcome=="win"]

# TASK 3 ///////////////////////////////////////////////////////////////
# Choose those battles with Balon/Euron Greyjoy as attacker_king and more than 500 attacker_size.
# Choose also only attacker_outcome column.



# //////////////////////////////////////////////////////////////////////

#################################################################################
# Basic Exploratory Data Analysis
boxplot(data$defender_size)
hist(data$defender_size)
hist(data$defender_size, breaks=10)

mean(data$defender_size)
mean(data$defender_size,na.rm=TRUE)

quantile(data$defender_size,probs=0.5,na.rm=TRUE)
quantile(data$defender_size,probs=c(0.25,0.5,0.75),na.rm=TRUE)

sd(data$defender_size,na.rm=TRUE)

min(data$defender_size,na.rm=TRUE)
max(data$defender_size,na.rm=TRUE)

## TASK 4: save the mean, median and standard deviation of the defender_size to one vector


# //////////////////////////////////////////////////////////////////////

## TASK 5: use boxplot to look at the attacker_size, are there any outliers? If so, how many?


# //////////////////////////////////////////////////////////////////////

## TASK 6: use R-help for the hist function and try to change color of the histogram for defender_size


# //////////////////////////////////////////////////////////////////////


###################################################################
### functions for skewness and kurtosis are in the package "moments"
install.packages("moments") # Firstly, install package (only once)
library(moments)  # activate the package - every time you start RStudio and want to work with it
skewness(data$defender_size,na.rm=TRUE)
kurtosis(data$defender_size,na.rm=TRUE)-3

## !! skewness and kurtosis are saved in more packages and might have slightly different definiton
# to be sure that we use them from the "moments" package
moments::skewness(data$defender_size,na.rm=TRUE)
moments::kurtosis(data$defender_size,na.rm=TRUE)-3

####################################################################
## you can define your own function
f <- function(x,y){
  (x^2)+(y^2)
}
f(10,15)

## TASK 7: define your own function for r-Permutations without repetition


# //////////////////////////////////////////////////////////////////////



