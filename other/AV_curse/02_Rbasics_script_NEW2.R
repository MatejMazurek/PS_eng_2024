#######################        RStudio Basics       ##################################
###################    Adela Vrtkova, Martina Litschmannova   ########################
################### Dep. of Applied Mathematics, FEECS, VB-TUO ######################

#/////////////////////////////////////////////////////////////////////////////////////
## 0. Introduction, Environment, Help, Packages   ####################################

?mean   # Displaying the Help file for a specific function
help("mean")

getwd() # Getting working directory
setwd("C:/Myfiles")  # Setting working directory (only "/" !)

install.packages("dplyr")  # Installing a package called dplyr - only once
library(dplyr)             # Activating a package called dplyr - every time after restarting RStudio

#/////////////////////////////////////////////////////////////////////////////////////
## 1. Simple commands, brackets  ###########################################

# Issue a simple command 
2+4
5/2
3^7          # 3 to the power of 7

factorial(5)
choose(5,2)  # binomial coefficient

# Brackets - only round brackets () are used for counting, square brackets [] and braces {} are used in a different way
{[(10+2)*(340-33)]-2}/3  # won't work

(((10+2)*(340-33))-2)/3

# TASK ------------------------------------------------------------------------------
# Revision + Practising - Combinatorics -> exercises from https://homel.vsb.cz/~vrt0020/statistics/Lesson01_Combinatorics_exercises.pdf

## Exercise 1 - how many "passwords" are possible for each padlock? The safest one is the one with most possible options.
choose(10,4)  # first padlock
choose(10,6)  # second padlock
10^4 # third padlock
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
# ---


#/////////////////////////////////////////////////////////////////////////////////////
## 2. Data types ###########################################
# vector
# factor (special type of vector - necessary to know for Exploratory Data Analysis)
# matrix 
# data.frame
# list

## 2.1 Vectors ####
c(5,9,6,1)  # numeric vector
1:10        # sequence
10:1        # sequence
rep(5,20)   # replicate the values
seq(1,10,2) # sequence
c("A","B","C") # character vector

c("A",1,"C",4) # check the result

# How to find data type?
a = c(5,9,6,1)
b = c("A","B","C")

class(a)
class(b)

# numbers as character - note the quotation marks
a
as.character(a)

# character as factor
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

# TASK -----------------------------------------------------------------------------
# Revision + Practising - Random Variable -> exercises from https://homel.vsb.cz/~vrt0020/statistics/Lesson03_RandomVariable_exercises.pdf


# ---

## 2.2 Matrices ####
values = 1:12
matrix(values, nrow = 3)
matrix(values, nrow = 3, byrow = T)

# naming of rows and columns of matrix
M = matrix(values, nrow = 3, byrow = T)
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

# TASK ---------------------------------------------
# a) Build a matrix with numbers 1 to 4 in the first column 
#    and numbers 5 to 8 in the second column
# b) Build a matrix with numbers 1 to 4 in the first row 
#    and numbers 5 to 8 in the second row



# ---

## 2.3 Data frames ####
# Dataset can be imported - from the file on the internet, from the file in your laptops
# You can import e.g. RData, xls(x), csv, or you can use some dataset which is saved in R

# Importing from the internet (not recommended, usually you don't know the structure)
data=read.csv(file="http://homel.vsb.cz/~vrt0020/statistics/battles.csv")

# Recommendation - save it to your laptops, check the structure and import it from the file
data=read.csv2("C:/Users/USER1/battles.csv",header=TRUE)

# ...or use function getwd() and setwd() to set your working directory 
# and import the dataset without specifying the path in read.csv2...
getwd()
setwd("C:/Users/USER1")
data=read.csv2("battles.csv",header=TRUE)

# ...or use the button "Import dataset" in "Environment"...

# ...or you can use some dataset which is saved in R (usually in some extra package).
install.packages("car") # downloading package from the internet to your laptop - only once
library(car) # activating the functions from this package - every time you re-open RStudio

iris  # can be used directly but you can't see it in the Environment
my_data=iris # now it shows up (recommended)

# When working with RData file - double-click (or "Open with...")
#################################################################################
# Let's work with the GoT dataset (GOT_Data.RData)

# Working with dataset
rownames(battles)
colnames(battles)
battles$attacker_king
battles$defender_size

battles[battles$attacker_king=="J/T Baratheon",] # notice the double "=="
Joffrey=battles[battles$attacker_king=="J/T Baratheon",]

battles[battles$attacker_king=="Robb Stark" & battles$attacker_outcome=="win",]

battles$name[battles$attacker_king=="Robb Stark" & battles$attacker_outcome=="win"]

# TASK 3 ///////////////////////////////////////////////////////////////
# Choose those battles with Balon/Euron Greyjoy as attacker_king and more than 500 attacker_size.
# Choose also only attacker_outcome column.



# //////////////////////////////////////////////////////////////////////

#################################################################################
# Basic Exploratory Data Analysis
boxplot(battles$defender_size)
hist(battles$defender_size)
hist(battles$defender_size, breaks=10)

mean(battles$defender_size)
mean(battles$defender_size,na.rm=TRUE)

quantile(battles$defender_size,probs=0.5,na.rm=TRUE)
quantile(battles$defender_size,probs=c(0.25,0.5,0.75),na.rm=TRUE)

sd(battles$defender_size,na.rm=TRUE)

min(battles$defender_size,na.rm=TRUE)
max(battles$defender_size,na.rm=TRUE)

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
skewness(battles$defender_size,na.rm=TRUE)
kurtosis(battles$defender_size,na.rm=TRUE)-3

## !! skewness and kurtosis are saved in more packages and might have slightly different definiton
# to be sure that we use them from the "moments" package
moments::skewness(battles$defender_size,na.rm=TRUE)
moments::kurtosis(battles$defender_size,na.rm=TRUE)-3

