# ......................................................................................
# ..................Exercise 1 - Brief introduction to R, Combinatorics.................
# ......................................................................................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

# First we will go through some basics of R.
#  
#  Brief introduction to R ####
#  


# simple arithmetic operations
2+4
5/2

# BEWARE of brackets! Only round ones are used for counting!
# Square and compound have a different role in R.!
(((10+2)*(340-33))-2)/3

# combination number, factorials
choose(10,2)
factorial(4)

# data types ->numeric, character, logical,(complex)
# the class function determines the type of the object
a=2+3
class(a)

b="some text"
class(b)

c=(1>3)
class(c)

d=3+1i
class(d)

# * data structures in R ####
#  
# - vector (column vector)
#  
# - factor (special case of vector)
#  
# - matrix (matrix with dimensions n x m)
#  
# - data.frame (data frame with columns representing diferent types of informations and
# rows representing single records)


# vector definition
a = c(3,4,6,7)
a <- c(3,4,6,7)
a[2]

# other options
rep(1,4) # creates a vector with four ones

seq(1,10,2) # sequence from 1 to 10 with step 2

1:10  # sequence from 1 to 10 with step 1

b=c("A","B","C","D")
b

class(b)

# redefining an object to another type - eg as.vector, as.matrix, as.factor,...
b=as.factor(b)
b

# working with vectors - merging by columns/rows
cbind(a,b)

rbind(a,b)

c(a,b)

# matrix definition
A=matrix(c(3,4,6,7,3,2),nrow=2,ncol=3)
B=matrix(c(3,4,6,7,3,2),nrow=2,ncol=3,byrow=TRUE)
C=matrix(c(3,4,6,7,3,2),nrow=3,ncol=2)

B

B[1,3]

A[1,]

A[,2:3]

# diagonal matrix
diag(4)

diag(4,2)

# matrix operations - pay attention to matrix multiplication -> %*%
A+B

A-B

A*B

A%*%C

