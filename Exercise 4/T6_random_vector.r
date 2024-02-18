# ......................................................................................
# ..............................Exercise 4 - Random vector..............................
# he content of this script is only as a supplementary illustration to the exercise, it is not necessary to know at the exam. It is important to be able to calculate manually.**
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  Example ####
#  
# Random vector $Z =(Y;X)^T$ has a probability function specified by the table </br> 
#  ![Image.png](attachment:image.png)
#  
# * a) Determine the missing value of the combined probability function, ####
#  


data = c(0.01, 0.04, 0.12,       
         0.02, 0.16, 0.07,       
         0.03, 0,    0.06,       
         0.25, 0.05, 0.01)
P = matrix(data , nrow=3, ncol=4) # possibly byrow=...
X = c(3, 5, 7)
Y = c(1, 2, 3, 4)
dimnames(P) = list(X,Y)
P

sum(P)

# do not run this cell twice, otherwise you will set the value back to 0,
# Do you know why?
p_5_3 = 1 - sum(P)
P["5","3"] = p_5_3
P

# * b) Specify the distribution function ####
#  
# **Attention! The vector Z is $(Y,X)^T$ so the first parameter is the value Y and the
# second value X.**
#  


# F(2.8; 7.1)
# P(Y<2.8, X<7.1)
P[X<7.1, Y<2.8]
sum(P[X<7.1, Y<2.8])

F = matrix(rep(0,4*5), nrow=4, ncol=5)
dimnames(F) = list(c('(-inf,3>', '(3,5>', '(5,7>', '(7,inf)'),
                   c('(-inf,1>', '(1,2>', '(2,3>', '(3,4>', '(4,inf)'))
F

# we go through the rows and columns, we always take one value
# from the relevant row or column
x_vals = c(3,5,7,8)
y_vals = c(1,2,3,4,5)
for(i in 1:4){
    for(j in 1:5){
        x = x_vals[i]
        y = y_vals[j]
        F[i,j] = sum(P[X<x, Y<y])
    }
}
F

# * c) Determine the marginal distribution ####
#  


P_x = rowSums(P)
P_x

F_x = c(0, cumsum(P_x))
F_x

P_y = colSums(P)
P_y

F_y = c(0, cumsum(P_y))
F_y

# * d) Conditional probabilities and conditional probability functions $P(x|y), P(y|x)$ ####
#  


# P(Y>2.1|X<5.3)
# P(Y>2.1 ∧ X<5.3)/P(X<5.3)
sum(P[X<5.3, Y>2.1])
sum(P[X<5.3,])
sum(P[X<5.3, Y>2.1])/sum(P[X<5.3,])

# P(X=5|Y=1)
# P(X=5 ∧ Y=1)/P(Y=1)
P['5','1']/sum(P[,'1'])
P['5','1']/sum(P_y['1'])

# **$P(x|y)=\frac{P(X=x,Y=y)}{P_Y(y)}$**
#  


P_xy = P # it's the same size, so we'll steal the formatting
X_lab = c('3', '5', '7')
Y_lab = c('1', '2', '3', '4')
for(x in X_lab){
    for(y in Y_lab){
        P_xy[x, y] = P[x, y]/P_y[y]
    }
}
P_xy
colSums(P_xy)

# **$P(y|x)$**
#  


P_yx = P # it's the same size, so we'll steal the formatting
for(x in X_lab){
    for(y in Y_lab){
        P_yx[x, y] = P[x, y]/P_x[x]
    }
}
P_yx
rowSums(P_yx)

# * e) basic characteristics of random variables X and Y ####
#  


E_X = sum(X*P_x)
E_X
E_XX = sum(X*X*P_x)
D_X = E_XX - E_X^2
D_X

E_Y = sum(Y*P_y)
E_Y
E_YY = sum(Y*Y*P_y)
D_Y = E_YY - E_Y^2
D_Y

# * f) conditional mean E(X|Y=2) ####
#  


# P(x|Y=2)
P_xy[,'2']
E_X_Y2 = sum(X*P_xy[,'2'])
E_X_Y2

# * g) covariance and correlation ####
#  


X_Y = P # matrix where in each column is the value x * y
for(x in X){
    for(y in Y){
        X_Y[toString(x), toString(y)] = x*y  
    }
}
X_Y

# or we can use matrix multiplication 
X %*% t(Y)

# mean value of E(X * Y)
E_XY = sum(X_Y*P)
E_XY

# covariance
cov_XY = E_XY-E_X*E_Y
cov_XY

# correlation
cov_XY/sqrt(D_X*D_Y)



