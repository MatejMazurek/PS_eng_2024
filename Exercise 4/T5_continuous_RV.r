# ......................................................................................
# ........................Exercise 4 - Continuous random variable.......................
# ......................................................................................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

# **The content of this script is only as a supplementary illustration to the exercise,
# it is not necessary to know at the exam. It is important to be able to calculate
# manually.**
#  
# * Numerical integration in R ####
#  
# R function **integrate**integrate(f, a, b)=$\int_{a}^{b}f(x)dx$
#  
# - **f** is a R function(defined by us) which has one input argument - a vector of
# values in which to return its values
#  
# - **a** lower integration limit
#  
# - **b** upper integration limit
#  


f = function(x){return(x*x)} # x ^ 2
a = -1
b = 2
integrate(f, a, b)

x = seq(0,10,0.1)

y=x*x/100
plot(x,y)

(9*50-20^2)/9

#  Examples ####
#  
# * Example 1. ####
#  
# Random variable X has distribution function$F(x)=\begin{cases} 0    &      x \leq
# 0 \\ cx^2 &  0 < x \leq 1 \\ 1    &  1 < x \end{cases}$What values can the
# constant c take?
#  


# derivative of F(x) is the density of density f(x)
# corresponding probability density at interval<0.1>
f = function(x){return(2*x)} # f(x)=2x
a = 0
b = 1
integrate(f, a, b)$value

# c=1, so the distribution function looks like this:
F.dist = function(x){
    res = x*x     # x ^ 2
    res[x<=0] = 0 # 0 for x<=0
    res[x>1] = 1  # 1 for x>1
    return(res)
}

x = seq(from = -1, to = 2, by = 0.01) # points on the x-axis
FX = F.dist(x) # values of F(x)
plot(x, FX, type = 'l') # draw as a line


# * Example 2. ####
#  
# The distribution of a random variable X is given by the density$f(x)=\begin{cases}
# 2x+2 & x \in <-1;0> \\ 0    & x \notin <-1;0> \end{cases}$Specify:
#  
# ** 2. a) ####
#  
# $F(x)$,
#  


f.dens = function(x){
    res = 2*x + 2 
    # watch out for x<-1 because '<-' is in the assignment line
    res[x < -1] = 0 # 0 for x<=0
    res[x > 0] = 0  # 1 for x>1
    return(res)
}

x = seq(from = -2, to = 1, by = 0.01) # points on the x-axis
fx = f.dens(x) # values of f(x)
plot(x, fx, cex=0.2) # draw dots(cex is the size)


F.dist = function(x){
    res = x*x+2*x+1     # x ^ 2 + 2x + 1
    res[x < -1] = 0 # 0 for x<=0
    res[x > 0] = 1  # 1 for x>1
    return(res)
}

x = seq(from = -2, to = 1, by = 0.01) # points on the x-axis
FX = F.dist(x) # values of f(x)
plot(x, FX, type='l') # draw dots(cex is the size)


# ** 2. b) ####
#  
# P(−2 ≤ X ≤ 0.5), P(−2 ≤ X ≤ −1), P(X>0.5), P(X=0.3)
#  


# P(−2 ≤ X ≤.50.5)
integrate(f.dens, -2, -0.5)$value
integrate(f.dens, -1, -0.5)$value

# P(−2 ≤ X − −1)
integrate(f.dens, -2, -1)$value

# P(X>0.5)
integrate(f.dens, 0.5, 1e16)$value # This will not always work


# P(X=0.3)
integrate(f.dens, 0.3, 0.3)$value
# it is clear that this probability is 0
# corresponds to the integral sa=b, ie with zero size on the x-axis


# ** 2. c) ####
#  
# mean, variance and standard deviation of the random variable X.
#  


# E(X)
x_fx = function(x){
    fx = f.dens(x)
    return(x*fx)
} 
# we integrate only where we know that f(x) is nonzero
E_X = integrate(x_fx, -1, 0)$value
E_X
-1/3

# E(X ^ 2)
xx_fx = function(x){
    fx = f.dens(x) 
    return(x*x*fx)
} 
# we integrate only where we know that f(x) is nonzero
E_XX = integrate(xx_fx, -1, 0)$value
E_XX
1/6

# D(X)
D_X = E_XX - E_X^2
D_X
1/18

# sigma(x)
std_X = sqrt(D_X)
std_X
sqrt(2)/6

# ** 2. d) ####
#  
# mode $\hat{x}$
#  


# mode=0


# ** 2. e) ####
#  
# median $x_{0,5}$
#  


x = seq(from = -2, to = 1, by = 0.001) # points on the x-axis
FX = F.dist(x)
plot(x, FX, type='l')
lines(c(-2, 1),c(0.5, 0.5))

x[FX >= 0.5][1] # first element zx for which F(x)>=0.5
(-2+sqrt(2))/2

# * Example 3. ####
#  
# The random variable Y is defined as: Y=3X + 1, where X is the random variable from the
# previous example. Specify:
#  
# ** 3. a) ####
#  
# $F_Y(y)$
#  


FY.dist = function(y){
    # calculated from the relation FY(y)=P(Y<y)=P(3X + 1<y)=...
    x = (y-1)/3 
    FY = F.dist(x)
    return(FY)
}
y = seq(from = -3, to = 2, by = 0.001) # points on the x-axis
FY = FY.dist(y)
plot(y, FY, type='l')

# ** 3. b) ####
#  
# $f_Y(y)$
#  


# derivation of F_Y
fY.dens = function(y){
    res = 2/9*(y + 2) 
    res[y < -2] = 0 # 0 for x<-2
    res[y > 1] = 0  # 1 for x>1
    return(res)
}
integrate(fY.dens,-2,1)$value # total integral check
y = seq(from = -3, to = 2, by = 0.001) # points on the x-axis
fY = fY.dens(y)
plot(y, fY, cex=0.2)

# ** 3. c) ####
#  
# E(Y), D(Y), σ(Y)
#  


# E(Y)
y_fy = function(y){
    fy = fY.dens(y)
    return(y*fy)
} 
# we integrate only where we know that f(y) is nonzero
E_Y = integrate(y_fy, -2, 1)$value
E_Y
0

# alternatively
E_Y = 3*E_X + 1
E_Y

# E(Y ^ 2)
yy_fy = function(y){
    fy = fY.dens(y)
    return(y*y*fy)
} 
# we integrate only where we know that f(y) is nonzero
E_YY = integrate(yy_fy, -2, 1)$value
E_YY
1/2

# D(Y)
D_Y = E_YY - E_Y^2
D_Y
1/2

# alternatively
D_Y = 3^2*D_X
D_Y

# sigma(Y)
sqrt(D_Y)
sqrt(2)/2

# * Example 4 ####
#  
# Calculate $\omega$ such that a random variable X with probability
# density:$f(x)=\begin{cases} 0 & x < 0 \\ 3e^{-3x}    & x \geq 0 \end{cases}$is
# greater than $\omega$ with probability 0.3.
#  


F.dist = function(x){
    res = 1 - exp(-3*x)
    res[x < 0] = 0 # 0 for x<=0
    return(res)
}

x = seq(from = -1, to = 3, by = 0.001) # points on the x-axis
FX = F.dist(x)
plot(x, FX, type='l')
lines(c(-1, 3),c(0.7, 0.7))

x[FX >= 0.7][1]

-1/3*log(0.3)



