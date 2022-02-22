# ......................................................................................
# ..........Exercise 6 - Selected distributions of a continuous random variable.........
# ......................................................................................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  Overview of distributions and their functions ####
#  
# * Introduction: Probability Density, Distribution Function and Quantile Function ####
#  
# ** Probability density ####
#  
# - starts with the letter **d**: p=d...(x,...)
#  
# ** Distribution function ####
#  
# - starts with the letter **p**: $p = P(X < x)$:p=p...(x,...)
#  
# ** Quantile function ####
#  
# - starts with the letter **q**: find x for the given p: $p = F(x) \rightarrow x =
# F^{-1}(p)$: x=q...(p,...)
#  


# * Uniform distribution: $X \sim U(a, b)$ ####
#  
# - the random variable takes on only values greater than a and less than b
#  
# - all values have the same occurrence density ->the probability density is constant
# between a and b, elsewhere zero
#  


# Probability density f(x)
a = 2   # from where
b = 4   # up to
x = 3   
dunif(x, a, b)

# we plot the probability density
x = seq(from = 0, to = 6, by = 0.01)
f_x = dunif(x, a, b)
plot(x, f_x, cex = 0.1) # cex is the size of the markers
grid()

# Distribution function F(x)=P(X<x)
a = 2   # from where
b = 4   # where
x = 3   
punif(x, a, b)

# let's draw the Distribution function
x = seq(from = 0, to = 6, by = 0.01)
F_x = punif(x, a, b)
plot(x, F_x, type = 'l')
grid()

# quantile function F ^(- 1)(p)=x: P(X<x)=p
a = 2   # from where
b = 4   # where
p = 0.75   
qunif(p, a, b)

# plot - quantile function F ^(- 1)(p)=x
p = seq(from=0, to=1, by=0.01)   
x = qunif(p, a, b)
plot(p, x, type = 'l')
grid()

# * Exponential distribution: $X \sim Exp(\lambda)$ ####
#  
# - time to 1st event, time between events(only in the period of stable life - Poisson
# process)
#  
# - the parameter $\lambda$ is the same as in the Poisson distribution
#  
# - the mean value is: $E(X)=1 / \lambda$
#  


# Probability density f(x)
lambda = 2
x = 1
dexp(x, lambda)

# we plot the probability density
x = seq(from = 0, to = 6, by = 0.01)
f_x = dexp(x, lambda)
plot(x, f_x, type='l')
grid()

# Distribution function F(x)=P(X<x)
lambda = 2
x = 1
pexp(x, lambda)

# let's draw the Distribution function
x = seq(from = 0, to = 6, by = 0.01)
F_x = pexp(x, lambda)
plot(x, F_x, type = 'l')
grid()

# quantile function F ^(- 1)(p)=x: P(X<x)=p
lambda = 2
p = 0.5 
qexp(p, a, b)

# plot - quantile function F ^(- 1)(p)=x
p = seq(from=0, to=1, by=0.001)   
x = qexp(p, lambda)
plot(p, x, type = 'l')
grid()

# * Weibull distribution: $X \sim W(\theta,\beta)$ ####
#  
# - time until the 1st event(faults)(suitable choice of β allows use in any period of
# fault intensity)
#  
# - extension of exponential distribution Exp(λ)=W(Θ=1/λ, β=1)
#  


# Probability density f(x)
theta = 1/2 # equivalent of 1/lambda in exp. distribution
beta = 1  # beta=1 ->exponential distribution
x = 5
dweibull(x,shape=beta, scale=theta)

# we plot the probability density
x = seq(from = 0, to = 6, by = 0.01)
f_x = dweibull(x,shape=beta, scale=theta)
plot(x, f_x, type='l')
grid()

# Distribution function F(x)=P(X<x)
theta = 3 # equivalent of 1/lambda in exp. distribution
beta = 2  # beta=1 ->exponential distribution
x = 5
pweibull(x,shape=beta, scale=theta)

# we draw the Distribution function
x = seq(from = 0, to = 6, by = 0.01)
F_x = pweibull(x,shape=beta, scale=theta)
plot(x, F_x, type = 'l')
grid()

# quantile function F ^(- 1)(p)=x: P(X<x)=p
theta = 3 # equivalent of 1/lambda in exp. distribution
beta = 2  # beta=1 ->exponential distribution
p = 0.5
qweibull(p,shape=beta, scale=theta)

# plot - quantile function F ^(- 1)(p)=x
p = seq(from=0, to=1, by=0.01)   
x = qweibull(p,shape=beta, scale=theta)
plot(p, x, type = 'l')
grid()

# * Normal distribution: $X \sim N(\mu,\sigma^2)$ ####
#  
# - distribution modeling eg. measurement errors, sum/average behavior of many other
# random variables
#  
# - viz. Central limit theorem
#  
# - $\mu$ is the average distribution value: $E(X)=\mu$
#  
# - $\sigma$ is the directly standard deviation of the distribution: $D(X)=\sigma^2$
#  
# - with parameters $\mu=0,\sigma=1$ is called normalized Normal distribution
#  


# Probability density f(x)
mu = 2
sigma = 3
x = 4
dnorm(x, mean=mu, sd=sigma)

# we plot the probability density
x = seq(from = -5, to = 10, by = 0.01)
f_x = dnorm(x, mean=mu, sd=sigma)
plot(x, f_x, type='l')
grid()

# Distribution function F(x)=P(X<x)
mu = 2
sigma = 3
x = 4
pnorm(x, mean=mu, sd=sigma)

# let's draw the Distribution function
x = seq(from = -5, to = 10, by = 0.01)
F_x = pnorm(x, mean=mu, sd=sigma)
plot(x, F_x, type = 'l')
grid()

# quantile function F ^(- 1)(p)=x: P(X<x)=p
mu = 2
sigma = 3
p = 0.5
qnorm(p, mean=mu, sd=sigma)

# plot - quantile function F ^(- 1)(p)=x
p = seq(from=0, to=1, by=0.01)   
x = qnorm(p, mean=mu, sd=sigma)
plot(p, x, type = 'l')
grid()

#  Examples ####
#  


# * Example 1. ####
#  
# Height in the population of boys aged 3.5-4 years has a normal distribution with a
# mean value of 102 cm and a standard deviation of 4.5 cm. Determine what percentage of
# boys at that age are less than or equal to 93 cm in height.
#  


# X... height of boys aged 3.5 to 4 years(cm)
# X~N(mu=102, sd=4.5)

mu = 102
sigma = 4.5

# P(X<=93)=F(93)
pnorm(93, mean=mu, sd=sigma) 

# * Example 2. ####
#  
# The average life of a machine part is 30,000 hours. Assume that the component is in a
# period of stable life. Specify:
#  


# X... component life(h)
# X~Exp(lambda), where E(X)=1/lambda

lambda = 1/30000

# ** a) ####
#  
# probability that the component will not last more than 2,000 hours,
#  


# a) P(X<2000)=F(2000)
pexp(2000, lambda)

# ** b) ####
#  
# probability that the component will last more than 35,000 hours,
#  


# b) P(X>35000)=1-F(35000)
1 - pexp(35000, lambda)

# ** c) ####
#  
# time until 95% of the components fail.
#  


# c) P(X<t)=0.95 ->F(t)=0.95 ->t… 95% quantile
qexp(0.95, lambda)

# * Example 3. ####
#  
# The production facility fails on average once every 2000 hours. The quantity Y
# representing the time before the fault has an exponential distribution. Determine the
# time T0 so that the probability that the device will run longer than T0 is 0.99.
#  


# X... fault waiting time(h)
# X~Exp(lambda), where E(X)=1/lambda
lambda = 1/2000

# P(X>t)=0.99 ->1-F(t)=0.99 ->F(t)=0.01 ->t… 1% quant.
qexp(0.01, lambda)

# * Example 4. ####
#  
# The measurement results are corrupted only with a normally distributed error with zero
# mean and a standard deviation of 3 mm. What is the probability that there will be an
# error in the interval(0 mm; 2.4 mm) at least once in 3 measurements?
#  


# Y measurement error size(mm)
# Y~N(mu=0, sigma=3)

mu = 0
sigma = 3

# pp… true that the measurement error will be in int. 0.0-2.4mm
pp = pnorm(2.4,mean=mu,sd=sigma) - pnorm(0,mean=mu,sd=sigma)
pp

# X… number of measurement errors in int. 0 mm -2.4 mm in 3 measures.
# X~Bi(n=3, p=pp)
n = 3
p = pp

# P(X>=1)=1-P(X=0)
1 - dbinom(0, n, p)

# * Example 5. ####
#  
# An average of 25 users per hour log on to a large computer network. Determine the
# probability that:
#  
# ** a) ####
#  
# no one logs in during 14:30 - 14:36,
#  


# X… number of users logged in in 6 minutes
# X~Po(lt=2.5)

lambda = 25/60
t = 6
lt = lambda*t

# P(X=0)
dpois(0, lt)

# ** b) ####
#  
# 2-3 minutes will elapse before the next login.
#  


# Y… time until next login
# Y~Exp(lambda=25/60), where E(X)=1/lambda
lambda = 25/60 

# P(2<Y<3)=F(3) -F(2)
pexp(3, lambda) - pexp(2, lambda)

# ** c) ####
#  
# Specify the maximum length of the time interval so that the probability that no one
# will log in is at least 0.90.
#  


# P(Y>t)=0.90 ->1-F(t)=0.90 ->F(t)=0.10 ->t… 10% kv.

qexp(0.10, lambda)*60

# * Example 6. ####
#  
# The random variable X has a normal distribution N(µ; σ). Specify:
#  
# ** a) ####
#  
# P(µ - 2σ<X<µ + 2σ),
#  


# P(µ - 2σ<X<µ + 2σ)=F(µ + 2σ) - F(µ - 2σ)
# X~N(µ, σ)
# it doesn't matter what values we choose
mu = -105.5447 
sigma = 2.654

pnorm(mu + 2*sigma, mean=mu, sd=sigma) - 
pnorm(mu - 2*sigma, mean=mu, sd=sigma)

# ** b) ####
#  
# smallest k ∈ Z, so that P(µ - kσ<X<µ + kσ)>0.99.
#  


# normal distribution is symmetric
# P(µ - kσ<X<µ + kσ)=
# 1 -(P(X<µ - kσ) + P(X>µ + kσ))=
# 1 - 2 * P(X>µ + kσ)=0.99 ->P(X>µ + kσ)=0.005
# ->P(X<µ + kσ)=0.995

# x=µ + kσ
x = qnorm(0.995, mean=mu, sd=sigma)
(x - mu)/sigma

for(k in 1:5){
    p = pnorm(mu + k*sigma, mean=mu, sd=sigma) - 
        pnorm(mu - k*sigma, mean=mu, sd=sigma)
    print(paste0(k,":",p))
}

# * Example 7. ####
#  
# An accompanying film about the life of the author of the exhibited works is screened
# on a tour of the exhibition. His screening begins every 20 minutes. Determine the
# probability that if you come to the screening room at random time (and you have no
# clue about screening shedule),
#  


# Y… time until the start of the next projection
# Y~Ro(a=0, b=20)

a = 0
b = 20

# ** a) ####
#  
# you won't have to wait more than 5 minutes for the movie to start,
#  


# P(X<5)
punif(5, a, b)

# ** b) ####
#  
# you will wait between 5 and 10 minutes,
#  


# P(5<X<10)
punif(10, a, b) - punif(5, a, b)

# ** c) ####
#  
# mean and standard deviation of the waiting time for the beginning of the movie.
#  


E_X = (a + b)/2
E_X

D_X = (a - b)^2/12
D_X

sigma_X = sqrt(D_X)
sigma_X

# * Example 8. ####
#  
# During quality control, we only accept the part if its dimension is in the range of
# 26-27 mm. The dimensions of the components have a normal distribution with a mean
# value of 26.4 mm and a standard deviation of 0.2 mm. What is the probability that the
# size of the part randomly selected for inspection will be within the required limits?
#  


# X... part size(mm)
# X~N(mu=26.4, sigma=0.2)

mu = 26.4
sigma = 0.2

# P(26<X<27)=F(27) -F(26)
pnorm(27, mean=mu, sd=sigma) - pnorm(26, mean=mu, sd=sigma)

# * Example 9. ####
#  
# The length of jumps of the athlete Jakub measured in cm has a normal distribution
# N(µ1; σ1), where µ1=690 and σ1=10. The length of jumps of the athlete Aleš measured in
# cm also has a normal distribution N(µ2; σ2), where µ2=705 and σ2=15. The one who jumps
# more than 700 cm from two jumps at least once qualifies for the races.
#  


# SJ... length of Jakub's jump
# SJ~N(mu=690, sigma=10)
mu_J = 690
sigma_J = 10

# SA Ale Aleš's jump length
# SA~N(mu=705, sigma=15)
mu_A = 705
sigma_A = 15

# J... Jakub's jump is successful(longer than 700 cm)
# And... Aleš's jump is successful(longer than 700 cm)

# P(J)=P(SJ>700)=1-F(700)
P.J = 1-pnorm(700,mean=mu_J,sd=sigma_J)
P.J

# P(A)=P(SA>700)=1-F(700)
P.A = 1-pnorm(700,mean=mu_A, sd=sigma_A)
P.A

# KJ… Jakub qualifies for races,
# P(KJ)=1-(1-P(J))(1-P(J))
P.KJ=1-(1-P.J)*(1-P.J)
P.KJ

# KA… Aleš qualifies for races,
# P(KA)=1-(1-P(A))(1-P(A))
P.KA=1-(1-P.A)*(1-P.A)
P.KA

# ** a) ####
#  
# How likely are they both to qualify for the race?
#  


# ada)
P.KJ*P.KA

# ** b) ####
#  
# With what probability does Aleš qualify, but Jakub does not?
#  


# adb)
(1-P.KJ)*P.KA



