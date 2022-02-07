# ......................................................................................
# .....................Exercise 9. Interval estimates (one sample) .....................
# ................Michal Béreš, Martina Litschmannová, Veronika Kubíčková...............
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  Demonstration at the beginning - what is interval estimation? ####
#  
# Consider a random variable from the normal distribution with a mean value of $\mu$ and
# a standard deviation of $\sigma$. We will work with selections from this random
# variable and help them try to estimate the mean value of the distribution(here we know
# its true value, but in practice its value is unknown).
#  


n = 30        # selection size
mu = 100      # mean value
sigma = 10    # direction. deviation

# simulation of random selection from a given random variable
vyber = rnorm(n = n, mean = mu, sd = sigma)

X = mean(vyber) # sampling average as a point estimate
S = sd(vyber)   # selection direction. departure
X
S

# For clarity, we can visualize the selection.
#  


options(repr.plot.width = 12) # width of graphs in Jupyter
par(mfrow = c(1, 2))          # graph matrix 1x2

hist(vyber)
boxplot(vyber)

# ** The construction of the interval estimation itself using a selection ####
# characteristic
#  
# We will use this selection characteristic:(we assume that we do not know any real
# parameters of the distribution, only that it is normal)$Y=\frac{\bar X -
# \mu}{S}\sqrt{n} \sim t_{n-1}$Since we know the distribution of Y, we are able to
# compute $a$ a $b$ in the following expression:$P(a<Y<b)\geq 1 - \alpha$
#  
# - $\alpha$ is called the significance level(the probability that the searched value is
# outside our range)
#  
# - $1-\alpha$ is called the interval estimation reliability
#  


# $a$ a $b$ so that they are symmetric in probability, ie:
#  
# - $P(Y<a)\leq \alpha / 2 \rightarrow a=t_{\alpha / 2;n-1}$
#  
# - $P(b<Y)\leq \alpha / 2 \rightarrow P(Y\leq b)\geq 1 - \alpha / 2 \rightarrow
# b=t_{1-\alpha / 2;n-1}$
#  


# maximum probability with which we allow
# real st. hours lay outside the constructed interval
alpha = 0.05 

# relevant quantiles of the student's distribution
t_low = qt(alpha/2, df = n-1)
t_high = qt(1 - alpha/2, df = n-1)

t_low
t_high

# Next we just add to the expression and modify:$P(t_{\alpha / 2;n-1}<\frac{\bar X -
# \mu}{S}\sqrt{n}<t_{1-\alpha / 2;n-1})\geq 1 - \alpha$$P(\bar X - t_{1-\alpha /
# 2;n-1}\frac{S}{\sqrt{n}}<\mu<\bar X - t_{\alpha / 2;n-1}\frac{S}{\sqrt{n}})\geq 1 -
# \alpha$
#  


I_dolni = X - t_high*S/sqrt(n)
I_horni = X - t_low*S/sqrt(n)
paste("P(", I_dolni, " < µ < ", I_horni, ") ≥ ", 1-alpha)

# This particular estimate can also be obtained using the Rkov function t.test:
#  


t.test(vyber, alternative = 'two.sided', conf.level = 1-alpha)$conf.int

# ** Testing interval estimation on multiple selections ####
#  


count_pokusu = 10000 # number of selections

n = 30             # selection size
mu = 100           # mean value
sigma = 10         # guided. deviation.

alpha = 0.05        # significance level

# relevant quantiles of the student's distribution
t_low = qt(alpha/2, df = n-1)
t_high = qt(1 - alpha/2, df = n-1)

# plot of the actual mean value
plot(c(1, count_pokusu), c(mu, mu), type = 'l', ylim = c(90,110))

count_neuspesnych = 0
# cycle through individual selections
for(i in 1:count_pokusu){
    vyber = rnorm(n = n, mean = mu, sd = sigma)
    X = mean(vyber)
    S = sd(vyber)
    I_dolni = X - t_high*S/sqrt(n)
    I_horni = X - t_low*S/sqrt(n)
    
    # select the plot color, depending on whether the IO contains the middle hour.
    if( I_dolni<mu & mu<I_horni){
        barva = "blue"      
    }else{
        barva = "red"
        count_neuspesnych = count_neuspesnych + 1
    }
    # draw the IC as a vertical line
    lines(c(i, i), c(I_dolni, I_horni), col=barva)  
}
paste('alpha = ', alpha, ', relativní četnost něúspěšných IO = ', 
      count_neuspesnych/count_pokusu)

# I will return the width to the standard size
options(repr.plot.width = 8) # width of graphs in Jupyter


#  Types of interval estimates ####
#  
# Examples of estimating the mean value of data from a normal distribution.)
#  
# * Bottom/Left IC ####
#  
# - $P(M_D^* < \mu) = 1-\alpha$
#  
# - in Rku **alternative="greater"**
#  


vyber = rnorm(n = 30, mean = 100, sd = 10)
alpha = 0.05
t.test(vyber, alternative = 'greater', conf.level = 1-alpha)$conf.int

# * Top/Right IO ####
#  
# - $P(\mu < M_H^*) = 1-\alpha$
#  
# - in Rku **alternative="less"**
#  


vyber = rnorm(n = 30, mean = 100, sd = 10)
alpha = 0.05
t.test(vyber, alternative = 'less', conf.level = 1-alpha)$conf.int

# * Double-sided IC ####
#  
# - $P(M_D < \mu < M_H) = 1-\alpha$
#  
# - in Rku **alternative="two.sided"**
#  


vyber = rnorm(n = 30, mean = 100, sd = 10)
alpha = 0.05
t.test(vyber, alternative = 'two.sided', conf.level = 1-alpha)$conf.int

#  Overview of selection parameters and their point/interval estimates ####
#  
# We usually have more IO constructions(functions in Rk that will do this for us), but
# each construction has different data requirements and produces different "quality"(in
# terms of IO size) estimates. We will always select the "best quality" IC that **has
# met** the prerequisites for use.The order of the various ICs below will always be
# from "best" to most robust.
#  
# * Position measures of one selection ####
#  
# By position measures we mean the data that determines the position of the data, no
# matter how scattered. For data from the normal distribution we can estimate the mean
# value, for others the median.
#  
# *** a) student's IC t-test ####
#  
# - we estimate the mean value - the point estimate is the sample average
#  
# - the data must come from a normal distribution
#  
# - exploratory: skewness and sharpness lie in(-2,2)
#  
# - Explosive: The QQ graph has points approximately on the line
#  
# - exact: using a statistical test, eg Shapiro-Wilk test(shapiro.test(data))
#  


vyber = rnorm(n = 30, mean = 100, sd = 10)
alpha = 0.05

# exploratory normality test
# library(moments) - we can avoid this by calling moments ::
# it's safer - we're sure we're calling a feature from this package
moments::skewness(vyber)
moments::kurtosis(vyber) - 3
qqnorm(vyber)
qqline(vyber)

# exact data normality test
shapiro.test(vyber)$p.value
# the resulting p-value must be greater than hl. challenge(eg 0.05)


# point estimate
mean(vyber)
# IO
t.test(vyber, alternative = 'two.sided', conf.level = 1-alpha)$conf.int

# *** b) Wilcoxn IC test ####
#  
# - we estimate the median - the point estimate is the sample median
#  
# - the data must come from a symmetric distribution
#  
# - exploratory: the slope lies in(-2,2)
#  
# - exploratory: the histogram looks approximately symmetrical
#  
# - exactly: using a statistical test, eg "lawstat" package, "symmetry.test(data,
# boot=FALSE)" function
#  
# - function in Rk requires additional parameter(conf.int=TRUE)
#  


vyber = runif(n = 30, min = 80, max = 120)
alpha = 0.05

# exploratory
moments::skewness(vyber)
hist(vyber, breaks = 5)

# exact: symmetry test
# install.packages("lawstat")
library(lawstat)
symmetry.test(vyber,boot=FALSE)$p.value
# the resulting p-value must be greater than hl. challenge(eg 0.05)


# point estimate
quantile(vyber, probs = 0.5)
median(vyber)
# IO
wilcox.test(vyber, alternative = 'two.sided', conf.level = 1-alpha, 
            conf.int = TRUE)$conf.int

# *** c) sign test IO test ####
#  
# - we estimate the median - the point estimate is the sample median
#  
# - larger range selection(>10)
#  
# - function in Rk requires additional parameter(conf.int=TRUE)
#  
# - requires "BSDA" library
#  
# - as the most robust test, it can also be used for discontinuous data - eg order in a
# list
#  


vyber = rexp(n = 30, rate = 1/100)
alpha = 0.05

# true median
qexp(p = 0.5, rate = 1/100)

# point estimate
# quantile(select, probs=0.5)
median(vyber)
# IO
# install.packages("BSDA")
library(BSDA)
SIGN.test(vyber, alternative = 'two.sided', conf.level = 1-alpha, 
          conf.int = TRUE)$conf.int

# * Measures of variability of one selection ####
#  
# By measures of variability we mean the data determining the dispersion/variability of
# the data, regardless of the total values. For data from the normal distribution, we
# can estimate the standard deviation.
#  
# *** IO standard deviations ####
#  
# - we estimate the standard deviation - the point estimate is the sample standard
# deviation
#  
# - the data must come from a normal distribution
#  
# - exploratory: skewness and sharpness lie in(-2,2)
#  
# - exploratory: The QQ graph has points approximately on the line
#  
# - exactly: using a statistical test, eg Shapiro-Wilk test(shapiro.test(data))
#  
# - requires "EnvStats" package
#  
# - function in Rku, gives the calculation of variance - the square root of the result
# is necessary
#  


vyber = rnorm(n = 30, mean = 100, sd = 10)
alpha = 0.05

# exploratory normality test
moments::skewness(vyber)
moments::kurtosis(vyber) - 3
qqnorm(vyber)
qqline(vyber)

# exact data normality test
shapiro.test(vyber)$p.value
# the resulting p-value must be greater than hl. challenge(eg 0.05)


# point estimate
sd(vyber)
# IO
# install.packages("EnvStats")
library(EnvStats)
sqrt(varTest(vyber, alternative = 'two.sided', conf.level = 1-alpha)$conf.int)

# Let's add a manual calculation:
#  
# - based on statistics: $\frac{S^2}{\sigma^2}(n-1) \sim \chi^2_{n-1}$
#  
# - Upper limit:
#  
# - $P(\frac{S^2}{\sigma^2}(n-1) < \chi^2_{\alpha /2, n-1}) = \alpha /2$
#  
# - $P(\frac{S^2}{\chi^2_{\alpha /2, n-1}}(n-1) < \sigma^2 ) = \alpha /2$
#  
# - Lower limit:
#  
# - $P(\frac{S^2}{\sigma^2}(n-1) > \chi^2_{1-\alpha /2, n-1}) = \alpha /2$
#  
# - $P(\frac{S^2}{\chi^2_{1-\alpha /2, n-1}}(n-1) > \sigma^2 ) = \alpha /2$
#  
# - Together: $P(\frac{S^2}{\chi^2_{1-\alpha /2, n-1}}(n-1) < \sigma^2
# <\frac{S^2}{\chi^2_{\alpha /2, n-1}}(n-1)) = 1 - \alpha$
#  


# manual calculation
alpha = 0.05
n = 30
S = sd(vyber)

hor_q = qchisq(1 - alpha/2, n-1)
dol_q = qchisq(alpha/2, n-1)
hor_q
dol_q

sqrt(S^2*(n-1)/dol_q)
sqrt(S^2*(n-1)/hor_q)

# * Probability of occurrence with one selection ####
#  
# *** IO probability ####
#  
# - we estimate the probability - the point estimate is the relative frequency
#  
# - we need enough data: $n>\frac{9}{p(1-p)}$
#  
# - Clopper - Pearson estimate(binom.test)
#  
# - does not take data as a parameter, but the number of successes and the number of
# observations
#  
# - Wald's - from selection characteristics
#  


pi = 0.3
n = 60
alpha = 0.05
vyber = runif(n = n, min = 0, max = 1) < pi

# verification of assumptions
p = mean(vyber)
p
9/(p*(1-p))

# point estimate
p
# Clopper - Pearson interval estimation
celk_count = length(vyber)
count_poz = sum(vyber)
binom.test(x = count_poz, n = celk_count, alternative = 'two.sided', 
           conf.level = 1 - alpha)$conf.int

# Wald's interval estimation
dol_q = qnorm(alpha/2)
hor_q = qnorm(1-alpha/2)

p - hor_q*sqrt(p*(1-p)/n)   # lower IO limit
p - dol_q*sqrt(p*(1-p)/n)       # upper IO limit


# Calculation of the 11 most frequently used confidence intervals param. bin. distribution
# using binom package
# install.packages("binom")
library(binom)
binom.confint(n = celk_count, x = count_poz)

#  Examples ####
#  
# * Example 1. ####
#  
# During control tests of 16 light bulbs, an estimate of the mean value of $\bar
# x$=3,000 hours and the standard deviation s=20 hours of their service life were
# determined. Assuming that the lamp life has a normal distribution, determine a 90%
# interval estimate for the µ and σ parameters
#  


# We estimate the mean value and standard deviation of the lamp life
# Part of the input is information about data normality

n = 16         # file range
x.bar = 3000   # hours.... average(point estimate of mean value)
s = 20         # hours.... sample standard deviation(point estimate of standard deviation)
alpha = 0.1    # significance level(reliability 1-alpha=0.9)


# Bilateral interval estimate of the mean
dol_q = qt(alpha/2,n-1)
hor_q = qt(1 - alpha/2,n-1)

x.bar - hor_q*s/sqrt(n)   # lower limit of IO
x.bar - dol_q*s/sqrt(n)   # upper limit of IO


# Bilateral interval estimate of the standard deviation
dol_q = qchisq(alpha/2,n-1)
hor_q =  qchisq(1 - alpha/2,n-1)

sqrt((n-1)*s^2/hor_q)      # lower IO limit
sqrt((n-1)*s^2/dol_q)      # upper limit of IO


# * Example 2. ####
#  
# The depth of the sea is measured with an instrument whose systematic error is zero and
# the random errors have a normal distribution with a standard deviation of 20 m.
#  


# We determine the estimate of the required selection range(number of required measurements)

# We assume data normality, with known variance(according to assignment)

sigma = 20   # meters.... known standard deviation
alpha = 0.05 # significance level(reliability 1-alpha=0.95)
delta = 10   # meters... permissible measurement error

# Estimate selection range
# Y=delta/sigma * sqrt(n)~N(0,1), delta=X-mu
# P(Y>Z_(1-alpha/2))=alpha/2

(qnorm(1 - alpha/2)*sigma/delta)^2 

# * Example 3. ####
#  
# The task is to determine the average serum cholesterol level in a certain population
# of men. In a random sample(derived from the normal distribution) of 25 men, the sample
# mean is 6.3 mmol/l and the sample standard deviation is 1.3 mmol/l.
#  


# We estimate mean serum cholesterol levels
# We assume data normality(according to assignment)

n = 25        # file range
x.bar = 6.3   # mmol/l.... average(point estimate of mean value)
s = 1.3       # mmol/l.... selection direction. deviation(point estimate of deviation)
alpha = 0.05  # significance level(reliability 1-alpha=0.95)


# Bilateral interval estimate of the mean
dol_q = qt(alpha/2, n-1)
hor_q = qt(1 -alpha/2, n-1)

x.bar - hor_q*s/sqrt(n)   # lower IO limit
x.bar - dol_q*s/sqrt(n)   # upper limit of IC


# * Example 4. ####
#  
# Suppose that in a random selection of 200 young men, 120 of them have higher than
# recommended serum cholesterol levels. Determine a 95% confidence interval for the
# percentage of young men with higher cholesterol levels in the population.
#  


# We estimate the proportion of men with higher cholesterol levels in the entire population,
# ie the probability that a randomly selected man will have a higher cholesterol level

n = 200  # file range
x = 120  # number of "successes"
p = x/n  # relative frequency(probability point estimate)
p
alpha = 0.05 # significance level(reliability 1-alpha=0.95)


# Verification of assumptions
9/(p*(1-p))

# Bilateral Clopper - Pearson(exact) int. Estimate param. binom. distribution
binom.test(x,n,alternative="two.sided",conf.level=0.95)$conf.int

# # Wald's(asymptotic) estimate(z-statistic) - approx. normal distribution according to CLV
dol_q = qnorm(alpha/2)
hor_q = qnorm(1-alpha/2)

p - hor_q*sqrt(p*(1-p)/n)   # lower IO limit
p - dol_q*sqrt(p*(1-p)/n)       # upper limit of IC


# * Example 5. ####
#  
# In a research study, we are working with a random selection of 70 women from the Czech
# population. Hemoglobin was measured in each of the women with an accuracy of 0.1 g/100
# ml. The measured values are listed in the Hemoglobin.xls file. Find 95% interval
# estimates of standard deviation and mean hemoglobin in the population of Czech
# women.(Check the normality based on the exploration graphs.)
#  


# # We estimate the mean and standard deviation of hemoglobin in serum

# # Read data from xlsx file(using readxl package)
library(readxl)
hem = read_excel("data/intervalove_odhady.xlsx",
                  sheet = "Hemoglobin")
colnames(hem) = "hodnoty"
head(hem)

# # Exploratory analysis
boxplot(hem$hodnoty)

# Data does not contain remote observations.
summary(hem$hodnoty)
sd(hem$hodnoty)

# Verification of normality - exploratory
qqnorm(hem$hodnoty)
qqline(hem$hodnoty)

moments::skewness(hem$hodnoty)
moments::kurtosis(hem$hodnoty) - 3
# Both skew and sharpness meet the standards. distribution.


# normality verification: exact - normality test.
# If we know hypothesis testing, we verify the Shapirs. Wilk's test.
shapiro.test(hem$hodnoty)$p.value
# In hl. significance 0.05


# 95% bilateral interval estimate of the mean
mean(hem$hodnoty)
t.test(hem$hodnoty, altarnative="two.sided", conf.level=0.95)$conf.int

# # 95% two-way interval standard deviation estimate
library(EnvStats)
sd(hem$hodnoty)

sqrt(varTest(hem$hodnoty, alternative = "two.sided", conf.level = 0.95)$conf.int)

# * Example 6. ####
#  
# What must be the number of observations if we want to determine the average hemoglobin
# value in newborns with an error of at most 1.0 $g/l$ with a probability of 0.95.
# Population variance is estimated at $g^2/l^2$.
#  


# We determine the estimated required range(number of newborns we have to test)

# We assume data normality, without this assumption the example is unsolvable

sigma = sqrt(46)  # g/l.... known standard deviation
alpha = 0.05      # significance level(reliability 1-alpha=0.95)
delta = 1         # g/l... permissible measurement error


# Estimate the range of the selection
# Y=delta/sigma * sqrt(n)~N(0,1), delta=X-mu
# P(Y>Z_(1-alpha/2))=alpha/2

(qnorm(1 - alpha/2)*sigma/delta)^2 

# * Example 7. ####
#  
# In the data file pr7.xlsx you will find the measurement of noise caused by the
# computer fan [dB]. Calculate the 95% interval estimate of the average noise and the
# 95% interval estimate of the noise variability.
#  


library(readxl)
# read data
data = read_excel("data/pr7.xlsx")
head(data)

length(data$dB)

# visualization
pom = boxplot(data$dB)

# removal of OP
data_op = data
data_op$dB[data_op$dB %in% pom$out] = NA
data_op = na.omit(data_op)
boxplot(data_op$dB)

# data normality test exploratory
moments::skewness(data_op$dB)
moments::kurtosis(data_op$dB) - 3

qqnorm(data_op$dB)
qqline(data_op$dB)

# normality test exactly
shapiro.test(data_op$dB)$p.value

# point and interval estimation of the mean
mean(data_op$dB)

t.test(data_op$dB, alternative = "two.sided", conf.level = 0.95)$conf.int

# point and interval estimation of the standard deviation
sd(data_op$dB)

sqrt(varTest(data_op$dB,alternative = "two.sided", conf.level = 0.95)$conf.int)

# * Example 8. ####
#  
# In the data file pr8.xlsx you will find the measurement of the time to failure of the
# electrical component [h]. Calculate the 99% interval estimate of the average life of a
# given component type.
#  


library(readxl)
# read data
data = read_excel("data/pr8.xlsx")
head(data)

length(data$cas_h)

# visualization and verification of OP
boxplot(data$cas_h)

hist(data$cas_h)

# data normality test exploratory
moments::skewness(data$cas_h)
moments::kurtosis(data$cas_h) - 3

qqnorm(data$cas_h)
qqline(data$cas_h)

# normality test exactly
shapiro.test(data$cas_h)$p.value

# symmetry test exploratory
moments::skewness(data$cas_h)
hist(data$cas_h)

# exact: symmetry test
# install.packages("lawstat")
library(lawstat)
symmetry.test(data$cas_h,boot=FALSE)$p.value
# the resulting p-value must be greater than hl. challenge(eg 0.05)


# median point and interval estimation
median(data$cas_h)
# IO
# install.packages("BSDA")
alpha = 0.01
library(BSDA)
SIGN.test(data$cas_h, alternative = 'two.sided', conf.level = 1-alpha, 
          conf.int = TRUE)$conf.int

sd(data$cas_h)



