# ......................................................................................
# ........Exercise 10. Introduction to the hypothesis testing, one-sample tests ........
# ..........................Michal Béreš, Martina Litschmannová.........................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  From interval estimates to hypothesis tests ####
#  
# * What is a statistical hypothesis test? ####
#  
# Let's have the following:
#  
# - random variable X (for example men's height)
# - selection from a random variable (height measurement of 30 men)
#  
# Statistical testing of hypotheses decides the validity of statistical statement
# (hypothesis) based on the data obtained:
#  
# - $H_0$ - null hypotheses
# - $H_A$ - alternative hypotheses
#  
# For example:
# 
# $H_0$: $\mu_X = 175$
# 
# $H_A$: $\mu_X > 175$
# 
# Since this is a statistical decision, it will always be tied to some level of
# significance $\alpha$. We can always reach only 2 different decisions:
#  
# - I reject $H_0$ in favor of $H_A$
#     - this means that I claim that $H_0$ does not apply
#     - this decision is with the maximum error $\alpha$(significance level, type I
# error) - this means that we are able to influence the size of this error
# - I don't reject $H_0$
#     - this means that I claim that due to the obtained data(selection) it is not
# possible to reject $H_0$
#     - this decision is with error $\beta$(type II error), this error is not directly
# controllable and depends on the type of test used
#  
# How hypothesis tests relate to interval estimates and how the level of significance
# enters them will be shown in the next section.


# * Interval estimation and significance level ####
#  


data = readxl::read_excel("data/uvod.xlsx")
head(data)

options(repr.plot.width = 12) # width of graphs in Jupyter
par(mfrow = c(1, 2))          # graph graph matrices 1x2

boxplot(data$data)
hist(data$data)

moments::skewness(data$data)       # oblique
moments::kurtosis(data$data) - 3   # sharpness

shapiro.test(data$data)$p.value    # normality test


length(data$data)
mean(data$data)
sd(data$data)

# We make a 95% interval estimate of the mean using a t-test:
#  


t.test(data$data, alternative = "two.sided", conf.level = 0.95)$conf.int

# Now imagine that we want to test the hypothesis:$H_0$: $\mu = 100$$H_A$: $\mu
# \neq 100$What would be the decision with respect to the calculated IO and so the
# significance level $\alpha = 0.05$?
#  
# Well, confidence interval covers the value 100 with maximal error of 5% (confidence
# 95%). Therefore, we can say we cannot reject this hypothesis at significance value 5%.


# Let's further imagine that we want to test the hypothesis:$H_0$: $\mu =
# 105$$H_A$: $\mu \neq 105$What would be the decision with respect to the
# calculated IO and so the significance level $\alpha = 0.05$?
#  
# Well, now its a different case: confidence interval does not cover the value 105 with
# maximal error of 5% (confidence 95%). Therefore, we can say we can reject this
# hypothesis at significance value 5%.


# **What we just did is called a classic test.**
#  
# We will show you more classic tests for one-sided alternatives.$H_0$: $\mu =
# 105$$H_A$: $\mu > 105$
#  


t.test(data$data, alternative = "greater", conf.level = 0.95)$conf.int

# $H_0$: $\mu = 105$$H_A$: $\mu < 105$
#  


t.test(data$data, alternative = "less", conf.level = 0.95)$conf.int

# Note that the first of these one-sided alternatives led to a "rejection" of $H_0$.
# This is because of the comparison of the unlikely $H_0$ with the even less likely
# $H_A$.
#  
# *** P-values and connection with CI ####
#  
# An alternative to the classical test(where we create CI) is the so-called pure
# significance test:


# H_0: mu=105
# H_A: mu<>105
t.test(data$data, mu = 105, alternative = "two.sided")

t.test(data$data, mu = 105, alternative = "two.sided")$p.value

# The pure significance test results in a p-value. Based on it, we decide whether or not
# to reject $H_0$.
# 
# p-value can be understood as the highest possible level of significance, such that our
# decision is - I do not reject. Thus, the CI/field of acceptance would contain the
# examined value:


# H_0: mu=105
# H_A: mu<>105

p.hod = t.test(data$data, mu = 105, alternative = "two.sided")$p.value
p.hod

t.test(data$data, alternative = "two.sided", conf.level = 1 - p.hod)$conf.int

# H_0: mu=105
# H_A: mu>105

p.hod = t.test(data$data, mu = 105, alternative = "greater")$p.value
p.hod

t.test(data$data, alternative = "greater", conf.level = 1 - p.hod)$conf.int

# H_0: mu=105
# H_A: mu<105

p.hod = t.test(data$data, mu = 105, alternative = "less")$p.value
p.hod

t.test(data$data, alternative = "less", conf.level = 1 - p.hod)$conf.int

# * Overwiev of tests for one sample ####
#  
# ** Position measures ####
#  
# By position measures we mean the data that determines the position of the data. For
# data from the normal distribution we can estimate the mean value, for others the
# median.
#  
# *** a) student's t-test ####
#  
# - we test the mean value
# - the data must come from a normal distribution
# - exploratory: skewness and sharpness lie in(-2,2)
# - exploratory: The QQ graph has points approximately on the line
# - exact: using a statistical test, eg Shapiro-Wilk test(shapiro.test(data))


# H_0: mu=100
# H_A: mu<>100
t.test(data$data, mu = 100, alternative = 'two.sided')$p.value

# H_0: mu=100
# H_A: mu>100
t.test(data$data, mu = 100, alternative = 'greater')$p.value

# H_0: mu=100
# H_A: mu<100
t.test(data$data, mu = 100, alternative = 'less')$p.value

# *** b) Wilcoxn test ####
#  
# - we test the median
# - the data must come from a symmetric distribution
# - exploratory: skewness lies in(-2,2)
# - exploratory: histogram looks approximately symmetrical


# H_0: X_0.5=100
# H_A: X_0.5<>100
wilcox.test(data$data, mu = 100, alternative = 'two.sided')$p.value

# H_0: X_0.5=100
# H_A: X_0.5>100
wilcox.test(data$data, mu = 100, alternative = 'greater')$p.value

# H_0: X_0.5=100
# H_A: X_0.5<100
wilcox.test(data$data, mu = 100, alternative = 'less')$p.value

# *** c) sign test test ####
#  
# - we test the median
# - larger range selection(>10)
# - requires "BSDA" library
# - as the most robust test, it can also be used for discontinuous data


# H_0: X_0.5=100
# H_A: X_0.5<>100
BSDA::SIGN.test(data$data, md = 100, alternative = 'two.sided')$p.value

# H_0: X_0.5=100
# H_A: X_0.5>100
BSDA::SIGN.test(data$data, md = 100, alternative = 'greater')$p.value

# H_0: X_0.5=100
# H_A: X_0.5<100
BSDA::SIGN.test(data$data, md = 100, alternative = 'less')$p.value

# ** Variability measures ####
#  
# By measures of variability we mean the data determining the dispersion/variability of
# the data. For data from the normal distribution, we can estimate the standard
# deviation.
#  
# *** standard deviation test ####
#  
# - we test the standard deviation
# - the data must come from a normal distribution
# - exploratory: skewness and kurtosis lie in(-2,2)
# - Explosive: The QQ graph has points approximately on the line
# - exact: using a statistical test, eg Shapiro-Wilk test(shapiro.test(data))
# - requires "EnvStats" package
# - function in R, compares variance !!!


# H_0: sigma=10
# H_A: sigma<>10
EnvStats::varTest(data$data, sigma.squared = 10*10, 
                  alternative = 'two.sided')$p.value

# H_0: sigma=10
# H_A: sigma>10
EnvStats::varTest(data$data, sigma.squared = 10*10, 
                  alternative = 'greater')$p.value

# H_0: sigma=10
# H_A: sigma<10
EnvStats::varTest(data$data, sigma.squared = 10*10, 
                  alternative = 'less')$p.value

# * Probability ####
#  
# *** Test of probability ####
#  
# - We test the probability
# - We require sufficient data: $n>\frac{9}{p(1-p)}$
# - Clopper's - Pearson's estimate(binom.test)
# - does not take data as a parameter, but the number of successes and the number of
# observation


pi = 0.3
data_bin = runif(n = 100, min = 0, max = 1) < pi

n = length(data_bin)
x = sum(data_bin)
n
x

# H_0: pi=0.2
# H_A: pi<>0.2
binom.test(x = x, n = n, p = 0.2, alternative = 'two.sided')$p.value

# H_0: pi=0.2
# H_A: pi>0.2
binom.test(x = x, n = n, p = 0.2, alternative = 'greater')$p.value

# H_0: pi=0.2
# H_A: pi<0.2
binom.test(x = x, n = n, p = 0.2, alternative = 'less')$p.value

#  Examples ####


library(dplyr)
library(rstatix)

#  
# * Example 1. ####
#  
# We have a selection of 216 patients and we measured their protein serum(file
# testy_jednovyberove.xlsx sheet bilk_serum). Verify that the average protein
# serum(Albumin) of all patients of this type(population average µ) differs
# statistically significantly from 35 g/l.


# Reading data from xlsx file(using readxl package)
albumin = readxl::read_excel("data/testy_jednovyberove.xlsx",
                             sheet = "bilk_serum")
head(albumin)

colnames(albumin)="value"

# Exploratory analysis
boxplot(albumin$value)
summary(albumin$value)

length(albumin$value) # sd is rounded to 3 valid digits
sd(albumin$value)     # sd and position measures are rounded to the nearest thousandth

# **Position measurement test**
#  


# Verification of normality - exploratory
moments::skewness(albumin$value)    # skew
moments::kurtosis(albumin$value)-3  # sharpness

options(repr.plot.width = 12) # width of graphs in Jupyter
par(mfrow = c(1, 2))          # matrix of 1x2 graphs

qqnorm(albumin$value)
qqline(albumin$value)
hist(albumin$value)

# We will use the normality test for the final decision on data normality.

# The presumption of normality is verified by the Shapir - Wilk test.
# H0: Data is a selection from the normal distribution.
# Ha: Data is not a selection from the normal distribution.
shapiro.test(albumin$value)
# p-value>0.05 ->Na hl. significance of 0.05, the assumption of normality cannot be rejected.


# normal OK ->t.test

# H0: mu=35 g/l
# Ha: mu<>35 g/l

t.test(albumin$value, mu=35, alternative = "two.sided")

# p-value<0.05 ->at significance level of 0.05 we reject the null hypothesis
# in favor of the alternative hypothesis
# The mean albumin value differs statistically significantly from 35 g/l.

# * Example 2. ####
#  
# Survival times for 100 lung cancer patients treated with the new drug are listed in
# the tests_jednovyberove.xlsx sheet "preziti". It is known from previous studies that
# the average survival of such patients without the administration of a new drug is 22.2
# months. Can these data suggest that the new drug prolongs survival?
#  


# Reading data from xlsx file(using readxl package)
preziti = readxl::read_excel("data/testy_jednovyberove.xlsx",
                             sheet = "preziti")   
head(preziti)

colnames(preziti)="value"

# # Exploratory analysis
par(mfrow = c(1, 2))          # graph matrix 1x2

boxplot(preziti$value)
hist(preziti$value)

# **Data contains outliars -> we can delete them. Or note that this is probably an
# exponential distribution and the outliars are not actually there(the distribution
# simply behaves this way.)**
#  


# Data contains outliars. We can list them with the help of f-ce boxplot.
preziti$ID = seq(1,length(preziti$value))
outliers = preziti %>% identify_outliers(value)
outliers
# if we decided to remove outliers, then
preziti$value_no_outliars = ifelse(preziti$ID %in% outliers$ID,NA,preziti$value)

# Exploratory analysis for data without remote observations
boxplot(preziti$value_no_outliars)

length(na.omit(preziti$value_no_outliars))   # sd is rounded to 3 valid digits
sd(preziti$value_no_outliars,na.rm=TRUE)     # sd and position measurements round. to tenths

# **Position measure(mean/median) test**
#  


# Verification of normality - exploratory
moments::skewness(preziti$value_no_outliars,na.rm=TRUE)
moments::kurtosis(preziti$value_no_outliars,na.rm=TRUE)-3

par(mfrow = c(1, 2))          # graph matrix 1x2

qqnorm(preziti$value_no_outliars)
qqline(preziti$value_no_outliars)
hist(preziti$value_no_outliars)

# QQ - graph and history show that the choice of truth. is not a choice of standards. distribution.
# Slanting and pointing corresponds to standards. distribution.
# we will use the normality test.


# We verify the assumption of normality by the Shapirs. Wilkov's test.
shapiro.test(preziti$value_no_outliars)
# p-value<0.05 ->at significance 0.05, we reject the assumption of normality


# exploratory assessment of symmetry - exponential distribution - no symmetry

# normality rejected ->symmetry rejected ->Sign. test
# H0: median=22.2 months
# Ha: median>22.2 months

BSDA::SIGN.test(preziti$value_no_outliars, md=22.2,
                alternative="greater")

# p-value>0.05 -> at significance of 0.05, the null hypothesis cannot be rejected
# Median survival time is not statistically significantly greater than 22.2 months.

median(preziti$value_no_outliars, na.rm = TRUE)

# * Example 3. ####
#  
# The machine produces piston rings of a given diameter. The manufacturer states that
# the standard deviation of the ring diameter is 0.05 mm. To verify this information, 80
# rings were randomly selected and their diameter was measured(file
# testy_jednovyberove.xlsx sheet krouzky). Can the results obtained be considered
# statistically significant in terms of improving the quality of production?


# Reading data from xlsx file(using readxl package)
krouzky = readxl::read_excel("data/testy_jednovyberove.xlsx",
                             sheet = "krouzky")  
head(krouzky)

colnames(krouzky)="value"

# # Exploratory analysis
boxplot(krouzky$value)

# Data contains outliars. We can list them with the help of f-ce boxplot.
krouzky$ID = seq(1,length(krouzky$value))
outliers = krouzky %>% identify_outliers(value)
outliers
# if we decided to remove outliers, then
krouzky$value_no_outliars = ifelse(krouzky$ID %in% outliers$ID,NA,krouzky$value)

# Exploratory analysis for data without remote observations
summary(krouzky$value_no_outliars,na.rm=TRUE)
boxplot(krouzky$value_no_outliars)

length(na.omit(krouzky$value_no_outliars))# sd is rounded to 3 valid digits
sd(krouzky$value_no_outliars,na.rm=TRUE)  # sd and position measures round. per thousandths


# Verification of normality - exploratory
moments::skewness(krouzky$value_no_outliars,na.rm=TRUE)
moments::kurtosis(krouzky$value_no_outliars,na.rm=TRUE)-3

par(mfrow = c(1, 2))          # matrix of 1x2 graphs

qqnorm(krouzky$value_no_outliars)
qqline(krouzky$value_no_outliars)
hist(krouzky$value_no_outliars)
# Both skew and sharpness comply with standards. distribution.
# We will use for the final decision on data normality


# normality test.
# We verify the assumption of normality by the Shapirs. Wilkov's test.
shapiro.test(krouzky$value_no_outliars)
# p-value>0.05 ->Na hl. significance of 0.05 cannot be assumed norms. reject


# variability test ->variance test

# H0: sigma=0.05 mm
# Ha: sigma<0.05 mm
EnvStats::varTest(krouzky$value_no_outliars, sigma.squared = 0.05^2,
                  alternative = "less")

# p-value<0.05 ->At the significance level of 0.05 we reject H0 in favor of Ha


# How to find a 95% interval standard deviation estimate?
pom = EnvStats::varTest(krouzky$value_no_outliars,sigma.squared = 0.05^2,
                        alternative = "less", conf.level=0.95)

sqrt(pom$conf.int)

# * Example 4. ####
#  
# TT states that 1% of their resistors do not meet the required criteria. 15 unsuitable
# resistors were found in the tested delivery of 1000 pieces. Is this result with
# agreement with TT's assertion or can we reject it? 


n = 1000   # selection range
x = 15     # number of "successes"
p = x/n    # relative frequency(probability point estimate)
p 

# Verification of assumptions
9/(p*(1-p))

# Clopper - Pearson(exact) test
# H0: pi=0.01
# Ha: pi<>0.01

binom.test(x = x, n= n, p = 0.01, alternative="two.sided")

