#######################################################################################
########################## Hypothesis testing  ########################################
############################  Adéla Vrtková   #########################################
#######################################################################################

# This material lacks formal notation of the null and the alternative hypothesis!!!
# In this script you will find the necessary commands + comments to the interpretation.
# It is crucial to look at the solution in the context of the exercises from the presentation!!!

##################################################
# Exercise 1 - Cholesterol
# Import cholesterol.RData

# Empirical tools - outliers? symmetry? normality?
boxplot(cholesterol$values)
hist(cholesterol$values)

moments::skewness(cholesterol$values)
moments::kurtosis(cholesterol$values)-3
qqnorm(cholesterol$values)
qqline(cholesterol$values)

# Exact tool for normality! Shapiro-Wilk test!
shapiro.test(cholesterol$values)
# Interpretation! We don't reject the normality-assumption (p-value of Shapiro-Wilk test is greater than 0,05).

# Normality wasn't rejected -> One Sample T-test
t.test(cholesterol$values, mu=175, alternative = "two.sided")
# We reject the null hypothesis against the alternative. 
# Interpretation! It means that the mean cholesterol level is different from the value 175.

##################################################
# Exercise 2 - Survival Time
# Import time.RData

# Empirical tools - outliers? symmetry? normality?
boxplot(time$values)
hist(time$values)

moments::skewness(time$values)
moments::kurtosis(time$values)-3
qqnorm(time$values)
qqline(time$values)

# Exact tool for normality! Shapiro-Wilk test!
shapiro.test(time$values)
# We reject the normality - assumption (p-value of Shapiro-Wilk test is under 0,05).
# We can't use the t-test about the mean, so we have to settle with Wilcoxon test about the median (symmetry is ok, skewness is very close to 0)

# Normality was rejected -> One-sample Wilcoxon test
wilcox.test(time$values, mu=4, alternative = "greater")
# We reject the null hypothesis against the alternative. 
# Interpretation! It means that the median survival time is truly greater than 4 years.
# So we can conclude that the drug prolongs the survival.

##################################################
# Exercise 3 - Products
# Estimating the proportion
p=15/1000
p

# Assumption
1000 > 9/(p*(1-p))

# Testing the hypothesis
binom.test(15, 1000, p=0.01, alternative = "greater")
# We can't reject the null hypothesis. 
# So we can conclude that the proportion of faulty products is only 1% and that it is not greater.



