#######################################################################################
########################## Hypothesis testing  ########################################
############################  Adéla Vrtková   #########################################
#######################################################################################

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
# point estimator
p=15/1000
p

# assumption
1000 > 9/(p*(1-p))

# testing the hypothesis
binom.test(15, 1000, p=0.01, alternative = "greater")
# We can't reject the null hypothesis. So we can conclude that the proportion of faulty products is only 1% and that it is not greater.

#####################################################
# Exercise 4 - teachers
data3=read.csv2(file="https://homel.vsb.cz/~vrt0020/statistics/teachers.csv")

# EDA - Outliers?
boxplot(data3) # it seems that students of Jones had better results
#
# EDA for both variables!

# Normality
shapiro.test(data3$Smith)
shapiro.test(data3$Jones)
# Interpretation: 

# Equal variances? Independence?
var.test(data3$Smith,data3$Jones)
# Interpretation: 

# Independence + Normality + UNequal variances
t.test(data3$Smith, data3$Jones, alternative="two.sided", var.equal = FALSE)
# Interpretation: 

#############################################################
# Exercise 5 - batteries
data4=read.csv2(file="https://homel.vsb.cz/~vrt0020/statistics/batteries.csv")
boxplot(data4) # it seems that new batteries are slightly better
#
# EDA for both variables!

# Normality
shapiro.test(data4$old)
shapiro.test(data4$new)
# Interpretation: 

# Equal variances? Independence?
var.test(data4$old,data4$new)
# Interpretation: 

# Independence + Normality + equal variances
t.test(data4$old, data4$new, alternative="less", var.equal = TRUE)
# Interpretation: 




