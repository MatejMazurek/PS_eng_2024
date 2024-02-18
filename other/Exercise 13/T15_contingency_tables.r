# ......................................................................................
# ...............Exercise 13. Nonparametric tests, goodness-of-fit tests ...............
# ..................Michal Béreš, Martina Litschmannová, Adéla Vrtková..................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  Conformance distribution probability testing of discrete NV(finite number of values) ####
# - good agreement test
#  
# - we test whether the measured data(their relative frequencies) agree with any
# specific distribution(ie its probabilities)
#  
# - we test using the $\chi^2$ good match test
#  
# - assumptions of the test:(ATTENTION relate to the expected frequencies - ie those
# that we would monitor if the measured data were 100% according to the distribution in
# the hypothesis)
#  
# - Expected frequencies ≥ 2,
#  
# - at least 80% of expected frequencies>5
#  
# - test statistic(the one with $\chi^2$ distribution) is $G = \sum_{i = 1}^k (O_i -
# E_i)^2 / E_i$
#  
# - distribution has degree of freedom $df = k - 1 - h$
#  
# - k is the number of options
#  
# - h is the number of estimated parameters(this applies to incompletely specified
# tests)
#  


# example see example 1


# * Good match test for a continuous random variable(or discrete with an infinite ####
# number of values)
#  
# - we have to convert to a table with a finite number of values
#  
# - for discrete(eg poison) we group from a certain number of columns eg 4,5,6,... to "4
# and more"
#  
# - for continuous we produce a series of intervals and see how many values fall within
# the given interval
#  
# - eg:(- $\infty$, 3),<3, 4),...,<10, $\infty$)
#  
# - then we have to calculate for each interval how many% of data belong to each
# interval, thus obtaining a table of expected probabilities
#  
# - we continue as before
#  
# - there is a pearson.test(data) function from the nortest package to test the
# normality of the distribution
#  


# example 2,3,4


#  PivotTables ####
#  
# - tables containing data depending on two factors
#  
# - one of the factors is, as a rule, an independent variable in which we monitor
# whether it affects the other factor(dependent variable)
#  
# - The independent variable is usually in rows
#  
# - is usually dependent in columns
#  
# - beware the whole test examines the correlation, not the causality! Causality can be
# assessed by "expert" evaluation
#  
# - statistical conclusion: there is a statistically significant dependence between the
# independent and dependent variable(correlation)
#  
# - expert assessment: an independent variable statistically significantly affects the
# dependent variable(causality)
#  


# * PivotTable Visualization ####
#  
# - visualization eg using the barplot function
#  
# - pay attention to what rows and columns are, we always want individual divided
# columns to be via independent variables(each column for one value of an independent
# variable)
#  
# - beside=T determines whether we want to merge adjacent columns into one split column
# or not
#  
# - preferred visualization using mosaicplot
#  
# - the same as for barlot, the connected columns must be via independent variables
#  


# examples in Examples 5,6,7


# * Dependency table dependencies ####
#  
# - Correlation coefficient CC
#  
# - Corrected correlation coefficient CCcor
#  
# - Cramer's coefficient V
#  
# - we will use this first
#  
# - cramersV(cont.tab) function from lsr package
#  


# Examples in Examples 5,6,7


# * PivotTable Dependency Test ####
#  
# - $H_0:$ there is no dependency between an independent(eg is a smoker) and a
# dependent(eg suffering from a disease) variable
#  
# - $H_A: \neg H_0$
#  
# - chisq.test function(cont.tab)
#  
# - assumptions: Expected frequencies ≥ 2, at least 80% of expected frequencies>5
#  
# - expected frequencies can be found from chisq.test(cont.tab) \ $ expected
#  


#  Association tables ####
#  
# - this is a special PivotTable case
#  
# - always has exactly 2 options for the dependent and exactly 2 options for the
# independent variable
#  


# * Mandatory form of the association table ####
#  
# - Lines indicate independent variable options
#  
# - the first line is the so-called exposed part of the population(the one exposed to
# the phenomenon we study - eg smokers try to study the effects of smoking)
#  
# - the second line is the unexposed part of the population
#  
# - The columns indicate the options of the dependent variable
#  
# - the first column indicates the occurrence of the phenomenon under investigation(eg
# occurrence of the disease, product error,...)
#  
# - the second column indicates the remainder - no occurrence of the investigated
# phenomenon
#  


# * Relative risk and odds ratio ####
#  
# - Relative risk and odds ratio provide the same information, only in a different
# format
#  
# - all point IOs are calculated using the function epi.2by2(associ.tab) from the
# package epiR
#  
# - the function takes the association table as input, which must be in the correct
# format!
#  


# ** Relative risk ####
#  
# - denotes $RR$
#  
# - this is the risk ratio(probability of occurrence of the investigated phenomenon) in
# exposed and unexposed populations
#  
# - if it is equal to 1, it means the same probabilities of occurrence in exposed and
# unexposed
#  
# - if it is greater than 1 then the exposed population is more likely to occur
#  
# - if less than 1 then the exposed population is less likely to occur
#  
# - point estimate $\hat{RR}$ is calculated as the ratio rel. frequency of the studied
# phenomenon in exposed and unexposed populations
#  
# - epi.2by2 provides interval estimates
#  
# - if the IO does not contain the value 1 then there is a statistically significant
# dependence between the dependent and independent variable
#  


# ** Odds ratio ####
#  
# - we denote $OR$
#  
# - this is the ratio of chances(chance of occurrence of the studied phenomenon) in
# exposed and unexposed populations
#  
# - if it is equal to 1, it means the same chances of occurrence in exposed and
# unexposed
#  
# - if it is greater than 1 then the exposed population has a greater chance of
# occurrence
#  
# - if less than 1 then the exposed population has a lower chance of occurrence
#  
# - point estimate $\hat{OR}$ is calculated as the ratio of chances(selective) of the
# studied phenomenon in exposed and unexposed population
#  
# - epi.2by2 provides interval estimates
#  
# - if the IO does not contain the value 1 then there is a statistically significant
# dependence between the dependent and independent variable
#  


# example in example 7


#  Examples(good match tests) ####
#  
# * Example 1. ####
#  
# The dice were rolled 6,000 times and the number of falling meshes was recorded.
# [image.png](attachment:64f1169e-6bc1-470a-8afb-b282230c2c9f.png)
#  


# H0: The cube is fair.(so all probabilities are 1/6)
# Ha: The cube is not fair.(H0 negation)

x = c(1,2,3,4,5,6)
n.obs = c(979,1002,1015,980,1040,984)
p.exp = c(1/6,1/6,1/6,1/6,1/6,1/6)
n.exp = 6000*p.exp
n.exp # test assumptions must be checked
# All expected frequencies are greater than 5.


x.obs = sum(((n.obs-n.exp)^2)/n.exp)
x.obs

p.hodnota = 1 - pchisq(x.obs,5)
p.hodnota

# At the significance level of 0.05 we do not reject HO(p-value=0.711,
# Chi-square test of independence, df=5).


# * Example 2. ####
#  
# The manufacturing company estimates the number of failures of a particular device in
# 100 hours using a Poisson distribution with parameter 1.2. Employees recorded the
# actual number of failures at a total of 150 100-hour intervals for inspection(results
# are shown in the table). Verify with a clean significance test that the number of
# failures of a given device within 100 hours actually has a Poisson distribution with
# the parameter λt=1.2.
#  


# Fully specified test

# H0: The number of faults during 100 operating hours can be modeled
# Poisson distribution with parameter 1.2.
# Ha: The number of faults during 100 operating hours cannot be modeled
# Poisson distribution with parameter 1.2.

x = c(0,1,2,3,4)
n.obs = c(52,48,36,10,4)

p.exp = dpois(x,1.2)
p.exp[5] = 1 - sum(p.exp[1:4])
p.exp
sum(p.exp)

n.exp = 150*p.exp
n.exp # test assumptions must be checked
# 4 of the 5 expected frequencies, ie 80%, are greater than 5.


x.obs = sum(((n.obs-n.exp)^2)/n.exp)
x.obs

p.hodnota = 1-pchisq(x.obs,4)
p.hodnota

# At the significance level of 0.05 we do not reject HO(p-value=0.590,
# Chi-square test of independence, df=4).


# * Example 3. ####
#  
# Employees recorded a total of 150 100-hour breakdowns at check(results are shown in
# the table). Use a clean significance test to see if the number of failures in a given
# device has a true Poisson distribution in 100 hours. </br> 
#  ![Image.png](attachment:4da89057-87d1-4bcd-a488-3365237654f7.png)
#  


# Incompletely specified test

# H0: The number of faults during 100 operating hours can be modeled
# Poisson distribution.
# Ha: The number of faults during 100 operating hours cannot be modeled
# Poisson distribution.

x = c(0,1,2,3,4)
n.obs = c(52,48,36,10,4)

lambda.t = weighted.mean(x,n.obs)   # Poisson distribution parameter estimate
lambda.t

p.exp = dpois(x,lambda.t)
p.exp[5] = 1 - sum(p.exp[1:4])
p.exp
sum(p.exp)

n.exp = 150*p.exp
n.exp # test assumptions must be checked
# 4 out of 5 expected frequencies, ie 80%, are greater than 5.


x.obs = sum(((n.obs-n.exp)^2)/n.exp)
x.obs

p.hodnota = 1-pchisq(x.obs,3)
p.hodnota

# At the significance level of 0.05 we do not reject HO(p-value=0.491,
# Chi-square test of independence, df=3).


# * Example 4. ####
#  
# Time intervals(s) between the passages of individual vehicles were measured on the
# motorway within a few minutes. The detected values of these distances are recorded in
# the file dalnice.xlsx. Verify that this is data from a normal distribution(use a good
# match test).
#  


# automatic test of good agreement from continuous data

dalnice = readxl::read_excel("data/neparametricke_hypotezy.xlsx", sheet=2)
colnames(dalnice)="hodnoty"
head(dalnice)

mu = mean(dalnice$hodnoty)
sigma = sd(dalnice$hodnoty)
mu 
sigma

# Generate values for the x-axis
xfit=seq(from = -20, to = 30, length = 100)   
# Generate values for the y-axis
yfit=dnorm(xfit, mean = mu, sd = sigma)  

hist(dalnice$hodnoty, freq = FALSE, xlim = c(-20, 30))
# Add a curve to the last graph based on the values generated above
lines(xfit, yfit, col="black", lwd=2)    

# install.packages("nortest")


# H0: Spacing between vehicles can be modeled by normal distribution.
# Ha: Spacing between vehicles cannot be modeled by normal distribution.

nortest::pearson.test(dalnice$hodnoty)

# Specify the number of degrees of freedom
pom = nortest::pearson.test(dalnice$hodnoty, n.classes = 5)
attributes(pom)

pom$method
pom$n.classes
pom$df
pom$p.value

# HO can be rejected at the significance level of 0.05(p-value<<0.001,
# Chi-square test of good agreement, df=12).


# test what you already know
shapiro.test(dalnice$hodnoty)

#  Examples of PivotTable and Association Tables ####
#  
# * Example 5. ####
#  
# Decide on the basis of the data file experimentovani-s-telem.xls(Dudová, J. -
# Experimentování s tělem(survey results), 2013. Available online at
# http://experimentovani-stelem.vyplnto.cz), whether there is a connection between
# gender of respondents and whether they have tattoos. Use Cramer V to assess the
# contingency rate.
#  


tet = readxl::read_excel("data/neparametricke_hypotezy.xlsx", sheet=3)
head(tet)

tet = tet[,c(6,10)]
colnames(tet) = c("pohlavi","tetovani")
head(tet)

# Preprocessing
# Variants of cat. Variables(factors) must be arranged and named so
# how they should be arranged and named in the accounts. table


kont.tab = table(tet$pohlavi, tet$tetovani)
kont.tab

colnames(kont.tab) = c("má tetování","nemá tetování")
kont.tab

# Exploratory analysis
prop.table(kont.tab)    # associated relative frequencies
prop.table(kont.tab,1)  # line relative frequencies
prop.table(kont.tab,2)  # columnar relative frequencies


# Visualization in standard R
# Cluster bar graph
# compare graphs, which of the graphs is more suitable for the presentation of the data

options(repr.plot.width = 12) # width of graphs in Jupyter
par(mfrow = c(1, 2))          # matrix of 1x2 graphs

barplot(t(kont.tab),
        legend = colnames(kont.tab),
        beside = T)

barplot(kont.tab,
        legend = rownames(kont.tab),
        beside = T)

# Stacked bar graph

options(repr.plot.width = 12) # width of graphs in Jupyter
par(mfrow = c(1, 2))          # matrix of graphs 1x2

barplot(t(kont.tab),
        legend = colnames(kont.tab))

barplot(kont.tab,
        legend = rownames(kont.tab))

# Mosaic chart

options(repr.plot.width = 8) # width of graphs in Jupyter

mosaicplot(t(kont.tab),
           las = 1, # Rotate y-axis labels
           color = gray.colors(2))

# compare which of the graphs is more suitable for the presentation of the given data
mosaicplot(kont.tab,
           las = 1,
           color = gray.colors(2))

# install.packages("lsr")


# Cramer V calculation ####
lsr::cramersV(kont.tab)

# PivotTable independence test

# H0: Data is independent ->whether the individual is male or female
# Does not affect his likelihood of having a tattoo
# HA: negation H0(there is a dependency)

pom = chisq.test(kont.tab)
attributes(pom)

pom$expected # Necessary for verification of assumptions
# All expected frequencies are greater than 5.


pom

# HO can be rejected at the significance level of 0.05(p-value=0.003,
# Chi-square goodness-of-fit test, df=1).
# The observed dependence can be assessed as weak(Cramer's V=0.121).


# * Example 6. ####
#  
# For a differentiated approach in personnel policy, the company's management needs to
# know whether job satisfaction depends on whether it is a Prague plant or non-Prague
# plants. The results of the survey are in the following table. Display the data using a
# mosaic chart and, based on the independence test in the combination table, decide on
# the dependence of job satisfaction on the company's location. To assess the
# contingency rate, use Cramer V. </br> 
#  ![Image.png](attachment:ebc6062c-1020-4fab-8855-6665d65d59a7.png)
#  


# We do not have a data matrix, ie cont. we must enter the table "manually"
kont.tab = matrix(c(10,25,50,15,20,10,130,40),
                  nrow=2,byrow=T)
rownames(kont.tab) = c("Praha","Venkov")
colnames(kont.tab) = c("velmi nespokojen","spíše nespokojen",
                       "spíše spokojen","velmi spokojen")
kont.tab = as.table(kont.tab)
kont.tab

# Exploratory Analysis ####
prop.table(kont.tab)  # associated relative frequencies
prop.table(kont.tab,1)  # line relative frequencies
prop.table(kont.tab,2)  # columnar relative frequencies


# Visualization in standard R
# Mosaic chart
mosaicplot(kont.tab,
           las = 1, # Rotate yo axis labels 90
           color = gray.colors(4))

# Cramer V
lsr::cramersV(kont.tab)

# H0: There is no connection between job satisfaction and company location.
# Ha: There is a connection between job satisfaction and the location of the company.

# Chi-square PivotTable Independence Test ####
pom = chisq.test(kont.tab)
pom$expected
# All expected frequencies are greater than 5.


pom

# HO can be rejected at the significance level of 0.05(p-value<<0.001,
# Chi-square test of good agreement, df=3).
# The observed dependence can be assessed as moderate(Cramer's V=0.296)


# * Example 7.(Association table) ####
#  
# Between 1965 and 1968, a cohort study of cardiovascular disease under the Honolulu
# Heart Program began monitoring 8,006 men, of whom 7,872 did not have a history of
# stroke at the start of the study. Of this number, there were 3,435 smokers and 4,437
# non-smokers. When followed for 12 years, 171 men in the group of smokers and 117 men
# in the group of non-smokers suffered a stroke.
#  
# *** a) ####
#  
# Record the results in the association table.
#  


kont.tab = matrix(c(171,3264,117,4320),nrow=2,byrow=T)
rownames(kont.tab) = c("kuřák","nekuřák")
colnames(kont.tab) = c("ano","ne")
kont.tab

# completion of the table of absolute frequencies
kont.tab.full = matrix(rep(0,9), nrow=3, ncol=3)
rownames(kont.tab.full) = c("kuřák", "nekuřák", "sum")
colnames(kont.tab.full) = c("ano", "ne", "sum")

kont.tab.full[1:2, 1:2] = kont.tab
kont.tab.full[1:2, 3] = rowSums(kont.tab)
kont.tab.full[3, 1:2] = colSums(kont.tab)
kont.tab.full[3, 3] = sum(kont.tab) 
kont.tab.full

# addition of the table of relative frequencies
kont.tab.rel = matrix(rep(0,9), nrow=3, ncol=3)
rownames(kont.tab.rel) = c("kuřák", "nekuřák", "sum")
colnames(kont.tab.rel) = c("ano", "ne", "sum")

kont.tab.rel[1:2, 1:2] = prop.table(kont.tab)
kont.tab.rel[1:2, 3] = rowSums(kont.tab.rel[1:2, 1:2])
kont.tab.rel[3, 1:2] = colSums(kont.tab.rel[1:2, 1:2])
kont.tab.rel[3, 3] = sum(kont.tab.rel[1:2, 1:2]) 
kont.tab.rel

# *** b) ####
#  
# Based on visual assessment, estimate the effect of smoking on the incidence of
# cardiovascular disease.
#  


# Visualization by mosaic graph in basic R
mosaicplot(kont.tab,
           color = gray.colors(2))

# Cramer V calculation ####
lsr::cramersV(kont.tab)

# According to the mosaic graph and Cramer's V(0.061) there is a connection between smoking
# and the occurrence of apoplexy evaluated as very weak.


# *** c) ####
#  
# Determine the absolute risk of cardiovascular disease in smokers and non-smokers.
#  


# risk=probability
kont.tab.full

# Smokers
# Assumptions check
p = 171/3435
p

9/(p*(1-p)) 
# OK(3,435>190.3)


# Calculation of point and 95% Clopper-Pearson interval estimation
prop.test(x = 171, n = 3435)

# The smoker has an apoplexy risk of about 5.0%. 95% Clopper-Pearson
# the interval estimate of this risk is 4.2% to 5.8%.


# Non-smokers
# Assumption check
p = 117/4437
p

9/p/(1-p) 
# OK(4,437>350.6)


# Calculation of point and 95% Clopper-Pearson interval estimation
prop.test(117,4437)

# In non-smokers, the risk of apoplexy is about 2.6%. 95% Clopper-Pearson
# the interval estimate for this risk is 2.1% to 3.2%.


# *** d) ####
#  
# Determine the relative risk(including 95% of the interval estimate) of cardiovascular
# disease in smokers and non-smokers. Explain the practical significance of the results
# obtained.
#  


# install.packages("epiR")


kont.tab

epiR::epi.2by2(kont.tab)

# Smokers have about 1.89 times higher risk of apoplexy than non-smokers. 95%
# the interval estimate for this relative risk is 1.50 to 2.38.
# According to the interval estimate of relative risk, it is clear that at the surface
# significance 0.05 is a statistically significantly higher risk for smokers
# apoplexy than in non-smokers.


# *** e) ####
#  
# Determine the absolute chances of cardiovascular disease in smokers and non-smokers.
#  


# In a smoker, the chance of apoplexy is about 52: 1,000. at 1,052 smokers
# about 52 occurrences of apoplexy can be expected.
# In non-smokers, the chance of developing apoplexy is about 27: 1,000. for 1,027 non-smokers
# approximately 27 occurrences of apoplexy can be expected.


# *** f) Determine the relative chances of cardiovascular disease in smokers. ####
#  


# Smokers have about 1.93(=0.0524/0.0271) times higher chance of apoplexy
# than for non-smokers. 95% interval estimate of this ratio
# the odds are 1.52 to 2.46.
# According to the interval estimate of the odds ratio, it is clear that on the surface
# Significance 0.05 is for smokers
# statistically significantly higher chance of developing apoplexy than in non-smokers.


# *** g) ####
#  
# Decide at a significance level of 0.05 on the dependence of the incidence of
# cardiovascular disease on smoking.
#  


# Attention! The epi.2by2 command does not output the expected frequency for
# Chi-square test of independence.
# It is not possible to verify the test assumptions!

# H0: There is no link between smoking and the occurrence of apoplexy.
# Ha: There is a link between smoking and the occurrence of apoplexy.

pom = chisq.test(kont.tab)
pom$expected
# All expected frequencies are greater than 5.


pom

lsr::cramersV(kont.tab)

# At the significance level of 0.05, HO can be rejected(p-value<<0.001,
# Chi-square goodness-of-fit test,
# df=1). The observed dependence can be assessed as very weak
# Cramer's V=0.061).




