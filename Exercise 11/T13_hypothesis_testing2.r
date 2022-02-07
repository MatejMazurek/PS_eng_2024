# ......................................................................................
# ...................Exercise 11. Two-sample tests/Interval estimates ..................
# ..........................Michal Béreš, Martina Litschmannová.........................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  Overview of IO tests/constructions ####
#  
# two-sample data
#  
# - Paired data indicates data that are taken to two measurements of the same entities
# ->data columns are dependent.
#  
# - If they are independent, this is a two-sample test.
#  
# - For paired data, we calculate the difference between the columns(or another function
# according to the input) and use one-sample tests for this difference.
#  


# ** Examples of paired data: ####
#  
# - measuring bulbs at two different temperatures(if each piece is measured twice - at
# temperature 1 and temperature 2)
#  
# - be careful here, it can happen that the tests are eg destructive and it is not
# possible to measure twice the same entity(product). Then we would consider two
# independent selections, each for one type of measurement ->independent data columns
# ->two-sample tests
#  
# - measurement of the patient's blood values before and after drug administration
#  
# - again pay attention to, for example, drug testing in two groups(placebo/real drug)
# ->two independent groups ->two-sample tests
#  


# * In general for two-sample tests/IO ####
#  
# - the test is always tied to the appropriate IO ->same conditions of use
#  
# - if the test has conditions of use(eg: normality of data, symmetry of data) then this
# condition must be met **both files**, if at least one does not meet, we consider the
# assumption to be broken
#  
# - one of the very important assumptions is data independence
#  
# - eg: measurement of products of manufacturer A and products of manufacturer B - here
# it is reasonable to assume that the products of manufacturer A are separate entities
# from the products of manufacturer B
#  


# * Two-sample tests/IO - difference of position measures ####
#  


# we make test data - so it can be used everywhere
data1 = rnorm(n = 30, mean = 105, sd = 10)
data2 = rnorm(n = 30, mean = 100, sd = 10)
boxplot(data1,data2)

# ** Two-sample Student's t-test ####
#  
# - Tests/estimates mean difference: $H_0: \mu_{1} - \mu_{2} = a$
#  
# - requirements:
#  
# - Data normality
#  
# - Homoskedasticity(scatter matching)
#  
# - independence of selections
#  
# - the function must have the parameter var.equal=TRUE
#  


# H0: mu1 - mu2=2
# HA: mu1 - mu2!=2

t.test(x = data1, y = data2, mu = 2, alternative = "two.sided",
       var.equal = TRUE, conf.level = 0.95)

# H0: mu1 - mu2=2
# HA: mu1 - mu2>2

t.test(x = data1, y = data2, mu = 2, alternative = "greater",
       var.equal = TRUE, conf.level = 0.95)

# H0: mu1 - mu2=2
# HA: mu1 - mu2<2

t.test(x = data1, y = data2, mu = 2, alternative = "less",
       var.equal = TRUE, conf.level = 0.95)

# ** Aspin-Welsh test ####
#  
# - Tests/estimates the mean difference: $H_0: \mu_{1} - \mu_{2} = a$
#  
# - requirements:
#  
# - Data normality
#  
# - independence of selections
#  
# - the function must have the parameter var.equal=FALSE
#  


# H0: mu1 - mu2=2
# HA: mu1 - mu2!=2

t.test(x = data1, y = data2, mu = 2, alternative = "two.sided",
       var.equal = FALSE, conf.level = 0.95)

# H0: mu1 - mu2=2
# HA: mu1 - mu2>2

t.test(x = data1, y = data2, mu = 0, alternative = "greater",
       var.equal = FALSE, conf.level = 0.95)

# H0: mu1 - mu2=2
# HA: mu1 - mu2<2

t.test(x = data1, y = data2, mu = 0, alternative = "less",
       var.equal = FALSE, conf.level = 0.95)

# ** Mann-Whitney test ####
#  
# - Tests/estimates median difference: $H_0: X_{0.5,1} - X_{0.5,2} = a$
#  
# - requirements:
#  
# - independence of selections
#  
# -(same type of division)
#  
# - requires conf.int=TRUE, to calculate IO
#  


# H0: X0.5,1 - X0.5,2=2
# HA: X0.5,1 - X0.5,2!=2

wilcox.test(x = data1, y = data2, mu = 2, alternative = "two.sided",
            conf.level=0.95, conf.int = TRUE)

# H0: X0.5,1 - X0.5,2=2
# HA: X0.5,1 - X0.5,2>2

wilcox.test(x = data1, y = data2, mu = 2, alternative = "greater",
            conf.level=0.95, conf.int = TRUE)

# H0: X0.5,1 - X0.5,2=2
# HA: X0.5,1 - X0.5,2<2

wilcox.test(x = data1, y = data2, mu = 2, alternative = "less",
            conf.level=0.95, conf.int = TRUE)

# * Two-sample tests/IO - proportion of variances ####
#  
# ** F-test ####
#  
# - Tests/estimates the variance ratio: $H_0: \sigma^2_{1} / \sigma^2_{2} = a$
#  
# - requirements:
#  
# - normality dat
#  
# - independence of selections
#  


# H0: sigma1 ^ 2/sigma2 ^ 2=1
# H0: sigma1 ^ 2/sigma2 ^ 2!=1

var.test(x = data1, y = data2, ratio = 1, alternative = "two.sided",
         conf.level = 0.95)

# H0: sigma1 ^ 2/sigma2 ^ 2=1
# H0: sigma1 ^ 2/sigma2 ^ 2>1

var.test(x = data1, y = data2, ratio = 1, alternative = "greater",
         conf.level = 0.95)

# H0: sigma1 ^ 2/sigma2 ^ 2=1
# H0: sigma1 ^ 2/sigma2 ^ 2<1

var.test(x = data1, y = data2, ratio = 1, alternative = "less",
         conf.level = 0.95)

# ** Levene's test ####
#  
# - Tests equality of variances: $H_0: \sigma^2_{1} = \sigma^2_{2}$!
#  
# - requirements:
#  
# - independence of selections
#  
# - requires data in standard data format
#  
# - leveneTest function in the car package
#  


# we produce data in a standard data format

data1.df = as.data.frame(data1)
data1.df$typ = "d1"
colnames(data1.df) = c("data", "typ")

data2.df = as.data.frame(data2)
data2.df$typ = "d2"
colnames(data2.df) = c("data", "typ")

data = rbind(data1.df, data2.df)
data$typ = as.factor(data$typ)

head(data)

# install.packages("car")

# H0: sigma1 ^ 2=sigma2 ^ 2
# HA: sigma1 ^ 2!=Sigma2 ^ 2

car::leveneTest(data$data ~ data$typ)

# * Two-sample tests/IO - probability difference ####
#  
# ** Homogeneity test of two binomial distributions ####
#  
# - Tests the match/estimates the probability difference: $H_0: \pi_{1} - \pi_{2} = 0$
#  
# - requirements:
#  
# - sufficient selection size: $n_i>\frac{9}{p_i(1-p_i)}$
#  
# - independence of selections
#  


# we will produce suitable data
pi1 = 0.4
pi2 = 0.3

dp1 = runif(n = 100, min = 0, max = 1) < pi1
dp2 = runif(n = 130, min = 0, max = 1) < pi2

x1 = sum(dp1)
n1 = length(dp1)

x2 = sum(dp2)
n2 = length(dp2)

x1
n1
x2
n2

# H0: pi1 - pi2=0
# HA: pi1 - pi2!=0

prop.test(x = c(x1, x2), n = c(n1, n2), alternative="two.sided",
          conf.level=0.95)

# H0: pi1 - pi2=0
# HA: pi1 - pi2>0

prop.test(x = c(x1, x2), n = c(n1, n2), alternative="greater",
          conf.level=0.95)

# H0: pi1 - pi2=0
# HA: pi1 - pi2<0

prop.test(x = c(x1, x2), n = c(n1, n2), alternative="less",
          conf.level=0.95)

#  Examples ####
#  
# * Příkald 1. ####
#  
# Data in the cholesterol2.xls file indicate the blood cholesterol level of men of two
# different age groups(20-30 years and 40-50 years). Verify at the significance level
# 0.05 hypothesis that the cholesterol level in the blood of older men does not differ
# from the cholesterol level in the blood of younger men.
#  


# Load data
chol = readxl::read_excel("data/testy_dvouvyberove.xlsx",
                  sheet = "cholesterol2",
                  skip = 1)   
colnames(chol)=c("mladsi","starsi")
head(chol)

# Convert to standard data format
chol.s = stack(chol)
chol.s = na.omit(chol.s)
colnames(chol.s) = c ("hodnoty","skupina")
head(chol.s)

# Exploratory analysis
boxplot(chol.s$hodnoty ~ chol.s$skupina)

# Elimination of remote observations:
chol.s$hodnoty.bez = chol.s$hodnoty

pom = boxplot(chol.s$hodnoty[chol.s$skupina == "mladsi"], plot = FALSE)
chol.s$hodnoty.bez[chol.s$skupina == "mladsi" & 
                   chol.s$hodnoty %in% pom$out] = NA

pom = boxplot(chol.s$hodnoty[chol.s$skupina == "starsi"], plot = FALSE)
chol.s$hodnoty.bez[chol.s$skupina == "starsi" & 
                   chol.s$hodnoty %in% pom$out] = NA

boxplot(chol.s$hodnoty.bez~chol.s$skupina)

# be careful in the data we have NA and we have to reckon with it !!!
# eg for length determination)


library(dplyr)

chol.s %>%  group_by(skupina) %>% 
            summarise(count = sum(!is.na(hodnoty.bez)),
                      prumer = mean(hodnoty.bez, na.rm = TRUE), 
                      smer.odch = sd(hodnoty.bez, na.rm = TRUE))

# rounding ->3 valid digits ->according to sd to thousands


# **Mean/median compliance test**
#  


# Verification of normality
chol.s %>%  group_by(skupina) %>% 
            summarise(norm.pval = shapiro.test(hodnoty.bez)$p.value)

# normality in hl. significance 0.05 OK


# Verification of variance agreement

# Exploratory
rozptyly = chol.s %>%  group_by(skupina) %>% 
            summarise(rozptyl = sd(hodnoty.bez, na.rm = TRUE)^2)
rozptyly
max(rozptyly$rozptyl)/min(rozptyly$rozptyl)

# exploratory assessment: the ratio of the largest to the smallest is>than 2
# ->I do not assume variance agreement


# Exactly by F-test

# H0: sigma.starsi=sigma.mladsi
# Ha: sigma.starsi<>sigma.mladsi

# I select the required data
starsi.bez = chol.s$hodnoty.bez[chol.s$skupina == "starsi"]
mladsi.bez = chol.s$hodnoty.bez[chol.s$skupina == "mladsi"]

var.test(x = starsi.bez, y = mladsi.bez, ratio = 1, conf.level=0.95)

# In hl. significance 0.05 we reject the assumption of agreement of variances
# The observed discrepancy between the variances can be at the significance level of 0.05
# Mark as statistically significant.


# Verification of agreement of mean values(Aspin-Welch test)

# H0: mu.starsi - mu.mladsi=0
# Ha: mu.starsi - mu.mladsi!=0

t.test(x = starsi.bez, y = mladsi.bez, mu = 0, 
       alternative = "two.sided", var.equal=FALSE, conf.level=0.95)

# in hl. significance 0.05 we reject H0->there is a stat. significant difference.


# H0: mu.starsi=mu.mladsi(mu.starsi - mu.mladsi=0)
# Ha: mu.starsi>mu.mladsi(mu.starsi - mu.mladsi>0)

t.test(x = starsi.bez, y = mladsi.bez, mu = 0, alternative = "greater",
       var.equal = FALSE, conf.level = 0.95)

# At the significance level of 0.05, we reject the assumption of medium agreement
# Cholesterol levels in groups of younger and older men in favor
# Alternatives that older men have higher mean cholesterol levels
# than men younger
# According to the results of the sample survey, we expect a medium content
# cholesterol in the blood of scared men will be about 0.524 mmol/l higher than
# medium chol. in younger men. According to 95% left intervention.
# to estimate the difference we expect the mean cholesterol content at
# older men at least 0.457 mmol/l higher than the mean value
# Cholesterol in younger men.


# * Example 2. ####
#  
# The data in the depression.xls file represent the length of remission in days from a
# simple random selection of two different groups of patients(patients with endogenous
# depression and patients with neurotic depression). Verify that the observed difference
# in mean remission length in these two groups of patients is statistically significant.
#  


# Read data from xlsx file(using readxl package)
deprese = readxl::read_excel("data/testy_dvouvyberove.xlsx",
                     sheet = "deprese")   
colnames(deprese)=c("endo","neuro")

head(deprese)

# Conversion to standard data format
deprese.s = stack(deprese)
deprese.s = na.omit(deprese.s)
colnames(deprese.s) = c ("hodnoty","skupina")

head(deprese.s)

# Exploratory analysis
boxplot(deprese.s$hodnoty~deprese.s$skupina)

# Data does not contain remote observations.


library(dplyr)

deprese.s %>%  group_by(skupina) %>% 
                summarise(count = length(hodnoty),
                          prumer = mean(hodnoty), 
                          smer.odch = sd(hodnoty))

# rounding ->3 valid digits ->according to sd per unit


# **Mean/median compliance test**
#  


# Normality verification
# We assume the assumption of normality by the Shapir - Wilkov test.
deprese.s %>%  group_by(skupina) %>% 
                summarise(norm.pval = shapiro.test(hodnoty)$p.value)


# In hl. significance of 0.05, we reject the assumption of normality


# at least as a guide, we check the similarity of the distribution

# we choose data for easier processing

neuro = deprese.s$hodnoty[deprese.s$skupina == "neuro"]
endo = deprese.s$hodnoty[deprese.s$skupina == "endo"]


par(mfrow = c(1,2))
hist(neuro)
hist(endo)

# Verification of median compliance(Mann-Whitney test)

# According to the histograms, we assume that the data have the same type of distribution.

# H0: med.neuro=med.endo(med.neuro - med.endo=0)
# Ha: med.neuro!=Med.endo(med.neuro - med.endo!=0)

wilcox.test(x = neuro, y = endo, mu = 0, alternative = "two.sided",
            conf.level=0.95, conf.int = TRUE)

# in hl. significance 0.05 we reject H0->there is a stat. significant difference


# H0: med.neuro=med.endo(med.neuro - med.endo=0)
# Ha: med.neuro>med.endo(med.neuro - med.endo>0)

wilcox.test(x = neuro, y = endo, mu = 0, alternative = "greater",
            conf.level=0.95, conf.int = TRUE)

# At the significance level of 0.05 we reject hyp. on the agreement of median times to
# disease remission for both groups of patients in favor of the alternative

# Median remission time is statistically significant in patients with neurotic depression
# significantly greater than in patients with endogenous depression.

# The remission time of patients with neurotic depression is about 191 days longer
# than in patients with endogenous depression. According to 95% left
# interval estimation is expected for patients with neuro. depression have
# at least 168 days longer remission than endo patients. depression.


# * Example 3. ####
#  
# We monitor urine osmolality at the inpatient station at 08:00 and 11:00 at 16 men.
# Based on the results in the osmolality.xls file, verify that the osmolality has
# increased statistically significantly.
#  


# Load data
osmolalita = readxl::read_excel("data/testy_dvouvyberove.xlsx",
                        sheet = "osmolalita", skip = 1)  
osmolalita = osmolalita[,c(2,3)]
colnames(osmolalita)=c("o8","o11")
head(osmolalita)

# Calculation of osmolality increase
osmolalita$narust = osmolalita$o11 -  osmolalita$o8

# Exploratory analysis
par(mfrow = c(1,1))
boxplot(osmolalita$narust)

# Data contains remote observations.


# Remove outliers
pom = boxplot(osmolalita$narust,plot = F)

osmolalita$narust.bez = osmolalita$narust
osmolalita$narust.bez[osmolalita$narust %in% pom$out] = NA

boxplot(osmolalita$narust.bez)

# Exploratory analysis for data without remote observations
library(dplyr)

osmolalita %>% summarise(count = sum(!is.na(narust.bez)),
                         prumer = mean(narust.bez, na.rm = TRUE), 
                         smer.odch = sd(narust.bez, na.rm = TRUE))

# rounding ->2 valid digits ->according to sd per unit


# Verification of normality
# The presumption of normality is verified by the Shapir - Wilk test.
shapiro.test(osmolalita$narust.bez)
# In hl. significance of 0.05, the assumption of normality cannot be rejected
# Shapir-Wilk test, W=0.949, p-value=0.545).


# Paired t-test
# H0: mu.narust=0 mm
# Ha: mu.narust>0 mm

t.test(osmolalita$narust.bez, mu = 0, alternative = "greater")

# According to a sample survey, urine osmolality can be expected to increase
# between 8 and 11 o'clock increases by about 24 mmol/kg. According to the 95% interval estimate
# osmolality can be expected to increase by at least 10 mmol/kg).
# At a significance level of 0.05, this increase can be described as statistically
# significant(paired t-test, t=3.1, df=13, p-value=0.005).


# * Example 4. ####
#  
# Semiconductor components of two manufacturers - MM and PP - were tested. MM claims
# that its products have a lower percentage of defective pieces. To verify this claim,
# 200 components were randomly selected from MM production, of which 14 were defective.
# A similar experiment was performed at PP with the result of 10 defective out of 100
# randomly selected components.
#  
# ** a) ####
#  
# Test MM's claim with a clean significance test.
#  


x.MM = 14
n.MM = 200
p.MM = x.MM/n.MM
p.MM

x.PP = 10
n.PP = 100
p.PP = x.PP/n.PP
p.PP

# Verification of assumptions
9/(p.MM*(1-p.MM))
9/(p.PP*(1-p.PP))

# Furthermore, for both companies we assume that n/N<0.05, ie that the given
# population(components) has a range of at least 20 * n, ie 20 * 200(4,000), resp. 20 *
# 150(3,000) components, which is probably a fairly realistic assumption.
#  


# Pearson's X2 test
# H0: pi.PP=pi.MM
# Ha: pi.PP>pi.MM

prop.test(x = c(x.PP,x.MM),n = c(n.PP,n.MM), alternative = "greater",
          conf.level = 0.95)

# Due to the p-value>hl. significance 0.05 we do not reject H0 - ie assumption.
# identical error rates. Therefore, it cannot be said that MM has better production.


# Pearson's X2 test
# H0: pi.PP=pi.MM
# Ha: pi.PP!=Pi.MM

prop.test(x = c(x.PP,x.MM),n = c(n.PP,n.MM), alternative = "two.sided",
          conf.level = 0.95)

# ** b) ####
#  
# Test MM's statement using an interval estimate of a significance level of 0.05.
#  


# Based on 95% Clopper - Pearson right - hand interval estimation
# -0.036; 1,000) the observed difference in production quality can be described as
# not statistically significant. We can reach the same conclusions on the basis of
# Pearson's right-hand test


