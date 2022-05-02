# ......................................................................................
# ........................Exercise 12. Tests for multiple samples.......................
# ..................Michal Béreš, Martina Litschmannová, Adéla Vrtková..................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

# * Test data for a function call example ####
#  


# I will create some data from the normal distribution with same variances
a = as.data.frame(rnorm(n = 35, mean = 100, sd = 10))
b = as.data.frame(rnorm(n = 30, mean = 108, sd = 10))
c = as.data.frame(rnorm(n = 40, mean = 104, sd = 10))
d = as.data.frame(rnorm(n = 32, mean = 112, sd = 10))

# I will rename the column name
colnames(a) = c("value")
colnames(b) = c("value")
colnames(c) = c("value")
colnames(d) = c("value")

# I will add a type for all frame data
a$type = "group1"
b$type = "group2"
c$type = "group3"
d$type = "group4"

# I glue the lines together
data = rbind(a,b,c,d)

# Convert type to type factor (needed for some tests)
data$type = as.factor(data$type)

head(data)

boxplot(data$value ~ data$type)
# if there are any outliars, I will ignore them
# I know the data is from a normal distribution!
# I also know they have the same variance sd = 10

#  Overview of tests and their functions ####
#  
# * Comparing the measures of Variability (variances) ####


# ** Bartlett test ####
# - verifies the equality of variances 
# - $H_0: \sigma^2_1 = \sigma^2_2 = \sigma^2_3 = \ldots$
# - $H_A: \neg H_0$
# - the assumtion data normality(and of course independence and continuity)


bartlett.test(data$value ~ data$type)

# ** Levene's test ####
# - verifies the equality of variances 
# - $H_0: \sigma^2_1 = \sigma^2_2 = \sigma^2_3 = \ldots$
# - $H_A: \neg H_0$
# - only independence and continuity are required


car::leveneTest(data$value ~ data$type)

# ** Cochran's and Hartley's test ####
# - verifies the equality of variances 
# - require data normality and so-called balanced sorting
#     - balanced sorting means that we have approximately the same amount of data in
# each group
# - we will not use them


# * Comparing the measures of Position (means or medians)  ####
# 
# ** ANOVA(analysis of variance) ####
# - test the equality of mean values 
# - $H_0: \mu_1 = \mu_2 = \mu_3 = \ldots$
# - $H_A: \neg H_0$
# - prerequisites:
#     - normality dat
#     - homoskedasticity(identical variances)
#     - (and of course independence and continuity)
# - if we reject $H_0$ Post-Hoc analysis is required
#     - using TukeyHSD test
#     - we want the result in the form of letter scheme and effects


# ANOVA
# H0: mu1=mu2=mu3=mu4
# HA:~H0(H0 negation)

res = aov(data$value~ data$type)
summary(res)

# Post-Hoc analysis

TukeyHSD(res)

# effect computation
library(dplyr)

# overall average
mean_overall = mean(data$value)
mean_overall

# averages in groups
effects = data %>% group_by(type) %>% 
    summarize(mean_group = mean(value))

# effects
effects$effect = effects$mean_group - mean_overall

# list them sorted
effects %>% arrange(desc(effect))

# letter scheme, library rcompanion

# install.packages("rcompanion")
posthoc = TukeyHSD(res)

# how to get the matrix of values out of the result
matrix_posthoc = posthoc[[1]]
matrix_posthoc
# now we make a dataframe with columns of pairs and pvalues
posthoc_DF = data.frame(pairs = rownames(matrix_posthoc), 
                   pval = matrix_posthoc[,'p adj'])
posthoc_DF

rcompanion::cldList(pval ~ pairs, 
        data = posthoc_DF,
        threshold = 0.05)

# ** Kruskal - Wallis test ####
#  
# - verifiesthe equality of medians
# - $H_0: X_{0.5,1} = X_{0.5,2} = X_{0.5,3} = \ldots$
# - $H_A: \neg H_0$
# - prerequisites:
#     - data symmetry
#     - (and of course independence and continuity)
# - if we reject $H_0$ Post-Hoc analysis is required
#     - using the Dunn test
#         - method = "bonferroni" 
#     - we want the result in the form of letter scheme and effects


# KW test
# H0: X0.5,1=X0.5,2=X0.5,3=X0.5,4
# HA:~H0(H0 negation)

kruskal.test(data$value ~ data$type)

# Post-Hoc analysis

# install.packages("FSA")
FSA::dunnTest(data$value ~ data$type,   # FSA library
              method="bonferroni")

# effects

# overall median
median_overall = median(data$value)
median_overall

# medians in groups
effects = data %>% group_by(type) %>% 
    summarize(median_group = median(value))

# effects
effects$effect = effects$median_group - median_overall

# list them sorted
effects %>% arrange(desc(effect))

# letter scheme, library rcompanion

# install.packages("rcompanion")
posthoc = FSA::dunnTest(data$value ~ data$type,   # FSA library
              method="bonferroni")

# how to get the matrix of values out of the result
posthoc_DF = posthoc$res
posthoc_DF
# its in the data frame form already

rcompanion::cldList(P.adj ~ Comparison, 
        data = posthoc_DF,
        threshold = 0.05)

#  Examples ####


# * Example 1. ####
# 
# 122 patients who underwent heart surgery were randomly divided into three groups 
# - **Group 1:** Patients received 50% nitrous oxide and 50% oxygen mixed continuously
# for 24 hours.
# - **Group 2:** Patients received 50% nitric oxide and 50% oxygen only during surgery.
# - **Group 3:** Patients received no nitrous oxide but received 35-50% oxygen for 24
# hours.
# 
# The data in the sheet 1 of testy_vicevyberove.xlsx file correspond to the folic acid
# salt concentrations in the red blood cells in all three groups 24 hours after the
# surgery. Verify that the observed differences between the folic acid salt
# concentrations are statistically significant, i.e. that there is an effect of the
# composition of the mixture on the monitored parameter.


acid = readxl::read_excel("data/testy_vicevyberove.xlsx", sheet=1)
colnames(acid) = c("Group 1","Group 2","Group 3")   # rename columns
head(acid)

# conversion to standard data format
acid.s = stack(acid)
colnames(acid.s) = c("values","group")
acid.s = na.omit(acid.s)
head(acid.s)

boxplot(acid.s$values ~ acid.s$group, xlab = "Test groups", ylab = "Folic acid salt concentration")
# Data do not contain any outliars

# we test the normality using S-W. test

acid.s %>% group_by(group) %>% 
    summarise(pval_SW = shapiro.test(values)$p.value)

# Information needed to set rounding

acid.s %>% group_by(group) %>% 
    summarise(len = length(values), st.dev = sd(values))

# sd is rounded to 3 valid digits
# sd and position measures are rounded to tenths


# equality of variance
s2 = acid.s %>% group_by(group) %>% 
        summarise(var = sd(values)^2)
s2 # sampling variances

max(s2$var)/min(s2$var)
# According to the box chart and information on the ratio of the largest and smallest
# variances(<2) we do not assume that the variances differ statistically significantly

# The assumption of normality was not rejected -> Bartlett's test

bartlett.test(acid.s$values ~ acid.s$group)

# At the significance level of 0.05, there are no statistically significant differences in variances

# We want to compare the mean values of independent samples from normal distributions
# with same variances -> ANOVA
# The aov() command requires data in the standard data format

results = aov(acid.s$values ~ acid.s$group) 
summary(results)  

# At the significance level of 0.05, there are statistically significant differences in mean values

# post-hoc analysis
TukeyHSD(results)

# effect computation

# overall average
mean_overall = mean(acid.s$values)
mean_overall

# averages in groups
effects = acid.s %>% group_by(group) %>% 
    summarize(mean_group = mean(values))

# effects
effects$effect = effects$mean_group - mean_overall

# list them sorted
effects %>% arrange(desc(effect))

# letter scheme, library rcompanion

# make a dataframe with columns of pairs and pvalues
matrix_posthoc = TukeyHSD(results)[[1]]
posthoc_DF = data.frame(pairs = rownames(matrix_posthoc), 
                   pval = matrix_posthoc[,'p adj'])
# letter scheme
rcompanion::cldList(pval ~ pairs, 
        data = posthoc_DF,
        threshold = 0.05)

# * Example 2. ####
#  
# Three breeds of rabbits are bred on the farm. An experiment was performed on sheet 2
# of testy_vicevyberove.xlsx, the aim of which was to find out whether, even if we keep
# all the rabbits for the same time and under the same conditions (food, environment),
# there is a statistically significant difference between breeds in rabbit weights.
# Verify.


rabbits = readxl::read_excel("data/testy_vicevyberove.xlsx", sheet=2)
colnames(rabbits) = c("Vienna","Czech","Kalif")   # rename columns
head(rabbits)

# conversion to standard data format
rabbits.s = stack(rabbits)
colnames(rabbits.s) = c("value","group")
rabbits.s = na.omit(rabbits.s)
head(rabbits.s)

boxplot(rabbits.s$value ~ rabbits.s$group)
# data contains an outliar

# Eliminate outliar

rabbits.s$id = seq(1,length(rabbits.s$value))

outliars = rabbits.s %>% group_by(group) %>% rstatix::identify_outliers(value)
outliars

rabbits.s$value_cleared = ifelse(rabbits.s$id %in% outliars$id, NA, rabbits.s$value)

# Box chart
boxplot(rabbits.s$value_cleared ~ rabbits.s$group)

library(dplyr)

rabbits.s %>% group_by(group) %>% 
    summarise(norm.pval = shapiro.test(value_cleared)$p.value)

# At the significance level of 0.05, we do not reject the assumption of normality.

# Information needed for correct rounding
rabbits.s %>% group_by(group) %>%
    summarize(len = sum(!is.nan(value_cleared)), 
              sd = sd(value_cleared, na.rm = TRUE))

# sd is rounded to 2 valid digits
# sd and position measurements round to hundredths

# The assumption of normality was not rejected ->Bartlett's test
bartlett.test(rabbits.s$value_cleared ~ rabbits.s$group) 

# At the significance level of 0.05, the equality of variances cannot be rejected

# We want to compare the mean values of independent samples from normal
# distributions with the same variances -> ANOVA
# The aov() command requires data in the standard data format

results = aov(rabbits.s$value_cleared ~ rabbits.s$group)
summary(results)  

# At the significance level of 0.05, we reject the hypothesis of equality of the mean values

# post-hoc analysis
TukeyHSD(results)

# effect computation

# overall average
mean_overall = mean(rabbits.s$value_cleared, na.rm = TRUE)
mean_overall

# averages in groups
effects = rabbits.s %>% group_by(group) %>% 
    summarize(mean_group = mean(value_cleared, na.rm = TRUE))

# effects
effects$effect = effects$mean_group - mean_overall

# list them sorted
effects %>% arrange(desc(effect))

# * Example 3. ####
#  
# Four manufacturers A, B, C, D sent a total of 66 products to the competition for the
# best product quality. The jury compiled the ranking (only the position of the product
# in the list of 66 from best to worst), which is listed in the sheet 3 of the file
# testy_vicevyberove.xlsx. On the basis of the above data, assess whether the origin of
# the products affects its quality.


quality = readxl::read_excel("data/testy_vicevyberove.xlsx", sheet = 3)
colnames(quality) = c("ranking", "manufacturer")   # rename columns
head(quality)
# data is already in standard format

boxplot(quality$ranking ~ quality$manufacturer)

# the data are not independent by nature, also they are not continuous!
# outliars should not be present by the nature of the dataset
# the assumptions are corrupted for all tests -> our best bet is the most robust test in our arsenal
# we skip directly into KW test

# Symmetry verification

quality %>% group_by(manufacturer) %>% 
    summarize(skewness = moments::skewness(ranking))

# We want to compare the medians of "independent" samples -> Kruskal-Wallis test
kruskal.test(quality$ranking ~ quality$manufacturer)

# At the significance level of 0.05, there are no statistically significant differences in medians

# * Example 4. ####
#  
# The effect of three types of medicaments on blood clotting was studied (so called
# thrombin time). Data of 42 monitored persons are recorded in the sheet 4 of the file
# testy_vicevyberove.xlsx. Does the thrombin time depend on which preparation was used?


trombin.s = readxl::read_excel("data/testy_vicevyberove.xlsx", 
                               sheet=4, skip = 1)
colnames(trombin.s) = c("value","group")   # rename columns

head(trombin.s)
# data is already in standard format


# exploratory analysis
boxplot(trombin.s$value ~ trombin.s$group)
# no outliars

# verification of normality
library(dplyr)

trombin.s %>% group_by(group) %>%
    summarize(norm.pval = shapiro.test(value)$p.value)

# At the significance level of 0.05 we reject the assumption of normality(for A)

# we can at least test the equality of variances -> same variances
# means better KW test result in terms of type II error

# The assumption of normality was rejected -> Levene's test

car::leveneTest(trombin.s$value ~ trombin.s$group) 

# the assumption of homoskedasticity was rejected (at significance 0.05)

# Symmetry verification
trombin.s %>% group_by(group) %>% 
  summarize(skewness = moments::skewness(value))
# we do not reject the assumption of data symmetry

# We want to compare medians (data not from normal dist.)-> Kruskal - Wallis test

kruskal.test(trombin.s$value,trombin.s$group)

# At the significance level of 0.05, we found statistically significant differences in medians

FSA::dunnTest(trombin.s$value~trombin.s$group,method = "bonferroni")      

# effect counting
library(dplyr)

# overall average
median_overall = median(trombin.s$value)
median_overall

# averages in groups
effects = trombin.s %>% group_by(group) %>% 
    summarize(median_group = median(value))

# effects
effects$effect = effects$median_group - median_overall

# List sorted
effects %>% arrange(desc(effect))

# * Example 5.(multiple groups) ####
#  
# When Snow White got to the seven dwarves, she sensed an opportunity to make a lot of
# money. The Dwarves basically fell in love with the Snow White and immediately handed
# over all of their mined gold. However, even this is not enough for Snow White and she
# feels that she could benefit more from the dwarves. Therefore, she began to record how
# many kilograms of gold a day she received from each of the dwarves(snehurka.xlsx).
# Verify that the dwarves differ in the amount of gold mined.


gold = readxl::read_excel("data/snehurka.xlsx")
colnames(gold) = c("ammount","dwarf")
head(gold)
# data is in the standard data format

boxplot(gold$ammount ~ gold$dwarf)
# data does not outliars

# verification of normality
library(dplyr)

gold %>% group_by(dwarf) %>%
    summarize(p.val = shapiro.test(ammount)$p.value)

# The assumption of normality was not rejected -> Bartlett's test
bartlett.test(gold$ammount ~ gold$dwarf) 

# At the significance level of 0.05, there are no statistically significant differences in variances

# ANOVA
results = aov(gold$ammount ~ gold$dwarf) 
summary(results)  

# POST-HOC
res = TukeyHSD(results)[[1]]
res

# effects computation
library(dplyr)

# overall average
overall = mean(gold$ammount)
overall

# averages in groups
effects = gold %>% group_by(dwarf) %>% 
    summarize(mean_dwarf = mean(ammount))

# effects
effects$effect = effects$mean_dwarf - overall

# list sorted
effects %>% arrange(desc(effect))

# letter scheme, library rcompanion

# make a dataframe with columns of pairs and pvalues
matrix_posthoc = TukeyHSD(results)[[1]]
posthoc_DF = data.frame(pairs = rownames(matrix_posthoc), 
                   pval = matrix_posthoc[,'p adj'])
# letter scheme
rcompanion::cldList(pval ~ pairs, 
        data = posthoc_DF,
        threshold = 0.05)



