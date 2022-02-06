#######################################################################################
########################## Estimation theory  #########################################
############ Adéla Vrtková, Martina Litschmannová #####################################

################## Exercise 1. (Cholesterol) #########################################
#######################################################################################
# Import cholesterol.RData

# Our goal is to find the CI for the population mean
# Assumption is normality -> EDA!

summary(cholesterol$values)

boxplot(cholesterol$values)
hist(cholesterol$values)

qqnorm(cholesterol$values)
qqline(cholesterol$values)

library(moments)
moments::skewness(cholesterol$values)
moments::kurtosis(cholesterol$values)-3

### EDA - Conclusion
# Mean and Median are very similar. The boxplot and histogram - both suggest symmetrical data (which is good, for normality, we need symmetry!)
# No outliers in the boxplot.
# QQ-plot - points are close to the line.
# Skewness and kurtosis are around 0 (between -2 and 2)
# We assume that normality is not violated -> so we can find out the CI for the mean.

t.test(cholesterol$values, conf.level = 0.95)

# The 95% confidence interval for the mean is (177,3 ; 182.8). The sample mean is 180,0.

# If the normality is violated, we can't work with the mean. 
# So, if the data are quite symmetrical, we will work with the median.
# Let us demonstrate how to obtain CI for the median.

median(cholesterol$values) # sample median
wilcox.test(cholesterol$values, conf.level = 0.95, conf.int = T) # 95% CI

################## Exercise 2. (Cholesterol) #########################################
#######################################################################################

n = 200  		# sample size
x = 120 	# number of "successes"
p = x/n 	# relative frequency, sample proportion
p

# Assumptions - they have to be verified!
n > 9/(p*(1-p)) # logical value TRUE/FALSE or you can simply test the assumption with calculator

## Clopper - Pearson - confidence interval for proportion
binom.test(x,n,conf.level=0.95)

# or simply

binom.test(120,200,conf.level=0.95)

# There is approximately 60% of young men with high cholesterol level in the population.
# With 95% probability this proportion is between 53% and 67%.


################## Exercise 3. (Cookies) ############################################
#######################################################################################
# Import cookies.RData

# Our goal is to find the CI for the population std. deviation
# Assumption is normality -> EDA!

## Perform EDA...


## Write conclusion - can we continue in our analysis? Is normality violated?
#
#
#


## 95% confidence interval for std. deviation with varTest function (require EnvStats library)
install.packages("EnvStats") # install it to your computers

library(EnvStats)
varTest(cookies$values, conf.level=0.95)

# LCL and UCL are for VARIANCE -> need for sd=sqrt(var)
## E.g. simply using Ctrl+C, Ctrl+V
sqrt(30.00133) # LCL
sqrt(73.71502) # UCL

# OR more advanced
CI=varTest(cookie$values,conf.level=0.95)  # saving the object
CI$conf.int # taking only part with conf. interval
sqrt(CI$conf.int) # sqrt -> conf. interval for sd

## Revision - find the 95% confidence interval for the mean



################## Exercise 4. (Survival time) ############################################
#######################################################################################
# Import time.RData (26.11.2019 Data.zip was updated, download the updated file)

# Our goal is to find the CI for the population mean
# Assumption is normality -> EDA!

## Perform EDA


## Write conclusion
#
#
#
#

# The normality assumption is definitely violated!
# We can't compute the CI for the mean. The results could be misleading!
# However, the symmetry is still quite good (look at the skewness)
# So, in this case, we can compute the CI for the median ("plan B").
# The symmetry assumption means that you should avoid this procedure with highly skewed data (skewness > 2 or < -2).

median(time$values) # sample median
wilcox.test(time$values,conf.int=T) # 95% CI for median

# The population median of the survival time is approx. 5,1 years.
# With 95% certainity, the population median is between 4,3 and 5,9 years. 



