#######################################################################################
################ Exploratory data analysis ############################################
############### Adéla Vrtková, Martina Litschmannová ##################################
#######################################################################################

#   1. Run all packages which are necessary
#   2. Set working directory (if necessary)
#   3. Import data (by code or by "Import Dataset" button)
#   4. Check if they are in "standard data matrix" structure
#   5. Analysis of qualitative variables (if needed)
#   6. Analysis of quantitative variables !!!
#   7. Identification of outliers and decision about their possible removal (if present)

#######################################################################################
## 1. Install and run packages ##########################

install.packages("moments") 
library(moments)

#######################################################################################
## 2. Set working directory ###############

getwd()
setwd("C:/Users/MyFiles/R")


#######################################################################################
## 3. Import data ########################################################

# read.table, read.csv, read.csv2, ...

# load GOT_Data.RData

#######################################################################################
## 4. Checking the structure ##############################################################

data = battles

# First six rows
head(data)

# Last six rows
tail(data)

# 10th row
data[10,]

# 5th column 
data[,5]

# variable attacker_king
data$attacker_king 

# 1st and 5th column
data[,c(1,5)]

# Our dataset is in standard data matrix format - each row represents one battle
# removing NAs from data - now it is not recommended! 
# data=na.omit(data)

### Graphics with R ######################################
# high-level functions - create new graph
# low-level functions - add st. to existing graph - abline, points, lines, legend, title, axis ...
# before "low-level", "high-level" is necessary - plot, boxplot, hist, barplot, pie,...

# http://www.statmethods.net/advgraphs/parameters.html
# https://flowingdata.com/2015/03/17/r-cheat-sheet-for-graphical-parameters/
# http://bcb.dfci.harvard.edu/~aedin/courses/BiocDec2011/2.Plotting.pdf

## Colours in R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf


#######################################################################################
## 5. Qualitative variable ##########################

## frequencies (relative) ##########################################
freq=table(data$defender_king) 
freq

rel.freq=100*freq/sum(freq)  
rel.freq

## piechart ##########################################
pie(freq)

# colours and labels
pie(freq,
    col=c("red","green","yellow","blue","pink","grey","white"))
pie(freq,
    col=heat.colors(7))

pie(freq,
    col=heat.colors(7),
    main="Defender Kings")

## barplot #####################################################
barplot(freq)

# colours and labels
barplot(freq,
        col=heat.colors(7),
        main="Defender Kings",
        space=0.6)                          

# labels, legend
barplot(freq,
        col=heat.colors(7),
        horiz=TRUE,                            
        border=FALSE,				                   
        main="Defender Kings")

#######################################################################################
#####Pie charts are a very bad way of displaying information.##########################
##The eye is good at judging linear measures and bad at judging relative areas.########
##A bar chart or dot chart is a preferable way of displaying this type of data. #######
#######################################################################################

#######################################################################################
## 6. Quantitative variable !!!! #########################

summary(data$defender_size)

mean(data$defender_size)

# Careful with NAs
mean(data$defender_size,na.rm=TRUE)

# Median
quantile(data$defender_size,probs=0.5,na.rm=TRUE)

# more characteristics -> var(), sd(), min(), max()

## Skewness and kurtosis
moments::skewness(data$defender_size,na.rm=TRUE)
moments::kurtosis(data$defender_size,na.rm=TRUE)-3

## boxplot #####################################################
boxplot(data$defender_size)

boxplot(data$defender_size,
        main="Size of defender's army")

boxplot(data$defender_size,
        ylab="Size of defender's army",
        col="grey")

# Horizontal boxplot
boxplot(data$defender_size,
        main="Size of defender's army",
        horizontal=TRUE)

# Boxplot for identifying outliers
bp=boxplot(data$defender_size,plot=FALSE)
bp$out # no outliers (which we can see from the boxplot) - compare it with attacker_size

## multiple boxplot ################################
data2=data[which(data$defender_king=="Joffrey/Tommen Baratheon" | data$defender_king=="Robb Stark"),]
data2$defender_king=factor(data2$defender_king) # making a subset to demonstrate working with multiple boxplot

boxplot(data2$defender_size~data2$defender_king)

## Histogram ##########################################################################
hist(data$defender_size)
hist(data$defender_size,breaks=8)

hist(data$defender_size, 
     main="Histogram for the size of defender's army", 
     ylab="frequency",
     col="blue", 
     border="grey",
     labels=TRUE) 

## QQ-plot - graphical tool for checking the normality ############################
qqnorm(data$defender_size)
qqline(data$defender_size)

#######################################################################################
## 7. Identification of outliers + removal? #############
# Usually, outliers are present in data and you have to decide about their removal.
# In our lessons, we will work with data without outliers.
# However, keep in mind that something like outliers exist and you have to think about them.
#################################################################################################

#############################################################
## Conclusion for GoT - defender_size variable analysis
# boxplot doesn't seem symetrical - the length of the whiskers is different
# the same can be seen on histogram - data are not symetrical (doesn't correspond to the Bell curve)
# skewness and kurtosis are between numbers -2 and 2 but the graphical tools suggest that the normality will be violated
# finally, in the QQ-plot the points are not in one line, rather they are quite far from it
# the EDA suggests violation of normality
#################################################################

## TASK: Instal the package "robustbase", activate it and save the dataset called aircraft as mydata
install.packages("robustbase")
library(robustbase)
mydata=aircraft

# Look in the Help documentation for the description of the dataset
# Choose one of the variables and perform EDA (as in the defender_size GoT dataset)
# The main goal is to analyse the normality of the data
# Try to describe to data (see Conclusion for GoT for inspiration)


