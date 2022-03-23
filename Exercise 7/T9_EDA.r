# ......................................................................................
# ...............Exercise 7. Data preprocessing and exploratory analysis ...............
# ..................Adéla Vrtková, Martina Litschmannová, Michal Béreš..................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

#  1. Packages - installation and loading ####
#  


# You only need to install packages once(if you don't already have them)
# install.packages("readxl")
# install.packages("dplyr")
# install.packages("openxlsx")
# install.packages("rstatix")

# Loading the package(must be repeated every time you restart R, it is advisable to have it 
# at the beginning of the script)
library(readxl)
library(dplyr)
library(openxlsx)
library(rstatix)
# contains notifications of overwritten functions or older versions of the package

#  2. Working directory - where we load and where we store data ####
#  
# - **Attention, the current open folder in Rstudio, or the location of the Rskcript is
# not automatically a working directory**
#  


# Working directory listing
getwd()

# Working directory setting -> in quotation marks, full path(relative or absolute)
setwd("./data")

getwd() # Where are we now?


setwd("./..") # back again


getwd() # for control


#  3. Load data file ####
#  


# * From CSV file ####
#  
# Basic functions - read.table, read.csv, read.csv2,... It depends mainly on the file
# format(.txt,.csv), the so-called separator of individual values, decimal point/dot
#  


# Load and save a data file in csv2 format from the working directory
data = read.csv2(file="aku.csv")

head(data)

data = read.csv2(file="aku.csv", sep=";", quote="", skip=0, header=TRUE)
head(data)

# Load and save a csv2 data file from the local disk to the data frame
data = read.csv2(file="./data/aku.csv")

# Load and save a csv2 data file from the Internet to the data frame
data = read.csv2(file="http://am-nas.vsb.cz/lit40/DATA/aku.csv")

# * From Excel(xlsx file) ####
#  
# Loading and saving a data file in xlsx format from the local disk to the data frame We
# use the function from the readxl package, which we expanded in the introduction
#  


data = read_excel("./data/aku.xlsx", 
                  sheet = "Data",           # worksheet specification in xlsx file
                  skip = 3)                 # lines to be skipped

head(data)

# * Remove unnecessary rows/columns and name rows/columns for easier data addressing ####
#  


# Indexing with negative indexes returns everything except the index value
# do not mix negative and positive indices!
data = data[,-1] # delete the first column with indexes
head(data)

# Rename columns - if necessary
colnames(data)=c("A5","B5","C5","D5","A100","B100","C100","D100") 
head(data)

# *** Note(which is good to read until the end....) ####
#  
# in Rstudio) it is possible to import using "Import Dataset" from the Environment
# window without having to write the code. In this case, however, there must be no
# special characters(hooks, commas) in the "path" to the file. Otherwise, an error will
# appear. The object imported this way will be in the new RSstudio as type "tibble".
# This is a more modern "data.frame" and in some features it can cause problems and
# throw errors! You can easily convert this object to type data.frame using
# **as.data.frame()**


#  4. Pre-processing data + Dplyr library ####
#  
# ** Overview of Dplyr library functions ####
#  
# - **%>%** is a so-called pipe operator, typical usage is "res=data %>% operation",
# where the result is a operation calibrated to data
#  
# - **select(...)** is one of the operations that we can insert into the "pipe" operator
# - it is used to select data
#     - select(1) - selects the first column
#     - select(A5) - selects the column named A5
#     - select(1,3,5) - selects columns 1,3,5
#  
# - **mutate(new_column=...)** is an operation that produces a new data column in the
# data frame using the specified calculation over the current columns
#     - data %>% mutate(C=A-B) produces a new column named "C" in the "data" data frame
# as the difference of the values in the existing columns "A" and "B"
#  
# - **filter(...)** filters values from the data that meet the specified requirements
#     - data %>% filter(manufacturer=="A"|manufacturer=="B") returns a data file that
# has only "A" or "B" values in the "manufacturer" column
#     - data %>% filter(manufacturer=="A", values>1000) if we write the requirements one
# after the other(separated by a comma) we understand it as logical **and**
#  
# - **summarize(...)** calculate the prescribed numerical characteristics within the
# specified columns(suitable for combination with group.by)
#     - data %>% summarize(prum=mean(kap5), median=median(kap5))
#  
# - **arrange(...)** ascending or descending row order
#     - data %>% arrange ascending
#     - data %>% arrange(desc) descending
#  
# - **group_by(...)** grouping of data according to unique values in the specified
# column
#     - data %>% group_by(manufacturer)
#  
# Very useful "cheat sheet" can be found here:
# https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf


# ** Column/row selections ####
#  


# Display of the first six lines
head(data)

# Display of the last six lines
tail(data)

# Display of line 10
data[10,]

# Display of the 3rd column - several ways
tmp = data[,3]
head(tmp)

# or(if we know the name of the variable written in the 3rd column)
data$C5

# or using the dplyr package select function, which selects the selected columns
tmp = data %>% select(C5)
head(tmp)

# Save the first and fifth columns of data frame
data_1_5 = data[,c(1,5)]
head(data_1_5)

# or using the dplyr function
data_1_5 = data %>% select(1,5)
head(data_1_5)

# or by name
data_1_5 = data %>% select(A5, "A100")
head(data_1_5)

# **Exclude data from the file.**


# Exclude the first and fifth columns from the data. data frames and data storage. framework attempt
temp_data = data[,-c(1,5)]
head(temp_data)

# or using dplyr
temp_data = data %>% select(-1, -5)
head(temp_data)

# or by name
temp_data = data %>% select(-A5,-A100)
head(temp_data)

# ** Basic conversion of a simple data matrix into a standard data format - stack(...) ####


data5 = data[,1:4] # from the data we select those columns that correspond to measurements after 5 cycles
colnames(data5) = c("A","B","C","D") # Rename columns
head(data5)

data5S = stack(data5)         # and transfer to st. data format
colnames(data5S) = c("kap5","manufacturer") # and edit the column names once more
head(data5S)

# We do the same for measurements performed after 100 cycles
data100 = data[,5:8] # we select from the data those columns that correspond to measurements after 100 cycles
colnames(data100) = c("A","B","C","D") # Rename columns
data100S = stack(data100)         # and transfer to st. data format
colnames(data100S) = c("kap100","manufacturer") # and edit the column names once more

# **If we want standard data type with both measurements, we should use reshape
# function:**


dataS=reshape(data=as.data.frame(data),
                  direction="long", # means we are going from data matrix (wide format)
                                    # into standard data format - long formant
                  varying=list(c("A5","B5","C5","D5"), # list of vectors with values for each
                               c("A100","B100","C100","D100")), # resulting column
                  v.names=c("cycles5","cycles100"), # name of columns in the result
                  times=c("A","B","C","D"),  # values of sorting variable
                  timevar="manufacturer")
head(dataS)
# you can use na.omit(dataS) to remove NaN values from data frame

# **!!! Handle the na.omit function extremely carefully so that you do not inadvertently
# lose data !!!**
#  


# ** Defining new columns in a data frame ####


# Defining a new variable of the drop in the capacity
dataS$drop = dataS$cycles5 - dataS$cycles100

head(dataS)

# or using a function from the dplyr package
dataS = dataS %>% mutate(drop=cycles5-cycles100)

# ** Select data from standard data format ####
#  


dataS$cycles5

# May be useful - create separate variables
a5 = dataS$cycles5[dataS$manufacturer=="A"] # Class(type) numeric
a5

# using dplyr with a data frame result
a5.df = dataS %>%
  filter(manufacturer=="A") %>%  # filters rows corresponding to manufacturer A
  select(cycles5)   # Selects only the values in column kap5,
head(a5.df)

# ** More detailed window for Dplyr library functions - work on data in standard data ####
# format
#  


# It is necessary to apply to data in st. data format !!! Pipe operator %>% - helps with
# chaining functions - in the new RSstudio shortcut key Ctrl + Shift + M
#  
# *** filter - applies a filter to the given column ####
#  


# filter - selects/filters rows based on given conditions
# Selection of products from the manufacturer
tmp = dataS %>% filter(manufacturer=="A")
head(tmp)

# Selection of products from manufacturer A or B
# separating conditions correspond to the logical "or"
tmp = dataS %>% filter(manufacturer=="A" | manufacturer=="B")  
head(tmp)

# Selection of all products with a decrease of 200 mAh and more from the manufacturer C
# comma separating conditions corresponds to logical "and at the same time"
tmp = dataS %>% filter(drop>=200, manufacturer=="C") 
head(tmp)

# *** mutate - produce a new column ####
#  


# mutate - adds a new variable or transforms an existing one
# Creating a new column drop_Ah, which indicates the capacity drop in Ah(original data in mAh, 1 Ah=1000 mAh)
tmp = dataS %>% mutate(drop_Ah=drop/1000)
head(tmp)
# Attention! if we do not save the result with the new column, it will only be printed and disappear

# *** summarize - generates summary characteristics of various variables ####
#  


# Calculation of the mean and median of all values of the variable cycles5
dataS %>% summarise(average=mean(cycles5),median=median(cycles5))

# **If the results contain NaNs, it means that the original data contained NaNs. There
# are two options, either drop NaNs from data, or set the function to ignore them.**
# **Be carefull with droping NaN values, you can loose data you want to keep. E.g. of
# you have data for capacity for 5 cycles, but not for 100, na.omit(...) will drop the
# whole line.**


tmp = na.omit(dataS)
tmp %>% summarise(average=mean(cycles5),median=median(cycles5))

dataS %>% summarise(average=mean(cycles5,na.rm = TRUE),median=median(cycles5,na.rm = TRUE))

# *** arrange - sorts rows according to the selected variable ####
#  


# Ascending and descending order of rows according to the decrease value
tmp = dataS %>% arrange(drop)
head(tmp)

tmp = dataS %>% arrange(desc(drop))
head(tmp)

# *** group_by - groups values into groups according to the selected variable ####
#  


# the table is "virtually" divided into groups for later processing, eg summarize
head(dataS %>% group_by(manufacturer))

# Ideal for calculating summary characteristics for each manufacturer separately, eg average
dataS %>%
  group_by(manufacturer) %>% 
  summarise(average=mean(cycles5,na.rm = TRUE), "st.dev."=sd(cycles5,na.rm = TRUE))

# **Final note on dplyr(which is good to finish until the end...)**
#  
# **Some operations may throw a "tibble" object. This is a more modern data.frame,
# however it can cause problems and cause error messages in some functions! You can
# easily convert this "tibble" object to data.frame using as.data.frame().**


#  5. Data conversion to the standard data format (for the two most common data ####
# formats)
#  
# * From data in Data Matrix format (already seen before) ####


data_DM = read_excel("./data/datova_matice.xlsx")
head(data_DM)

data_DM = data_DM[,-1]
colnames(data_DM) = c("A22", "A5", "B22", "B5", "C22", "C5", "D22", "D5")
head(data_DM)

# ** Reshape function ####
#  
# Its parameters:
# - **data** - data to be converted must be fe format data.frame(as.data.frame(data))
#  
# - **direction** - which direction we want to transform
#     - "long" - to standard format
#     - "wide" - back to the data matrix
#  
# - **varying** - column names that indicate the same data for different categories
#     - it is a sheet of vectors
#     - each sheet item is one measurement
#     - each vector is then a list of columns
#  
# - **v.names** - column names in st. give. format
#     - The number of names must match the number of vectors in varying
#  
# - **times** - names of individual categories
#     - ATTENTION !! must be in the same order as the varying variable
#  
# - **timevar** - column name with categories


data_DM_S=reshape(data=as.data.frame(data_DM),
                  direction="long",
                  varying=list(c("A5", "B5", "C5", "D5"),
                               c("A22","B22","C22","D22")),
                  v.names=c("5 C","22  C"),   
                  times=c("Amber","Bright","Clear","Dim"),  
                  timevar="vyrobce")
head(data_DM_S)

# and if we want, we can convert the data back
data_DM_2=reshape(data=data_DM_S,
                  direction="wide",
                  varying=list(c("A5", "B5", "C5", "D5"),
                               c("A22","B22","C22","D22")),
                  v.names=c("5 C","22  C"),   
                  times=c("Amber","Bright","Clear","Dim"),  
                  timevar="vyrobce")
head(data_DM_2)

# * From a data file where the categories are in individual Excel sheets ####
#  


data_A = read_excel("./data/po_listech.xlsx", sheet=1)
head(data_A)
data_B = read_excel("./data/po_listech.xlsx", sheet=2)
data_C = read_excel("./data/po_listech.xlsx", sheet=3)
data_D = read_excel("./data/po_listech.xlsx", sheet=4)

data_A$vyrobce = "Amber"
data_B$vyrobce = "Bright"
data_C$vyrobce = "Clear"
data_D$vyrobce = "Dim"
head(data_A)

data_PL_S = rbind(data_A, data_B, data_C, data_D)
head(data_PL_S)

#  6. Exploratory analysis and visualization of a categorical variable ####
#  
# ** Notes on graphics in R ####
#  
# the basis are the so-called high-level functions, which create a graph(ie open the
# graphics window and draw according to the specified parameters) followed by the
# so-called low-level functions, which add something to the active graphics window, do
# not open new low-level functions - eg abline, points, lines, legend, title, axis...
# which add a line, points, legend... ie. before using the "low-level" function it is
# necessary to call the "high-level" function(eg plot, boxplot, hist, barplot, pie,...)
# Further graphic parameters can be found in the help or eg here
# http://www.statmethods.net/advgraphs/parameters.html or here
# https://flowingdata.com/2015/03/17/r-cheat-sheet-for-graphical-parameters/or
# http://bcb.dfci.harvard.edu/~aedin/courses/BiocDec2011/2.Plotting.pdf Colors in R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf Saving
# graphs is possible using the function dev.print, jpeg, pdf and others. More easily in
# the Plots ->Export window
#  


# Table of absolute frequencies of the manufacturer's categorical variable...
freq = table(dataS$manufacturer)
freq # listing - object of type "table" - mostly more suitable, but more difficult conversion to type data.frame

# Looks weird, we should remember, that we had NaNs and we converted it from data
# matrix, thats why we got same numbers!


tmp = na.omit(dataS)
freq = table(tmp$manufacturer)
freq

# .. and using dplyr functions(more complex)
freq_df = tmp %>%  group_by(manufacturer) %>%
                  summarise(freq = n())  # number of products for each manufacturer
freq_df # listing - object type "tibble" - useful when we need to simply convert to type data.frame

# ** Relative frequency table ####
#  


# By direct calculation
rel.freq=100*freq/sum(freq)   
rel.freq

# or using the prop.table function
rel.freq=prop.table(freq)*100
rel.freq # statement

# or using the dplyr functions, where absolute frequencies will also be included
freq_all = tmp %>%  group_by(manufacturer) %>%
                    summarise(freq = n()) %>%   
                    mutate(rel_freq = 100*(freq/sum(freq)))
freq_all 
t(freq_all) # maybe more elegant in transpose form

# For relative frequencies tables, rounding must be included, 
# and summation to 1 (or 100 in case of %) kept
rel.freq=round(rel.freq,digits=1) # rounded to 1 decimal place
rel.freq[4]=100-sum(rel.freq[1:3]) # rounding error monitoring
rel.freq

# The procedure for table_abs_rel is different due to a different format(tibble)
freq_all[1:4,3] = round(freq_all[1:4,3],digits=1) # rounded to 1 decimal place
freq_all[4,3] = 100-sum((freq_all[1:3,3]))
freq_all

# ** Visualization using graphs ####


# Bar graph
# The basic R functionality (i.e. no package required) bar graph is based on the frequency table we have prepared
barplot(freq)

# Change colors, add name
barplot(freq,
        col=heat.colors(4), # alt. a vector of specific colors can be chosen, eg c("blue", "yellow," red "," green ")
        # or other scales(heat.colors, topo.colors, terrain.colors and many others)
        main="Selection size of different manufacturers",
        space=0.6) # The space parameter creates a space between columns


#  7. Exploratory analysis and visualization of a quantitative variable ####
#  


# Descriptive statistics
summary(dataS$cycles5)

# Beware of missing values
# Calculation of the average of one variable
mean(dataS$cycles5, na.rm = TRUE)

# Calculation of the median of one variable
quantile(dataS$cycles5, probs=0.5, na.rm = TRUE)

# Range determination
length(dataS$cycles5)

# beware NaNs
length(na.omit(dataS$cycles5))

# *** Other characteristics ->var(), sd(), min(), max(),... ####
#  
# Attention! The functions for calculating skewness and kurtosis are not part of the
# basic R, you will find them in the package moments. sharpness in the interval(1,5) To
# standardize the sharpness, it is necessary to subtract 3 from the calculated value. If
# you write the package name and "::" before the function name, you will ensure that the
# function from the given package will be used. packages have different functions under
# the same name
#  


# install.packages("moments")

library(moments)

skewness(a5,na.rm = TRUE)

kurtosis(a5,na.rm = TRUE)-3

# If we want to calculate the given characteristic for variable capacity after 5 cycles
# according to the manufacturers, we can use the tapply function
tapply(dataS$cycles5, dataS$manufacturer, mean, na.rm=TRUE)

# or using dplyr - here pay attention to automatic(not always correct rounding)
dataS %>% 
  group_by(manufacturer) %>% 
  summarise(mean(cycles5,na.rm=TRUE))

# To simplify the work, we can use the dplyr function and put all the characteristics in one table
dataS %>%    # without using group_by for the whole kap5 variable
  summarise(size=length(na.omit(cycles5)),
            min=min(cycles5,na.rm=TRUE),     # preventive na.rm=T
            Q1=quantile(cycles5,0.25,na.rm=TRUE),
            average=mean(cycles5,na.rm=TRUE),
            median=median(cycles5,na.rm=TRUE),
            Q3=quantile(cycles5,0.75,na.rm=TRUE),
            max=max(cycles5,na.rm=TRUE),
            variance=var(cycles5,na.rm=TRUE),
            st.dev.=sd(cycles5,na.rm=TRUE),
            variation_coeff=(100*(st.dev./average)),  # coefficient of variation in percent
            skewness=(moments::skewness(cycles5,na.rm=TRUE)),       # moments package precaution
            kurtosis=(moments::kurtosis(cycles5,na.rm=TRUE)-3)) 

# Don't forget to round correctly!
# We use group_by and get the characteristics for the capacity after 5 cycles according to the manufacturers
result = dataS %>%   
            group_by(manufacturer) %>%
            summarise(size=length(na.omit(cycles5)),
            min=min(cycles5,na.rm=TRUE),     # preventive na.rm=T
            Q1=quantile(cycles5,0.25,na.rm=TRUE),
            average=mean(cycles5,na.rm=TRUE),
            median=median(cycles5,na.rm=TRUE),
            Q3=quantile(cycles5,0.75,na.rm=TRUE),
            max=max(cycles5,na.rm=TRUE),
            variance=var(cycles5,na.rm=TRUE),
            st.dev.=sd(cycles5,na.rm=TRUE),
            variation_coeff=(100*(st.dev./average)),  # coefficient of variation in percent
            skewness=(moments::skewness(cycles5,na.rm=TRUE)),       # moments package precaution
            kurtosis=(moments::kurtosis(cycles5,na.rm=TRUE)-3)) 

t(result) # more favourable looks as transposed

# ** Box chart ####
#  
# **We always plot for the original data and observe the outliers.**


# Simple and fast rendering using the basic function only for manufacturer A and 5 cycles
boxplot(a5)

# And draw a multiple box graph
boxplot(dataS$cycles5~dataS$manufacturer) # graphic parameters can be set similarly to the previous ones

# Further modification of the graph, use of the points function to display the average
boxplot(dataS$cycles5~dataS$manufacturer,
        main="Capacity after 5 cycles (mAh)", 
        xlab="Manufacturer",
        ylab="Capacity (mAh)",
        col="grey")

# * Removing outliers ####


outliers_cycles5 = 
  dataS %>% 
  group_by(manufacturer) %>% 
  identify_outliers(cycles5)
outliers_cycles5

# **Important!! - we need a column with unique indentifier - if we dont have it we can
# add it. By default during e.g. reshape it is added.**


dataS$id2 = 1:length(dataS$manufacturer)
head(dataS)

# Now we use the id column for creating new data column free of outliers


dataS$cycles5_out = ifelse(dataS$id %in% outliers_cycles5$id,NA,dataS$cycles5) 

# compare
boxplot(dataS$cycles5~dataS$manufacturer)
boxplot(dataS$cycles5_out~dataS$manufacturer)

# ** Histogram ####
#  
# **We always plot for data without outliers !!**
#  


a5_no_outliers = dataS %>% filter(manufacturer=="A") %>% select(cycles5_out)
head(a5_no_outliers)

# hist does not like input as data frame, we can cheat it by selecting all of its values
hist(a5_no_outliers[,1], breaks=10)

# Labels, colors and other parameters can be set traditionally
hist(a5_no_outliers[,1], 
     main="Histogram of capacity of bateries of manufacturer A after 5 cycles", 
     xlab="capacity (mAh)",
     ylab="frequency",
     col="blue",       # fill color
     border="grey",    # column border color
     labels=TRUE)         # adds the absolute frequencies of the given categories in the form of labels

# ** QQ-graph ####
#  
# **We always plot for data without remote observations !!**
#  


# .. with adjustment of axis labels...
qqnorm(a5_no_outliers[,1], 
       xlab="Theoretical quantiles (normal dist.)",
       ylab="Sample quantiles",
       main="QQ-plot of capacity after 5 cycles of manufacturer A")
qqline(a5_no_outliers[,1])

#  8. rule 3 $\sigma$ and Chebyshev's inequality ####
#  
# * Empirical verification of normality ####
#  
# **Based on data after deleting outliers:**
#  


# we will use the data from the removal example op
a5_no_outliers_cleared = na.omit(a5_no_outliers)[,1]
a5_no_outliers_cleared

# We plot the QQ graph and calculate the skewness and sharpness:
#  


qqnorm(a5_no_outliers_cleared)
qqline(a5_no_outliers_cleared)

skewness(a5_no_outliers_cleared)
kurtosis(a5_no_outliers_cleared) - 3 # another definition shifted by 3

# - the dots in the QQ graph must lie approximately on the line - ie. the quantiles
# correspond approximately to the quantiles of the normal distribution
#  
# - skewness must lie in the interval <-2, 2>
#  
# - kurtosis must lie in the interval <-2.2>
#     - be careful we have to reduce the result of the R function by 3
#  
#  
# **If data normality is met -> rule 3σ**
#  
# σ: P(µ - σ<X<µ + σ)=0.68272σ: P(µ - 2σ<X<µ + 2σ)=0.95453σ: P(µ - 3σ<X<µ +
# 3σ)=0.9973
#  
# **If data normality is not met -> Chebyshev inequality**
#  
# σ: P(µ - σ<X<µ + σ)=02σ: P(µ - 2σ<X<µ + 2σ)=0.753σ: P(µ - 3σ<X<µ +
# 3σ)=0.8889
#  


mu = mean(a5_no_outliers_cleared)
sigma = sd(a5_no_outliers_cleared)
paste0("<", mu - sigma, ", ", mu + sigma, ">")
paste0("<", mu - 2*sigma, ", ", mu + 2*sigma, ">")
paste0("<", mu - 3*sigma, ", ", mu + 3*sigma, ">")

#  9. Rounding ####
#  
# Most important:
#  
# - the standard deviation is rounded up to the prescribed number of digits(ceiling)
# - data file size = <2,10> -> 1 valid digit
# - data file size = (10,30> -> 2 valid digits
# - data file size = (30,2000> -> 3 valid digits
# - position measures(averages, quantiles,...) are then rounded (classically) to the
# same valid digit as the standard deviation


length(a5_no_outliers_cleared)
stdev = sd(a5_no_outliers_cleared)
stdev

average = mean(a5_no_outliers_cleared)
average

max(a5_no_outliers_cleared)

