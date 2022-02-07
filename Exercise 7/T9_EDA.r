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


# Loading the package(must be repeated every time Rka is run, it is advisable to have it at the beginning of the script)
library(readxl)
library(dplyr)
library(openxlsx)
# contains notifications of overwritten functions or older versions of the package


#  2. Working directory - where we load and where we store data ####
#  
# - Attention, the current open folder in Rstudio, or the location of the Rskcript is
# not automatically a working directory
#  


# Working directory listing
getwd()

# Working directory settings ->in quotation marks, full path(relative or absolute)
setwd("./data")

getwd() # Where are we now?


setwd("./..") # back again


getwd() # control


#  3. Load data file ####
#  


# * From CSV file ####
#  
# Basic functions - read.table, read.csv, read.csv2,... It depends mainly on the file
# format(.txt,.csv), the so-called separator of individual values, decimal point/dot
#  


# Load and save a data file in csv2 format from the working directory
data = read.csv2(file="aku.csv")

data

data = read.csv2(file="aku.csv", sep=";", quote="", skip=0, header=TRUE)
data

help(read.csv2)

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
# **as.data.frame()** If you have a problem with a function that does not take a column
# from "tibble" as a non-numeric output, you can fix it with the command pull: data [,
# 1] replace pull(data, 1)
#  


#  4. Pre-processing data + Dplyr library ####
#  
# ** Overview of Dplyr library functions ####
#  
# - **>%** is a so-called pipe operator, typical usage is "res=data>%operation", where
# the result is a operation calibrated to data
#  
# - **select(...)** is one of the operations that we can insert into the "pipe" operator
# - it is used to select data
#  
# - select(1) - selects the first column
#  
# - select(A5) - selects the column named A5
#  
# - select(1,3,5) - selects columns 1,3,5
#  
# - **mutate(new_column=...)** is an operation that produces a new data column in the
# data frame using the specified calculation over the current columns
#  
# - data%>% mutate(C=AB) produces a new column named "C" in the "data" data frame as the
# difference of the values in the existing columns "A" and "B"
#  
# - **filter(...)** filters values from the data that meet the specified requirements
#  
# - data%>% filter(manufacturer=="A"|manufacturer=="B") returns a data file that has
# only "A" or "B" values in the "manufacturer" column
#  
# - data%>% filter(manufacturer=="A", values>1000) if we write the requirements one
# after the other(separated by a comma) we understand it as and at the same time
#  
# - **summarize(...)** calculate the prescribed numerical characteristics within the
# specified columns(suitable for combination with group.by)
#  
# - data%>% summarize(prum=mean(kap5), median=median(kap5))
#  
# - **arrange(...)** ascending or descending row order
#  
# - data%>% arrange ascending
#  
# - data%>% arrange(desc) descending
#  
# - **group_by(...)** grouping of data according to unique values in the specified
# column
#  
# - data%>% group_by(manufacturer)
#  
# Very useful Dgasr "cheat sheet" can be found here:
# https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf
#  


# ** Column/row selections ####
#  


# Data file listing
data

# Display of the first six lines
head(data)

# Display of the last six lines
tail(data)

# Display of line 10
data[10,]

# Display of the 3rd column - several ways
data[,3]

# or(if we know the name of the variable written in the 3rd column)
data$C5

# or using the dplyr package select function, which selects the selected columns
data %>% select(C5)

# hr>
#  


# Save the first and fifth columns of data. frames data to data. framework attempt
pokus = data[,c(1,5)]
head(pokus)

# or using the dplyr function
pokus = data %>% select(1,5)
head(pokus)

# or by name
naz_sl = "A100"
pokus = data %>% select(A5, naz_sl)
head(pokus)

# hr>Exclude data from the file.
#  


# Exclude the first and fifth columns from the data. data frames and data storage. framework attempt
pokus = data[,-c(1,5)]
head(pokus)

# or using dplyr
pokus = data %>% select(-1, -5)
head(pokus)

# or by name
pokus = data %>% select(-A5,-A100)
head(pokus)

# hr>Modification of data into several smaller logical units with different structure
# Note. when storing data, we think of clarity in names
#  
# ** Basic conversion of a simple data matrix into a standard data format - stack(...) ####
#  


data5 = data[,1:4] # from the data we select those columns that correspond to measurements after 5 cycles
colnames(data5) = c("A","B","C","D") # Rename columns
head(data5)

data5S = stack(data5)         # and transfer to st. data format
colnames(data5S) = c("kap5","vyrobce") # and edit the column names once more
head(data5S)

# We do the same for measurements performed after 100 cycles
data100 = data[,5:8] # we select from the data those columns that correspond to measurements after 100 cycles
colnames(data100) = c("A","B","C","D") # Rename columns
data100S = stack(data100)         # and transfer to st. data format
colnames(data100S) = c("kap100","vyrobce") # and edit the column names once more


# Finally we will create a data file in st. data format with all data
dataS = cbind(data5S,data100S) # merge "by columns"
head(dataS)

dataS = dataS[,-2] # omit the extra second column
dataS = na.omit(dataS) # omit rows with NA values
head(dataS)

# **!!! Handle the na.omit function extremely carefully so that you do not inadvertently
# lose data !!!**
#  


# hr>
#  
# ** Defining new columns in a data frame ####
#  


# Defining a new drop variable
dataS$pokles = dataS$kap5 - dataS$kap100

head(dataS)

# or using a function from the dplyr package
dataS = dataS %>% mutate(pokles=kap5-kap100)

# ** Select data from standard data format ####
#  


dataS$kap5

# May be useful - create separate variables
a5 = dataS$kap5[dataS$vyrobce=="A"] # Class(type) numeric
a5

# as follows with a data frame result
a5.df = dataS %>%
  filter(vyrobce=="A") %>%  # filters rows corresponding to manufacturer A
  select(kap5)   # Selects only the values in column kap5,
head(a5.df)

# Other separate variables(only one method specified)
b5=dataS$kap5[dataS$vyrobce=="B"]
c5=dataS$kap5[dataS$vyrobce=="C"]
d5=dataS$kap5[dataS$vyrobce=="D"]

a100=dataS$kap100[dataS$vyrobce=="A"]
b100=dataS$kap100[dataS$vyrobce=="B"]
c100=dataS$kap100[dataS$vyrobce=="C"]
d100=dataS$kap100[dataS$vyrobce=="D"]

pokles.a=dataS$pokles[dataS$vyrobce=="A"]
pokles.b=dataS$pokles[dataS$vyrobce=="B"]
pokles.c=dataS$pokles[dataS$vyrobce=="C"]
pokles.d=dataS$pokles[dataS$vyrobce=="D"]

# ** More detailed window for Dplyr library functions - work on data in standard data ####
# format
#  


# It is necessary to apply to data in st. data format !!! Pipe operator%>% - helps with
# chaining functions - in the new RSstudio shortcut key Ctrl + Shift + M
#  
# *** filter - applies a filter to the given column ####
#  


# filter - selects/filters rows based on given conditions
# Selection of products from the manufacturer
dataS %>% filter(vyrobce=="A")

# Selection of products from manufacturer A or B
# separating conditions correspond to the logical "or"
dataS %>% filter(vyrobce=="A" | vyrobce=="B")  

# Selection of all products with a decrease of 200 mAh and more from the manufacturer C
# comma separating conditions corresponds to logical "and at the same time"
dataS %>% filter(pokles>=200, vyrobce=="C")  

# *** mutate - produce a new column ####
#  


# mutate - adds a new variable or transforms an existing one
# Creating a new column drop_Ah, which indicates the capacity drop in Ah(original data in mAh, 1 Ah=1000 mAh)
pokus = dataS %>% mutate(pokles_Ah=pokles/1000)
head(pokus)
# Attention! if we do not save the result with the new column, it will only be printed and disappear


# *** summarize - generates summary characteristics of various variables ####
#  


# Calculation of the mean and median of all values of the variable kap5
dataS %>% summarise(prum=mean(kap5),median=median(kap5))

# *** arrange - sorts rows according to the selected variable ####
#  


# Ascending and descending order of rows according to the decrease value
dataS %>% arrange(pokles)

dataS %>% arrange(desc(pokles))

# *** group_by - groups values into groups according to the selected variable ####
#  


# the table is "virtually" divided into groups for later processing, eg summarize
dataS %>% group_by(vyrobce)

# Ideal for calculating summary characteristics for each manufacturer separately, eg average
dataS %>%
  group_by(vyrobce) %>% 
  summarise(prum=mean(kap5), "směrodatná odchylka"=sd(kap5))

# ** Final note on dplyr(which is good to finish until the end...)
#  
# Some operations may throw a "tibble" object. This is a more modern data.frame, however
# it can cause problems and cause error messages in some functions! You can easily
# convert this "tibble" object to data.frame using as.data.frame(). **
#  


#  5. Data conversion to standard data format(for the two most common data formats) ####
#  
# * From data in Data Matrix format ####
#  


data_DM = read_excel("./data/datova_matice.xlsx")
head(data_DM)

data_DM = data_DM[,-1]
colnames(data_DM) = c("A22", "A5", "B22", "B5", "C22", "C5", "D22", "D5")
head(data_DM)

# ** Reshape function ####
#  
# Its parameters:
#  
# - **data** -datato be converted must be fe format data.frame(as.data.frame(data))
#  
# - **direction** - whichdirectionwe want to transform
#  
# - "long" - to standard format
#  
# - "wide" - back to the data matrix
#  
# - **varying** - column names that indicate the same data for different categories
#  
# - it is a sheet of vectors
#  
# - each sheet item is one measurement
#  
# - each vector is then a list of columns
#  
# - **v.names** - column names in st. give. format
#  
# - The number of names must match the number of vectors in varying
#  
# - **times** - names of individual categories
#  
# - ATTENTION !! must be in the same order as the varying variable
#  
# - **timevar** - column name with categories
#  


data_DM_S=reshape(data=as.data.frame(data_DM),
                  direction="long",
                  varying=list(c("A5", "B5", "C5", "D5"),
                               c("A22","B22","C22","D22")),
                  v.names=c("5 C","22  C"),   
                  times=c("Amber","Bright","Clear","Dim"),  
                  timevar="vyrobce")
head(data_DM_S)

help(reshape)

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
data_PL_S

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
cetnosti=table(dataS$vyrobce)
cetnosti # listing - object of type "table" - mostly more suitable, but more difficult conversion to type data.frame


# .. and using dplyr functions(more complex)
abs.cetnosti = dataS %>%
                  group_by(vyrobce) %>%
                  summarise(cetnost = n())  # number of products for each manufacturer


abs.cetnosti # listing - object type "tibble" - useful when we need to simply convert to type data.frame


# ** Relative frequency table ####
#  


# By direct calculation
rel.cetnosti=100*cetnosti/sum(cetnosti)   
rel.cetnosti # statement


# or using the prop.table function
rel.cetnosti2=prop.table(cetnosti)*100
rel.cetnosti2 # statement


# or using the dplyr functions, where absolute frequencies will also be included
tabulka_abs_rel = dataS %>%
                    group_by(vyrobce) %>%
                    summarise(cetnost = n()) %>%   
                    mutate(rel_cet_proc = round(100*(cetnost / sum(cetnost) ),1) )
tabulka_abs_rel # statement
t(tabulka_abs_rel)

# For all tables, rounding and the associated risk of rounding error must be observed.
# The procedure for frequency and frequency2 is the same.
rel.cetnosti=round(rel.cetnosti,digits=1) # rounded to 1 decimal place
rel.cetnosti[4]=100-sum(rel.cetnosti[1:3]) # rounding error monitoring
rel.cetnosti

# The procedure for table_abs_rel is different due to a different format(tibble)
tabulka_abs_rel[4,3]=100-sum((tabulka_abs_rel[1:3,3]))
tabulka_abs_rel

# *** Create table with absolute and rel. frequencies(without gas). We have: ####
#  


cetnosti

rel.cetnosti

tabulka=cbind(cetnosti,rel.cetnosti)  # merge tables
colnames(tabulka)=c("četnost","rel.četnost (%)") # change column names
tabulka

# *** Save table to csv file ####
#  


write.csv2(tabulka,file="tabulka.csv")

# Where is the table stored? It is stored in the working directory without specifying the complete path in the previous command.
getwd()

# hr>
#  
# ** Visualization using graphs ####
#  


# Bar graph
# The basic(ie no package required) bar graph is based on the frequency table we have prepared
par(mfrow = c(1,1), # simple division of the graphics window - 1 row, 1 column
    mar = c(2,2,2,2), # margins around each of the graphs in line numbers - - c(bottom, left, top, right)
    oma = c(2,2,2,2)) # outer margins in number of lines - c(bottom, left, top, right)
barplot(cetnosti)

# Change colors, add name
barplot(cetnosti,
        col=heat.colors(4), # alt. a vector of specific colors can be chosen, eg c("blue", "yellow," red "," green ")
        # or other scales(heat.colors, topo.colors, terrain.colors and many others)
        main="Zastoupení výrobců ve výběru",
        space=0.6) # The space parameter creates a space between columns


# Add additional labels and legends
barplot(cetnosti,
        col=heat.colors(4),
        horiz=TRUE,                            # horizontal orientation of the graph
        border=FALSE,                   # does not draw a line around the bars
        main="Zastoupení výrobců ve výběru",
        names.arg=paste0("Výrobce \n",names(cetnosti))) 
# The paste0 function allows you to merge text strings and variable values, the "\ n" symbol forms a new line in the text
legend("right",                             # placing a legend next to a bar graph is very tricky
       paste("Výrobce",names(cetnosti)),       # it is much easier to work with ggplot2 in this case
       col=heat.colors(4),
       fill=heat.colors(4),
       border=TRUE,
       bty="n")

# Add absolute and relative frequencies to the corresponding columns
bp = barplot(cetnosti,
             col=heat.colors(4),
             main="Zastoupení výrobců ve výběru",
             names.arg=paste("Výrobce",names(cetnosti)))
text(bp,
     cetnosti,paste0(cetnosti,"; ",rel.cetnosti,"%"),
     pos=1)
# parameter pos specifies where the text will be given with respect to the given position(1=below, 2=left, 3=above, 4=right)


# Try to use the previous code and create a bar graph for the Manufacturer variable by
# yourself.
#  


#  7. Exploratory analysis and visualization of a quantitative variable ####
#  


# Descriptive statistics
summary(dataS$kap5)

# Calculation of the average of one variable
mean(dataS$kap5)

mean(a5)

# Beware of missing values
mean(data$C5)

mean(data$C5,na.rm=TRUE)

# Calculation of the median of one variable
quantile(dataS$kap5,probs=0.5)

quantile(a5,probs=0.5)

# Range determination
length(dataS$kap5)

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

skewness(a5)

kurtosis(a5)-3

# If we want to calculate the given characteristic for variable capacity after 5 cycles
# according to the manufacturers, we can use the tapply function
tapply(dataS$kap5, dataS$vyrobce, mean, na.rm=TRUE)

# or using dplyr - here pay attention to automatic(not always correct rounding)
dataS %>% 
  group_by(vyrobce) %>% 
  summarise(mean(kap5,na.rm=T))

# To simplify the work, we can use the dplyr function and put all the characteristics in one table
dataS %>%    # without using group_by for the whole kap5 variable
  summarise(rozsah=length(kap5),
            minimum=min(kap5,na.rm=T),     # preventive na.rm=T
            Q1=quantile(kap5,0.25,na.rm=T),
            prumer=mean(kap5,na.rm=T),
            median=median(kap5,na.rm=T),
            Q3=quantile(kap5,0.75,na.rm=T),
            maximum=max(kap5,na.rm=T),
            rozptyl=var(kap5,na.rm=T),
            smerodatna_odchylka=sd(kap5,na.rm=T),
            variacni_koeficient=(100*(smerodatna_odchylka/prumer)),  # coefficient of variation in percent
            sikmost=(moments::skewness(kap5,na.rm=T)),       # moments package precaution
            spicatost=(moments::kurtosis(kap5,na.rm=T)-3)) 

# Don't forget to round correctly!
# We use group_by and get the characteristics for the capacity after 5 cycles according to the manufacturers
# Due to the incomplete statement, it is advisable to save the output and view it in a new window
charakteristiky_dle_vyrobce = 
  dataS %>%
    group_by(vyrobce) %>% 
    summarise(rozsah=length(kap5),
            minimum=min(kap5,na.rm=T),
            Q1=quantile(kap5,0.25,na.rm=T),
            prumer=mean(kap5,na.rm=T),
            median=median(kap5,na.rm=T),
            Q3=quantile(kap5,0.75,na.rm=T),
            maximum=max(kap5,na.rm=T),
            rozptyl=var(kap5,na.rm=T),
            smerodatna_odchylka=sd(kap5,na.rm=T),
            variacni_koeficient=(100*(smerodatna_odchylka/prumer)),  # coefficient of variation in percent
            sikmost=(moments::skewness(kap5,na.rm=T)),
            spicatost=(moments::kurtosis(kap5,na.rm=T)-3))

charakteristiky_dle_vyrobce

# ** Box chart ####
#  
# **We draw for original data, we can add rendering for data without OP.**
#  


# Simple and fast rendering using the basic function only for manufacturer C
boxplot(c5)

# Further modification of the graph, use of the points function to display the average
boxplot(c5,
        main="Kapacita po 5 cyklech (mAh)", 
        xlab="Výrobce C",
        ylab="kapacita (mAh)",
        col="grey")
points(1, mean(c5,na.rm=TRUE), pch=3) # adds a point showing the average to the existing graph


# Horizontal orientation, box width change
boxplot(c5,
        main="Kapacita po 5 cyklech (mAh), výrobce C", 
        horizontal=TRUE,  # in horizontal orientation, the opposite setting of labels must be observed
        xlab="kapacita (mAh)",
        boxwex=0.5)  # changes the box width to 1/2


# Use the previous code and create a box chart according to you.
#  




# And draw a multiple box graph
boxplot(dataS$kap5~dataS$vyrobce) # graphic parameters can be set similarly to the previous ones


boxplot(a5,b5,c5,d5)

# ** Histogram ####
#  
# **We always plot for data without remote observations !!**
#  


# Simple and fast rendering
hist(a5)

hist(a5,breaks=20) # What do the different values of the breaks parameter do with the graph?


# Labels, colors and other parameters can be set traditionally
hist(a5, 
     main="Histogram pro kapacitu akumulátorů po 5 cyklech, výrobce A", 
     xlab="kapacita (mAh)",
     ylab="četnost",
     col="blue",       # fill color
     border="grey",    # column border color
     labels=TRUE)         # adds the absolute frequencies of the given categories in the form of labels


# Y-axis scaling to plot a probability density estimate
hist(a5, 
     main="Histogram pro kapacitu akumulátorů po 5 cyklech, výrobce A", 
     xlab="kapacita (mAh)",
     ylab="f(x)",
     col="cadetblue1", 
     border="grey",
     freq=FALSE)          # scaling on the y-axis ->f(x)
lines(density(a5))        # Attaches a probability density estimation graph
# Generate normal distribution density and add to histogram
xfit=seq(min(a5), max(a5), length=40)     # Generate values for the x-axis
yfit=dnorm(xfit, mean=mean(a5), sd=sd(a5))  # Generate values for the y-axis
lines(xfit, yfit, col="black", lwd=2)    # Add a curve to the last graph based on the values generated above
# This combined graph can be used for visual assessment of normality.


# **Use the previous code and create a histogram of yourself.**
#  




# ** QQ-graph ####
#  
# **We always plot for data without remote observations !!**
#  


# Simple and very fast rendering...
qqnorm(a5)
qqline(a5)

# .. with adjustment of axis labels...
qqnorm(a5, 
       xlab="Teoretické kvantily",
       ylab="Výběrové kvantily",
       main="QQ-graf, kapacita po 5 cyklech, výrobce A")
qqline(a5)



# For advanced and interested - automation, use of for-cycle, multiple graphs in one
# image If we use basic functions(barplot, boxplot, histogram), then the function par()
# or layout() is used. In these functions we specify the structure - how we want to draw
# more pictures
#  


# Eg. we want to draw a histogram and boxplot for capacity after 5 battery cycles from manufacturer A
pom=layout(mat = matrix(1:2,2,1, byrow=FALSE), height = c(2.5,1)) # structure creation
par(oma=c(2,2,3,2),mar=c(2,2,3,2)) # margin size adjustment

hist(a5, 
     main="Výrobce A",
     xlab="kapacita (mAh) po 5 cyklech", 
     ylab="četnost", 
     ylim=c(0,32), 
     xlim=c(1730,2040))
boxplot(a5, 
        horizontal=TRUE, 
        ylim=c(1700,2040), 
        boxwex=1.5)

# Using for-cycle histograms and boxplots for all manufacturers
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(1.5,1))

for (i in 1:4){
  hist(pull(data5,i), 
       main=paste("Výrobce",colnames(data5)[i]), 
       xlab="", 
       ylab="četnost", 
       xlim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
       ylim=c(0,32))
  boxplot(pull(data5,i), 
          horizontal=TRUE, 
          ylim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
          xlab="kapacita (mAh) po 5 cyklech",
          boxwex=1.5)
}
mtext("Kapacita akumulátorů (mAh) po 5 cyklech dle výrobců", cex = 1.1, outer=TRUE, side=3)

# Combination of histogram and QQ-fence
pom=layout(mat = matrix(1:8,2,4, byrow=FALSE), height = c(2,1.5))
par(oma=c(2,2,3,2), mar=c(2,2,3,2))

for (i in 1:4){
  hist(pull(data5,i), 
       main=paste("Výrobce",colnames(data5)[i]), 
       xlab="kapacita (mAh) po 5 cyklech", 
       ylab="četnost", 
       xlim=c(min(data5,na.rm=TRUE), max(data5,na.rm=TRUE)), 
       ylim=c(0,0.037),
       freq=FALSE)
  lines(density(pull(data5,i), na.rm=TRUE))
  xfit=seq(min(pull(data5,i), na.rm=TRUE), max(pull(data5,i), na.rm=TRUE), length=40) 
  yfit=dnorm(xfit, mean=mean(pull(data5,i), na.rm=TRUE), sd=sd(pull(data5,i), na.rm=TRUE)) 
  lines(xfit, yfit, col="blue", lty=2)
  qqnorm(pull(data5,i),main = "")
  qqline(pull(data5,i))
}
mtext("Kapacita akumulátorů po 5 cyklech (mAh)", cex = 1.5, outer=TRUE, side=3)

#  8. Internal walls and identification of remote observations ####
#  
# * Manual removal by counting the inner walls ####
#  


# data column separation for manufacturer
data_A_kap5 = dataS$kap5[dataS$vyrobce == "A"]
data_A_kap5

dolni_kvartil = quantile(data_A_kap5,0.25,na.rm=T)
horni_kvartil = quantile(data_A_kap5,0.75,na.rm=T)
IQR = horni_kvartil - dolni_kvartil  # interquartile margins
dolni_mez = dolni_kvartil - 1.5*IQR  # calculation of the lower between the inner walls
horni_mez = horni_kvartil + 1.5*IQR  # calculation of the upper between the inner walls
dolni_mez
horni_mez

data_A_kap5_bezOP = data_A_kap5
# set values that are out of limits to NA
data_A_kap5_bezOP[data_A_kap5>=horni_mez | data_A_kap5<=dolni_mez] = NA
data_A_kap5_bezOP

# we can delete NA hondots
data_A_kap5_bezOP = na.omit(data_A_kap5_bezOP)

# * Automatic removal according to the box fence ####
#  


pom = boxplot(data_A_kap5)

pom

data_A_kap5_bezOP = data_A_kap5
data_A_kap5_bezOP[data_A_kap5 %in% pom$out] = NA
data_A_kap5_bezOP

# ** How to do this for standard multi-category data? ####
#  


head(data5S)

pom = boxplot(data5S$kap5 ~ data5S$vyrobce)
pom

# **Caution, this needs to be done by category, otherwise we risk deleting the data into
# the data. the file belongs to !!!**
#  


vyrobci = unique(data5S$vyrobce) # c("A", "B",..)
vyrobci

data5S_bezOP = data5S

for(vyrobce in vyrobci){
    pom = boxplot(data5S$kap5[data5S$vyrobce == vyrobce],plot = FALSE)
    data5S_bezOP$kap5[data5S$kap5 %in% pom$out & data5S$vyrobce == vyrobce]=NA
}

boxplot(data5S_bezOP$kap5 ~ data5S_bezOP$vyrobce)

# **The analyst can always say that he will not delete outgoing observations, but he
# must include this information in the analysis report!**
#  


#  9 rule 3 $\sigma$ and Chebyshev's inequality ####
#  
# * Empirical verification of normality ####
#  
# **Based on data after deleting remote observations:**
#  


# we will use the data from the removal example op
data_A_kap5_bezOP = na.omit(data_A_kap5_bezOP)
data_A_kap5_bezOP

# We draw the QQ graph and calculate the skewness and sharpness:
#  


qqnorm(data_A_kap5_bezOP)
qqline(data_A_kap5_bezOP)

skewness(data_A_kap5_bezOP)
kurtosis(data_A_kap5_bezOP) - 3 # another definition shifted by 3


# - the dots in the QQ graph must lie approximately on the line - ie. the quantiles
# correspond approximately to the quantiles of the normal distribution
#  
# - skewness must lie in the interval<-2, 2>
#  
# - kurtosis must lie in the interval<-2.2>
#  
# - be careful we have to reduce the result of the R function by 3
#  
#  
# **If data normality is met ->rule 3σ**
#  
# σ: P(µ - σ<X<µ + σ)=0.68272σ: P(µ - 2σ<X<µ + 2σ)=0.95453σ: P(µ - 3σ<X<µ +
# 3σ)=0.9973
#  
# **If data normality is not met ->Chebyshev inequality**
#  
# σ: P(µ - σ<X<µ + σ)=02σ: P(µ - 2σ<X<µ + 2σ)=0.753σ: P(µ - 3σ<X<µ +
# 3σ)=0.8889
#  


mu = mean(data_A_kap5_bezOP)
sigma = sd(data_A_kap5_bezOP)
paste0("<", mu - sigma, ", ", mu + sigma, ">")
paste0("<", mu - 2*sigma, ", ", mu + 2*sigma, ">")
paste0("<", mu - 3*sigma, ", ", mu + 3*sigma, ">")

#  10. Rounding ####
#  
# Everything needed for rounding can be found on the LMS in the rounding document.
# https://lms.vsb.cz/pluginfile.php/1298954/mod_folder/content/0
# /Leg%C3%A1ln%C3%AD%20tah%C3%A1ky/zaokrouhlovani.pdfMost important:
#  
# - the standard deviation is rounded up to the prescribed number of digits(ceiling)
#  
# - data file size=<2.10>->1 valid digit
#  
# - data file size=(10.30>->2 valid digits
#  
# - data file size=(30,2000>->3 valid digits
#  
# - position measures(averages, quantiles,...) are then rounded to the same valid digit
# as the standard deviation
#  


length(data_A_kap5_bezOP)
smer_odch = sd(data_A_kap5_bezOP)
smer_odch

ceiling(smer_odch*10)/10

prumer = mean(data_A_kap5_bezOP)
prumer

round(prumer,digits=1)

max(data_A_kap5_bezOP)



