# Tutorial for Rstudio ####

# If the text does not display correctly, set File \ Reopen with Encoding ... to UTF-8
# Use CTRL + SHIFT + O to display the contents of the script
# Use CTRL + ENTER to run commands on each line

# * Installing packages ####
# Packages only need to be installed once and will then be available to everyone
# application using the given Rko

# install.packages ('readxl')
# Use TAB to turn on whispering.

# You can see the progress of the command at the bottom of the console
library(readxl)

# * Načítání dat ####

# The path you see in 'Files' is not always the same as wd
getwd()

# setwd ('path to current file')
# using 'More' in the file browser

data = read.csv2 (file = "./data/aku.csv")

# loading can be done using GUI
# File-> Import Dataset (csv, Excel reader)

# * Displaying outputs ####
# render to console
data
# or open via 'Enviroment'

# plotting a graph - appears in 'Plots'
boxplot (data)
# Use 'Zoom' to open it in its own window
# we can save to a file

# * Stop current calculation, restart kernel ####
# Session->Interupt R
# Session->Restart R
a = 1
while(5>3)
{
  a = a + 1
}
a

# * help
help(sequence)

