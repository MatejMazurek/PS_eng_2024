# ......................................................................................
# ............Exercise 12. Multiselective tests - Extra for those interested ...........
# .....................................Michal Béreš.....................................
# ......................................................................................

# If text does not display correctly, set File \Reopen with Encoding ... to UTF-8 
# Use CTRL + SHIFT + O to display the contents of the script 
# Use CTRL + ENTER to run commands on a single line 

# * We load test data and produce post-hoc + effects for ANOVA and KW test ####
#  


data = readxl::read_excel("data/snehurka.xlsx")
# data is in standard data format


# POST-HOC ANOVA
vysledky = aov(data$hodnota ~ data$typ) 
PH.ANOVA = TukeyHSD(vysledky)[[1]]
PH.ANOVA

# ANOVA effects calculation
library(dplyr)

# overall average
prumer_vsech = mean(data$hodnota)

# averages in groups
efekty = data %>% group_by(typ) %>% 
    summarize(mean_skup = mean(hodnota))

# effects
efekty$efekt = efekty$mean_skup - prumer_vsech

# list sorted
efekty.ANOVA = efekty %>% arrange(desc(efekt))
efekty.ANOVA

# POST-HOC KW
# post hoc - another function with an output that suits us better
# numerically corresponds to that used for the exercise

# install.packages("FSA")
result = FSA::dunnTest(data$hodnota ~ data$typ,   # FSA library
              method="bonferroni")
PH.KW = result$res
PH.KW

# KW effect calculation
library(dplyr)

# overall average
prumer_vsech = median(data$hodnota)

# group averages
efekty = data %>% group_by(typ) %>% 
    summarize(mean_skup = median(hodnota))

# effects
efekty$efekt = efekty$mean_skup - prumer_vsech

# list sorted
efekty.KW = efekty %>% arrange(desc(efekt))
efekty.KW

#  For those interested(optional) - creation of a sorted table of p-values/letter ####
# scheme automatically
#  


# install.packages("strings")
# this is a text search library
# we'll look for smurf names in paired post-hoc tests

# we initialize the matrix(for a nice result as a text)
# 7x7 because we have 7 smurfs
POST.HOC.Phodnoty = matrix(rep("-", len = 7*7), nrow = 7, ncol = 7)
# we name its columns and rows according to the sorted smudges
colnames(POST.HOC.Phodnoty) = efekty.ANOVA$typ
rownames(POST.HOC.Phodnoty) = efekty.ANOVA$typ
POST.HOC.Phodnoty

# loop through all tests in post-hoc(over column names)
for(pair.test in rownames(PH.ANOVA)){
    # Which dwarves are present in this paired test?
    trp.truefalse = stringi::stri_detect_fixed(pair.test, efekty.ANOVA$typ)
    # what are the indices of these dwarves
    # indexes for writing to the matrix - always 2 values
    indexy.trp = which(trp.truefalse) 
    # I write to the matrix(the first index is smaller ->automatically to
    # upper triangle)
    POST.HOC.Phodnoty[indexy.trp[1], indexy.trp[2]] = 
        round(max(PH.ANOVA[pair.test, 'p adj'], 0.001), digits = 3)
    # we write text(if the matrix is text, the numbers are automatically
    # convert to text), values to thousands
}
POST.HOC.Phodnoty

# ** Functions for automated sign scheme(handwritten and from package) ####
#  
# *** Handwritten functions(what we would do on paper) ####
#  


# table of p-values

tabulka.phodnot = function(setrizene.typy, parove.testy.nazvy, 
                           parove.testy.phodnoty){
    # number of groups
    n = length(setrizene.typy)
    POST.HOC.Phodnoty = matrix(rep(0, len = n*n), nrow = n, ncol = n)
    # we name its columns and rows according to sorted types
    colnames(POST.HOC.Phodnoty) = setrizene.typy
    rownames(POST.HOC.Phodnoty) = setrizene.typy
    
    # loop through all tests in post-hoc(over column names)
    for(i in 1:length(parove.testy.nazvy)){
        pair.test = parove.testy.nazvy[i]
        # Which dwarves are present in this paired test?
        typ.truefalse = stringi::stri_detect_fixed(pair.test, setrizene.typy)
        # what are the indices of these dwarves
        # indexes for writing to the matrix - always 2 values
        indexy.typ = which(typ.truefalse) 
        # I write to the matrix(the first index is smaller ->automatically to
        # upper triangle)
        POST.HOC.Phodnoty[indexy.typ[1], indexy.typ[2]] = 
            parove.testy.phodnoty[i]
    }
    return(POST.HOC.Phodnoty)  
}

# letter diagram from the table

pismenkove.schema = function(POST.HOC.Phodnoty, alpha){
    # how big is the matrix
    n = nrow(POST.HOC.Phodnoty)
    # matrix initialization
    pis.schema = matrix(rep(0, len = n*n), nrow = n, ncol = n)
    # line names - copy from input
    rownames(pis.schema) = rownames(POST.HOC.Phodnoty)
    # set the diagonal to 1 - is in the given group
    diag(pis.schema) = 1
    
    # cycle through all columns where we can fill something
    for(i in 1:(n-1)){
        # cycle through all the rows in the column where we follow the gallop
        for(j in (i+1):n){
            # if pval>alpha then we add to hom. groups
            pis.schema[j, i] = POST.HOC.Phodnoty[i,j]>alpha
        }
    }
    return(pis.schema)
}

# *** How to use handwritten functions for ANOVA and KW test? ####
#  


# How to do it from POST-HOC ANOVA:

# we produce input data
setrizene.typy = efekty.ANOVA$typ
parove.testy.nazvy = rownames(PH.ANOVA)
parove.testy.phodnoty = PH.ANOVA[,'p adj']

# we produce a sorted phodnot table
p.val.tab = tabulka.phodnot(setrizene.typy, parove.testy.nazvy, 
                            parove.testy.phodnoty)
# draw rounded to thousands
round(p.val.tab, digits = 3)

# we make a letter scheme from the phodnot table
pis.schema = pismenkove.schema(p.val.tab, 0.05)
pis.schema

# How to do it from POST-HOC KW:

# we produce input data
setrizene.typy = efekty.KW$typ
parove.testy.nazvy = PH.KW$Comparison
parove.testy.phodnoty = PH.KW$P.adj

# we produce a sorted phodnot table
p.val.tab = tabulka.phodnot(setrizene.typy, parove.testy.nazvy, 
                            parove.testy.phodnoty)
# draw rounded to thousands
round(p.val.tab, digits = 3)

# we make a letter diagram from the phodnot table
pis.schema = pismenkove.schema(p.val.tab, 0.05)
pis.schema

# * Letter scheme using the built-in Rk function ####
#  
# rcompanion package, cldList function
#  


# in case of ANOVA

# first we make a dataframe with columns of pairs and phodnot
input = data.frame(dvojice = rownames(PH.ANOVA), 
                   pval = PH.ANOVA[,'p adj'])

# letter scheme, library rcompanion
# install.packages("rcompanion")
rcompanion::cldList(pval ~ dvojice, 
        data = input,
        threshold = 0.05)

# in the case of KW

# first we make a dataframe with columns of pairs and phodnot
input = data.frame(dvojice = PH.KW$Comparison, 
                   pval = PH.KW$P.adj)

# letter scheme, library rcompanion
# install.packages("rcompanion")
rcompanion::cldList(pval ~ dvojice, 
        data = input,
        threshold = 0.05)



