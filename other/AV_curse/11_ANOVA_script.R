###########################################################################
####################### ANOVA #############################################
###################### Adéla Vrtkova ######################################

# Import data - Rabbits
# for ANOVA, it is useful to import standard data matrix and also the "table" (both structures)
# let's call the table-format data and the standard data matrix dataS
data=read.csv2("https://homel.vsb.cz/~vrt0020/statistics/rabbits.csv")
dataS=read.csv2("https://homel.vsb.cz/~vrt0020/statistics/rabbits_stand.csv")

boxplot(data) # first look, outliers?, possible results?

# testing normality
shapiro.test(data$rabA)
shapiro.test(data$rabB)
shapiro.test(data$rabC)

# normality not rejected -> Bartlett test for equality of variances
bartlett.test(data)

# Independent + Normality + Equal Variances
results <- aov(dataS$values~dataS$ind) # standard data matrix required !!!
summary(results)

TukeyHSD(results)

## + Interpretation!!! 

#######################################################################################################

# Import data - Soil-samples
# for ANOVA, it is useful to import standard data matrix and also the "table" (both structures)
# let's call the table-format data and the standard data matrix dataS
data=read.csv2("https://homel.vsb.cz/~vrt0020/statistics/soil.csv")
dataS=read.csv2("https://homel.vsb.cz/~vrt0020/statistics/soil_stand.csv")

boxplot(data) # first look, outliers?, possbile results?

# testing normality
shapiro.test(data$khed)
shapiro.test(data$bhor)
shapiro.test(data$velhe)

# normality not rejected -> Bartlett test for the equality of variances
bartlett.test(data)

# Independent + Normality + Equal variances -> ANOVA
results = aov(dataS$values~dataS$ind) # standard data matrix required
summary(results) # equality of means is rejected -> Post-hoc analysis

TukeyHSD(results)

# + Interpretation
