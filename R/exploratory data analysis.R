#exploratory data analysis of conflict data

#My exploratory data analysis plan for the armed conflict data will include viewing 
#the data at a glance (using head, tail, a random slice, and glimpse), 
#then calculating summary statistics for numeric variables. I may also use 
#Rohan Alexander's suggested method of looking for influential observations 
#when a random subset of the data are excluded, to look for influential 
#observations. I will investigate missing data (if any is missing). 
#I will learn the class of each variable, their contents (using 'unique'), 
#and look at their distribution using histograms. I will look for any 
#duplicates and remove them. I will also look at relationships between 
#certain variables by graphing them with scatter plots. 
#Variables that I would be interested in exploring in the armed conflict 
#data include the various mortality rates in each country, 
#changes across the years, relationship between presence of conflict and 
#mortality rates, and the relationship between education and conflict. 

#libraries
library(table1)
library(tableone)

###viewing the data
here()
final <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)
head(final)
tail(final)
glimpse(final)
slice_sample(final, n=5)
summary(final)

#variable definitions
#maternal mortality ratio per 100 000 live births (same with neonatal,
#under 5, and infant)
#urban residence (percentage of the population living in urban areas)
#popdens percentage of the population living in a density of >1,000 people/km 

#missing data
#the following variables have missing data: GDP, popdens, urban, male edu,
#temp, matmort,neomort,under5mort,infantmort

#frequency tables of categorical variables
table(final$binconf)
table(final$drought)
table(final$earthquake)

#distribution of categorical variables
hist(final$GDP)
hist(final$OECD)
hist(final$OECD2023)
hist(final$popdens)
hist(final$urban)
hist(final$matmort)
hist(final$neomort)
hist(final$under5mort)
hist(final$infantmort)

#plotting binconf against mortality rates to explore relationship
final %>% ggplot(aes(x=binconf, y=matmort)) + geom_point()

#mortality trend over the years
final %>% ggplot(aes(x=Year, y=matmort, group=ISO)) + geom_line(aes(color = as.factor(binconf)), alpha = 0.5) + labs(y = "Maternal mortality rate per 100 000", x = "Year") 
final %>% ggplot(aes(x=Year, y=neomort, group=ISO)) + geom_line(aes(color = as.factor(binconf)), alpha = 0.5) + labs(y = "Neonatal mortality rate per 100 000", x = "Year")
final %>% ggplot(aes(x=Year, y=infantmort, group=ISO)) + geom_line(aes(color = as.factor(binconf)), alpha = 0.5) + labs(y = "Infant mortality rate per 100 000", x = "Year")
final %>% ggplot(aes(x=Year, y=under5mort, group=ISO)) + geom_line(aes(color = as.factor(binconf)), alpha = 0.5) + labs(y = "Under 5 mortality rate per 100 000", x = "Year")



