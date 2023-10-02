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

###viewing the data
here()
final <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)
head(final)
tail(final)
glimpse(final)
slice_sample(final, n=5)
summary(final)

#variable definitions
#country
#year
#ISO = country code (standard)
#region
#gdp
#oecd
#popdens
#urban
#agedep
#maternal mortality ratio per 100 000 live births (same with neonatal,
#under 5, and infant)

#missing data
#the following variables have missing data: GDP, popdens, urban, male edu,
#temp, matmort,neomort,under5mort,infantmort


