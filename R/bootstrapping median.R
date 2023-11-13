###bootstrapping median se
#nov 13 2023 in class

#libraries
library(dplyr)
library(boot)
library(here)
library(pander)

#read in data
finaldata <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)

#Summary statistics by armed conflict
#subsetting 2017
data2017 <- finaldata[finaldata$Year == 2017, ]

#subsetting only if matmort is not missing
data2017 <- data2017[complete.cases(data2017$matmort), ]

#matmort values by conflict in 2017, removing missing
data2017 |>
  group_by(binconf) |>
  summarise(n = n(),
            median.matmor = median(matmort, na.rm = T))

#difference in medians by conflict status
obs.med.diff <- median(data2017[data2017$binconf == 1,]$matmort) - median(data2017[data2017$binconf == 0,]$matmort)
obs.med.diff
#126.5

#How can we put a confidence interval around the difference in medians?
#Use stratified bootstrap (sampling from appropriate level of armed conflict)
#this is a manual way of bootstrapping - then we will look at the boot function from the package
#both work but boot package is easier

matmor.arm1 <- finaldata |>
  dplyr::filter(Year == 2017 & !is.na(matmort) & binconf == 1) |>
  dplyr::select(ISO, matmort)
matmor.arm0 <- finaldata |>
  dplyr::filter(Year == 2017 & !is.na(matmort) & binconf == 0) |>
  dplyr::select(ISO, matmort)
B <- 1000
med.diff <- rep(NA, B)
for(b in 1:B){
  resamp.arm1 <- matmor.arm1[sample(nrow(matmor.arm1), size = nrow(matmor.arm1), replace = TRUE),]
  resamp.arm0 <- matmor.arm0[sample(nrow(matmor.arm0), size = nrow(matmor.arm0), replace = TRUE),]
  med.diff[b] <- median(resamp.arm1$matmort) - median(resamp.arm0$matmort)
}
head(resamp.arm1, 12)

hist(med.diff, main = "Distribution of bootstrap statistic")

#Use the boot package
#Need to write a function for computing the statistic with at least two arguments

#First argument passed will always be the original data
#Second argument is a vector of indices
#With the strata option, the resampling is done within the specified strata

getmeddiff <- function(data, indices) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data$matmort, sample_data$binconf, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}
#this makes the function we can use to calculate the median difference in 2 groups, then we bootstrap that with boot function

bootout <- boot(data2017, statistic = getmeddiff, strata = data2017$binconf, R = 1000)
#want median difference (exposed and unexposed)
#data2017 to subset 2017 observations
#deleted any with missing matmort
#stratified by armed conflict status
#number of iterations is 1000

bootout

#Output from boot function
#The observed value of the statistic applied to data
#call it with t0
bootout$t0

#A matrix with sum(R) rows each of which is a bootstrap replicate of the 
#result of calling statistic.
head(bootout$t)

#Bootstrap standard error estimate is the standard deviation of the bootstraps values
sd(bootout$t)

#Bootstrap confidence intervals
#There are several different methods to calculate confidence intervals from the
#bootstrap samples:
  
#Percentile
#we have a distribution of the statistic
#this method selects the alpha/2th percentile as the lower CI and 1-alpha/2th as upper
quantile(bootout$t, probs = c(0.025, 0.975))

#Basic
#thetahat is the observed statistic, 126.5 in this case
#take it and subtract the upper percentile CI
#same with lower
#lower CI here goes lower than the distribution, so this method isn't range preserving for skewed data

2 * bootout$t0 - quantile(bootout$t, probs = 0.975)
2 * bootout$t0 - quantile(bootout$t, probs = 0.025)

#Bias-corrected and accelerated (BCa)
#best method because biased corrected
#to understand it, we also look at basic and percentile methods
boot.ci(boot.out = bootout, conf = 0.95, type = c("basic", "perc", "bca"))

###in class assignment
#Create a table (in HTML or PDF) that shows the differences in median 
#(with the BCa bootstrap 95% confidence intervals) maternal, infant, neonatal,
#and under-5 mortality between the countries exposed to versus not exposed to 
#armed conflict for the year 2017. Be specific about the sample sizes used in 
#each statistic. The table should be fully reproducible. Push the script that 
#creates the table to your GitHub repository.
#use at least 1000 bootstrap iterations

#making function generalizable
getmeddiff1 <- function(data, indices, mort) {
  sample_data <- data[indices, ]
  group_meds <- tapply(sample_data[,mort], sample_data$binconf, FUN = median)
  meddiff <- group_meds[2] - group_meds[1]
  return(meddiff)
}

#indices specifies rows of data, here I am using all in the matmort set
#testing out function
getmeddiff1(data2017, 1:183, mort = "matmort")
#mort then needs to be specified in the boot function as well

#infant mortality
data2017inf <- finaldata[finaldata$Year == 2017 & complete.cases(finaldata$infantmort), ]
bootoutinf <- boot(data2017inf, statistic = getmeddiff1, strata = data2017inf$binconf, R = 1000, mort = "infantmort")
bootoutinf
bootoutinf$t0
bootinfci <- boot.ci(boot.out = bootoutinf, conf = 0.95, type = "bca")
bootinfci

#neonatal mortality
data2017neo <- finaldata[finaldata$Year == 2017 & complete.cases(finaldata$neomort), ]
bootoutneo <- boot(data2017neo, statistic = getmeddiff1, strata = data2017neo$binconf, R = 1000, mort = "neomort")
bootoutneo
bootoutneo$t0
bootneoci <- boot.ci(boot.out = bootoutneo, conf = 0.95, type = "bca")
bootneoci

#under5mortality
data2017und <- finaldata[finaldata$Year == 2017 & complete.cases(finaldata$under5mort), ]
bootoutund <- boot(data2017und, statistic = getmeddiff1, strata = data2017und$binconf, R = 1000, mort = "under5mort")
bootoutund
bootoutund$t0
bootundci <- boot.ci(boot.out = bootoutund, conf = 0.95, type = "bca")
bootundci

#again for matmort to make table building easier
data2017mat <- finaldata[finaldata$Year == 2017 & complete.cases(finaldata$matmort), ]
bootoutmat <- boot(data2017mat, statistic = getmeddiff1, strata = data2017mat$binconf, R = 1000, mort = "matmort")
bootoutmat
bootoutmat$t0
bootmatci <- boot.ci(boot.out = bootoutmat, conf = 0.95, type = "bca")
bootmatci

#make table with values
#store each variable in a vector
#make table of new dataframe

#vector for sample size
samplesize <- c(length(data2017mat$matmort), length(data2017neo$neomort), length(data2017inf$infantmort), length(data2017und$under5mort))

#vector for med diff
diffs <- c(bootoutmat$t0, bootoutneo$t0, bootoutinf$t0, bootoutund$t0)

#vector for CIs
lowCI <- round(c(bootmatci$bca[4], bootneoci$bca[4], bootinfci$bca[4], bootundci$bca[4]),2)
uppCI <- round(c(bootmatci$bca[5], bootneoci$bca[5], bootinfci$bca[5], bootundci$bca[5]),2)

CIs <- paste0("(",lowCI,",",uppCI,")")
CIs
#vector for mortality type
morttype <- c("Maternal", "Neonatal", "Infant", "Under 5")

#want table to have: mortality type, n, median diff, bca CI
#create dataframe
tabdata <- data.frame(morttype, samplesize, diffs, CIs)
tabdata

#final table
pander(tabdata)
