### week 11 in class optimization
## Libraries and data
library(here)
library(optimx)
library(tidyverse)
library(dplyr)
library(CIplot)

here()
final <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)
head(final)

# derive Y, E and D
#unlag year: 
#treat year of binary conflict as if it is the year before, since we added 1 before merging
#so 2018 year for armed conflict would be 2019 in the final dataset, since 2018 data was +1

final$Y <- ifelse(final$Year == 2019 & final$binconf == 1, 1, 0)
final$E <- ifelse(final$Year == 2010 & final$earthquake == 1 | final$Year == 2011 & final$earthquake == 1 | final$Year == 2012 & final$earthquake == 1 | final$Year == 2013 & final$earthquake == 1 | final$Year == 2014 & final$earthquake == 1 | final$Year == 2015 & final$earthquake == 1 | final$Year == 2016 & final$earthquake == 1 | final$Year == 2017 & final$earthquake == 1, 1, 0)
final$D <- ifelse(final$Year == 2010 & final$drought == 1 | final$Year == 2011 & final$drought == 1 | final$Year == 2012 & final$drought == 1 | final$Year == 2013 & final$drought == 1 | final$Year == 2014 & final$drought == 1 | final$Year == 2015 & final$drought == 1 | final$Year == 2016 & final$drought == 1 | final$Year == 2017 & final$drought == 1, 1, 0)

head(final)

#now I need to merge the dataset so that there is one row per country only
#and if there was a 1 at all for the country for Y, E or D, it is retained, otherwise it's a 0
#drop other variables

collapsed_data <- final %>%
  group_by(ISO) %>%
  summarise(
    Y_present = as.numeric(any(Y == 1)),
    E_present = as.numeric(any(E == 1)),
    D_present = as.numeric(any(D == 1)))

#converting from tibble to dataframe
collapsed <- as.data.frame(collapsed_data)
head(collapsed)

#create glm logistic regression model with Y as the outcome and D and E as the predictors
model <- glm(Y_present ~ D_present + E_present, data = collapsed, family = binomial)
summary(model)$coef

## using optimx package
negll <- function(par){
  y <- collapsed$Y_present
  x1 <- collapsed$E_present
  x2 <- collapsed$D_present 
  # 1. Calculate xbeta
  xbeta <- par[1] + par[2] * x1 + par[3] * x2
  # 2. Calculate p
  p <- exp(xbeta) / (1 + exp(xbeta))
  # 3. Calculate negative log-likelihood
  val <- -sum(y * log(p) + (1 - y) * log(1 - p))
  return(val)
}

library(optimx)
opt <- optimx(
  par = c(0,0,0),
  fn = negll,
  control = list(trace = 0, all.methods = TRUE)
)

summary(opt, order = "convcode")

# Extract hessian matrix for BFGS optimization
hessian_m <- attributes(opt)$details["BFGS", "nhatend"][[1]]
fisher_info <- solve(hessian_m)
prop_se <- sqrt(diag(fisher_info))
prop_se

## compare GLM to Optimx
# glm parameters
# intercept -2.0463054 SE 0.3042874
# beta_drought 0.9522947 SE 0.3746477
# beta_earth 0.9034511 SE 0.3917962

# optimx parameters BFGS
# intercept -2.046141 SE 0.3042700
# beta_drought 0.952264801 SE 0.3746297
# beta_earth 0.90360267  SE 0.3917786

#optimx BFGS values are very similar to glm values
# similar values across the first 7 methods of optimizing