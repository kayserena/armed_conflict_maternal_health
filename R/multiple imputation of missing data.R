### multiple imputation of missing data

## instructions
# 1. Use the mice package to multiply impute the final data with 𝑚 = 10 imputations. 
# a. Use 2l.pan to impute all continuous level-1 variables.
# b. Note that the character variable ISO needs to be converted into a
# numeric variable. 
# 2. Compare the results from MI to complete case data for each outcome.

# libraries
library(mice)
library(here)

# read in data
finaldata <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)

# change ISO to numeric
finaldata$ISOnum <- as.numeric(as.factor(finaldata$ISO))

# setting methods for variables
mi0 <- mice(finaldata, seed = 1, m = 1, maxit = 0, print = F)
meth <- mi0$method
meth

meth[c("GDP_scaled", "popdens_scaled", "urban", "male_edu", "temp", "matmort", "neomort", "under5mort", "infantmort")] <- "2l.pan"
meth

# specify class of variables that are unchanging across repeated rows (by ISO) for one participant
pred <- mi0$predictorMatrix

pred[c("GDP_scaled", "popdens_scaled", "urban", "male_edu", "temp", "matmort", "neomort", "under5mort", "infantmort"), "ISOnum"] <- -2
pred

mice.multi.out  <- mice(finaldata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred, print = F)
save(mice.multi.out, file = "miceout.Rda")
load("miceout.Rda")
plot(mice.multi.out)

complete.data.multi <- complete(mice.multi.out, "all")

##check the first imputed dataset
head(complete.data.multi$`1`, n=20)

## redoing the scaling (oops I should have done that before)
complete.data.multi$`1`$GDP_scaled <- complete.data.multi$`1`$GDP / 1000
complete.data.multi$`1`$popdens_scaled <- complete.data.multi$`1`$popdens / 100

## running the linear models again
preds <- as.formula(" ~ binconf + GDP_scaled + OECD + popdens_scaled + urban + agedep + male_edu + temp + drought + earthquake + ISO + as.factor(Year)")

# create linear models
matmod <- lm(update.formula(preds, matmort ~ .), data = complete.data.multi$`1`)
neomod <- lm(update.formula(preds, neomort ~ .), data = complete.data.multi$`1`)
undmod <- lm(update.formula(preds, under5mort ~ .), data = complete.data.multi$`1`)
infmod <- lm(update.formula(preds, infantmort ~ .), data = complete.data.multi$`1`)

keepvars <- list("binconf" = "Armed conflict",
                 "GDP_scaled" = "GDP",
                 "OECD" = "OECD",
                 "popdens_scaled" = "Population density",
                 "urban" = "Urban",
                 "agedep" = "Age dependency",
                 "male_edu" = "Male education",
                 "temp" = "Average temperature",
                 "earthquake" = "Earthquake",
                 "drought" = "Drought")

# table of all model results

screenreg(list(matmod, undmod, infmod, neomod), 
          ci.force = TRUE,
          custom.coef.map = keepvars,
          custom.model.names = c("Maternal mortality", "Under-5 mortality",
                                 "Infant mortality", "Neonatal mortality"),
          caption = "Results from linear regression models")

# need to pool the estimates for the 10 imputed data sets
# need to replace variables here

fit.multi2 <- with(mice.multi.out, 
                   model1 <- geeglm(as.numeric(as.character(Y)) ~ age + skin + 
                                      treatment + gender + exposure + year + year2, 
                                    id = ID, scale.fix = TRUE,
                                    corstr = "ar1",
                                    family = binomial("logit")))
summary(pool(fit.multi2))