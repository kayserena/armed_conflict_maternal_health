### linear models

library(here)

## modelling 4 outcomes: matmort, under5mort, infantmort, neomort (4 models)

## need to re-scale the coefficients: GDP was scaled up by 1000, need to scale it down
## population density needs to be rescaled from 0 to 1

# read in data
finaldata <- read.csv(here("cleandata", "finaldata.csv"), header = TRUE)

# rescale
finaldata$GDP_scaled <- finaldata$GDP / 1000
finaldata$popdens_scaled <- finaldata$popdens / 100

# put predictors into formula
preds <- as.formula(" ~ binconf + GDP_scaled + OECD + popdens_scaled + urban + agedep + male_edu + temp + drought + earthquake + ISO + as.factor(Year)")

# create linear models
matmod <- lm(update.formula(preds, matmort ~ .), data = finaldata)
neomod <- lm(update.formula(preds, neomort ~ .), data = finaldata)
undmod <- lm(update.formula(preds, under5mort ~ .), data = finaldata)
infmod <- lm(update.formula(preds, infantmort ~ .), data = finaldata)

# labelling variables
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

# to create a tex table, need to knit table

texreg(list(matmod, undmod, infmod, neomod), 
          ci.force = TRUE,
          custom.coef.map = keepvars,
          custom.model.names = c("Maternal mortality", "Under-5 mortality",
                                 "Infant mortality", "Neonatal mortality"),
          caption = "Results from linear regression models")
