###MCS for coffee, smoking and lung cancer example

##Questions

#Rewrite the simulation script below using the SimDesign package.
#a. We will focus on comparing the empirical Type I ERROR rate of the effect of
#coffee drinking on lung cancer between the model adjusted for smoking and model
#unadjusted for smoking. In another words, out of 1,000 simulations, 
#how many times do we REJECT the null hypothesis: there is no effect of coffee 
#drinking on lung cancer, given that the true effect is null.
#b. Note that you can use the EDR() function in SimDesign package to compute 
#the above probability in the Summarise step.
#c. Note also that we need to fit two models, unadjusted and adjusted. 
#We can fit two models in one Analyze function or write two separate Analyze 
#functions like in this example.
#d. We will investigate the type I error rate by varying the sample size, 
#probability of smokers in the sample, probability of coffee drinkers in the 
#sample, and effect of smoking on coffee drinking.
#i. 𝑛=(50,500) 
#ii. 𝑝𝑧 = (0.2, 0.8) 
#iii. 𝛼0 =(−1,0,1) 
#iv. 𝛼1 =(0,0.5,1)
#e. First use a low number of iterations (5-10) to test the functions. 
#When ready, run the full simulation with 1,000 iterations.

#Create a figure to visually present the simulation results. 
#An example of a figure is shown below.

#Discuss results with your partner

##Answers

library(SimDesign)
library(dplyr)
library(tidyr)
library(ggplot2)
?SimFunctions
SimFunctions()

# logit(Pr(X=1)) = alpha0 + alpha1 * Z
# logit(Pr(Y=1)) = beta0 + beta1 * X + beta2 * Z
# X - coffee drinker
# Z - smoker
# Y - lung cancer


Design <- createDesign(sample_size = c(50, 500),
                       pz = c(0.2, 0.8),
                       alpha0 = c(-1, 0, 1),
                       alpha1 = c(0, 0.5, 1))

Generate <- function(condition, fixed_objects = NULL){
  alpha0 <- condition$alpha0
  alpha1 <- condition$alpha1
  pz <- condition$pz
  n <- condition$sample_size
  beta0 <- -3
  beta1 <- 0
  beta2 <- 2
  z <- rbinom(n, size = 1, prob = pz)
  px <- exp(alpha0 + alpha1 * z) / (1 + exp(alpha0 + alpha1 * z))
  x <- rbinom(n, size = 1, prob = px)
  py <- exp(beta0 + beta1 * x + beta2 * z) / (1 + exp(beta0 + beta1 * x + beta2 * z))
  y <- rbinom(n, size = 1, prob = py)
  dat <- data.frame(lung = y, coffee = x, smoke = z)
  dat
}

Analyse.unadj <- function(condition, dat, fixed_objects = NULL){
  glmout <- glm(lung ~ coffee, data = dat, family = "binomial")
  beta <- coef(glmout)[2]
  pval <- summary(glmout)$coef[2,4]
  ret <- c(unadj = unname(pval))
  ret
}

Analyse.adj <- function(condition, dat, fixed_objects = NULL){
  glmout <- glm(lung ~ coffee + smoke, data = dat, family = "binomial")
  beta <- coef(glmout)[2]
  pval <- summary(glmout)$coef[2,4]
  ret <- c(adj = unname(pval))
  ret
}

Summarise <- function(condition, results, fixed_objects = NULL){
  ret <- EDR(results, alpha = 0.05)
  ret
}

res <- runSimulation(design = Design, replications = 1000,
                     generate = Generate, analyse = list(Analyse.unadj, Analyse.adj), 
                     summarise=Summarise, save_results = TRUE)
res

## Create a figure that summarizes the results

reslong <- res %>%
  pivot_longer(cols = c("unadj", "adj"),
               names_to = "model",
               values_to = "edr")


reslong$sample_size <- factor(reslong$sample_size, levels = c(50,500),
                              ordered = TRUE, labels=c("n == 50", "n == 500"))
reslong$alpha0 <- factor(reslong$alpha0, levels = c(-1,0,1),
                         ordered = TRUE, labels=c("alpha[0] == -1", "alpha[0] == 0", "alpha[0] == 1"))

ggplot(reslong, aes(x = alpha1, y = edr, color = model, 
                    linetype = as.factor(pz), shape = as.factor(pz), 
                    group = interaction(model, pz))) +
  geom_point(size = 2) + 
  geom_line(linewidth = 1) + 
  scale_color_discrete(labels=c('Adjusted', 'Unadjusted')) + 
  facet_grid(sample_size ~ alpha0, labeller = label_parsed) + 
  theme_bw(base_size = 12) + 
  geom_hline(yintercept = 0.05, color = "darkgray", linetype = "longdash") + 
  labs(y = "Type I error", x = bquote(alpha[1]~values), color = "Model", shape = "Pr(Z=1)", linetype = "Pr(Z=1)", 
       title = "Empirical type I error rates for adjusted and unadjusted models \nfrom each simulation scenario (gray dashed horizontal line at 0.05) ")

