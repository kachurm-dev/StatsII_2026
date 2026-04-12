#####################
# Title: Problem Set 4
# Author: Mairi Kachur
# Date: 10 April 2025
#####################
# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}


lapply(c("nnet", "MASS", "survival", "eha", "stargazer", "sampleSelection"),  pkgTest)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#####################
# Problem 1
#####################

# load data on child mortality by mother's background and child gender
data("child")

str(child)

# Creating new dataset with only relevant variables 
child_data <- na.omit(child[, c("enter", "exit", "event", "m.age", "sex")])

# fit Cox proportional hazards model
child_cox_model <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child_data)

# model output
summary(child_cox_model)

# Calculating 95% confidence interval

exp(confint(child_cox_model))

stargazer(
  child_cox_model,
  type = "latex",
  title = "Cox Proportional Hazards Model of Child Mortality",
  dep.var.labels = "Hazard of child death",
  covariate.labels = c("Mother's age", "Female"),
  digits = 3,
  single.row = FALSE,
  no.space = TRUE
)

#####################
# Problem 2
#####################
disaster <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/refs/heads/main/datasets/disaster_response.csv")

# retain only the variables required for Question 2
disaster_clean <- disaster[, c(
  "binContribution",
  "originalContributionMillionUSDLogged",
  "occurrences",
  "deathsEM",
  "normalizedDamageEMLogged"
)]

# inspect the main distributional features and check for missing values
summary(disaster_clean)
colSums(is.na(disaster_clean))

# verify that binContribution is coded as 0/1 and inspect the class balance
table(disaster_clean$binContribution)
# -> the classes are uneven, but not enough to cause major concern here

# check whether the logged contribution variable uses -25.328 as a marker for no donation
tapply(
  disaster_clean$originalContributionMillionUSDLogged,
  disaster_clean$binContribution,
  summary
)

# -> when binContribution = 0, the logged contribution always equals -25.328
# -> when binContribution = 1, the values show real variation

# in the Heckman setup, the outcome is only defined for observations with selection = 1
# therefore, assign NA to contribution amounts when no donation was made
disaster_clean$originalContributionMillionUSDLogged[
  disaster_clean$binContribution == 0
] <- NA

# keep observations with valid selection and predictor data;
# missing outcome values are acceptable only when selection = 0
disaster_clean <- subset(
  disaster_clean,
  !is.na(binContribution) &
    !is.na(occurrences) &
    !is.na(deathsEM) &
    !is.na(normalizedDamageEMLogged) &
    !(binContribution == 1 & is.na(originalContributionMillionUSDLogged))
)

# recode the selection indicator as a binary factor
# the value "1" represents cases where a donation is observed
disaster_clean$binContribution <- factor(disaster_clean$binContribution, levels = c(0, 1))

# estimate the Heckman selection model using maximum likelihood
disaster_heckman <- selection(
  selection = binContribution ~ occurrences + deathsEM + normalizedDamageEMLogged,
  outcome   = originalContributionMillionUSDLogged ~ occurrences + deathsEM + normalizedDamageEMLogged,
  data      = disaster_clean,
  method    = "ml"
)

# display the complete set of model results
summary(disaster_heckman)

