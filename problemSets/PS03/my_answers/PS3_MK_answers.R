#####################
# Title: Problem Set 3
# Author: Mairi Kachur
# Date: 24 March 2026
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

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS", "stargazer"),  pkgTest)

# set wd for current folder

setwd("~/Documents/08. PhD Work/03. Classes/2026 Quants II HT /Problem Sets/PS03")

#####################
# Problem 1
#####################

# load data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/gdpChange.csv", stringsAsFactors = F)

# Look at data
summary(gdp_data)
str(gdp_data)

# First: Keep only relevant variables (GDPWdiff, REG, OIL) for Problem 1, 
# drop any missing values in dataset
gdp_q1 <- na.omit(gdp_data[, c("REG", "OIL", "GDPWdiff")])

# Then: create 3-category outcome from numeric GDP change
gdp_q1$GDPWdiff_cat <- ifelse(gdp_q1$GDPWdiff < 0, "negative",
                              ifelse(gdp_data$GDPWdiff == 0, "no change",
                                     ifelse(gdp_data$GDPWdiff > 0, "positive", NA)))
gdp_q1$GDPWdiff_cat <- factor(gdp_q1$GDPWdiff_cat,
                              levels = c("negative", "no change", "positive"))

# Check that step 2 worked: 
table(gdp_q1$GDPWdiff_cat)

# Question 1: Unordered multinomial logit with GDPWdiff as output
# and "no change" as reference category, including the estimated cutoff points and coefficients

# making "no change" the reference category
gdp_q1$GDPWdiff_unordered <- relevel(
  factor(trimws(gdp_q1$GDPWdiff_cat),
         levels = c("no change", "negative", "positive")),
  ref = "no change"
)


# Run unordered multinomial logit
unordered_model <- multinom(GDPWdiff_unordered ~ OIL + REG, data = gdp_q1)
summary(unordered_model)


stargazer(unordered_model,
          type = "latex",
          title = "Table of Coefficients: Unordered Multinomial Logit",
          covariate.labels = c("Oil", "Democracy"))



# Question 2: Ordered multinomial logit with GDPWdiff as outcome,
# including estimated cutoff points and coefficients


# Re-order the GDPWdiff variable
gdp_q1$GDPWdiff_ordered <- ordered(
  trimws(gdp_q1$GDPWdiff_cat),
  levels = c("negative", "no change", "positive"))


ordered_model <- polr(GDPWdiff_ordered ~ REG + OIL,
                   data = gdp_q1,
                   method = "logistic",
                   Hess = TRUE)

summary(ordered_model)

stargazer(ordered_model, 
          type = "latex",
          title = "Ordered Multinomial Logit Model",
          dep.var.labels = "GDPWdiff (negative < no change < positive)",
          digits = 3,
          single.row = FALSE
)



#####################
# Problem 2
#####################

# load data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_2026/main/datasets/MexicoMuniData.csv")


## Run a poisson regression
# run model
poisson_model_mexico <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + 
                       PAN.governor.06, data = mexico_elections,
                     family = "poisson")
summary(poisson_model_mexico)

# create table
stargazer(poisson_model_mexico,
          report = "vctp*",
          type = "latex",
          title = "Table of Coefficients: Poisson Model Mexico",
          style = "default",
          covariate.labels = c("Competitive District", 
                               "Poverty",
                               "PAN-Affilitated Governor"))


## Estimate mean number of visits
# create dataframe with the hypothetical values
hypothetical_values <- data.frame(
  competitive.district = 1,
  marginality.06 = 0,
  PAN.governor.06 = 1)

# use predict function and hypothetical df to generate prediction
mean_visits <- predict(poisson_model_mexico, hypothetical_values, type = "response")

# calculation by hand
mean_visits_manual <- exp(-3.81023 - 0.08135*1 - 0.31158*1)

# compare
print(c(mean_visits, mean_visits_manual))

