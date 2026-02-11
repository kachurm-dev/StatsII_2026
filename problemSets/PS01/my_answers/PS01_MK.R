#####################
# Title: Problem Set 1, Quantitative Methods 2
# Author: Mairi Kachur
# Date last modified: 10 February 2026
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

lapply(c(),  pkgTest)

# set wd for current folder
setwd("~/Documents/PhD/03. Classes/2026 Quants II HT /Problem Sets")


# 1) KS statistic (manual, from definition) and cleaning input
ks_statistic_manual <- function(x, F_fun) {
  x <- x[is.finite(x)]
  n <- length(x)
  
  x_sorted <- sort(x) # sorting data because KS uses ordered values
  F_i <- F_fun(x_sorted) 
  
  # guard against tiny numerical issues
  F_i <- pmin(pmax(F_i, 0), 1)
  
  i <- seq_len(n)
  D_plus  <- max(i / n - F_i)
  D_minus <- max(F_i - (i - 1) / n)
  D <- max(D_plus, D_minus)
  
  list(D = D, D_plus = D_plus, D_minus = D_minus, n = n, x_sorted = x_sorted, F_i = F_i)
}

# 2) KS CDF approximation:
# Provided formula: P(D <= d) = (sqrt(2*pi) / d) * sum_{k=1}^inf exp(-(2k-1)^2 * pi^2 / (8 d^2))
# I approximate infinity by summing k=1..K and stop early if terms become tiny.
ks_cdf <- function(d, K = 5000, tol = 1e-12) {
  if (!is.finite(d) || d <= 0) return(0)
  
  k <- 1:K
  expo <- -((2 * k - 1)^2 * pi^2) / (8 * d^2)
  terms <- exp(expo)
  
  # optional early stop: keep terms until they drop below tol
  idx <- which(terms < tol)
  if (length(idx) > 0) {
    terms <- terms[1:(min(idx) - 1)]
  }
  
  cdf <- (sqrt(2 * pi) / d) * sum(terms)
  
  # clamp to [0,1] for numerical safety
  cdf <- pmin(pmax(cdf, 0), 1)
  cdf
}

# 3) Full manual KS test wrapper for Normal reference
ks_test_manual_normal <- function(x, mean = 0, sd = 1, K = 5000, tol = 1e-12,
                                  make_plot = FALSE, plot_file = "ECDF_vs_Normal.pdf") {
  if (!is.finite(sd) || sd <= 0) stop("sd must be > 0.")
  F_fun <- function(t) pnorm(t, mean = mean, sd = sd)
  
  stat <- ks_statistic_manual(x, F_fun)
  D <- stat$D
  
  # PS series gives P(D <= d)
  D <- stat$D
  d_scaled <- sqrt(stat$n) * D
  
  cdf_prob <- ks_cdf(d_scaled, K = K, tol = tol)
  p_value <- 1 - cdf_prob
  
  
  if (isTRUE(make_plot)) {
    pdf(plot_file, width = 8, height = 5)
    plot.ecdf(x, main = "ECDF(data) vs Normal CDF", verticals = TRUE,
              do.points = FALSE, xlab = "x", ylab = "CDF")
    curve(pnorm(x, mean = mean, sd = sd), add = TRUE, lty = 2)
    legend("bottomright",
           legend = c("ECDF(data)", sprintf("Normal CDF (mean=%g, sd=%g)", mean, sd)),
           lty = c(1, 2), bty = "n")
    dev.off()
  }
  
  list(
    D = D,
    D_plus = stat$D_plus,
    D_minus = stat$D_minus,
    n = stat$n,
    mean = mean,
    sd = sd,
    cdf_at_D = cdf_prob,
    p_value = p_value
  )
}

set.seed(123)
x <- rcauchy(1000, location = 0, scale = 1)


res <- ks_test_manual_normal(x, mean = 0, sd = 1, K = 5000, make_plot = TRUE)

print(res)

# compare this to built-in function: 

ks.test(x, "pnorm", mean = 0, sd = 1)

#####################
# Problem 2
#####################

# Generating the data, where Bo = o and B1 = 2.75
set.seed (123)
data1 <- data.frame(x = runif(200, 1, 10))
data1$y <- 0 + 2.75*data1$x + rnorm(200, 0, 1.5)

# First test lm() function
testlm<- lm(y ~ x, data = data1)
summary(testlm)

# Now doing this manually: trying maximize the likelihood of my model 
# The optim() function expects a negative log likelihood,
# so we need to define this first:

neg_loglik_normal <- function(theta, y, x) {
  beta0 <- theta[1]
  beta1 <- theta[2]
  sigma <- theta[3]
  
  # sigma is not allowed to be negative, so add safety measure
  if (sigma <= 0) return(Inf) 
  
  mu <- beta0 + beta1 * x
  n <- length(y)
  
  nll <- (n/2) * log(2 * pi) +
    n * log(sigma) +
    sum((y - mu)^2) / (2 * sigma^2)
  
  return(nll)
}

# Then estimate model using BFGS, optimization
# We need to take a higher starting value for sigma, otherwise wrong results
# produced
opt_result <- optim(
  par = c(0, 0, 5),         
  fn = neg_loglik_normal,
  x = data1$x,
  y = data1$y,
  method = "BFGS"
)

# My estimated coefficients are

opt_result$par

# compare with lm() output
summary(testlm)

# Results are almost identical!