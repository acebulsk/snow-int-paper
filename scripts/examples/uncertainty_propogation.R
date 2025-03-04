# popogating errors used for interception efficiency analysis to propogate
# errors in throughfall and snowfall to the I/P ratio

# https://courses.washington.edu/phys431/propagation_errors_UCh.pdf from See
# Chapter 3 in Taylor, An Introduction to Error Analysis.

# Quick check 3.9 for random and independent errors
# Use step-by-step propagation
# to find the quantity q = x/(y − z) with its
# uncertainty

prop_err_ratio <- function(ratio, x, y, sigma_x, sigma_y){
  sigma_ratio <- ratio*sqrt((sigma_x/x)^2+(sigma_y/y)^2)
  return(sigma_ratio)
}

prop_err_sum <- function(sigma_x, sigma_y){
  sigma_sum <- sqrt(sigma_x^2 + sigma_y^2)
  return(sigma_sum)
}

# Define the accumulated precipitation values
x <- 200
y <- 50
z <- 40

# Define the uncertainties associated with A and B
sigma_x <- 2
sigma_y <- 2
sigma_z <- 2

d <- y-z
sigma_d <- (sqrt(sigma_y^2 + sigma_z^2)) |> round()
sigma_test <- prop_err_sum(sigma_y, sigma_z)

q <- x/d
sigma_q <- (q*sqrt((sigma_x/x)^2+(sigma_d/d)^2)) |> round()

sigma_test <- prop_err_ratio(q, x, d, sigma_x, sigma_d)

cat("Uncertainty in the ratio is then:", q, "+/-", sigma_q, "\n")
