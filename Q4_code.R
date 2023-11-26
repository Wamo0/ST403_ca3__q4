# Q3 code

df <- with(mtcars, data.frame(y = mpg, x1 = disp, x2 = hp, x3 = wt))



## 3.2

nll_lm <- function(data, par) {
  
  y <- data$y
  X <- cbind(1, as.matrix(data[, -1]))  # Add a column of ones for the intercept
  
  # Calculate residuals
  eps <- y - X %*% par
  
  # negative log-likelihood
  nll <- -sum(dnorm(eps, mean = 0, sd = sqrt(var(eps)), log = TRUE))
  
  return(nll)
}

nll_lm(df, c(0,0,0,0))




## 3.3

lower_bounds <- c(-1000, -100, -100, -100) 
upper_bounds <- c(1000, 100, 100, 100)  

initial_guess <- c(mean(df$y),0,0,0)

# Use optim to find MLE
result <- optim(
  par = initial_guess,
  fn = nll_lm,
  data = df,
  lower = lower_bounds,
  upper = upper_bounds,
  method = "L-BFGS-B"  
)


beta_hat <- result$par
sigma_hat <- sqrt(var(df$y - cbind(1, as.matrix(df[, -1])) %*% beta_hat))

cat("Estimated Coefficients (beta):", beta_hat, "\n")
cat("Estimated Residual Standard Deviation (sigma):", sigma_hat, "\n")



## 3.4
The optim() function minimizes other functions by default. Since we are looking for the max.likelihiood, we minimize negative likelihood.

## 3.5


# Adds a column of ones for intercept
y <- df$y
# Calculate coefficients using matrix 
X <- cbind(1, as.matrix(df[, -1]))  

beta_optim <- result$par

# Uses beta_LS3 function from lecture 5
beta_LS3 <- function(X, y) {
  solve(crossprod(X), crossprod(X, y))
}


beta_ls3 <- beta_LS3(X, y)

# Compare coefficients
comparison <- data.frame(
  Parameter = c("Intercept", "beta_x1", "beta_x2", "beta_x3"),
  Coef_optim = beta_optim,
  Coef_ls3 = beta_ls3
)

print(comparison)


## 3.6

# Using optim result
sigma_optim <- sigma_hat

# Using matrix operations
residuals <- df$y - cbind(1, as.matrix(df[, -1])) %*% beta_hat
sigma_matrix <- sqrt(var(residuals))

# Compare standard deviations
cat("Estimated Residual Standard Deviation (sigma) - optim:", sigma_optim, "\n")
cat("Estimated Residual Standard Deviation (sigma) - matrix operations:", sigma_matrix, "\n")



## 3.8

hessian <- vcov(lm(y ~ x1 + x2 + x3, data = df))

se <- sqrt(diag(hessian))
se

