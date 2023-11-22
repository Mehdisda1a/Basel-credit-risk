maturity <- 5
exposure <- 1000
rho <- 0.5
lgd <- c(0, 10, 20, 25, 30, 40, 50, 60, 70, 75, 80, 90, 100)
freq <- c(1, 2, 10, 25, 10, 2, 0, 2, 10, 25, 10, 2, 1)
num_simulations <- 100000


portfolio_losses_empirical <- numeric(num_simulations)
for (i in 1:num_simulations) {
  X <- rnorm(1, mean = 0, sd = 1)
  epsilon <- rnorm(length(lgd), mean = 0, sd = 1)
  u <- pnorm(sqrt(rho)*X + sqrt(1-rho)*epsilon)
  default_times <- qunif(u)
  lgd_cdf<-sample(lgd,10,prob=freq/100)  
  losses_empirical <- rep(0, length(lgd))
  
  for (j in 1:length(lgd)) {
   if (default_times[j] <= maturity/12) {
      losses_empirical[j] <- exposure * lgd_cdf[j] / 100
    }
  }
  portfolio_losses_empirical[i] <- sum(losses_empirical)
  
}
summary(portfolio_losses_empirical)
hist(portfolio_losses_empirical, breaks = 50, main = "Distribution of Portfolio Loss (Empirical) ",
     xlab = "Portfolio Loss")

# VaR values
sorted<- sort(portfolio_losses_empirical)
VaR <- sorted[ceiling(0.01*num_simulations)]


#VaR<- quantile(portfolio_losses_empirical, 0.95)

VaR


