maturity <- 5
exposure <- 1000
intensity <- c(10, 10, 25, 25, 50, 100, 250, 500, 500, 1000)
lgd <- c(0, 10, 20, 25, 30, 40, 50, 60, 70, 75, 80, 90, 100)
freq <- c(1, 2, 10, 25, 10, 2, 0, 2, 10, 25, 10, 2, 1)
num_simulations <- 10000000
default_times <- rexp(length(intensity), rate = intensity/10000)
losses_empirical <- rep(0, length(intensity))
for (i in 1:length(intensity)) {
  if (default_times[i] <= maturity) {
    losses_empirical[i] <- exposure * lgd[i] / 100
  }
}
alpha <-1.310282 # estimated alpha parameter from prev. calibration
beta <-1.310282 # estimated beta parameter from prev. calibration
lgd_cdf<-sample(lgd,10,prob=freq/100)  
losses_calibrated <- rbeta(length(intensity), alpha, beta) * exposure
portfolio_losses_empirical <- numeric(num_simulations)
portfolio_losses_calibrated <- numeric(num_simulations)
portfolio_losses_granular <- numeric(num_simulations)

for (i in 1:num_simulations) {
  default_times <- rexp(length(intensity), rate = intensity/10000)
  lgd_cdf<-sample(lgd,10,prob=freq/100)  
  losses_empirical <- rep(0, length(intensity))
  losses_calibrated <- rep(0, length(intensity))
  losses_granular <- rep(0, length(intensity))
  
  for (j in 1:length(intensity)) {
    if (default_times[j] <= maturity) {
      losses_empirical[j] <- exposure * lgd_cdf[j] / 100
      losses_calibrated[j] <- rbeta(1, alpha, beta) * exposure
      losses_granular[j] <- exposure * 0.5
    }
  }
  
  portfolio_losses_empirical[i] <- sum(losses_empirical)
  portfolio_losses_calibrated[i] <- sum(losses_calibrated)
  portfolio_losses_granular[i] <- sum(losses_granular)
}
summary(portfolio_losses_empirical)
summary(portfolio_losses_calibrated)
summary(portfolio_losses_granular)

hist(portfolio_losses_empirical, breaks = 50, main = "Distribution of Portfolio Loss (Empirical)",
     xlab = "Portfolio Loss")
hist(portfolio_losses_calibrated, breaks = 50, main = "Distribution of Portfolio Loss (Beta Dstribution)",
     xlab = "Portfolio Loss")
hist(portfolio_losses_granular, breaks = 50, main = "Distribution of Portfolio Loss (E[LGD]=50%)",
     xlab = "Portfolio Loss")
