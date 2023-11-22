library(ggplot2)

# LGD values (in %)
lgd <- c(0, 0.10, 0.20, 0.25, 0.30, 0.40, 0.50, 0.60, 0.70, 0.75, 0.80, 0.90, 1)

# p^ values (empirical frequencies)
p_hat <- c(0.01, 0.02, 0.10, 0.25, 0.10, 0.02, 0, 0.02, 0.10, 0.25, 0.10, 0.02, 0.01)

# Create a data frame
data <- data.frame(LGD = lgd, p_hat = p_hat)

# Calculate the total frequency
total_freq <- sum(data$p_hat)

# Estimate the mean LGD
mean_lgd <- sum(data$LGD * data$p_hat) / total_freq

# Calculate the standard deviation of LGD (σLGD)
var_lgd <- sum(data$LGD^2 * data$p_hat)-(sum(data$LGD * data$p_hat))^2


# Estimate the parameters of the Beta distribution
alpha <- ((1 - mean_lgd) / var_lgd - 1 / mean_lgd) * mean_lgd^2
beta <- alpha * (1 / mean_lgd - 1)

# Generate x-values for the rescaled Beta distribution
x <- seq(0, 1, length.out = 100)

# Calculate the rescaled Beta distribution using the estimated parameters
pdf_rescaled <- dbeta(x, alpha, beta)

# Create a data frame for the rescaled Beta distribution
df_beta <- data.frame(x = x, PDF_Rescaled = pdf_rescaled)

# Plot the histogram and overlay the rescaled Beta distribution using ggplot2
ggplot(data, aes(x = LGD, y = p_hat)) +
  geom_col(fill = "blue") +
  geom_line(data = df_beta, aes(x = x, y = PDF_Rescaled), color = "red", size = 1.5) +
  labs(x = "LGD (%)", y = "Frequency", title = "Histogram of LGD with Rescaled Beta Distribution") +
  theme_minimal()
