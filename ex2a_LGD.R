library(fitdistrplus)
library(ggplot2)

# Observed losses given default
losses <- c(0.68, 0.90, 0.22, 0.45, 0.17, 0.25, 0.89, 0.65, 0.75, 0.56, 0.87, 0.92, 0.46)

# Method of Moments estimation
mean_moments <- mean(losses)
variance_moments <- var(losses)
alpha_moments <- mean_moments * ((mean_moments * (1 - mean_moments) / variance_moments) - 1)
beta_moments <- (1 - mean_moments) * ((mean_moments * (1 - mean_moments) / variance_moments) - 1)

# Maximum Likelihood estimation
mle_result <- fitdist(losses, "beta")

# Extract estimated parameters (Maximum Likelihood)
alpha_mle <- mle_result$estimate[1]
beta_mle <- mle_result$estimate[2]

# Generate x-values for plotting the PDF
x <- seq(0, 1, length.out = 1000)

# Calculate the PDF using the estimated parameters (Method of Moments)
pdf_moments <- dbeta(x, alpha_moments, beta_moments)

# Calculate the PDF using the estimated parameters (Maximum Likelihood)
pdf_mle <- dbeta(x, alpha_mle, beta_mle)

# Create a data frame for plotting
df <- data.frame(x = x, PDF_MoM = pdf_moments, PDF_MLE = pdf_mle)

# Plotting the PDFs
ggplot(data = df) +
  geom_line(aes(x = x, y = PDF_MoM, color = "Method of Moments"), size = 1.5) +
  geom_line(aes(x = x, y = PDF_MLE, color = "Maximum Likelihood"), size = 1.5) +
  labs(x = "Losses", y = "Probability Density", title = "Fitting Beta Distribution to Observed Losses") +
  scale_color_manual(values = c("Method of Moments" = "blue", "Maximum Likelihood" = "red")) +
  theme_minimal()

