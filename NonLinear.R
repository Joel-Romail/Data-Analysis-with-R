# Load data
data(trees)

# Non-linear model: Volume = β0 * Girth^β1 * Height^β2
nonlinear_model <- nls(
  Volume ~ b0 * Girth^b1 * Height^b2, 
  data = trees,
  start = list(b0 = 0.1, b1 = 2, b2 = 1)  # Initial guesses
)

# View model summary
summary(nonlinear_model)

# Residuals plot
plot(fitted(nonlinear_model), residuals(nonlinear_model), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19)
abline(h=0, col="red", lwd=2)

new_data <- data.frame(Girth = c(10, 15), Height = c(70, 80))
predict(nonlinear_model, newdata = new_data)

library(ggplot2)

# Scatter plot of original data
ggplot(trees, aes(x = Girth, y = Volume)) +
  geom_point(color = "blue") +  # Data points
  stat_smooth(method = "nls", 
              formula = y ~ b0 * x^b1, 
              method.args = list(start = list(b0 = 1, b1 = 2)), 
              se = FALSE, 
              col = "red") +  # Non-linear regression line
  labs(title = "Non-Linear Regression: Volume vs. Girth",
       x = "Girth (inches)", 
       y = "Volume (cubic ft)") +
  theme_minimal()
residuals(nonlinear_model)
shapiro.test(residuals(nonlinear_model))

ggplot(data.frame(residuals = residuals(nonlinear_model)), aes(x = residuals)) +
  geom_histogram(color = "black", fill = "blue", bins = 10) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency") +
  theme_minimal()

ggplot(data.frame(fitted = fitted(nonlinear_model), residuals = residuals(nonlinear_model)), 
       aes(x = fitted, y = residuals)) +
  geom_point(color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
  theme_minimal()

ggplot(data.frame(Girth = trees$Girth, Residuals = residuals(nonlinear_model)), 
       aes(x = Girth, y = Residuals)) +
  geom_point(color = "purple") +
  geom_smooth(method = "loess", col = "black") +
  labs(title = "Residuals vs Girth", x = "Girth", y = "Residuals") +
  theme_minimal()



library(scatterplot3d)

# 3D Scatterplot with non-linear regression
scatterplot3d(
  trees$Girth, trees$Height, trees$Volume,
  main = "3D Scatterplot with Non-Linear Fit",
  xlab = "Girth",
  ylab = "Height",
  zlab = "Volume",
  pch = 16,
  highlight.3d = TRUE
)

new_data <- data.frame(Girth = c(10, 15), Height = c(70, 80))
predict(nonlinear_model, newdata = new_data)

