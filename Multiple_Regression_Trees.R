library(ggplot2)
head(trees)
summary(trees)

ggplot(trees, aes(x = Girth, y = Volume))+
  geom_point()+
  labs(title = "Scatter Plot", x = "Girth in Inches", y = "Volume in cubic fts")

model_trees_log <- lm(log(Volume)~log(Girth) + log(Height), data = trees)
summary(model_trees_log)

shapiro.test(model_trees_log$residuals)
plot(model_trees_log, which = 2)
plot(model_trees_log, which = 2)
plot(model_trees_log, which = 1)

ggplot(trees, aes(x = log(Girth), y = log(Volume)))+
  geom_point()+
  geom_smooth(method = "lm", col = "blue")+
  labs(title = "Log-Log Scatter Plot", x = "Log(Girth)", y = "Log(Volume")


predict(model_trees_log, newdata = data.frame(Girth = 10, Height = 70), interval = "confidence")

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

scatterplot3d(
  x = log(trees$Girth),
  y = log(trees$Height),
  z = log(trees$Volume),
  pch = 16,
  highlight.3d = TRUE,
  main = "3D Scatterplot of Log-Transformed Data",
  xlab = "log(Girth)",
  ylab = "log(Height)",
  zlab = "log(Volume)"
)

