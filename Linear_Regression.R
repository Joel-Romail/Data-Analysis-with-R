#Linear Regression

# Equation -> y = b0 + b1*x + e
# b0 = Intercept, b1 = Slope, e = error

#Linear Regression Coefficients

#### Steps to Performs Linear Regression 
#1. Import data.
#2. Visualize the data.
#3. Fit the regression model.
#4. Evaluate model diagnostics.
#5. Interpret the results.


library(ggplot2)
head(wheat)

# x = independantVariable, y = DependentVariable
#ggplot(data, aes(x = yield, y = protien,))+
#  geom_point()+
#  labs(title = "Scatter Plot", x = "IndependantVariable", y = "DependantVariable")



ggplot(wheat, aes(x = yield, y = protien,))+
  geom_point()+
  labs(title = "Scatter Plot", x = "Wheat Yield t ha-1", y = "Protien Contentgr kg-1")

#Fit the regression model 
#model <- lm(DependantVariable~IndependantVariable, data = data)
#Summary of the Model
#summary(model)

wheat_model <- lm(protien~yield, data = wheat)
summary(wheat_model)

# Regression Equation: y = 223.68 - 14.1 + e
# If wheat yield increases by 1 t ha, the protein content
# in seeds decreases by -14 gr per kg.

#Conclusions:
#  p-value = 0.00197<alpha=0.05 - - reject null hypothesis with alpha=0.05 and accept alternative hypothesis (slope is signficant);
#p-value = 1.26*10-5< alpha=0.05 - reject null hypothesis with alpha=0.05 and accept alternative hypothesis (intercept is signficant);

# Multiple R-Squered = 0.767
# Conclusion: 77% of the variation in seed protein content is 
# explained by variation in wheat yield.


# Regression Assumption
# - Normality of Residuals 
# ResidualPlots - residuals(model)
# plot(model, which = 2)
# Normality Test
# shapiro.test(model$residuals)

residuals(wheat_model)
plot(wheat_model, which = 2)

shapiro.test(wheat_model$residuals)
# p-value=0.496>alpha=0.05, the null hypothesis is not rejected.
# Residuals normally distributed.
residuals(wheat_model)
plot(wheat_model$residuals, which = 1)

#• Key Outputs from calculations:
#  - β₀ (Intercept): is significant
#- β₁ (Slope): is significant
#- R²: 77% of the variation in seed protein content is explained by the variation in wheat yield.
#- p-value =0.002: relationship is significant
#- Shapiro test: residuals are normaly distributed
#- Homoscedasticity: errors with constant variance 
#• Can use model for prediction if include mode data.

#Regression line visualization

#ggplot(data, aes(x = IndependentVariable, y = DependentVariable)) +
#  geom_point() +
#  geom_smooth(method = "lm", col = "blue") +
#  labs(title = "Regression Line", x = "Independent Variable", y = "Dependent Variable")

ggplot(wheat, aes(x = yield, y = protien))+
  geom_point()+
  geom_smooth(method = "lm", col = "blue")+
  labs(title = "Scatter Plot", x = "wheat yield", y = "Protein")

