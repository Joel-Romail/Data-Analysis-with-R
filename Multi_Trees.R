library(ggplot2)
head(trees)
summary(trees)
options(scipen = 999)


model_trees_log <- lm(log(Volume)~log(Girth)+log(Height), data = trees)
summary(model_trees_log)

coefficients(model_trees_log)

#Shapiro Test

shapiro.test(model_trees_log$residuals)
plot(model_trees_log, which = 1)

ggplot(trees, aes(x=log(Girth), y=log(Volume), color=log(Height)))+
  geom_point(size = 3)+
  labs(title = "Multiple Regression: Volume vs Girth & Height",
       x="Log(Girth)", y="Log(Volume)",
       color = "Log(Height)")+
  theme_minimal()

# Interpretation: 
# A 1% increase in Girth leads to ~1% increase in volume and Height.
# P-value is significant. and R squered is close to 1. 
# Therefore, 
# This model can be used for predictions.
       
       