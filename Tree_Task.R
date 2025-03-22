#Task for regression
head(trees)
summary(trees)
library(ggplot2)
? trees
options(scipen = 999)



#Scatter Plot
ggplot(trees, aes(x=Girth, y=Volume))+geom_point()+
  labs(title = "Scatter Plot", x="Girth in inches", y="Volume of the timber in cubic-ft")

#Fit regression model
model_trees <- lm(Volume ~ Girth, data = trees)
summary(model_trees)

#Normality Test
shapiro.test(model_trees$residuals)
plot(model_trees, which = 2)

#Residual Plots
residuals(model_trees)
plot(model_trees, which = 1)


#Regression Lin Visualization
ggplot(trees, aes(x=Girth, y=Volume))+
  geom_point()+
  geom_smooth(method = "lm", col="blue")+
  labs(title = "Scatter Plot", x="Girth",
       y="Volume")
