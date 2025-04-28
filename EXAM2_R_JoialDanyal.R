library(ggplot2)
head(computer_test)


#cor(computer_test$v5a, computer_test$v4a, use = "complete.obs")
#cor(computer_test$v5a, computer_test$v4b, use = "complete.obs")
#cor(computer_test$v5a, computer_test$v4c, use = "complete.obs")
#cor(computer_test$v5a, computer_test$v4d, use = "complete.obs")
#cor(computer_test$v5a, computer_test$v4e, use = "complete.obs")

v4_vars <- computer_test[, c("v4a", "v4b", "v4c", "v4d", "v4e")]
v5a <- computer_test$v5a

cor_matrix <- cor(cbind(v5a, v4_vars), use = "complete.obs")
print(cor_matrix)
# v4b - Internet Banking has the highest correlation (0.33) with v5a.
# v4c - Social Networks has the second highest correlation (0.299) with v5a. 

### Therefore the regression equation should be based on v4b and v4c. 

library(car)

model <- lm(v5a~v4c+v4e, data = computer_test)
summary(model)



######
#Final Multivarient Regression Equation = v5a=0.08257+0.09386*v4c+0.03846*v4e

## assumptions of regression 
plot(model$fitted.values, model$residuals, main = "Residuals vs. Fitted")
abline(h=0, col="red")
#Linearity of residuals is ok as they are rendomly scatterd. 

shapiro.test(model$residuals)

# Since P-value is < 0.05 so residuals are not normally distributed.


#### TASK 2

computer_test$v5a_code <- ifelse(computer_test$v5a > 3, 1, 0)

vars_v1_v3 <- computer_test[, c("v1a", "v1b", "v1c", "v3a", "v3b", "v3c")]
# Discrimination analysis LDA
library(caret)
library(MASS)
lda_data <- data.frame(vars_v1_v3, v5a_code=computer_test$v5a_code)
lda_model <- lda(v5a_code~., data = data.frame(vars_v1_v3, v5a_code=computer_test$v5a))
lda_model

# Predicting 
pred <- predict(lda_model)
lda_model$scaling # producing discriminate model coefficients 

####
predictions <- predict(lda_model)

library(caret)

conf_matrix <- confusionMatrix(as.factor(predictions$class), as.factor(lda_data$v5a_code))
print(conf_matrix)
table(Predicted = predictions$class, Actual = lda_data$v5a_code)
###
# Conclusion:
# The discrimination analysis function performed poorly.
# Accuracy is very low ~19.6%. 
# Model only predicted 0 for everyone. 
# A better model may need different variables.
# Chosen variables do no explain the gaming behavior.

### TASK 3
library(factoextra)
cluster_data <- computer_test[100:200, c("v1a", "v1b", "v1c")]

head(cluster_data)

#Scaling Data for Clustering 
cluster_data_scaled <- scale(cluster_data)

summary(cluster_data)
apply(cluster_data, 2, var)

#Scaling Data for Clustering 
#cluster_data_scaled <- scale(cluster_data)

fviz_nbclust(cluster_data_scaled, kmeans, method = "wss")

set.seed(123)
kmeans_result <- kmeans(cluster_data_scaled, centers = 3, nstart = 25)

kmeans_result$centers

table(kmeans_result$cluster)


fviz_cluster(kmeans_result, data = cluster_data_scaled)

