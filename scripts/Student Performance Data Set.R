library(dplyr)

stu<-read.csv("~/Desktop/Student Performance Data Set/student-mat.csv",
              sep = ",")

str(stu)
head(stu)
summary(stu)


# Calculate the correlation matrix for numeric variables
cor_matrix <- cor(stu[, sapply(stu, is.numeric)])

# Print the correlation matrix
print(cor_matrix)
#Correlation Analysis: 
#G1 and G2 are highly correlated with the final grade G3, 
#showing the strong impact of semester grades on the final score. 
#failures has a negative correlation with G3, 
#while absences has little to no correlation with G3.

#Regression Analysis: 
#The regression model shows that G1 and G2 are the strongest predictors, 
#with failures also having a significant impact. 
#studytime and absences have a small and insignificant effect on G3. 
#The overall model's R² is 0.8287, indicating the model explains a good portion of the variation in the final grade.


library(corrplot)
# Plot the correlation matrix as a heatmap
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black")

# Linear regression model: Predict G3 using G1, G2, studytime, failures, absences
model <- lm(G3 ~ G1 + G2 + studytime + failures + absences, data = stu)

# Summary of the regression model
summary(model)

#Regression Model: 
#The model predicts students' final grade G3 using variables 
#like G1 (first semester grade), G2 (second semester grade), studytime (study time), failures, and absences.

#Significance: 
#G1, G2, and absences have significant effects on G3, 
#while failures also significantly affects the score. studytime has no significant effect on the final grade.

#Model Fit: 
#The R^2 value is 0.8287, 
#indicating that the model explains about 82.87% of the variation in the final grade. 
#The model is significant overall (p-value < 2.2e-16).



#Distribution of Final Grades (G3)
library(ggplot2)
# Plot the distribution of G3 (Final Grades)
ggplot(stu, aes(x = G3)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Final Grades (G3)", x = "Final Grade (G3)", y = "Frequency")
#Grade Distribution: The distribution of G3 (final grades) shows that most students' grades are concentrated around 10, with the frequency for G3 = 10 being the highest, over 50 times. Next is G3 = 11, with a frequency close to 40. The frequency of grade 0 is also relatively high, close to 40, but this could be due to low grades in the data.
#Distribution Pattern: Overall, the distribution shows lower frequencies at both ends and higher frequencies in the middle, forming a roughly normal distribution shape.


# Plot study time vs final grades (G3)
ggplot(stu, aes(x = studytime, y = G3)) +
  geom_point(aes(color = studytime), size = 2) +
  labs(title = "Study Time vs Final Grades (G3)", x = "Study Time", y = "Final Grade (G3)") +
  theme_minimal()

#Study Time vs Final Grades: The plot shows no clear linear relationship between study time (studytime) and final grade (G3).

#Study Time 1: For students with 1 hour of study time, final grades range from 5 to 18, showing some variation.

#Study Time 2: For students with 2 hours of study time, the final grades are more widely distributed from 4 to 18, showing considerable variability.

#Study Time 3: For students with 3 hours of study time, final grades range from 7 to 18, with a more concentrated distribution.

#Study Time 4: For students with 4 hours of study time, grades mostly range from 6 to 20, with 20 being the highest, appearing only once. There is a noticeable "gap" between this point and the others, and other students with 4 hours of study time have final grades around 17.5.



#Absences vs Final Grades
# Plot absences vs final grades (G3)
ggplot(stu, aes(x = absences, y = G3)) +
  geom_point(aes(color = absences), size = 2) +
  labs(title = "Absences vs Final Grades (G3)", x = "Absences", y = "Final Grade (G3)") +
  theme_minimal()
#Absences vs Final Grades: The plot shows no clear linear relationship between absences (absences) and final grade (G3).

#Data Points Analysis:
#One student has both absences and final grade equal to 0.

#For absences less than 60, the final grade is approximately 8.

#One student has a final grade of 20 with absences around 5.

#When absences are 50, the final grade is approximately 7.

#When absences are 55, the final grade is 12.

#Conclusion: Although there are some extreme data points (such as final grade 0 or 20), the majority of final grades fall between 5 and 19. Extreme values in absences (e.g., around 60) could indicate outliers or errors in the data.



# Boxplot to visualize potential outliers in final grade (G3)
ggplot(stu, aes(y = G3)) + 
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Final Grades (G3)", y = "Final Grade (G3)") +
  theme_minimal()

# Calculate the IQR for G3 (final grade)
Q1 <- quantile(stu$G3, 0.25)  # First quartile
Q3 <- quantile(stu$G3, 0.75)  # Third quartile
IQR <- Q3 - Q1  # Interquartile range

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- stu$G3[stu$G3 < lower_bound | stu$G3 > upper_bound]
outliers  # Display outliers

# Check for missing values in the dataset
sum(is.na(stu)) 
# Remove rows with missing values
stu_clean <- na.omit(stu)

# Check for missing values in the dataset
missing_values <- sum(is.na(stu))  # Count the number of missing values in the entire dataset
missing_values  # Display the result
# Remove rows with missing values
stu_clean <- na.omit(stu)  # This removes rows that contain missing values

# Fill missing values in 'studytime' column with the median
stu$studytime[is.na(stu$studytime)] <- median(stu$studytime, na.rm = TRUE)

# Alternatively, fill missing values in 'G3' column with the mean
stu$G3[is.na(stu$G3)] <- mean(stu$G3, na.rm = TRUE)


# Calculate the correlation matrix for the selected variables
cor_matrix <- cor(stu[, c("G1", "G2", "studytime", "failures", "absences", "G3")], use = "complete.obs")
# Display the correlation matrix
print(cor_matrix)  # Show correlation values



library(ggplot2)
library(tidyr)
# Reshape the correlation matrix for plotting using tidyr
cor_matrix_melted <- as.data.frame(as.table(cor_matrix))

# Plot the heatmap of the correlation matrix using ggplot2
ggplot(cor_matrix_melted, aes(Var1, Var2, fill = Freq)) + 
  geom_tile() + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  theme_minimal() + 
  labs(title = "Correlation Matrix Heatmap", x = "Variables", y = "Variables")

#Regression Analysis)
# Linear regression model to predict G3 using other variables
model <- lm(G3 ~ G1 + G2 + studytime + failures + absences, data = stu)

# Summary of the regression model to view coefficients, significance, and R-squared value
summary(model)

#In the linear regression analysis, we used G1, G2, studytime, failures, and absences as predictors to predict G3 (final grade). The regression results show:

#G2 (previous grades) has the largest and most significant effect on G3 (p < 2e-16).

#failures and absences also have significant effects, with p = 0.043968 and p = 0.002530, respectively, indicating that both factors affect the final grade.

#studytime does not have a significant effect on the final grade (p = 0.129177).

#The model's R-squared value is 0.8287, meaning the model explains about 82.87% of the variance in the data.





library(glmnet)
# Prepare data for glmnet model (X: predictors, Y: response variable)
X <- as.matrix(stu[, c("G1", "G2", "studytime", "failures", "absences")])
Y <- stu$G3

# Fit ridge regression model (alpha = 0 for ridge)
ridge_model <- cv.glmnet(X, Y, alpha = 0)

# Display ridge regression model results
print(ridge_model)
#The ridge regression model's cross-validation results show that the optimal λ value is 0.414, with a mean squared error (MSE) of 3.851. The model uses 5 non-zero coefficients at this λ value. This indicates that ridge regression has handled multicollinearity in the model through regularization and produced a more compact model.


#Model Evaluation and Diagnostics
# Plot residuals to check for patterns
residuals <- resid(model)
plot(residuals, main = "Residuals of the Linear Model", ylab = "Residuals", xlab = "Index")

# QQ plot to check for normality of residuals
qqnorm(residuals)
qqline(residuals, col = "red")
#From the residual plot and QQ plot results, the residuals do not fully meet the normality assumption:
#Residual Plot: At x-axis values near -1 and -3, the residuals deviate significantly from the center (i.e., the red line), indicating possible non-linearity or poor model fit.
#QQ Plot: The tails of the residuals, especially near -3, deviate from the theoretical normal distribution line, which may suggest skewness or outliers in the data, leading to imperfect model fitting.


#Other Regression Models
# Fit Lasso regression model (alpha = 1 for Lasso)
lasso_model <- cv.glmnet(X, Y, alpha = 1)

# Display Lasso regression model results
print(lasso_model)

#Lasso Regression Results: The model, using cross-validation (cv.glmnet), calculated the optimal regularization parameter (lambda). Two key lambda values are:
#Minimum lambda (min): λ = 0.0573, with a mean squared error of 3.707, and 5 non-zero coefficients.
#One standard error lambda (1se): λ = 0.6441, with a mean squared error of 4.192, and 2 non-zero coefficients.
#This indicates that at the minimum lambda, the model selects more features (5 non-zero coefficients), while at the 1-standard-error lambda, the model is simplified to only 2 features.


#Model Comparison
# Compare models' performance
mse_lm <- mean(resid(model)^2)  # MSE of linear model
mse_ridge <- mean(ridge_model$cvm)  # MSE of ridge regression
mse_lasso <- mean(lasso_model$cvm)  # MSE of Lasso regression
print(paste("Linear Model MSE:", mse_lm))
print(paste("Ridge Model MSE:", mse_ridge))
print(paste("Lasso Model MSE:", mse_lasso))
#Linear Model: The Mean Squared Error (MSE) is 3.59, which is the smallest among the three models, indicating the best fit.
#Ridge Model: The MSE is 13.53, which is relatively high, suggesting that Ridge regression performs worse than both the Linear and Lasso models on this dataset.
#Lasso Model: The MSE is 5.39, showing performance between that of the Linear and Ridge models.


#Visualization and Interpretation)
# Plot predicted vs actual values for the linear regression model
predicted_values <- predict(model)
ggplot(data.frame(Actual = Y, Predicted = predicted_values), aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Actual vs Predicted Values", x = "Actual Values", y = "Predicted Values")

#From the Actual vs Predicted values plot, the overall trends are as follows:
#The actual values between 5 and 20 show a more concentrated distribution, indicating that most students' final grades (G3) are in this range.
#For actual values of 0, the points are more concentrated, suggesting that there are many students with a final grade of 0.
#Predicted values: The points below the red line (ideal prediction) are more concentrated, while those above the red line show a wider range of predicted values between 2.5 and 10, indicating that the model has some prediction error for lower actual grades (like 0).



#Check Residuals
# Calculate residuals
residuals <- Y - predicted_values

# Plot residuals to check for patterns
ggplot(data.frame(Actual = Y, Predicted = predicted_values, Residuals = residuals), aes(x = Actual, y = Residuals)) +
  geom_point(color = "blue") +
  labs(title = "Residuals vs Actual Values", x = "Actual Values", y = "Residuals") +
  theme_minimal()

# Check for normality of residuals (QQ plot)
qqnorm(residuals)
qqline(residuals, col = "red")


#Model Adjustment
library(rpart)
tree_model <- rpart(G3 ~ G1 + G2 + studytime + failures + absences, data = stu)
summary(tree_model)

# Predict with Decision Tree
tree_pred <- predict(tree_model, stu)

#Cross-validation
library(caret)
train_control <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation
cv_model <- train(G3 ~ G1 + G2 + studytime + failures + absences, data = stu, method = "lm", trControl = train_control)
summary(cv_model)









# Load necessary libraries
library(caret)
library(glmnet)
library(randomForest)

# 1. Linear Regression
lm_model <- train(G3 ~ G1 + G2 + studytime + failures + absences, data = stu, method = "lm", trControl = trainControl(method = "cv", number = 10))

# 2. Ridge Regression
ridge_model <- train(G3 ~ G1 + G2 + studytime + failures + absences, data = stu, method = "glmnet", 
                     trControl = trainControl(method = "cv", number = 10), tuneGrid = expand.grid(alpha = 0, lambda = seq(0, 1, length = 10)))

# 3. Lasso Regression
lasso_model <- train(G3 ~ G1 + G2 + studytime + failures + absences, data = stu, method = "glmnet", 
                     trControl = trainControl(method = "cv", number = 10), tuneGrid = expand.grid(alpha = 1, lambda = seq(0, 1, length = 10)))

# 4. Random Forest
rf_model <- train(G3 ~ G1 + G2 + studytime + failures + absences, data = stu, method = "rf", trControl = trainControl(method = "cv", number = 10))

# Compare models' performance
results <- resamples(list(LM = lm_model, Ridge = ridge_model, Lasso = lasso_model, RF = rf_model))
summary(results)


# Define the parameter grid
param_grid <- expand.grid(
  mtry = c(2, 3, 4, 5, 6)         # Number of variables to randomly sample at each split
)

# Set up cross-validation method (10-fold cross-validation)
train_control <- trainControl(method = "cv", number = 10)

# Run the GridSearchCV with Random Forest model
rf_optimized <- train(
  G3 ~ G1 + G2 + studytime + failures + absences,
  data = stu,
  method = "rf",
  trControl = train_control,
  tuneGrid = param_grid
)

# Print the best tuning parameters and results
print(rf_optimized$bestTune)    # Best tuning parameters = 4
print(rf_optimized$results)     # Detailed results of the tuning process




saveRDS(rf_optimized$finalModel, "rf_optimized_model.rds")
rf_model <- readRDS("rf_optimized_model.rds")






