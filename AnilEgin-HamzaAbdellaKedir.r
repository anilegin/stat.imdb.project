setwd("C:/Users/anili/Desktop/stat_project/R")
# Load data
movies=read.csv("movies.csv")

#Clean the data
moviemissing_values <- is.na(movies$score) | is.na(movies$budget) | is.na(movies$gross) | is.na(movies$runtime) | is.na(movies$genre) | is.na(movies$votes) | is.na(movies$year) 
movies_cleaned <- movies[!moviemissing_values, ]
movies_cleaned = movies_cleaned[movies_cleaned$genre != "Family",]
movies_cleaned = movies_cleaned[movies_cleaned$genre != "Fantasy",]
movies_cleaned = movies_cleaned[movies_cleaned$genre != "Mystery",]
movies_cleaned = movies_cleaned[movies_cleaned$genre != "Romance",]
movies_cleaned = movies_cleaned[movies_cleaned$genre != "Sci-Fi",]
movies_cleaned = movies_cleaned[movies_cleaned$genre != "Thriller",]
movies_cleaned = movies_cleaned[movies_cleaned$genre != "Western",]
movies_cleaned <- subset(movies_cleaned, select = -c(released, name, country, star, writer, director,company,rating))

ggplot(movies_cleaned, aes(x = budget, y = gross, color = genre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Scatterplot of genre and  Gross Earnings")


ggplot(movies_cleaned, aes(x = runtime, y = gross, color = genre)) +
  geom_point() +
  ggtitle("Scatterplot of Runtime and  Gross Earnings")


ggplot(movies_cleaned, aes(x = score, y = gross, color = genre)) +
  geom_point() +
  ggtitle("Scatterplot of Score and  Gross Earnings")


ggplot(movies_cleaned, aes(x = year, y = gross, color = genre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Scatterplot of year and Gross Earnings")


ggplot(movies_cleaned, aes(x = votes, y = gross, color = genre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Scatterplot of votes and Gross Earnings")


ggplot(movies_cleaned, aes(x = genre, y = year, fill = gross)) +
  geom_violin() +
  geom_smooth(method = "lm", se = FALSE)+
  ggtitle("Violinplot of genre and year with Gross Earnings")

movies_cleaned$genre = factor(movies_cleaned$genre)
movies_cleaned$genre = as.numeric(movies_cleaned$genre)
movies_cleaned$genre = factor(movies_cleaned$genre)

X <- model.matrix(~ genre, data = movies_cleaned)
#movies_cleaned$genre <- relevel(movies_cleaned$genre, ref = "2")
#for (i in 1:nrow(movies_cleaned)) {
# Set the value to 1 for the corresponding genre
#movies_cleaned[i, paste0("genre", movies_cleaned[i, "genre"])]<-1
#}
#movies_cleaned[is.na(movies_cleaned)]=0
#movies_cleaned <- subset(movies_cleaned, select = -c(genre))

#required libraries
library(tidyverse)
library(stats)
library(caret)
library(ggplot2)
library(leaps)
library(MASS)
library(tseries)
library(mvtnorm)
library(dplyr)


#splitting the data into train and test 
set.seed(123)
movies_cleaned <- movies_cleaned[sample(nrow(movies_cleaned)), ]
train_index <- sample(1:nrow(movies_cleaned), 0.9 * nrow(movies_cleaned))
movies_train <- movies_cleaned[train_index, ]
movies_test <- movies_cleaned[-train_index, ]

# Fit the full model 
full.model <- lm(gross ~., data = movies_train)

summary(full.model)

library(olsrr)
k = ols_step_all_possible(full.model)

ols_step_forward_p(full.model, penter=0.05)
ols_step_backward_p(full.model, prem=0.1)
#ols_step_both_p(full.model, penter=0.05,prem=0.1)

# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
(step.model)

models <- regsubsets(gross~., data = movies_train, nvmax = 5, method = "seqrep")
summary(models)

# Set seed for reproducibility
set.seed(123)

# Set up repeated k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

# Train the model
#step.model <- train(gross ~., data = movies_train, method = "leapBackward",tuneGrid = data.frame(nvmax = 1:5),trControl = train.control)
step.model <- train(gross ~., data = movies_train, method = "leapForward",tuneGrid = data.frame(nvmax = 1:12),trControl = train.control)

step.model$results

step.model$bestTune

summary(step.model$finalModel)
coef(step.model$finalModel, 5)
best_model=lm(gross ~ genre+votes+budget, data = movies_train)
predictions <- predict(best_model, newdata = movies_test)
plot(movies_test$gross, predictions, xlab = "Actual values", ylab = "Predicted values")
abline(0, 1)


# Fit a linear model
model3 <- aov(gross ~ votes + genre + budget, data = movies_cleaned)
anova(model3)
summary(model3)

# Extract the residual sum of squares (RSS) and total sum of squares (TSS)
rss <- sum(model3$residuals^2)
tss <- sum((movies_cleaned$gross - mean(movies_cleaned$gross))^2)

# Calculate the R-squared value
r_squared <- 1 - rss / tss
print(r_squared)

cor(movies_cleaned$votes,movies_cleaned$budget)

# Extract the residuals from the model
residuals <- best_model$residuals

# Perform a normality test using the Shapiro-Wilk test
shapiro_result= shapiro.test(residuals)

# Perform a normality test using the Kolmogorov-Smirnov test
ks_result=ks.test(residuals, "pnorm")

normality_summary <- data.frame(
  Test = c("Shapiro-Wilk", "Kolmogorov-Smirnov"),
  Statistic = c(shapiro_result$statistic, ks_result$statistic),
  p.value = c(shapiro_result$p.value, ks_result$p.value)
)
normality_summary

# Get the fitted values of the model
fitted <- best_model$fitted.values
ggplot(movies_train, aes(x = fitted, y = residuals)) +
  geom_point()

plot(residuals)
hist(residuals, breaks=100)
qqnorm(residuals,ylab="Residuals",xlab="Normal Scores") 
qqline(residuals, col="red")

plot(best_model)

