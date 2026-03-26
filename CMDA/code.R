<<<<<<< HEAD
setwd("C:/Users/15408/OneDrive/Desktop/CMDA/")
raw.data <- read.csv("county_complete.csv")
subset <- c(4, 13, 15, 18, 19, 23, 25, 27, 29, 30, 32, 34, 36, 37, 41, 44, 46, 
            49, 51, 53, 54, 55, 61, 63, 65, 69, 93, 94, 95, 136, 137, 138,
            139, 140, 141, 142, 143)
data <- raw.data[, subset]
data <- na.omit(data)

## Standardized
standardize_column <- function(column) {
  mean_col <- mean(column)
  sd_col <- sd(column)
  standardized <- (column - mean_col) / sd_col
  return(standardized)
}

data$median_household_income_2017 <- standardize_column(data[, 26])






library(leaps)
library(ggplot2)
library(GGally)
library(MASS)
library(gmodels)
library(e1071)

## Histogram of Heart Disease
ggplot(data = data, aes(x = heart.disease.death)) +
  geom_histogram() + labs(x = "Heart Disease Fatalities (per 100,000)",
                          y = "Count",
                          title = "Histogram of Heart Fatalities") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

## Boxplot of Heart Disease
ggplot(data = data, aes(y = heart.disease.death)) +
  geom_boxplot(width = 0.1) +
  labs(y = "Heart Disease Fatalities (per 100,000)", 
       title = "Boxplot of Heart Fatalities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  
# Full Model Fit
full.model <- lm(heart.disease.death ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                   some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                   uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
                   (hs_grad_2017 * median_household_income_2017) + 
                   (white_not_hispanic_2017 * median_household_income_2017), data = data)

avPlots(full.model)

## Heatmap
rename.data <- data
rename.data$Y1 <- data$heart.disease.death #38
rename.data$X1 <- data$median_age_2017
rename.data$X2 <- data$white_not_hispanic_2017 #39
rename.data$X3 <- data$hs_grad_2017 # 40 
rename.data$X4 <- data$some_college_2017 # 41
rename.data$X5 <- data$bachelors_2017 #42
rename.data$X6 <- data$median_household_income_2017 #43
rename.data$X7 <- data$poverty_2017 #44
rename.data$X8 <- data$uninsured_2017 #45
rename.data$X9 <- data$unemployment_rate_2017 #46
rename.data$X10 <- data$black_2017 #47
rename.data$X11 <- data$hispanic_2017 #48
rename.data$X12 <- data$asian_2017 #49

data.cor <- cor(rename.data[, c(38:50)])

palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = data.cor, col = palette, symm = TRUE, main = "Heatmap")

## Residual Analysis
ggplot(data = NULL, aes(x = full.model$fitted.values, y = rstudent(full.model))) + 
  geom_point() + theme_bw() + labs(x = "Fitted Values", y = "Studentized Residuals",
                                   title = "Versus Fits Plot") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0)

plot(full.model, which = 2)

ggplot(data = NULL, aes(x = rstudent(weighted.full.model))) + 
  geom_histogram() + labs(x = "Studentized Residual", y = "Count",
                          title = "Histogram of Studentized Residuals") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

## Leverage Analysis
ggplot(data = NULL, aes(x = hatvalues(full.model), y = rstudent(full.model))) + 
  geom_point(aes(color = ifelse(abs(rstudent(full.model)) <= 3 
                                & hatvalues(full.model) <= 0.008, "normal", 
                                ifelse(abs(rstudent(full.model)) > 3 & hatvalues(full.model) < 0.008, 
                                       "outlier", ifelse(abs(rstudent(full.model)) > 3 & 
                                                           hatvalues(full.model) > 0.008, "outlier & leverage", "leverage"))))) + 
  geom_vline(xintercept = 0.008) + geom_hline(yintercept = c(-3, 3)) + 
  labs(x = "Leverage", y = "Studentized Residual", title = "Outlier and Leverage Diagnostics") + 
  theme_bw() + scale_color_manual(
    name = "Observation",
    values = c("normal" = "blue", "outlier" = "red", "outlier & leverage" = "purple", "leverage" = "green")
  ) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")


## Scatterplot Matrix
ggpairs(full.model, columns = c("heart.disease.death", "median_age_2017", "white_not_hispanic_2017", "hs_grad_2017", 
                                "some_college_2017", "bachelors_2017", "median_household_income_2017", "poverty_2017",
                                "uninsured_2017", "unemployment_rate_2017", "hispanic_2017", "black_2017"),
        title = "Scatterplot Matrix", axisLabels = "none",
        upper = list(continuous = "smooth"), lower = list(continuous = "cor")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

## Regressors vs. Response
ggplot(data = NULL, aes(x = full.model$fitted.values, y = rstudent(full.model))) + 
  geom_point() + labs(x = "Fitted Values", y = "Studentized Residual", title = "Versus Fits Plot") + 
  geom_hline(yintercept = 0) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))


## Weights
res <- lm(abs(residuals(full.model)) ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
            some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
            uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
            (hs_grad_2017 * median_household_income_2017) + 
            (white_not_hispanic_2017 * median_household_income_2017), data = data)
wts <- 1 / fitted(res)^2

## Weighted LS Fit
weighted.full.model <- lm(log(heart.disease.death) ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                            some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                            uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
                            (hs_grad_2017 * median_household_income_2017) + 
                            (white_not_hispanic_2017 * median_household_income_2017), data = data, weights = wts)

anova(weighted.full.model, full.model)

## Residual Analysis
ggplot(data = NULL, aes(x = weighted.full.model$fitted.values, y = rstudent(weighted.full.model))) + 
  geom_point() + theme_bw() + labs(x = "Fitted Values", y = "Studentized Residuals",
                                   title = "Versus Fits Plot") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0)

plot(weighted.full.model, which = 2)

ggplot(data = NULL, aes(x = rstudent(weighted.full.model))) + 
  geom_histogram() + labs(x = "Studentized Residual", y = "Count",
                          title = "Histogram of Studentized Residuals") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))







## Reducing Model
null.model <- lm(log(heart.disease.death) ~ 1, weights = wts, data = data)
best.subsets <- regsubsets(log(heart.disease.death) ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                             some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                             uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
                             (hs_grad_2017 * median_household_income_2017) + 
                             (white_not_hispanic_2017 * median_household_income_2017), data = data, weights = wts)
best.subsets.results <- summary(best.subsets)
print( tmpdf <- data.frame(best.subsets.results$outmat,
                           "adjR2" = best.subsets.results$adjr2,
                           "Cp" = best.subsets.results$cp,
                           "BIC" = best.subsets.results$bic ) )


## Fit Reduced Model
reduced.model <- lm(log(heart.disease.death) ~ median_age_2017 + white_not_hispanic_2017 + 
                      some_college_2017 + bachelors_2017 + poverty_2017 + black_2017 + unemployment_rate_2017
                    ,data = data, weights = wts)

anova(reduced.model, full.model)

## Residual Analysis of Reduced Model
ggplot(data = NULL, aes(x = rstudent(reduced.model))) + 
  geom_histogram() + labs(x = "Studentized Residual", y = "Count",
                          title = "Histogram of Studentized Residuals") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = NULL, aes(x = weighted.full.model$fitted.values, y = rstudent(reduced.model))) + 
  geom_point() + theme_bw() + labs(x = "Fitted Values", y = "Studentized Residuals",
                                   title = "Versus Fits Plot") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0)

plot(reduced.model, which = 2)
vif(reduced.model)

## Heatmao
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = data.cor, col = palette, symm = TRUE,  main = "Heatmap")

predict.data <- data.frame(median_age_2017 = 0,
                   white_not_hispanic_2017 = 76,
                   some_college_2017 = 30.5,
                   bachelors_2017 = 21.2,
                   poverty_2017 = 15,
                   black_2017 = 3,
                   unemployment_rate_2017 = 4)

predict(reduced.model, newdata = predict.data)
                   


## Naive Bayes
data$heart.median <- ifelse(data$heart.disease.death 
                          > median(data$heart.disease.death), 1, 0)

data <- data[sample(nrow(data)), ]
data.train <- data[1:2347, ]
data.test <- data[2348:nrow(data), ]

data.train.y <- data.train$heart.median
data.test.y <- data.test$heart.median

data.nb <- naiveBayes(heart.median ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                        some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                        uninsured_2017 + unemployment_rate_2017, data = data.train)
yhat <- predict(data.nb, data.test[, c(5, 13, 16, 17, 18, 26, 27, 30, 37, 38)])
CrossTable(yhat, data.test.y,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c("Predicted", "Actual"))

## Dimension Reduction

pca <- prcomp(data[, c(5, 13, 16, 17, 18, 26, 27, 30, 37)])
summary(pca)

plot(pca$x[, 1], pca$x[, 2], 
     main = "PCA Mapping", 
     xlab = "PC1", 
     ylab = "PC2")


race <- lm(heart.disease.death ~ black_2017 + white_not_hispanic_2017 + asian_2017 + hispanic_2017, data = data)
### MAP

library(usmap)
library(ggplot2)

county <- data.frame(
  fips = raw.data[, 1],
  values = raw.data[, 4]
)

state <- data.frame(
  state = mean.state[, 1],
  values = mean.state[, 2]
)

state$values <- state$mean_value

mean.state <- raw.data %>% 
  group_by(state) %>% 
  summarize(mean_value = mean(heart.disease.death, na.rm = TRUE))

plot_usmap(data = county) + labs(title = "Heart Disease Fatalities (per 100,000) by U.S. County") +
  scale_fill_continuous(low = "green", high = "red", 
                        name = "Heart Disease Fatalities (per 100,000)", 
                        limits = c(100, 800)) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"), legend.position = "bottom")

plot_usmap(data = state, regions = "state") + 
  labs(title = "Heart Disease Fatalities (per 100,000) by U.S. State") +
  scale_fill_continuous(low = "green", high = "red", 
                        name = "Heart Disease Fatalities (per 100,000)", 
                        limits = c(100, 800)) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"), legend.position = "bottom")

                                                                            













=======
setwd("C:/Users/15408/OneDrive/Desktop/CMDA/")
raw.data <- read.csv("county_complete.csv")
subset <- c(4, 13, 15, 18, 19, 23, 25, 27, 29, 30, 32, 34, 36, 37, 41, 44, 46, 
            49, 51, 53, 54, 55, 61, 63, 65, 69, 93, 94, 95, 136, 137, 138,
            139, 140, 141, 142, 143)
data <- raw.data[, subset]
data <- na.omit(data)

## Standardized
standardize_column <- function(column) {
  mean_col <- mean(column)
  sd_col <- sd(column)
  standardized <- (column - mean_col) / sd_col
  return(standardized)
}

data$median_household_income_2017 <- standardize_column(data[, 26])






library(leaps)
library(ggplot2)
library(GGally)
library(MASS)
library(gmodels)
library(e1071)

## Histogram of Heart Disease
ggplot(data = data, aes(x = heart.disease.death)) +
  geom_histogram() + labs(x = "Heart Disease Fatalities (per 100,000)",
                          y = "Count",
                          title = "Histogram of Heart Fatalities") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

## Boxplot of Heart Disease
ggplot(data = data, aes(y = heart.disease.death)) +
  geom_boxplot(width = 0.1) +
  labs(y = "Heart Disease Fatalities (per 100,000)", 
       title = "Boxplot of Heart Fatalities") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  
# Full Model Fit
full.model <- lm(heart.disease.death ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                   some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                   uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
                   (hs_grad_2017 * median_household_income_2017) + 
                   (white_not_hispanic_2017 * median_household_income_2017), data = data)

avPlots(full.model)

## Heatmap
rename.data <- data
rename.data$Y1 <- data$heart.disease.death #38
rename.data$X1 <- data$median_age_2017
rename.data$X2 <- data$white_not_hispanic_2017 #39
rename.data$X3 <- data$hs_grad_2017 # 40 
rename.data$X4 <- data$some_college_2017 # 41
rename.data$X5 <- data$bachelors_2017 #42
rename.data$X6 <- data$median_household_income_2017 #43
rename.data$X7 <- data$poverty_2017 #44
rename.data$X8 <- data$uninsured_2017 #45
rename.data$X9 <- data$unemployment_rate_2017 #46
rename.data$X10 <- data$black_2017 #47
rename.data$X11 <- data$hispanic_2017 #48
rename.data$X12 <- data$asian_2017 #49

data.cor <- cor(rename.data[, c(38:50)])

palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = data.cor, col = palette, symm = TRUE, main = "Heatmap")

## Residual Analysis
ggplot(data = NULL, aes(x = full.model$fitted.values, y = rstudent(full.model))) + 
  geom_point() + theme_bw() + labs(x = "Fitted Values", y = "Studentized Residuals",
                                   title = "Versus Fits Plot") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0)

plot(full.model, which = 2)

ggplot(data = NULL, aes(x = rstudent(weighted.full.model))) + 
  geom_histogram() + labs(x = "Studentized Residual", y = "Count",
                          title = "Histogram of Studentized Residuals") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

## Leverage Analysis
ggplot(data = NULL, aes(x = hatvalues(full.model), y = rstudent(full.model))) + 
  geom_point(aes(color = ifelse(abs(rstudent(full.model)) <= 3 
                                & hatvalues(full.model) <= 0.008, "normal", 
                                ifelse(abs(rstudent(full.model)) > 3 & hatvalues(full.model) < 0.008, 
                                       "outlier", ifelse(abs(rstudent(full.model)) > 3 & 
                                                           hatvalues(full.model) > 0.008, "outlier & leverage", "leverage"))))) + 
  geom_vline(xintercept = 0.008) + geom_hline(yintercept = c(-3, 3)) + 
  labs(x = "Leverage", y = "Studentized Residual", title = "Outlier and Leverage Diagnostics") + 
  theme_bw() + scale_color_manual(
    name = "Observation",
    values = c("normal" = "blue", "outlier" = "red", "outlier & leverage" = "purple", "leverage" = "green")
  ) + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")


## Scatterplot Matrix
ggpairs(full.model, columns = c("heart.disease.death", "median_age_2017", "white_not_hispanic_2017", "hs_grad_2017", 
                                "some_college_2017", "bachelors_2017", "median_household_income_2017", "poverty_2017",
                                "uninsured_2017", "unemployment_rate_2017", "hispanic_2017", "black_2017"),
        title = "Scatterplot Matrix", axisLabels = "none",
        upper = list(continuous = "smooth"), lower = list(continuous = "cor")) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

## Regressors vs. Response
ggplot(data = NULL, aes(x = full.model$fitted.values, y = rstudent(full.model))) + 
  geom_point() + labs(x = "Fitted Values", y = "Studentized Residual", title = "Versus Fits Plot") + 
  geom_hline(yintercept = 0) + theme_bw() + theme(plot.title = element_text(hjust = 0.5))


## Weights
res <- lm(abs(residuals(full.model)) ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
            some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
            uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
            (hs_grad_2017 * median_household_income_2017) + 
            (white_not_hispanic_2017 * median_household_income_2017), data = data)
wts <- 1 / fitted(res)^2

## Weighted LS Fit
weighted.full.model <- lm(log(heart.disease.death) ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                            some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                            uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
                            (hs_grad_2017 * median_household_income_2017) + 
                            (white_not_hispanic_2017 * median_household_income_2017), data = data, weights = wts)

anova(weighted.full.model, full.model)

## Residual Analysis
ggplot(data = NULL, aes(x = weighted.full.model$fitted.values, y = rstudent(weighted.full.model))) + 
  geom_point() + theme_bw() + labs(x = "Fitted Values", y = "Studentized Residuals",
                                   title = "Versus Fits Plot") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0)

plot(weighted.full.model, which = 2)

ggplot(data = NULL, aes(x = rstudent(weighted.full.model))) + 
  geom_histogram() + labs(x = "Studentized Residual", y = "Count",
                          title = "Histogram of Studentized Residuals") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))







## Reducing Model
null.model <- lm(log(heart.disease.death) ~ 1, weights = wts, data = data)
best.subsets <- regsubsets(log(heart.disease.death) ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                             some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                             uninsured_2017 + unemployment_rate_2017 + black_2017 + hispanic_2017 + asian_2017 + 
                             (hs_grad_2017 * median_household_income_2017) + 
                             (white_not_hispanic_2017 * median_household_income_2017), data = data, weights = wts)
best.subsets.results <- summary(best.subsets)
print( tmpdf <- data.frame(best.subsets.results$outmat,
                           "adjR2" = best.subsets.results$adjr2,
                           "Cp" = best.subsets.results$cp,
                           "BIC" = best.subsets.results$bic ) )


## Fit Reduced Model
reduced.model <- lm(log(heart.disease.death) ~ median_age_2017 + white_not_hispanic_2017 + 
                      some_college_2017 + bachelors_2017 + poverty_2017 + black_2017 + unemployment_rate_2017
                    ,data = data, weights = wts)

anova(reduced.model, full.model)

## Residual Analysis of Reduced Model
ggplot(data = NULL, aes(x = rstudent(reduced.model))) + 
  geom_histogram() + labs(x = "Studentized Residual", y = "Count",
                          title = "Histogram of Studentized Residuals") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = NULL, aes(x = weighted.full.model$fitted.values, y = rstudent(reduced.model))) + 
  geom_point() + theme_bw() + labs(x = "Fitted Values", y = "Studentized Residuals",
                                   title = "Versus Fits Plot") + 
  theme(plot.title = element_text(hjust = 0.5)) + geom_hline(yintercept = 0)

plot(reduced.model, which = 2)
vif(reduced.model)

## Heatmao
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = data.cor, col = palette, symm = TRUE,  main = "Heatmap")

predict.data <- data.frame(median_age_2017 = 0,
                   white_not_hispanic_2017 = 76,
                   some_college_2017 = 30.5,
                   bachelors_2017 = 21.2,
                   poverty_2017 = 15,
                   black_2017 = 3,
                   unemployment_rate_2017 = 4)

predict(reduced.model, newdata = predict.data)
                   


## Naive Bayes
data$heart.median <- ifelse(data$heart.disease.death 
                          > median(data$heart.disease.death), 1, 0)

data <- data[sample(nrow(data)), ]
data.train <- data[1:2347, ]
data.test <- data[2348:nrow(data), ]

data.train.y <- data.train$heart.median
data.test.y <- data.test$heart.median

data.nb <- naiveBayes(heart.median ~ median_age_2017 + white_not_hispanic_2017 + hs_grad_2017 + 
                        some_college_2017 + bachelors_2017 + median_household_income_2017 + poverty_2017 +
                        uninsured_2017 + unemployment_rate_2017, data = data.train)
yhat <- predict(data.nb, data.test[, c(5, 13, 16, 17, 18, 26, 27, 30, 37, 38)])
CrossTable(yhat, data.test.y,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c("Predicted", "Actual"))

## Dimension Reduction

pca <- prcomp(data[, c(5, 13, 16, 17, 18, 26, 27, 30, 37)])
summary(pca)

plot(pca$x[, 1], pca$x[, 2], 
     main = "PCA Mapping", 
     xlab = "PC1", 
     ylab = "PC2")


race <- lm(heart.disease.death ~ black_2017 + white_not_hispanic_2017 + asian_2017 + hispanic_2017, data = data)
### MAP

library(usmap)
library(ggplot2)

county <- data.frame(
  fips = raw.data[, 1],
  values = raw.data[, 4]
)

state <- data.frame(
  state = mean.state[, 1],
  values = mean.state[, 2]
)

state$values <- state$mean_value

mean.state <- raw.data %>% 
  group_by(state) %>% 
  summarize(mean_value = mean(heart.disease.death, na.rm = TRUE))

plot_usmap(data = county) + labs(title = "Heart Disease Fatalities (per 100,000) by U.S. County") +
  scale_fill_continuous(low = "green", high = "red", 
                        name = "Heart Disease Fatalities (per 100,000)", 
                        limits = c(100, 800)) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"), legend.position = "bottom")

plot_usmap(data = state, regions = "state") + 
  labs(title = "Heart Disease Fatalities (per 100,000) by U.S. State") +
  scale_fill_continuous(low = "green", high = "red", 
                        name = "Heart Disease Fatalities (per 100,000)", 
                        limits = c(100, 800)) + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"), legend.position = "bottom")

                                                                            













>>>>>>> ad19494d5eb91763b080ea8483ccd79ceebd8b02
