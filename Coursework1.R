library(readxl)
studentscores <- read_excel("studentscores.xlsx")
View(studentscores_1)
# checking the column names for each column
dimnames(studentscores)
explanatory <- studentscores$'Sleep Hours'
response <- studentscores$'Performance Index'
# Plot the graph
plot(explanatory, response, main='Sleep vs Performance', xlab='Amount of Sleep', ylab='Performance Index')
# Find the least squares estimates of regression parameters using linear model function
linmod <- lm(response ~ explanatory)
print(linmod)
summary(linmod)
abline(linmod) # Add line of best fit
anova(linmod) # Analysis of variance table
# Plot standard residuals vs x
stdres <- rstandard(linmod)
plot(explanatory, stdres, main="Standard residuals vs x")
# Plot standard residuals vs fitted values
plot(fitted(linmod), stdres, main="Standardised vs Fitted")
# Quantile-Quantile plot
qqnorm(stdres)
qqline(stdres)
# Shapiro-Wilk normality test
shapiro.test(stdres)

