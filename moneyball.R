if (!require(ggplot2)) (install.packages(ggplot2))
if (!require(reshape2)) (install.packages(reshape2))
if (!require(ggpubr)) (install.packages(ggpubr))
if (!require(corrplot)) (install.packages(corrplot))
if (!require(Hmisc)) (install.packages(Hmisc))
if (!require(psych)) (install.packages(psych))
if (!require(PerformanceAnalytics)) (install.packages(PerformanceAnalytics))
if (!require(MASS)) (install.packages(MASS))
if (!require(mice)) (install.packages(mice))
if (!require(caret)) (install.packages(caret))
if (!require(rlang)) (install.packages(rlang))


##Part 1
#Load Data
train_url <- 'https://raw.githubusercontent.com/dquarshie89/Data-621/master/moneyball-training-data.csv'
train <- read.csv(train_url, header=TRUE)
colnames(train)
train$INDEX <- NULL

test_url <- 'https://raw.githubusercontent.com/dquarshie89/Data-621/master/moneyball-evaluation-data.csv'
test <- read.csv(test_url, header=T)
test$INDEX <- NULL  

#Data Sample
head(train,5)

#Data Dimensions
dim(train)

#Data Summary
summary(train)

#Variable Scatterplots
ggplot(melt(train), aes(factor(variable), value)) + geom_boxplot() + facet_wrap(~variable, scale="free") + labs(x="", y="")


#Wins Histomgram
mean_wins <- mean(train$TARGET_WINS, na.rm=TRUE)
sd_wins <- sd(train$TARGET_WINS)
ggplot(train, aes(x=TARGET_WINS)) + geom_histogram(binwidth=1, col = 'black', fill = "green", aes(y=..density..))
+ labs(x="Number of Wins", y = "Distribution") + geom_density(alpha=.2) +
  geom_vline(aes(xintercept=mean(TARGET_WINS, na.rm=TRUE)),color="red", linetype="dashed", size=1) + 
  geom_vline(xintercept = (mean_wins + 2 * sd_wins), color='blue', linetype='dashed', size=1) + 
  geom_vline(xintercept = (mean_wins - 2 * sd_wins), color='blue', linetype='dashed', size=1)

describe(train$TARGET_WINS)


##Part 2  

#Missing Values
rev(sort(colSums(sapply(train, is.na))/nrow(train) * 100))

#New DF with Missings Removed
train_new <- train[, !names(train) %in% c('TEAM_BATTING_HBP','TEAM_BASERUN_CS','TEAM_FIELDING_DP')]
summary(train_new)

#Impute NAs with Median
train_imputed <- mice(train_new, m=5, maxit = 5, method = 'pmm')
train_final <- complete(train_imputed)
summary(train_final)

ggplot(melt(train_final), aes(x=value)) + geom_histogram() + facet_wrap(~variable, scale='free') + labs(x='', y='Frequency')

#Replace Error Maxs
train_final$TEAM_PITCHING_H[train_final$TEAM_PITCHING_H > 3*sd(train_final$TEAM_PITCHING_H)] <- median(train_final$TEAM_PITCHING_H)
train_final$TEAM_PITCHING_BB[train_final$TEAM_PITCHING_BB > 3*sd(train_final$TEAM_PITCHING_BB)] <- median(train_final$TEAM_PITCHING_BB)
train_final$TEAM_PITCHING_SO[train_final$TEAM_PITCHING_SO > 3*sd(train_final$TEAM_PITCHING_SO)] <- median(train_final$TEAM_PITCHING_SO)
train_final$TEAM_FIELDING_E[train_final$TEAM_FIELDING_E > 3*sd(train_final$TEAM_FIELDING_E)] <- median(train_final$TEAM_FIELDING_E)

summary(train_final)

##Part 3

#Model with all variables
allvar_model <- lm(TARGET_WINS ~., train_final)
summary(allvar_model)
par(mfrow=c(2,2))
plot(allvar_model)
hist(resid(allvar_model), main="Histogram of Residuals")

#Model with significant variables
sigvar_model <- lm(TARGET_WINS ~ TEAM_BATTING_H  + TEAM_BATTING_3B  + TEAM_BATTING_HR  + TEAM_BATTING_BB + TEAM_BATTING_SO + TEAM_BASERUN_SB + TEAM_PITCHING_SO + TEAM_PITCHING_H + TEAM_PITCHING_SO + TEAM_FIELDING_E , train_final)
summary(sigvar_model)
par(mfrow=c(2,2))
plot(sigvar_model)
hist(resid(sigvar_model), main="Histogram of Residuals")

##Model 3  
model3 <- lm(TARGET_WINS ~ TEAM_BATTING_H  + TEAM_BATTING_3B  + TEAM_BATTING_HR  + TEAM_BATTING_BB + TEAM_BATTING_SO  + TEAM_BASERUN_SB  + TEAM_FIELDING_E , train_final)
summary(model3)  
par(mfrow=c(2,2))
plot(model3)
hist(resid(model3), main="Residuals")

##Part 4  
#Model 3 Stats
sum_mod3 <- summary(model3)
RSS <- c(crossprod(model3$residuals))
MSE <- RSS/length(model3$residuals)
print(paste0("Mean Squared Error: ", MSE))
print(paste0("Root MSE: ", sqrt(MSE)))
print(paste0("Adjusted R-squared: ", sum_mod3$adj.r.squared))
print(paste0("F-statistic: ",sum_mod3$fstatistic[1]))
plot(resid(model3))
abline(h=0, col=2)

#Clean and Impute Test Data
test_new <- test[, !names(test) %in% c('TEAM_BATTING_HBP','TEAM_BASERUN_CS','TEAM_FIELDING_DP')]
test_imputed <- mice(test_new, m=5, maxit = 5, method = 'pmm')
test_final <- complete(test_imputed)
test_final$TEAM_PITCHING_H[test_final$TEAM_PITCHING_H > 3*sd(test_final$TEAM_PITCHING_H)] <- median(test_final$TEAM_PITCHING_H)
test_final$TEAM_PITCHING_BB[test_final$TEAM_PITCHING_BB > 3*sd(test_final$TEAM_PITCHING_BB)] <- median(test_final$TEAM_PITCHING_BB)
test_final$TEAM_PITCHING_SO[test_final$TEAM_PITCHING_SO > 3*sd(test_final$TEAM_PITCHING_SO)] <- median(test_final$TEAM_PITCHING_SO)
test_final$TEAM_FIELDING_E[test_final$TEAM_FIELDING_E > 3*sd(test_final$TEAM_FIELDING_E)] <- median(test_final$TEAM_FIELDING_E)

#Predict Values
final <- predict(model3, newdata = test_final, interval="prediction")
final
summary(final)

