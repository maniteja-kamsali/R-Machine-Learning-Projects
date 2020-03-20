#reading file
df = read.csv('C:/Users/MANI/Documents/R sem 2/studentdata/student-mat.csv',sep=';')
df


summary(df)

str(df)

any(is.na(df))

library(ggplot2)
#install.packages('ggthemes')

library(ggthemes)

# Grab only numeric columns
num.cols <- sapply(df, is.numeric)

# Filter to numeric columns for correlation
cor.data <- cor(df[,num.cols])

cor.data

#install.packages('corrgram',repos = 'http://cran.us.r-project.org')
#install.packages('corrplot',repos = 'http://cran.us.r-project.org')

library(corrplot)
library(corrgram)

corrplot(cor.data,method='color')

corrgram(df,order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

ggplot(df,aes(x=G3)) + geom_histogram(bins=20,alpha=1,fill='blue') + theme_minimal()
#model=lm(log(PINCP,base=10)#we want to predict)~ (AGEp + sex variables available to make prediction),data=dtrain)
#dtest$predlogPINCp = predict(model,newdata=dtest)
#dtrain$predLogPINCP =predict(model,newdata=dtrain)

#building a model
library(caTools)
set.seed(101)

sample <- sample.split(df$G3, SplitRatio = 0.70)

# Training Data
train = subset(df, sample == TRUE)

# Testing Data
test = subset(df, sample == FALSE)

#multiple_linear_regression
model <- lm(G3 ~ .,train)

summary(model)

# Grab residuals
res <- residuals(model)

# Convert to DataFrame for gglpot
res <- as.data.frame(res)

head(res)

# Histogram of residuals
ggplot(res,aes(res)) +  geom_histogram(fill='blue',alpha=0.5)

#predictions
G3.predictions <- predict(model,test)

results <- cbind(G3.predictions,test$G3) 
colnames(results) <- c('pred','real')
results <- as.data.frame(results)
results

#removing negative predictions
to_zero <- function(x){
  if  (x < 0){
    return(0)
  }else{
    return(x)
  }
}

results$pred <- sapply(results$pred,to_zero)
print(results)


#evaluate the model
mse <- mean((results$real-results$pred)^2)
print(mse)

mse^0.5

SSE = sum((results$pred - results$real)^2)
SST = sum( (mean(df$G3) - results$real)^2)

R2 = 1 - SSE/SST
R2
