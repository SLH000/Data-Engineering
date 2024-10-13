##Linear regression model from another dataset
lm_df <- read.csv('student-mat.csv', sep = ';')
summary(lm_df)
sum(is.na(lm_df))
str(lm_df)

# EDA find the correlation 
library(corrgram)
plot_corr <- corrgram(lm_df)

#Building a machine learning model
library(caTools)
set.seed(101)
sample_N <- sample.split(lm_df$age, SplitRatio = 0.7)
train_N <- subset(lm_df, sample_N == TRUE)
test_N <- subset(lm_df, sample_N == FALSE)

model_N <- lm(G3 ~ ., train_N)
summary(model_N)

#Residual plot with ggplot
res_N  <- residuals(model_N)
res_N <- as.data.frame(res_N)
res_plot <- ggplot(res_N, aes(res_N)) + geom_histogram(fill = 'lightblue', alpha = 0.9)
plot(model_N)

G3.prediction <- predict(model_N, test_N)
results_lm <- cbind(G3.prediction, test_N$G3)
colnames(results_lm) <- c('prediction', 'actual')
results_lm <- as.data.frame(results_lm)

#validate the model
mse_N <- mean((results_lm$actual - results_lm$prediction)^2)
rmse_N <- mse_N^0.5
