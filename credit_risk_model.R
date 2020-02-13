library(readr)
library(gmodels)

loan_data <- readRDS('./data/loan_data1.rds')
write_csv(loan_data, 'loan_data1.csv')
head(loan_data)
str(loan_data)
names(loan_data) #see coumn names

#comparing non-default and default loan (0 = non-default, 1 = default)
CrossTable(loan_data$loan_status) 
#create cross table of credit grade vs loan status 
CrossTable(loan_data$grade, loan_data$loan_status, prop.r = TRUE, 
           prop.c = FALSE, prop.t = FALSE, prop.chisq = FALSE) 
#histogram plot of interest rate
hist(loan_data$int_rate, main = 'Histogram of interest rate', xlab = 'Interest rates')

#plots of annual income
plot(loan_data$annual_inc, ylab = 'annual income') #show an outlier at about 3 million dollars

#cleaning outliers 1) use expert suggestion
#2) use rule of thump: outlier if bigger or smaller than > Q3 + 1.5*(IQR)
ind_outliers <- which(loan_data$annual_inc > 3000000)
loan_data_no_outliers <- loan_data[-ind_outliers,]
#cutoff <- quantile(loan_data$annual_inc, 0.75) + 1.5 * IQR(loan_data$annual_inc)
#ind_outliers <- which(loan_data$annual_inc > cutoff)
#loan_data_no_outliers <- loan_data[-ind_outliers,]
hist(loan_data_no_outliers$annual_inc, xlab = 'annual income')
hist(loan_data_no_outliers$annual_inc, breaks = sqrt(nrow(loan_data_no_outliers)), xlab = 'annual income')

#histogram of loan amount
hist1 <- hist(loan_data_no_outliers$loan_amnt)
hist1$breaks
hist1 <- hist(loan_data_no_outliers$loan_amnt, breaks = 100, xlab = 'loan amount', 
              main = 'Histogram of loan amount')

plot(loan_data$age, loan_data$annual_inc, xlab = 'age', ylab = 'annual income')

#remove age outliers
#ind_highage <- which(loan_data_no_outliers$age > 120)
#loan_data_no_outliers <- loan_data_no_outliers[-ind_highage,]

#remove missing data
summary(loan_data_no_outliers)

#remove rows that employment length (emp_length) are NAs (don't use this when there are too many NAs)
#ind_NA <- which(is.na(loan_data_no_outliers$emp_length))
#ind_NA_int <- which(is.na(loan_data_no_outliers$int_rate))
#loan_data_no_outliers_no_NA <- loan_data_no_outliers[-c(ind_NA, ind_NA_int),]

#remove the whole column
#loan_data_no_outliers_no_emp <- loan_data_no_outliers
#loan_data_no_outliers_no_emp$emp_length <- NULL

#replace missing data with median (median imputation)
#loan_data_no_outliers_replace <- loan_data_no_outliers
#loan_data_no_outliers_replace$emp_length[ind_NA] <- median(loan_data_no_outliers$emp_length, na.rm = TRUE)

#keep missing data: coarse classification (age)
loan_data_no_outliers$ir_cat <-  rep(NA, length(loan_data_no_outliers$int_rate))
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate <= 8)] <- '0-8'
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate > 8 & 
                                     loan_data_no_outliers$int_rate <= 11)] <- '8-11'
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate > 11 & 
                                     loan_data_no_outliers$int_rate <= 13.5)] <- '11-13.5'
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate > 13.5)] <- '13.5+'
loan_data_no_outliers$ir_cat[which(is.na(loan_data_no_outliers$int_rate))] <- 'Missing'
loan_data_no_outliers$ir_cat <- as.factor(loan_data_no_outliers$ir_cat)
plot(loan_data_no_outliers$ir_cat)
table(loan_data_no_outliers$ir_cat)

#keep missing data: coarse classification (employment length)
loan_data_no_outliers$emp_cat <-  rep(NA, length(loan_data_no_outliers$emp_length))
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length <= 15)] <- '0-15'
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length > 15 & 
                                     loan_data_no_outliers$emp_length <= 30)] <- '15-30'
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length > 30 & 
                                     loan_data_no_outliers$emp_length <= 45)] <- '30-45'
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length > 45)] <- '45+'
loan_data_no_outliers$emp_cat[which(is.na(loan_data_no_outliers$emp_length))] <- 'Missing'
loan_data_no_outliers$emp_cat <- as.factor(loan_data_no_outliers$emp_cat)
plot(loan_data_no_outliers$emp_cat)
table(loan_data_no_outliers$emp_cat)

loan_data_no_outliers$int_rate <- NULL
loan_data_no_outliers$emp_length <- NULL

#split data to training set and test set
set.seed(49)
ind_train <- sample(1:nrow(loan_data_no_outliers), 2/3 * nrow(loan_data_no_outliers))
training_set <- loan_data_no_outliers[ind_train,]
test_set <- loan_data_no_outliers[-ind_train,]

#logistic regression
 log_model_ir_cat <- glm(loan_status ~ ir_cat, family = 'binomial', data = training_set)
# the interest rates that are between 8% and 11% compared to the reference category 
# with interest rates between 0% and 8%, the odds in favor of default change by a multiple of
# exp(0.6637) = 1.94

#multiple logistic regression
log_model_multi <- glm(loan_status ~ age + ir_cat + grade + loan_amnt + annual_inc, 
                       family = 'binomial', data = training_set) 
summary(log_model_multi)
#P value: mild significant(.), strong significant(***) 

log_model_age_homeowner <- glm(loan_status ~ age + home_ownership, 
                               family='binomial', data=training_set)

#predicting one case
test_case <- as.data.frame(test_set[1,])
predict_test <- predict(log_model_age_homeowner, newdata = test_case, 
                        type='response') #return probability

#predict all test set
predict_test_set <- predict(log_model_age_homeowner, newdata = test_set, type = 'response')
range(predict_test_set)

#logistic regression with all variables
log_model_full <- glm(loan_status ~ ., family='binomial', data = training_set)

predict_full <- predict(log_model_full, newdata = test_set, type = 'response')
range(predict_full)



      