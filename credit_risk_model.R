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
ind_NA <- which(is.na(loan_data_no_outliers$emp_length))
loan_data_no_outliers_no_NA <- loan_data_no_outliers[-c(ind_NA),]

#remove the whole column
loan_data_no_outliers_no_emp <- loan_data_no_outliers
loan_data_no_outliers_no_emp$emp_length <- NULL

#replace missing data with median (median imputation)
loan_data_no_outliers_replace <- loan_data_no_outliers
loan_data_no_outliers_replace$emp_length[ind_NA] <- median(loan_data_no_outliers$emp_length, na.rm = TRUE)

#
summary(loan_data_no_outliers$int_rate)

#split data to training set and test set
set.seed(49)
ind_train <- sample(1:nrow())

