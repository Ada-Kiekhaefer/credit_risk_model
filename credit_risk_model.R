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

#change loan_status to categorical variable
loan_data$loan_status <- as.factor(loan_data$loan_status)


#cleaning outliers 1) use expert suggestion
#2) use rule of thump: outlier if bigger or smaller than > Q3 + 1.5*(IQR)
ind_outliers <- which(loan_data$annual_inc > 2000000)
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
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate <= 5)] <- '0-5'
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate > 5 & 
                                     loan_data_no_outliers$int_rate <= 10)] <- '5-10'
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate > 10 & 
                                     loan_data_no_outliers$int_rate <= 13.5)] <- '10-13.5'
loan_data_no_outliers$ir_cat[which(loan_data_no_outliers$int_rate > 13.5)] <- '13.5+'
loan_data_no_outliers$ir_cat[which(is.na(loan_data_no_outliers$int_rate))] <- 'Missing'
loan_data_no_outliers$ir_cat <- as.factor(loan_data_no_outliers$ir_cat)
levels(loan_data_no_outliers$ir_cat) <- c('0-5','5-10','10-13.5','13.5+','Missing')
plot(loan_data_no_outliers$ir_cat)
table(loan_data_no_outliers$ir_cat)

#keep missing data: coarse classification (employment length)
loan_data_no_outliers$emp_cat <-  rep(NA, length(loan_data_no_outliers$emp_length))
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length <= 2)] <- '0-2'
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length > 2 & 
                                     loan_data_no_outliers$emp_length <= 10)] <- '2-10'
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length > 10 & 
                                     loan_data_no_outliers$emp_length <= 20)] <- '10-20'
loan_data_no_outliers$emp_cat[which(loan_data_no_outliers$emp_length > 20)] <- '20+'
loan_data_no_outliers$emp_cat[which(is.na(loan_data_no_outliers$emp_length))] <- 'Missing'
loan_data_no_outliers$emp_cat <- as.factor(loan_data_no_outliers$emp_cat)
levels(loan_data_no_outliers$emp_cat) <- c('0-2','2-10','10-20','20+','Missing')
plot(loan_data_no_outliers$emp_cat)
table(loan_data_no_outliers$emp_cat)

loan_data_no_outliers$int_rate <- NULL
loan_data_no_outliers$emp_length <- NULL
save(loan_data_no_outliers, file = './data/loan_data_no_outliers.RData')

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
range(predict_full) #1.387049e-05 4.747289e-01

##Model evaluation: set cut off or treshold value
# Make a binary predictions-vector using a cut-off of 15%
pred_cutoff_15 <- ifelse(predict_full > 0.15, 1, 0)

# Construct a confusion matrix
conf_mat_cutoff_15 <- table(test_set$loan_status, pred_cutoff_15)
accuracy_cutoff_15 <- sum(diag(conf_mat_cutoff_15))/sum(conf_mat_cutoff_15)
sensitivity_cutoff_15 <- conf_mat_cutoff_15[2,2]/sum(conf_mat_cutoff_15[2,])
specificity_cutoff_15 <- conf_mat_cutoff_15[1,1]/sum(conf_mat_cutoff_15[1,])

print(paste('accuracy_cutoff_15 = ', accuracy_cutoff_15))
print(paste('sensitivity_cutoff_15 = ', sensitivity_cutoff_15))
print(paste('specificity_cutoff_15 = ', specificity_cutoff_15))

# Make a binary predictions-vector using a cut-off of 10%
pred_cutoff_20 <- ifelse(predict_full > 0.2, 1, 0)

# Construct a confusion matrix
conf_mat_cutoff_20 <- table(test_set$loan_status, pred_cutoff_20)
accuracy_cutoff_20 <- sum(diag(conf_mat_cutoff_20))/sum(conf_mat_cutoff_20)
sensitivity_cutoff_20 <- conf_mat_cutoff_20[2,2]/sum(conf_mat_cutoff_20[2,])
specificity_cutoff_20 <- conf_mat_cutoff_20[1,1]/sum(conf_mat_cutoff_20[1,])

print(paste('accuracy_cutoff_20 = ', accuracy_cutoff_20))
print(paste('sensitivity_cutoff_20 = ', sensitivity_cutoff_20))
print(paste('specificity_cutoff_20 = ', specificity_cutoff_20))

# Decision tree model -----------------------------------------------------
library(rpart)
# model_credit_tree <- rpart(loan_status ~ ., 
#                                        method = 'class', 
#                                        data = training_set,
#                                        control = rpart.control(cp = 0.001))
# 
# plot(model_credit_tree, uniform = TRUE)
# text(model_credit_tree)
# Error in plot.rpart(model_credit_tree, uniform = TRUE) : fit is not a tree, just a root


#credit risk data are unbalance (little default compare to non-default)
#Three technique to overcome unbalance 
#1) undersampling the over-representated group (non-default) 
#2) Change prior probability
#3) include a loss matrix

#undersampling training data
set.seed(98)
ind_default <- which(training_set$loan_status == 1)
ind_non_default <- which(training_set$loan_status == 0)
ind_non_default_undersample <- sample(ind_non_default, 2 * length(ind_default))
training_set_undersample <- loan_data_no_outliers[c(ind_default,ind_non_default_undersample),]

#decision tree model
model_credit_tree_undersample <- rpart(loan_status ~ ., 
                    method = 'class', 
#                    control = rpart.control(cp = 0.001),
                    control = rpart.control(cp = 0.0005),
                    data = training_set_undersample)

#cp, the complexity parameter, is the threshold value for a decrease in 
#overall lack of fit for any split. If cp is not met, 
#further splits will no longer be pursued. cp's default value is 0.01, 
#but for complex problems, it is advised to relax cp.
plot(model_credit_tree_undersample, uniform = TRUE)
text(model_credit_tree_undersample)

## pruning decision tree
# too big trees are hard to interpret and may lead to overfitting

# minimize xerror (cross validated error of the decision tree)
printcp(model_credit_tree_undersample)
plotcp(model_credit_tree_undersample)

ptree_undersample <- prune(model_credit_tree_undersample, cp = 0.001)
plot(ptree_undersample, uniform = TRUE)
text(ptree_undersample)
# text(ptree_undersample, use.n = TRUE)

library(rpart.plot)
prp(ptree_undersample)


#change prior probability to adjust the importance of misclassifications for each class
# parms = list(prior=c(non_default_proportion, default_proportion))

model_tree_prior <- rpart(loan_status ~ .,
                          method = 'class',
                          parms = list(prior = c(0.7, 0.3)),
                          control = rpart.control(cp = 0.001),
                          data = training_set
                          )

plot(model_tree_prior, uniform = TRUE)
text(model_tree_prior)

printcp(model_tree_prior)
plotcp(model_tree_prior)

ind_prior <- which.min(model_tree_prior$cptable[,'xerror'])
min_cp_prior <- model_tree_prior$cptable[ind_prior,'CP']

ptree_prior <- prune(model_tree_prior, cp = min_cp_prior)
prp(ptree_prior)

# include a loss matrix, heavily pernalize misclassifying a default as a non-default
# parms = list(loss = matrix(c(0, cost_def_as_nondef, cost_nondef_as_def, 0), ncol=2))
# construct a 2x2-matrix with changed loss penalties off-diagonal. 
#The default loss matrix is all ones off-diagonal.
set.seed(123)
model_tree_loss <- rpart(loan_status ~ ., 
                         method = 'class',
                         parms = list(loss = matrix(c(0, 10, 1, 0), ncol = 2)),
                         control = rpart.control(cp = 0.0005),
                         data = training_set
                         )

plot(model_tree_loss, uniform = TRUE)
text(model_tree_loss)

printcp(model_tree_loss)
plotcp(model_tree_loss)

#ind_loss <- which.min(model_tree_loss$cptable[,'xerror'])
#min_cp_loss <- model_tree_loss$cptable[ind_loss, 'CP']
ptree_loss <- prune(model_tree_loss, cp = 0.001)

prp(ptree_loss)

##weighted tree
#create weight vector: contains weights of 1 for the non-defaults 
#and weights of 3 for defaults 
weight_vec <- rep(1,nrow(training_set)) 
ind_default <- which(training_set$loan_status == 1)
weight_vec[ind_default] <- 3

#minsplit : minimum number of splits that are allowed in a node 
#minbucket :  minimum number of observations allowed in leaf nodes 
set.seed(123)
model_tree_weights <- rpart(loan_status ~ .,
                            method = 'class',
                            weights = weight_vec,
                            control = rpart.control(minsplit = 5, 
                                                    minbucket = 2, 
                                                    cp = 0.001),
                            data = training_set)
prp(model_tree_weights)

plotcp(model_tree_weights)
printcp(model_tree_weights)
ind_weight <- which.min(model_tree_weights$cptable[,'xerror'])
min_cp_weight <- model_tree_weights$cptable[ind_weight, 'CP']

ptree_weights <- prune(model_tree_weights, cp = min_cp_weight)
prp(ptree_weights, extra = 1)

## make prediction
pred_undersample <- predict(ptree_undersample, newdata = test_set, type = 'class')
pred_prior <- predict(ptree_prior, newdata = test_set, type = 'class')
pred_loss <- predict(ptree_loss, newdata = test_set, type = 'class')
pred_weights <- predict(ptree_weights, newdata = test_set, type = 'class')

#confusion matrix
confmat_undersample <- table(test_set$loan_status, pred_undersample)
confmat_prior <- table(test_set$loan_status, pred_prior)
confmat_loss <- table(test_set$loan_status, pred_loss)
confmat_weight <- table(test_set$loan_status, pred_weights)

#compute accuracies
acc_undersample <- sum(diag(confmat_undersample))/nrow(test_set)
acc_prior <- sum(diag(confmat_prior))/nrow(test_set)
acc_loss <- sum(diag(confmat_loss))/nrow(test_set)
acc_weight <- sum(diag(confmat_weight))/nrow(test_set)

#compute sensitivity
sensitivity_undersample <- confmat_undersample[2,2]/sum(confmat_undersample[2,])
sensitivity_prior <- confmat_prior[2,2]/sum(confmat_prior[2,])
sensitivity_loss <- confmat_loss[2,2]/sum(confmat_loss[2,])
sensitivity_weight <- confmat_weight[2,2]/sum(confmat_weight[2,])

#compute specificity
specificity_undersample <- confmat_undersample[1,1]/sum(confmat_undersample[1,])
specificity_prior <- confmat_prior[1,1]/sum(confmat_prior[1,])
specificity_loss <- confmat_loss[1,1]/sum(confmat_loss[1,])
specificity_weight <- confmat_weight[1,1]/sum(confmat_weight[1,])


