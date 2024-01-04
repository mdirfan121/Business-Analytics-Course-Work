# clear the console area
cat("\014")

# read the train dataset
train_data <- read.csv("train.csv")

# read the test dataset
test_data <- read.csv("test.csv")

#print the head of the train dataset
print(head(train_data))

#print the head of test dataset
print(head(test_data))

# lets see the summary of  dataset
summary(train_data)
summary(test_data)

# checking for the datatype of all the feature of train dataset
str(train_data)

# checking for the datatype of all the feature of test dataset
str(test_data)

# ----------------- missing values and unnecessary columns------------------

# check missing values column wise for train dataset
colSums( is.na(train_data) | train_data == "")

# check missing values column wise for test dataset
colSums( is.na(test_data) | test_data == "")

#replace -999 with NA (some columns have -999 values in them) 
# some test data also contain ? in them so replace such data with NA 

# replace negative value with NA from train dataset
DataSet <- replace_negative_value(DataSet)

# replace negative value with NA from test dataset
TestSet <- replace_negative_value(TestSet)

# replace ? with NA from test dataset
TestSet <- replace_negative_value(TestSet,"?")

#determine the columns to drop
columns_to_drop <- c("Customer.ID", "Name","Property.ID")

# Subset the data frame to exclude the specified columns for train dataset
train_data <- train_data[, !(names(train_data) %in%columns_to_drop)]

# Subset the data frame to exclude the specified columns for test dataset
test_data <- test_data[, !(names(test_data) %in%columns_to_drop)]

# lets check the dataset after excluding unnecessary field
print(head(train_data))
print(head(test_data))

# determine the numeric column with missing values
train_cols_with_missing_values <- c("Income..USD.","Current.Loan.Expenses..USD.","Credit.Score",
                         "Property.Age","Property.Price","Loan.Sanction.Amount..USD.")

# numeric columns with missing value for test set is different because target variable is not there is test dataset.
test_cols_with_missing_values <- c("Income..USD.","Current.Loan.Expenses..USD.",
                                 "Credit.Score","Property.Age")

# ReplaceWithMean is the user defined function which is defined in the bottom of the file.

# filling the missing values with mean value
train_data<-ReplaceWithMean(train_data,train_cols_with_missing_values)

test_data<-ReplaceWithMean(test_data,test_cols_with_missing_values)

#determine the categorical columns with missing values
Cat_cols_with_missing_values<- c("Income.Stability","Dependents","Has.Active.Credit.Card",
                     "Property.Location","Type.of.Employment","Co.Applicant")

# Note:- Categorical column with missing values are common for both train ad test data.

# replace the missing value of categorical column using Roulette Wheel approach
train_data<-ReplaceWithRoule(train_data,Cat_cols_with_missing_values)
test_data<-ReplaceWithRoule(test_data,Cat_cols_with_missing_values)


#Categorical column names which have outliers  
catg_outliers<-c("Dependents","Profession")
#Replace outliers of Catgorial Columns
train_data<-replace_catg_outliers(train_data,catg_outliers,8)
test_data<-replace_catg_outliers(test_data,catg_outliers,8)

print(table(train_data$Dependents))
print(table(train_data$Profession))

print(table(test_data$Dependents))
print(table(test_data$Profession))


# determine numeric column names which have outliers         
num_outliers<-c("Income..USD.","Loan.Amount.Request..USD.","Current.Loan.Expenses..USD."
                ,"Property.Price","Loan.Sanction.Amount..USD.","Property.Age")


#find outliers and replace them with mean (Use coefficient 4)
train_data<-replace_num_outliers(train_data,num_outliers, 4)
train_data<-replace_num_outliers(train_data,c("Property.Age"), 1)

test_data<-replace_num_outliers(test_data,c("Property.Age"), 1)


# lets see the summary of dataset after cleaning and removing outliers
summary(train_data)
summary(test_data)

# label encoding of categorical data
labelencode_cols <- c("Gender","Income.Stability","Profession","Type.of.Employment","Location"
                      ,"Expense.Type.1","Expense.Type.2","Has.Active.Credit.Card","Property.Location")

#label code the label encode_columns
train_data<-label_encode(train_data,labelencode_cols)
test_data<-label_encode(test_data,labelencode_cols)

# lets see the head of dataset after label encoding of categorical data
print(head(train_data))
print(head(test_data))

#determining the numeric columns to scale
train_columns_to_scale <- c("Age","Income..USD.", "Loan.Amount.Request..USD.","Current.Loan.Expenses..USD.",
                "Credit.Score","Property.Age","Property.Price","Loan.Sanction.Amount..USD.")
test_columns_to_scale <- c("Age","Income..USD.", "Loan.Amount.Request..USD.","Current.Loan.Expenses..USD.",
                        "Credit.Score","Property.Age","Property.Price")

#scale the cols_scale of the Dataset
train_data<-rescale(train_data,train_columns_to_scale)

#Property.Price type is char while it should be numeric so we will convert it before rescaling
test_data["Property.Price"] <- lapply(test_data["Property.Price"], as.numeric)
test_data["Co.Applicant"] <- lapply(test_data["Co.Applicant"], as.numeric)
test_data<-rescale(test_data,test_columns_to_scale)


#----------------Randomize and Split the data into test and train-------------------------

# Randomize  the data set
train_data<-train_data[sample(1:nrow(train_data)),]

# number of rows
print(nrow(train_data))


HOLDOUT <- 70 

# Create a TRAINING dataset using first 70% of the records
# and the remaining 30% is used as TEST
training_records<-round(nrow(train_data)* (HOLDOUT/100))
training_data <- train_data[1:training_records,]
testing_data = train_data[-(1:training_records),]


# ******** Linear Regression with all variables, "." means all variables,
linearModelTransformAllInputs<-lm(Loan.Sanction.Amount..USD.~.,data=training_data)

#This calculates the r squared value - a measure of fit
r2<-Nr2(linearModelTransformAllInputs)
print(paste("### Calculated linear model Training r^2 with all variables added=",r2))
# summary(linearModelTransformAllInputs) #preview test
# plot(linearModelTransformAllInputs)

# Use caret library to determine scaled "importance"
importance<-as.data.frame(caret::varImp(linearModelTransformAllInputs, scale = TRUE)) 

# Sort and print the importances
importance <- importance[order(-importance$Overall), , drop = FALSE]
# print(importance)#preview test

# Plot the % importance ordered from lowest to highest
barplot(t(importance[order(importance$Overall),,drop=FALSE]),las=2, cex.names = 0.8)


# ******** Linear Regression with with columns which have p<0.06 (p of each column can be obtained from summary of linearModelTransformAllInputs  )
columns_to_keep <- c("Loan.Amount.Request..USD.","Credit.Score","Co.Applicant","Income.Stability"
                     ,"Loan.Sanction.Amount..USD.")

# Linear Regression with only columns_to_keep variables, "." means all variable in columns_to_keep
linearModelTransformInputs<-lm(Loan.Sanction.Amount..USD.~.,data=training_data[, columns_to_keep])

#This calculates the r squared value - a measure of fit
r2<-Nr2(linearModelTransformInputs)
print(paste("### linear model Training -> r^2 with only 4 variables added=",r2))
# summary(linearModelTransformInputs) #preview test  

#======>  same results but second model is much simpler so we will use linear regression   
#======>  with the second data_set to  train and predict

#Testing the model on remainder portion of the data (usually 25 percent)
test_linear <- predict(linearModelTransformInputs, newdata = testing_data)

print(c("### Metrics for Testing the model on remainder portion
            of the data (usually 25 percent) ",print_r2_mse(testing_data,test_linear)))

# Additional: Residual Analysis
# Residuals are the differences between actual and predicted values
residuals <- testing_data$Loan.Sanction.Amount..USD. - test_linear

# Plot residuals vs. predicted values
plot(test_linear, residuals, main = "Residuals vs. Predicted Values", 
     xlab = "Predicted Values", ylab = "Residuals")

# Add a horizontal line at y = 0 for reference
abline(h = 0, col = "red", lty = 2)





ReplaceWithMean<-function(dataset,columns){
   
   for(col_name in columns){
     #Find the mean of this column to be replace instead of the null values.
     mean_value<-mean(dataset[[col_name]], na.rm = TRUE)
     dataset[[col_name]][is.na(dataset[[col_name]])] <- mean_value
   }
   return( dataset)
}  
 
ReplaceWithRoule<-function(dataset,columns){
  
  for(col_name in columns){
    
    #find all values except null 
    all_values <- subset(dataset, !(is.na(dataset[[col_name]]) | dataset[[col_name]] == ""))
    
    #Find unique values among non-null values
    unique_values <- unique(all_values[[col_name]])
    
    #Calculate the probabilities for each unique value
    #probabilities <- table(all_values[[col_name]])/length(all_values[[col_name]])
    probabilities <- table(all_values[[col_name]]) / length(all_values[[col_name]])
    
    # Sort unique values and probabilities in the same order
    unique_values<- unique_values[order(unique_values)]
    probabilities <- probabilities[order(unique_values)]
    
    
    # finding indexes of the missing values
    missing_indices <- which(is.na(dataset[[col_name]])| dataset[[col_name]] == "")
    # Replace missing values based on Roulette wheel selection
    dataset[[col_name]][missing_indices] <- sample(unique_values, length(missing_indices), replace = TRUE, prob = probabilities)
    
  }
  return( dataset)
}

replace_negative_value <- function(dataset,value=-999) {
  
  #check in every column
  for (col_name in colnames(dataset)) {
    
    # Check for -999 values in the column
    indices <- dataset[[col_name]] == value
    
    # Replace -999 values with NA
    dataset[[col_name]][indices] <- NA
    
  }
  
  return(dataset)
}

replace_catg_outliers <- function(dataset, columns, threshold) {
  
  for (col_name in columns) {
    #for the categorical columns, replace outliers with the mode
    mode_category <- names(sort(table(dataset[[col_name]]), decreasing = TRUE)[1])
    dataset[[col_name]][dataset[[col_name]] %in% mode_category] <- mode_category
    
    #calculate the z-scores and identify rows with the outliers
    category_frequencies <- table(dataset[[col_name]])
    outliers_categories <- names(category_frequencies[category_frequencies < threshold])
    
    #replace the outliers with mode
    mode_category <- names(sort(table(dataset[[col_name]]), decreasing = TRUE)[1])
    dataset[[col_name]][dataset[[col_name]] %in% outliers_categories] <- mode_category
    # #Testing #Display values of the outliers
    # print(c("outliers of ",col_name,dataset[[col_name]][outliers_categories]))
    # print(c("outliers of ",outliers_categories))
    # print(c("mode of ",mode_category))
  }
  return(dataset)
}

replace_num_outliers <- function(dataset, columns, threshold) {
  
  for (col_name in columns) {
    
    #calculate the z-scores and identify rows with the outliers
    z_scores <- scale(dataset[[col_name]])
    outliers <- which(abs(z_scores) > threshold)
    
    #Testing #Display values of the outliers
    #print(c("outliers of ",col_name,dataset[[col_name]][outliers]))
    
    #replace the outliers with mean
    non_outliers <- dataset[[col_name]][-outliers]
    mean_non_outliers <- mean(non_outliers, na.rm = TRUE)
    dataset[[col_name]][outliers] <- mean_non_outliers
    # print(col_name)
    # print(dataset[[col_name]][outliers])
  }
  return(dataset)
}

label_encode <- function(dataset, columns) {
  
  for(col_name in columns){
    
    #find all values except null 
    all_values <- subset(dataset, !(is.na(dataset[[col_name]]) | dataset[[col_name]] == ""))
    
    #Find unique values among non-null values
    unique_values <- unique(all_values[[col_name]])
    
    
    # Create a mapping of levels to numeric values
    level_mapping <- setNames(seq_along(unique_values), unique_values)
    #print(c("unique_values",unique_values))
    #print(c("level_mapping",level_mapping))
    
    # Apply label encoding to the specified column
    dataset[[col_name]] <- level_mapping[dataset[[col_name]]]
    
    
  }
  return( dataset)
}

rescale <- function(dataset, columns) {
  
  for (col_name in columns) {
    dataset[[col_name]] <-scale(dataset[[col_name]])
  }
  return(dataset)
}

Nr2<-function(linearModel){
  
  rsq<-summary(linearModel)$adj.r.squared
  r2<-round(rsq,digits=4)
  
  return(r2)
}

print_r2_mse<-function(data,predictions){
  # Evaluate the model on the test set
  test_mse <- mean((data$Loan.Sanction.Amount..USD. - predictions)^2)
  test_r2 <- 1 - (sum((data$Loan.Sanction.Amount..USD. - predictions)^2) / sum((data$Loan.Sanction.Amount..USD. - mean(data$Loan.Sanction.Amount..USD.))^2))
  
  # Round R2 and MSE to four digits
  test_r2 <- round(test_r2, digits = 4)
  test_mse <- round(test_mse, digits = 4)
  
  # Print test set metrics
  return(paste("R2:", test_r2,"MSE:", test_mse,"\n" ))
  
}