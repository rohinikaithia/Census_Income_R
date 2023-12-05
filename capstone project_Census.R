# Data Science Project
#Final Project on “Census Income” Dataset

# Tasks to be done:

# 1.	Data Preprocessing:
   #a)	Replace all the missing values with NA.
   #b)	Remove all the rows that contain NA values.
   #c)	Remove all whitespaces from the columns.

library(readr)

census<-read.csv("/Users/rohinikaithia/Documents/capstone project/census-income_.csv")
View(census)
str(census)
library(plyr)
library(dplyr)
library(stringr)

sum(census == 0)
colSums(census == 0)

# a) Replacing all missing values with NA
replace(census, census == "?", NA)-> census

sum(is.na(census))
colSums(is.na(census))

# b) Removing NA values 
census<- na.omit(census)
sum(is.na(census))

# c) Removing whitespace 
census %>% mutate_if(is.character,str_trim)-> census 
View(census)

########################################################################

# 2.	Data Manipulation:
  # a)	Extract the “education” column and store it in “census_ed” .
census$education->census_ed
as.data.frame(census_ed)-> census_ed #converting vector into df
View(census_ed)

  # b)	Extract all the columns from “age” to “relationship” and store it in “census_seq”.
census_seq=c(census[0:8])
as.data.frame(census_seq)->census_seq
View(census_seq)

  # c)	Extract the column number “5”, “8”, “11” and store it in “census_col”.
census_col=c(census[5],census[8],census[11])
as.data.frame(census_col)-> census_col
View(census_col)
  
# d)	Extract all the male employees who work in state-gov and store it in “male_gov”.
(census$sex=="Male" & census$workclass=="State-gov")->male_gov
subset(census,male_gov=="TRUE")-> male_gov
as.data.frame(male_gov)->male_gov
View(male_gov)

# e)	Extract all the 39 year olds who either have a bachelor's degree or who are native of United States and 
#store the result in “census_us”.
census_us=(census$age=="39" & (census$education=="Bachelors" | census$native.country=="United-States"))
subset(census,census_us=="TRUE")-> census_us
as.data.frame(census_us)->census_us 
View(census_us)

 # f)	Extract 200 random rows from the “census” data frame and store it in “census_200”.
sample_n(census,200)->census_200
View(census_200)
  
# g)	Get the count of different levels of the “workclass” column.
count(census,workclass)
table(census$workclass)

  # h)	Calculate the mean of “capital.gain” column grouped according to “workclass”.
census %>% group_by(workclass) %>% summarise(mean(capital.gain))
summarise(group_by(census,workclass),mean(capital.gain))

########################################################################

# 3.	Data Visualization:
 # a)	Build a bar-plot for the “relationship” column and fill the bars according to the “race” column.
 # i.	Set x-axis label to ‘Categories of Relationships’
 # ii.	Set y-axis label to ‘Count of Categories’
 # iii.	Fill the bars according to “sex”
 # iv.	Set the position of the bars to “dodge”
 # v.	Set the title of plot to be ‘Distribution of Relationships by Sex”

census=read.csv("/Users/rohinikaithia/Documents/capstone project/census-income_.csv",stringsAsFactors = T)
library(ggplot2)
install.packages("plotly")
library(plotly)
library(reshape2)

# a)	Build a bar-plot for the “relationship” column and fill the bars according to the “race” column.
# i.	Set x-axis label to ‘Categories of Relationships’
# ii.	Set y-axis label to ‘Count of Categories’
ggplot(census, aes(x=relationship, fill=race))+ xlab("Categories of Relationships")+ylab("Count of Categories")+geom_bar()+ggtitle("Distribution of Relationship by Race")

# iii.	Fill the bars according to “sex”
# iv.	Set the position of the bars to “dodge”
# v.	Set the title of plot to be ‘Distribution of Relationships by Sex”
ggplot(census, aes(x=relationship, fill=sex))+ xlab("Categories of Relationships")+ylab("Count of Categories")+geom_bar(position="dodge")+ggtitle("Distribution of Relationship by Sex")

####################################

# b)	Build a Histogram for the “age” column with number of bins equal to 50.
# i)	Fill the bars of the histogram according to yearly income column i.e., “X”
# ii)	Set the title of the plot to "Distribution of Age". 
# iii)Set the legend title to "Yearly income".
# iv) Set the theme of the plot to black and white.
ggplot(census, aes(x=age, fill=X))+geom_histogram(bins=50)+ggtitle("Distribution of Age")+theme(panel.background=(element_rect(fill="black")),plot.background=(element_rect((fill="white"))))+guides(fill=guide_legend(title = "Yearly Income"))

####################################

# c)	Build a scatter-plot between “capital.gain” and “hours.per.week”. Map “capital.gain” on the x- axis and “hours.per.week” on the y-axis.
# i)	Set the transparency of the points to 40% and size as 2.
# ii)	Set the color of the points according to the “X” (yearly income) column. 
# iii)Set the x-axis label to “Capital Gain”, y-axis label to “Hours per Week”, title to “Capital Gain vs Hours per Week by Income”, and legend label to “Yearly Income”.
ggplot(census, aes(x=capital.gain, y=hours.per.week, col=X))+xlab("Capital Gain")+ylab("Hours per Week")+ggtitle("Capital Gain vs Hours per Week by Income")+geom_point(size=2, alpha=0.6)+labs(col = "Yearly Income")

####################################

# d)	Build a box-plot between “education” and “age” column.Map “education” on the x-axis and “age” on the y-axis.
# i)	Fill the box-plots according to the “sex” column.
# ii)	Set the title to "Box-Plot of age by Education and Sex".
ggplot(census, aes(x=education, y=age, fill=sex))+geom_boxplot()+ggtitle("Box-Plot of age by Education and Sex")

########################################################################

# 4.	Linear Regression:
# a)	Build a simple linear regression model as follows:
# i)	Divide the dataset into training and test sets in 70:30 ratio.
# ii)	Build a linear model on the test set where the dependent variable is “hours.per.week” and independent variable is “education.num”.
# iii)	Predict the values on the train set and find the error in prediction. 
# iv) Find the root-mean-square error (RMSE).

census=read.csv("/Users/rohinikaithia/Documents/capstone project/census-income_.csv")
library(ggplot2)
library(plotly)
library(reshape2)
library(caTools)
library(readr)

# Checking if data meets four main assumptions for linear regression.
hist(census$education.num)
hist(census$hours.per.week)
plot(education.num ~ hours.per.week, data = census)

sample.split(census$hours.per.week, SplitRatio = 0.70)-> split_tag_census
subset(census, split_tag_census==T)->train_census
subset(census, split_tag_census==F)->test_census

nrow(census)
nrow(train_census)
nrow(test_census)

lm(hours.per.week~education.num,data=train_census)-> mod_census
summary(mod_census)

predict(mod_census,newdata = test_census)-> result_census
cbind(Actual=test_census$education.num,Predicted=result_census)-> final_data_census
as.data.frame(final_data_census)->final_data_census
final_data_census$Actual-final_data_census$Predicted->error_census
as.data.frame(error_census)->error_census
View(error_census)
cbind(final_data_census,error_census)->final_final_data_census
View(final_final_data_census)
plot(mod_census)
sqrt(mean(final_final_data_census$error_census)^2)-> RSME_census
RSME_census # [1] 30.35287

########################################################################

# 5.	Logistic Regression:
# a)	Build a simple logistic regression model as follows:
# i)	Divide the dataset into training and test sets in 65:35 ratio.
# ii)	Build a logistic regression model where the dependent variable is “X”(yearly income) and independent variable is “occupation”.
# iii) Predict the values on the test set.
# iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off.
# v)	Build a confusion matrix and find the accuracy.
# vi)	Plot the ROC curve and find the auc(Area Under Curve).

census=read.csv("/Users/rohinikaithia/Documents/capstone project/census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
install.packages("rockr")
library(ROCR)
install.packages("pROC")
library(pROC)

sample.split(census$X,SplitRatio = 0.65)->census_simlog
subset(census,census_simlog==T)->train_simlog
subset(census,census_simlog==F)->test_simlog
glm(as.factor(X)~occupation,data=train_simlog,family = "binomial")->mod_simlog
predict(mod_simlog,newdata=test_simlog,type="response")->result_simlog
prediction(result_simlog,test_simlog$X)->pred_simlog
performance(pred_simlog,"acc")->acc_simlog
plot(acc_simlog)
table(test_simlog$X,result_simlog>0.45)->mat_simlog
sum(diag(mat_simlog))/sum(mat_simlog)->acc1_simlog #0.78439
performance(pred_simlog,"tpr","fpr")->roc_simlog
plot(roc_simlog,colorize=T)
auc(test_simlog$X,result_simlog)->auc_simlog
auc_simlog #Area under the curve: 0.7275

####################################

# b) Build a multiple logistic regression model as follows:
# i)	Divide the dataset into training and test sets in 80:20 ratio.
# ii)	Build a logistic regression model where the dependent variable is “X”(yearly income) and independent variables are “age”, “workclass”, and “education”.
# iii)Predict the values on the test set.
# iv)	Plot accuracy vs cut-off and pick an ideal value for cut-off. 
# v) Build a confusion matrix and find the accuracy.
# vi) Plot the ROC curve and calculate the auc(Area Under Curve).

census=read.csv("/Users/rohinikaithia/Documents/capstone project/census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
library(ROCR)
library(pROC)

sample.split(census$X,SplitRatio = 0.8)->census_mullog
subset(census,census_mullog==T)->train_mullog
subset(census,census_mullog==F)->test_mullog
glm(as.factor(X)~age+workclass+education,data=train_mullog,family = "binomial")->mod_mullog
predict(mod_mullog,newdata=test_mullog,type="response")->result_mullog
prediction(result_mullog,test_mullog$X)->pred_mullog
performance(pred_mullog,"acc")->acc_mullog
plot(acc_mullog)
table(test_mullog$X,result_mullog>0.45)->mat_mullog
sum(diag(mat_mullog))/sum(mat_mullog)->acc1_mullog #0.74017
performance(pred_mullog,"tpr","fpr")->roc_mullog
plot(roc_mullog,colorize=T)
auc(test_mullog$X,result_mullog)->auc_mullog
auc_mullog #Area under the curve: 0.7761

########################################################################

# 6.	Decision Tree:
# a)	Build a decision tree model as follows:
# i)	Divide the dataset into training and test sets in 70:30 ratio.
# ii)	Build a decision tree model where the dependent variable is “X”(Yearly Income) and the rest of the variables as independent variables.
# iii)	Plot the decision tree.
# iv)	Predict the values on the test set.
# v)	Build a confusion matrix and calculate the accuracy.

census=read.csv("/Users/rohinikaithia/Documents/capstone project/census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
install.packages("tree")
library(tree)
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
library(ggplot2)
library(corrplot)
install.packages("ggcorrplot")
library(ggcorrplot)

sample.split(census$X,SplitRatio = 0.70)-> split_tag_census_dt
subset(census,split_tag_census_dt==T)->train_census_dt
subset(census,split_tag_census_dt==F)->test_census_dt
dim(train_census_dt)
dim(test_census_dt)

prop.table(table(train_census_dt$X)) 
prop.table(table(test_census_dt$X))

mod_census_dt=rpart(X~.,data=census,method="class")
rpart.plot(mod_census_dt)
predict(mod_census_dt,newdata = test_census_dt,type = "class")->predict_unseen_census_dt

table(test_census_dt$X,predict_unseen_census_dt)->table_mat_census_dt
table_mat_census_dt

# Output
# > table_mat_census_dt
# predict_unseen_census_dt
#        <=50K  >50K
# <=50K   7035   381
# >50K    1152  1200

7035/(7035+381) # [1] 0.9486246
1152/(1152+1200) # [1] 0.4897959

specificity_dt<-(7035)/(7035+1152)
specificity_dt 
# [1] 0.8592891

misclass_rate_dt<-(381+1200)/sum(table_mat_census_dt)
misclass_rate_dt
# [1] 0.161855

sum(diag(table_mat_census_dt))/sum(table_mat_census_dt)->acc_census_dt
acc_census_dt #[1] 0.843059
compare_census_dt=data.frame(Actual_data=test_census_dt$X,Predicted_data=predict_unseen_census_dt)
View(compare_census_dt)
table(compare_census_dt$Actual_data)
table(compare_census_dt$Predicted_data)

mutate(compare_census_dt,Check=ifelse(compare_census_dt$Actual_data==compare_census_dt$Predicted_data,"Matching","Not Matching"))->match_census_dt
View(match_census_dt)
table(match_census_dt$Check)

# Output
# > table(match_census_dt$Check)
# 
# Matching Not Matching 
# 8235         1533 

8235/(8235+1533) # [1] 0.843059

########################################################################

# 7.	Random Forest:
# a)	Build a random forest model as follows:
# i)	Divide the dataset into training and test sets in 80:20 ratio.
# ii)	Build a random forest model where the dependent variable is “X”(Yearly Income) and the rest of the variables as independent variables and number of trees as 300.
# iii)	Predict values on the test set
# iv)	Build a confusion matrix and calculate the accuracy

census=read.csv("/Users/rohinikaithia/Documents/capstone project/census-income_.csv", stringsAsFactors = T)
library(dplyr)
library(reshape2)
library(caTools)
library(readr)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
install.packages("randomForest")
library(randomForest)
install.packages("superml")
library(superml)

sample.split(census,SplitRatio=0.80)->split_tag1_census_rf
subset(census,split_tag1_census_rf==T)->train_census_rf
subset(census,split_tag1_census_rf==F)->test_census_rf
nrow(train_census_rf)
nrow(test_census_rf)
randomForest(X~.,data=train_census_rf,mtry=3,ntree=300)->mod_forest_census_rf

importance(mod_forest_census_rf)
varImpPlot(mod_forest_census_rf)
plot(mod_forest_census_rf)

predict(mod_forest_census_rf,newdata=test_census_rf,type="class")->result_forest_census_rf
head(result_forest_census_rf)

table(Actual=test_census_rf$X,Predicted=result_forest_census_rf)-> t_census_rf
View(t_census_rf)
# Output
# > t_census_rf
#           Predicted
# Actual  <=50K  >50K
# <=50K   4598   291
# >50K     579  1045

plot(t_census_rf)

#checking accuracy via confusion matrix
sum(diag(t_census_rf))/sum(t_census_rf)-> acc_census_rf
acc_census_rf # [1] 0.866421













