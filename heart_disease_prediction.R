heart_failure = read.csv(file.choose(), header = T)
str(heart_failure)
View(heart_failure)

#################################### Data Understanding #######################################
#1. Data Shape
dimensions = dim(heart_failure)
dimensions
#Data consisting of 918 rows and 12 columns.there are 918 observations (rows) and 11 common features 
#in addition to 1 output class (columns).

#2. Information of data
str(heart_failure)

#3. Statistical Information
summary(heart_failure)
#In relation to the response variable (Heart Disease), the data are less divergent from the arithmetic mean because 
#the higher the standard deviation value, the more the data deviate from the mean, and the smaller the standard deviation, 
#the less the data deviate from the mean.

########################################## Data Preprocessing - Data Cleaning ##########################################

#1. Missing values
#In data cleaning, missing (null) values, duplication of observations and outliers will be investigated.
## Finding and cleaning null values
colSums(is.na(heart_failure))
#There are no null values in the dataset.

#2. Duplicated Rows
heart = unique(heart_failure)
heart
dim(heart)
#There is no duplicate data in the dataset.

#3. Outliers
#Handling of outliers will only be performed on continuous data only, i.e. Age, RestingBP, Cholesterol, MaxHR and Oldpeak. 
#Box plot is employed to identify the potential outliers visually.
variables <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")
boxplot_data <- heart[, variables]

install.packages("RColorBrewer")
library("RColorBrewer")
# Define the colors using a color palette from RColorBrewer
box_colors <- brewer.pal(length(variables), "Set2")

# Create the boxplot with customized colors
boxplot(boxplot_data, col = box_colors, main = "Boxplot of Continouos Variables")
#From boxplots, it can be seen that the RestingBP and Cholesterol are having a number of observations that are exceed that 
#maximum range, while for RestingBP, Cholesterol and MaxHR are observed to have outliers that stay at below minimum range.

#After inspected the plots, outliers are decided to be removed via interquartile range (IQR) methods, where any values that 
#fall outside of 1.5 times the IQR will be considered as outliers.

# IQR methods to remove outliers
# Copy df for modification
heart_clean <- heart

# Iterate over continuous data columns only
for (col in c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")) {
  
  # Calculate Q1, Q3 & IQR
  q1 <- quantile(heart[[col]], 0.25)
  q3 <- quantile(heart[[col]], 0.75)
  iqr <- q3 - q1
  
  # Define the lower bound and upper bound
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  cat(col, ": lower bound is", round(lower_bound, 3), ", upper bound is", round(upper_bound, 3), "\n")
  
  # Remove outliers by filtering based on lower & upper bounds
  heart_clean <- heart_clean[heart_clean[[col]] >= lower_bound & heart_clean[[col]] <= upper_bound, ]
}

# Dimension of dataset after outlier removal
# Set back df to original name
heart <- heart_clean
dim(heart)

#After removing the outliers for all continuous data columns, the dimension of dataset is updated from 918 rows to 702 rows 
#with 12 variables. 216 rows of observations has been recognized as outliers and removed.

#4. converting char and response variables to factors
heart$HeartDisease = as.factor(heart$HeartDisease)
heart$Sex = as.factor(heart$Sex)
heart$ChestPainType = as.factor(heart$ChestPainType)
heart$FastingBS = as.factor(heart$FastingBS)
heart$RestingECG = as.factor(heart$RestingECG)
heart$ExerciseAngina = as.factor(heart$ExerciseAngina)
heart$ST_Slope = as.factor(heart$ST_Slope)

str(heart)
View(heart) 



#5. Feature Selection
#Feature selection is a techniques to select the best set of features (subset of relevant features) for use in construction of 
#optimized models.
install.packages("Boruta")
library("Boruta")
feature_select <- Boruta(HeartDisease ~ ., data = heart)
feature_select$finalDecision

############################################### DATA EXPLORATION #################################################
#1. HISTOGRAM
#All continuous data is analysed with histogram to observe the overall distribution.
#Create a histogram - "Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak"
variables <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")

# Define the colors using a color palette from RColorBrewer
hist_colors <- brewer.pal(length(variables), "Set2")

for (i in seq_along(variables)) {
  col <- variables[i]
  if (is.numeric(heart[[col]])) {
    hist(heart[[col]], main = col, xlab = "", col = hist_colors[i])
  }
}
#The histogram of Age, RestingBP, Cholesterol and MaxHR are observed to be normally distributed, no outlier, 
#skewed data or any abnormality.
#For Oldpeak, data seems skewed to the left. Further inspect the raw (before transformed) data, there are
#around 43% of observation has 0 as the Oldpeak value, suggests that there is no ST segment depression observed 
#in the electrocardiogram (ECG) reading (Oldpeak = ST (Numeric value measured in depression)).

#2. Correlation
#The relationship between 2 continuous variables (normally distributed) are investigated.
pairs(heart[c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")], main = "Relationship between Continous variable",
      col = "blue", pch = 20)
#The scatter plots indicate weak relationships between the variables. The data points are widely scattered, showing no clear trend 
#or correlation between pairs such as Age and MaxHR, or Cholesterol and RestingBP. This suggests that if there are relationships, 
#they may be non-linear or influenced by other factors not included in these variables.

#3. Heatmap
#The relationship between each variables are investigated through the correlation heatmap as below.
install.packages("data.table")
library("data.table")
#create correlation heatmap

corr_matrix <- cor(heart[, sapply(heart, is.numeric)])

# Convert the correlation matrix to a data frame
corr_df <- melt(corr_matrix)

install.packages("ggplot2")  # Install ggplot2 package if not already installed
library(ggplot2)

ggplot(data = corr_df, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = round(value, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Heatmap") +
  theme_minimal() +
  labs(x = "", y = "", title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#The heatmap suggests that Age has a moderate positive correlation with RestingBP and Oldpeak, and a moderate negative correlation 
#with MaxHR. RestingBP and Oldpeak also show a slight positive correlation. There are no strong correlations between Cholesterol and 
#any of the other variables. Overall, the heatmap indicates some expected relationships between age, heart rate, and blood pressure, 
#but cholesterol levels appear to be largely independent of the other variables in this particular dataset.

#4. Bar Charts
#Bar charts for categorical variable (Sex, ChestPainType, ExerciseAngina, RestingECG, ST_Slope, FastingBS) are generated to 
#see the distribution of target variable, heart disease, across each category within the independent variable.

# Sex with heartdisease 
ggplot(heart, aes(x = factor(Sex, labels = c("F", "M")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("N", "Y"))), position = "dodge") +
  labs(x = "Sex", fill = "Heart Disease")

# ChestPainType with heartdisease 
ggplot(heart, aes(x = factor(ChestPainType, labels = c("ASY", "ATA", "NAP", "TA")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("N", "Y"))), position = "dodge") +
  labs(x = "ChestPainType", fill = "Heart Disease")

# ExerciseAngina with heartdisease 
ggplot(heart, aes(x = factor(ExerciseAngina, labels = c("N", "Y")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("N", "Y"))), position = "dodge") +
  labs(x = "ExerciseAngina", fill = "Heart Disease")

# RestingECG with heartdisease 
ggplot(heart, aes(x = factor(RestingECG, labels = c("Normal", "ST", "LVH")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("N", "Y"))), position = "dodge") +
  labs(x = "RestingECG", fill = "Heart Disease")

# ST_Slope with heartdisease 
ggplot(heart, aes(x = factor(ST_Slope, labels = c("Up", "Flat", "Down")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("N", "Y"))), position = "dodge") +
  labs(x = "ST_Slope", fill = "Heart Disease")

# FastingBS with heartdisease 
ggplot(heart, aes(x = factor(FastingBS, labels = c("Otherwise", "FastingBS > 120 mg/dl")))) +
  geom_bar(aes(fill = factor(HeartDisease, labels = c("N", "Y"))), position = "dodge") +
  labs(x = "FastingBS", fill = "Heart Disease")

# Most of the heart disease individuals is male.
# Most of the heart disease individuals are with Asymptomatic Chest Pain Type.
# Most of the non-heart disease individuals without exercise-induced angina.
# Individuals who have ST-T wave abnormality on resting electrocardiogram results have the highest percentage in the train dataset.
# Most of the heart disease individuals with flat ST slope while most of the non-heart disease individuals with up ST slope.
# For individuals with fasting blood sugar > 120mg/dl, most of the individuals with heart disease.

#5. Scatterplots
#Scatterplots for continuous variable (Age, RestingBP, Cholesterol, MaxHR) are generated to see the relationships between them 
#with the consideration of heart disease, the target variable as factor.

# Age & RestingBP with heartdisease
ggplot(heart, aes(x = Age, y = RestingBP, shape = factor(HeartDisease))) +
  geom_point(aes(color = factor(HeartDisease))) +
  labs(x = "Age", y = "RestingBP") +
  scale_shape_manual(values = c(19, 15)) +
  theme_classic()  
#The scatterplot shows that as age and resting blood pressure increases, there is an increase in the proportion of individuals 
#with heart disease compared to those without heart disease. This suggests a positive correlation between resting blood pressure 
#and heart disease.

# Age & Cholesterol with heartdisease 
ggplot(heart, aes(x = Age, y = Cholesterol, shape = factor(HeartDisease))) +
  geom_point(aes(color = factor(HeartDisease))) +
  labs(x = "Age", y = "Cholesterol") +
  scale_shape_manual(values = c(19, 15)) +
  theme_classic() 
#The scatterplot shows that as age and cholesterol level increases, there is a slight increase in the proportion of individuals 
#with heart disease compared to those without heart disease. This suggests a positive correlation between an increase in age, 
#cholesterol with the presence of heart disease.

# Age & MaxHR with heartdisease 
ggplot(heart, aes(x = Age, y = MaxHR, shape = factor(HeartDisease))) +
  geom_point(aes(color = factor(HeartDisease))) +
  labs(x = "Age", y = "MaxHR") +
  scale_shape_manual(values = c(19, 15)) +
  theme_classic()  
#The scatterplot shows that when the maximum heart rate decreases with an increase in age, there is an increase in the proportion 
#of individuals with heart disease. This suggests a negative correlation between maximum heart rate and heart disease.

# RestingBP & Cholesterol with heartdisease 
ggplot(heart, aes(x = RestingBP, y = Cholesterol, shape = factor(HeartDisease))) +
  geom_point(aes(color = factor(HeartDisease))) +
  labs(x = "RestingBP", y = "Cholesterol") +
  scale_shape_manual(values = c(19, 15)) +
  theme_classic()  
#The scatterplot shows that as cholesterol levels and resting blood pressure increases, there is an increase in the proportion of 
#individuals with heart disease. This suggests a positive correlation between cholesterol levels, resting blood pleasure and heart 
#disease.

# RestingBP & MaxHR with heartdisease 
ggplot(heart, aes(x = RestingBP, y = MaxHR, shape = factor(HeartDisease))) +
  geom_point(aes(color = factor(HeartDisease))) +
  labs(x = "RestingBP", y = "MaxHR") +
  scale_shape_manual(values = c(19, 15)) +
  theme_classic()  
#The scatterplot shows the relationship between resting blood pressure and maximum heart rate, with individuals with no heart 
#disease (red dots) and those with heart disease (blue dots) represented separately. The scatterplot suggests that there is no 
#clear pattern or correlation between resting blood pressure and maximum heart rate for both groups.

# Cholesterol & MaxHR with heartdisease 
ggplot(heart, aes(x = Cholesterol, y = MaxHR, shape = factor(HeartDisease))) +
  geom_point(aes(color = factor(HeartDisease))) +
  labs(x = "Cholestrol", y = "MaxHR") +
  scale_shape_manual(values = c(19, 15)) +
  theme_classic()      
#The scatterplot visually represents the correlation between maximum heart rate and cholesterol levels, with two distinct groups: 
#individuals without heart disease (represented by red dots) and those with heart disease (represented by blue dots). The plot 
#clearly demonstrates that as cholesterol levels increase, maximum heart rate decreases, indicating a heightened risk of 
#heart disease.


################################################# DATA MODELING ###################################################
str(heart)
View(heart)

#1. Logistic Regression
#2. SVM
#3. Decision Tree
#4. Random Forest
#5. Gaussian Naive Bayes

################################################## 1. Logistic Regression #########################################

heart1 = heart
str(heart1)

#Creating training - 80% and validation - 20%
idx1 = sort(sample(nrow(heart1), 0.8 * nrow(heart1)))

t_heart1 = heart1[idx1, ]
str(t_heart1)
v_heart1 = heart[-idx1, ]
str(v_heart1)

summary(t_heart1$HeartDisease)

#303*2 = 606 - Using this value for Oversample
#258*2 = 516 - Using this value for Undersample

install.packages("ROSE")
library("ROSE")

#Oversample dataset
over_heart1 = ovun.sample(HeartDisease ~., data = t_heart1, method = "over", N = 606)$data
str(over_heart1)
View(over_heart1)

#Undersample dataset
under_heart1 = ovun.sample(HeartDisease ~., data = t_heart1, method = "under", N = 516)$data
str(under_heart1)
View(under_heart1)

#Combination dataset
comb_heart1 = ovun.sample(HeartDisease ~., data = t_heart1, method = "both", N = nrow(t_heart1))$data
str(comb_heart1)
View(comb_heart1)

#Building original dataset model
LG_heart1 = glm(HeartDisease ~., data = t_heart1, family = "binomial")
summary(LG_heart1) 

#Building Oversample model
LG_heart1_over = glm(HeartDisease ~., data = over_heart1, family = "binomial")
summary(LG_heart1_over) 

#Building Undersample model
LG_heart1_under = glm(HeartDisease ~., data = under_heart1, family = "binomial")
summary(LG_heart1_under) 

#Building Combination model
LG_heart1_comb = glm(HeartDisease ~., data = comb_heart1, family = "binomial")
summary(LG_heart1_comb) 

install.packages("caret")
library('caret')

#Making Original dataset Predictions
p1_h1 = predict(LG_heart1, newdata = v_heart1, type = "response")
View(as.data.frame(p1_h1))

p1_class = ifelse(p1_h1 >= 0.5, 1, 0)
p1_class = as.factor(p1_class)
summary(p1_class)
View(data.frame(p1_h1, p1_class))


DF1_heart1 = data.frame("Actual" = v_heart1$HeartDisease,
                       "Predicted" = p1_class)
View(DF1_heart1)


cf1_heart1 = confusionMatrix(DF1_heart1$Predicted, DF1_heart1$Actual, mode = "everything")
cf1_heart1

#Oversample dataset Predictions
p2_h1 = predict(LG_heart1_over, newdata = v_heart1, type = "response")
View(as.data.frame(p2_h1))

p2_class = ifelse(p2_h1 >= 0.5, 1, 0)
p2_class = as.factor(p2_class)
summary(p2_class)
View(data.frame(p2_h1, p2_class))


DF2_heart1 = data.frame("Actual" = v_heart1$HeartDisease,
                       "Predicted" = p2_class)
View(DF2_heart1)

#confusion matrix - oversample
cf2_heart1 = confusionMatrix(DF2_heart1$Predicted, DF2_heart1$Actual, mode = "everything")
cf2_heart1

#Undersample dataset Predictions
p3_h1 = predict(LG_heart1_under, newdata = v_heart1, type = "response")
View(as.data.frame(p3_h1))

p3_class = ifelse(p3_h1 >= 0.5, 1, 0)
p3_class = as.factor(p3_class)
summary(p3_class)
View(data.frame(p3_h1, p3_class))


DF3_heart1 = data.frame("Actual" = v_heart1$HeartDisease,
                       "Predicted" = p3_class)
View(DF3_heart1)

#confusion matrix - undersample
cf3_heart1 = confusionMatrix(DF3_heart1$Predicted, DF3_heart1$Actual, mode = "everything")
cf3_heart1

#Combination dataset Predictions
p4_h1 = predict(LG_heart1_comb, newdata = v_heart1, type = "response")
View(as.data.frame(p4_h1))

p4_class = ifelse(p4_h1 >= 0.5, 1, 0)
p4_class = as.factor(p4_class)
summary(p4_class)
View(data.frame(p4_h1, p4_class))


DF4_heart1 = data.frame("Actual" = v_heart1$HeartDisease,
                       "Predicted" = p4_class)
View(DF4_heart1)

#confusion matrix - undersample
cf4_heart1 = confusionMatrix(DF4_heart1$Predicted, DF4_heart1$Actual, mode = "everything")
cf4_heart1


########################################################### 2. SVM ###############################################################

heart2 = heart
str(heart2)

ATA = ifelse(heart2$ChestPainType == 'ATA',1,0)
NAP = ifelse(heart2$ChestPainType == 'NAP',1,0)
ASY = ifelse(heart2$ChestPainType == 'ASY',1,0)
TA = ifelse(heart2$ChestPainType == 'TA',1,0)

heart2 <- cbind(heart2,ATA,NAP,ASY,TA)
str(heart2)

heart2[,c(1,4,5,8,10)] = scale(heart2[,c(1,4,5,8,10)], center = TRUE, scale = TRUE)
str(heart2)

heart2 = heart2[,-3]
str(heart2)

#Converting_various_types_of_chest_pain_to_factors

heart2$ATA = as.factor(heart2$ATA)
heart2$NAP = as.factor(heart2$NAP)
heart2$ASY = as.factor(heart2$ASY)
heart2$TA = as.factor(heart2$TA)
str(heart2)

#creating traing and validation
index_svm = sort(sample(nrow(heart2), 0.8*nrow(heart2)))

t_svm = heart2[index_svm, ]
v_svm = heart2[-index_svm, ]
str(t_svm)
str(v_svm)

set.seed(1234)
#check HeartDisease
summary(t_svm$HeartDisease)

#297*2 = 594
#264*2 = 528

#install.packages('ROSE')
library("ROSE")

#Oversample dataset
svm_over = ovun.sample(HeartDisease~., data = t_svm, method = "over", N = 594)$data
str(svm_over)
View(svm_over)

#UnderSample dataset
svm_under = ovun.sample(HeartDisease~., data = t_svm, method = "under", N = 528)$data
str(svm_under)
View(svm_under)

#Combination dataset
svm_comb = ovun.sample(HeartDisease ~., data = t_svm, method = "both", N = nrow(t_svm))$data
str(svm_comb)
View(svm_comb)

install.packages("e1071")
library("e1071")

#original model
svm1 = svm(HeartDisease~., data = t_svm, type = "C-classification", kernal = "linear")
svm1
#Number of Support Vectors:  231

#predictions
pr1_svm = predict(svm1, newdata = v_svm)
View(as.data.frame(pr1_svm))

df1_svm = data.frame("Actual" = v_svm$HeartDisease,
                              "Predicted" = pr1_svm)
View(df1_svm)

install.packages("caret")
library("caret")

cf1_svm = confusionMatrix(df1_svm$Actual, df1_svm$Predicted, mode = "everything")
cf1_svm

#oversample
svm2 = svm(HeartDisease~., data = svm_over, type = "C-classification", kernal = "linear")
svm2

#predictions
pr2_svm= predict(svm2, newdata = v_svm)
View(as.data.frame(pr2_svm))

df2_svm = data.frame("Actual" = v_svm$HeartDisease,
                              "Predicted" = pr2_svm)
View(df2_svm)

cf2_svm = confusionMatrix(df2_svm$Actual, df2_svm$Predicted, mode = "everything")
cf2_svm

#undersample
svm3 = svm(HeartDisease~., data = svm_under, type = "C-classification", kernal = "linear")
svm3

#predictions
pr3_svm = predict(svm3, newdata = v_svm)
View(as.data.frame(pr3_svm))

df3_svm = data.frame("Actual" = v_svm$HeartDisease,
                              "Predicted" = pr3_svm)
View(df3_svm)

cf3_svm = confusionMatrix(df3_svm$Actual, df3_svm$Predicted, mode = "everything")
cf3_svm

#combination
svm4 = svm(HeartDisease~., data = svm_comb, type = "C-classification", kernal = "linear")
svm4

#predictions
pr4_svm = predict(svm4, newdata = v_svm)
View(as.data.frame(pr4_svm))

df4_svm = data.frame("Actual" = v_svm$HeartDisease,
                              "Predicted" = pr4_svm)
View(df4_svm)

cf4_svm = confusionMatrix(df4_svm$Actual, df4_svm$Predicted, mode = "everything")
cf4_svm


cf1_svm
cf2_svm
cf3_svm
cf4_svm


########################################################### 3. Decision Tree ######################################################
install.packages("rpart")
library("rpart")

install.packages("rpart.plot")
library("rpart.plot")

heart3 = heart
str(heart3)

idx3 = sort(sample(nrow(heart3), 0.8 * nrow(heart3)))
train_h3 = heart3[idx3, ]
test_h3 = heart3[-idx3, ]

summary(train_h3$HeartDisease)

#290*2 = 580 - Using this value for Oversample
#271*2 = 542 - Using this value for Undersample

install.packages("ROSE")
library("ROSE")

#Oversample dataset
over_heart3 = ovun.sample(HeartDisease ~., data = train_h3, method = "over", N = 580)$data
str(over_heart3)
View(over_heart3)

#Undersample dataset
under_heart3 = ovun.sample(HeartDisease ~., data = train_h3, method = "under", N = 542)$data
str(under_heart3)
View(under_heart3)

#Combination dataset
comb_heart3 = ovun.sample(HeartDisease ~., data = train_h3, method = "both", N = nrow(train_h3))$data
str(comb_heart3)
View(comb_heart3)


#Original 
dt1_h3 = rpart(HeartDisease ~ ., data = train_h3, method = "class")
summary(dt1_h3)   
rpart.plot(dt1_h3)

p1_h3 = predict(dt1_h3, newdata = test_h3, type = "class")
View(as.data.frame(p1_h3))

df1_h3 = data.frame(Estimate = p1_h3, Actual = test_h3$HeartDisease)
View(df1_h3)

#confusion matrix - original
cf1_h3 = confusionMatrix(df1_h3$Estimate, df1_h3$Actual, mode = "everything")
cf1_h3

#Oversample 
dt2_h3 = rpart(HeartDisease ~ ., data = over_heart3, method = "class")
summary(dt2_h3)   
rpart.plot(dt2_h3)

p2_h3 = predict(dt2_h3, newdata = test_h3, type = "class")
View(as.data.frame(p2_h3))

df2_h3 = data.frame(Estimate = p2_h3, Actual = test_h3$HeartDisease)
View(df2_h3)

#confusion matrix - oversample
cf2_h3 = confusionMatrix(df2_h3$Estimate, df2_h3$Actual, mode = "everything")
cf2_h3

#Undersample 
dt3_h3 = rpart(HeartDisease ~ ., data = under_heart3, method = "class")
summary(dt3_h3)   
rpart.plot(dt3_h3)

p3_h3 = predict(dt3_h3, newdata = test_h3, type = "class")
View(as.data.frame(p3_h3))

df3_h3 = data.frame(Estimate = p3_h3, Actual = test_h3$HeartDisease)
View(df3_h3)

#confusion matrix - undersample
cf3_h3 = confusionMatrix(df3_h3$Estimate, df3_h3$Actual, mode = "everything")
cf3_h3

#Combination 
dt4_h3 = rpart(HeartDisease ~ ., data = comb_heart3, method = "class")
summary(dt4_h3)   
rpart.plot(dt4_h3)


p4_h3 = predict(dt4_h3, newdata = test_h3, type = "class")
View(as.data.frame(p4_h3))

df4_h3 = data.frame(Estimate = p4_h3, Actual = test_h3$HeartDisease)
View(df4_h3)

#confusion matrix - Combination
cf4_h3 = confusionMatrix(df4_h3$Estimate, df4_h3$Actual, mode = "everything")
cf4_h3

#################################################### 4. Random Forest ##########################################################
heart4 = heart
str(heart4)

idx4 = sort(sample(nrow(heart4), 0.8 * nrow(heart4)))
train_h4 = heart4[idx4, ]
test_h4 = heart4[-idx4, ]

summary(train_h4$HeartDisease)

#302*2 = 604 - Using this value for Oversample
#259*2 = 518 - Using this value for Undersample


#Oversample dataset
over_heart4 = ovun.sample(HeartDisease ~., data = train_h4, method = "over", N = 604)$data
str(over_heart4)
View(over_heart4)

#Undersample dataset
under_heart4 = ovun.sample(HeartDisease ~., data = train_h4, method = "under", N = 518)$data
str(under_heart4)
View(under_heart4)

#Combination dataset
comb_heart4 = ovun.sample(HeartDisease ~., data = train_h4, method = "both", N = nrow(train_h4))$data
str(comb_heart4)
View(comb_heart4)

#Original
rf1_h4 = randomForest(HeartDisease ~ ., data = train_h4, ntree = 100)
rf1_h4

p1_h4 = predict(rf1_h4, test_h4, type = "class")
View(as.data.frame(p1_h4))

df1_h4 = data.frame(Estimate = p1_h4, Actual = test_h4$HeartDisease)
View(df1_h4)

#confusion matrix - Original
cf1_h4 = confusionMatrix(df1_h4$Estimate, df1_h4$Actual, mode = "everything")
cf1_h4

#Oversample
rf2_h4 = randomForest(HeartDisease ~ ., data = over_heart4, ntree = 100)
rf2_h4

p2_h4 = predict(rf2_h4, test_h4, type = "class")
View(as.data.frame(p2_h4))

df2_h4 = data.frame(Estimate = p2_h4, Actual = test_h4$HeartDisease)
View(df2_h4)

#confusion matrix - Oversample
cf2_h4 = confusionMatrix(df2_h4$Estimate, df2_h4$Actual, mode = "everything")
cf2_h4

#Undersample
rf3_h4 = randomForest(HeartDisease ~ ., data = under_heart4, ntree = 100)
rf3_h4

p3_h4 = predict(rf3_h4, test_h4, type = "class")
View(as.data.frame(p3_h4))

df3_h4 = data.frame(Estimate = p3_h4, Actual = test_h4$HeartDisease)
View(df3_h4)

#confusion matrix - Undersample
cf3_h4 = confusionMatrix(df3_h4$Estimate, df3_h4$Actual, mode = "everything")
cf3_h4

#Combination
rf4_h4 = randomForest(HeartDisease ~ ., data = comb_heart4, ntree = 100)
rf4_h4

p4_h4 = predict(rf4_h4, test_h4, type = "class")
View(as.data.frame(p4_h4))

df4_h4 = data.frame(Estimate = p4_h4, Actual = test_h4$HeartDisease)
View(df4_h4)

#confusion matrix - Combination
cf4_h4 = confusionMatrix(df4_h4$Estimate, df4_h4$Actual, mode = "everything")
cf4_h4

#################################################### 5. Gaussian Naive Bayes ##########################################################

heart5 = heart
str(heart5)

new_Oldpeak <- log(heart5$Oldpeak)

View(as.data.frame(new_Oldpeak))

Oldpeak = ifelse(new_Oldpeak == "-Inf", 0, new_Oldpeak)
View(as.data.frame(Oldpeak))
hist(Oldpeak)

heart5 = heart5[,-10]
str(heart5)

heart6 = cbind(heart5, Oldpeak)
str(heart6)
View(heart6)

index_nb = sort(sample(nrow(heart6), 0.8 * nrow(heart6)))
train_nb = heart6[index_nb, ]
valid_nb = heart6[-index_nb, ]

summary(train_nb$HeartDisease)

#303*2 = 606 Oversample
#258*2 = 516 Undersample

#Sampling
#Oversample
nb_over = ovun.sample(HeartDisease ~., data = train_nb, method = "over", N = 606)$data
str(nb_over)
View(nb_over)

#undersample
nb_under = ovun.sample(HeartDisease ~., data = train_nb, method = "under", N = 516)$data
str(nb_under)
View(nb_under)

#undersample
nb_comb = ovun.sample(HeartDisease ~., data = train_nb, method = "both", N = nrow(train_nb))$data
str(nb_comb)
View(nb_comb)


#creating model
#original
nb1 = naiveBayes(HeartDisease ~., data = train_nb)
nb1

#predictions
pr1_nb = predict(nb1, newdata = valid_nb)
View(as.data.frame(pr1_nb))

df1_nb = data.frame("Actual" = valid_nb$HeartDisease,
                    "Predicted" = pr1_nb)
View(df1_nb)

cf1_nb = confusionMatrix(df1_nb$Predicted, df1_nb$Actual, mode = "everything")
cf1_nb

#oversample
nb2 = naiveBayes(HeartDisease ~., data = nb_over)
nb2

#predictions
pr2_nb = predict(nb2, newdata = valid_nb)
View(as.data.frame(pr2_nb))

df2_nb = data.frame("Actual" = valid_nb$HeartDisease,
                    "Predicted" = pr2_nb)
View(df2_nb)

cf2_nb = confusionMatrix(df2_nb$Predicted, df2_nb$Actual, mode = "everything")
cf2_nb

#undersample
nb3 = naiveBayes(HeartDisease ~., data = nb_under)
nb3

#predictions
pr3_nb = predict(nb3, newdata = valid_nb)
View(as.data.frame(pr3_nb))

df3_nb = data.frame("Actual" = valid_nb$HeartDisease,
                    "Predicted" = pr3_nb)
View(df3_nb)

cf3_nb = confusionMatrix(df3_nb$Predicted, df3_nb$Actual, mode = "everything")
cf3_nb

#Combination
nb4 = naiveBayes(HeartDisease ~., data = nb_comb)
nb4

#predictions
pr4_nb = predict(nb4, newdata = valid_nb)
View(as.data.frame(pr4_nb))

df4_nb = data.frame("Actual" = valid_nb$HeartDisease,
                    "Predicted" = pr4_nb)
View(df4_nb)

cf4_nb = confusionMatrix(df4_nb$Predicted, df4_nb$Actual, mode = "everything")
cf4_nb


