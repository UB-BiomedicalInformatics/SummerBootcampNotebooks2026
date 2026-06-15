# R is a free software environment for statistical computing and graphics.
# I personally believe that R provides user friendly coding, especially for visual learners.
# It is also one of the best methods available for creating highly customizable, publication quality graphics.

# Like Python, R can perform mathematic functions for us
5 + 5
5 * 5
5 / 5
# exponentiation
5 ** 5
# Modular division (what is the remainder?)
8 %% 3

# When using R studio, you can see variable values on the right hand side (below global environment)
x = 5

# You can alter variables with mathematic functions by including that variable in what you are setting it
# equal to (compared to python where we can use += )
x = x + 2
x = x * 3

# Observing data types in R (Double is essentially float in python). Note that even whole numbers default to double.
typeof(x)

# You can convert data types using the different functions: as.numeric(), as.double(), as.integer(), as.character()
x=5.8
x = as.integer(x)
typeof(x)
# Note the L in the right hand side after the numeric value indicates the number is now in integer format. Conversion
# to an integer does not round, it simply cuts off the decimal point and all numbers after it.

# Concatenating characters in R requires a function such as paste(), rather than just using + (unlike python strings)
y = "Hello"
z = "world!"
yz = paste(y, z, sep=" ")


# You can also utilize loops in r like you can in python. However, the syntax is different.
# The example below is a while loop designed to print every even number between and including 0 to 20
# The conditional states to enter the loop if x is less than or equal to 20. Since 0 satisfies this,
# it enters the loop, prints the iterable x, then adds 2 to x. Since 2 is less than or equal to 20, it
# reenters the loops. This process continues until x = 22.

x = 0
while (x <= 20) {
print(x)
x = x + 2
}

# The example below creates a numeric set, x, and then counts the number of even numbers in that set.
# This for loop does this by setting a counter to zero. It then loops through all the values in the set x.
# The loop states that if the value of the current element can be divided by 2 with a remainder of 0, then to
# add 1 to the count. This process continues until you loop through all the elements in set x. Finally, we print the count.
x <- c(2,5,3,9,8,11,6,22,11,2,4,3,2,6,8)
count = 0
for (val in x) {
if(val %% 2 == 0) count = count+1
}
print(count)

#Loading in a custom data frame. Use the following syntax
df <- data.frame(Name = c("Skyler", "Rachael", "Keith", "Adam", "Tina"),
                 Age = c(29, 25, 30, 11, 40)
)
print (df)


# The install commands are commented out because they only need to be ran once on your machine. Remove the # to run the line.
#install.packages('ggplot2', dep = TRUE)
#install.packages('ggthemes', dep = TRUE)

# The library will need to be loaded every time you reopen the file. We will use ggplot2 later

library(ggplot2)

# We will now set our working directory. This is my favorite way to do it because it can be saved for later, but you can also go to the
# the top of the screen and use session -> set working directory -> choose directory, or control shift H in Windows.
# You must alter the path to where you have your files saved. Below is an example that will only work for me.
setwd("C:/Users/Skyler/desktop")

# The line below loads in your .csv file. You can convert the file to a .csv file using Excel (save as).
# This file, along with the next one that we will load in, comes from publicly available SPARCS data.
# These can be downloaded from https://health.data.ny.gov/ .

# The dataset below is population statistics for the state of New York, from 2003 to 2017

Data <- read.csv("New_York_State_Population_Data__Beginning_2003.csv",header=TRUE)

# Subsetting data - Subsetting your data can be extremely useful in R. However, you must first understand your scientific
# inquiries before you begin to do this. You must also understand your dataset and how it is presented.
# We work to isolate the cohorts as required for our studies and then we can perform statistical analyses.

# First, let us assume that we want to observe populations of individuals who are age 20 to 34. Our dataset has two categories that
# have to do with age. These are Age.Group.Code and Age.Group.Description. Let's view a breakdown of these two categories using
# the table() function.

table(Data$Age.Group.Code)
table(Data$Age.Group.Description)

# At first glance, it seems as though Age.Group.Description would be the better category to use for this task. However, this data column is
# not well curated because there are multiple categories that mean the same things. Example: "20-24" and "20 to 24". Therefore, it might
# be easier to organize using the Age.Group.Codes.

# The following lines can be used to subset the data. The first line subsets the file to only include rows involving Age Group Codes 4 and 5.
# These are 20-24 and 25-34. We do this by telling it to include rows with 4 OR 5 for the Age.Group.Code.
# The | symbol means "or".

Age_Filter <- subset(Data, Data$Age.Group.Code==4 | Data$Age.Group.Code==5)
# Note that for some of these functions, the variable Data does not have to be called upon multiple times like this.
# However, if you type in Data$ , it will autofill with your heading options.

# Note that if we wanted to use the Age.Group.Descriptions, it is possible if we reorganize the data manually. We 
# can concatenate data categories that mean the same thing by renaming them. In the example below, we take our
# Age_Filter subset and rename instances described as "20 to 24" to "20-24". The same is then done with the "25 to 34" category.

table(Age_Filter$Age.Group.Description)
Age_Filter$Age.Group.Description[Age_Filter$Age.Group.Description=="20 to 24"] <- "20-24"
Age_Filter$Age.Group.Description[Age_Filter$Age.Group.Description=="25 to 34"] <- "25-34"
table(Age_Filter$Age.Group.Description)

# The next line subsets the data further based on location. != means not equal to. Therefore, the first line of code removes any rows of
# data pertaining to either Clinton or Wyoming county. 
# The following two lines split the entire file based on gender (note that this is subsetting the original file, not just the 
# groups aged 20-34. You can change this to by replacing "Data" with "Age_Filter").
# The final three lines subset the original file by Population (below 10,000, 10,000-20,000, and 20,000+).
#      |   "or"  means true if any conditional is true
#      &   "and"   means true if all conditionals are true

Age_County_Filter <- subset(Age_Filter, Age_Filter$County.Name!="Clinton" & Age_Filter$County.Name!="Wyoming")

Males <- subset(Data, Data$Gender.Description=="Male")
Females <- subset(Data, Data$Gender.Description=="Female")


Low_Pop <- subset(Data, Data$Population<10000)
Medium_Pop <- subset(Data, Data$Population>=10000 & Data$Population<=20000)
High_Pop <- subset(Data, Data$Population>20000)

# Recombining subsets - Use rbind()
Males_or_Females <- rbind(Males,Females)


# The following line shows you how you can create subsets of specific tables using only specified columns. The below examples remove
# the codes and only keep descriptions.

Col_High_Pop <- subset(High_Pop, select = c(Year,Age.Group.Description,Gender.Description,Race.Ethnicity.Description,County.Name,Population))

# The next two lines can quickly show you the first or last six lines (respectively), plus the header of a particular table.
# I did this to easily check that the previous code worked. This is also important because clicking into the data using
# the global environments to the right won't display all the data if there are too many columns or rows.

head(Col_High_Pop)
tail(Col_High_Pop)

# Data summary statistics
# Gathering summary statistics for categorical data

nrow(Col_High_Pop)
table(Col_High_Pop$Race.Ethnicity.Description)
length(unique(Col_High_Pop$Race.Ethnicity.Description))
prop.table(table(Col_High_Pop$Race.Ethnicity.Description))


# The next few lines show you how you can begin to analyze the numerical data in a particular column.
# The histogram function has additional parameters that could customize it further.
# See documentation for hist() online or use the final line of code in this section.

mean(Low_Pop$Population)
range(Low_Pop$Population)
sd(Low_Pop$Population)
# Remove the # below to install the package Rmisc
#install.packages("Rmisc")
library(Rmisc)
CI(Low_Pop$Population, .95)
hist(Low_Pop$Population)
help(hist)

# You can also use different packages to create highly customizable, publication quality figures. ggplot is excellent for this
# The code below gives box plots showing the Populations of the Low Population Categories based on Race Ethnicity Description (x-axis)
# and Gender Description (key). Use Google to search for ggplot chart types to find one that can display what you intend to look at.
# The documentation will have additional options for modifications.

library(ggthemes)
g <- ggplot(Low_Pop, aes(Race.Ethnicity.Description, Population))
g + geom_boxplot(aes(fill=Gender.Description)) + 
  theme(text = element_text(size=15), axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot",
       subtitle="",
       caption="",
       x="Race Ethnicity Description",
       y="Population")

# Why does this distribution look so strange? Think about the question that we are asking. It is not focused enough. Simply
# limiting what we are viewing to only include the lower population categories does not give us any real insight when
# we have so many different years, counties (including one category that is the entire state of New York), Age Groups, etc.


# Let's focus and ask a different question. What if we want to understand the Population distribution across all of New York
# State for each Race Ethnicity Description, split apart by Gender, and backed by multiple years of data? Let's create a new subset for this.

Data2 <- subset(Data, County.Name == "New York State" & Age.Group.Description=="Total" & Gender.Description!="Total"
                & Race.Ethnicity.Description!="Total")

# This subset includes only the rows pertaining to the entirety of New York State, it includes only data that is the total of all age
# groups, and then we remove the total categories from both Gender Description and Race Ethnicity Description. This is a much more targeted
# question and may display information better.

g <- ggplot(Data2, aes(Race.Ethnicity.Description, Population))
g + geom_boxplot(aes(fill=Gender.Description)) + 
  theme(text = element_text(size=15), axis.text.x = element_text(angle=65, vjust=0.6,)) + 
  labs(title="Box plot",
       subtitle="",
       caption="",
       x="Race Ethnicity Description",
       y="Population") +
  scale_y_continuous(labels = scales::comma)

# These are the sorts of things that you will have to do if your dataset is messy or contains more data than you need.


# Use the broom icon in the upper right hand corner to remove variables that we no longer need to use.
# You can then free up any excess memory being used via the following command

gc()

# Lets load in our next dataset. This has to do with COVID-19 Admissions by Race/Ethnicity In NYS. We will use this to learn about
# creating new columns.

COVID <- read.csv("New_York_State_Statewide_COVID-19_Admissions_by_Race_Ethnicity.csv", header = TRUE)


# Modifying data with ifelse function:
# The arguments are as follows: a conditional, a value if true, then a value if false.
# Here, we create a column titled "From_Big_Apple". We then assign the value "Yes" If the Region is "New York City" and "No" otherwise.
COVID$From_Big_Apple <- ifelse(COVID$Region=="New York City","Yes","No")

# You can also perform mathematic functions to create new columns. The following example will create a column, "More_New_Info" that
# is the Number of Admissions multiplied by two. You can also perform mathematic functions across different columns within the same row.
# Simply replace the 2 with whatever column you want to multiply Number of Admissions by.
COVID$More_New_Info <- (COVID$Number.of.Admissions*2)  
  

# What if we want to see how much time has passed between two columns? The first line here will create a column that contains
# a specific date in it for each row. I chose 04/05/2022 because it was the last time I modified this file.
# The line after that will compare two specified columns with dates in them and add a new column, "Date_Diff", that shows
# the amount of time between those two columns. The format argument shows that the dates are in month/day/year format. If you have
# columns in a different format, the code will need to be changed.
COVID$Todays_Date <- "04/05/2022"
COVID$Date_Diff <- as.Date(as.character(COVID$Todays_Date), format="%m/%d/%Y") - as.Date(as.character(COVID$Week.Starting), format="%m/%d/%Y")

# We can then use the head function to make sure that all the functions we used above worked properly.
head(COVID)

# Finally, this line can create a .csv file from one of your subsets. The file will save to your working directory unless 
# you specify otherwise. I also include an argument here specifying not to include row numbers.
write.csv(COVID, file = "New_COVID_File.csv", row.names = FALSE)



# Now let's start looking at Linear Regression - This information and more can be found in the Jupyter Notebook file. 

# Regression is a method of predicting a response/dependent/target value based on predictors/independent/features.
# Regression techniques primarily differ based the type of relationship between the independent and dependent variables.

# Examples

# Linear Regression: the response (y) is continuous

# Logistic Regression: the response (y) is categorical

# For all models, the predictors (x) can be binary (2 categories), ordinal, continuous, count, or categorical.


# Linear Regression Equation:
#   y = b0 + b1*x1 + ..... + bn*xn + error

# What do we want to do? Find the best values for our variables.

# How do we do that? Minimize the error between the predicted value and the actual value


# Problem: Predict the level of prostate-specific antigen using clinical measures in men who were about to receive a radical prostectomy.

# Data:
#  A data frame with 97 observations on the following 10 variables.

# lcavol: log cancer volume
# lweight: log prostate weight
# age: in years
# lbph: log of the amount of benign prostatic hyperplasia
# svi: seminal vesicle invasion
# lcp: log of capsular penetration
# gleason: a numeric vector
# pgg45: percent of Gleason score 4 or 5
# lpsa: response (y)
# train: logical vector

prostate <- read.csv("C:/Users/Skyler/desktop/prostate.csv",header=TRUE)

# To fit a model and test it, you need to split the data into training, validation (to tune hyperparameters), and test sets.
# Training Dataset: The sample of data used to fit the model.
# Validation Dataset: The sample of data used to provide an unbiased evaluation of a model fit on the training dataset
#       while tuning model hyperparameters.
# Test Dataset: The sample of data used to provide an unbiased evaluation of a final model fit on the training dataset.
# What is cross-validation?
# The goal of cross-validation is to test the model's ability to predict new data that was not used in estimating it,
#       in order to flag problems like overfitting or selection bias and to give an insight on how the model will generalize
#       to an independent dataset. You use your training set to generate multiple splits and perform the analysis on one subset
#       (called the training set) and validate the analysis on the other subset (validation set). The most popular is k-fold cross validation.


## Since we do not have any hyperparamters to tune, let's split the data into training and test sets!

train <- subset(prostate, train=="TRUE")[,1:9]
test <- subset(prostate, train=="FALSE")[,1:9]

nrow(train)
nrow(test)


## visualize the data

pairs(prostate[,1:9], col="blue")

#Assessing data types
str(prostate)


## Let's fit our model using the training data

fit1 <- lm(lpsa ~ ., data = train)
# Note that the period here means to use all other headings as variables. This is effectively the same as writing
#    lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45

summary(fit1)
# lcavol and lweight are highly significant


# Compute 95% confidence interval
confint(fit1,level=0.95)


### How well does our model do when we generalize it to an independent test set?

# Prediction - Make predictions of your column 9, AKA actual level of prostate-specific antigen (lpsa) using the model.
prediction1 <- predict(fit1, newdata = test[,1:8])
predict(fit1, newdata = test[,1:8], interval = "confidence", level=0.95)


# Remember that we wanted to minimize error. What is our training error? What is our testing error?

# Calculate the training error (error of model predictions vs actual training set lpsa values). To do this, you have to understand what
# residuals are. Residuals are the difference between the observed value of the dependent variable and the predicted value, calculated
# within the model.
mean(fit1$residuals^2)
# Calculate the prediction error (error of model predictions vs actual test set lpsa values).
mean((prediction1-test[,9])^2)
