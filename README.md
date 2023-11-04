# HR-Employee-Attrition-Analysis-Report
Employee Attrition Analysis with R (IBM Dataset )


In the ever-changing business world, understanding and reducing employee attrition is crucial for the success of any organization. Employee turnover not only affects workforce stability but also has far-reaching implications for productivity and performance. In this project, I delve into the world of human resources data analytics, leveraging the power of R, a statistical programming language, to conduct a comprehensive analysis of employee attrition and its impact on performance. By dissecting HR data and employees performance with statistical techniques, we aim to uncover valuable insights into the factors driving attrition and how it relates to workforce performance.
Take this data-driven journey as we explore the patterns, correlations, and actionable strategies that can empower organizations to retain top talent and optimize their performance.
The goal of this analysis is to model employee attrition and determine the most dominant contributing factors that govern this turnover.
                                   About Me
Data Analyst: Shafic Sebanenya
Email: sebanenyashafic2@gmail.com
LinkedIn: Shafic Sebanenya
Contact: +971-56389 5861/ +971-5250 49127
##Key Findings of the project:

*There's a strong positive correlation between total years working and monthly income and between age and total years working.
*We found a correlation of 50% between age and monthly income, and this correlation can be attributed to various factors.
*Longevity is not a significant factor in employees' attrition. The length of time an employee stays with the company doesn't play a significant role in their decision to leave.
*There is a strong correlation between education and daily rate, especially in research & development departments.
*Younger workers were consequently more vulnerable to layoffs and were at a higher risk of experiencing attrition by HR as compared to their older counterparts.
*The combined factors of age and the total number of working years account for nearly 60% of the variability in total monthly income.

*Let's start with the data*:
In this project I act as a People Data Analyst intern for IBM in the Human Resources department. There's been lots of people leaving the company.
The employer has asked me to determine the reasons behind the sudden layoffs of so many employees. A former employee has claimed that ageism played a part in recent layoffs, and my employer wants me to investigate these claims. I've also been asked to look over the data to see if I have any insights I could provide.
The data used for this analysis is an augmented version created by real IBM data scientists, but isn't exactly 100% real data. You can find it here: Dataset. This data set is a mixed metrics of employee information and has various features. The data set is in CSV format which has 1470 rows and 35 columns. Each row represents an employee. This dataset includes important attributes like:
*Age
*Attrition
*Daily Rate
*Education
*Employee Number
*Gender
*Hourly Rate
*Job Role
*Monthly Income
*Total working years
Years at company
Years in current role

The dataset helps in the analytical stage to examine the metrics report to find trends and patterns that could affect a company. Various analytical techniques are employed based on the desired result. Descriptive analytics, prescriptive analytics, and predictive analytics are a few of them. The only goal of descriptive analytics is to comprehend previous data and identify areas for improvement. The goal of predictive analytics is to foresee future dangers or opportunities by analyzing previous data using statistical models.

There are two types of data used in this dataset, Numerical Discrete Data and Text Categorical(Nominal and Ordinal) Data.
Exploration:
For analysis and making visualizations in this project, I decided to use R. First of all I downloaded R and RStudio. I've uploaded this data in R Studio, which is an IDE, Integrated Development Environment of R. I then created a new R Notebook and imported data from the CSV file into RStudio as a data frame called hr_df.
In this analysis we will explore the following,
Read the data into R
Evaluate relationships (Correlations)
Create scatterplots
Create boxplots
Hypothesis Testing
Linear Regression
To see total job roles and employees for these roles, I created a pivot table in Excel. It indicates there are total 9 types of Job titles employees are serving and total number of employees in IBM are 1470.

                                            #R-Code
hr_df <- rename_with(HR.Employee.Attrition,tolower)
View(hr_df)
hr_df_corr <- select(hr_df,"age","dailyrate","distancefromhome","education","hourlyrate","monthlyincome","monthlyrate","numcompaniesworked","totalworkingyears","trainingtimeslastyear")
View(hr_df_corr)

# Is there a correlation between important demographics? If so,how strong is it.
install.packages("corrplot")
library(corrplot)

cor(hr_df_corr)

#Scatter pair plots showing relationship between monthly income,age 
#totalworkingyears and education
pairs(~monthlyincome +age + totalworkingyears+education,data=hr_df_corr,
      main="Scatterplot Matrix")

# A box whisker plot to see if age is a factor in employee attrition.
boxplot(age~ attrition,data= hr_df, main= "Who got Fired", xlab="attrition",
        ylab="age")

# To compare average ages and to calculate p-values, I created two variables  called
#yes_age & no_age
yes_age <- hr_df[(hr_df$attrition == "Yes"),'age']
no_age<- hr_df[(hr_df$attrition != "Yes"),'age']

# Running the t-test
t.test(yes_age,no_age)

# A Box whisker plot to see if employee number is a factor in employee attrition, and
# new employees were let go more than old employees.
boxplot(age~ attrition, data=hr_df, main= "Who got fired", xlab="attrition",ylab="employmentnumber")

# To compare employeenumbers of those who got fired and those who didnt, I
#created two variables  called yes_employeenumber and no_employeenumber
#to compute the p-values
yes_employeenumber <- hr_df[(hr_df$attrition == "Yes"),'employeenumber']
no_employeenumber<- hr_df[(hr_df$attrition != "Yes"),'employeenumber']

#Running sample t-test
t.test(yes_employeenumber,no_employeenumber)

# Installing and loading ggplot2 package
install.packages("ggplot2")
library(ggplot2)

#Scatter plots to find relationships between education and dailyrates
#and to analyze if gender or dept is a factor in employee attrition
ggplot(data=hr_df)+
  geom_point(mapping=aes(x=education, y=dailyrate, colour= department
                         )) +facet_grid(gender~department)+
  labs(title= "Employee Attrition: Daily Rate Vs Education",subtitle = 
         "Comparison of Departments based on Gender")

#A linear regression model that predicts the Monthly Income based upon Age
model1 = lm(monthlyincome ~ age,data= hr_df)
summary(model1)

#A linear regression model that predicts the monthly income based upon age and total
# working years.
model2 = lm(monthlyincome~ age + totalworkingyears, data= hr_df)
summary(model2)

                                          
