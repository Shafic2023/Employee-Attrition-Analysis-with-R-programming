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
