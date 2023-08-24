# As a preliminary step, the required packages are installed.

install.packages('gridExtra')
install.packages('patchwork')
#install.packages('moments')
install.packages('fBasics')
install.packages('lmtest')
install.packages('whitestrap')
install.packages('cumstats')

# Next, import the libraries.

library(dplyr)
library(moments)
library(fBasics)
library(stats)
library(lmtest)
library(whitestrap)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(cumstats)

# Import the data set, which is online in this address:
# https://raw.githubusercontent.com/JuanMirandaEq/deltacsvfiles/main/agefat.csv
composition<-read.csv("https://raw.githubusercontent.com/JuanMirandaEq/deltacsvfiles/main/agefat.csv")

# Identifying data set variables using the head function
head(composition)

# The library dplyr provides the function select to pick 
# or to drop columns. If the minus sign is placed 
# before the name of the column, it is dropped.
# Below, the minus sign is placed before the name of the variables
# X and sex to drop them.

composition<-composition%>%
  select(-X,-sex)

# The two variables were dropped, as can be seen in the 
# head of the data frame
head(composition)

# The function isnan() returns TRUE if a value of a column  is equal to nan 
# In both columns, age and fat, there are no missing values.
print('Column age:')
is.nan(composition$age)

print('Column fat:')
is.nan(composition$fat)

# Two methods of identifying outliers are presented: one graphically and another statistically.

# The ggplot2 library  has the function geom_boxplot, which returns a plot of the distribution of the data
# depicting: the median; the first, and third quartiles; and the whiskers. The whiskers represent the upper 
# and lower limits, which are equal to the maximum and minimum values of the data, respectively, if there are no 
# outliers. If there are outliers the whiskers are equal to the upper and lower reference values. Outliers are represented 
# as points above or below the whiskers.

# Boxplot
# Age's boxplot
par(mfrow = c(1, 2))
#create one horizontal boxplot

#create several horizontal boxplots by group
# text label position
abp=c(min(composition$age),median(composition$age)-2,max(composition$age))
# text label
ab=c(min(composition$age),median(composition$age),max(composition$age))
# text label position
fbp=c(min(composition$fat),median(composition$fat)-2,max(composition$fat))
# text label
fb=c(min(composition$fat),median(composition$fat),max(composition$fat))

# age boxplot
plot1<-ggplot(composition, aes(y=age))+
        geom_boxplot(fill='lightblue', color="black") + #boxplot border and filling colors
        coord_flip()+ # flip position of axis 
        ggtitle('Boxplot of age')+ # Include a title
        theme(plot.title = element_text(hjust = 0.5), # size of the title text 
              panel.background = element_blank(), # remove panel background
              axis.text.x=element_blank(), # remove x axis text
              axis.ticks.x=element_blank(), # remove x axis ticks
              axis.text.y=element_blank(), # remove y axis text
              axis.ticks.y=element_blank(), # remove y axis ticks
              axis.title.y = element_blank(), # remove y axis title
              aspect.ratio = 0.5
             )+
        geom_text(data=data.frame(), 
                  aes(x=c(0.05,0.05,0.05),
                      y=abp, label=ab), 
                  col='black', size=3)# insert text in the plot 

# fat boxplot
plot2 <- ggplot(composition, aes(y=fat)) + 
          geom_boxplot(fill='lightgreen', color="black") + #boxplot border and filling colors
          coord_flip()+ggtitle('Boxplot of fat')+ # flip position of axis 
          theme(plot.title = element_text(hjust = 0.5), # size of the title text 
                panel.background = element_blank(), # remove panel background
                axis.text.x=element_blank(), # remove x axis text
                axis.ticks.x=element_blank(),# remove x axis ticks
                axis.text.y=element_blank(), # remove y axis text
                axis.ticks.y=element_blank(), # remove y axis ticks
                axis.title.y = element_blank(), # remove y axis title
                aspect.ratio = 0.5
               )+
            geom_text(data=data.frame(), 
                      aes(x=c(0.05,0.05,0.05), 
                          y=fbp, label=fb), 
                      col='black', size=3)#inser text in the plot

grid.arrange(plot1, plot2,
             widths =c(1, 1) , 
             heights=c(1), ncol=2, nrow=1)

# As shown in the previous plot, the 'age' variable doesn't contain outliers, 
# as there are no points above or below the whiskers. 
# The whiskers for the 'age' variable are set at 61 and 23, with a median value of 49.

# The 'fat' variable doesn't contain outliers
# since there are not points above or below the whiskers.
# The 'fat' variable whiskers are set at 42 and 7.8, and the median value is 29.1.

# Histogram
# The function geom_histogram of the library ggplot2 plots a histogram

plot1<-ggplot(composition, aes(x=age)) + 
  geom_histogram(color="black", fill="lightblue")+ #histogram's border and filling colors
  theme(plot.title = element_text(hjust = 0.5), #size of the title text 
        panel.background = element_blank(), # remove panel background
        axis.text.x=element_blank(), # remove the text of the x axis
        axis.ticks.x=element_blank(),# remove ticks of the x axis
        #axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),# remove ticks of the y axis
        axis.title.y = element_blank(), # remove title of the y axis
        aspect.ratio = 0.5
  )

plot2<-ggplot(composition, aes(x=fat)) + 
  geom_histogram(color="black", fill="lightgreen")+#histogram's border and filling colors
  theme(plot.title = element_text(hjust = 0.5),#size of the title text
        panel.background = element_blank(), # remove panel background
        axis.text.x=element_blank(), # remove the text of the x axis
        axis.ticks.x=element_blank(), # remove ticks of the x axis
        #axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),# remove ticks of the y axis
        axis.title.y = element_blank(),# remove title of the y axis
        aspect.ratio = 0.5
  )

grid.arrange(plot1, plot2,
             widths =c(1, 1) , 
             heights=c(1), ncol=2, nrow=1)

# As shown in the 'age' histogram, there are no values considerably separated from the majority of the data.
# There are no outliers in the 'age' variable.
# Similarly, in the histogram for the 'fat' variable, there are no values considerably separated from the 
# majority of the data. There are no outliers in the 'fat' variable.

#2.4.2 Statistically

# Interquartile range

# The interquartile range - IQR includes 50% of the data, 
# which is in the middle of the distribution. It spans between the first and third quartiles, 
# and is used to estimate upper and lower reference values. The values above 
# or below these reference values are considered as outliers.


# "age" variable outliers
# IQR of the age variable
# First, obtain the appropriate quartiles.
# Use the quantile() function.
quantile(composition$age)
# First quartile
percentile25=quantile(composition$age)[2]
# Third quartile
percentile75=quantile(composition$age)[4]
print(paste("First quartile: ",percentile25))
print(paste("Third quartile: ",percentile75))

# Estimate the interquartile range by subtracting the first quartile from the third quartile:
iqr=percentile75-percentile25
print(paste("The interquartile range is: ", iqr))

# Estimate the uppper and lower reference values.
# The upper reference value is equal to the sum of the third quartile and 1.5 times the interquartile range.
upper_limit = percentile75 + 1.5 * iqr
# To estimate the lower reference, subtract 1.5 times the interquartile range from the first quartile.
lower_limit = percentile25 - 1.5 * iqr
print(paste("The upper reference value is: ", upper_limit))
print(paste("The lower reference value is: ",lower_limit))

# Detecting outliers involves identifying values that fall outside the reference range, 
# either above or below the reference values.
print('Age series outliers above the upper reference value:')
print(composition$age[composition$age > upper_limit])
print('Age series outliers below the lower reference value:')
print(composition$age[composition$age < lower_limit])

# "fat" variable outliers
# IQR fat variable
# First, obtain the appropriate quartiles.
# Use the quantile() function.
quantile(composition$fat)
# First quartile
percentile25=quantile(composition$fat)[2]
# Third quartile
percentile75=quantile(composition$fat)[4]
print(paste("First quartile: ",percentile25))
print(paste("Third quartile: ",percentile75))

# Estimate the interquartile range by subtracting the first quartile from the third quartile:
iqr=percentile75-percentile25
print(paste("The interquartile range is: ", iqr))

# Estimate the uppper and lower reference values
# The upper reference value is equal to the sum of the third quartile and 1.5 times the interquartile range.
upper_limit = percentile75 + 1.5 * iqr
# To estimate the lower reference, subtract 1.5 times the interquartile range from the first quartile.
lower_limit = percentile25 - 1.5 * iqr
print(paste("The upper reference value is: ", upper_limit))
print(paste("The lower reference value is: ",lower_limit))

# Detecting outliers involves identifying values that fall outside the reference range, 
# either above or below the reference values.
print('Fat series outliers above the upper reference value:')
print(composition$fat[composition$fat > upper_limit])
print('Fat series outliers below the lower reference value:')
print(composition$fat[composition$fat < lower_limit])

# 3.1. Descriptive statistics 
# The function summary returns the minimum, maximum, median, mean, first quantile, third quantile.
summary(composition)

# 3.2. Correlation matrix 
# The function cor returns a matrix of correlations among variables.
# In this example, the correlation among the variables is positive and equal to 0.66
round(cor(composition),2)

# 3.3. Plotting the variables 
# A grid of three plots is created, a scatter plot and two histograms, one for 
# for each variable of the scatter plot. 

# Age histogram plot
plot1<-ggplot(composition, aes(x=age)) + 
  geom_histogram(color="black", fill="lightblue")+# histogram's border and filling colors
  theme(plot.title = element_text(hjust = 0.5),#size of the title text
        panel.background = element_blank(),# remove panel background
        axis.text.x=element_blank(), # remove the text of the x axis
        axis.ticks.x=element_blank(), #remove the ticks of the x axis
        #axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),# remove the ticks of the y axis
        axis.title.y = element_blank(),# remove the title of the y axis
        axis.title = element_blank(), # remove the title of the plot
        aspect.ratio = 0.5
  )

# Fat histogram plot
plot2<-ggplot(composition, aes(x=fat)) + 
  geom_histogram(color="black", fill="lightgreen")+ # histogram's border and filling colors
  theme(plot.title = element_text(hjust = 0.5), #size of the title text
        panel.background = element_blank(),# remove panel background
        #axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(),# remove the ticks of the x axis
        axis.text.y=element_blank(), # remove the text of the y axis
        axis.ticks.y=element_blank(), # remove the ticks of the y axis
        axis.title.y = element_blank(), # remove the title of the y axis
        aspect.ratio = 1,
        axis.title = element_blank()# remove the title of the plot
  )+
  coord_flip()+scale_y_reverse()

# Fat- age scatter plot
plot3<-ggplot(composition, aes(x=age, y=fat)) +
  geom_point(size=2, shape=23)

plot4<-ggplot() + theme(panel.background = element_blank())

design <- "
  444222222
  444222222
  111333333
  111333333
"
# Plot grid 
plot2 +plot1 + plot3 +plot4+ plot_layout(design = design)

# The function lm is used to fit linear models. It can be used to carry out regressions, 
# single stratum analysis of variance and analysis of covariance.

model<-lm(fat~age,data=composition)
summary(model)

# Estimating the log likelihood
# Returns the natural logarithm of the likelihood. It measures a model's goodness of fit.
# Uses the library(stats)
logLik(model)

# Estimating the Akaike Information Criterion - AIC 
# Method to estimate how well a model fits the data.
AIC(model)

# Estimating the Bayesian Information Criterion-BIC 
#  Method for model scoring and selection
BIC(model)

# ANOVA
# Compute analysis of variance (or deviance) tables for one or more fitted model objects.
anova(model)

# Normality of residuals
# Normal distributions are symmetric about the mean, and data near the mean are more frequent.
# Skewness measures the symmetry of the residuals.
# A skewness value equal to 0 implies perfect symmetry. 
# A positive or negative value implies that the residuals are skewed to the right or left, accordingly.
# Use library(cumstats)
skewness(model$residuals)

# Kurtosis measures the thickness of the tails of a distribution
#  A normal distribution has a kurtosis of 3
# Use library(cumstats) 
kurtosis(model$residuals)

# Jarque-Bera Test 
# Tests for skewness and Kurtosis
# Null Hypothesis: Skewness and Kurtosis are equal to zero and three, accordingly.
# In this example, the p value is 0.874, the null hipothesis is not rejected. The residuals are 
# normally distributed.
# library(fBasics)
jarqueberaTest(model$resid)

# Durbin Watson test
# Test for independence of residuals
# Null hypothesis: Errors are serially uncorrelated
# In this example, the p-value is 0.57. The null hypothesis is not rejected.
# library(lmtest)
dwtest(model) 

# white_test 
# This function performs a White’s Test for heteroskedasticity.
# It takes as arguments the regression model. 
# Use library(whitestrap)

white_test(model)
attach(composition)
#par(mar = c(8, 8, 4, 4))


# Scatter plot to perform a heteroskedasticity analysis.
# Define the regression

# plot the regression
p1 <- composition %>% 
  ggplot(aes(x = age, y = fat)) +
  geom_point(colour = "#0000cc") +
  geom_smooth(method = "lm", se = TRUE, colour='#00cc00') +
  labs(title = "Simple Linear Regression") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5, size = 26, colour='#00A300'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(size = 16, colour = '#0000cc'),
        axis.title.x = element_text(size = 22, colour = '#00A300'),
        axis.title.y = element_text(size=22, colour = '#00A300'),
        axis.ticks = element_blank()
        )+
  labs(x='Age - years',
       y='Fat percentage')

p1

# As depicted in the plot, the residuals exhibit a similar distribution along the years range.

# To perform hypothesis tests use the function linearHypothesis.

# Hypothesis test: The intercept is equal to the coefficient of the variable age.
linearHypothesis(model, c("(Intercept)=age"))

# Hypothesis test: the coefficient of the variable age is equal to zero.
linearHypothesis(model, c("age=0"))

# Hypothesis test: the coefficient of the variable age is equal to zero.
# Using an alternative sintaxis. Notice the first vector, c("age") , contains the variable name 
# and the second, c(0),the value to which the variable is set.
linearHypothesis(model, c("age"),c(0))

# Hypothesis test: the intercept is equal to 10 and the coefficient of the variable age is equal to 0.
linearHypothesis(model, c("age=0","(Intercept)=10"))

# Hypothesis test: the intercept is equal to 10 and the coefficient of the variable 
# age is equal to 0.
# Using an alternative sintaxis. Notice the first vector,c("age","(Intercept)"), contains the
# variables names and the second, c(0,10) ,the values to which 
# these variables are set.
linearHypothesis(model, c("age","(Intercept)"),c(0,10))

# Hypothesis test: the intercept is equal to the coefficient of the variable age and this one equal to 1.
linearHypothesis(model, c("age=1","(Intercept)=1"))

# Hypothesis test: the intercept is equal to the coefficient of the variable age and this one equal to 1.
# Using an alternative sintaxis. Notice the first vector,c("age","(Intercept)"), contains the
# variables names and the second, c(1,1) , the values to which 
# these variables are set.
linearHypothesis(model, c("age","(Intercept)"),c(1,1))
