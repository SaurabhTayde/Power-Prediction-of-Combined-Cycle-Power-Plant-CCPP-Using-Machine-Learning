#setwd('G:/DSP/Project/Updated Codes')
setwd('G:/DSP/Project/Final Theoery & Codes')
PlantData_old = read.csv('PlantData.csv')


############# Overview 

dim(PlantData_old)      # dimension (number of rows and columns)
#View(PlantData_old)
names(PlantData_old)    # names of the variables
head(PlantData_old, n=10)
tail(PlantData_old, n=11)
str(PlantData_old)
class(PlantData_old)

summary(PlantData_old)
#The mean and median are pretty close and we can say that,
# the distribution of the variable is approximately normal
# (we will test this with a graphical technique at a later stage)

############### Check Missing Data:

colSums(is.na(PlantData_old))
table(is.na(PlantData_old))
table(complete.cases(PlantData_old))


############## Missing values Imputation using kNN:

install.packages('VIM')
library(VIM)
PlantData_Imputed = kNN(PlantData_old, k = 10)
#View(PlantData_Imputed)
table(is.na(PlantData_Imputed))


PlantData = subset(PlantData_Imputed, select = -c (AT_imp, V_imp, AP_imp, RH_imp, PE_imp))


############### Check Missing Data Again fter Imputation:

colSums(is.na(PlantData))
table(is.na(PlantData))
table(complete.cases(PlantData))    #So no missing values now


############## Overview after imputation:

summary(PlantData)
str(PlantData)

install.packages('Hmisc')
library(Hmisc)
describe(PlantData) # 'describe' in Hmisc function gives missing rows, filled rows, Mean, Percentiles of data



############# Now we need to roughly check whether our variables are closed to normality:

install.packages('moments')

library(moments)  #Moments package is used to determine skewness and kurtosis of the variables

skewness(PlantData, na.rm = T)
kurtosis(PlantData, na.rm = T)


########### Finding outliers:

# Boxplots using traditional graph:

boxplot(PlantData$AT, sub = paste(boxplot.stats(PlantData$AT)$out),
        main = 'Temperature of gas Turbine Boxplot',
        ylab = 'Temp in Degres', col = 2) #No outlier


boxplot(PlantData$V, sub = paste(boxplot.stats(PlantData$V)$out), 
        main = 'Exhaust Vacuum Boxplot',
        ylab = 'Exhaust Vacuum in Hg', col =3) #No outliers

boxplot(PlantData$AP, sub = paste(boxplot.stats(PlantData$AP)$out), 
        main = 'Pressure in the compressor Boxplot',
        ylab = 'Pressure in mb', col =4)   # AP has too many outliers on both sides


boxplot(PlantData$RH, sub = paste(boxplot.stats(PlantData$RH)$out), 
        main = 'Relative humidity Boxplot',
        ylab = 'Relative humidity in %', col = 5) # RH has outliers at negative side of the plot




# Boxplot using ggplot:

library(ggplot2)


#Boxplot of AT

#Basic plot:
ggplot(data = PlantData, aes(x = '', y = AT)) + geom_boxplot()

#Add label to plot:

ggplot(data = PlantData, aes(x = '', y = AT)) + geom_boxplot(fill = 'blue') +
      labs(title = 'Boxplot of AT')
       
#Add label to y axis too:

ggplot(data = PlantData, aes(x = '', y = AT)) + geom_boxplot() +
      labs(title = 'Boxplot of AT', y = 'Temp in Degres')


#Shift boxplot horizontally:

ggplot(data = PlantData, aes(x = '', y = AT)) + geom_boxplot(fill = 'blue') +
  labs(title = 'Boxplot of AT', y = 'Temp in Degres') + coord_flip() + 
  theme(
        panel.grid = element_line(color = 'grey'),
        plot.title = element_text(18),
        axis.title = element_text(20))


#Boxplot of V:

ggplot(data = PlantData, aes(x = '', y = V)) + geom_boxplot() +
  labs(title = 'Boxplot of V', y = 'Exhaust Vacuum in Hg') 

    #Shift boxplot horizontally:

ggplot(data = PlantData, aes(x = '', y = V)) + geom_boxplot() +
  labs(title = 'Boxplot of V', y = 'Exhaust Vacuum in Hg') + coord_flip()


#Boxplot of AP:

ggplot(data = PlantData, aes(x = '', y = AP)) + geom_boxplot() +
  labs(title = 'Boxplot of AP', y = 'Pressure in mb') 

  #Shift boxplot horizontally:

ggplot(data = PlantData, aes(x = '', y = AP)) + geom_boxplot() +
  labs(title = 'Boxplot of AP', y = 'Pressure in mb') + coord_flip()


#Boxplot of RH:

ggplot(data = PlantData, aes(x = '', y = RH)) + geom_boxplot() +
  labs(title = 'Relative humidity Boxplot', y = 'Pressure in mb') 

#Shift boxplot horizontally:

ggplot(data = PlantData, aes(x = '', y = RH)) + geom_boxplot() +
  labs(title = 'Relative humidity Boxplot', y = 'Pressure in mb') + coord_flip()



############ Capping outliers :


#Creating Function for capping outliers:

cap_outliers <- function(x) {
  L = 1.5 * IQR(x)
  y = x
  y[x < (quantile(x)[2] - L)] = quantile(x)[2] - L
  y[x > (quantile(x)[4] + L)] = quantile(x)[4] + L
  y
}

#Above function can be used for capping, but to remove outliers, we can simply use following code:

# x = x[!x %in% boxplot.stats(x)$out]


#Creating New Dataset for Capped Data:

PlantData_Capped = PlantData

names(PlantData)

#We will cap only variables 'AP' and 'RH' as these are the only two variables with outliers

#Capping variable AP:

PlantData_Capped$AP = cap_outliers(PlantData$AP)

describe(PlantData$AP)
describe(PlantData_Capped$AP)


#Capping variable RH:

PlantData_Capped$RH = cap_outliers(PlantData$RH)

describe(PlantData$RH)
describe(PlantData_Capped$RH)


#Let's check outliers in new dataset for variables AP and RH using boxplot:

#Boxplot of AP:

boxplot(PlantData_Capped$AP, sub = paste(boxplot.stats(PlantData_Capped$AP)$out), 
        main = 'Pressure in the compressor Boxplot',
        ylab = 'Pressure in mb', col =4)   

ggplot(data = PlantData_Capped, aes(x = '', y = AP)) + geom_boxplot() +
  labs(title = 'Boxplot of AP', y = 'Pressure in mb') 


#Boxplot of RH:

boxplot(PlantData_Capped$RH, sub = paste(boxplot.stats(PlantData_Capped$RH)$out), 
        main = 'Relative humidity Boxplot',
        ylab = 'Relative humidity in %', col = 5)


ggplot(data = PlantData_Capped, aes(x = '', y = RH)) + geom_boxplot() +
  labs(title = 'Relative humidity Boxplot', y = 'Pressure in mb') 

#So we have removed all the outliers in the data






# ############ Removing outliers (Traditional Approach) :
# 
# MinAP = quantile(PlantData$AP)[2] - 1.5*IQR(PlantData$AP)
# MaxAP = quantile(PlantData$AP)[4] + 1.5*IQR(PlantData$AP)
# 
# MinRH = quantile(PlantData$RH)[2] - 1.5*IQR(PlantData$RH)
# 
# 
# ############# Forming dataset by removing outliers ((Traditional Approach)):
# 
# PlantData2 = subset(PlantData, PlantData$AP >= MinAP & PlantData$AP <= MaxAP & PlantData$RH >= MinRH)
# dim(PlantData2)  # 100 rows removed



##############Using histograms to check whether variable is close to normality:


# Histogram for variable AT:

hist(PlantData_Capped$AT, main = 'Temperature of gas Turbine', xlab = 'AT')   #Basic histogram

hist(PlantData_Capped$AT, breaks = 20, col = 'red', xlim = c(0, 40),
     main = 'Temperature of gas Turbine', xlab = 'AT')

#OR we can also make 'freq' false and increase 'breaks' as follows (Both are separate things):

hist(PlantData_Capped$AT, breaks = 40, freq = F, col = 2, xlim = c(0, 40),
     main = 'Temperature of gas Turbine', xlab = 'AT')

lines(density(PlantData_Capped$AT),col= 3,lwd= 2)  #lwd denotes width of distribution curve


#Hsitogram for variable 'V'

hist(PlantData_Capped$V, freq = F, xlim = c(20, 90), col = 4, breaks = 40, 
     main = 'Exhaust Vaccum', xlab = 'V')

lines(density(PlantData_Capped$V), col = 5, lwd = 2)


#Histogram for variable 'AP'

hist(PlantData_Capped$AP, main = 'Pressure in the compressor', xlab = 'AP',breaks = 40, freq = F,
     xlim = c(990, 1040), col = 6)

lines(density(PlantData_Capped$AP), col = 7, lwd = 2)


#Histogram for variable 'RH'

hist(PlantData_Capped$RH, main = 'Relative Humidity', xlim = c(20, 110), 
     freq = F, xlab = 'RH', col = 8, breaks = 40)

lines(density(PlantData_Capped$RH), col = 9, lwd = 2)





#Histograms using ggplot:

# Temperature of gas Turbine
# Exhaust Vaccum
# Pressure in the compressor
# Relative Humidity


# Histogram for variable AT:

ggplot(data = PlantData_Capped, aes(x = AT)) +
  geom_histogram(fill = 'red', color = 'black') + labs(title = 'Temperature of gas Turbine')

  #We can change the number of bars/bins using 'bins' function:

ggplot(data = PlantData_Capped, aes(x = AT)) +
  geom_histogram(fill = 'red', color = 'black', bins = 40) + labs(title = 'Temperature of gas Turbine')


# Histogram of variable V (Exhaust Vaccum)

ggplot(data = PlantData_Capped, aes(x = V)) +
  geom_histogram(fill = 'green', color = 'black', bins = 40) + labs(title = 'Exhaust Vaccum')


# Histogram of variable AP (Pressure in the compressor)

ggplot(data = PlantData_Capped, aes(x = AP)) +
  geom_histogram(fill = 'purple', color = 'black', bins = 40) + labs(title = 'Pressure in the compressor')


# Histogram of variable RH (Relative Humidity)

ggplot(data = PlantData_Capped, aes(x = RH)) +
  geom_histogram(fill = 'blue', color = 'black', bins = 40) + labs(title = 'Relative Humidity')




############# Using Density plot to Check if the response variable is close to normality:


#Why do we need density plots even after having histograms with us?
#Like the histogram, it generally shows the "shape" of a particular variable.But there are differences. 
#In a histogram, the height of bar corresponds to the number of observations
#in that particular "bin." However, in the density plot,
#the height of the plot at a given x-value corresponds to the "density" of the data.
#Ultimately, the shape of a density plot is very similar to a histogram of the same data,
#but the interpretation will be a little different.


plot(density(PlantData_Capped$AT), main = 'Density plot for AT')
polygon(density(PlantData_Capped$AT), col ='red')

plot(density(PlantData_Capped$V), main = 'Density plot for V')
polygon(density(PlantData_Capped$V), col = 'blue')

plot(density(PlantData_Capped$AP), main = 'Density plot for AP')
polygon(density(PlantData_Capped$AP), col = 'green')

plot(density(PlantData_Capped$RH), main = 'Density plot for RH')
polygon(density(PlantData_Capped$RH), col = 'violet')


#Density plots using ggplot:

#Density plot of AT:

ggplot(data = PlantData_Capped, aes(x = AT)) + geom_density(fill = 'blue')

# Add title and add color in panel background :

ggplot(data = PlantData_Capped, aes(x = AT)) +
  geom_density(fill = 'cyan', color = 'cyan') +
  labs(title = 'The pressure variable is strongly left-skewed') +
  theme(panel.background = element_rect(fill = '#444B5A'))


# Add grid color, adjust plot title font, adjust axis title font:

ggplot(data = PlantData_Capped, aes(x = AT)) +
  geom_density(fill = 'cyan') +
  labs(title = 'Temperature of gas Turbine') +
  theme(panel.background = element_rect(fill = 'black'),
        panel.grid.major = element_line(color = 'grey'),
        panel.grid.minor = element_line(color = 'grey'),
        plot.title = element_text(size = 20),
        axis.title = element_text(size = 18, color = 'black')
        )


# Density plot of Exhaust Vacuum:

ggplot(data = PlantData_Capped, aes(x = V)) +
       geom_density(fill = 'cyan') +
       labs(title = 'Exhaust Vacuum') +
       theme(panel.background = element_rect(fill = 'black'),
            panel.grid = element_line( color = 'grey'),
            plot.title = element_text( size = 20),
            axis.title = element_text(size = 18)
      )


#Density plot Pressure in the compressor:

ggplot(data = PlantData_Capped, aes(x = AP)) + geom_density(fill = 'green') +
      labs(title = 'Pressure in the compressor') +
        theme (panel.background = element_rect(fill = 'black'),
               panel.grid = element_line(color = 'grey'),
               plot.title = element_text(size = 20),
               axis.title = element_text(size = 18)
        )



#Density plot of Relative Humidity - RH:


ggplot(data = PlantData_Capped, aes (x = RH)) + geom_density(fill = 'purple') +
      labs(title = 'Relative Humidity') +
      theme (panel.background = element_rect(fill = 'black'),
             panel.grid = element_line(color = 'grey'),
             plot.title = element_text(size = 20),
             axis.title =  element_text(size = 18))




############ Scatter plots can help visualize any linear relationships between the dependent (response) variable and independent (predictor) variables.


# Scatter Plot of AT using plot function:

plot(x = PlantData_Capped$AT, y = PlantData_Capped$PE)

# Scatter Plot of AT using scatter.smooth function:

scatter.smooth(x = PlantData_Capped$AT, y = PlantData_Capped$PE)

#Scatter plot of AT using GG plot (step by step for understading):

#ggplot short explanation:

#There are few main points in ggplot:
#   
# 1. The ggplot() function
# 2. The data = parameter
# 3. The aes() function
# 4. Geometric objects ("geoms")

# 
# 1. ggplot: Just a function where we start our code of ggplot
# 2. data: dataset on which we are working
# 3. aes() function: It is used to map the variables
# 4. geoms() :  a geometric object is the thing that we draw.
#             When we create a bar chart, we are drawing "bar geoms."
#             When we create a line chart, we are drawing "line geoms."
#             And when we create a scatter plot, you are drawing "point geoms."
#             The geom is the thing that you draw.
#             In ggplot2, we need to explicitly state the type of geom that we want to use
#             (bars, lines, points, etc). When drawing a scatter plot, we'll do this by using geom_point().

library(ggplot2)

ggplot(data = PlantData_Capped, aes(x = AT, y = PE)) + geom_point()

#We can add colour to the plot:

ggplot(data = PlantData_Capped, aes(x = AT, y = PE)) + geom_point(color = 'violet')

#Add the line (curved line) to the plot by adding stat_smooth function:

ggplot(data=PlantData_Capped, aes(x = AT, y = PE)) + geom_point(color = 'violet') + stat_smooth()


#Add the straight line to the plot by adding method in stat_smooth function:

ggplot(data = PlantData_Capped, aes(x = AT, y = PE)) + geom_point(color = 'violet') +
  stat_smooth(method = 'lm')


#Add title to the plot by adding labs() function:

ggplot(data = PlantData_Capped, aes(x = AT, y =PE)) + geom_point(color = 'violet') +
              stat_smooth(method = 'lm') + labs(title = 'ScatterPlot of AT')



# Scatter Plot of 'V' using ggplot:


ggplot(data = PlantData_Capped, aes(x = V, y = PE)) + geom_point(color = 'red') +
  stat_smooth() + labs(title = 'ScatterPlot of V')

ggplot(data = PlantData_Capped, aes(x = V, y = PE)) + geom_point(color = 'red') +
  stat_smooth(method = 'lm') + labs(title = 'ScatterPlot of V')


#Scatter Plot of 'AP' using ggplot:

ggplot(data = PlantData_Capped, aes(x = AP, y = PE)) + geom_point(color = 'purple') +
        stat_smooth() + labs(title = 'ScatterPlot of AP')

ggplot(data = PlantData_Capped, aes(x = AP, y = PE)) + geom_point(color = 'purple') +
        stat_smooth(method = 'lm') + labs(title = 'ScatterPlot of AP')


#Scatter Plot of 'RH' using ggplot:

ggplot(data = PlantData_Capped, aes(x = RH, y = PE)) + geom_point(color = 'green') +
      stat_smooth() + labs(title = 'ScatterPlot of RH')

ggplot(data = PlantData_Capped, aes(x = RH, y = PE)) + geom_point(color = 'green') +
         stat_smooth(method = 'lm') + labs(title = 'ScatterPlot of RH')





################# Multivariate Analysis : To find correlation between all the variables

cor(PlantData_Capped)


install.packages("ggcorrplot")
library(ggcorrplot)

correlations = cor(PlantData_Capped)

ggcorrplot(correlations)

#For circle instead of box, we can use circle as follows:

ggcorrplot(correlations, method = 'circle')

# To order the co-relations properly, we can use following function (its hierarchial clustering only)

ggcorrplot(correlations, hc.order = T)

#We can also set outline color(border color) as follows:

ggcorrplot(correlations, hc.order = T, outline.color = 'black')

#We can also add co-orelation coefficient to the plot with 'lab = T' command:

ggcorrplot(correlations, hc.order = T, outline.color = 'black', lab = T)

#We can also draw lower or upper triagle graphs as follows:

ggcorrplot(correlations, hc.order = T, outline.color = 'black', lab = T, type = 'lower')

ggcorrplot(correlations, hc.order = T, outline.color = 'black', lab = T, type = 'upper')



################## Till nowWe have followed common approach while performing EDA on the data.
################## We can also use a sepcial package in R which is used for EDA. It automatically gives all the required data



install.packages('DataExplorer') 
library(DataExplorer)

plot_str(PlantData)  #checks the dimension of the dataset and shows the variables


#To check the missing values:
plot_missing(PlantData_old)
plot_missing(PlantData_Capped)

#To draw histogram for single variable:
plot_histogram(PlantData_Capped$AT)

#To draw histogram for all the variables:
plot_histogram(PlantData_Capped)

#To draw desnity plot for single variable:
plot_density(PlantData_Capped$AT)

#To draw density plot for all the variables:
plot_density(PlantData_Capped)

# Multivariate Analysis : To draw plot to find co-relaton between all the variables
plot_correlation(PlantData_Capped)

# Multivariate Analysis : To draw barplot 
# This plot is only used for the dataset having categrical varaibles:
# plot_bar(Dataset)


#We can use following command that gives complete information about EDA in html:

create_report(PlantData_Capped)


################### Model Building


##   Separting data into train and test

install.packages('caTools')
library(caTools)
set.seed(100)

index = sample.split(PlantData_Capped, SplitRatio = 0.8)

PlantTrain = PlantData_Capped[index,]
PlantTest = PlantData_Capped[!index,]

dim(PlantTrain)
dim(PlantTest)


##   Model Building on train:

PlantModelTrain = lm(PE~., data = PlantTrain)
summary(PlantModelTrain)  # R-square = 92.7


## Draw ggplot to understand how ggplot can be used in linear model:


ggplot(PlantTrain, aes(x = AP, y = PE)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")


## We can also use for details:

install.packages("ggeffects")

library(ggeffects)


ggpredict(PlantModelTrain,se=TRUE,interactive=TRUE)


##   Residual Plots:

plot(PlantModelTrain)

library(ggplot2)
install.packages('ggfortify')
library(ggfortify)

autoplot(PlantModelTrain, label.size = 3)

autoplot(PlantModelTrain, which = 1:4, label.size = 3)


#Predictions and accuracy:

# Check multi-collinearity with VIF function:

library(MASS)
install.packages('fmsb')

library(fmsb)

VIF(PlantModelTrain)  


PredictedPE = predict(PlantModelTrain, PlantTest) #Find prediction on test using above model

Actual_Pred = data.frame(cbind(Actual = PlantTest$PE, Predicted = PredictedPE))  # make actuals_predicteds dataframe

head(Actual_Pred)

correlation_accuracy = cor(Actual_Pred)
correlation_accuracy  # 96.27%

min_max_accuracy = mean(apply(Actual_Pred, 1, min)/ apply(Actual_Pred, 1, max))
min_max_accuracy    # 99.91%

mape = mean(abs((Actual_Pred$Actual - Actual_Pred$Predicted))/ Actual_Pred$Actual)
mape  # 0.781%



######################### Linear Regression Assumptions:


# 1. There should be a linear and additive relationship between dependent (response) variable
#    and independent (predictor) variable(s). 

#Residual Vs fitted plot:

#How to check: Look for residual vs fitted value plots(explained below. Also we can check scatterplot)

#Residual Vs fitted: Red line shows if linearity assuption is met,
#Red line is should be fairly flat
#Variance also can be looked into cloud of points

plot(PlantModelTrain,1)
#OR
autoplot(PlantModelTrain, which = 1, label.size = 3)


#2. ???	Normal Distribution of error terms:

# Normal QQ plot:

# The error terms must be normally distributed.
# Normal Q-Q plot , points should follow on a diagonal line


plot(PlantModelTrain,2)
autoplot(PlantModelTrain, which = 2, label.size = 3)


#3. 	Homoskedasticity:

#Square root of residuals vs Fitted values plot

#The error terms must have constant variance. This phenomenon is known as homoskedasticity. 

plot(PlantModelTrain,3)
autoplot(PlantModelTrain, which = 3, label.size = 3)

library(car)

#Also we can check the the homoskedasticity using ncv test:
ncvTest(PlantModelTrain)
#In the output of this test, p value should be less than 0.05


#4 The fourth plot helps us find influential cases

#Cook's distance plot
#If you notice some of your data points cross that distance line, 
#you're not so good/ you have influential data points.


plot(PlantModelTrain,4)
autoplot(PlantModelTrain, which = 4, label.size = 3)


#5 Autocorrelation should not be there (There is a independence in the errors):

#There should be no correlation between the residual (error) terms.
#If co-relation exists, it is called as Autocorrelation.
#We are using Durbin - Watson (DW) statistic here

#Independence test

install.packages("lmtest")
library(lmtest)

dwtest(PlantModelTrain)

# This value must lie between 0 and 4. If DW = 2, implies no autocorrelation, 
# 0 < DW < 2 implies positive autocorrelation
# while 2 < DW < 4 indicates negative autocorrelation
