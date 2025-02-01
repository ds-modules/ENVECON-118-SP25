#Lecture5
#EEP 118 - this is presented in Lecture in jupyter notebook version
#-------------------------------------------
#install needed R packages
#you will learn this in Sections
#you only need to install them once then only call them using library()
#-------------------------------------------
#call them in
install.packages("readlx")
install.packages("psych")
library(readxl)
library(psych)
library(ggplot2)
library(haven)
library(data.table)
library(dplyr)
library(foreign)
#-------------------------------------------
#set your working directory
#-------------------------------------------
#setwd("/Users/sberto/Desktop/")
setwd("~/Villas-Boas Lab Dropbox/Sofia Villas-Boas/EEP118_Spring2025/Lectures/Lecture5")

#-------------------------------------------
#1. Read in data and see the top rows to see column names etc
#-------------------------------------------
my_data <- read.csv("Lecture5.csv")
head(my_data)

#summarize data
describe(my_data)

#lecture 4 only used year=87
my_data2 <- my_data[my_data$year ==87,] 
head(my_data2)

#regression
regLecture4 <- lm(crmrte ~ polpc,my_data2)
#show output
summary(regLecture4)

#predicted crime rate
my_data2$crmrte_hat <- regLecture4$fitted.values


#-------------------------------------------
#from Lecture 4 scatter plot Y and Yhat
#-------------------------------------------
scatter_Lect4 <- ggplot(my_data2, aes(x=polpc, y=crmrte)) +
  geom_point() +
  labs(x = "X = Police Per Capita",
       y = "Y = Crime Rate",
       title = "Scatter Plot of Y and X",
       subtitle = "1987 Observations Only (N=90)")
scatter_Lect4


#-------------------------------------------
#for Lecture 4 scatter plot Y and Yhat
#-------------------------------------------
scatter_Lect5 <- ggplot(my_data2, aes(x=crmrte, y=crmrte_hat)) +
  geom_point() +
  labs(x = "X = Crime Rate",
       y = "Y = Predicted Crime Rate",
       title = "Scatter Plot of Y and Yhat",
       subtitle = "1987 Observations Only (N=90)")
scatter_Lect5

scatter_Lect5n <- ggplot(my_data2, aes(x=polpc, y=crmrte_hat)) + # initiate plot
  geom_point() + # add points data
  labs(x = "X = Police Per Capita", # add labels
       y = "Yhat = Predicted Crime Rate",
       title = "Scatter Plot of x and Yhat",
       subtitle = "1987 Observations Only (N=90)")
scatter_Lect5n



#--------------------------------------------------------------------------------------
# vary sample size and show how standard errors of beta hats change
#--------------------------------------------------------------------------------------

#--------------------------------------------------------------------------------
#in Jupyter notebook you compare summary(regLecture4) using N=90 with N=630 below
#regression
regLectureN630 <- lm(crmrte ~ polpc,my_data)
#show output
summary(regLectureN630)


#generate fitted values
my_data$crmrte_hat<-regLectureN630$fitted.values


#make combined scatter plot of crime rate data and fitted values of crime rate given regression estimates
scatter_data_fittedVals <- ggplot(data = my_data) + 
  geom_point(aes(x=polpc, y=crmrte, color = "data")) +
  geom_point(aes(x=polpc, y=crmrte_hat, color = "fitted")) + 
  labs(x = "Police Per Capita",
       y = "Crime Rate",
       title = "Police Per Capita and Crime Rate Fitted Values in Blue and Data in Red",
       subtitle = "Full sample (N = 630)")

scatter_data_fittedVals

#--------------------------------------------------------------------------------------
#in lecture 5 in a
#jupyter notebook I also computer SSTx Sum of squared totals of police per capita
# and also SSR sum opf squared of uhats
#to get estimated varianve of the estimated coefficient for 
#police per capita, namely for betapolpc_hat
#--------------------------------------------------------------------------------------
#get SSTx

xbar<-mean(my_data$polpc)
my_data$xMxbar<-my_data$polpc-xbar
SSTx=sum(my_data$xMxbar*my_data$xMxbar)
SSTx

# add predicted crime rate to my_data
my_data <- mutate(my_data, crmrte_hat = regLectureN630$fitted.values)

#generate uhats to get variance of uhats
my_data <- mutate(my_data, uhat = regLectureN630$residuals)

head(my_data)

#get Sum of squared residuals SSR, sum of squared uhats
#Since uhat_bar is zero
SSR<-sum(my_data$uhat*my_data$uhat)
SSR


#Then the variance of uhats is sum uhat_i squared, 
#which is SSR divided by N-2 
#we divide by N-2) because the model lost two degrees of freedom a constant and an x)

(varuhat<-SSR/(630-2))





