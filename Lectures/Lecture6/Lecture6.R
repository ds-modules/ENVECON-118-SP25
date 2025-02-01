#Lecture6 

#-------------------------------------------
#install needed R packages
#you will learn this in Sections
#you only need to install them once then only call them using library()
#-------------------------------------------
#for reading escell data file install the package below
#install.packages("readxl")
#for summary stats install below
#install.packages("psych")
#for scatter plots etc install ggplot2
#install.packages("ggplot2")
#install haven
#install.packages("haven")
#install.packages("data.table")
#install.packages("dplyr")
#install.packages("foreign")
#call them in
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
setwd("~/Villas-Boas Lab Dropbox/Sofia Villas-Boas/EEP118_Spring2025/Lectures/Lecture6")

#-------------------------------------------
#1. Read in data and see the top rows to see column names etc
#-------------------------------------------
#read in Lecture6 data set
#read in a Stata dataset
my_data <- read_dta("Lecture6.dta")
head(my_data)

#-------------------------------------------
#summary stats of data
#-------------------------------------------
describe(my_data,skew=FALSE)


#------------------------------------------
#generate log of wage to be the depedent variable

my_data$lwage<-log(my_data$wage)
describe(my_data$lwage, skew=FALSE)



#reg0
reg0<-lm(lwage ~ educ+exper+female+services,my_data)
summary(reg0)

#reg1
reg1<-lm(lwage ~ educ+exper+female,my_data)
summary(reg1)

#reg2
reg2<-lm(lwage ~ educ+exper,my_data)
summary(reg2)

#reg3
reg3<-lm(lwage ~ educ,my_data)
summary(reg3)



#-----------------------------------------------------------------------------------------------
#run regressions 
#-----------------------------------------------------------------------------------------------
reg1<-lm(lwage ~ educ+exper+female,my_data)
summary(reg1)

#we ran Lecture6.R, and estimated a 34.3597% female wage gap
#we estimated the model lwage=beta0+ educ beta1+ exper beta2+ female beta3 + epsilon



#add non white
reg6<-lm(lwage ~ educ+exper+female+nonwhite,my_data)
summary(reg6)



#=======

#nice extra from a querstion of one of your colleagues 

#Extra, how would you see wage gap for non white and female?
#you would create a variable that is non white AND female, that is the product
#of non white indicator and the female indicator

my_data$nonwhiteFemale<-my_data$nonwhite*my_data$female
reg7<-lm(lwage ~ educ+exper+female+nonwhite+nonwhiteFemale,my_data)
summary(reg7)


#what variable would you create to estimate whether there are educ marginal effect on lwage
my_data$feduc<-my_data$educ*my_data$female
reg7extra<-lm(lwage ~ educ+exper+female+feduc,my_data)
summary(reg7extra)

