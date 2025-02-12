#Lecture 8.R

#start of the OVB survey analysis
#we will continue the OVB survey analysis in Lecture 8.R


# Loading packages
library(dplyr)
library(haven)
library(readr)
library(knitr)
library(haven)
library(dplyr)
library(readxl)
library(psych)
library(ggplot2)
library(stats4)
library(magrittr)
library(qwraps2)
library(stargazer)

# lfe for running fixed effects regression
# lmtest for displaying robust SE in output table
# haven for loading in dta files
# sandwich for producing robust Var-Cov matrix
# tidyverse for manipulating data and producing plots
# psych for using describe later on


#change directory
#-------------------------------------------
#set your working directory
#-------------------------------------------
#setwd("/Users/sberto/Desktop/")
setwd(""/Users/sofiavillas-boas/Villas-Boas Lab Dropbox/Sofia Villas-Boas/EEP118_Spring2025/Lectures/Lecture8")


#QUESTION 1
#-------------------------------------------
#1. Read in data
#-------------------------------------------
my_data <- read_dta("data2024.dta")

#describe data
describe(my_data,skew = FALSE)


#what is the proportion of correct question1?
mean(mean(my_data$correct1))

#answer: [1] 0.9722222

#what is the proportion of correct question2?
mean(mean(my_data$correct2))

#answer: [1] 0.5555556

#what is the proportion of both correct in general?
mean(mean(my_data$correctboth))

#answer [1] 0.5555556

#Let us construct the 95% confidence interval 
#for the true proportion os answering both questions 1 and 2 correctly
mean_sd(my_data$correctboth)
nobserv<-nrow(my_data)

#let phat be the estimated proportion of both correct in general
phat<-mean(my_data$correctboth)
var_phat<-phat*(1-phat)/nobserv


#Derive a 95\% confidence interval for p and interpret in a sentence.
#critical value df=108  is approx 1.96, two tailed, 5 percent column, 
#and row between 100 and 1000
se_phat<-sqrt(var_phat)


#95% confidence interval
ci95_l<-phat - ( 1.96 * se_phat )
ci95_u<-phat + ( 1.96 * se_phat )

ci95percent=cbind(ci5_l,ci5_u)
ci95percent


#         ci95_l     ci95_u
#[1,] 0.4618389 0.6492722


#end of Lecture 8 slides results