## Final Project 

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)

## install.packages("readxl")
library(readxl)

#load xls file 
getwd()
setwd("/Users/phu/Documents/info201_assignment/Final-Project")
df <- read_excel("Washington_Offense_Type_by_Agency_2012.xls")
View(df)

Total_Washington_Population <- sum(df$Population)
cat("There is a total of", Total_Washington_Population, "peoples in Washington", "\n")

df$Aggravated_Assault...8




# Prompt the user to enter their name { User Input }
name <- readline(prompt = "What is your name? ")

# Print a message that includes the user's name
cat("Hello, ", name, "! Nice to meet you.\n")




