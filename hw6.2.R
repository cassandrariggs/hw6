library(dplyr)
library(tidyverse)
library(readr)
rawtree <- read_csv("~/Desktop/hw6/rawtree.csv")

#making sure the City column matches thet string 
all(str_detect(rawtree$City, "[:alpha:]+[:punct:][:space:][:upper:]{2}$"))

#separating the city from the state
str_match(rawtree$City, "([:alpha:]+)[:punct:][:space:]([:upper:]{2})$")

#creating new columns for the city and state separately
rawtree[,c("city", "state")] = str_match(rawtree$City, "([:alpha:]+)[:punct:][:space:]([:upper:]{2})$")[,2:3]

#Creating a count colmn with the number 1 repeating so I can sum the record counts
#because I dont know how to do it another way
rawtree$count <- rep(c(1)) 

#grouping the rawtree data by state and summing the counts
states<-rawtree%>%
  group_by(state)%>%
  summarise(countsum=sum(count))

#creating a bar plot for the total records with labels
barplot(height=states$countsum, names=states$state, 
        main= "Total Records per state",
        xlab = "Count",
        ylab = "State",
        col="#69b3a2", horiz=T , las=1)

#filtering the data to only nc and sc states
ncsc <-rawtree %>%
  filter(state == "NC"| state == "SC")

#pulling the NC and SC city
unique(ncsc$city)

#making sure the names of the trees match the expression
all(str_detect(ncsc$ScientificName, "[:alpha:]+[:space:][:alpha:]+[:punct:]?$"))

#Separating the genus from the species
str_match(ncsc$ScientificName, "([:alpha:]+)[:space:][:alpha:]+[:punct:]?$")

#creating a coluumn for the genus only
ncsc[,c("genus")] = str_match(ncsc$ScientificName, "([:alpha:]+)[:space:][:alpha:]+[:punct:]?$")[,2]

#grouping by genus and averaging the diameter of the crown
genusgroup<-ncsc%>%
  group_by(genus)%>%
  summarise(avg=mean(`AvgCdia (m)`))

#bar chart for genus daimeter average
barplot(height=genusgroup$avg, names=genusgroup$genus, 
        main= "Average Crown Diameter by Genus",
        xlab = "Average Diameter (m)",
        ylab = "Genus",
        col="#69b3a2", horiz=T , las=1)
