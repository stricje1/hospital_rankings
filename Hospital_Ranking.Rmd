---
title: "R Programmig - Hospital Rankings"
author: "Jeffrey Strickland"
date: "12/18/2021"
output: word_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
## Introduction
U.S. Department of Health and Human Services administers the website Hospital Compare web site (http://hospitalcompare.hhs.gov). The purpose of the web site is to provide data and information regarding the quality of care at over 4,000 Medicare-certified hospitals in the U.S. AS such, the datasets for this assignment covers all major U.S. hospitals. 

```{r Warnings_off}
defaultW <- getOption("warn")
options(warn = -1)
```
## Read the data
```{r}
library(readr)
```
## Read the downloaded data from Hospital Compare
```{r}
getwd()
setwd("C:/Users/jeff/Documents/R_Programming/")
path <-getwd()
setwd("C:/Users/jeff/Documents/R_Programming/")
path <- "C:/Users/jeff/Documents/R_Programming/"
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
```

## Exolore the Hospital Care Data
We'll use the names() function to get the names of the outcome object. 
```{r}
# head(outcome) # Not helpful in the case
names(outcome)
```

Looking ahead to Example 2, we need to know what column in the dataset holds the data for 30-day mortality rates for heart attack. This data appears in the column named (column 11):

"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"

## Example 1. 30-Day Mortality Rates for Heart Attack Histogram

In this example, our task is to plot the 30-day mortality rates for heart attack patients at U.S. major hospitals. From our explorations so far, we determined that the data appears in Column 11.

```{r}
outcome[, 11] <- as.numeric(outcome[, 11]) # Column 11 for heart attack rates
#png(file="hospital_mortality3.png",width=900,height=660, res=150)
hist(outcome[, 11]
     ,xlab='Deaths'
     ,main='Hospital 30-Day Death (Mortality) Rates from Heart Attack'
     ,col="lightblue")
dev.off()
```

## Example 2. Best Hospital in a State

In this example, our task is to write a function called best() that take two arguments:

1. the 2-character abbreviated name of a state, and 
2. an outcome name. 

The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the best (i.e. lowest) 30-day mortality rate for the specified outcome in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular outcome will be excluded from the set of hospitals when deciding the rankings.

Note that we use the column-bind function cbind() to extract the columns from the dataset where the outcomes are conditions we are interested in, like "heart attack", and binds them together as a new dataframe called rates, comprised of state, hospital, heart attack, heart failure, and pneumonia (which are the corresponding column names defined in the next code snippet).

Also note that we use two logical operators, ! (not or negation) and %in%, a more intuitive interface as a binary operator, which returns a logical vector indicating if there is a match or not for its left operand. For example, !state %in% rates[,state] provides a condition where the names of the state requested is not in the list of states in the dataset. Also, !is.na, uses is.na (are there any NAs in the data) and ! as a condition for determining if valid rate value is present or not. For example, if the value is not NA, !is.na, the rates value is valid, otherwise it is not.

```{r}
best <- function(state, outcome) {
  ## Read outcome data
  
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],   # hospital 
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack 
                               outcomes[, 17],  # heart failure 
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid.
  
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  
  ## Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numeric values
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Only include values that are not NA 
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome]), ]
  
  ## Get names of hosptial with the lowest rate
  hNames <- hRates[hRates[, outcome] == min(hRates[,outcome]),1]
  
  ## Sort by hospital name if tie
  sort(hNames)[1]
}

```

Here are some sample outputs from best(). First, we look for the best hospital in Texas for heart attacks, and the the best hospital in Maryland for pneumonia.

```{r samples}
print (best("TX", "heart attack"))
print (best("MD", "pneumonia"))

```

## Example 3. Rank of Hospitals by Outcome in a State

Here, we are to write a function called rankhospital that takes three arguments: 

1. the 2-character abbreviated name of a state (state)
2. an outcome (outcome), and 
3. the ranking of a hospital in that state for that outcome (num)

The function reads the outcome-of-care-measures.csv file and returns a character vector with the name of the hospital that has the ranking specified by the num argument (argument 3). For example, the call rankhospital(“MD”, “heart failure”, 5) returns a character vector containing the name of the hospital with the 5th lowest 30-day death rate for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking (smaller numbers are better). If the number given by num is larger than the number of hospitals in that state, then the function will return NA. Hospitals that do not have data on a particular outcome should be excluded from the set of hospitals when deciding the rankings.

```{r ranking}
rankhospital <- function(state, outcome, num = 'best') {
  
  ## Read outcome data
  
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack
                               outcomes[, 17],  # heart failure
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  
  if(!state %in% rates[,"state"]){
    stop('invalid state')
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  ## Get only the hospitals in chosen state
  hRates <- rates[(rates[, "state"] == state), ]
  
  ## Convert outcome rate to numberic, gets a warning
  hRates[, outcome] <- as.numeric(hRates[, outcome])
  
  ## Remove NA values
  hRates <- hRates[!is.na(hRates[, outcome]), ]
  
  ## convert num argument to valid rank
  
  if(num == "best") {
    num <- 1 
  }
  
  if (num == "worst") {
    num <- nrow(hRates) 
  }
  
  ## Order by outcome rate
  hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
  
  ## Get names of hospital 
  
  hRates[num,1]
  
}
```

### Sample Outputs from rankhospital

```{r}
rankhospital("TX", "heart failure", 4) # 4th best hosptial in Texas for heart failure
rankhospital("MD", "heart attack", "worst") # worse hospitals in Maryland fr heart attack
```

## Example 4. Ranking Hospitals in all States

Next, we were to write a function called rankall that takes two arguments: 

1. an outcome name (outcome) and
2. a hospital ranking (num)

The function reads the outcome-of-care-measures.csv file and returns a 2-column data frame containing the hospital in each state that has the ranking specified in num. For example the function call rankall(“heart attack”, “best”) would return a data frame containing the names of the hospitals that are the best in their respective states for 30-day heart attack death rates. The function should return a value for every state (some may be NA). The first column in the data frame is named hospital, which contains the hospital name, and the second column is named state, which contains the 2-character abbreviation for the state name. Hospitals that do not have data on a particular outcome are excluded from the set of hospitals when deciding the rankings.

Notice that we reuse rates <- as.data.frame... from rankhospital(). We also include a simple for loop, explained in the code comments, and two if-then-else statements, also explained in the code comments.

```{r}
rankall <- function(outcome, num = 'best') {
  ## Read outcome data
  
  outcomes <- read.csv("outcome-of-care-measures.csv", 
                       colClasses = "character",
                       header = TRUE)
  
  ## Get data we're interested in
  
  rates <- as.data.frame(cbind(outcomes[, 2],   # hospital
                               outcomes[, 7],   # state
                               outcomes[, 11],  # heart attack
                               outcomes[, 17],  # heart failure
                               outcomes[, 23]), # pneumonia
                         stringsAsFactors = FALSE)
  
  ## Rename columns
  
  colnames(rates) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  
  ## Check outcome is valid: if the medical condition is not one of the 3 of interest, escape from the function
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop('invalid outcome')
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  hRank <- data.frame()
  
  ## For the requested state in a list of unique, sorted states, do...
  for(state in sort(unique(rates[,"state"]))){
    
    ## Get only the hospitals in this state (uses the comparison operator ==)
    ## type ?== to learn more
    hRates <- rates[(rates[, "state"] == state), ]
    
    ## Convert outcome rate to numberic, gets a warning
    hRates[, outcome] <- as.numeric(hRates[, outcome])
    
    ## Remove NA values
    hRates <- hRates[!is.na(hRates[, outcome]), ]
    
    ## convert num argument to valid rank number (rnum): 
    ## if the value is "best" assign it a value of 1.
    ## if the value is "worst" assign it the corresponding row-value
    ## for example, if Texas is the state in row 20 and is rated as "worst", the rnum returned is 20.
    
    if(num == "best") {
      rnum <- 1 
    } else if (num == "worst") {
      rnum <- nrow(hRates) 
    }
    else {rnum = num}
    
    ## Order by outcome rate & hospital name
    hRates <- hRates[order(hRates[, outcome], hRates[, "hospital"]), ]
    
    hName <- hRates[rnum,1]
    
    hRank <- rbind(hRank,
                   data.frame(hospital = hName,
                              state = state))
  }
  
  ## Return dataframe
  hRank
  
}
```

### Sample outputs: Rankings for Best Hospiatl for an medical condition in the State
First, let's look for the hospital ranked 20 for heart attacks.
```{r}
head(rankall("heart attack", 20), 10)
```

Next, let's look for the worst ranked hospital for pneumonia.
```{r}
tail(rankall("pneumonia", "worst"), 3)
```
## Summary
The primary "take-aways" from this chapter are:
* User defined functions can be created for customized functions
* Writing functions in R can include previously defined operators, like ==
* Writing functions in R can include functions that have already been written (packages)
* Writing functions in R can be done by nesting previously defined functions (packages)
* writing functions is R is fairly intuitive

```{r warnings_on}
options(warn = defaultW)
```

