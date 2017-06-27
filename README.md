# Process-Control

## loading data

### set wd according to location of files.
setwd("C:/Users/####/Desktop/PPC/CPK Data/")
getwd()


### loading European-style csv files. Ensure usage of UTF8 encoding. 
### This encoding option is not easily accessible with excel, but is when saving a csv file in the editor.

data <- read.csv("####", sep=";", dec=",")

### Defining USL and LSL for later
LSL <- 552
USL <- 655

### checking out data
head(data)
tail(data)
str(data)

### use in case columns need to be dropped.
data$X <- NULL 
data$X.1 <- NULL
data$X.2 <- NULL
