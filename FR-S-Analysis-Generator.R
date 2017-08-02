




# Activating potentially relevant packages.

library(ellipse)
library(ggplot2)
library(ggcorrplot)
library(magrittr)
library(gtable)
library(grid)
library(gridExtra)
library(hexbin)
library(nortest)

# Loading data from right location.

getwd()
setwd("C:/Users/j-thomas.netz/Desktop/PPC/Capability Data/")
getwd()
data <- read.csv("SV-L80 Type 1_S_1516.csv", sep=";", dec=",")



# Defining USL and LSL for later
LSL <- 552
USL <- 665



# renaming columns for comfort and usability
colnames(data) <-c("PONo","item", "ProdNo", 
                   "steelGrade", "heat", "rollingLot", "HTLot", 
                   "steelMillCode", "steelMill", 
                   "productGrade", "WT", "OD",
                   "ringNo", "controlLot", "sampleNo", 
                   "samplingDate", "op", "opRef", "operationType", 
                   "testDateAuto", "testDate", "tester",
                   "IPPN",
                   "descr0001", "sampleType", "unit0001",
                   "descr0002", "testTemp", "unit0002",
                   "descr0003", "orientation", "unit0003",
                   "descr0004", "width", "unit0004",
                   "descr0005", "thickness", "unit0005",
                   "descr0006", "val0006", "unit0006",
                   "descr0007", "crossSection", "unit0007",
                   "descr0008", "UTS", "unit0008",
                   "descr0009", "E", "unit0009",
                   "descr0010", "YS", "unit0010")

colnames(data)




# calculating process capability parameters

cpk <- function(var){
  mean <-mean(var)
  sd <- sd(var)
  cpk <- min(((USL-mean)/(3*sd)),((mean-LSL)/(3*sd)))
  return(cpk) # return a single cpk value
}
cp <- function(var){
  sd <- sd(var)
  cp <- (USL-LSL)/(6*sd)
  return(cp) # return a single cp value
}
cpu <- function(var){
  mean <-mean(var)
  sd <- sd(var)
  cpu <- (USL-mean)/(3*sd)
  return(cpu) # return a single cpu value
}
cpl <- function(var){
  mean <-mean(var)
  sd <- sd(var)
  cpl <- (mean-LSL)/(3*sd)
  return(cpl) # return a single cpl value
}


# creating stats DF based on a value and based on a pre-chosen factor
createStats <- function(col, factor){
  factorlist <- list(factor)
  
  stats <- aggregate(col, by = factorlist, function(col) c(N = length(col), 
                                                           mean = round(mean(col), digits=2), sd = round(sd(col), digits=2), 
                                                           cpk = round(cpk(col), digits=2), 
                                                           cp = round(cp(col), digits=2), 
                                                           cpu = round(cpu(col), digits=2), 
                                                           cpl = round(cpl(col), digits=2)))
  
  row.names(stats) <- levels(as.factor(factor))
  stats$Group.1 = NULL
  stats <- t(stats)
  row.names(stats) <- c("N", "mean", "sd", "cpk", "cp", "cpu", "cpl")
  # This part might have to be modifie depending on USL and LSL
  
  return (stats)
}




# Creation of the grid, building the facet grid and the stats chart

capability <- function(df, statsValueDF, statsFactorDF, valueString, colString, lineString, fillString){
  # create stats DF
  stats <- createStats(statsValueDF, statsFactorDF)  
  # create facet grid
  t1 <- ggplot(data=df, aes_string(x=valueString, fill=fillString)) + # strange that I can't parse a variable. 
    geom_histogram(colour="Black", binwidth = 1) +
    
    ggtitle(paste0(valueString, " value distribution \ndepending on ",
                   colString)) +
    geom_vline(aes(xintercept= LSL), colour="Red", linetype = "dashed")  +
    geom_vline(aes(xintercept= USL), colour="Red", linetype = "dashed") +
    #   coord_cartesian(xlim=c(540,660))+
    facet_grid(reformulate(colString,lineString), scales = "free")
  g1 <-ggplotGrob(t1)  
  
  t2 <- ggplot(data=df, aes_string(x=valueString, fill=fillString)) +
    geom_density(alpha = 0.3) +
    stat_function(fun=dnorm, colour="Black", 
                  args = list(mean = mean(data$YS), sd = sd(data$YS))) +
    geom_vline(aes(xintercept= LSL), colour="Red", linetype = "dashed")  +
    geom_vline(aes(xintercept= USL), colour="Red", linetype = "dashed") + 
    #  coord_cartesian(xlim=c(540,660), ylim=c(0,0.03)) +
    facet_grid(reformulate(colString,lineString), scales = "free") 
  g2 <- ggplotGrob(t2)
  maxWidth = unit.pmax(g1$widths[2:3], g2$widths[2:3])
  g1$widths[2:3] <- maxWidth
  g2$widths[2:3] <- maxWidth
  # create stats chart with 
  table <-tableGrob(stats, theme=ttheme_default(base_size=6))
  g <- list(g1,g2, table)
  grid.arrange(grobs = g,  
               layout_matrix = rbind(c(1,2), 
                                     c(1,2), c(1,2), c(3,3))) # Basic layouting  
  
}
# 
capability(data, data$YS, data$OD, "YS", "OD", "WT", "WT")







