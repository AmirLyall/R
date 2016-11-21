# Needed Libraries for Analysis #
library(graphics)
library(ks)
library(XLConnect)
library(plyr)
library(nortest)

###############################################################################################################

# Importing excel sheet 1 for projected prices, renaming the columns, removing unwanted rows #
Projections <- readWorksheet(loadWorkbook
("C:/Analysis_Data.xlsx"),sheet=1)
names(Projections)[1:4] <- c("Year", "High Oil Price", "Low Oil Price", "AEO2016 Reference")
Projections <- Projections[-c(1,2),]

# Importing excel sheet 2 for cleaned historical costs, renaming the columns, removing unwanted rows #
years91to06 <- readWorksheet(loadWorkbook
("C:/Analysis_Data.xlsx"),sheet=2)
years91to06 <- years91to06[-c(1,2),]
names(years91to06)[1:9] <- c("Year", "U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)",
                       "U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)",
                       "U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)",
                       "Geometric Return - Crude Oil", "Geometric Return - Natural Gas",
                       "Geometric Return - Dry Well", "U.S. Return Avg", "U.S. Nominal Cost Avg")

# Importing excel sheet 3 for original costs, renaming the columns, removing unwanted rows #
years60to07 <- readWorksheet(loadWorkbook
("C:/Analysis_Data.xlsx"),sheet=3)
years60to07 <- years60to07[-c(1,2),]
names(years60to07)[1:8] <- c("Year", "U.S. Nominal Cost per Crude Oil Well Drilled (Thousand Dollars per Well)",
                       "U.S. Nominal Cost per Natural Gas Well Drilled (Thousand Dollars per Well)",
                       "U.S. Nominal Cost per Dry Well Drilled (Thousand Dollars per Well)",
                       "Geometric Return - Crude Oil", "Geometric Return - Natural Gas",
                       "Geometric Return - Dry Well", "U.S. Nominal Cost Avg")

# Convert/Replace the percentages with decimal values #
years91to06$`Geometric Return - Crude Oil` <- as.numeric(sub("%", "",years91to06$`Geometric Return - Crude Oil`,fixed=TRUE))
years91to06$`Geometric Return - Natural Gas`<- as.numeric(sub("%", "",years91to06$`Geometric Return - Natural Gas`,fixed=TRUE))
years91to06$`Geometric Return - Dry Well` <- as.numeric(sub("%", "",years91to06$`Geometric Return - Dry Well`,fixed=TRUE))
years91to06$`U.S. Return Avg` <- as.numeric(sub("%", "",years91to06$`U.S. Return Avg`,fixed=TRUE))

##############################################################################################################

# Calculate mean of yearly returns 1991-2006 #
a <- mean(years91to06$`Geometric Return - Crude Oil`)
b <- mean(years91to06$`Geometric Return - Natural Gas`)
c <- mean(years91to06$`Geometric Return - Dry Well`)

# Calculate SD of yearly returns 1991-2006 #
d <- (years91to06$`Geometric Return - Crude Oil`)
e <- (years91to06$`Geometric Return - Natural Gas`)
f <- (years91to06$`Geometric Return - Dry Well`)

# mean=0.1111198 #
returnsmean <- mean(c(a,b,c))
returnsmean

# SD=0.1604925 #
returnssd <- sd(c(d,e,f))
returnssd

##############################################################################################################

# ND Simulation #
P0 <- 2279.8

set.seed(12121)
P2017nd <- rep(0,10000)
for(i in 1:10000){
  P0 <- 2279.8
  r <- rnorm(n=1, mean=returnsmean, sd=returnssd)
  
  Pt <- P0*(1 + r)
  
  for(j in 1:10){
    r <- rnorm(n=1, mean=returnsmean, sd=returnssd)
    Pt <- Pt*(1+r)
  }
  P2017nd[i] <- Pt
}

mean(P2017nd)
sd(P2017nd)

hist(P2017nd, breaks=50, main='2017 Cost Distribution - ND', xlab='Final Cost (Thousand Dollars per Well)')
abline(v = P0, col="red", lwd=2)
mtext("Initial Cost - 2006", at=P0, col="red")

##############################################################################################################

# Distribution of data #
Returns <- rbind(as.numeric(years91to06$`Geometric Return - Crude Oil`),
                  as.numeric(years91to06$`Geometric Return - Natural Gas`),
                  as.numeric(years91to06$`Geometric Return - Dry Well`))

Returns <- as.vector(Returns)
class(Returns)
sum(Returns)
hist(Returns, breaks=50, main='Histogram of Returns', xlab='')

# QQ Plot #
qqnorm(Returns)
qqline(Returns, col = 2)

# Anderson-Darling test for normality #
ad.test(Returns)

##############################################################################################################

# Kernel Estimation #
Density.returns <- density(Returns, bw="SJ-ste")
bw <- Density.returns$bw; bw; P0

set.seed(12121)
P2017kde <- rep(0,10000)
for(j in 1:10000){
  rk <- rkde(fhat=kde(Returns, h=bw), n=1)
  Ptk <- P0*(1+rk)
  for(i in 1:10){
    rk <- rkde(fhat=kde(Returns, h=bw), n=1)
    Ptk <- Ptk*(1+rk)
  }
  P2017kde[j] <- Ptk
}

mean(P2017kde)
sd(P2017kde)

hist(P2017kde, breaks=50, main='2017 Cost Distribution - KDE', xlab='Final Cost (Thousand Dollars per Well)')
abline(v = P0, col="red", lwd=2)
mtext("Initial Cost - 2006", at=P0, col="red")
