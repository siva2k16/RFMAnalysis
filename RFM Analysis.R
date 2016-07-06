# RFM Analysis 
# Set the Working directory
setwd("~/Desktop/Satish patil/RFM analysis")

# Load the libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggvis)
library(sqldf)

# Load the data into R 
retailsales <- read.csv("Online Retail.csv",sep = ",",header = TRUE)
str(retailsales)


# Total Transactions - 25900
# Total SKU's - 4070
# Description - 4224 different products
#  38 countries 

# Data Exploration 
retailsales$CustomerID <- as.factor(retailsales$CustomerID)
retailsales$InvoiceDate <- dmy_hm(retailsales$InvoiceDate)

summary(retailsales)

# Time Period of sales data 
max(retailsales$InvoiceDate)-min(retailsales$InvoiceDate) # The data is for 373.1833 days

# Filter the data with NA values in the Customer ID 
# The NA values here are generated because of some system error as the corresponding column of Unit price is 
# Zero 
length(retailsales$CustomerID[is.na(retailsales$CustomerID)]) # 135080 NA values in the data.
retailsales_filtered <- filter(retailsales, is.na(retailsales$CustomerID) == FALSE )

# -ve Values in Order Quantity
ggplot(retailsales_filtered, aes(Quantity,UnitPrice))+
  geom_point()

# Since this is a retail store and not an wholesale shop. Extreme values of < -50000 and > 50000 is not possible

retailsales_filtered_Quantity <- filter(retailsales_filtered, Quantity > -50000 & Quantity < 50000)

# Plot
ggplot(retailsales_filtered_Quantity, aes(Quantity,UnitPrice))+
  geom_point()

finalretailsales <- filter(retailsales_filtered_Quantity, Quantity >= 0 & UnitPrice >= 0)

# Plot the Quantity Histogram and Price Histogram 
finalretailsales %>% ggvis(~Quantity) %>% layer_histograms()
finalretailsales %>% ggvis(~UnitPrice) %>% layer_histograms()

# The Available data points include
# 397882 tuples and 8 variables

# RFM Analysis 
# Recency- The Last time a customer has bought the product from the company
# Frequency - The number of times the customer has purchased from the company
# Monetary - The amount he has spent in the given time period 


# Recency
Recency = finalretailsales %>% group_by(CustomerID) %>% summarise(lastpurchasedate = max(InvoiceDate))
Recency = mutate(Recency , Date = substr(x = Recency$lastpurchasedate, start = 0, stop = 10))
Recency$Date = ymd(Recency$Date)
maxdate = ymd(substr(max(retailsales$InvoiceDate), start = 0, stop = 10))
Recency = mutate(Recency, Recency = maxdate - Date)
Recency = Recency[,c(1,4)]


# Monetory value
finalretailsales = mutate(finalretailsales , TotalAmount = UnitPrice*Quantity)
Monetory = finalretailsales %>% group_by(CustomerID) %>% summarise(Monetory = sum(TotalAmount))


# Frequency 
# First sum the total according to Invoice number and Customer ID and then use the Count function on each 
# CustomerID 
Billing = finalretailsales %>% group_by(InvoiceNo,CustomerID) %>% summarise(TransactionAmount = sum(TotalAmount))
Frequency = sqldf("select CustomerID, count(CustomerID) as Frequency from Billing group by CustomerID")


# Joining the Recency,Freqency and Monetory Value tables to one table.

