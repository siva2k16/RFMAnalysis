# RFM Analysis 
# Set the Working directory
setwd("")

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
maxdate = ymd("2012-01-01")
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

# RFM Table
Recency$Recency = as.numeric(Recency$Recency)
RFM_Values <- sqldf("select Recency.CustomerID,Recency,Frequency,Monetory from Recency,Frequency,Monetory where Recency.CustomerID = Frequency.CustomerID and Frequency.CustomerID = Monetory.CustomerID")

# Assigning Scores to each and every customer based on his Recency, Frequency and Monetary values 

#Recency Score Allocation
summary(Recency$Recency)
Recency$Recency[Recency$Recency <= quantile(Recency$Recency,0.2)] <- 1
Recency$Recency[Recency$Recency > quantile(Recency$Recency,0.2) & Recency$Recency <= quantile(Recency$Recency,0.4)] <- 2
Recency$Recency[Recency$Recency > quantile(Recency$Recency,0.4) & Recency$Recency <= quantile(Recency$Recency,0.6)] <- 3
Recency$Recency[Recency$Recency > quantile(Recency$Recency,0.6) & Recency$Recency <= quantile(Recency$Recency,0.8)] <- 4
Recency$Recency[Recency$Recency > quantile(Recency$Recency,0.8) & Recency$Recency <= quantile(Recency$Recency,1)] <- 5


#Frequency Score Allocation 
str(Frequency)
Frequency$Frequency[Frequency$Frequency <= quantile(Frequency$Frequency,0.2)] <- 1
Frequency$Frequency[Frequency$Frequency > quantile(Frequency$Frequency,0.2) & Frequency$Frequency <= quantile(Frequency$Frequency,0.4)] <- 2
Frequency$Frequency[Frequency$Frequency > quantile(Frequency$Frequency,0.4) & Frequency$Frequency <= quantile(Frequency$Frequency,0.6)] <- 3
Frequency$Frequency[Frequency$Frequency > quantile(Frequency$Frequency,0.6) & Frequency$Frequency <= quantile(Frequency$Frequency,0.8)] <- 4
Frequency$Frequency[Frequency$Frequency > quantile(Frequency$Frequency,0.8) & Frequency$Frequency <= quantile(Frequency$Frequency,1)] <- 5


# Monetory Score Allocation 
str(Monetory)
Monetory$Monetory[Monetory$Monetory <= quantile(Monetory$Monetory,0.2)] <- 1
Monetory$Monetory[Monetory$Monetory > quantile(Monetory$Monetory,0.2) & Monetory$Monetory <= quantile(Monetory$Monetory,0.4)] <- 2
Monetory$Monetory[Monetory$Monetory > quantile(Monetory$Monetory,0.4) & Monetory$Monetory <= quantile(Monetory$Monetory,0.6)] <- 3
Monetory$Monetory[Monetory$Monetory > quantile(Monetory$Monetory,0.6) & Monetory$Monetory <= quantile(Monetory$Monetory,0.8)] <- 4
Monetory$Monetory[Monetory$Monetory > quantile(Monetory$Monetory,0.8) & Monetory$Monetory <= quantile(Monetory$Monetory,1)] <- 5


# Joining the Recency,Freqency and Monetory Value tables to one table.
RFM <- sqldf("select Recency.CustomerID,Recency,Frequency,Monetory from Recency,Frequency,Monetory where Recency.CustomerID = Frequency.CustomerID and Frequency.CustomerID = Monetory.CustomerID")

# Allocating score to each and every user 
RFM = mutate(RFM, RFMscore = paste0(RFM$Recency,RFM$Frequency,RFM$Monetory))
RFM$RFMscore <- as.integer(RFM$RFMscore)
str(RFM)


# Clustering the Users. We basically want to find the most valuable customers, Frequent Shoppers and 
# Expected or churn customers. So divding the Shoppers into 3 clusters 
## K-means

#Converting the 1st column into rownames in the RFM_Values data.
rownames(RFM_Values) <- RFM_Values[,1]
RFM_Values = RFM_Values[,-1]

# Set the seed.
set.seed(1)
RFMscale <- scale(RFM_Values)
Clustering <- kmeans(RFM_Values,centers = 3)

# list of cluster assignments
o = order(Clustering$cluster)
clustered_customers = data.frame(Clustering$cluster[o])
clustered_customers$CustomerID = rownames(clustered_customers)
colnames(clustered_customers)[1] <- "customer_segment"

# Bringing back the RFM_Values customer ID to the columns
RFM_Values$CustomerID = rownames(RFM_Values)

#Final_Clustered_Customers 
Final_clustered_customers = sqldf("select RFM_Values.CustomerID,RFM_Values.Recency,RFM_Values.Frequency,RFM_Values.Monetory,clustered_customers.customer_segment from RFM_Values,clustered_customers where RFM_Values.CustomerID = clustered_customers.CustomerID")
str(Final_clustered_customers)
Final_clustered_customers$CustomerID <- as.factor(Final_clustered_customers$CustomerID)


# 
RFMscore_segment = sqldf("select RFM.CustomerID,RFM.RFMscore,Fcc.customer_segment from RFM,Final_clustered_customers as Fcc where RFM.CustomerID = Fcc.CustomerID")



#Segmenting the customers
segment1 <- filter(Final_clustered_customers, customer_segment == 1) # Shoppers 
summary(segment1)
#Total Customers - 4 | Median Recency - 24 | Median Frequency - 67 | Median Monetory - 227104

segment2 <- filter(Final_clustered_customers, customer_segment == 2) # Best Valueable
summary(segment2)
#Total Customers - 32 | Median Recency - 27 | Median Frequency - 32 | Median Monetory - 41524

segment3 <- filter(Final_clustered_customers, customer_segment == 3)
summary(segment3)
#Total Customers - 32 | Median Recency - 74 | Median Frequency - 2 | Median Monetory - 664.1


#Description
#Segment1 are the most loyal and frequent shoppers. Privileage them.
#Segment2 are the most Valuable customers. cross sell/upsell them 
# Segment3 are Churn/dead customers. Entice them with offers to visit the stores.




