# 
# Set the Working directory
setwd("")

# Load the libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggvis)
library(sqldf)
library(tidyr)

# Load the data into R 
retailsales <- read.csv("Online Retail.csv",sep = ",",header = TRUE)
str(retailsales)


# Total Transactions - 25900
# Total SKU's - 4070
# Description - 4224 different products
#  38 countries 

## Data Pre-Processing 

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



# Converting the data to User - Product - Quantity
UserProduct <- finalretailsales[,c(3,4,7)]
UserProductQuantity <- UserProduct %>% group_by(CustomerID,Description) %>% summarise(Quantity = sum(Quantity))

# Finding the Number of Products and Number of Users 
length(levels(UserProductQuantity$Description)) # 4224 products 
length(levels(UserProductQuantity$CustomerID)) # 4372 products 

# Density of the data 
Sparsity <- 268293/(4224*4372) #0.01452

#Spread the data 
DataMatrix <- spread(UserProductQuantity,Description,Quantity,fill=0)
DataMatrix = as.data.frame(DataMatrix)
rownames(DataMatrix) <- DataMatrix[,1]
DataMatrix <- DataMatrix[,-1]
rowmeans <- rowSums(DataMatrix)

# Normalize the Data 
DataMatrixNormalized = DataMatrix/rowmeans

#Create a dummy martix 
Products <- as.list(colnames(DataMatrixNormalized))
similarity_matrix <- matrix(data = NA , nrow = ncol(DataMatrixNormalized), ncol = ncol(DataMatrixNormalized), dimnames = list(c(Products),c(Products)))

for(i in 1:length(DataMatrixNormalized)){
  for(j in 1:length(DataMatrixNormalized)){
    if( i == j){
      similarity_matrix[i,j] <- 0
    }
    else{
      similarity_matrix[i,j] <- (DataMatrixNormalized[,i] %*% DataMatrixNormalized[,j])/(((sum(DataMatrixNormalized[,i]^2))^0.5)*((sum(DataMatrixNormalized[,j]^2))^0.5))
    }
  }
}

write.csv(similarity_matrix,"similarity_matrix.csv")

# Predictions 
# We have to know each users Basket and N most similar Items to each Item in the Basket. Arrange the Items 
# according to the Similarity decreasing pattern and pick the top N similar Items.

top10 = NULL
un <- NULL
mylist = list()
for(i in 1:nrow(DataMatrix)){
  x <- DataMatrix[i,]
  y <- colnames(DataMatrix[,c(which(x>0))])
  if(length(y) == 0){ # If the User didn't buy anything. His column will be Zero and no recommendations for him.
    mylist[[name]] <- "No Recommendations"
  }
  else{
    x <- x[y]
    for(j in 1:length(x)){
      z <- similarity_matrix[,colnames(x[j])]
      z <- sort(z,decreasing = TRUE)
      z <- z[1:10]
      top10 <- c(top10,z)
      
    }
    m <- unique(names(top10))
    un <- NULL # This is important 
    for(l in 1:length(m)){
      x1 <- which(names(top10) == m[l])
      y1 <- top10[x1]
      y1 <- sort(y1,decreasing = TRUE)
      y1 <- y1[1]
      un <- c(un,y1)
      un = sort(un,decreasing = TRUE)
    }
    num <- NULL
    for(d in 1:length(x)){
      x1 <- which(names(un) == colnames(x)[d])
      num <- c(num,x1)
    }
    un <- un[-num]
    un <- un[1:10]
    name <- rownames(DataMatrix[i,])
    mylist[[name]] <- names(un)
    top10 = NULL # This is very important. 
  }
}

# A Large list with all the users Predictions are there in mylist 
mylist$`12348`
# This works just super fine. 
# Any bugs, please inform me.
