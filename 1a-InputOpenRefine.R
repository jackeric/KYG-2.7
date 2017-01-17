##### Prerequisite ##############################
library(dtplyr)
library(RJSONIO)                                                   # mandatory libraries to install 
library(rrefine)
library(stringr)
setwd("C:/Users/jacke/Documents/R/Guest Strategies")               # sets wd where the R script is (for PowerBI)

##### Get list of batches #######################
url <- "https://secure.ordyx.com/OrderExport.jsp?store_id=263&password=rFDTuXLzspqC&getBatches=true"
df_raw <- fromJSON(url)

# use sapply to unenvelope json structure
close   <- sapply(df_raw, function(x) x[[1]])
batchid <- sapply(df_raw, function(x) x[[2]])
open    <- sapply(df_raw, function(x) x[[3]])

df <- data.frame(cbind(open,batchid,close),stringsAsFactors=FALSE)

# adjust types and sort descending
df$open    <- as.Date(df$open)
df$batchid <- as.integer(df$batchid)
df$close    <- as.Date(df$close)
ranks <- order(df$batchid,decreasing=TRUE)
df <- df[ranks, ]
minBatch <- 1086
maxBatch <- df[1,2]

write.csv(df, "batches.csv", row.names=FALSE)                      # export as csv
rm(df)
rm(df_raw)

##### Get and merge csv files ###################
path1 <- paste("C:/Users/jacke/Desktop/MyPowerBI/Data/Orders/", minBatch, ".csv", collapse="")
path1 <- gsub("[[:space:]]", "", path1)
df_raw1 <- read.csv2(path1, sep=',', dec='.', stringsAsFactors=F)

path2 <- paste("C:/Users/jacke/Desktop/MyPowerBI/Data/Payments/", minBatch, ".csv", collapse="")
path2 <- gsub("[[:space:]]", "", path2)
df_raw2 <- read.csv2(path2, sep=',', dec='.', stringsAsFactors=F)

path3 <- paste("C:/Users/jacke/Desktop/MyPowerBI/Data/Selections/", minBatch, ".csv", collapse="")
path3 <- gsub("[[:space:]]", "", path3)
df_raw3 <- read.csv2(path3, sep=',', dec='.', stringsAsFactors=F)
#write.csv(df_raw3, "df_raw3.csv", row.names=FALSE)
minBatch <- minBatch+1

while (minBatch<maxBatch) {
  path1 <- paste("C:/Users/jacke/Desktop/MyPowerBI/Data/Orders/", minBatch, ".csv", collapse="")
  path1 <- gsub("[[:space:]]", "", path1)
  df_temp1 <- read.csv2(path1, sep=',', dec='.', stringsAsFactors=F)
  df_raw1 <- rbind(df_raw1,df_temp1)
  
  path2 <- paste("C:/Users/jacke/Desktop/MyPowerBI/Data/Payments/", minBatch, ".csv", collapse="")
  path2 <- gsub("[[:space:]]", "", path2)
  df_temp2 <- read.csv2(path2, sep=',', dec='.', stringsAsFactors=F)
  df_raw2 <- rbind(df_raw2,df_temp2)
  
  path3 <- paste("C:/Users/jacke/Desktop/MyPowerBI/Data/Selections/", minBatch, ".csv", collapse="")
  path3 <- gsub("[[:space:]]", "", path3)
  df_temp3 <- read.csv2(path3, sep=',', dec='.', stringsAsFactors=F)
  df_raw3 <- rbind(df_raw3,df_temp3)
  
  minBatch <- minBatch + 1
}

rm(df_temp1)
rm(df_temp2)
rm(df_temp3)

##### Dataframe manipulation ####################
# orders
orders <- df_raw1[,c(1,5:8,10:12,15,17,19:21,23,24)]

names <- c("id","totaloforder","type","opened","closed","createdbyname","loyaltyid",
           "email","first","last","city","state","zip","cell","seats")
names(orders) <- names

orders$totaloforder <- round(orders$totaloforder/100, digits=2)
subTotal <- round(df_raw1$subTotal/100, digits=2)
taxAmount <- round(df_raw1$taxAmount/100, digits=2)
totalvector <- subTotal+taxAmount
orders$discount <- round(totalvector-orders$totaloforder, digits = 2)
orders$type <- as.factor(orders$type)
orders$cell[is.na(orders$cell)] <- " "
orders$zip[is.na(orders$zip)] <- " "
orders$orderdate <- as.Date(orders$closed)

# payments
payments <- df_raw2[,c(1,3,7,9:13,5,6)]

names <- c("id","paymentid","total","paymentcreatedby","paymenttype","paymentname",
           "last4digits","cardtype","tip","gratuity")
names(payments) <- names

payments$total <- round(payments$total/100, digits=2)
payments$tip <- round(payments$tip/100, digits=2)
payments$gratuity <- round(payments$gratuity/100, digits=2)
payments$last4digits[is.na(payments$last4digits)] <- " "

# selections
selections <- df_raw3[,c(1,4:5,7:10)]

names <- c("id","quantity","charge","name","salesGroup","createdbyid","createdbyname")
names(selections) <- names

selections$charge <- round(selections$charge/100, digits=2)
selections$salesGroup <- as.factor(selections$salesGroup)

# COSMETIC FOR DEMO
#more_orders <- orders[orders$orderdate>=2016-10-01, ]
#fake_orders[rep(1:nrow(more_orders),each=3),]
#orders <- orders[!(orders$orderdate=="2016-09-27"), ]
ord <- orders
ord$orderdate <- ord$orderdate + 60
orders <- ord

# a unique code for any dish/item
x <- as.data.frame(table(selections$name))
y <- as.data.frame(unique(x$Var1))
y$uniqueItemID <- seq.int(nrow(y))                                 # create unique key
names <- c("name","uniqueItemID")
names(y) <- names
listofitems <- y[,c(2,1)]

rm(x)
rm(y)
rm(df_raw1)
rm(df_raw2)
rm(df_raw3)

##### Data quality with Google Open Refine ######
payments <- payments[!(is.na(payments$paymentname) | payments$paymentname==""), ]
payments$last4digits <- as.integer(payments$last4digits)
payments$last4digits <- as.character(payments$last4digits)
payments$last4digits <- str_pad(payments$last4digits, 4, pad = "0")

#write.csv(payments, "payments.csv", row.names=FALSE)      
#refine_upload(file = "payments.csv", project.name = "payments_full", open.browser = F)
#################################################