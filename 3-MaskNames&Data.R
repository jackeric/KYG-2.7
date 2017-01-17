##### CREATE A MASKED VERSION ####################
##### Random names
library(randomNames)
set.seed(69)
mask_payments <- payments

mask_RFM <- RFM
mask_RFM$mask_paymentname <- randomNames(length(mask_RFM$paymentname), name.order="first.last", name.sep=" ")

##### Payments
both_payments <- merge(mask_payments,mask_RFM,by = "paymentname")
mask_payments <- both_payments[,c(2,18,3:13)]

##### create Counts
counts <- merge(counts,mask_RFM,by = "paymentname")
counts <- counts[,c(9,2:7)]
write.csv(counts, "counts.csv", row.names=FALSE)                      # export info RFM plus values

##### RFM
mask_RFM <- mask_RFM[,c(6,2:5)]

##### Orders
# total of order
df <- aggregate(totaloforder~createdbyname, data=orders, function(x) sum(x))
df$waitername <- randomNames(nrow(df), name.order="first.last", name.sep=" ")
waiters <- df[,c(1,3)]
mask_orders <- orders
rm(df)
##### Selections
mask_selections <- selections

##### Export as csv
write.csv(mask_orders, "mask_orders.csv", row.names=FALSE)          
write.csv(mask_payments, "mask_payments.csv", row.names=FALSE)               
write.csv(mask_selections, "mask_selections.csv", row.names=FALSE)           
write.csv(mask_RFM, "mask_RFM.csv", row.names=FALSE)
write.csv(waiters, "waiters.csv", row.names=FALSE)
##################################################
