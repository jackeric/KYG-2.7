##### MAIN
both_payments_hist <- merge(RFMhist,both_payments,by="paymentname")
mask_RFMhist <- both_payments_hist[,c(23,2:6)]
rm(both_payments_hist)
mask_RFMhist <- aggregate(RFMstring.x~mask_paymentname+R.x+F.x+M.x+date, data=mask_RFMhist, function(x) min(x))
mask_RFMhist <- mask_RFMhist[,c(1:4,6,5)]
names <- c("paymentname","R","F","M","RFMstring","date")
names(mask_RFMhist) <- names
write.csv(mask_RFMhist, "mask_RFMhist.csv", row.names=FALSE)       # export as csv
###################################################################