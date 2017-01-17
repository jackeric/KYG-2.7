##################################################
pay_clean <- refine_export(project.name = "payments")              # upload to/reload from open refine

max <- aggregate(total ~ paymentname, data=pay_clean, function(x) max(x))
tot <- aggregate(total ~ paymentname, data=pay_clean, function(x) sum(x))
score <- merge(max,tot, by=c("paymentname"))
score <- score[order(score$paymentname), ]                         # sort name by alphabetical order

score$uniqueID <- seq.int(nrow(score))                             # create UNIQUE CUSTOMER KEY
rm(max)
rm(tot)

pay_clean <- merge(pay_clean, score, by="paymentname")
rm(score)

pay_clean <- pay_clean[,c(13,1:8,11:12,9:10)]
names <- c("uniqueID","paymentname","id","paymentid","total","paymentcreatedby","paymenttype",
           "last4digits","cardtype","MAX","LTV","tip","gratuity")
names(pay_clean) <- names
payments <- pay_clean
#numSummary(payments$total, groups=paymenttype, statistics=c("mean","sd","quantiles"), quantiles=c(0,.25,.5,.75,1))
rm(pay_clean)
write.csv(orders, "orders.csv", row.names=FALSE)                   # export as csv
write.csv(payments, "payments.csv", row.names=FALSE)               
write.csv(selections, "selections.csv", row.names=FALSE)           
write.csv(listofitems, "listofitems.csv", row.names=FALSE)
#################################################