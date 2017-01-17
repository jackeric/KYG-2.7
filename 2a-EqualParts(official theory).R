##### RFM #######################################
##### recency
payments <- payments[payments$paymenttype=="Credit",]
orders$recency <- as.integer(Sys.Date()-orders$orderdate)
rec <- merge(orders,payments,by = "id")                            # inner join
rec <- rec[,c("uniqueID","paymentname","MAX","LTV","recency")]

##### frequency
freq <- as.data.frame(table(payments$paymentname))
names <- c("paymentname","times")
names(freq) <- names
freq$paymentname <- as.character(freq$paymentname)

##### merge
RFM <- merge(rec,freq,by = "paymentname")                          # inner join

##### aggregate
v1 <- aggregate(recency ~ paymentname, data=RFM, function(x) min(x))
v2 <- aggregate(times ~ paymentname, data=RFM, function(x) min(x))
v3 <- aggregate(LTV ~ paymentname, data=RFM, function(x) min(x))

RFM <- merge(v1,v2,by = "paymentname")                             # inner join
RFM <- merge(RFM,v3,by = "paymentname")                            # inner join

names <- c("paymentname","R","F","M")
names(RFM) <- names
rm(rec)
rm(freq)
rm(v1)
rm(v2)
rm(v3)
#####################################
##### set quantiles
RFM <- RFM[order(-RFM$R), ]                                        # sort RFM by recency desc
RFM$R <- with(RFM, cut(RFM$R, 
                       breaks=quantile(RFM$R, probs=seq(0,1,by=0.2), na.rm=TRUE), 
                       include.lowest=TRUE))                       # set quantiles
levels(RFM$R) <- c('1','2','3','4','5')                            # level quantiles from 1 to 5

noise <- runif(nrow(RFM))                                          # add noise to allow quantiles to be created
RFM$F <- RFM$F + noise
RFM <- RFM[order(RFM$F), ]                                         # sort RFM by frequency asc
RFM$F <- with(RFM, cut(RFM$F, 
                       breaks=quantile(RFM$F, probs=seq(0,1,by=0.2), na.rm=TRUE), 
                       include.lowest=TRUE))                       # sets quantiles
levels(RFM$F) <- c('1','2','3','4','5')                            # level quantiles from 1 to 5

RFM <- RFM[order(RFM$M,RFM$paymentname), ]                         # sort RFM by monetary
RFM$M <- with(RFM, cut(RFM$M, 
                       breaks=quantile(RFM$M, probs=seq(0,1,by=0.2), na.rm=TRUE), 
                       include.lowest=TRUE))                       # sets quantiles
levels(RFM$M) <- c('1','2','3','4','5')                            # level quantiles from 1 to 5

RFM$RFMstring <- paste(as.character(RFM$R),as.character(RFM$F),as.character(RFM$M))
RFM$RFMstring <- gsub("[[:space:]]", "", RFM$RFMstring)
RFM$RFMstring <- as.integer(RFM$RFMstring)
RFMa <- RFM
write.csv(RFM, "RFMa.csv", row.names=FALSE)                        # export as csv
###################################################################