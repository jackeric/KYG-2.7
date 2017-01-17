##### MAIN #######################################
RFMsave <- RFM
RFMhist <- RFM
RFMhist$date <- as.Date("1900-01-01","%Y-%m-%d")
setwd("C:/Users/jacke/Documents/R/Guest Strategies")
orders <- orders[order(orders$opened),]
payments <- payments[payments$paymenttype=="Credit",]
#xdate <- min(orders$orderdate)+21                                     # sets a min of 21 days to have enough input data 
xdate <- as.Date("2015-10-01","%Y-%m-%d")

#while(xdate<(Sys.Date()-20)){
while(xdate<as.Date("2016-10-31","%Y-%m-%d")){

    hist_orders <- orders[(orders$orderdate<=xdate),]
    hist_orders$recency <- as.integer(xdate-hist_orders$orderdate)
    
    RFM <- merge(hist_orders,payments,by = "id")                       # inner join
    RFM <- RFM[,c("uniqueID","paymentname","MAX","LTV","recency")]
    RFM <- RFM[order(-RFM$recency,RFM$paymentname), ]                  # sort RFM by recency
    
    ##### frequency
    freq <- as.data.frame(table(payments$paymentname))
    names <- c("paymentname","times")
    names(freq) <- names
    freq$paymentname <- as.character(freq$paymentname)
    freqwithcounts <- freq                                             # save for later thresholds
    freq[freq==""] <- NA                                               # set blanks to NA and removes
    freq <- na.omit(freq)                                              
    freq <- freq[order(-freq$times,freq$paymentname), ]                # sort RFM by frequency asc
    
    ##### merge
    RFM <- merge(RFM,freq,by = "paymentname")                          # inner join
    
    ##### aggregate
    v1 <- aggregate(recency ~ paymentname, data=RFM, function(x) min(x))
    v2 <- aggregate(times ~ paymentname, data=RFM, function(x) sum(x))
    #v3 <- aggregate(MAX ~ paymentname, data=RFM, function(x) max(x))
    v3 <- aggregate(LTV ~ paymentname, data=RFM, function(x) sum(x))
    
    RFM <- merge(v1,v2,by = "paymentname")                             # inner join
    RFM <- merge(RFM,v3,by = "paymentname")                            # inner join
    
    names <- c("paymentname","R","F","M")
    names(RFM) <- names
    
    ##### set quantiles
    RFM <- RFM[order(-RFM$R), ]                                        # sort RFM by recency desc
    RFM$R <- with(RFM, cut(RFM$R, 
                           breaks=quantile(RFM$R, probs=seq(0,1,by=0.2), na.rm=TRUE), 
                           include.lowest=TRUE))                       # set quantiles
    levels(RFM$R) <- c('5','4','3','2','1')                            # level quantiles from 1 to 5
    
    RFM$F <- ifelse(RFM$F==1,1,ifelse(RFM$F==2,2,ifelse(RFM$F>25,5,ifelse(RFM$F>10,4,3))))
    RFM$F <- as.factor(RFM$F)
    
    RFM <- RFM[order(RFM$M,RFM$paymentname), ]                         # sort RFM by monetary
    RFM$M <- with(RFM, cut(RFM$M, 
                           breaks=quantile(RFM$M, probs=seq(0,1,by=0.2), na.rm=TRUE), 
                           include.lowest=TRUE))                       # sets quantiles
    levels(RFM$M) <- c('1','2','3','4','5')                            # level quantiles from 1 to 5
    
    RFM$RFMstring <- paste(as.character(RFM$R),as.character(RFM$F),as.character(RFM$M))
    RFM$RFMstring <- gsub("[[:space:]]", "", RFM$RFMstring)
    RFM$RFMstring <- as.integer(RFM$RFMstring)
    
    RFM$date <- xdate
    RFMhist <- rbind(RFMhist,RFM)
    
    rm(RFM)
    rm(freq)
    
    rm(v1)
    rm(v2)
    rm(v3)

    xdate <- xdate+7
}
rm(hist_orders)
library(data.table)
RFMhist <- subset(RFMhist, date>1900-01-01)
RFM <- RFMsave
rm(RFMsave)
write.csv(RFMhist, "RFMhist.csv", row.names=FALSE)                 # export as csv
###################################################################
