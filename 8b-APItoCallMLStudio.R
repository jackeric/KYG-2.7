##### CREATE SAMPLE INPUT DATA #####
m <- merge(orders, selections, by="id")
m <- m[,c(1,17,19:22)]
m <- merge(m,payments,by="id")
m <- m[,c(7,1,2,5,3,4,6)]
m$Address <- "Miami"
m$location <- "Brickell"
m$Age <- 40
m$Gender <- "M"
m <- m[,c(1,10,8,11,2:6,9,7)]
names <- c("UserId","Age","Address","Gender","TransactionId","Timestamp","ItemId",
           "Quantity","Value","Location","ProductCategory")
names(m) <- names
m$UserId <- as.character(m$UserId)
m$TransactionId <- as.character(m$TransactionId)
m$Timestamp <- as.character(m$Timestamp)
m$Quantity <- as.numeric(m$Quantity)
m$ProductCategory <- as.character(m$ProductCategory)
length <- nrow(m)
#write.csv(m, "sample.csv", row.names=FALSE)                 # export as csv

##### CHURN ML STUDIO CALL FROM R ################
library(RCurl)
library(rjson)

# Accept SSL certificates issued by public Certificate Authorities
options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))

h = basicTextGatherer()
hdr = basicHeaderGatherer()

req = list(
      Inputs = list(
              "input1" = list(
                         "ColumnNames" = list("UserId", "Age", "Address", "Gender", "TransactionId", "Timestamp", 
                                              "ItemId", "Quantity", "Value", "Location", "ProductCategory"),
                         "Values" = list(list(m[1,]),list(m[2,]),list(m[3,]))
                         )
                   ),
      GlobalParameters = list("ChurnPeriod","28")
)

body = enc2utf8(toJSON(req))
api_key = "F1XFcG95LjpUpbeTmpkZA7YNeDga7IqfDFHhqqhYigWn2enpN9/6OpYLgUlxXw29qYTfg9bM+pJhFDoGOo7Fww=="
authz_hdr = paste('Bearer', api_key, sep=' ')

h$reset()
curlPerform(url = "https://ussouthcentral.services.azureml.net/workspaces/03dbd758e8754a4f80f0329ecbf1fab4/services/b50fc435a6b447a3be524f8e946ef17c/execute?api-version=2.0&details=true",
            httpheader=c('Content-Type' = "application/json", 'Authorization' = authz_hdr),
            postfields=body,
            writefunction = h$update,
            headerfunction = hdr$update,
            verbose = TRUE
)

headers = hdr$value()
httpStatus = headers["status"]
if (httpStatus >= 400)
{
  print(paste("The request failed with status code:", httpStatus, sep=" "))
  
  # Print the headers - they include the requert ID and the timestamp, which are useful for debugging the failure
  print(headers)
}

print("Result:")
result = h$value()
print(fromJSON(result))
##################################################
