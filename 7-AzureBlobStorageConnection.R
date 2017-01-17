##### WORKAROUNDED BY MANUAL USE OF
##### MICROSOFT AZURE STORAGE EXPLORER
##### (can PUT but not GET at now...)

library(httr)
library(RCurl)

##### Create function
azureBlobCall <- function(url, verb, key, requestBody=NULL, headers=NULL, ifMatch="", md5="") { 
  urlcomponents <- httr::parse_url(url)
  account <- gsub(".blob.core.windows.net", "", urlcomponents$hostname, fixed = TRUE)
  container <- urlcomponents$path
  
  # get timestamp in US locale
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "us")
  `x-ms-date` <- format(Sys.time(),"%a, %d %b %Y %H:%M:%S %Z", tz="GMT")
  Sys.setlocale("LC_TIME", lct)
  
  # if requestBody exists, get content length in bytes and content type
  `Content-Length` <- ""; `Content-Type` <- ""
  if(!is.null(requestBody)) {
    if(class(requestBody) == "form_file") {
      `Content-Length` <- (file.info(requestBody$path))$size
      `Content-Type` <- requestBody$type 
    } else {
      requestBody <- enc2utf8(as.character(requestBody))
      `Content-Length` <- nchar(requestBody, "bytes")
      `Content-Type` <- "text/plain; charset=UTF-8" 
    }
  } 
  
  # combine timestamp and version headers with any input headers, order and create the CanonicalizedHeaders
  headers <- setNames(c(`x-ms-date`, "2015-04-05",  unlist(headers)), 
                      c("x-ms-date", "x-ms-version", unclass(names(unlist(headers)))))
  headers <- headers[order(names(headers))]
  CanonicalizedHeaders <- paste(names(headers), headers, sep=":", collapse = "\n")
  
  # create CanonicalizedResource headers and add any queries to it
  if(!is.null(urlcomponents$query)) {
    components <- setNames(unlist(urlcomponents$query), unclass(names(unlist(urlcomponents$query))))
    componentstring <- paste0("\n", paste(names(components[order(names(components))]),
                                          components[order(names(components))], sep=":", collapse = "\n"))
  } else componentstring <- ""
  CanonicalizedResource <- paste0("/",account,"/",container, componentstring)
  
  # create the authorizationtoken
  signaturestring <- paste0(verb, "\n\n\n", `Content-Length`, "\n", md5, "\n", `Content-Type`, "\n\n\n", 
                            ifMatch, "\n\n\n\n", CanonicalizedHeaders, "\n", CanonicalizedResource)
  requestspecificencodedkey <- RCurl::base64(
    digest::hmac(key=RCurl::base64Decode(key, mode="raw"),
                 object=enc2utf8(signaturestring),
                 algo= "sha256", raw=TRUE)
  )
  authorizationtoken <- paste0("SharedKey ", account, ":", requestspecificencodedkey)
  
  # make the call
  headers_final <- add_headers(Authorization=authorizationtoken, headers, `Content-Type` = `Content-Type`)
  call <- httr::VERB(verb=verb, url=url, config=headers_final, body=requestBody, verbose())
  
  print("signaturestring");print(signaturestring); 
  print(headers_final); print(call)
  return(content(call))
} 

##### MAIN
## Replace 'key' and 'accountName'(here 'devfooddata') with yours
key <- "i8EGfOkxjlQN94o5fDLD+wkNl0fHn8eRttw+JQppiVvfk5eFcl4pq1oNz928F8Hn923YHRLDRxAeG+q9WVKMGw=="
setwd("C:/Users/jacke/Documents/R/RFM")

# BLOB TESTS
# Creates a container named 'mycontainer'
#azureBlobCall("https://devfooddata.blob.core.windows.net/mycontainer?restype=container", "PUT", key)

# List all blobs in the container 'mycontainer'
#azureBlobCall("https://devfooddata.blob.core.windows.net/mycontainer?comp=list&restype=container", "GET", key)

# deletes the blob named 'testblob' 
#azureBlobCall("https://devfooddata.blob.core.windows.net/mycontainer/testblob", "DELETE", key)

# Creates a blob named 'blob' under container 'mycontainer' with the upload of desired files '*.csv'
azureBlobCall("https://devfooddata.blob.core.windows.net/mycontainer/blob", "PUT", key, 
              headers = c("x-ms-blob-type"="BlockBlob"), requestBody = upload_file("*.csv"))

# deletes the container named 'mycontainer' 
#azureBlobCall("https://devfooddata.blob.core.windows.net/mycontainer?restype=container", "DELETE", key)

# GET DATA (does not work)
#setwd("C:/Users/jacke/Desktop/MyPowerBI/Data")
#azureBlobCall("https://devfooddata.blob.core.windows.net/orders", "GET", key, 
#              headers = c("x-ms-blob-type"="BlockBlob"), download_file("*.csv"))
