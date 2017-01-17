##### COGNITIVE SERVICES API CALL FROM R ##############################
library(httr)
library(jsonlite)
library(dplyr)

# Below is a sample Request body for the API having text id 1 = Negative sentiments, id 2 = Positive sentiments
request_body <- data.frame(
  language = c("en","en","es"),
  id = c("1","2","3"),
  text = c("This is wasted! I'm angry","This is awesome! Good Job Team! appreciated",
           "Asqueroso, andate a lavar el Traste"))

# Below is a Request body for the API having text id 1 = Negative sentiments, id 2 = Positive sentiments
#request_body <- input.data.frame("language","id",text")
request_body <- read.csv2("C:/Users/jacke/Desktop/fb.csv", sep=',', dec='.')

# add appropriate column names
#names <- c("language","id","text")
#names(df) <- names

# Converting the Request body(Dataframe) to Request body(JSON)
request_body_json <- toJSON(list(documents = request_body), auto_unbox = TRUE)

# Below we are calling API (Adding Request headers using add_headers)
result <- POST("https://westus.api.cognitive.microsoft.com/text/analytics/v2.0/sentiment",
               body = request_body_json,
               add_headers(.headers = c("Content-Type"="application/json",
                                        "Ocp-Apim-Subscription-Key"="8f4e1b0b34644080836e3544e41b31c9")))
output <- content(result)

# use unlist to unenvelope structure
m <- matrix(unlist(output),ncol=2,byrow=TRUE)
df1 <- as.data.frame(m[,1])
df2 <- as.data.frame(m[,2])
df <- cbind(df1,df2)
names <- c("score","id")
names(df) <- names
df <- df[,c(2,1)]

df <- df %>% mutate_if(is.factor, as.character)       # unfactor
df$id <- as.integer(df$id)
df$score <- as.numeric(df$score)

plot(df$id, df$score, ylim=c(0,1))
##################################################
