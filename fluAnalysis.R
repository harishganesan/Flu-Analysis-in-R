library(rtweet)
library(ggmap)
library(RJSONIO)

appname <- "AAA_DIC_Lab1_Part3"
key <- "QWRrxAhLDVQqSXsvtl2n1sV9b"
secret <- "bhMxKL6gUlNTYHmze70oij09trtl7eM8QWxlwoIOeY2RRYjt6J"

## create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret)

home_directory <- "/Users/ashutoshahmadalexandar/Desktop/DIC/Lab/Part 3"
file_name <- file.path(home_directory, "twitter_token.rds")
saveRDS(twitter_token, file = file_name)

cat(paste0("TWITTER_ALEX=", file_name),
    file = file.path(home_directory, ".Renviron"),
    append = TRUE,
    fill = TRUE)

#-------Collecting tweets--
t_keys <- cbind("flu influenza", 
                   "flu shot",
                   "flu sick",
                   "flu sickness",
                   "flu fever",
                   "flu CDC",
                   "flu doctor",
                   "flu epidemic",
                   "flu ill",
                   "flu illness",
                   "flu vaccine",
                   "flu virus",
                   "flu hospital",
                   "flu cold",
                   "flu season",
                   "flu outbreak",
                   "flu bed",
                   "flu activity",
                   "flu caught",
                   "flu catch",
                   "flu doctor",
                   "flu home",
                   "flu feeling"
)

head(t1, 10)

for(i in 1:23) { 
  t1 <- search_tweets(t_keys[,i],n=18000,geocode = lookup_coords("usa"),retryonratelimit = TRUE,include_rts = FALSE)
  u1 <- users_data(t1)
  nam <- paste("A", i, sep = "")
  nam1 <- paste("B", i, sep = "")
  assign(nam, t1)
  assign(nam1, u1)
}
head(u1)

twitter_data <- rbind(A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23)
save_as_csv(twitter_data,"twitter_data.csv")
#-- save_as_csv(x, file_name, prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")
#-- Ran the above command to to make separate UserData file for each search tweet combination

for (i in 1:23){
  h <- read.csv(paste0("C:\\Users\\haris\\Desktop\\Acads\\Spring 2018\\DIC\\Part 2\\User Data\\User Data\\fluUsers",i,".users.csv"))
  nam <- paste("C", i, sep = "")
  assign(nam, h)
}

#----------States from lat long

latlong_data <- read.csv(file.choose()) #LatLon_Date.csv, after manually clearing duplicate rows
head(latlong_data)
state_data <- c();
state_data2 <- state_data
state_data <- data.frame(state_data)

state_data4 <- c()

user_dataset <- read.csv(file.choose()) #zz.csv -- all combined 23 user data files

q<-c()
for (i in 1:33021){
  g<- geocode(as.character(user_dataset$location[i]), source = "dsk", output = "latlon")
  q <- rbind(q,g)
}

latlong_data <- unique(q) #For unique values

#-- Swapping the latitude and longitude columns
head(latlong_data)
latlong_data$lat2 <- latlong_data$lon
latlong_data$lon2 <- latlong_data$lat
latlong_data$lon <- latlong_data$lon2
latlong_data$lat <- latlong_data$lat2
latlong_data$lat2 <- NULL
latlong_data$lon2 <- NULL

#-- Getting a list of locations from lat and long
data.json <- c()
#Open Connection
for(i in 1:dim(latlong_data)[1]){
  latlngStr <- gsub(' ','%20', paste(latlong_data[i,], collapse=","))  
  connectStr <- paste("http://www.datasciencetoolkit.org/coordinates2politics/",latlngStr, sep="")
  con <- url(connectStr)
  data.json[i] <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
}

#-- Creating a data frame with Valid states of USA
d <- unlist(data.json)
j <- c()
for(i in 1:length(d)){
  if(d[i] == "state"){
    j <- rbind(j,d[i+1])
  }
}
j1 <- as.data.frame(j)
colnames(j1) <- c("States")
j2 <- j1[1:3481,]
j2 <- as.data.frame(j2)
colnames(j2) <- c("States")
j2 <- as.data.frame(table(j2))
colnames(j2) <- c("States","Count")
j2$scode[j2$Count>0]="yes"
j2$scode = factor(j2$scode)
j3 <- subset(j2,scode == "yes")
head(j3)
j3$States <- tolower(j3$States)
p3_states <- map_data("state")
p3_merged <- merge(p3_states, j3, by.x = "region",by.y = "States")
head(p3_merged)
p3_merged$Count = factor(p3_merged$Count)
p3_merged$Activity[]
p3_merged$ActivityLevel<-cut(p3_merged$Count,breaks = 10)
p3_merged$Activity <- factor(p3_merged$ActivityLevel)

#-- Plotting the heat map
p3_map <- ggplot(p3_merged, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill=ActivityLevel))+
  coord_map()+
  ggtitle("2017-18 Influenza Season Twitter Data")+
  theme(plot.title = element_text(hjust = 0.5))

p3_map
