##### Modifying Text ####
setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_3/")
urlPath <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(urlPath, destfile = 'camera.csv', method = "curl")
cameraData <- read.csv('camera.csv')
names(cameraData)
tolower(names(cameraData))

splitNames = strsplit(names(cameraData), "\\.")
splitNames[[5]]
splitNames[[6]]

mylist <- list(letters = c("A", "b", "c"), numbers = 1:3, matrix(1:25, ncol = 5))
head(mylist)
mylist[1]
mylist$letters
mylist[[1]]

splitNames[[6]][1]
firstElement <- function(x){x[1]} # get the first element 
sapply(splitNames, firstElement) # return the first element of each item passed

fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2 <- "https://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="reviews.csv",method="curl")
download.file(fileUrl2,destfile="solutions.csv",method="curl")
reviews <- read.csv("reviews.csv"); solutions <- read.csv("solutions.csv")
head(reviews,2)
head(solutions,2)

names(reviews)
sub("_","", names(reviews),)

testName <- "this_is_a_test"
gsub("_", "", testName) # gsub replaces all instances
sub("_","", testName) # sub replaces the first instance

grep("Alameda", cameraData$intersection) # returns all instances where "alameda" is present
table(grepl("Alameda", cameraData$intersection)) # returns a logical table of frequencies 
cameraData2 <- cameraData[!grepl("Alameda", cameraData$intersection),] # returns a df w/o "Alamade" present

grep("Alameda", cameraData$intersection, value=TRUE) # returns all instances 
# where presence of string is true
grep("JeffStreet", cameraData$intersection)
length(grep("JeffStreet", cameraData$intersection))

require(stringr)
nchar("Jeffrey Leek") # tells you how many characters are in the string
substr("Jeffrey Leek", 1, 7) # returns elements through the indexes presented
paste("Jeffrey", "Leek")
paste0("Jeffrey", "Leek")# pastes with no separating space
str_trim("jeff       ")# removes white space?


##### Regex ####
# ^ represents the start of a line (i.e ^i think)
# $ represents the end of a line (i.e morning$)
# [Bb][Uu][Ss][Hh] will look for any word "bush" capitalized or not
# ^[Ii] am will look for any line starting with I/i am 
# ^[0-9][a-zA-Z] looks for any assortment of a number followed immediately by letters
# [^?.]$ "^" represents the same as "!" in logical statements so this would consider
# any sentence that DOES NOT end with a "?" or "."
# "." refers to any character soooo "9.11" would match "9/11", "9_11", "9-11", etc.
# "|" is the exact same as the or logical "|" so this: flood|fire returns lines that 
# contain flood OR fire (it can consider multiple: flood|earthquake|hurricane|coldfire)
# ^[Gg]ood|[Bb]ad will retrun all lines regardless of caps that contain good at the
# beggining of the line OR bad anywhere in the line
# ^([Gg]ood|[Bb]ad) ensure that the line MUST start w/ either good or bad
# "?" after an expression indicates it is optional 
# i.e: [Gg]eorge ( [Ww]\.)? [Bb]ush will return lines with george w. bush or just 
# george bush 
# the "\" before the period in ([Ww]\.) represents an escape character to distinguish 
# that we are actually looking for a period to follow the w or W rather than any character
# (.*) means we are looking for any character between parentheses 
# (i.e: "(24, m, germany)", "(east area)", "(0)")
# [0-9]+ (.*)[0-9]+ means we are looking for "at least" (+) one number followed by any 
# character followed by "at least" one number
# [Bb]ush( +[^ ]+ +){1,5} debate means that we are looking for the word B/bush followed
# by at least one space, at least one character that isn't a space, then at least one space
# repeated in a sequence between 1 & 5 times followed by the word debate

##### Dates ######
d1 = date() # date-time
d1
class(d1)
d2 = Sys.Date() # date
d2
class(d2)

# %d = day as number (0-31)
# %a = abbreviated weekday (i.e: Mon)
# %A = unabbreviated weekday (i.e: Monday)
# %b = abbreviated month (i.e: Jan)
# %B = unabbreviated month (i.e: January)
# %y = 2 digit year 
# %Y = 4 digit year

format(d2, "%a %b %d") # returns abbreviated weekday, abbreviated month and the day

x = c("1jan1960", '2jan1960', '31mar1960', '30jul1960'); z = as.Date(x, "%d%b%Y")
z
z[1] - z[2] # returns the difference in dates (i.e: -1 day)
as.numeric(z[1] - z[2]) # returns numerical difference

weekdays(d2)
months(d2)
julian(d2)

require(lubridate)
ymd("20140108")
mdy("08/04/2013")
dmy("03-04-2013")

ymd_hms("2011-08-03 10:15:03")
ymd_hms("2011-08-03 10:15:03", tz = "Pacific/Auckland")
?Sys.timezone

x = dmy(c("1jan2013", '2jan2013', '31mar2013', '30jul2013'))
wday(x[1]) # returns numerical day of the week (i.e. 3)
wday(x[1], label = T) # returns actual day of the week (i.e. Tuesday)

##### Week 4 Quiz #####
# 1
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv",
              'idahoHousing.csv')
idData <- read.csv('idahoHousing.csv')
names(idData)
newNames <- strsplit(names(idData), "\\wgtp")
newNames[123]
# 2
gdp<- read.csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv')

head(gdp)
tail(gdp, 150)
gdp <- gdp[c(-(1:4),-(236:nrow(gdp)),-222, -220, -195),]

require(dplyr)
gdpSemiClean <- gdp[1:190,] %>%
    select(X, Gross.domestic.product.2012, X.2, X.3) %>%
    rename(CountryCode = X, gdp.Rank = Gross.domestic.product.2012,
           CountryNames = X.2, gdp = X.3) %>%
    mutate(gdp.Millions = as.numeric(gsub(",", "", gdp)))

mean(gdpSemiClean$gdp.Millions, na.rm = T)
#4
edu <- read.csv('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv')
head(edu)
tail(edu)
View(edu)

mergedDf <- merge(gdpSemiClean,edu, by = 'CountryCode', all.x = T)
head(mergedDf$Special.Notes)
length(which(grepl("[Jj]une", mergedDf$Special.Notes)))

# 5
require(quantmod)
amzn = getSymbols("AMZN", auto.assign = F)
sampleTimes = index(amzn)
class(sampleTimes)
length(which(year(sampleTimes) == 2012))
length(which(year(sampleTimes) == 2012 & weekdays(sampleTimes) == "Monday"))
