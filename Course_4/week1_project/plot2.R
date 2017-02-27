rm(list = ls())

file_name <- 'exdata%2Fdata%2Fhousehold_power_consumption.zip'
if (file.exists(file_name)){
    file_name <- unzip(file_name)
}

packages <- c('data.table', 'dplyr')
sapply(packages, require, character.only = T, quietly = T)

data <- fread(file_name, na.strings = '?')

dateRange <- c('1/2/2007','2/2/2007')
dateSubset <- data %>% filter(Date %in% dateRange)
names(dateSubset)

#plot 2
# mutate does not support posixlt so had to convert to posixct
plot2_Data <- dateSubset %>% select(Date, Time, Global_active_power) %>%
    mutate(dateTime = as.POSIXct(strptime(paste(dateSubset$Date, dateSubset$Time, sep = " "), "%d/%m/%Y %H:%M:%S")),
           Global_active_power = as.numeric(dateSubset$Global_active_power))
png('plot2.png', width = 480, height = 480)
with(plot2_Data, plot(dateTime, Global_active_power, type = 'l',
                      xlab = '', ylab = "Global Active Power (kilowatts)"))
dev.off()
