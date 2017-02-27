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

#plot 1
png('plot1.png', width = 480, height = 480)
hist(dateSubset$Global_active_power, xlab = 'Global Acitve Power (kilowatts)',
     ylab = 'Frequency', main = 'Global Active Power', col = 'red')
dev.off()



