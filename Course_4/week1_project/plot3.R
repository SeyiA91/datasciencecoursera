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

#plot 3
plot3_Data <- dateSubset %>% select(Date, Time, Global_active_power, Sub_metering_1, Sub_metering_2, Sub_metering_3) %>%
    mutate( Sub_metering_1 = as.numeric(Sub_metering_1),
            Sub_metering_2 = as.numeric(Sub_metering_2),
            Sub_metering_3 = as.numeric(Sub_metering_3),
            dateTime = as.POSIXct(strptime(paste(Date, Time, sep = " "), "%d/%m/%Y %H:%M:%S")),
            Global_active_power = as.numeric(Global_active_power))

png('plot3.png', width = 480, height = 480)
with(plot3_Data, plot(dateTime, Sub_metering_1, type = 'l', xlab = '', ylab = 'Energy sub metering'))
with(plot3_Data, lines(dateTime, Sub_metering_2, type = 'l', col = 'red'))
with(plot3_Data, lines(dateTime, dateSubset$Sub_metering_3, type = 'l', col = 'blue'))
with(plot3_Data, legend('topright', c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
                        lty = 1, lwd = 2.5, col = c('black', 'red', 'blue')))
dev.off()