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

# plot 4
plot4_Data <- dateSubset %>% select(Voltage, Date, Time, Global_active_power,
                                    Global_reactive_power, Sub_metering_1, Sub_metering_2,
                                    Sub_metering_3) %>%
    mutate( Global_reactive_power = as.numeric(Global_reactive_power),
            Voltage = as.numeric(Voltage),
            Sub_metering_1 = as.numeric(Sub_metering_1),
            Sub_metering_2 = as.numeric(Sub_metering_2),
            Sub_metering_3 = as.numeric(Sub_metering_3),
            dateTime = as.POSIXct(strptime(paste(Date, Time, sep = " "), "%d/%m/%Y %H:%M:%S")),
            Global_active_power = as.numeric(Global_active_power))

# globalReactivePower <- as.numeric(dateSubset$Global_reactive_power)
# voltage <- as.numeric(dateSubset$Voltage)
png('plot4.png', width = 480, height = 480)
par(mfrow = c(2,2))
with(plot4_Data, plot(dateTime, Global_active_power, type = 'l', xlab = '', ylab = 'Global Active Power'))
with(plot4_Data, plot(dateTime, Voltage, type = 'l', ylab = 'Voltage', xlab = 'datetime'))
with(plot4_Data, plot(dateTime, Sub_metering_1, type = 'l'))
with(plot4_Data, lines(dateTime, Sub_metering_2, type = 'l', col = 'red'))
with(plot4_Data, lines(dateTime, Sub_metering_3, type = 'l', col = 'blue'))
with(plot4_Data, legend('topright', c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
                        lty = 1, lwd = 2.5, col = c('black', 'red', 'blue')))
with(plot4_Data, plot(dateTime, Global_reactive_power, type = 'l',
                      xlab = 'datetime', ylab = 'Global_reactive_power'))
dev.off()