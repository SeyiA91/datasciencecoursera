setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_4/")
rm(list = ls())

fp <- '~/Downloads/exdata%2Fdata%2FNEI_data.zip'
if (file.exists(fp)){
    fp <- unzip(fp)
}

sumPm25 <- readRDS(fp[2])
######## 1 ########
require(dplyr)
png('plot1.png', width = 480, height = 480)
sumPm25 %>% group_by(year) %>% summarise_each(funs(mean(., na.rm=TRUE))) %>% 
    select(year, Emissions) %>% with(., plot(year, Emissions, type = 'o', 
                                    ylab = 'Avg. Particulate Matter',
                                    xlab = 'Year', 
                                    main = 'Change in Average PM25 levels (1999-2008)'))
dev.off()
