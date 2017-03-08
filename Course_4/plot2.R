setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_4/")
rm(list = ls())

fp <- '~/Downloads/exdata%2Fdata%2FNEI_data.zip'
if (file.exists(fp)){
    fp <- unzip(fp)
}

sumPm25 <- readRDS(fp[2])

####### 2 ########
png('plot2.png', width = 480, height = 480)
sumPm25 %>% filter(fips == '24510') %>% group_by(year) %>% 
    summarise_each(funs(mean(., na.rm = T))) %>% select(year, Emissions) %>%
    with(., plot(year, Emissions, type = 'o', ylab = 'Avg. Particulate Matter',
                 xlab = 'Year', 
                 main = 'Change in Average PM25 levels in Baltimore (1999-2008)'))
dev.off()
