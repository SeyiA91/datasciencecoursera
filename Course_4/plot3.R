setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_4/")
rm(list = ls())

fp <- '~/Downloads/exdata%2Fdata%2FNEI_data.zip'
if (file.exists(fp)){
    fp <- unzip(fp)
}

sumPm25 <- readRDS(fp[2])
######## 3 #######
require(ggplot2)

png('plot3.png', width = 480, height = 480)
sumPm25 %>% filter(fips == '24510') %>%
    group_by(year) %>% ggplot(., aes(year, Emissions)) + geom_point() +
    geom_line() + facet_wrap(~ type) + ggtitle('Changes in Particulate Matter by Source')
dev.off()
