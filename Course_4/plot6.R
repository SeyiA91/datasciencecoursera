setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_4/")
rm(list = ls())

fp <- '~/Downloads/exdata%2Fdata%2FNEI_data.zip'
if (file.exists(fp)){
    fp <- unzip(fp)
}

########## 6 ###########
sumPm25 <- readRDS(fp[2])
scc <- readRDS(fp[1])
names(sumPm25)
str(scc)
head(scc$Short.Name)

cars <- scc %>% filter(grepl('Vehicle', SCC.Level.Two)) %>%
    select(EI.Sector, SCC, Short.Name) %>% unique
cars.scc <- cars$SCC

png('plot6.png', width = 480, height = 480)
sumPm25 %>% filter(SCC %in% cars.scc, fips == '24510'| fips == '06037') %>%
    group_by(year,fips) %>% select(year, Emissions, fips) %>%
    mutate(city = {if (any(fips == '24510')) 'Baltimore' else 'Los Angeles'}) %>% 
    group_by(city,year) %>% select(city,year,Emissions) %>% summarise_each(funs(mean(., na.rm = T))) %>%
    as.data.frame %>% ggplot(., aes(year, Emissions, group = city, color = city)) +
    geom_line() + geom_point() + ggtitle('Changes in Particulate Matter by Vehicle Source in Los Angeles and Baltimore')
dev.off()
