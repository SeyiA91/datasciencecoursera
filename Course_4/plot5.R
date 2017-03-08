setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_4/")
rm(list = ls())

fp <- '~/Downloads/exdata%2Fdata%2FNEI_data.zip'
if (file.exists(fp)){
    fp <- unzip(fp)
}

########## 5 ###########
sumPm25 <- readRDS(fp[2])
scc <- readRDS(fp[1])
names(sumPm25)
str(scc)
head(scc$Short.Name)

cars <- scc %>% filter(grepl('Vehicle', SCC.Level.Two)) %>%
    select(EI.Sector, SCC, Short.Name) %>% unique
cars.scc <- cars$SCC

png('plot5.png', width = 480, height = 480)
sumPm25 %>% filter(SCC %in% cars.scc, fips == '24510') %>%
    group_by(year) %>% select(year, Emissions) %>%
    summarise_each(funs(mean(., na.rm = T))) %>%
    with(., plot(year, Emissions, type = 'o', ylab = 'Avg. Particulate Matter',
                 xlab = 'Year', 
                 main = 'Change in Average PM25 Levels From Motor 
                 Vehicle Sources in Baltimore (1999-2008)'))
dev.off()
