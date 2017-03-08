setwd("~/dev/datasciencecoursera/datasciencecoursera/Course_4/")
rm(list = ls())

fp <- '~/Downloads/exdata%2Fdata%2FNEI_data.zip'
if (file.exists(fp)){
    fp <- unzip(fp)
}

########## 4 ###########
sumPm25 <- readRDS(fp[2])
scc <- readRDS(fp[1])
names(sumPm25)
str(scc)
head(scc$Short.Name)

coal <- scc %>% filter(grepl('[Cc]oal', EI.Sector)) %>%
    select(EI.Sector, SCC) %>% unique
coal.scc <- coal$SCC

png('plot4.png', width = 480, height = 480)
sumPm25 %>% filter(SCC %in% coal.scc) %>%
    group_by(year) %>% select(year, Emissions) %>%
    summarise_each(funs(mean(., na.rm = T))) %>%
    with(., plot(year, Emissions, type = 'o', ylab = 'Avg. Particulate Matter',
                 xlab = 'Year', 
                 main = 'Change in Average PM25 Levels From Coal Combustion (1999-2008)'))
dev.off()
