
corr <- function(directory, threshold = 0) {
    path <- '~/dev/datasciencecoursera/Course_2/'
    setwd(paste(path, directory, sep = ''))
    
    target.files <- list.files()
    corr.vec <- numeric()
    for (i in 1:length(target.files)) {
        df <- read.csv(target.files[i], header = TRUE)
        nobs <- length(which(complete.cases(df) == TRUE))
        if (nobs > threshold){
            corr.vec <- c(corr.vec, cor(df$sulfate, df$nitrate, use = 'complete.obs'))
        }
    }
    corr.vec
}
