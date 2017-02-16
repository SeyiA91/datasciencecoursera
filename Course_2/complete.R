
complete <- function(directory, id = 1:332) {
    path <- '~/dev/datasciencecoursera/Course_2/'
    setwd(paste(path, directory, sep = ''))
    
    vector.length = length(id)
    df.ids <- as.character()
    nobs <- as.integer()
    if (vector.length > 1){
        for (i in 1:vector.length){
            file.number <- id[i]
            if (file.number < 10){
                file.number <- paste('00', file.number, sep = '')
            }else if(file.number < 100){
                file.number <- paste('0', file.number, sep = '')
            }
            doc <- paste(file.number, '.csv', sep = '')
            if (file.exists(doc)){
                df <- read.csv(doc, header = T)
                df.ids <- c(df.ids, id[i])
                complete <- length(which(complete.cases(df) == TRUE))
                nobs <- c(nobs, complete)
            }
        }
        finalDf <- data.frame(id = df.ids, nobs = nobs)
        print(finalDf)
    }else {
        file.number <- id
        if (file.number < 10){
            file.number <- paste('00', file.number, sep = '')
        }else if(file.number < 100){
            file.number <- paste('0', file.number, sep = '')
        }
        doc <- paste(file.number, '.csv', sep = '')
        complete <- 0
        if (file.exists(doc)){
            df <- read.csv(doc, header = T)
            df.ids <- c(df.ids, id)
            complete <- length(which(complete.cases(df) == TRUE))
            finalDf <- data.frame(id = df.ids, nobs = complete)
            print(finalDf)
        }else {
            print('The file does not exist.')
        }
    }
}
