

pollutantmean <- function(directory, pollutant, id = 1:332){
    path <- '~/dev/datasciencecoursera/Course_2/'
    setwd(paste(path, directory, sep = ''))
    
    values <- numeric()
    vector.length <- length(id)
    
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
                df <- read.csv(doc, header = TRUE)
                col.index <- grep(pollutant, colnames(df))
                for (i in 1:nrow(df)){
                    if (!is.na(df[col.index][i,])){
                        values <- c(values, df[col.index][i,])
                    }
                }
            }
        }
        avg <- mean(values)
        print(paste('The mean is:', avg, sep = ' '))
    }else{
        file.number <- id
        if (file.number < 10){
            file.number <- paste('00', file.number, sep = '')
        }else if(file.number < 100){
            file.number <- paste('0', file.number, sep = '')
        }
        doc <- paste(file.number, '.csv', sep = '')
        if (file.exists(doc)){
            df <- read.csv(doc, header = TRUE)
            col.index <- grep(pollutant, colnames(df))
            for (i in 1:nrow(df)){
                if (!is.na(df[col.index][i,])){
                    values <- c(values, df[col.index][i,])
                }
            }
            avg <- mean(values)
            print(paste('The mean is:', avg, sep = ' '))
        }else{
            print('The file does not exist.')
        }
    }
}

