
rankall <- function(outcome, num = 'best') {
    # read outcome data
    path <- '~/dev/datasciencecoursera/ProgrammingAssignment3/'
    outcomeDf <- read.csv(paste(path,'outcome-of-care-measures.csv', sep = ''),
                          header = T,colClasses = 'character')
    
    ## check that state and outcome are valid entries
    outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    if (!isTRUE(is.element(outcome, outcomes))){
        stop('invalid outcome')
    }
    
    ## return hospital name in that state with given rank & death rate
    mortality.col.inx <- NULL
    if (outcome == 'heart attack'){
        mortality.col.inx <- 11
    }else if (outcome == 'heart failure') {
        mortality.col.inx <- 17
    }else {
        mortality.col.inx <- 23
    }
    
    subDf <- outcomeDf[,c(2,7,mortality.col.inx)]
    names(subDf) <- c('hospital', 'state', 'outcome')
    subDf[,3] <- as.numeric(subDf[,3])
    complete.subDf <- subDf[complete.cases(subDf) == TRUE,]
    # ordering entire df by hospital  name
    complete.subDf <- complete.subDf[order(complete.subDf[,1]),]
    # splitting df by state
    states <- split(complete.subDf, complete.subDf$state)
    # applying ranks to each hospital within each state
    state_ranks <- lapply(states, function(x) 
    {x$rank <- rank(x$outcome, ties.method = 'first'); x})
    
    hospital_names <- as.character()
    state_abr <- as.character()
    for (i in 1:length(state_ranks)){
        if (num == 'best'){
            row <- state_ranks[[i]][state_ranks[[i]]$rank == 1,]
            hospital_names <- c(hospital_names, row[,1])
            state_abr <- c(state_abr, row[,2])
        }else if (num == 'worst'){
            row <- state_ranks[[i]][state_ranks[[i]]$rank == nrow(state_ranks[[i]]),]
            hospital_names <- c(hospital_names, row[,1])
            state_abr <- c(state_abr, row[,2])
        }else if (num > nrow(state_ranks[[i]])){
            NA
        }else {
            row <- state_ranks[[i]][state_ranks[[i]]$rank == num,]
            hospital_names <- c(hospital_names, row[,1])
            state_abr <- c(state_abr, row[,2])
        }
    }
    finalDf <- data.frame(hospital = hospital_names, state = state_abr)
}
