
best <- function(state, outcome) {
    # read outcome data
    path <- '~/dev/datasciencecoursera/ProgrammingAssignment3/'
    outcomeDf <- read.csv(paste(path,'outcome-of-care-measures.csv', sep = ''),
                          header = T,colClasses = 'character')
    ## check that state and outcome are valid entries
    outcomes <- c('heart attack', 'heart failure', 'pneumonia')
    if (!isTRUE(is.element(state, outcomeDf$State))) {
        stop('invalid state')
    }
    if (!isTRUE(is.element(outcome, outcomes))){
        stop('invalid outcome')
    }
    
    # subset df with only hospital names, mortality rates, state
    
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
    # ensuring that only hospitals with mortality rates for the ailment
    # are considered
    complete.subDf <- subDf[subDf$state == state & complete.cases(subDf) == TRUE,]
    
    ## return hospital name in that state with lowest 30-day death rate
    orderedDf = complete.subDf[order(complete.subDf[,3],complete.subDf[,1]),]
    # head(orderedDf)
    orderedDf[1,1]
}


