
rankhospital <- function(state, outcome, num) {
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
    # ensuring that only hospitals with mortality rates for the ailment
    # are considered
    complete.subDf <- subDf[complete.cases(subDf) == TRUE,]
    
    # split df into state DFs
    states <- split(complete.subDf, complete.subDf$state)
    
    # access state Df selected, rank rates, and return results based on num input
    stateDf <- states[[state]]
    stateDf <- stateDf[order(stateDf[,3],stateDf[,1]),]
    stateDf$rank <- 1:nrow(stateDf)
    
    if (num == 'best'){
        stateDf$hospital[stateDf$rank == 1]
    }else if (num == 'worst'){
        stateDf$hospital[stateDf$rank == nrow(stateDf)]
    }else if (num > nrow(stateDf)){
        NA
    }else {
        stateDf$hospital[stateDf$rank == num]
    }
}
