rankall <- function(outcome, num = "best") {
        ## Read outcome data
        dat <-
                read.csv(
                        "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                        colClasses = "character"
                )
        
        ## Check that state
        states <- levels(as.factor(dat$State))
        
        if (!(state %in% states)) {
                stop(message("invalid state"))
        }
        
        dat[, 11] <- as.numeric(dat[, 11])
        dat[, 17] <- as.numeric(dat[, 17])
        dat[, 23] <- as.numeric(dat[, 23])
        
        ##split the frame into list of frames by state
        dat_st <- split(dat, dat$State)
        
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        
        
        
        stop("invalid outcome")
        
        
}