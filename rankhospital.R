rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        dat <-
                read.csv(
                        "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                        colClasses = "character"
                )
        dat[, 11] <- as.numeric(dat[, 11])
        dat[, 17] <- as.numeric(dat[, 17])
        dat[, 23] <- as.numeric(dat[, 23])
        
        ## Check that state and outcome are valid
        states <- levels(as.factor(dat$State))
        
        if (!(state %in% states)) {
                stop(message("invalid state"))
        }
        ##split the frame into list of frames by state
        dat_st <- split(dat, dat$State)
        
        if (outcome == "heart attack") {
                ##find the hospital
                m <- sort(dat_st[[state]][, 11]) ##, na.last = T)
                hosp <- which(dat_st[[state]][, 11] == m)
                
                return(dat_st[[state]][hosp[1],]$Hospital.Name)
                
        }
        
        if (outcome == "heart failure") {
                m <- min(dat_st[[state]][, 17], na.rm = T)
                hosp <- which(dat_st[[state]][, 17] == m)
                hosp <- sort(hosp)
                
                return(dat_st[[state]][hosp[1],]$Hospital.Name)
        }
        
        if (outcome == "pneumonia") {
                m <- min(dat_st[[state]][, 23], na.rm = T)
                hosp <- which(dat_st[[state]][, 23] == m)
                hosp <- sort(hosp)
                
                return(dat_st[[state]][hosp[1], ]$Hospital.Name)
        }
        
        ##else{
        stop("invalid outcome")
        ##}
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}