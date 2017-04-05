rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        dat <-
                read.csv(
                        "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv",
                        colClasses = "character"
                )
        
        ## Check that state and outcome are valid
        states <- levels(as.factor(dat$State))
        
        if (!(state %in% states)) {
                stop(message("invalid state"))
        }
        
        dat[, 11] <- as.numeric(dat[, 11])
        dat[, 17] <- as.numeric(dat[, 17])
        dat[, 23] <- as.numeric(dat[, 23])
        
        ##split the frame into list of frames by state
        dat_st <- split(dat, dat$State)
        
        if (outcome == "heart attack") {
                ##find the hospital
                ##m <- sort(dat_st[[state]][, 11]) ##, na.last = T)
                
                if(num == "best"){
                        m <- order(dat_st[[state]][,11],dat_st[[state]]$Hospital.Name)
                        return(dat_st[[state]][m,]$Hospital.Name[1])        
                }
                if(num == "worst"){
                        w <- order(dat_st[[state]][,11],dat_st[[state]]$Hospital.Name,decreasing = T)
                        return(dat_st[[state]][w,]$Hospital.Name[1])        
                }
                if(is.numeric(num)){
                        m <- order(dat_st[[state]][,11],dat_st[[state]]$Hospital.Name)
                        return(dat_st[[state]][m,]$Hospital.Name[num])        
                }
                
        }
        
        if (outcome == "heart failure") {
                if(num == "best"){
                        m <- order(dat_st[[state]][,17],dat_st[[state]]$Hospital.Name)
                        return(dat_st[[state]][m,]$Hospital.Name[1])        
                }
                if(num == "worst"){
                        m <- order(dat_st[[state]][,17],dat_st[[state]]$Hospital.Name,decreasing = T)
                        return(dat_st[[state]][m,]$Hospital.Name[1])        
                }
                if(is.numeric(num)){
                        m <- order(dat_st[[state]][,17],dat_st[[state]]$Hospital.Name)
                        return(dat_st[[state]][m,]$Hospital.Name[num])        
                }
        }
        
        if (outcome == "pneumonia") {
                if(num == "best"){
                        m <- order(dat_st[[state]][,23],dat_st[[state]]$Hospital.Name)
                        return(dat_st[[state]][m,]$Hospital.Name[1])        
                }
                if(num == "worst"){
                        m <- order(dat_st[[state]][,23],dat_st[[state]]$Hospital.Name,decreasing = T)
                        return(dat_st[[state]][m,]$Hospital.Name[1])        
                }
                if(is.numeric(num)){
                        m <- order(dat_st[[state]][,23],dat_st[[state]]$Hospital.Name)
                        return(dat_st[[state]][m,]$Hospital.Name[num])        
                }
        }
        
        stop("invalid outcome")
}