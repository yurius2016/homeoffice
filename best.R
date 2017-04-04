best <- function(state, outcome) {
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
        data_on_states <- split(dat, dat$State)
        
        if (outcome == "heart attack") {
                ##find the hospital
                m <- min(data_on_states[[state]][, 11], na.rm = T)
                hosp <- which(data_on_states[[state]][, 11] == m)
                hosp <- sort(hosp)
                
                return(data_on_states[[state]][hosp[1], ]$Hospital.Name)
                
        }
        
        if (outcome == "heart failure") {
                m <- min(data_on_states[[state]][, 17], na.rm = T)
                hosp <- which(data_on_states[[state]][, 17] == m)
                hosp <- sort(hosp)
                
                #message(m)
                
                return(data_on_states[[state]][hosp[1], ]$Hospital.Name)
        }
        
        if (outcome == "pneumonia") {
                m <- min(data_on_states[[state]][, 23], na.rm = T)
                hosp <- which(data_on_states[[state]][, 23] == m)
                hosp <- sort(hosp)
                
                return(data_on_states[[state]][hosp[1],]$Hospital.Name)
                
        }
        
        ##else{
        stop("invalid outcome")
        ##}
        ## Return hospital name in that state with lowest 30-day death
        
        ## rate
}