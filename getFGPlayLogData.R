####################################################################################
## This function returns a data.table of every plate appearance in every home game played
## by the specified MLB team in the specified date range. Data is retrieved
## from http://fangraphs.com/plays.aspx for every day on which a game was played by the
## specified team. Please follow Fangraphs' ToS if you use this data for anything published.
## ToS found here: http://www.fangraphs.com/tos.aspx
##
## Parameters:
## ------------------------------------------
## startDate: the beginning of the date range; REQUIRED
##
## endDate: the end of the date range. If not specified, the function will only return
## plays from the startDate specified, if there was a home game for the team on that day.
##
## team: the team to pull play data for. If not specified, it will look for plays from
## home games from the greatest team on Earth.
## ------------------------------------------
## Note: the code has not been optimized and could run slow on your machine.
## Dependencies: XML, data.table
####################################################################################

getFGPlayLogData <- function(startDate, endDate = NULL, team = "Cubs") {
    
    ## Check that start and end dates are valid
    startDate <- try(as.Date(startDate, format = "%Y-%m-%d"))
    if( class(startDate) == "try-error" || is.na(startDate) )
    {
        stop("Invalid start date. Please enter dates in format {YYYY}-{MM}-{DD}.")
    }
    
    if (!(is.null(endDate)))
    {
        endDate <- try(as.Date(endDate, format = "%Y-%m-%d"))
        if( class(endDate) == "try-error" || is.na(endDate) )
        {
            stop("Invalid end date. Please enter dates in format {YYYY}-{MM}-{DD}.")
        }
        
        if (endDate < startDate)
        {
            stop("Error: Invalid date range.")
        }
    
        ## set date range and establish play.log data table
        dateRange <- as.character(seq(startDate, endDate, by="days"))
    }
    else
    {
        dateRange <- as.character(startDate)
    }
    play.log <- data.table()
    
    ## for each date in the range...
    for (date in dateRange)
    {
        ## send the proper Web request to get that day's game
        doc <- htmlTreeParse(paste("http://www.fangraphs.com/plays.aspx?date=", 
                                   date,
                                   "&team=",
                                   team,
                                   "&dh=0&season=",
                                   format(as.Date(date, "%Y"), "%Y"),
                                   sep = ""), useInternalNodes = TRUE)
        
        ## if there was no game that day, you won't get column headers (among other things),
        ## so you can skip that day.
        if(is.null(xpathSApply(doc, "//table[@class='rgMasterTable']/thead/tr/th", xmlValue)))
        {
            next
        }
        
        ## pull the values from the td tags in rgMasterTable and shape the data
        ## into a 14-column data.table with the appropriate column headers from the page
        raw.td <- xpathSApply(doc, "//table[@class='rgMasterTable']/tbody/tr/td", xmlValue)
        game.play.log <- matrix(raw.td, ncol = 14, byrow = TRUE)
        game.play.log <- data.table(game.play.log)
        names(game.play.log) <- xpathSApply(doc, "//table[@class='rgMasterTable']/thead/tr/th", xmlValue)
        
        ## add a column for the date of the game and then bind it to the big play.log
        game.play.log <- game.play.log[,Date:=date]
        play.log <- rbind(play.log, game.play.log)
    }
    
    ## convert play.log's columns to the appropriate data types and add Team or Visitor
    if(ncol(play.log))
    {
        play.log[,c(3:4, 8:9, 11:14)] <- lapply(play.log[,c(3:4, 8:9, 11:14)], function(x) abs(as.numeric(as.character(x))))
    }
    else
    {
        play.log <- NULL
    }
    
    play.log
}
