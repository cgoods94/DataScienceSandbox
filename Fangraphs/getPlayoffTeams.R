#################################################################################
## Connor Gooding
## August 30, 2017
## http://github.com/cgoods94/DataScienceSandbox
#######################################
##
## This script takes a two-column data.frame, playoff_table, with two columns and 
## converts it into a table giving just the season, team name (no city), and a 
## combined string of those two values.
#################################################################################

getPlayoffTeams <- function(playoff_table)
{
    names(playoff_table) <- c("Season", "Team")
    
    playoff_table$Season <- as.numeric(as.character(playoff_table$Season))
    
    playoff_table$Team <- as.character(playoff_table$Team)
    
    playoff_table <- playoff_table[!(playoff_table$Season %in% 
                                         c(1904, 1994, format(Sys.Date(), "%Y"))),]

    
    playoff_table$Team <- gsub('[^a-zA-Z]', '', playoff_table$Team)
    playoff_table$Team <- sub('[A-Z]{1,2}$', '', playoff_table$Team)
    playoff_table$Team <- sub('New|St|San|Los|Kansas|Tampa', '', playoff_table$Team)
    playoff_table$Team <- sub('^[A-Z][a-z]*', '', playoff_table$Team)
    playoff_table$Team <- sub('([a-z])([A-Z])', '\\1 \\2', playoff_table$Team, perl = T)
    playoff_table$Team <- sub('of Anaheim(Wc)?$', '', playoff_table$Team)
    
    playoff_table$SeasonTeam <- paste(playoff_table$Season, playoff_table$Team)
    
    playoff_table
}
