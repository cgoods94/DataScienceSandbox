getDSLosers <- function(url)
{
    r <- GET(url)
    
    wiki <- readHTMLTable(doc=content(r, "text"))
    ds_losers_raw <- wiki[[3]]
    ds_losers_raw <- data.frame(lapply(ds_losers_raw, as.character), stringsAsFactors=FALSE)
    
    width <- dim(ds_losers_raw)[2]
    height <- dim(ds_losers_raw)[1]
    
    ds_losers <- ds_losers_raw
    ds_losers[seq(2, height-1, 2),2:width] <- ds_losers[seq(2, height-1, 2),1:(width-1)]
    ds_losers[seq(2, height-1, 2),1] <- ds_losers[seq(1, height-2, 2), 1]
    ds_losers <- ds_losers[1:height-1,c(1,3)]
    ds_losers <- getPlayoffTeams(ds_losers)
}