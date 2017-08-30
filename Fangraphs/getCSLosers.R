getCSLosers <- function(url)
{
    r <- GET(url)
    
    wiki <- readHTMLTable(doc=content(r, "text"))
    cs_losers1 <- wiki[[4]][,c(1, 7)]
    cs_losers1$Year <- sub('\\[.\\]', '', cs_losers1$Year)
    cs_losers1 <- getPlayoffTeams(cs_losers1)
    
    cs_losers2 <- wiki[[5]][,c(1, 7)]
    cs_losers2 <- getPlayoffTeams(cs_losers2)
    
    rbind(cs_losers1, cs_losers2)
}