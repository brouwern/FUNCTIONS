get.git.csv <- function(URL){
  temp <- getURL(URL)
  temp <- read.csv(text = temp)
  return(temp)
}