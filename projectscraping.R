## needs R package "ScrapeR"
require(scrapeR)
require (jsonlite)

## defining the final dataframe
dataset <- data.frame(
  timestamp = numeric(0),
  platform = character(0), 
  projectname = character(0),
  pledged = numeric(0),
  received = numeric(0),
  facebookurl = character(0),
  commentcount = character(0),
  backercount = character(0),
  pitch = character(0))
  
## read overview of platforms from github csv
projects <- read.csv("projects.csv", header=TRUE, sep=",")

## loop through projects

for(i in 1:nrow(projects)){

  ## IOBY
  
  if (toString(projects[i, 2])=="ioby"){
        

        
        ## loop through pages
            ## for j in projects]
            projecturl <- projects[i, 3]
            pagesource <- readLines(projecturl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            
          
      }
  
}


## export dataframe to file
write.csv(projects, file = "dataset.csv", quote = FALSE)