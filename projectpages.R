## needs R package "ScrapeR"
install.packages("scrapeR")
library (scrapeR)

## defining the dataframe
projects <- data.frame()
#2DO: define dataframe
  
## read overview of platforms from github csv
platforms <- read.csv("https://raw.githubusercontent.com/basbaccarne/crowdfunding/master/platforms.csv", header=TRUE, sep=",")
#2DO: error handling

## loop through platforms
for(i in 1:nrow(platforms)){

## voorjebuurt
      if (toString(platforms[i, 1])=="voorjebuurt"){
            platformurl <- toString(platforms[i, 3])    
                ##testing in -  enkel voor je buurt## 
                  ##platformurl = toString(platforms[2,3])
                ##testing out - enkel voor je buurt##
            pagesource <- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//h3//a/@href"))
            ##2DO: data can be read better with other than readLines (cfr Curl)
            ##2DO: add to dataframe with var "voorjebuurt"

## other platforms
      } else {
            print (toString(platforms[i, 1]))
      }
}

#2DO: once dataframe complete: export to file