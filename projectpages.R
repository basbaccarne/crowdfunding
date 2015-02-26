## needs R package "ScrapeR"
library (scrapeR)

## read overview of platforms
platforms <- read.csv("https://github.com/basbaccarne/crowdfunding/blob/master/platforms.csv", header=TRUE, sep=";")

## loop through platforms + error handling
for(i in 1:nrow(platforms)){
  tryCatch({
    
## voorjebuurt
      if (toString(platforms[i, 1])=="voorjebuurt"){
            platformurl <- toString(platforms[i, 3])    
            ##testing in -  enkel voor je buurt##
            platformurl = toString(platforms[2,3])
            ##testing out - enkel voor je buurt##
            pagesource <- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//h3//a/@href"))
      
      } else {
            print (toString(platforms[i, 1]))
      }
  })
}