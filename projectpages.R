## needs R package "ScrapeR"
install.packages("scrapeR")
library (scrapeR)

## defining the final dataframe
projects <- data.frame()
  
## read overview of platforms from github csv
platforms <- read.csv("https://raw.githubusercontent.com/basbaccarne/crowdfunding/master/platforms.csv", header=TRUE, sep=",")

## loop through platforms
# 2DO: error handling
for(i in 1:nrow(platforms)){

## platform: voorjebuurt
      if (toString(platforms[i, 1])=="voorjebuurt"){
            platformurl <- toString(platforms[i, 3])    
            pagesource <- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//h3//a/@href"))
            projects.add <- data.frame(platform = "voorjebuurt", url = links)
            projects <- rbind (projects, projects.add)
            
            # 2DO: loop through pages

## platform: citizinvestor
      } else if (toString(platforms[i, 1])=="citizinvestor"){
            platformurl <- toString(platforms[i, 3]) 
            pagesource <- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//div[@class='project type-project']/@href"))
        
            # 2DO: fix challenge: javacontent

## platform: growfunding
      } else if (toString(platforms[i, 1])=="growfunding"){
            platformurl <- toString(platforms[i, 3]) 
            pagesource <- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//a[@class='project_link btn btn-danger']/@href"))
            links.complete <- paste("http://www.growfunding.be", links, sep = "")
            projects.add <- data.frame(platform = "growfunding", url = links.complete)
            projects <- rbind (projects, projects.add)
            
        
## other platforms
      } else {
            print (toString(platforms[i, 1]))
      }
}

## export dataframe to file
write.csv(projects, file = "projects.csv", quote = FALSE)