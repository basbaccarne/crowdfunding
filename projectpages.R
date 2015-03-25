## this function returns all project pages + the platform
## execution writes a csv to the working dir
## store in var if you want to work with the R object

projectpages <- function () {
## needs R package "ScrapeR"
require(scrapeR)
require (jsonlite)

## defining the final dataframe
projects <- data.frame()
  
## read overview of platforms from github csv
platforms <- read.csv("https://raw.githubusercontent.com/basbaccarne/crowdfunding/master/platforms.csv", header=TRUE, sep=",")

## loop through platforms
# 2DO: error handling
# 2DO: simplify with vars
for(i in 1:nrow(platforms)){

## platform: [1] citizinvestor
        
        if (toString(platforms[i, 1])=="citizinvestor"){
                pagesource <- "http://www.citizinvestor.com/projects/get/projects/status_nearby/all/all/"
                pagesource.json <- fromJSON(pagesource)
                links.df <- (pagesource.json$project[6])
                links <- as.vector(unlist(links.df))
                links.complete <- paste("http://www.citizinvestor.com/project/", links, sep = "")
                projects.add <- data.frame(platform = "citizinvestor", url = links.complete)
                projects <- rbind (projects, projects.add)
        
        # Status: operational -- more metadata in json file
        
## platform: [2] voorjebuurt

      } else if (toString(platforms[i, 1])=="voorjebuurt"){
            platformurl <- toString("https://www.voorjebuurt.nl/campaigns/")    
            ## first page
            pagesource <- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//h3//a/@href"))
            projects.add <- data.frame(platform = "voorjebuurt", url = links)
            projects <- rbind (projects, projects.add)
            ## loop trough pages (N = 9)
            platformurl.pages <- paste(platformurl, "page/", c(2:9), sep ="")
            for (j in platformurl.pages) {
              pagesource <- readLines(j)
              pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
              links <- as.vector(xpathSApply(pagesource.raw, "//h3//a/@href"))
              projects.add <- data.frame(platform = "voorjebuurt", url = links)
              projects <- rbind (projects, projects.add)
            }
            
            # Status: operational

## platform: [3] growfunding

      } else if (toString(platforms[i, 1])=="growfunding"){
            platformurl <- toString("https://www.growfunding.be/bxl/project/archive") 
            pagesource <- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//a[@class='project_link btn btn-danger']/@href"))
            links.complete <- paste("http://www.growfunding.be", links, sep = "")
            projects.add <- data.frame(platform = "growfunding", url = links.complete)
            projects <- rbind (projects, projects.add)
            
            # Status: operational
            
## platform: [4] neighbour.ly

      } else if (toString(platforms[i, 1])=="neighbourly"){
            platformurl <- toString("https://www.neighbourly.com/projects") 
            pagesource<- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//a[@data-external='true']/@href"))
            links.complete <- paste("https://www.neighbourly.com", links, sep = "")
            projects.add <- data.frame(platform = "neighbourly", url = links.complete)
            projects <- rbind (projects, projects.add)
            
            ## Status: in progress: only 29 project urls are scraped // this is not everything
            
## platform: [5] spacehive

      } else if (toString(platforms[i, 1])=="spacehive"){
            platformurl <- "https://spacehive.com/ProjectSearch/"
            pagesource<- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//div[@class = 'image-container']/a/@href"))
            links.complete <- paste("https://spacehive.com/", links, sep = "")
            projects.add <- data.frame(platform = "spacehive", url = links.complete)
            projects <- rbind (projects, projects.add)
            
            ## Status: only page one

## platform: [6] IOBY

      } else if (toString(platforms[i, 1])=="ioby"){
            platformurl <- "https://www.ioby.org/projects?phrase=&city=&province=&status=1&vols=All&sort_by=title&sort_order=ASC&items_per_page=All"
            pagesource<- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//h3/a/@href"))
            links.complete <- paste("https://www.ioby.org", links, sep = "")
            projects.add <- data.frame(platform = "ioby", url = links.complete)
            projects <- rbind (projects, projects.add)
        
            ## Status: operational
            
## platform: [7] geeferom

      } else if (toString(platforms[i, 1])=="geeferom"){
              platformurl <- "http://www.geeferom.nl/alle-projecten"
              pagesource<- readLines(platformurl)
              pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
              links <- as.vector(xpathSApply(pagesource.raw, "//div[@class = 'project-titel']/a/@href"))
              links.complete <- paste("http://www.geeferom.nl", links, sep = "")
              projects.add <- data.frame(platform = "geeferom", url = links.complete)
              projects <- rbind (projects, projects.add)
                
              ## Status: operational

## platform: [8] hkb

      } else if (toString(platforms[i, 1])=="hkb"){
              platformurl <- "http://www.hkbu.nl/read/projecten"
              pagesource<- readLines(platformurl)
              pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
              links <- as.vector(xpathSApply(pagesource.raw, "//div[@class = 'start_project_title']/a/@href"))
              links.complete <- paste("http://www.hkbu.nl", links, sep = "")
              projects.add <- data.frame(platform = "hkb", url = links.complete)
              projects <- rbind (projects, projects.add)
              
              ## Status: operational
              
## platform: [9] zcfp

      } else if (toString(platforms[i, 1])=="zcfp"){
              platformurl <- "http://www.zcfp.nl/project/default?lang=nl"
              pagesource<- readLines(platformurl)
              pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
              links <- as.vector(xpathSApply(pagesource.raw, "//div[@class = 'project-linkbutton']/a/@href"))
              links.complete <- paste("http://www.zcfp.nl", links, sep = "")
              projects.add <- data.frame(platform = "zcfp", url = links.complete)
              projects <- rbind (projects, projects.add)
              
              ## Status: operational

## platform: [10] dordrechtvanstart

      } else if (toString(platforms[i, 1])=="dordrechtvanstart"){
              platformurl <- "http://www.dordrechtvanstart.nl/help-starten/?p="
              platformurl.pages <- paste(platformurl, c(1:2), sep ="")
              for (j in platformurl.pages) {
                      pagesource <- readLines(j)
                      pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                      links <- as.vector(xpathSApply(pagesource.raw, "//a[@class = 'tPurple']/@href"))
                      links.complete <- paste("www.dordrechtvanstart.nl", links, sep = "")
                      projects.add <- data.frame(platform = "dordrechtvanstart", url = links.complete)
                      projects <- rbind (projects, projects.add)
              }
              
              ## Status: operational            
            
## platform: [11] maakcapelle

      } else if (toString(platforms[i, 1])=="maakcapelle"){
              platformurl <- "http://www.maakcapelle.nl/activities.php?p12="
              platformurl.pages <- paste(platformurl, c(1:2), '&s12=DateEvent&d12=DESC&r12=9&q12=&d0=all&d1=date', sep ="")
              for (j in platformurl.pages) {
                      pagesource <- readLines(j)
                      pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                      links <- as.vector(xpathSApply(pagesource.raw, "//div[@class = 'boxWrapper fixedWidth']/@onclick"))
                      patterns <- c("window.location = '", "';")
                      links <- gsub(patterns[1], "", links)
                      links <- gsub(patterns[2], "", links)
                      links.complete <- paste("http://www.maakcapelle.nl", links, sep = "")
                      projects.add <- data.frame(platform = "maakcapelle", url = links.complete)
                      projects <- rbind (projects, projects.add)
              }
              
              ## Status: operational  
              
## platform: [12] onepercentclub



              ## Status: To process

## platform: [13] urbankit



             ## Status: To process

## platform: [14] goteo



             ## Status: To process

## platform: [15] communityfunded

      } else if (toString(platforms[i, 1])=="communityfunded"){
              platformurl <- "http://communityfunded.com/explore-projects/page/"
              platformurl.pages <- paste(platformurl, c(1:2), "/?pfilter=100&aux&t", sep ="")
              platformurl.pages <- c(platformurl.pages, paste(platformurl, c(1:6), "/?pfilter=100&aux=success&t", sep =""))
              for (j in platformurl.pages) {
                      pagesource <- readLines(j)
                      pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                      links <- as.vector(xpathSApply(pagesource.raw, "//a[@class = 'project-card-link']/@href"))
                      projects.add <- data.frame(platform = "communityfunded", url = links)
                      projects <- rbind (projects, projects.add)
              }
              
              ## Status: operational

## platform: [16] uruut

      } else if (toString(platforms[i, 1])=="uruut"){
              platformurl <- "https://www.uruut.com/browse/projects?"
              platformurl.pages <- paste(platformurl, "successful=", c(1:2), sep ="")
              platformurl.pages <- c(platformurl.pages, paste(platformurl, "featured=", c(1:6), sep =""))
              doublepages <- vector()
              for (j in platformurl.pages) {
                      pagesource <- readLines(j)
                      pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                      links <- as.vector(xpathSApply(pagesource.raw, "//h3//a/@href"))
                      doublepages <- c(doublepages, links)
              }
              links.complete <- paste("https://www.uruut.com", unique(doublepages), sep = "")
              projects.add <- data.frame(platform = "uruut", url = links.complete)
              projects <- rbind (projects, projects.add)
              
              ## Status: operational

## platform: [17] smallknot

      } else if (toString(platforms[i, 1])=="smallknot"){
              platformurl <- "http://smallknot.com/browse/campaigns"
              pagesource<- readLines(platformurl)
              pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
              links <- as.vector(xpathSApply(pagesource.raw, "//div[@class = 'details']/a/@href"))
              projects.add <- data.frame(platform = "smallknot", url = links)
              projects <- rbind (projects, projects.add)
              
              ## Status: operational

## platform: [18] ideaginger
              
              
              
              ## Status: To process

## platform: [19] neighborly



             ## Status: To process

## platform: [20] catarse



             ## Status: To process      

## platform: [21] kickstarter



             ## Status: To process   

## platform: [22] townmeet



             ## Status: To process   

## platform: [23] zenfunder



            ## Status: To process  

## other platforms

      } else {
            print (toString(platforms[i, 1]))
      }
}


## // bottle the data//

projects <- cbind (projects, measured.on = Sys.Date())          ## add timestamp 
write.csv(projects, file = "projects.csv", quote = FALSE)       ## export dataframe to file 

projects

}
