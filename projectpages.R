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
        
        if (platforms[i, 1]=="citizinvestor"){
                pagesource <- "http://www.citizinvestor.com/projects/get/projects/status_nearby/all/all/"
                pagesource.json <- fromJSON(pagesource)
                links.df <- (pagesource.json$project[6])
                links <- as.vector(unlist(links.df))
                links.complete <- paste("http://www.citizinvestor.com/project/", links, sep = "")
                projects.add <- data.frame(platform = "citizinvestor", url = links.complete)
                projects <- rbind (projects, projects.add)
        
                cat ("added citizinvestor to cache: ", length(projects$platform[projects$platform=="citizinvestor"]))
                
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
            
            cat ("added voorjebuurt to cache: ", length(projects$platform[projects$platform=="voorjebuurt"]))
            
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
            
            cat ("added growfunding to cache: ", length(projects$platform[projects$platform=="growfunding"]))
            
            # Status: operational
            
## platform: [4] neighbourly

      } else if (toString(platforms[i, 1])=="neighbourly"){
              pagesource <- "https://www.neighbourly.com/api/projects/?page=1&pageSize=1500&filter=&orderBy=id+desc"
              pagesource.json <- fromJSON(pagesource)
              links.df <- (pagesource.json[,1])
              links <- as.vector(unlist(links.df))
              links.complete <- paste("https://www.neighbourly.com/projects/", links, sep = "")
              projects.add <- data.frame(platform = "neighbourly", url = links.complete)
              projects <- rbind (projects, projects.add)
              
              cat ("added neighbourly to cache: ", length(projects$platform[projects$platform=="neighbourly"]))
              
              # Status: operational -- more metadata in json file
            
## platform: [5] spacehive

      } else if (toString(platforms[i, 1])=="spacehive"){
              platformurl <- "http://spacehive.com/ProjectSearch#Concept=false&Design=false&Verified=false&Fundraising=false&Complete=false&Sport+%26+Play=false&Green+Space=false&Art+%26+Performance=false&Civic+Space=false&Food+%26+Farms=false&Infrastructure=false&Something+Different=false&orderBy=&page="
              platformurl.pages <- paste(platformurl, c(1:89), sep ="")
              for (j in platformurl.pages) {
                      pagesource<- scrape(j)
                      links <- as.vector(xpathSApply(pagesource[[1]], "//div[@class = 'image-container']/a/@href"))
                      links.complete <- paste("https://spacehive.com", links, sep = "")
                      projects.add <- data.frame(platform = "spacehive", url = links.complete)
                      projects <- rbind (projects, projects.add)
              }
              
              cat ("added spacehive to cache: ", length(projects$platform[projects$platform=="spacehive"]))
            
            ## Status: operational

## platform: [6] IOBY

      } else if (toString(platforms[i, 1])=="ioby"){
            platformurl <- "https://www.ioby.org/projects?phrase=&city=&province=&status=1&vols=All&sort_by=title&sort_order=ASC&items_per_page=All"
            pagesource<- readLines(platformurl)
            pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
            links <- as.vector(xpathSApply(pagesource.raw, "//h3/a/@href"))
            links.complete <- paste("https://www.ioby.org", links, sep = "")
            projects.add <- data.frame(platform = "ioby", url = links.complete)
            projects <- rbind (projects, projects.add)
        
            cat ("added IOBY to cache: ", length(projects$platform[projects$platform=="ioby"]))
            
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
                
              cat ("added geeferom to cache: ", length(projects$platform[projects$platform=="geeferom"]))
              
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
              
              cat ("added hkb to cache: ", length(projects$platform[projects$platform=="hkb"]))
              
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
              
              cat ("added zcfp to cache: ", length(projects$platform[projects$platform=="zcfp"]))
              
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
              
              cat ("added dordrechtvanstart to cache: ", length(projects$platform[projects$platform=="dordrechtvanstart"]))
              
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
              
              cat ("added maakcapelle to cache: ", length(projects$platform[projects$platform=="maakcapelle"]))
              
              ## Status: operational  
              
## platform: [12] onepercentclub

      } else if (toString(platforms[i, 1])=="onepercentclub"){
                platformurl <- "https://onepercentclub.com/api/bb_projects/previews/?page="
                platformurl.pages <- paste(platformurl, c(1:9), "&page_size=100&format=json", sep ="")
                
                for (j in platformurl.pages) {
                        pagesource.json <- fromJSON(j)
                        links <- (pagesource.json$results$id)
                        links.complete <- paste("https://onepercentclub.com/en/#!/projects/", links, sep = "")
                        projects.add <- data.frame(platform = "onepercentclub", url = links.complete)
                        projects <- rbind (projects, projects.add)
                }
        
                cat ("added onepercentclub to cache: ", length(projects$platform[projects$platform=="onepercentclub"]))
                
                # Status: operational -- more metadata in json file

## platform: [13] urbankit

      } else if (toString(platforms[i, 1])=="urbankit"){
              print ("urbankit was not imported. Reason: platform went offline")
              
              ## Status: platform offline

## platform: [14] goteo
      } else if (toString(platforms[i, 1])=="goteo"){
                platformurl <- "http://www.goteo.org/discover/view/success?page="
                platformurl.pages <- paste(platformurl, c(1:40), sep ="")
                for (j in platformurl.pages) {
                        pagesource <- readLines(j)
                        pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                        links <- as.vector(xpathSApply(pagesource.raw, "//h3[contains(@class, 'title')]/a/@href"))
                        links.complete <- paste("http://www.goteo.org", links, sep = "")
                        projects.add <- data.frame(platform = "goteo", url = links.complete)
                        projects <- rbind (projects, projects.add)
                }

                cat ("added goteo to cache: ", length(projects$platform[projects$platform=="goteo"]))
                
                ## Status: operational

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
              
              cat ("added communityfunded to cache: ", length(projects$platform[projects$platform=="communityfunded"]))
              
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
              
              cat ("added uruut to cache: ", length(projects$platform[projects$platform=="uruut"]))
              
              ## Status: operational

## platform: [17] smallknot

      } else if (toString(platforms[i, 1])=="smallknot"){
              platformurl <- "http://smallknot.com/browse/campaigns"
              pagesource<- readLines(platformurl)
              pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
              links <- as.vector(xpathSApply(pagesource.raw, "//div[@class = 'details']/a/@href"))
              projects.add <- data.frame(platform = "smallknot", url = links)
              projects <- rbind (projects, projects.add)
              
              cat ("added smallknot to cache: ", length(projects$platform[projects$platform=="smallknot"]))
              
              ## Status: operational

## platform: [18] ideaginger

      } else if (toString(platforms[i, 1])=="ideaginger"){
              platformurl <- "http://www.ideaginger.it/progetti.html"
              pagesource<- readLines(platformurl)
              pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
              links <- as.vector(xpathSApply(pagesource.raw, "//div[@class='titolo_prog_ns']/*/a/@href"))
              links.complete <- paste("http:/www.ideaginger.it", links, sep = "")
              projects.add <- data.frame(platform = "ideaginger", url = links.complete)
              projects <- rbind (projects, projects.add)
              
              cat ("added ideaginger to cache: ", length(projects$platform[projects$platform=="ideaginger"]))
              
              ## Status: operational

## platform: [19] neighborly

        } else if (toString(platforms[i, 1])=="neighborly"){
                pagesource <- "https://neighborly.com/discover.json"
                pagesource.json <- fromJSON(pagesource)
                links.df <- (pagesource.json$deal_profile$deals$html_url)
                links <- as.vector(unlist(links.df))
                projects.add <- data.frame(platform = "neighborly", url = links)
                projects <- rbind (projects, projects.add)

                cat ("added neighborly to cache: ", length(projects$platform[projects$platform=="neighborly"]))
                
                ## Status: operational

## platform: [20] catarse

        } else if (toString(platforms[i, 1])=="catarse"){
                platformurl <- "https://www.catarse.me/pt/projects?page="
                platformurl.pages <- paste(platformurl, c(1:30), sep ="")
                doubles <- NULL
                for (j in platformurl.pages) {
                        pagesource <- readLines(j)
                        pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                        links <- as.vector(xpathSApply(pagesource.raw, "//a[@class = 'link-hidden']/@href"))
                        doubles <- c(doubles,links)
                }
                links <- unique(doubles)
                links.complete <- paste("https:/www.catarse.me", links, sep = "")
                projects.add <- data.frame(platform = "catarse", url = links.complete)
                projects <- rbind (projects, projects.add)

                cat ("added catarse to cache: ", length(projects$platform[projects$platform=="catarse"]))
                
                ## Status: operational   

## platform: [21] kickstarter

        } else if (toString(platforms[i, 1])=="kickstarter"){
                platformurl <- "https://www.kickstarter.com/discover/advanced?google_chrome_workaround&page="
                platformurl.pages <- paste(platformurl, c(1:6), "&category_id=259&woe_id=0&sort=magic", sep ="")
                for (j in platformurl.pages) {
                        pagesource <- readLines(j)
                        pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                        links <- as.vector(xpathSApply(pagesource.raw, "//h6/a/@href"))
                        links.complete <- paste("https://www.kickstarter.com", links[3:length(links)], sep= "")
                        projects.add <- data.frame(platform = "kickstarter", url = links.complete)
                        projects <- rbind (projects, projects.add)
                }

                cat ("added kickstarter to cache: ", length(projects$platform[projects$platform=="kickstarter"]))
                
                ## Status: operational

## platform: [22] crowdera

        } else if (toString(platforms[i, 1])=="crowdera"){
                pagesource <- "http://crowdera.co/campaigns"
                pagesource.raw <- htmlTreeParse(pagesource, useInternalNodes = T)
                links <- as.vector(xpathSApply(pagesource.raw, "//div[@class='project-title']/a/@href"))
                links.complete <- paste("http://crowdera.co", links, sep= "")
                projects.add <- data.frame(platform = "crowdera", url = links.complete)
                projects <- rbind (projects, projects.add)

                cat ("added crowdera to cache: ", length(projects$platform[projects$platform=="crowdera"]))
                
                ## Status: operational   

## platform: [23] zenfunder

        } else if (toString(platforms[i, 1])=="zenfunder"){
                print ("zenfunder was not imported. Reason: platform went offline")
                
                ## Status: platform offline

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

projects <- projectpages()
