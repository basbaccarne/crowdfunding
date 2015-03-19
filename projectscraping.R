####################### PART I: INITIATLISATION ####################### 

require(scrapeR)
require (jsonlite)

initiatlize <- function (){

        ## read overview of platforms
        projects <<- read.csv("projects.csv", header=TRUE, sep=",")
        
        ## initialising the final dataframe
        dataset <<- data.frame(
          url = character(0),
          timestamp = numeric(0),
          platform = character(0),
          lang = character(0),
          projectname = character(0),
          pledged = numeric(0),
          received = numeric(0),
          currency = character(0),
          success = logical(),
          status = character(0),
          facebookurl = character(0),
          facebooklikes = numeric(0),
          commentcount = character(0),
          backercount = character(0),
          pitch = character(0))

}
initiatlize()


#################### PART II: GETTING THE RAW DATA #####################

getRawData <- function() {

        if (file.exists("rawdata")){
                filenames <- list.files("rawdata", full.names=T)
                for (file in filenames){
                        filename <- gsub(".R", ".raw", file)
                        filename <- gsub("rawdata/", "", filename)
                        raw.data <- dget(file)
                        assign(filename, dget(file), envir=.GlobalEnv)
                }
                
                cat ("## added to global environment from cache:", filenames)
                
        } else {
                                
                # platform by platform to avoid server errors
                # heavy load!!
                
                dumpToRawdata <- function (platform){
                        fresh.dump <- data.frame(
                                url = projects$url[projects$platform == platform],
                                dump = as.character(
                                        lapply(as.character(
                                                projects$url[projects$platform == platform]), 
                                               readLines)))
                        filename <- paste("./rawdata/", platform, ".R", sep = "")
                        dput (fresh.dump, filename)  
                }    
                
        }
        
}
getRawData()


#################### PART III: DOM PARSE FUNCTIONS #################### 

voorjebuurt <- function (url, dump){
        #dump <- as.character(voorjebuurt.raw$dump[1])
        #url <- voorjebuurt.raw$url[1]
        pagesource <- htmlTreeParse(as.character(dump), useInternalNodes = T)
        
        url <- url
        timestamp <- Sys.Date()
        platform <- "voorjebuurt"
        lang <- "NL"
        projectname <- xpathSApply(pagesource, "//h1", xmlValue)[2]
        pledged <- gsub("22% van â,¬", "", xpathSApply(pagesource, "//p", xmlValue)[2])
        pledged <- as.numeric(gsub(",", ".", pledged))
        received <- gsub("â,¬", "", xpathSApply(pagesource, "//h3", xmlValue)[2])
        received <- as.numeric(gsub(",", ".", received))
        currency <- "euro"
        success <- received > pledged
        start <- NA
        end <- NA        
        status <- NA
        facebookurl <- NA
        facebooklikes <- NA
        commentcount <- paste (url, "#comments", sep ="")
        backercount <- paste (url, "#backers", sep ="")
        pitch <- NA
        
        data.frame(url, timestamp, platform, lang, projectname, pledged, received, currency,
                   success, status, facebookurl, facebooklikes, commentcount, 
                   backercount, pitch)
}
## Status: WIP (lots of NAs)

citizinvestor <- function (url, dump){
        dump <- as.character(citizinvestor.raw$dump[1])
        url <- citizinvestor.raw$url[1]
        pagesource <- htmlTreeParse(as.character(dump), useInternalNodes = T)
        
        url <- url
        timestamp <- Sys.Date()
        platform <- "citizinvestor"
        lang <- "EN"
        projectname <- xpathSApply(pagesource, "//h1", xmlValue)[2]
        pledged <- gsub ("out of \\$", "", xpathSApply(pagesource, "//span", xmlValue)[6])
        pledged <- gsub (" required", "", pledged)
        pledged <- as.numeric(gsub (",", ".", pledged))
        received <- as.numeric(gsub ("\\$", "", xpathSApply(pagesource, "//span", xmlValue)[5]))
        currency <- "usd"
        success <- received > pledged
        start <- NA
        end <- NA        
        daysleft <- as.numeric(xpathSApply(pagesource, "//span", xmlValue)[1])
        status <- NA
        if (daysleft > 0) {
                status <- "active"
        }else if (daysleft < 0) {
                status <- "completed"}
        facebookurl <- NA
        facebooklikes <- NA
        commentcount <- NA
        backercount <- as.numeric(xpathSApply(pagesource, "//span", xmlValue)[3])
        pitch <- xpathSApply(pagesource, "//p", xmlValue)[2]
        
        data.frame(url, timestamp, platform, lang, projectname, pledged, received, currency,
                   success, status, facebookurl, facebooklikes, commentcount, 
                   backercount, pitch)
}

zcfp <- function(url, dump){
        #dump <- as.character(raw.data$dump[246])
        pagesource <- htmlTreeParse(as.character(dump), useInternalNodes = T)
        
        url <- url
        timestamp <- Sys.Date()
        platform <- "zcfp"
        lang <- "NL"
        projectname <- xpathSApply(pagesource, "//h2", xmlValue)[1]
        pledged  <- NA
        received <- gsub("Ã¢Â,Â¬ ", "", xmlValue(xpathSApply(pagesource, "//h4")[[2]]))
        currency <- "euro"
        success <- received > pledged
        status <- NA
        facebookurl <- NA
        facebooklikes <- NA
        commentcount <- NA
        backercount <- length(xpathSApply(pagesource, "//tr", xmlValue))-1
        pitch <- xpathSApply(pagesource, "//p", xmlValue)
        pitch <- paste(pitch[2:(length(pitch)-2)], collapse = " ")

        data.frame(url, timestamp, platform, lang, projectname, pledged, received, currency,
          success, status, facebookurl, facebooklikes, commentcount, 
          backercount, pitch)
}
## Status: WIP (only 2 projects, different page structure)



################## PART IV: EXECUTE PARSE FUNCTIONS ################## 

for (i in 1:nrow(voorjebuurt.raw)){
        dataset <- rbind(dataset, voorjebuurt(
                voorjebuurt.raw$url[i], 
                voorjebuurt.raw$dump[i]))
}

for (i in 1:nrow(citizinvestor.raw)){
        dataset <- rbind(dataset, citizinvestor(
                citizinvestor.raw$url[i], 
                citizinvestor.raw$dump[i]))
}

for (i in 1:nrow(zcfp.raw)){
        dataset <- rbind(dataset, zcfp(zcfp.raw$url[i], zcfp.raw$dump[i]))
}

