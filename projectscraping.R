####################### PART I: INITIATLISATION ####################### 

require(scrapeR)
require (stringr)

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
          start = character(0),
          end = character(0),
          runtime = character(0),
          finished = logical(),
          facebookurl = character(0),
          facebooklikes = numeric(0),
          commentcount = character(0),
          backercount = character(0),
          pitch = character(0))
}
initiatlize()

#################### PART II: GETTING THE RAW DATA #####################

getRawData <- function(platform = "all") {

        if (file.exists("rawdata")){
                if (platform=="all"){
                        filenames <- list.files("rawdata", full.names=T)
                        for (file in filenames){
                                filename <- gsub(".R", ".raw", file)
                                filename <- gsub("rawdata/", "", filename)
                                raw.data <- dget(file)
                                assign(filename, dget(file), envir=.GlobalEnv)
                        }
                } else {
                        file <- paste("rawdata/", platform, ".R", sep="")
                        if (!file.exists(file)){
                                stop ("platform not found, please check 'rawdata' folder of run dumpToRawdata()")
                                return
                        }
                        filenames <- paste(platform, ".raw", sep="")
                        raw.data <- dget(file)
                        assign(filenames, dget(file), envir=.GlobalEnv)
                }
                cat ("## added to global environment from cache:", filenames)
                
        } else {
                stop ("rawdata folder not found, please check working dir or create folder")
        }
        
}
dumpToRawdata <- function (platform){
        
        # platform by platform to avoid server errors
        # heavy load!!
        
        fresh.dump <- data.frame(
                url = projects$url[projects$platform == platform],
                dump = as.character(
                        lapply(as.character(
                                projects$url[projects$platform == platform]), 
                               readLines)))
        filename <- paste("./rawdata/", platform, ".R", sep = "")
        dput (fresh.dump, filename)  
        
        ## status: error for communityfunded & dordrechtvanstart
        
}  

#################### PART III: DOM PARSE FUNCTIONS #################### 

voorjebuurt <- function (url, dump){
        #dump <- as.character(voorjebuurt.raw$dump[41])
        #url <- voorjebuurt.raw$url[41]
        pagesource <- htmlTreeParse(as.character(dump), useInternalNodes = T)
        
        url <- url
        timestamp <- Sys.Date()
        platform <- "voorjebuurt"
        lang <- "NL"
        projectname <- xpathSApply(pagesource, "//h1", xmlValue)[2]
        pledged <- xpathSApply(pagesource, "//li[contains(@class, 'progress')]/p", xmlValue)
        pledged <- gsub("â,¬","",(strsplit(pledged, " ")[[1]][3]))
        pledged <- as.numeric(gsub(",",".",gsub(".","",pledged, fixed = TRUE)))
        received <- gsub("â,¬", "", xpathSApply(pagesource, "//li[contains(@class, 'progress')]/h3", xmlValue))
        received <- as.numeric(gsub(",",".",gsub(".","",received, fixed = TRUE)))
        currency <- "euro"
        success <- received >= pledged
        start <- gsub("\\\", \\\"\\\\t\\\\t\\\", \\\"\\\\t\\\\tGestart op ","",xpathSApply(pagesource, "//div[contains(@class, 'date')]", xmlValue))
        start <- strptime(gsub("\\\\t", "", start), "%d %B %Y") 
        end <- xpathSApply(pagesource, "//div[contains(@class, 'funding-ends')]", xmlValue)
        end <- gsub("\\\", \\\"\\\\t\\\\t\\\", \\\"\\\\t\\\\tEindigt op ","",end,)
        end <- strptime(gsub("\\\\t", "", end), "%d %B %Y")
        if(length(end) == 0) end <- NA
        runtime <- difftime(end, start, units = 'days')
        finished <- difftime(timestamp, end, units = 'days')>0
        facebookurl <- xpathSApply(pagesource, "//a[contains(@href,'www.facebook.com')]", xmlValue)[1]
        facebooklikes <- NA
        commentcount <- xpathSApply(pagesource, "//h2[contains(@class,'comments-title')]", xmlValue)
        commentcount <- gsub("\\\", \\\"\\\\t\\\\t\\\\t","",commentcount)
        commentcount <- as.numeric(gsub(" Reactie\\\\t\\\\t","",commentcount))
        if(length(commentcount) == 0) commentcount <- 0
        backercount <- as.numeric(xpathSApply(pagesource, "//li[contains(@class,'backer-count')]/h3", xmlValue)[1])
        pitch <- xpathSApply(pagesource, "//div[contains(@id, 'descrip')]", xmlValue)
        pitch <- gsub("\\\", \\\"\\\\t\\\\t\\\\t\\\\t\\\\t\\\\t\\\\t","", pitch)
        pitch <- gsub("\\\", \\\"\\\", \\n\\\"\\\", \\n\\\"\\\", \\\"\\\", \\\"\\\", \\n\\\"\\\", \\n\\\"\\\", \\\"\\\\t\\\\t\\\\t\\\\t\\\\t\\\\t","",pitch)
        pitch <- gsub("\\\", \\n\\\"","",gsub("\\\", \\\"","",pitch))
        
        data.frame(url, timestamp, platform, lang, projectname, pledged, received, currency,
                   success, start, end, runtime, finished, facebookurl, facebooklikes, commentcount, 
                   backercount, pitch)
}
## Status: WIP (look at NAs, and fact check)

citizinvestor <- function (url, dump){
        #dump <- as.character(citizinvestor.raw$dump[1])
        #url <- citizinvestor.raw$url[1]
        pagesource <- htmlTreeParse(as.character(dump), useInternalNodes = T)
        
        url <- url
        timestamp <- Sys.Date()
        platform <- "citizinvestor"
        lang <- "EN"
        projectname <- xpathSApply(pagesource, "//h1", xmlValue)[2]
        pledged <- xpathSApply(pagesource, "//div[contains(@class, 'amount')]/span", xmlValue)[2]
        pledged <- as.numeric(gsub(",",".",gsub(" required","",gsub("out of \\$","",pledged))))
        received <- xpathSApply(pagesource, "//span[contains(@class, 'number')]", xmlValue)[3]
        received <- as.numeric(gsub("\\$","",received))
        currency <- "usd"
        success <- received >= pledged
        start <- NA
        end <- xpathSApply(pagesource, "//span[contains(@class, 'number')]", xmlValue)[1]
        end <- as.Date(timestamp + as.numeric(end))
        if(length(end) == 0) end <- NA
        runtime <- NA
        finished <- difftime(timestamp, end, units = 'days')>0
        facebookurl <- xpathSApply(pagesource, "//a[contains(@href,'www.facebook.com')]", xmlValue)[1]
        facebooklikes <- NA
        commentcount <- NA
        backercount <- as.numeric(xpathSApply(pagesource, "//span[contains(@class,'number')]", xmlValue)[2])
        pitch <- paste(xpathSApply(pagesource, "//div[contains(@class, 'summary')]/*/p", xmlValue), collapse="")
        pitch <- str_replace_all(pitch, "[^[:alnum:]]", " ")
        pitch <- gsub("     ","",pitch)
        
        data.frame(url, timestamp, platform, lang, projectname, pledged, received, currency,
                   success, start, end, runtime, finished, facebookurl, facebooklikes, commentcount, 
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
        start <- NA
        end <- NA
        runtime <- NA
        status <- NA
        facebookurl <- NA
        facebooklikes <- NA
        commentcount <- NA
        backercount <- length(xpathSApply(pagesource, "//tr", xmlValue))-1
        pitch <- xpathSApply(pagesource, "//p", xmlValue)
        pitch <- paste(pitch[2:(length(pitch)-2)], collapse = " ")

        data.frame(url, timestamp, platform, lang, projectname, pledged, received, currency,
          success, start, end, runtime, finished, facebookurl, facebooklikes, commentcount, 
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

