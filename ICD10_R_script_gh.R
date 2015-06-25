setwd("path")

##function will identify ICD10 codes from a text document using regx##
get_ICD_codes <- function(myicddoc1) { #function takes input, identifies icd codes and outputs them as a vector
  m <- scan(myicddoc1, sep='', strip.white = TRUE, what = 'character', blank.lines.skip = TRUE)
  mycodes <- vector(mode = "character", 0)  #creates vector for function output
  icd10 <- "^[A-Z][0-9]{2,6}"
  icd9 <- "^[0-9]{3,6}"  
  for(i in m){
    match <- grep(icd10, i, perl=TRUE, value=TRUE, ignore.case=TRUE)
    mycodes <- append(mycodes, match)}
  mycodes
}
codes <- get_ICD_codes("path")

##function to insert decimals into the ICD codes##
decimate <- function(mycodes){
    mycodes2 <- vector(mode = "character", 0)  #creates vector for function output  
    for (code in mycodes){
        if((nchar(code)>3) == TRUE) {
            code2 <- paste(substr(code, 1,3), ".", 
            substr(code, 4, 5), sep = "")
            mycodes2 <- append(mycodes2, code2)} else {
                code2 <- code 
                mycodes2 <- append(mycodes2, code2)
                }
            mycodes2
            }  
        mycodes2
    }
codes2 <- decimate(codes)

#need to add version and sep options to specify comma sep and 9/10 versions

##this function uses a list of icd10 codes, queries ICD10data website 
##and returns diagnosis descriptions
query_ICD_data <- function(myicddoc2) {
  library("RCurl")
    myicdtable <- data.frame(diagnosis = character(), description = character())
    for(diag in myicddoc2) {
        requesturl <- paste(
            "www.icd10data.com/Search.aspx?search=", diag, "&codebook=ICD10CM", sep="")
        cat("querying:",diag,"\n")
        txt <- getURL(requesturl, .encoding = "UTF-8")
        Encoding(txt) <- "bytes"
        txt2 <- strsplit(x = txt, split = "class=", fixed = TRUE, perl = FALSE, useBytes = FALSE)
        txtdf <- data.frame(txt2)
        junkstrings <- "(</div>|<table |<div )"
        resultdesc <- "SearchResultDescription"
        for (line in txtdf){
          line <- gsub(junkstrings, "", line, perl = TRUE)
          matches <- grep(resultdesc, line, perl=FALSE, fixed = TRUE, value=TRUE, ignore.case=FALSE)
        description <- substr(matches [1], 27, 1000)
          myicdtable <- rbind(myicdtable, data.frame(diagnosis = diag, description = description))}
        myicdtable    
    }
    myicdtable
}
codes3<-query_ICD_data(codes2)   

# add output in various formats; sas format sas array R
sasarray <- function(output) {  
  sa <- data.frame(codes_as_array = paste("('", codes3$diagnosis, "' = diag(i)) /*", codes3$description, "*/|"), sep = "")
  write.table(sa, output, sep="\n", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
sasarray(output="output.txt")

sasformat <- function(output) {  
  sf <- data.frame(codes_as_format = paste("'", codes3$diagnosis, "' = ", "1", " /*", codes3$description, " */ "), sep = "")
  write.table(sf, output, sep="\n", quote = FALSE, row.names = FALSE, col.names = FALSE)
}
sasformat("output.txt")



