library(XML)


#download all datasets

# getstr() is my customized function
# http://www.r-bloggers.com/how-to-extract-a-string-between-2-characters-in-r-and-sas/
# it extracts a string between 2 characters in a string variable
getstr = function(mystring, initial.character, final.character)
{
  # check that all 3 inputs are character variables
  if (!is.character(mystring))
  {
    stop('The parent string must be a character variable.')
  }
  if (!is.character(initial.character))
  {
    stop('The initial character must be a character variable.')
  }
  if (!is.character(final.character))
  {
    stop('The final character must be a character variable.')
  }
  
  # pre-allocate a vector to store the extracted strings
  snippet = rep(0, length(mystring))
  
  for (i in 1:length(mystring))
  {
    # extract the initial position
    initial.position = gregexpr(initial.character, mystring[i])[[1]][1] + 1
    
    # extract the final position
    final.position = gregexpr(final.character, mystring[i])[[1]][1] - 1
    
    # extract the substring between the initial and final positions, inclusively
    snippet[i] = substr(mystring[i], initial.position, final.position)
  }
  return(snippet)
}

# Generate list of URLS from the NHANES website for all available datasets
dataurls = function() {
  pages = c("Demographics", "Dietary", "Examination", "Laboratory", "Questionnaire") #"Non-Public"
  urls = c()
  for(item in pages) {
    thepage = readLines(paste0('http://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=',item))
    mypattern = '/Nchs/Nhanes/.*.XPT'
    datalines = grep(mypattern,thepage,value=TRUE)
    urls = c(urls, paste0("http://wwwn.cdc.gov/",getstr(datalines,"/Nchs",".XPT"),".XPT"))
  }
  return(urls)
}

# If dataset from list is not already downloaded, download dataset
datadownload = function(datalist) {
  path = paste0("./data", substr(getstr(datalist, "/Nhanes/", ".XPT"),17,30), ".XPT")
  for(n in 1:length(datalist)) {
    if(!file.exists(path[n])) {
      download.file(datalist[n], path[n], mode="wb")
    }
    }
  }

# Load all datasets into R
loaddata = function(datalist) {
  path = paste0("./data", substr(getstr(datalist, "/Nhanes/", ".XPT"),17,30), ".XPT")
  datasets = substr(getstr(datalist, "Nhanes/", ".XPT"),17,30) 
  for(n in 1:length(path)) {
    assign(datasets[n], read.xport(path[n]))
  }  
}

# Look at variables
theurl = "http://wwwn.cdc.gov/Nchs/Nhanes/Search/variablelist.aspx?Component="
tables = readHTMLTable(theurl)
allvars = as.data.frame(tables)

allvars$ all = ifelse(as.numeric(as.character(allvars$ContentPlaceHolder1_GridView1.Begin.Year)) <= 1999 &
         as.numeric(as.character(allvars$ContentPlaceHolder1_GridView1.EndYear)) >=2014, 1, 0)

dropbyyear <- function(frame, fromname, toname, p=1) {
  datayear <- unique(adply(datayear, 1, summarise, year = seq(from, to))[c("set","set_base", "year")])
  
  for(y in unique(datayear$year)) {
    datayear[paste("y",y,sep="")] <- ifelse(datayear$year==y,1,0) }
  
  yearsum <- aggregate(datayear[,4:19], by=list(Category=datayear$set_base), FUN=sum)
  yearsum$avg <- rowMeans(yearsum[,2:17])
  
  return(yearsum[which(yearsum$avg >= p),1])
}

  
#  datayear <- data.frame(cbind(as.character(datasets$V1), datasets$V3))
#  colnames(datayear) <- c("years","set")
#  datayear$set_base <- gsub("_.*$", "", datayear$set)
#  datayear$from <- as.numeric(substr(datayear$years, 1, 4))
#  datayear$to <- as.numeric(substr(datayear$years, 6, 9))
  

# datadownload(dataurls())

