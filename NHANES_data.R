

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

datalist = c("Demographics", "Dietary", "Examination", "Laboratory", "Questionnaire") #"Non-Public"
for(item in datalist) {
  thepage = readLines(paste0('http://wwwn.cdc.gov/Nchs/Nhanes/Search/DataPage.aspx?Component=',item)  
}

mypattern = '/Nchs/Nhanes/.*.XPT'
datalines = grep(mypattern,thepage,value=TRUE)
getstr(datalines,"/Nchs",".XPT")

