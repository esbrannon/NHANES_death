# NHANES Mortality Data Analysis
# Mortality Data - Lines 7 - 40
# NHANES Data - Lines 44 - 70

library(nhanesdata)
library(plyr)

### Mortality Data

years <- c("1999_2000", "2001_2002", "2003_2004", "2005_2006", "2007_2008", "2009_2010", "2011_2012", "2013_2014", "2015_2016")
years2 <- gsub("_", "-", years)
mort_filename1 = "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_"
mort_filename2 = "_MORT_2011_PUBLIC.dat"

mort_widths = c(14,1,1,1,3,1,1,1,4,8,8,3,3,1,1,1,1,1)
mort_names = 	c("publicid",       # str 14
                "eligstat", 		  # 15
                "mortstat", 		  # 16
                "causeavl",		    # 17
                "ucod_leading", 	# str 18-20 
                "diabetes", 		  # 21 
                "hyperten", 		  # 22 
                "dodqtr",			    # 23	/*nhis and lsoa ii only*/
                "dodyear",			  # 24-27	/*nhis and lsoa ii only*/
                "wgt_new",			  # 28-35	/*nhis only*/
                "sa_wgt_new",		  # 36-43 	/*nhis only*/
                "permth_int",		  # 44-46	/*nhanes only*/
                "permth_exm",		  # 47-49	/*nhanes only*/
                "mortsrce_ndi",		# 50
                "mortsrce_cms",		# 51
                "mortsrce_ssa",		# 52
                "mortsrce_dc",		# 53
                "mortsrce_dcl"    # 54 
                )

mort_data <- data.frame()
for (item in years) {
  mort_data <- rbind(mort_data, read.fwf(file=paste0(mort_filename1, item, mort_filename2), 
                                         col.names=mort_names, widths=mort_widths))
}

### Mortality data


### NHANES data

datasets <- c('demo', 'dietary', 'exam', 'lab', 'question', 'limited')
for (item in datasets) {
  temp <- cbind(item, read.table(paste0("./", item, ".txt"), "\t", header=FALSE))
  assign(item, temp)
}
datasets <- rbind(demo, dietary, exam, lab, question, limited)
colnames(datasets)[1] <- 'dataset'
datasets[[7]] <- gsub(" *.* ", "", datasets[[4]])
datasets[[4]] <- gsub(" .*", "", datasets[[4]])
rm(temp,demo,dietary,exam,lab,question,limited)




###################### Determine percentage of 
datayear <- data.frame(cbind(as.character(datasets$V1), datasets$V3))
colnames(datayear) <- c("years","set")
datayear$type <- gsub("_.*$", "", datayear$set)
datayear$from <- as.numeric(substr(datayear$years, 1, 4))
datayear$to <- as.numeric(substr(datayear$years, 6, 9))

datayear <- unique(adply(datayear, 1, summarise, year = seq(from, to))[c("set","type", "year")])


for(y in unique(datayear$year)) {
     datayear[paste("y",y,sep="")] <- ifelse(datayear$year==y,1,0) }

yearsum <- aggregate(datayear[,4:19], by=list(Category=datayear$type), FUN=sum)
yearsum$avg <- rowMeans(yearsum[,2:17])

finalsets <- yearsum[which(yearsum$avg >= .75),1]

####################

datasets <- datasets[gsub("_.*$", "", datayear$set) %in% finalsets, ]





data(list = tolower(datasets[[4]]))

for (item in ls()) {
  if(!("SEQN" %in% names(eval(parse(text = item))))) {
    rm(list = item)  
  }
}

for (item in ls()) {
  print(item)
  if (item != "demo" && item != "zdemo" && item != "demo_b" && item != "demo_c") {
    demo <- merge(demo, eval(parse(text = item)), by = "SEQN", all.x = TRUE)
    demo <- Filter(function(x)!all(is.na(x)), demo)
  }
}