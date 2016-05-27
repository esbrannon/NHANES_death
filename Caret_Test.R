library(nhanesdata)
library(caret)
library(dplyr)
library(magrittr)

data("demo")

### Load NHANES mortality data for 1999-2000
loadmortality <- function() { 
  mort_filename1 = "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_"
  mort_filename2 = "_MORT_2011_PUBLIC.dat"
  mort_widths = c(14,1,1,1,3,1,1,1,4,8,8,3,3,1,1,1,1,1)
  mort_names = 	c("SEQN",       # str 14
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
  mort_data <- rbind(mort_data, read.fwf(file=paste0(mort_filename1, "1999_2000", mort_filename2), 
                                           col.names=mort_names, widths=mort_widths))
  return(mort_data)
  }


mort_data <- loadmortality()
data <- merge(demo, mort_data, by="SEQN")
data <- data[which(!is.na(data$mortstat)),]

set.seed(2)
inTrain <- createDataPartition(y = data$mortstat, p = .66, list = FALSE)

set.seed(5252016)
inTrain2 = sample(1:nrow(data),round(nrow(data)*2/3))

data$training[inTrain2] = TRUE
data$training[-inTrain2] = FALSE

data %>% group_by(training,mortstat) %>% summarize(n())

data_caret = 
  data %>% as_data_frame %>%
  mutate_each(funs(
    . %>% {ifelse(is.na(.),9999999999,.)}
                   )) %>% 
  mutate_each(funs(. %>% {paste0('A',.)}),RIDSTATR:RIAGENDR,RIDRETH1:INDFMINC,mortstat) %>%
  mutate_each(funs(as.factor),RIDSTATR:RIAGENDR,RIDRETH1:INDFMINC,mortstat) %>%
  select(RIDSTATR:INDFMINC,mortstat)

training <- data_caret[ inTrain2,]
testing <- data_caret[-inTrain2,]

caret_settings = trainControl(
  method='cv',
  number=10,
  verboseIter = TRUE,
  returnData=FALSE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

rf_11 = 
  train(x = training %>% select(-mortstat) %>% as.data.frame,
        y = training$mortstat,
        trControl=caret_settings,
        method='rf',
        metric='ROC',
        tuneLength=1,
        ntrees=11
        )

rf_11
rf_11$resample %>% summary

predictions = predict(rf_11,training %>% select(-mortstat) %>% as.data.frame)
confusionMatrix(predictions,training$mortstat,positive='A1')

predictions = predict(rf_11,testing %>% select(-mortstat) %>% as.data.frame)
confusionMatrix(predictions,testing$mortstat,positive='A1')
