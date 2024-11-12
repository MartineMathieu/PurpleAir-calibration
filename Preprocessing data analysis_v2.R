
#########################################
#########################################
## Martine Mathieu-Campbell

#Calibration of Low-Cost Particulate Matter Sensors PurpleAir: 
# Model Development for Air Quality under High Relative Humidity Conditions
#########################################
######################################### 

######################################### PREPROCESSING ANALYSIS - 0.5 KM RADIUS - HOURLY DATA

# Libraries
###############################
# Creating a vector of all the packages
all_packages <- c("dplyr",
                  "data.table",
                  "stringr",
                  "vctrs",
                  "sf",
                  "tidyverse",
                  "lubridate",
                  "readr",
                  "openair") 


new_packages <- all_packages[!(all_packages %in% installed.packages()[,"Package"])]

# install new packages
if(length(new_packages)) install.packages(new_packages)

# load all packages
sapply(all_packages, require, character.only = T)


# Project directory
###############################
dir_data <- paste0("~PURPLEAIR_Data Correction/")
setwd(paste0(dir_data))

new_dirs <- paste0(dir_data, 
                   c( "DATA",
                      "clean_DATA")); new_dirs

for (i in 1:length(new_dirs)){
  
  if(!dir.exists(new_dirs[i])){ 
    dir.create(new_dirs[i])  
  } else {
    print(paste0(new_dirs[i], " already exists")) 
  }
}



######################################### PREPROCESSING ANALYSIS - 0.5 KM RADIUS - HOURLY DATA
#########################################
dir_data <- paste0("~PURPLEAIR_Data Correction/")
setwd(paste0(dir_data))
project_subdir0 <- paste0("DATA/PurpleAir Data/")

setwd(paste0(dir_data, project_subdir0))

# CREATE A LIST FILES IN DIRECTORY
filelist_PA <- list.files(pattern = ".csv", full.names = F)
#Read csv files in list 
df_read_PA <- lapply(filelist_PA, read.csv, header=TRUE)
# ADD sensor index to each PA file
fileword_PA <- lapply(filelist_PA, function(df){
  x <- word(df[1])
  return(x)
})
df_1stWord <- unlist(fileword_PA)
df_1stWord <- as.data.frame(df_1stWord)
df_1stWord_List <- split(df_1stWord, seq(nrow(df_1stWord)))

# Delete dataframe if empty
deleteFile_PA <- list_drop_empty(df_read_PA) 

# Joining data
jointlist <- lapply(deleteFile_PA, function(df){
  joinTHEM <- cbind(df, df_1stWord_List)
})
length(jointlist[[1]])
cleandf_all <- as.data.frame(unlist(jointlist))
clean_df_all <- bind_rows(cleandf_all , .id = "ID")
names(clean_df_all)
colnames(clean_df_all)[9] ="sensor_index"

names(clean_df_all)

setwd(paste0(dir_data))
write_csv(clean_df_all, "clean_DATA/PA_all_data_Jan2021_Aug2023.csv")


##############################################################
############## 2- EVALUATION OF THE RAW COMPILED DATASET
#############################################################
clean_df_all <- read.csv("/Users/martineelisabethmathieu/Documents/NCSU /RESEARCH/PURPLEAIR_Data Correction/PURPLEAIR-AQS-NEW APPROACH_Summer2023/PurpleAir_Data_Download/PAData_AnyAge/RawPMData_2_5km/PA_all_data_Jan2021_Aug2023.csv")

# FIND NAs FOR PM2.5

sum(is.na(clean_df_all$pm2.5_cf_1_a))
sum(is.na(clean_df_all$pm2.5_cf_1_b))
sum((is.na(clean_df_all$pm2.5_cf_1_a)) & (is.na(clean_df_all$pm2.5_cf_1_a))) # 1215

# Percentage NAs for PM
nrow(clean_df_all)
channels_NAs <- (((sum(is.na(clean_df_all$pm2.5_cf_1_b)) + sum(is.na(clean_df_all$pm2.5_cf_1_a))) * 100) / (nrow(clean_df_all)))
channels_NAs2 <- (((sum(is.na(clean_df_all$pm2.5_cf_1_b))) * 100) / (nrow(clean_df_all)))


# FIND NAs FOR METEOROLOGICAL DATA

# For temperature
sum(is.na(clean_df_all$temperature_a)) 
sum(is.na(clean_df_all$temperature_b)) 

# For humidity
sum(is.na(clean_df_all$humidity_a))
sum(is.na(clean_df_all$humidity_b))
sum((is.na(clean_df_all$humidity_a)) & (is.na(clean_df_all$humidity_b)))

sum((is.na(clean_df_all$temperature_a)) & (is.na(clean_df_all$temperature_b)) & (is.na(clean_df_all$humidity_a)) & (is.na(clean_df_all$humidity_b))) 


# Check for PM2.5, Humidity and Temperature
variables_all <-  sum((is.na(clean_df_all$temperature_a)) & (is.na(clean_df_all$temperature_b)) &
                        (is.na(clean_df_all$humidity_a)) & (is.na(clean_df_all$humidity_b)) &
                        (is.na(clean_df_all$pm2.5_cf_1_a)) & (is.na(clean_df_all$pm2.5_cf_1_b))) # 173


##############################################################
############## 3- CLEANING PROCESS
#############################################################


##############################################################
############## Cleaning process 1
### REMOVE NAs DATA FOR PM, T, AND RH

#remove rows with NA value
clean_df_all_nonNAs <- clean_df_all %>%
  filter_at(vars(pm2.5_cf_1_a, pm2.5_cf_1_b, temperature_a, humidity_a), all_vars(!is.na(.)))


# REMOVE UNNECESSARY VARIABLES
names(clean_df_all_nonNAs)
clean_df_all_nonNAs2 <- clean_df_all_nonNAs[,-c(4,6)]


##############################################################
############## Cleaning process 2
### DEFINE LIMIT OF DETECTION


# STEP0 : PRELIMINARY CALCULATIONS

#calculate row-wise quality measures
# Mean Channels A & B
clean_df_all_nonNAs3<-clean_df_all_nonNAs2 %>% rowwise() %>% mutate(A_B_Avg=(pm2.5_cf_1_a+pm2.5_cf_1_b)/2)%>% 
  # Channels A & B deviation
  mutate(A_B_Diff=abs(pm2.5_cf_1_a-pm2.5_cf_1_b)) %>% 
  # Percent error deviation
  mutate(A_B_ErDeviation=abs(((pm2.5_cf_1_a-A_B_Avg)/A_B_Avg))*100)

head(clean_df_all_nonNAs3)


# STEP1: CONSIDER A LOD > 1.50

# LOD >1.50
clean_df_all_nonNAs3_LOD <- filter(clean_df_all_nonNAs3, (pm2.5_cf_1_a >1.50) & (pm2.5_cf_1_b > 1.50))

# DATA EVALUATION
sensorCount1 <- clean_df_all_nonNAs3_LOD %>% group_by(sensor_index) %>%tally()
sensorCount1$day <- (sensorCount1$n) /24
# Data removed
(((nrow(clean_df_all_nonNAs3)) - (nrow(clean_df_all_nonNAs3_LOD))) *100)/ (nrow(clean_df_all_nonNAs3)) 

# EVALUATION OF THE REMAINING SENSORS:
# group by column1 values and count the total in each
sensorCount <- clean_df_all_nonNAs3_LOD %>% group_by(sensor_index) %>%tally()
sensorCount$day <- (sensorCount$n) /24


# STEP2: LOW & HIGH CONCENTRATIONS

#Add index column to data frame
clean_df_all_nonNAs3_LOD$ID2 <- 1:nrow(clean_df_all_nonNAs3_LOD)

clean_df_all_nonNAs3_LOD_Low_HighCompiled <- subset(clean_df_all_nonNAs3_LOD,
                                               !(clean_df_all_nonNAs3_LOD$A_B_Avg <= 25.000 & ((A_B_Diff >= 5.000) & (A_B_ErDeviation >= 21.000))) &
                                                 !(clean_df_all_nonNAs3_LOD$A_B_Avg > 25.000 & (A_B_ErDeviation <= 20.000)))

nrow(clean_df_all_nonNAs3_LOD_Low_HighCompiled)


# Total Percent data considered
((nrow(clean_df_all_nonNAs3_LOD_Low_HighCompiled))*100)/ (nrow(clean_df_all))
# Total Percent data removed
(100) - ((nrow(clean_df_all_nonNAs3_LOD_Low_HighCompiled))*100)/ (nrow(clean_df_all))


##############################################################
############## Cleaning process 3
###

# STEP3: REMOVE HIGH CONCENTRATION GREATER THAN 1000 MICROG/M3 

clean_df_all_nonNAs3_LOD_Low_HighCompiled_1000 <- clean_df_all_nonNAs3_LOD_Low_HighCompiled[clean_df_all_nonNAs3_LOD_Low_HighCompiled$A_B_Avg <= 1000.000,]


# Total Percent data considered
((nrow(clean_df_all_nonNAs3_LOD_Low_HighCompiled_1000))*100)/ (nrow(clean_df_all))
# Total Percent data removed
(100) - ((nrow(clean_df_all_nonNAs3_LOD_Low_HighCompiled_1000))*100)/ (nrow(clean_df_all))

unique(clean_df_all_nonNAs3_LOD_Low_HighCompiled_1000$sensor_index)

setwd(paste0(dir_data))
write_csv(clean_df_all, "clean_DATA/PA_all_data_Jan2021_Aug2023_Final_cleanProcessFinal.csv")

#####END QUALITY CONTROL


##############################################################
############## 4- AQS AND PA DATA COMPILATION FOR 0.5 KM RADIUS
#############################################################



############## Process 1: AQS AND PA DATA COMPILATION : COMPILATION OF THE UNIQUE DATA FILES

project_subdir1 <- paste0("DATA/AQS Data/")

setwd(paste0(dir_data, project_subdir1))


allAQSdata_df <- read.csv("allAQSdata_1H_Jan21_Aug23.csv", header = TRUE)
head(allAQSdata_df)
# UNIQUE location AQS data 
allAQSdataUnique_sf <- st_read("allAQSdataUNIQUE_1H_Jan21_Aug23_clipped.shp",crs=st_crs(4269))
head(allAQSdataUnique_sf)
# UNIQUE location AQS data with BUFFER
allAQSdataUnique_sf_Buf0_5 <- st_read("Buf_AQSdataUNIQUE_1H_Jan21_Aug23_clipped0_5KM.shp",crs=st_crs(4269))
head(allAQSdataUnique_sf_Buf0_5)

# GET the CENTROID of AQS buffers

allAQSdataUnique_sf_Buf0_5$centroids <- st_transform(allAQSdataUnique_sf_Buf0_5, 4269) %>% 
  st_centroid() %>%  
  st_geometry()

head(allAQSdataUnique_sf_Buf0_5)


# UNIQUE location PA data

AnyAgePA_0_5km_Unique <- st_read("AnyAgePA_0_5km.shp",crs=st_crs(4269))
head(AnyAgePA_0_5km_Unique)

# CONVERT multipoint to point
AnyAgePA_0_5km_Unique_points <- st_cast(AnyAgePA_0_5km_Unique, "POINT")
head(AnyAgePA_0_5km_Unique_points)

# FIND and JOIN PA points within AQS BUFFER polygons
allAQS_and_PA_dataUnique__Buf0_5 <- st_join(AnyAgePA_0_5km_Unique_points, allAQSdataUnique_sf_Buf0_5, join = st_within) 
head(allAQS_and_PA_dataUnique__Buf0_5)
# SELECT necessary variables
allAQS_and_PA_dataUnique__Buf0_5_Select <- allAQS_and_PA_dataUnique__Buf0_5 %>% select(c(1,7,8,9))
head(allAQS_and_PA_dataUnique__Buf0_5_Select)

# GET distance between PA points and AQS BUFFER CENTROID
allAQS_and_PA_dataUnique__Buf0_5_Select2 <- allAQS_and_PA_dataUnique__Buf0_5_Select
allAQS_and_PA_dataUnique__Buf0_5_Select2$distance <- mapply(st_distance, allAQS_and_PA_dataUnique__Buf0_5_Select2$geometry, allAQS_and_PA_dataUnique__Buf0_5_Select2$centroids)


allAQS_and_PA_dataUnique__Buf0_5_Select3 <- allAQS_and_PA_dataUnique__Buf0_5_Select2

# ASSIGN sequence to row names 
rownames(allAQS_and_PA_dataUnique__Buf0_5_Select3) <- 1:nrow(allAQS_and_PA_dataUnique__Buf0_5_Select3)    

# SELECT ALL duplicate sensor IDs
# 0 duplicates AS WELL
allAQS_and_PA_dataUnique__Buf0_5_Select3_2ID <- subset(allAQS_and_PA_dataUnique__Buf0_5_Select3,ave(1:nrow(allAQS_and_PA_dataUnique__Buf0_5_Select3),sensor_ind, FUN = length)>1)

# CHECK duplicate elements
allAQS_and_PA_dataUnique__Buf0_5_Select3_clean$sensor_ind[duplicated(allAQS_and_PA_dataUnique__Buf0_5_Select3_clean$sensor_ind)]
# CHECKED

# REMOVE NA values
# NO NA FOUND as well
allAQS_and_PA_dataUnique__Buf0_5_Select3_cleanNA <- na.omit(allAQS_and_PA_dataUnique__Buf0_5_Select3_clean)
head(allAQS_and_PA_dataUnique__Buf0_5_Select3_cleanNA)



############## Process 2: AQS AND PA DATA COMPILATION : COMPILATION BETWEEN UNIQUE AND FULL DATA FILES


# INNER JOIN
# SENSOR INDEX as character
clean_df_all_nonNAs3_LOD_Low_HighCompiled_1000$sensor_index <- as.character(clean_df_all_nonNAs3_LOD_Low_HighCompiled_1000$sensor_index)

fullAQS_and_PA_data <- clean_df_all_nonNAs3_LOD_Low_HighCompiled_1000 %>% inner_join(allAQS_and_PA_dataUnique__Buf0_5_Select3_cleanNA,
                                                                                     by=c("sensor_index" = "sensor_ind"))
sum(duplicated(fullAQS_and_PA_data))


# ADD FULL AQS DATA

# Define project directory and UPLOAD full AQS data
allAQSdataCOMBINED_Jan21_Aug23 <- read.csv("allAQSdata_1H_Jan21_Aug23_updated.csv", header = TRUE)
head(allAQSdataCOMBINED_Jan21_Aug23)
sum(duplicated(allAQSdataCOMBINED_Jan21_Aug23))
# SELECT necessary variables
allAQSdataCOMBINED_Jan21_Aug23 <- allAQSdataCOMBINED_Jan21_Aug23 %>% select(c(1,2,3,4,5,9))
head(allAQSdataCOMBINED_Jan21_Aug23)


# CHANGE TIME FORMAT BEFORE INNER JOIN
fullAQS_and_PA_data1 <- fullAQS_and_PA_data
fullAQS_and_PA_data1$time_stamp<- ymd_hms(fullAQS_and_PA_data1$time_stamp)
sum(duplicated(fullAQS_and_PA_data1))
allAQSdataCOMBINED_Jan21_Aug23_1 <- allAQSdataCOMBINED_Jan21_Aug23
allAQSdataCOMBINED_Jan21_Aug23_1$UTC <- ymd_hm(allAQSdataCOMBINED_Jan21_Aug23_1$UTC)
sum(duplicated(allAQSdataCOMBINED_Jan21_Aug23_1))

# PROCEED WITH THE INNER JOIN
names(fullAQS_and_PA_data1)
names(allAQSdataCOMBINED_Jan21_Aug23_1)
allAQSdataCOMBINED_Jan21_Aug23_1 <- allAQSdataCOMBINED_Jan21_Aug23_1%>% select(-c(1))

fullAQS_and_PA_data_BOTH <- fullAQS_and_PA_data1 %>% inner_join(allAQSdataCOMBINED_Jan21_Aug23_1,
                                                                by=c("AQSID" = "AQSID",
                                                                     "time_stamp" = "UTC"))


# CHECK for HUMIDITY range values
#### No HUMIDITY > 100 %
summary(fullAQS_and_PA_data_BOTH$humidity_a)
# CHECK for TEMPERATURE range values
summary(fullAQS_and_PA_data_BOTH2$temperature_a)
# REMOVE records with TEMPERATURE  > 130 F and < 0 F
fullAQS_and_PA_data_BOTH_clean <- subset(fullAQS_and_PA_data_BOTH2, (fullAQS_and_PA_data_BOTH2$temperature_a < 131))
fullAQS_and_PA_data_BOTH_clean <- subset(fullAQS_and_PA_data_BOTH_clean, (fullAQS_and_PA_data_BOTH_clean$temperature_a > 0.00) )

# ESTIMATE % removed
100 - (nrow(fullAQS_and_PA_data_BOTH_clean) * 100) / nrow(fullAQS_and_PA_data_BOTH2)


######################### END OF AQS AND PA DATA COMPILATION



##############################################################
############## 5- DEWPOINT TEMPERATURE
#############################################################


############## Process 1: CONVERT TEMPERATURE INTO CELSIUS

fullAQS_and_PA_data_BOTH_clean$temp_Celsius <- ((fullAQS_and_PA_data_BOTH_clean$temperature_a - 32) * (5/9))
head(fullAQS_and_PA_data_BOTH_clean)

############## Process 2: ESTIMATE DEWPOINT TEMPERATURE IN CELSIUS

fullAQS_and_PA_data_BOTH_clean$dewPoint <- ((fullAQS_and_PA_data_BOTH_clean$temp_Celsius) - ((100 - fullAQS_and_PA_data_BOTH_clean$humidity_a)/5))
head(fullAQS_and_PA_data_BOTH_clean)


# REMOVE UNNECESSARY VARIABLES AND ADD NEW ID

names(fullAQS_and_PA_data_BOTH_clean)
#ADD index column to data frame
fullAQS_and_PA_data_BOTH_clean$data_ID <- 1:nrow(fullAQS_and_PA_data_BOTH_clean)
names(fullAQS_and_PA_data_BOTH_clean)
# SELECT variables
fullAQS_and_PA_data_BOTH_clean2 <- fullAQS_and_PA_data_BOTH_clean %>% select(c(21,2,12,18,7,5,6,8,3,4,19,20,13,14))
names(fullAQS_and_PA_data_BOTH_clean2)
head(fullAQS_and_PA_data_BOTH_clean2)


# REMOVE ALL AQS CONC OBS < 0 FROM THE FINAL AQS-PA JOINED DATASET


summary(fullAQS_and_PA_data_BOTH_clean2)

fullAQS_and_PA_data_BOTH_clean3 <- subset(fullAQS_and_PA_data_BOTH_clean2, conc >= 0) 
nrow(fullAQS_and_PA_data_BOTH_clean2) - nrow(fullAQS_and_PA_data_BOTH_clean3) 


# RE-SAVE FINAL FILE: Final saving

write_csv(fullAQS_and_PA_data_BOTH_clean3, "r0_5km_processeddata_PA_AQS_Jan2021_Aug2023_Final.csv")



#####################################
#  DAILY AVERAGE DATA
#####################################

class(all_PA_forDaily$date2)
all_PA_forDaily <- all_PA_forDaily %>% mutate(DAILY=as.Date(date2))
names(all_PA_forDaily)

# Mean on all columns
all_PA_forDaily_group <- all_PA_forDaily %>%                                 
  group_by(sensor_index, DAILY) %>%
  dplyr::summarize(conc = mean(conc), A_B_Avg = mean(A_B_Avg), pm2.5_cf_1_a = mean(pm2.5_cf_1_a),
                   pm2.5_cf_1_b = mean(pm2.5_cf_1_b), humidity_a = mean(humidity_a), 
                   temp_Celsius = mean(temp_Celsius), dewPoint = mean(dewPoint))%>%
  
  as.data.frame()
head(all_PA_forDaily_group) 

#####################################
# 90% COMPLETENESS
#####################################
sensorCount_0.5 <- all_PA_forDaily %>% group_by(sensor_index, DAILY) %>%tally()
names(sensorCount_0.5)

all_PA_forDaily_group_90 <- all_PA_forDaily_group %>% inner_join(sensorCount_0.5,by=c("sensor_index", "DAILY"))
names(all_PA_forDaily_group_90)

all_PA_NOAA2_sub_DAILY_90_clean <- subset(all_PA_forDaily_group_90, n>=21)

setwd(paste0(dir_data))
write_csv(all_PA_NOAA2_sub_DAILY_90_clean, "clean_DATA/DAILY90_r0_5km_all_PA_NOAA_withNA_NOflawedPA.csv")

######################### END PREPROCESSING 
##########################
##################### 
##########################
