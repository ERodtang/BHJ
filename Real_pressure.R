#Clear workspace
rm(list=ls())

#Load Libraries
library(tidyverse)

Calibration_coeff <- 1
Indentor_radius <- 1.5#cm
Indentor_radius <- 1.5*0.01#m
Indentor_area <- pi*(Indentor_radius^2)

data_folder <-"B:\\Data\\Bridge_ice\\Trondelag\\Sokna\\BHJ\\2021_02_22_Datadump_CF_30\\"
#filename <- "B:\\Data\\Bridge_ice\\Trondelag\\Sokna\\BHJ\\2021_02_22_Datadump_CF_30\\2021_02_10_BHJ2_hole7_directionU_depth6cm_test17_sokna_dam2.lvm"

files_list <- list.files(path = data_folder, ".lvm")#Retrieve list of all .lvm files in data folder
files_list <- as.list(files_list)
#long_files_list <- lapply(files_list, function(x){long_list_item<- paste(data_folder,x, sep = "")
#  return(long_list_item)})

All_data <- lapply(files_list, function(x){
  csv_file <- read.csv(paste(data_folder,x, sep=""), sep="\t", skip=10, col.names = c("Time", "Force1","Force2"))
  csv_file <- csv_file %>% add_column(File = x)
  return(csv_file)})

All_data <- do.call(rbind, All_data)

Real_pressure <- function(data){
  #Add forces
  data <- data %>% mutate(Sum_force = -(Force1+Force2))#unit=KN
  #Multiply by calibration coefficient, note that calibration coefficient depends on temperature
  data <- data %>% mutate(Calibrated_force = Sum_force*Calibration_coeff)#unit=KN
  #Divide by indentor area to get pressure
  data <- data %>% mutate(Pressure = Calibrated_force/Indentor_radius)#unit=KN/m^2
  #Convert to pressure to MPa
  data <- data %>% mutate(Pressure =Pressure/1000)
  return(data)
}

All_data <- Real_pressure(All_data)

df1 <- aggregate(Calibrated_force ~ File, All_data, max)
df2 <- aggregate(Pressure ~ File, All_data, max)
Peak_data <-merge(df1, df2, by="File")
Peak_data <- inner_join(Peak_data, All_data)
Peak_data <- select(Peak_data, -c("Force1","Force2","Sum_force"))
rm(df1,df2)

depth_list <- str_extract_all(as.matrix(Peak_data["File"]),"_depth.*?_", simplify=TRUE)
depth_list <- str_extract_all(depth_list,"[[:digit:]]+", simplify=TRUE)
depth_list <- as.data.frame(as.numeric(depth_list))
colnames(depth_list)<-"Depth"
Peak_data <- bind_cols(Peak_data, depth_list)

test_list <- str_extract_all(as.matrix(Peak_data["File"]),"_test.*?_", simplify=TRUE)
test_list <- str_extract_all(test_list,"[[:digit:]]+", simplify=TRUE)
test_list <- as.data.frame(as.numeric(test_list))
colnames(test_list)<-"Test_num"
Peak_data <- bind_cols(Peak_data, test_list)

depth_list <- str_extract_all(as.matrix(Peak_data["File"]),"_depth.*?_", simplify=TRUE)
depth_list <- str_extract_all(depth_list,"[[:digit:]]+", simplify=TRUE)
depth_list <- as.data.frame(as.numeric(depth_list))
colnames(depth_list)<-"Depth"
Peak_data <- bind_cols(Peak_data, depth_list)



