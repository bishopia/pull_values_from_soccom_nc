### take downloaded soccom .nc file and crop and get time- and depth-averaged values for a set of coordinates
## ian bishop
## may 26, 2023

interact -t 1:00:00 -m 96G
conda activate soccom

#crop by z and lat at once, need at least 96G
ncks -d Z,0,11 -d YC,0,300 Fe_bsoseI139_2013to2021_5dy.nc highlatsurfaceFe.nc
#check you've got the right depths and lats here, iterate:
ncdump -v YC highlatsurfaceFe.nc

#then crop to DJF
input_file="highlatsurfaceFe.nc"
output_file="highlatsurfaceFe_DJF.nc"
# select the months (Dec, Jan, Feb)
cdo selmon,12,1,2 "$input_file" "$output_file"

#or for specific dates/season
input_file="highlatsurfaceFe.nc"
output_file="highlatsurfaceFe_1617season.nc"
start_date="2016-12-01"
end_date="2017-02-27"
# Select the time steps
cdo seldate,"$start_date","$end_date" "$input_file" "$output_file"


###now take mean over time for each 3D coordinate
#these inputs outputs from here on are different depending on the scope you want
#input_file="highlatsurfaceFe_DJF.nc"
#input_file="highlatsurfaceFe.nc"
input_file="highlatsurfaceFe_1617season.nc"
#output_file="highlatsurfaceFe_DJF_timeaverage.nc"
#output_file="highlatsurfaceFe_annual_timeaverage.nc"
output_file="highlatsurfaceFe_1617_timeaverage.nc"

# Calculate the mean across time
cdo timmean "$input_file" "$output_file"


###now take mean over depth (Z)
#input_file="highlatsurfaceFe_DJF_timeaverage.nc"
#input_file="highlatsurfaceFe_annual_timeaverage.nc"
input_file="highlatsurfaceFe_1617_timeaverage.nc"
#output_file="highlatsurfaceFe_DJF_timeanddepthaverage.nc"
#output_file="highlatsurfaceFe_annual_timeanddepthaverage.nc"
output_file="highlatsurfaceFe_1617_timeanddepthaverage.nc"

# Calculate the mean across depth
ncwa -a Z "$input_file" "$output_file"


#now you have a single value for each lat/long coordinate, time- (DJF over last 10 years) and depth- (top 100M) averaged

#load R
R

#libraries
library(tidyverse)
library(ncdf4)

#input_file <- "highlatsurfaceFe_DJF_timeanddepthaverage.nc"
#input_file <- "highlatsurfaceFe_annual_timeanddepthaverage.nc"
input_file <- "highlatsurfaceFe_1617_timeanddepthaverage.nc"

#open nc
nc <- nc_open(input_file)

#get dimensions
lat <- ncvar_get(nc, "YC")
lon <- ncvar_get(nc, "XC")

#get TRAC06 variable
trac06 <- ncvar_get(nc, "TRAC06")

#create a matrix
matrix_data <- matrix(trac06, nrow = length(lat), ncol = length(lon), byrow = TRUE)

#close nc file
nc_close(nc)



#assuming you have the 'matrix_data' from the previous step

#convert matrix to a df
df <- as.data.frame(matrix_data)
colnames(df) <- lon
df$lat <- lat

#convert to long form
df_long <- df %>% 
  pivot_longer(cols = -c(lat), names_to = "lon", values_to = "value") %>%
  mutate(LONGITUDE=as.numeric(lon), LATITUDE=as.numeric(lat)) %>%
  dplyr::select(-lon, -lat) %>%
  dplyr::select(LONGITUDE, LATITUDE, FE=value)

#export
write.csv(df_long, file = "output.csv", row.names = FALSE)


#declare function to collect value by lat long ROI
filter_by_coordinate <- function(df, lat, lon) {
  filtered_df <- df %>%
    filter(abs(LATITUDE - lat) <= 1, abs(LONGITUDE - lon) <= 1)

  mean_value <- mean(filtered_df$FE, na.rm = TRUE)
  
  return(mean_value)
}

#load station lat long
aa <- read.csv("aa_stations.csv")  # Replace with the actual file name or data frame

#add FE value per station
aa <- aa %>%
  rowwise() %>%
  mutate(FE = filter_by_coordinate(df_long, LATITUDE, LONGITUDE))

#export table df
to_export <- aa %>%
    select(STATION=STNNBR, FE)

#export to_export
write.csv(to_export, file = "aa_stations_Fe2.csv", row.names = FALSE)

