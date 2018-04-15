
# Small ADDITIONAL code for the calculation of NDVI or other index that can be calculated using spectralIndices()
# (Input bands might change). Nir and Red data is expected to be in the format "band_YYYYMMDD.dataformat" or 
# "band_DOY_dataformat"

# Please speficy all directories to your requirements!

# Install and load required packages 
#(Credits to Steven Worthington, https://gist.github.com/stevenworthington/3178163 )

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("RStoolbox","snow","raster")
ipak(packages)

# Set general working directory 
getwd()
setwd("E:/R_project/Data Trial/All/")

all_nir <- list.files("nir", full.names = T, pattern=".tif")
all_red <- list.files("red", full.names = T, pattern=".tif")
Nir_stack <- stack(all_nir)
Red_stack <- stack(all_red)
Stack_all <- stack(Red_stack,Nir_stack)



# The DOY is extracted from the file names using a loop
# if nchar(D[i]) == 8 then the data naming is YYYYMMDD, otherwise its DOY

N_Red_stack <- names(Red_stack)
D <- character(length = length(N_Red_stack))
for (i in 1:length(N_Red_stack)){D[i] <- strsplit(N_Red_stack,"_")[[i]][2]}

if (nchar(D[i]) == 8) {
  print(paste("Date format is YYYYMMDD"))
  D <- as.Date(D,"%Y%m%d")
  doy <- as.numeric(strftime(D,format = "%j"))
  
} else {
  print(paste("Date format is DOY"))
  doy <- as.numeric(D)
}

doy

# Calculation of NDVI 
beginCluster()
for ( i in 1:length(N_Nir_stack)){
NDVI <- spectralIndices(Stack_all, red=names(Red_stack[[i]]),nir=names(Nir_stack[[i]]),indices = "NDVI",
                             filename=paste("All/ndvi_",a[i],".tif"), format="GTiff", overwrite=T)
}
endCluster()
