
# Visualizing Time Series Data - R Project
# Author: Silvan Steiner
# Date: 12.04.2018

# Description: 

# R code for the visualization of time series data using ggplot. The variable names in this code are written for the 
# vegetation index NDVI but can easily be changed to fit to any other index. After the calculation of basic statistics, a raster plot, 
# a line plot and detailed statistics are plotted for each time step. All three plots are aggregated into one plot per time step.
# Finally, all time steps are animated as a GIF.

# Input: The user is expected to name the input in one of two possible ways: either "nameofindex_YYYYMMDD.format" 
# or nameofindex_DOY.format". The input file format must be specified manually.

# IMPORTANT: Please change the directories according to your requirements, especially line 41 (working directory),
# line 44 (folder with input data within wd), line 77 (output of table),  lines 115-118 (dont change the last folder name),
# line 138, 162, 185 and 196 for plot output, 213 for plot input and 216 for animation output


#############################################################
# First steps and calculation of statistics
#############################################################


# Install and load required packages 
#(Credits therefore to Steven Worthington, https://gist.github.com/stevenworthington/3178163 )

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("RStoolbox","ggplot2","ggpubr","raster","magick")
ipak(packages)

# Set general working directory 
getwd()
setwd("E:/R_project/Data Trial/")

# List the files in the index directory, set the file format, and stack the data
all_ndvi <- list.files("All", full.names = T, pattern=".tif")
Ndvi_stack <- stack(all_ndvi)

# save the names in a variable
nm <- names(Ndvi_stack)

# Calculate the number of cells in the rasterand take a random sample cell values from the first stack layer
# The sample size is specified as 1/10 of the total cell number, na values are removed
nCell <- ncell(Ndvi_stack)
Sample_Value <- sampleRandom(Ndvi_stack[[1]],nCell/10,na.rm=TRUE,cells=T,sp=T)
Sample_Value$cell

# Create a matrix built up of seven rows and the number of time steps as columns
mat <- matrix(nrow = 7,ncol=nlayers((Ndvi_stack))) 

# fill the matrix with statistical measures: mean, standard deviation, 25% quantile, 75% quantile,
# maximum and minimum values of each layer and the median of the random sample

for (i in 1:nlayers(Ndvi_stack)){
  print(i)
  
  mat[1,i] <- cellStats(Ndvi_stack[[i]],stat='mean')
  mat[2,i] <- cellStats(Ndvi_stack[[i]],stat='sd')
  mat[3,i] <- median(Ndvi_stack[[i]][Sample_Value$cell],na.rm=T)
  mat[4,i] <- quantile(Ndvi_stack[[i]],probs=0.25)
  mat[5,i] <- quantile(Ndvi_stack[[i]],probs=0.75) 
  mat[6,i] <- round(maxValue(Ndvi_stack[[i]]),digits=4)
  mat[7,i] <- round(minValue(Ndvi_stack[[i]]),digits=4) 
  print(paste("End of",i))
}

# Show Mean, Sd, Median, 25quant, 75quant, Max, Min and save statistics in a table
mat 
write.csv(mat, file="All/Table.csv")

# Additionally, save each statistical measure in a variable
Mean        <- mat[1,]
Sd          <- mat[2,]
Median      <- mat[3,]
quantile_25 <- mat[4,]
quantile_75 <- mat[5,]
Max         <- mat[6,]
Min         <- mat[7,]


# The DOY is extracted from the file names using a loop
# if nchar(D[i]) == 8 then the data naming is YYYYMMDD, otherwise its DOY

D <- character(length = length(nm))
for (i in 1:length(nm)){D[i] <- strsplit(nm,"_")[[i]][2]}

 if (nchar(D[i]) == 8) {
   print(paste("Date format is YYYYMMDD"))
   D <- as.Date(D,"%Y%m%d")
   doy <- as.numeric(strftime(D,format = "%j"))
   
 } else {
  print(paste("Date format is DOY"))
  doy <- as.numeric(D)
 }

# Create a data frame of the time data and the statistical measures
data <- data.frame(doy, Median, quantile_25,quantile_75)

###################################################################
# Main loop creating raster plot, line plot and statistics
# Aggregation of plots
###################################################################

# Create an output folder for each plot 

dir.create("All/Raster_Plot")
dir.create("All/Line_Plot")
dir.create("All/ARA_Plot")
dir.create("All/Details")

# Main loop

for(i in 1:nlayers(Ndvi_stack)){

  print(paste("Start with:",i))

# The variable p is where the raster plot of each time step is saved. 
# Stretch and quantiles for visualization can specified or omitted. 
# A colour scheme ad the upper and lower limits of the legend can be set.
# The legend title and the raster plot title can be specified as wished.
  

p <- print(ggR(Ndvi_stack[[i]],geom_raster = T,stretch="hist",quantiles=c(0.05,0.95))+
          scale_fill_gradientn(colours = terrain.colors(5),limits=c(-0.5,1),
          space = "Lab",name=paste("NDVI \n"))+
          theme(plot.title=element_text(color="red",hjust =0.5))+
          labs(title= paste("Raster Plot NDVI:",nm[[i]])))

ggsave(p$plot, filename= paste("All/Raster_Plot/Plot_",sep ="",nm[i],".png"))


# The line plot is saved in the the variable a. In the line plot, the developement
# over time of the 25% quantile, the 75% quantile and the median is plotted. A vertical 
# line indicates the position of each time step. Colours, aesthics and titles can be specified.


a <- print(ggplot(data, aes(doy)) +
                  ylab("Value")+
                  xlab("DOY")+
                  geom_line(aes(y=Median,colour='Median')) +
                  geom_line(aes(y=quantile_25,colour='25 Quantile'))+
                  geom_line(aes(y=quantile_75,colour='75 Quantile'))+
                  scale_colour_manual("",
                  breaks=c("Median","25 Quantile", "75 Quantile"),
                  values=c("red","blue","green"))+
                  theme(legend.position = "top",legend.justification =c("center") )+
                  geom_segment(aes(x=doy[[i]],y=0, xend=doy[[i]], yend=quantile_75[[i]]),
                  linetype="dashed")+
                  theme(plot.title=element_text(color='red',hjust =0.5))+
                  labs(title="Line Plot NDVI"))


ggsave(a$plot, filename= paste("All/Line_Plot/Line_Plot_",sep="",nm[i],".png"))

# The calculated statistics of each time step are listed together in the variable text.
# This variable is then transferred into a blank ggplot. 

text <- paste("\n DOY:",doy[i],"\n \n",
            "Minimum NDVI:",Min[i],"\n",
            "Maximum NDVI:",Max[i],"\n",
            "Mean NDVI:",round(Mean[i],digits = 4),"\n",
            "Median NDVI:",round(Median[i],digits=4),"\n",
            "StDev:", round(Sd[i], digits=4),"\n",
            "75 Quantile is:",round(quantile_75[i],digits = 4),"\n",
            "25 Quantile is:",round(quantile_25[i],digits = 4),"\n")

Details <-print(ggplot()+ 
        annotate("text",x = 1,y = 8,size =5,label=text,hjust=0)+
        theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y=element_blank(),axis.text.x=element_blank(),
        axis.title.x = element_blank(),axis.title.y = element_blank(),
        axis.line = element_blank(),axis.ticks = element_blank()))

ggsave(Details$plot, filename= paste("All/Details/Details_",sep="",nm[i],".png"))

# Using ggarrange, all three plots are aggregated. The raster plot is
# specified to cover two rows and one column, both other plots are in one 
# column and one row each

ARA <- print(ggarrange(p$plot,
                 ggarrange(a$plot,Details$plot,nrow=2,labels = c("B", "C")),
                 labels = "A",ncol=2))
x11(30,20)
print(ARA)
savePlot(filename = paste("All/ARA_Plot/Ara_Plot_",nm[i],sep=""), type="png",
         device=dev.cur(),restoreConsole = T )

dev.off() 
print(paste("End of:",i))

if (i == nlayers(Ndvi_stack)) {print(paste("Finished Loop"))}

}

#############################################################
# Animation of all aggregated plots
#############################################################

# The aggregated plots are read and animated. Two animation arguments were specified,
# namely delay and loop. Animation is saved as a GIF.
        
all_ARA <- paste0(list.files("E:/R_project/Data Trial/All/ARA_Plot", full.names = T, pattern=".png"), collapse = " ")
all_ARA

system(paste0('"E:/Image_Magick/convert.exe" -loop 5 -delay 100 ', all_ARA, 'All/Plot_test.gif'))

