## ----Animal IDs---------------------------------------------------------------
# Get directory; assumes the R script is in the parent folder of Worked example data
pth<-getwd()  

# Set sub folder name containing your data
Extrct_Fold="Worked example data/Example1_tri-chromat_no_UV/Test Data"

# Paste the two locations together to specify your global data location
loc<-paste(pth,Extrct_Fold, sep="/")

# Detect species -these could also be locations depending on your data set.
Species=list.files(path=loc) 

Species# Should contain species folders and a log file with QCPA processing details

# Here, we use the "Ap" pattern in both species names to subset the Species list
Species<-Species[grepl("Ap", Species)]# Removes the log file from vector

# Note: For multiple different species patterns, can search using the or condition '|'
# e.g., Species<-Species[grepl("A|B", Species)], returns species with pattern 'A' or 'B'

Species


## ----Animal Loop--------------------------------------------------------------
Animal_Info<-data.frame(NULL)# First create an empty data frame

# Then create a loop to iterate over the species folders,
# extracting viewing distances and individual animal IDs 
for (i in 1:length(Species)) {# Here i will be either 1 or 2
    Ind_pth<-paste0(loc, "/",Species[i])# Pastes the ith species from the Species object
    Indviduals_list<-list.files(path=Ind_pth)# Extracts all individual folder names
    
    # Iterate over the number of individuals in each species folder
    for (ind in 1:length(Indviduals_list)){
      # The variable dist lists all the unique viewing distance folder names 
      # identified by "cm"
      Dists<-list.files(paste0(Ind_pth,"/", Indviduals_list[ind]), pattern = "cm")
      
      # Add to Animal_Info a new row with species name, individual and viewing distance 
      rbind(Animal_Info, cbind(Species[i], Indviduals_list[ind], c(Dists)))-> Animal_Info
    }
}
colnames(Animal_Info)<-c("Species", "Ind", "Dist")# Rename the column names in Animal_Info 


## ----Check data, fig.align='center', fig.cap='Figure 1. Counts of individuals within species for each viewing distance.'----
# Check data
Animal_Info

# Convert character data into factors
Animal_Info[,colnames(Animal_Info)] <-lapply(Animal_Info[,colnames(Animal_Info)],factor)

str(Animal_Info)# Observe structure of the data

# Check the number of unique species, individuals and viewing distances
lapply(Animal_Info, nlevels)

# To visualise the experimental design, we first aggregate the data to determine the 
# number (length) of each individual by species and distance
Ani_info_tab<-aggregate(Ind~Species+Dist, data=Animal_Info, length)
barplot(Ind~Dist+Species, beside=TRUE, ylab = "Count",
        legend.text=TRUE, data=Ani_info_tab)


## ----Check ROIs---------------------------------------------------------------
# Create vector containing all the individual animal sub directories 
tmpAnidir<-apply(Animal_Info,1, function (x) paste(unlist(x), collapse = "/"))

# Paste the global file directories to the front of the sub directory paths
tmpAnidir <- paste(loc, tmpAnidir, sep="/") 

# This should highlight the unique regions of interest, if there are
# more than expected - look for naming inconsistencies
unique(unlist(strsplit(list.files(tmpAnidir, pattern=".tif"), "_"))[ c(TRUE,FALSE)])


## ----simple ROIs--------------------------------------------------------------
ROI<-c("animal","animal+background","background")# Add the names of ROIs




## ----ROI----------------------------------------------------------------------
# Here, we create a character vector determining whether animal, 
# background and/or animal+background options were used
# Using the first row of Animal_Info to specify the location to examine 
# the number and types of _Summary Results.csv files 

# Find all the ROI output files for '_Summary Results.csv'
ROI<- list.files(paste(loc,paste(unlist(Animal_Info[1,]), collapse = "/"),
                            sep="/"), pattern = "_Summary Results.csv")

# Here we remove the "_Summary Results.csv" information
ROI<- gsub("_Summary Results.csv", "", ROI)
ROI# In the QCPA batch script, we have investigated three ROIs


## ----merge ROI----------------------------------------------------------------
# Expand each unique row of the Animal_Info data frame to included a unique ROI value
Animal_Info_ROI<-merge(Animal_Info, ROI)

colnames(Animal_Info_ROI)[4]<-"ROI" # Name column 4 to ROI

Animal_Info_ROI$ROI<-factor(Animal_Info_ROI$ROI)# Convert to factor

head(Animal_Info_ROI)# Check data

str(Animal_Info_ROI)# Check that ROI is a factor


## ----reduced function---------------------------------------------------------
# Part A:
# Here, we specify what image analysis so that we can use the correct file extension
exnfile="_Summary Results.csv"

Ani_ID_dat=Animal_Info_ROI # Assign our animal ID data frame

path=loc # This is the global location set in section 2.1

# Part B:
# Once the file extension condition is met, we use lapply to extract the data,
# using a nested function within lapply
Sum_Res_list<-lapply(1:nrow(Ani_ID_dat), # for each row in Ani_ID_dat...
                  function (x){ # start of our nested function
                    
                    # Extract the individual row information
                    tmpLoc<- paste(unlist(Ani_ID_dat[x,]), collapse = "/")
                    
                    # Create a directory string
                    tmpLoc<-paste0(paste(path,tmpLoc, sep="/"), exnfile)
                    
                    # Read in the data using the newly specified location
                    data <- read.csv(tmpLoc)
                    
                    # Add the specific animal information to the data
                    data <- merge(Ani_ID_dat[x,], data)
                    
                    data # Returns data
                  })

# Part C:
# We then collapse the Sum_Res_list list into a data frame by 
# calling rbind in the function do.call
Sum_Res_df<-do.call(rbind, Sum_Res_list)

# Check the imported data
dim(Sum_Res_df)# 18 rows by 133 columns

head(Sum_Res_df)[,1:12]# check the first six rows and first twelve columns


# Finally, to prevent any errors we remove the objects Ani_ID_dat, exnfile, path 
# from the global environment because we'll be recycling them in the second function
rm(exnfile); rm(Ani_ID_dat); rm(path)




## ----readQCPA function--------------------------------------------------------
readQCPA<- function(Ani_ID_dat,filetype="NA",path=loc){
  # loc is assigned by default, but can be changed
  
  # Part A: 
  # Here, we specify arguments for different image analyses so that we can 
  # extract the correct file extension
  if(filetype=="VCA"){
    exnfile="_Summary Results.csv"
  } else if (filetype=="PartAn"){
    exnfile="_Cluster Particle Analysis Summary Results.csv"
  } else if (filetype=="Clust"){
    exnfile="_Cluster Results.csv"
  } else if (filetype=="IndParticle"){
    exnfile="_Individual Particle Results.csv"
  } else {
    stop('Specify analysis output type: e.g., 
         filetype =\n"VCA"\n"PartAn"\n"Clust"\n"IndParticle"')
  }# stop command halts the function and specifies how the user went wrong
  
  # Part B: 
  # Once the file extension condition is met, we use lapply to extract the data,
  # using a nested function within lapply
  data_list<-lapply(1:nrow(Ani_ID_dat), function (x){# for each row in Ani_ID_dat...
    
    #extract the individual row information
    tmpLoc<- paste(unlist(Ani_ID_dat[x,]), collapse = "/")
    
    # create a directory string
    tmpLoc<-paste0(paste(path,tmpLoc, sep="/"), exnfile)
    
    # read in the data using the newly specified location
    data <- read.csv(tmpLoc)
    
    # add the specific animal information to the data
    data <- merge(Ani_ID_dat[x,], data)
    
    data #returns data
  })
  
  # Part C: 
  # We then collapse the data_list list into a data frame by calling rbind 
  # in the function do.call
  data_df<-do.call(rbind, data_list)
  
  return(data_df)# outputs the data frame
}


## ----empty readQCPA, error=TRUE-----------------------------------------------
readQCPA(Animal_Info_ROI)# We include only the animal id data set


## ----implementing readQCPA----------------------------------------------------
# VCA,BSA,CAA analysis:
# Read in and assign analysis
VCA_analysis<-readQCPA(Animal_Info_ROI, filetype = "VCA") 

#head(VCA_analysis)# Inspect data

# Subset data excluding the column Image and X
VCA_analysis<-VCA_analysis[, !colnames(VCA_analysis) %in% c("X", "Image")]

dim(VCA_analysis)# 18 rows, by 131 columns

VCA_analysis[1:2,]# Observe the first two rows of data

# Cluster Particle Analysis:
# Read in and assign analysis
Particle_analysis<-readQCPA(Animal_Info_ROI, filetype = "PartAn") 

#head(Particle_analysis)# Inspect data

dim(Particle_analysis)# 52 rows, by 20 columns

colnames(Particle_analysis)[5]

# Change column name to be consistent with Cluster analysis
colnames(Particle_analysis)[5]<-"ClusterID" 

Particle_analysis[1:2,]# Observe the first two rows of data


# Cluster Results:
# Read in and assign analysis
Cluster_analysis<-readQCPA(Animal_Info_ROI, filetype = "Clust")

#head(Cluster_Analysis)# inspect data

# Subset data excluding the column Image and X
Cluster_analysis<-Cluster_analysis[, !colnames(Cluster_analysis) %in% c("X", "Image")]

dim(Cluster_analysis)# 52 rows, by 21 columns

Cluster_analysis[1:2,]# Observe the first two rows of data

# Individual Particle Analysis:
# Read in and assign analysis
IndParticle_analysis<-readQCPA(Animal_Info_ROI, filetype = "IndParticle")

#head(Cluster_Analysis)# inspect data

# Subset data excluding the column X.1
IndParticle_analysis<-IndParticle_analysis[,colnames(IndParticle_analysis)!="X.1"]

dim(IndParticle_analysis)# 143 rows, by 23 columns

IndParticle_analysis[1:2,]# Observe the first two rows of data


## ----LEIA CSV Data------------------------------------------------------------
LEIA_Res_list<-lapply(1:nrow(Animal_Info_ROI), # For each row in Animal_Info_ROI
                  function (x){ # Start of our nested function
                    
                    # Convert row into a character vector
                    tmpLoc <- as.character(unlist(Animal_Info_ROI[x,]))
                    
                    # Add and order the new sub folder LEIA before ROI
                    tmpLoc <- c(tmpLoc, "LEIA")[c(1:3,5,4)]
                    
                    # Paste tmploc into a directory character
                    tmpLoc <- paste(tmpLoc, collapse = "/")
                    
                    # Create a directory string
                    tmpLoc <-paste0(paste(loc,tmpLoc, sep="/"), 
                                    "/_Local Edge Intensity Analysis.csv")
                    
                    # Read in the data using the newly specified location
                    data <- read.csv(tmpLoc)
                    
                    # Add the specific animal information to the data
                    data <- merge(Animal_Info_ROI[x,], data)
                    
                    data #returns data
                  })

# We then collapse the LEIA_Res_list list into a data frame by 
# calling rbind in the function do.call
LEIA_Res_analysis<-do.call(rbind, LEIA_Res_list)

# Subset data excluding the column Image and X
LEIA_Res_analysis<-LEIA_Res_analysis[, !colnames(LEIA_Res_analysis) %in% c("X", "Image")]

# Check the imported data
dim(LEIA_Res_analysis)# 18 rows by 37 columns

LEIA_Res_analysis[1:2,]# Observe the first two rows of data


## ----GabRat CSV Data----------------------------------------------------------
# Because we are not using ROI which adds three levels (rows), 
# we trim data frame using unique()
# For each the 6 rows in Animal_Info_ROI
GabRat_Res_list<-lapply(1:nrow(unique(Animal_Info_ROI[,1:3])), 
                  function (x){ # Start of our nested function
                    
                    # Using the first three columns, convert row into a character vector
                    tmpLoc <- as.character(unlist(Animal_Info_ROI[x,1:3]))
                    
                    tmpLoc <- c(tmpLoc, "GabRat")# Add the new sub folder GabRat
                    
                    # Paste tmploc into a directory character
                    tmpLoc <- paste(tmpLoc, collapse = "/")
                    
                    # Create a directory string
                    tmpLoc <-paste0(paste(loc,tmpLoc, sep="/"), "/_GabRat_Results.csv")
                    
                    # Read in the data using the newly specified location
                    data <- read.csv(tmpLoc)
                    
                    # Add the specific animal information to the data,
                    # using only the first three columns
                    data <- merge(Animal_Info_ROI[x,1:3], data)
                    
                    data # Returns data
                  })

# We then collapse the LEIA_Res_list list into a data frame by calling 
# rbind in the function do.call
GabRat_Res_analysis<-do.call(rbind, GabRat_Res_list)

# Check the imported data
dim(GabRat_Res_analysis)# 72 rows by 7 columns
head(GabRat_Res_analysis)# We only want every ~ fourth row for the dbl values

# Set up a true/false condition on whether row has the pattern "_dbl",
# then subset data on this condition
GabRat_Res_analysis<-GabRat_Res_analysis[endsWith(GabRat_Res_analysis$ID, "_dbl"),]
rownames(GabRat_Res_analysis)<-NULL # Reset row numbers

# Then we want to determine ROI where we split the ID column 
# data on the shared pattern "WholeImage_"
# We then only take the second elemnt, using a FALSE,TRUE index 
GabRat_Res_analysis$ROI<-unlist(
  strsplit(GabRat_Res_analysis$ID, "WholeImage_"))[c(FALSE,TRUE)]

# Then remove the '_dbl'
GabRat_Res_analysis$ROI<-gsub("_dbl","", GabRat_Res_analysis$ROI)

# Remove unnecessary columns, subset data excluding the column X
GabRat_Res_analysis<-GabRat_Res_analysis[, !colnames(GabRat_Res_analysis) 
                                         %in% c("X","ID", "Sigma")]

# Check the imported data
dim(GabRat_Res_analysis)# 18 rows by 5 columns

GabRat_Res_analysis[1:2,]# Observe the first two rows of data


## ----merge two particle analyses files----------------------------------------
# Merging two files together
Cluster_Particle_analysis<-merge(Cluster_analysis, Particle_analysis, 
                                 by=c("Species","Ind","Dist","ROI", "ClusterID"))

dim(Cluster_Particle_analysis)# 52 rows and 36 columns

Cluster_Particle_analysis[1:2,]# Inspect data


## ----combine VCA LEIA and GabRat----------------------------------------------
# Merge the first two data frames (VCA_analysis and LEIA_Res_analysis) together
VCA_LEIA_GabRat_analysis<-merge(VCA_analysis, LEIA_Res_analysis, 
                                by=c("Species","Ind","Dist","ROI"))

VCA_LEIA_GabRat_analysis[1:2,]# Inspect data
dim(VCA_LEIA_GabRat_analysis)# 18 rows and 164 columns

# Merge the new data frame (VCA_LEIA_GabRat_analysis) with GabRat_Res_analysis
VCA_LEIA_GabRat_analysis<-merge(VCA_LEIA_GabRat_analysis, GabRat_Res_analysis,
                                by=c("Species","Ind","Dist","ROI"))

VCA_LEIA_GabRat_analysis[1:2,]# Inspect data again

dim(VCA_LEIA_GabRat_analysis)# 18 rows and 165 columns


## ----Removing NAs-------------------------------------------------------------
# Specify column names
ColumnNames<-c("Species","Ind","Dist","ROI","BSA.BsL.Hrz","Lum.mean",
               "CAA.Scpl","Lum.CoV","BSA.BML","Col.mean","GabRat")

# Subset data by column names
V.L.GbRt_subset<-VCA_LEIA_GabRat_analysis[, colnames(VCA_LEIA_GabRat_analysis) 
                                          %in% ColumnNames]

dim(V.L.GbRt_subset)# 18 rows by 11 columns
summary(V.L.GbRt_subset) # Initial summary of the data

# View the data frame
View(V.L.GbRt_subset)

# Replace infinite values with NA using sapply
V.L.GbRt_subset[sapply(V.L.GbRt_subset, is.infinite)] <- NA

View(V.L.GbRt_subset)# Observe change the data frame

# Remove NA/NaNs
V.L.GbRt_subset<-na.omit(V.L.GbRt_subset)

View(V.L.GbRt_subset)# Observe change the data frame

dim(V.L.GbRt_subset)# 11 rows by 11 columns

summary(V.L.GbRt_subset)
apply(V.L.GbRt_subset[,5:11], 2, var) # Determine the colour variable variances


## ----Normalising and standardising data---------------------------------------
par(mfrow=c(3,3))
Colnames_subset<-colnames(V.L.GbRt_subset[,5:11])
for (i in Colnames_subset) {
  hist(V.L.GbRt_subset[,i], breaks = 10, 
       main=i, xlab="")
}

# Test for normality
lapply(V.L.GbRt_subset[,5:11], shapiro.test)



# Inspect scaled values and column means
scale(V.L.GbRt_subset[,5:11], center = TRUE, scale = TRUE)

# Update the V.L.GbRt_subset data frame with the standardised values 
V.L.GbRt_subset[,5:11]<-scale(V.L.GbRt_subset[,5:11], center = TRUE, scale = TRUE)

# Check standardisation
summary(V.L.GbRt_subset)# Mean values are zero
apply(V.L.GbRt_subset[,5:11], 2, var) # Confirm variances are 1

# # Correlation panel
# panel.cor <- function(x, y){
#     usr <- par("usr"); on.exit(par(usr))
#     par(usr = c(0, 1, 0, 1))
#     r <- round(cor(x, y), digits=2)
#     txt <- paste0("R = ", r)
#     text(0.5, 0.5, txt, cex=1.5)
# }
# 
# my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")
# # Scatterplot panel
# panel.scat <- function(x, y){
# points(x,y, col=my_cols[subset$ROI],
#     cex=1.1, pch=19)
# }
# 
# par(oma=c(0,0,0,10), xpd=TRUE)
# pairs(subset[,5:10],
#       main="Six colour variables for two viewing distances",
#       upper.panel = panel.cor,
#       lower.panel = panel.scat,  gap=0)
# 
# Colour_ID<-unique(data.frame(as.character(subset$ROI),my_cols[subset$ROI]))
# legend(0.7, 0.8,Colour_ID[,1], pch =19,cex=1.1,bty="n", col=Colour_ID[,2])
# 
# library(corrplot)
# corrplot(cor(subset[,5:10]))
# 
# dim(VCA_LEIA_GabRat_analysis)# 18 rows and 165 columns
# summary(VCA_LEIA_GabRat_analysis)


## ----Output files-------------------------------------------------------------
dir.create(paste(c(loc, "Output"), collapse = "/"))# Create the Output directory

Out_path = paste(c(loc, "Output"), collapse = "/")# Save Output directory path

# For a single file:
write.csv(GabRat_Res_analysis, row.names = FALSE, # Remove row names
          file=paste(c(Out_path, "GabRat_Res_analysis.csv"), 
                     collapse = "/"))# Save to the folder Output in Test Data

# For multiple files:
savefile_list<-ls(pattern = "analysis")# Save all objects with "analysis" in the name

savefile_list# Inspect object names

# Iterate over each data frame in savefile_list
for (i in savefile_list){
  
  # Create file name and add the .csv extension
  # Add date of creation using format(Sys.time(), "_%d_%b_%Y")
  # Remove "_analysis" using gsub and add "_data" and date created
  fname=paste0(gsub("_analysis", "", i), "_data",format(Sys.time(), "_%d_%b_%Y"),".csv")
  
  # Save to the folder Output in Test Data
  write.csv(GabRat_Res_analysis, row.names = FALSE, # Remove row names
          file=paste(c(Out_path, fname), collapse = "/"))
}

# Check that your files have saved
list.files(Out_path)


## ----data.table---------------------------------------------------------------
library(data.table)


