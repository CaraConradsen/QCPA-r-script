## ----Animal IDs--------------------------------------------------------------------------------------------------------------------------------------
pth<-getwd() #get directory; assumes the R script is saved in the parent folder of Worked example data
 
Extrct_Fold="Worked example data/Example1_tri-chromat_no_UV/Test Data" #set sub folder name containing your data

loc<-paste(pth,Extrct_Fold, sep="/")#Paste the two locations together to specify your global data location

Species=list.files(path=loc) #detects what species are in the data set. These could also be locations depending on your data set.

Species# should contain species folders and a log file

Species = Species[-length(Species)] #removes the log file

Species


## ----Animal Loop-------------------------------------------------------------------------------------------------------------------------------------
Animal_Info<-data.frame(NULL)# first create an empty data frame

# Then create a loop to iterate over the species folders, extracting viewing distances and individual animal IDs 
for (i in 1:length(Species)) {# here i will be either 1 or 2
    Ind_pth<-paste0(loc, "/",Species[i])# this pastes the ith species name from the Species object
    Indviduals_list<-list.files(path=Ind_pth)# extracts all individual folder names
    for (ind in 1:length(Indviduals_list)){# iterate over the number of individuals in each species folder
      # the variable dist lists all the unique viewing distance folder names identified by the "cm"
      Dists<-list.files(paste0(Ind_pth,"/", Indviduals_list[ind]),pattern = "cm")
      #we then add to Animal_Info a new row containing the species name, individual and viewing distance 
      rbind(Animal_Info, cbind(Species[i], Indviduals_list[ind], c(Dists)))-> Animal_Info
    }
}
colnames(Animal_Info)<-c("Species", "Ind", "Dist")# rename the column names in Animal_Info 


## ----Check data--------------------------------------------------------------------------------------------------------------------------------------
#check data
Animal_Info
Animal_Info[,colnames(Animal_Info)] <-lapply(Animal_Info[,colnames(Animal_Info)],factor) # Convert character data into factors
str(Animal_Info)# observe structure of the data
lapply(Animal_Info, nlevels)# Check the number of unique species, individuals and viewing distances

# To visualize the experimental design we first aggregate the data to determine the number (length) of each individual by species and distance
Ani_info_tab<-aggregate(Ind~Species+Dist, data=Animal_Info, length)
barplot(Ind~Dist+Species, beside=TRUE, legend.text=TRUE, data=Ani_info_tab)


## ----Check ROIs--------------------------------------------------------------------------------------------------------------------------------------
tmpAnidir<-apply(Animal_Info,1, function (x) paste(unlist(x), collapse = "/"))# create vector containing all the individual animal sub directories 
tmpAnidir <- paste(loc, tmpAnidir, sep="/") # paste the global file directories to the front of the sub directory paths
unique(unlist(strsplit(list.files(tmpAnidir, pattern=".tif"), "_"))[ c(TRUE,FALSE)])# This should highlight the unique regions of interest, if there are more than expected - look for naming inconsistencies


## ----simple ROIs-------------------------------------------------------------------------------------------------------------------------------------
ROI<-c("animal","animal+background","background")# add the names of ROIs




## ----ROI---------------------------------------------------------------------------------------------------------------------------------------------
# Here we create a character vector determining whether animal, 
# background and/or animal+background options were used
# Using the first row of Animal_Info to specify the location to examine the number and types of _Summary Results.csv files 
ROI<- list.files(paste(loc,paste(unlist(Animal_Info[1,]), collapse = "/"),
                            sep="/"), pattern = "_Summary Results.csv")# find all the ROI output files for '_Summary Results.csv'
ROI<- gsub("_Summary Results.csv", "", ROI)# Here we remove the "_Summary Results.csv" information
ROI# In the QCPA batch script, we have investigated three ROIs


## ----merge ROI---------------------------------------------------------------------------------------------------------------------------------------
Animal_Info_ROI<-merge(Animal_Info, ROI)# Expand each unique row of the Animal_Info data frame to included a unique ROI value
colnames(Animal_Info_ROI)[4]<-"ROI" #Name column 4 to ROI
Animal_Info_ROI$ROI<-factor(Animal_Info_ROI$ROI)
head(Animal_Info_ROI)# check data
str(Animal_Info_ROI)# check that ROI is a factor


## ----reduced function--------------------------------------------------------------------------------------------------------------------------------
# Part A.
# here we specify what image analysis so that we can use the correct file extension
exnfile="_Summary Results.csv"
Ani_ID_dat=Animal_Info_ROI #assign our animal ID data frame
path=loc # this is the global location set in section 2.1

# Part B.
# once the file extension condition is met, we use lapply to extract the data,
# using a nested function within lapply
Sum_Res_list<-lapply(1:nrow(Ani_ID_dat), # for each row in Ani_ID_dat...
                  function (x){ # start of our nested function
                    tmpLoc<- paste(unlist(Ani_ID_dat[x,]), collapse = "/")#extract the individual row information
                    tmpLoc<-paste0(paste(path,tmpLoc, sep="/"), exnfile)# create a directory string
                    data <- read.csv(tmpLoc)# read in the data using the newly specified location
                    data <- merge(Ani_ID_dat[x,], data)# add the specific animal information to the data
                    data #returns data
                  })
# Part C.
#we then collapse the Sum_Res_list list into a data frame by calling rbind in the function do.call
Sum_Res_df<-do.call(rbind, Sum_Res_list)

#check the imported data
dim(Sum_Res_df)# 18 rows by 133 columns
head(Sum_Res_df)[,1:12]# check the first six rows and first twelve columns, trimmed due to space


# Finally, to prevent any errors we remove the objects Ani_ID_dat, exnfile, path 
# from the global environment because we'll be recycling them in the second function
rm(exnfile); rm(Ani_ID_dat); rm(path)




## ----readQCPA function-------------------------------------------------------------------------------------------------------------------------------
readQCPA<- function(Ani_ID_dat,filetype="NA",path=loc){
  #loc is assigned by default, but can be changed
  
  # Part A. 
  # here we specify arguments for different image analyses so that we can extract the correct file extension
  if(filetype=="VCA"){
    exnfile="_Summary Results.csv"
  } else if (filetype=="PartAn"){
    exnfile="_Cluster Particle Analysis Summary Results.csv"
  } else if (filetype=="Clust"){
    exnfile="_Cluster Results.csv"
  } else if (filetype=="IndParticle"){
    exnfile="_Individual Particle Results.csv"
  } else {
    stop('Please specify analysis output type: e.g., filetype =\n"VCA"\n"PartAn"\n"Clust"\n"IndParticle"')
  }# stop command halts the function and specifies how the user went wrong
  
  # Part B. 
  # once the file extension condition is met, we use lapply to extract the data,
  # using a nested function within lapply
  data_list<-lapply(1:nrow(Ani_ID_dat), function (x){# for each row in Ani_ID_dat...
    tmpLoc<- paste(unlist(Ani_ID_dat[x,]), collapse = "/")#extract the individual row information
    tmpLoc<-paste0(paste(path,tmpLoc, sep="/"), exnfile)# create a directory string
    data <- read.csv(tmpLoc)# read in the data using the newly specified location
    data <- merge(Ani_ID_dat[x,], data)# add the specific animal information to the data
    data #returns data
  })
  
  # Part C. 
  #we then collapse the data_list list into a data frame by calling rbind in the function do.call
  data_df<-do.call(rbind, data_list)
  return(data_df)#outputs the data frame
}


## ----empty readQCPA, error=TRUE----------------------------------------------------------------------------------------------------------------------
readQCPA(Animal_Info_ROI)# we include only the animal id dataset


## ----implementing readQCPA---------------------------------------------------------------------------------------------------------------------------
# VCA,BSA,CAA analysis
VCA_analysis<-readQCPA(Animal_Info_ROI, filetype = "VCA") # read in and assign analysis 
#head(VCA_analysis)# inspect data
VCA_analysis<-VCA_analysis[, !colnames(VCA_analysis) %in% c("X", "Image")]# subset data excluding the column Image and X
dim(VCA_analysis)# 18 rows, by 131 columns
VCA_analysis[1:2,]#observe the first two rows of data

# Cluster Particle Analysis
Particle_analysis<-readQCPA(Animal_Info_ROI, filetype = "PartAn") # read in and assign analysis
#head(Particle_analysis)# inspect data
dim(Particle_analysis)# 52 rows, by 20 columns
colnames(Particle_analysis)[5]
colnames(Particle_analysis)[5]<-"ClusterID" # change column name to be consistent with Cluster analysis
Particle_analysis[1:2,]#observe the first two rows of data

# Cluster Results
Cluster_analysis<-readQCPA(Animal_Info_ROI, filetype = "Clust") # read in and assign analysis
#head(Cluster_Analysis)# inspect data
Cluster_analysis<-Cluster_analysis[, !colnames(Cluster_analysis) %in% c("X", "Image")]# subset data excluding the column Image and X
dim(Cluster_analysis)# 52 rows, by 21 columns
Cluster_analysis[1:2,]#observe the first two rows of data

# Individual Particle Analysis
IndParticle_analysis<-readQCPA(Animal_Info_ROI, filetype = "IndParticle") # read in and assign analysis
#head(Cluster_Analysis)# inspect data
IndParticle_analysis<-IndParticle_analysis[,colnames(IndParticle_analysis)!="X.1"]# subset data excluding the column X.1
dim(IndParticle_analysis)# 143 rows, by 23 columns
IndParticle_analysis[1:2,]#observe the first two rows of data


## ----LEIA CSV Data-----------------------------------------------------------------------------------------------------------------------------------
LEIA_Res_list<-lapply(1:nrow(Animal_Info_ROI), # for each row in Animal_Info_ROI
                  function (x){ # start of our nested function
                    tmpLoc <- as.character(unlist(Animal_Info_ROI[x,]))# convert row into a character vector
                    tmpLoc <- c(tmpLoc, "LEIA")[c(1:3,5,4)]# Add and order the new sub folder LEIA before ROI
                    tmpLoc <- paste(tmpLoc, collapse = "/")#paste tmploc into a directory character
                    tmpLoc <-paste0(paste(loc,tmpLoc, sep="/"), "/_Local Edge Intensity Analysis.csv")# create a directory string
                    data <- read.csv(tmpLoc)# read in the data using the newly specified location
                    data <- merge(Animal_Info_ROI[x,], data)# add the specific animal information to the data
                    data #returns data
                  })
#we then collapse the LEIA_Res_list list into a data frame by calling rbind in the function do.call
LEIA_Res_analysis<-do.call(rbind, LEIA_Res_list)
LEIA_Res_analysis<-LEIA_Res_analysis[, !colnames(LEIA_Res_analysis) %in% c("X", "Image")]# subset data excluding the column Image and X

#check the imported data
dim(LEIA_Res_analysis)# 18 rows by 37 columns
LEIA_Res_analysis[1:2,]#observe the first two rows of data


## ----GabRat CSV Data---------------------------------------------------------------------------------------------------------------------------------
# because we are not using ROI which adds three levels, we trim data frame using unique()
GabRat_Res_list<-lapply(1:nrow(unique(Animal_Info_ROI[,1:3])), # for each the 6 rows in Animal_Info_ROI
                  function (x){ # start of our nested function
                    tmpLoc <- as.character(unlist(Animal_Info_ROI[x,1:3]))# using only the first three columns, convert row into a character vector
                    tmpLoc <- c(tmpLoc, "GabRat")# Add the new sub folder GabRat
                    tmpLoc <- paste(tmpLoc, collapse = "/")# paste tmploc into a directory character
                    tmpLoc <-paste0(paste(loc,tmpLoc, sep="/"), "/_GabRat_Results.csv")# create a directory string
                    data <- read.csv(tmpLoc)# read in the data using the newly specified location
                    data <- merge(Animal_Info_ROI[x,1:3], data)# add the specific animal information to the data,using only the first three columns
                    data #returns data
                  })
#we then collapse the LEIA_Res_list list into a data frame by calling rbind in the function do.call
GabRat_Res_analysis<-do.call(rbind, GabRat_Res_list)

#check the imported data
dim(GabRat_Res_analysis)# 72 rows by 7 columns
head(GabRat_Res_analysis)# We only want every ~ fourth row for the dbl values

# set up a true/false condition on whether row has the pattern "_dbl"
GabRat_Res_analysis<-GabRat_Res_analysis[endsWith(GabRat_Res_analysis$ID, "_dbl"),]
rownames(GabRat_Res_analysis)<-NULL # reset row numbers

# Then we want to determine ROI where we split the ID column data on the shared pattern "WholeImage_"
# We then only take the second elemnt, using a FALSE,TRUE index 
GabRat_Res_analysis$ROI<-unlist(strsplit(GabRat_Res_analysis$ID, "WholeImage_"))[c(FALSE,TRUE)]
# then remove the '_dbl'
GabRat_Res_analysis$ROI<-gsub("_dbl","", GabRat_Res_analysis$ROI)

# remove unnecessary columns 
GabRat_Res_analysis<-GabRat_Res_analysis[, !colnames(GabRat_Res_analysis) %in% c("X","ID", "Sigma")]# subset data excluding the column X

#check the imported data
dim(GabRat_Res_analysis)# 18 rows by 5 columns
GabRat_Res_analysis[1:2,]#observe the first two rows of data


## ----merge two particle analyses files---------------------------------------------------------------------------------------------------------------
# Merging two files together
Cluster_Particle_analysis<-merge(Cluster_analysis, Particle_analysis, by=c("Species","Ind","Dist","ROI", "ClusterID"))
dim(Cluster_Particle_analysis)# 52 rows and 36 columns
Cluster_Particle_analysis[1:2,]#inspect data


## ----combine VCA LEIA and GabRat---------------------------------------------------------------------------------------------------------------------
# Merge the first two files together
VCA_LEIA_GabRat_analysis<-merge(VCA_analysis, LEIA_Res_analysis, by=c("Species","Ind","Dist","ROI"))
VCA_LEIA_GabRat_analysis[1:2,]# inspect data
dim(VCA_LEIA_GabRat_analysis)# 18 rows and 164 columns

VCA_LEIA_GabRat_analysis<-merge(VCA_LEIA_GabRat_analysis, GabRat_Res_analysis, by=c("Species","Ind","Dist","ROI"))
VCA_LEIA_GabRat_analysis[1:2,]#inspect data again
dim(VCA_LEIA_GabRat_analysis)# 18 rows and 165 columns


## ----Output files------------------------------------------------------------------------------------------------------------------------------------
dir.create(paste(c(loc, "Output"), collapse = "/"))# Create the Output directory
Out_path = paste(c(loc, "Output"), collapse = "/")

# For a single file
write.csv(GabRat_Res_analysis, row.names = FALSE, #remove row names
          file=paste(c(Out_path, "GabRat_Res_analysis.csv"), collapse = "/"))# save to the folder Output in Test Data

# For multiple files
savefile_list<-ls(pattern = "analysis")
savefile_list
for (i in savefile_list){
  # create file name and add the .csv extension
  # add date of creation using format(Sys.time(), "_%d_%b_%Y"), formatted to day_month_year
  fname=paste0(gsub("_analysis", "", i), "_data",format(Sys.time(), "_%d_%b_%Y"),".csv")# remove "_analysis" and add "_data" and date created
  write.csv(GabRat_Res_analysis, row.names = FALSE, #remove row names
          file=paste(c(Out_path, fname), collapse = "/"))# save to the folder Output in Test Data
}

# check that your files have saved
list.files(Out_path)

