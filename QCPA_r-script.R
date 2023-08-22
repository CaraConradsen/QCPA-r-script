## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----Animal IDs-----------------------------------------------------------------------------------------------------------------------------
pth<-getwd() #get directory; assumes the R script is saved in the parent folder of Worked example data
 
Extrct_Fold="Worked example data/Example1_tri-chromat_no_UV/Test Data" #set sub folder name containing your data

loc<-paste(pth,Extrct_Fold, sep="/")#Paste the two locations together to specify your global data location

Species=list.files(path=loc) #detects what species are in the data set. These could also be locations depending on your data set.

Species# should contain species folders and a log file

Species = Species[-length(Species)] #removes the log file

Species


## ----Animal Loop----------------------------------------------------------------------------------------------------------------------------
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


## ----Check data-----------------------------------------------------------------------------------------------------------------------------
#check data
head(Animal_Info)
Animal_Info[,colnames(Animal_Info)] <-lapply(Animal_Info[,colnames(Animal_Info)],factor) # Convert character data into factors
str(Animal_Info)# obeserve data
lapply(Animal_Info, nlevels)# Check the number of unique species, individuals and viewing distances


## ----Check ROIs-----------------------------------------------------------------------------------------------------------------------------
tmpAnidir<-apply(Animal_Info,1, function (x) paste(unlist(x), collapse = "/"))# create vector containing all the individual animal sub directories 
tmpAnidir <- paste(loc, tmpAnidir, sep="/") # paste the global file directories to the front of the sub directory paths
unique(unlist(strsplit(list.files(tmpAnidir, pattern=".tif"), "_"))[ c(TRUE,FALSE)])# This should highlight the unique regions of interest, if there are more than expected - look for naming inconsistencies


## ----ROI------------------------------------------------------------------------------------------------------------------------------------
# Here we create a character vector determining whether animal, background and/or animal+background options were used
# Using the first row of Animal_Info to specify the location to examine the number and types of _Summary Results.csv files 
ROI<- gsub("_Summary Results.csv", "",
           list.files(paste(loc,paste(unlist(Animal_Info[1,]), collapse = "/"),
                            sep="/"), pattern = "_Summary Results.csv"))
Animal_Info_ROI<-merge(Animal_Info, ROI)# Expand each unique row of the Animal_Info data frame to included a unique ROI value
colnames(Animal_Info_ROI)[4]<-"ROI" #Name column 4 to ROI
Animal_Info_ROI$ROI<-factor(Animal_Info_ROI$ROI)
head(Animal_Info_ROI)# check data
str(Animal_Info_ROI)# check that ROI is a factor


## ----kable table exn out--------------------------------------------------------------------------------------------------------------------
library("kableExtra")
exns<-sort(unique(gsub("^.*?_", "", list.files(
  paste0(loc,"/Aphelodoris varia/3_AphelodorisVaria_10_NF_D_RAW"),
  pattern=".csv", recursive = TRUE))))

exns<-as.data.frame(cbind(c(rep("Particle analysis",2), "GabRat","Particle analysis","LEIA","Particle analysis","CAA, VCA, BSA"), exns))
colnames(exns)<-c("Image Analysis", "File Name")
knitr::kable(exns, align="l",
             caption = "A tale of two tables",
             format = "pipe", padding = 2)


## ----Import Data----------------------------------------------------------------------------------------------------------------------------
dim(Animal_Info_ROI)# 18 rows, 4 columns
# Explain mechanics of the function below
nfile="_Summary Results.csv"
Ani_Data_list<-lapply(1:nrow(Animal_Info_ROI), function (x){
    tmpLoc<- paste(unlist(Animal_Info_ROI[x,]), collapse = "/")
    tmpLoc<-paste0(paste(loc,tmpLoc, sep="/"), nfile)
    data <- read.csv(tmpLoc)
    data <- merge(Animal_Info_ROI[x,], data)
    data
  })
# the above code produces a list, but we need to convert the data into a data frame 
class(Ani_Data_list)
Ani_Data_list[1:2]# Look at the structure for the first two elements in Ani_Data_list, note the [[]] notation

#Convert data from list into dataframe by binding the rows together
# using do.call allows us to pass the rbind function across each list element
Ani_Data_list<-do.call(rbind, Ani_Data_list)
class(Ani_Data_list)
head(Ani_Data_list)
dim(Ani_Data_list)
str(Ani_Data_list)

# Remove unnecessary columns, e.g., excel row numbers under column name 'X' and/or Image name
colnames(Ani_Data_list)
Ani_Data_list = Ani_Data_list[,!(names(Ani_Data_list) %in% c("X", "Image"))]# subset all names that aren't present in the vector - "X" or "Image"
colnames(Ani_Data_list)# "X" and "Image" have been removed


## ----readDat function-----------------------------------------------------------------------------------------------------------------------
readDatFile<- function(Ani_ID_dat,filetype="NA",path=loc){
  #loc is assigned by default, but can be changed
  if(filetype=="VCA"){
    exnfile="_Summary Results.csv"
  } else if (filetype=="PartAn"){
    exnfile="_Cluster Particle Analysis Summary Results.csv"
  } else if (filetype=="Clust"){
    exnfile="_Cluster Results.csv"
  } else if (filetype=="IndParticle"){
    exnfile="_Individual Particle Results.csv"
  } else {
    stop("Please specify analysis output type: e.g.,\nVCA\nPartA\nClust\nIndParticle")
  }
  data_list<-lapply(1:nrow(Ani_ID_dat), function (x){
    tmpLoc<- paste(unlist(Ani_ID_dat[x,]), collapse = "/")
    tmpLoc<-paste0(paste(path,tmpLoc, sep="/"), exnfile)
    data <- read.csv(tmpLoc)
    data <- merge(Ani_ID_dat[x,], data)
    data
  })
  data_list<-do.call(rbind, data_list)
  return(data_list)
}

# readDatFile(Animal_Info_ROI)#VSA data
# readDatFile(Animal_Info_ROI, filetype = "P")#VSA data


## -------------------------------------------------------------------------------------------------------------------------------------------
#knitr::purl("QCPA_r-script.Rmd")
print("hello")

