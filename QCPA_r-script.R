## ----animal_ids------------------------------------------------------------------------------------------------------------------------
# Construct the global data location path. This assumes the R script is in the
# project root
data_location <- file.path(getwd(), "example_data/eg1_trichromat_nouv/test_data")

# List the species folders in the data location
# These could also be locations depending on your data set
species_folders <- list.files(path = data_location)

# Print the list of species folders and log file with QCPA processing details
print(species_folders)

# Filter species folders based on the "Ap" pattern in their names
# This removes the log file from the vector
species <- grep("ap", species_folders, value = TRUE)

# Note: For multiple different species patterns, can search using the or 
# condition '|'. e.g., species <- species[grepl("A|B", species)], 
# returns species with pattern 'A' or 'B'

# Print the filtered list of species folders
print(species)


## ----Animal Loop-----------------------------------------------------------------------------------------------------------------------
Animal_Info <- data.frame(NULL)  # First create an empty data frame

# Then create a loop to iterate over the species folders,
# extracting viewing distances and individual animal IDs
for (i in 1:length(species)) { # Here i will be either 1 or 2
  Ind_pth <- paste0(data_location, "/", species[i]) # Pastes the ith species from the species object
  Indviduals_list <- list.files(path = Ind_pth) # Extracts all individual folder names

  # Iterate over the number of individuals in each species folder
  for (ind in 1:length(Indviduals_list)) {
    # The variable dist lists all the unique viewing distance folder names
    # identified by "cm"
    Dists <- list.files(paste0(Ind_pth, "/", Indviduals_list[ind]), pattern = "cm")

    # Add to Animal_Info a new row with species name, individual and viewing distance
    rbind(Animal_Info, cbind(species[i], Indviduals_list[ind], c(Dists))) -> Animal_Info
  }
}
colnames(Animal_Info) <- c("Species", "Ind", "Dist") # Rename the column names in Animal_Info


## ----Check data, fig.align='center', fig.cap='Figure 1. Counts of individuals within species for each viewing distance.'---------------
# Check data
Animal_Info

# Convert character data into factors
Animal_Info[, colnames(Animal_Info)] <- lapply(Animal_Info[, colnames(Animal_Info)], factor)

str(Animal_Info) # Observe structure of the data

# Check the number of unique species, individuals and viewing distances
lapply(Animal_Info, nlevels)

# To visualise the experimental design, we first aggregate the data to determine the
# number (length) of each individual by species and distance
Ani_info_tab <- aggregate(Ind ~ Species + Dist, data = Animal_Info, length)
barplot(Ind ~ Dist + Species,
  beside = TRUE, ylab = "Count",
  legend.text = TRUE, data = Ani_info_tab
)


## ----Check ROIs------------------------------------------------------------------------------------------------------------------------
# Create vector containing all the individual animal sub directories 
tmpAnidir <- apply(Animal_Info, 1, function(x) paste(unlist(x), collapse = "/"))

# Paste the global file directories to the front of the sub directory paths
tmpAnidir <- paste(data_location, tmpAnidir, sep = "/")

# This should highlight the unique regions of interest, if there are
# more than expected - look for naming inconsistencies
unique(unlist(strsplit(list.files(tmpAnidir, pattern = ".tif"), "_"))[c(TRUE, FALSE)])


## ----simple ROIs-----------------------------------------------------------------------------------------------------------------------
ROI <- c("animal", "animal+background", "background") # Add the names of ROIs




## ----ROI-------------------------------------------------------------------------------------------------------------------------------
# Here, we create a character vector determining whether animal,
# background and/or animal+background options were used
# Using the first row of Animal_Info to specify the location to examine
# the number and types of _Summary Results.csv files

# Find all the ROI output files for '_Summary Results.csv'
ROI <- list.files(paste(data_location, paste(unlist(Animal_Info[1, ]), collapse = "/"),
  sep = "/"
), pattern = "_Summary Results.csv")

# Here we remove the "_Summary Results.csv" information
ROI <- gsub("_Summary Results.csv", "", ROI)
ROI # In the QCPA batch script, we have investigated three ROIs


## ----merge ROI-------------------------------------------------------------------------------------------------------------------------
# Expand each unique row of the Animal_Info data frame to included a unique ROI value
Animal_Info_ROI <- merge(Animal_Info, ROI)

colnames(Animal_Info_ROI)[4] <- "ROI"  # Name column 4 to ROI

Animal_Info_ROI$ROI <- factor(Animal_Info_ROI$ROI)  # Convert to factor

head(Animal_Info_ROI)  # Check data

str(Animal_Info_ROI)  # Check that ROI is a factor


## ----reduced function------------------------------------------------------------------------------------------------------------------
# Part A:
# Here, we specify what image analysis so that we can use the correct file extension
exnfile <- "_Summary Results.csv"

Ani_ID_dat <- Animal_Info_ROI # Assign our animal ID data frame

path <- data_location # This is the global location set in section 2.1

# Part B:
# Once the file extension condition is met, we use lapply to extract the data,
# using a nested function within lapply
Sum_Res_list <- lapply(
  1:nrow(Ani_ID_dat), # for each row in Ani_ID_dat...
  function(x) { # start of our nested function

    # Extract the individual row information
    tmpLoc <- paste(unlist(Ani_ID_dat[x, ]), collapse = "/")

    # Create a directory string
    tmpLoc <- paste0(paste(path, tmpLoc, sep = "/"), exnfile)

    # Read in the data using the newly specified location
    data <- read.csv(tmpLoc)

    # Add the specific animal information to the data
    data <- merge(Ani_ID_dat[x, ], data)

    data # Returns data
  }
)

# Part C:
# We then collapse the Sum_Res_list list into a data frame by
# calling rbind in the function do.call
Sum_Res_df <- do.call(rbind, Sum_Res_list)

# Check the imported data
dim(Sum_Res_df) # 18 rows by 133 columns

head(Sum_Res_df)[, 1:12] # check the first six rows and first twelve columns

# Finally, to prevent any errors we remove the objects Ani_ID_dat, exnfile, path
# from the global environment because we'll be recycling them in the second function
rm(exnfile)
rm(Ani_ID_dat)
rm(path)




## ----read_qcpa function----------------------------------------------------------------------------------------------------------------
read_qcpa <- function(Ani_ID_dat, filetype = "NA", path = data_location) {
  # Default location is assigned, but can be changed

  # Part A: Specify arguments for different image analyses
  file_extensions <- list(
    VCA = "_Summary Results.csv",
    PartAn = "_Cluster Particle Analysis Summary Results.csv",
    Clust = "_Cluster Results.csv",
    IndParticle = "_Individual Particle Results.csv"
  )

  if (!filetype %in% names(file_extensions)) {
    stop('Specify analysis output type: filetype = "VCA", "PartAn", "Clust", or "IndParticle"')
  }

  exnfile <- file_extensions[[filetype]]

  # Part B: Extract data using lapply
  data_list <- lapply(seq_len(nrow(Ani_ID_dat)), function(x) {
    # Extract row information and create directory string
    dir_components <- unlist(Ani_ID_dat[x, ])
    tmpLoc <- file.path(path, paste(dir_components[1:3], collapse = "/"), 
                        paste(dir_components[4], exnfile, sep = ""))

    # Check if file exists before trying to read it
    if (!file.exists(tmpLoc)) {
      stop("File does not exist: ", tmpLoc)
    }

    # Read data and add animal information
    data <- read.csv(tmpLoc)
    data <- merge(Ani_ID_dat[x, ], data)

    return(data)
  })

  # Part C: Combine list into a data frame
  data_df <- do.call(rbind, data_list)

  return(data_df)
}



## ----empty read_qcpa, error=TRUE-------------------------------------------------------------------------------------------------------
read_qcpa(Animal_Info_ROI) # We include only the animal id data set


## ----implementing read_qcpa------------------------------------------------------------------------------------------------------------
# VCA,BSA,CAA analysis:
# Read in and assign analysis
VCA_analysis <- read_qcpa(Animal_Info_ROI, filetype = "VCA")

# head(VCA_analysis) # Inspect data

# Subset data excluding the column Image and X
VCA_analysis <- VCA_analysis[, !colnames(VCA_analysis) %in% c("X", "Image")]

dim(VCA_analysis) # 18 rows, by 131 columns

VCA_analysis[1:2, ] # Observe the first two rows of data

# Cluster Particle Analysis:
# Read in and assign analysis
Particle_analysis <- read_qcpa(Animal_Info_ROI, filetype = "PartAn")

# head(Particle_analysis) # Inspect data

dim(Particle_analysis) # 52 rows, by 20 columns

colnames(Particle_analysis)[5]

# Change column name to be consistent with Cluster analysis
colnames(Particle_analysis)[5] <- "ClusterID"

Particle_analysis[1:2, ] # Observe the first two rows of data

# Cluster Results:
# Read in and assign analysis
Cluster_analysis <- read_qcpa(Animal_Info_ROI, filetype = "Clust")

# head(Cluster_Analysis) # Inspect data

# Subset data excluding the column Image and X
Cluster_analysis <- Cluster_analysis[, !colnames(Cluster_analysis) %in% c("X", "Image")]

dim(Cluster_analysis) # 52 rows, by 21 columns

Cluster_analysis[1:2, ] # Observe the first two rows of data

# Individual Particle Analysis:
# Read in and assign analysis
IndParticle_analysis <- read_qcpa(Animal_Info_ROI, filetype = "IndParticle")

# head(Cluster_Analysis) # Inspect data

# Subset data excluding the column X.1
IndParticle_analysis <- IndParticle_analysis[, colnames(IndParticle_analysis) != "X.1"]

dim(IndParticle_analysis) # 143 rows, by 23 columns

IndParticle_analysis[1:2, ] # Observe the first two rows of data


## ----LEIA CSV Data---------------------------------------------------------------------------------------------------------------------
# Define a function to generate file paths, read data, and merge animal information
read_leia <- function(index, animal_info, base_location) {
  # Extract row as a character vector
  row_values <- as.character(unlist(animal_info[index, ]))
  
  # Construct the file path
  file_path <- file.path(
    base_location, 
    row_values[1], 
    row_values[2], 
    row_values[3], 
    "LEIA", 
    row_values[4],
    "_Local Edge Intensity Analysis.csv"
  )
  
  # Read data
  data <- read.csv(file_path)
  
  # Add animal information to the data
  data <- merge(animal_info[index, ], data)
  
  return(data)
}

# Read LEIA data using lapply
LEIA_Res_list <- lapply(
  seq_len(nrow(Animal_Info_ROI)),
  function(x) read_leia(x, Animal_Info_ROI, data_location)
)

# Combine list into a data frame
LEIA_Res_analysis <- do.call(rbind, LEIA_Res_list)

# Subset data to exclude specific columns
LEIA_Res_analysis <- LEIA_Res_analysis[, !(colnames(LEIA_Res_analysis) 
                                           %in% c("X", "Image"))]

# Check the resulting data
dim(LEIA_Res_analysis) # Print dimensions of the data
head(LEIA_Res_analysis, 2) # Print the first two rows of data



## ----GabRat CSV Data-------------------------------------------------------------------------------------------------------------------
# Because we are not using ROI which adds three levels (rows),
# we trim data frame using unique()
# For each the 6 rows in Animal_Info_ROI
GabRat_Res_list <- lapply(
  1:nrow(unique(Animal_Info_ROI[, 1:3])),
  function(x) { # Start of our nested function

    # Using the first three columns, convert row into a character vector
    tmpLoc <- as.character(unlist(Animal_Info_ROI[x, 1:3]))

    tmpLoc <- c(tmpLoc, "GabRat") # Add the new sub folder GabRat

    # Paste tmploc into a directory character
    tmpLoc <- paste(tmpLoc, collapse = "/")

    # Create a directory string
    tmpLoc <- paste0(paste(data_location, tmpLoc, sep = "/"), "/_GabRat_Results.csv")

    # Read in the data using the newly specified location
    data <- read.csv(tmpLoc)

    # Add the specific animal information to the data,
    # using only the first three columns
    data <- merge(Animal_Info_ROI[x, 1:3], data)

    data # Returns data
  }
)

# We then collapse the LEIA_Res_list list into a data frame by calling
# rbind in the function do.call
GabRat_Res_analysis <- do.call(rbind, GabRat_Res_list)

# Check the imported data
dim(GabRat_Res_analysis) # 72 rows by 7 columns
head(GabRat_Res_analysis) # We only want every ~ fourth row for the dbl values

# Set up a true/false condition on whether row has the pattern "_dbl",
# then subset data on this condition
GabRat_Res_analysis <- GabRat_Res_analysis[endsWith(GabRat_Res_analysis$ID, "_dbl"), ]
rownames(GabRat_Res_analysis) <- NULL # Reset row numbers

# Then we want to determine ROI where we split the ID column
# data on the shared pattern "WholeImage_"
# We then only take the second elemnt, using a FALSE,TRUE index
GabRat_Res_analysis$ROI <- unlist(
  strsplit(GabRat_Res_analysis$ID, "WholeImage_")
)[c(FALSE, TRUE)]

# Then remove the '_dbl'
GabRat_Res_analysis$ROI <- gsub("_dbl", "", GabRat_Res_analysis$ROI)

# Remove unnecessary columns, subset data excluding the column X
GabRat_Res_analysis <- GabRat_Res_analysis[, !colnames(GabRat_Res_analysis)
%in% c("X", "ID", "Sigma")]

# Check the imported data
dim(GabRat_Res_analysis) # 18 rows by 5 columns

GabRat_Res_analysis[1:2, ] # Observe the first two rows of data


## ----merge two particle analyses files-------------------------------------------------------------------------------------------------
# Merging two files together
Cluster_Particle_analysis <- merge(Cluster_analysis, Particle_analysis,
  by = c("Species", "Ind", "Dist", "ROI", "ClusterID")
)

dim(Cluster_Particle_analysis) # 52 rows and 36 columns

Cluster_Particle_analysis[1:2, ] # Inspect data


## ----combine VCA LEIA and GabRat-------------------------------------------------------------------------------------------------------
# Merge the first two data frames (VCA_analysis and LEIA_Res_analysis) together
VCA_LEIA_GabRat_analysis <- merge(VCA_analysis, LEIA_Res_analysis,
  by = c("Species", "Ind", "Dist", "ROI")
)

VCA_LEIA_GabRat_analysis[1:2, ] # Inspect data
dim(VCA_LEIA_GabRat_analysis) # 18 rows and 164 columns

# Merge the new data frame (VCA_LEIA_GabRat_analysis) with GabRat_Res_analysis
VCA_LEIA_GabRat_analysis <- merge(VCA_LEIA_GabRat_analysis, GabRat_Res_analysis,
  by = c("Species", "Ind", "Dist", "ROI")
)

VCA_LEIA_GabRat_analysis[1:2, ] # Inspect data again

dim(VCA_LEIA_GabRat_analysis) # 18 rows and 165 columns


## ----Removing NAs----------------------------------------------------------------------------------------------------------------------
# Specify column names
ColumnNames <- c(
  "Species", "Ind", "Dist", "ROI", "BSA.BsL.Hrz", "Lum.mean",
  "CAA.Scpl", "Lum.CoV", "BSA.BML", "Col.mean", "GabRat"
)

# Subset data by column names
V.L.GbRt_sub_analysis <- VCA_LEIA_GabRat_analysis[, colnames(VCA_LEIA_GabRat_analysis)
%in% ColumnNames]

dim(V.L.GbRt_sub_analysis) # 18 rows by 11 columns
summary(V.L.GbRt_sub_analysis) # Initial summary of the data

# View the data frame
View(V.L.GbRt_sub_analysis)

# Replace infinite values with NA using sapply
V.L.GbRt_sub_analysis[sapply(V.L.GbRt_sub_analysis, is.infinite)] <- NA

View(V.L.GbRt_sub_analysis) # Observe change the data frame

# Remove NA/NaNs
V.L.GbRt_sub_analysis <- na.omit(V.L.GbRt_sub_analysis)

View(V.L.GbRt_sub_analysis) # Observe change the data frame

dim(V.L.GbRt_sub_analysis) # 11 rows by 11 columns

summary(V.L.GbRt_sub_analysis) # Inspect column distributions

# Inspect the variance for each colour variable
apply(V.L.GbRt_sub_analysis[, 5:11], 2, var)


## ----Normalising data, fig.align='center', fig.height=7, fig.width=8, fig.cap='Figure 2. Histograms of seven colour variables.'--------
# Part A:
# Test for normality Shapiro-Wilk test
lapply(V.L.GbRt_sub_analysis[, 5:11], shapiro.test)

# Part B:
# Visualise distribution of the data using histograms
Colnames_subset <- colnames(V.L.GbRt_sub_analysis[, 5:11]) # Select only the colour variables

# Set the plot dimensions to display plots as 3 rows by 3 columns
par(mfrow = c(3, 3))

# Use a for loop to iterate over the column names
for (i in Colnames_subset) {
  # Plot the histogram, using 10 bins, and the colour variable as the main title
  hist(V.L.GbRt_sub_analysis[, i],
    breaks = 10,
    main = i, xlab = ""
  )
}


## ----Pairs and correlation plot, warning=FALSE, fig.align='center', fig.height=7, fig.width=9, fig.cap="Figure 3. Paired scatter plots of seven colour variables. Lower off-diagonal are the paired scatterplots where coloured points represent regions of interest (animal+background is yellow, bacground is red and animal is blue). Upper off-diagonal represents Pearson's correlation between paired colur varaibles."----
# Correlation panel
panel.cor <- function(x, y) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))

  # Calculate Pearson's correlation
  r <- round(cor(x, y, method = "pearson"), digits = 2)

  # Save as text
  txt <- paste0("R = ", r)

  # Plot the text
  text(0.5, 0.5, txt, cex = 1)
}

my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")

# Scatter plot panel
panel.scat <- function(x, y) {
  # Specify scatter plot points to colour by ROI
  points(x, y,
    col = my_cols[V.L.GbRt_sub_analysis$ROI],
    cex = 1.1, pch = 19
  )
}

pairs(V.L.GbRt_sub_analysis[, 5:11],
  # Set title
  main = "Seven colour variables for two viewing distances",

  # Parse in the new panels
  upper.panel = panel.cor,
  lower.panel = panel.scat,

  # Add margin space and reduce gaps between plots
  oma = c(5, 5, 6, 18), gap = 0
)

# Use the data to create a figure legend
Colour_ID <- unique(data.frame(
  as.character(V.L.GbRt_sub_analysis$ROI),
  my_cols[V.L.GbRt_sub_analysis$ROI]
))

# Add figure legend to plot
legend("right", Colour_ID[, 1],
  xpd = TRUE,
  pch = 19, cex = 1.1, bty = "n", col = Colour_ID[, 2]
)



## ----Boxplots, fig.align='center', fig.height=6.5, fig.width=9, fig.cap="Figure 4. Boxplots of the seven colour variables."------------
# Part A:
# For the boxplot function, the data has to be in long format
# We need the reshape package for this
library(reshape2)

# Here we 'melt' the data to long format
V.L.GbRt_sub_analysis_lng <- melt(V.L.GbRt_sub_analysis,
  id.vars = c("Species", "Ind", "Dist", "ROI")
)

# Set the plot dimensions to display a single plot, correcting outer margins
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# Inspect boxplots for outliers
boxplot(value ~ variable,
  pch = 16, boxwex = 0.5, xlab = "Colour variable",
  data = V.L.GbRt_sub_analysis_lng
)


## ----outliers--------------------------------------------------------------------------------------------------------------------------
# Calculate z-scores and look for outliers
apply(V.L.GbRt_sub_analysis[, 5:11], 2, function(x) (x - mean(x)) / sd(x))


## ----Standardising data----------------------------------------------------------------------------------------------------------------
# Inspect scaled values and column means
scale(V.L.GbRt_sub_analysis[, 5:11], center = TRUE, scale = TRUE)

# Update the V.L.GbRt_sub_analysis data frame with the standardised values
V.L.GbRt_sub_analysis[, 5:11] <- scale(V.L.GbRt_sub_analysis[, 5:11],
  center = TRUE, scale = TRUE
)

# Check standardisation
summary(V.L.GbRt_sub_analysis) # Mean values are zero
apply(V.L.GbRt_sub_analysis[, 5:11], 2, var) # Confirm variances are 1


## ----Mahalanobis-----------------------------------------------------------------------------------------------------------------------
# Using R's Mahalanobis function, add a new column to your data
V.L.GbRt_sub_analysis$mahalnobis <- mahalanobis(
  V.L.GbRt_sub_analysis[, 5:11],

  # Get a vector of column means
  colMeans(V.L.GbRt_sub_analysis[, 5:11]),

  # Get a covaraince matrix
  cov(V.L.GbRt_sub_analysis[, 5:11])
)

# Calcualte p-values using k-1 df = 6
V.L.GbRt_sub_analysis$pvalue <- pchisq(V.L.GbRt_sub_analysis$mahalnobis,
  df = 6, lower.tail = FALSE
)

# Inspect Mahalnobis distances and p-values
V.L.GbRt_sub_analysis[, colnames(V.L.GbRt_sub_analysis) %in% c("mahalnobis", "pvalue")]


## ----Output files----------------------------------------------------------------------------------------------------------------------
dir.create(paste(c(data_location, "Output"), collapse = "/")) # Create the Output directory

Out_path <- paste(c(data_location, "Output"), collapse = "/") # Save Output directory path

# For a single file:
write.csv(GabRat_Res_analysis,
  row.names = FALSE, # Remove row names
  file = paste(c(Out_path, "GabRat_Res_analysis.csv"),
    collapse = "/"
  )
) # Save to the folder Output in test_data

# For multiple files:
savefile_list <- ls(pattern = "analysis") # Save all objects with "analysis" in the name

savefile_list # Inspect object names

# Iterate over each data frame in savefile_list
for (i in savefile_list) {
  # Create file name and add the .csv extension
  # Add date of creation using format(Sys.time(), "_%d_%b_%Y")
  # Remove "_analysis" using gsub and add "_data" and date created
  fname <- paste0(gsub("_analysis", "", i), "_data", format(Sys.time(), "_%d_%b_%Y"), ".csv")

  # Save to the folder Output in test_data
  write.csv(GabRat_Res_analysis,
    row.names = FALSE, # Remove row names
    file = paste(c(Out_path, fname), collapse = "/")
  )
}

# Check that your files have saved
list.files(Out_path)


## ----eval=FALSE------------------------------------------------------------------------------------------------------------------------
## FROM[WHERE, SELECT, GROUP BY]
## DT  [i,     j,      by]


## ----data.table, warning=FALSE---------------------------------------------------------------------------------------------------------
library(data.table)

# Convert Animal_Info_ROI into data.table format
Animal_Info_ROI_DT <- setDT(Animal_Info_ROI)

# From Animal_Info_ROI we can paste the column information together
# and then read in data row by row, specifying 'by=1:NROW(Animal_Info_ROI)'
LEIA_analysis_DT <- Animal_Info_ROI[, fread(paste(data_location, Species, Ind, Dist, "LEIA", ROI,
  "_Local Edge Intensity Analysis.csv",
  sep = "/"
)),
by = 1:NROW(Animal_Info_ROI)
]

# Remove unwanted columns, and automatically assign using ':='
LEIA_analysis_DT[, c("NROW", "V1", "Image", "Transform") := NULL]

# Add the Animal_Info_ROI information
LEIA_analysis_DT <- cbind(Animal_Info_ROI, LEIA_analysis_DT)

# Inspect the data.table
head(LEIA_analysis_DT)

# Create save filename
fname <- paste0(
  Out_path, "/", gsub("_analysis", "", "LEIA_analysis_DT"),
  "_data", format(Sys.time(), "_%d_%b_%Y"), ".csv"
)
# Save output
fwrite(LEIA_analysis_DT, file = fname, row.names = FALSE)

