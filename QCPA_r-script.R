## ----Rename files and folders------------------------------------------------------------------------------------------------------------
# Level 1: 
# We first correct the file name of the parent directory 'Worked_example_data'
# Specify the old file name and the new file name
old_name <- "Worked_example_data"
new_name <- "example_data"
# Then use base function file.name to correct the file name to snake case
file.rename(old_name,new_name)

# Level 2: 
# We then change the UV and no UV example folder names within "example_data"
# Examine the files in example_data using list.files
list.files("example_data")

# Save the list of names to old_eg_names 
old_eg_names <- list.files("example_data")

# Modify the names in old_eg_names to snake case and save to new_names
# Using a nested gsub we can replace the space, dash and 'Example'
new_eg_names <- gsub("-", "",# Remove the dashes
                 gsub(" ", "_",# Replace all spaces with underscores
                      gsub("Example", "eg", old_eg_names)))# Abbreviate Example to 'eg'
     
new_eg_names <- tolower(new_eg_names) # Convert to lowercase  

# Add the directory level to the parent folder example_data
old_eg_names <- paste0("example_data/", old_eg_names)
new_eg_names <- paste0("example_data/", new_eg_names)

# Then replace the folder names
file.rename(old_eg_names,new_eg_names)

# Level 3:
# Here we correct the cone mapping and the test folder names
# using full.names = TRUE to save specifying directory pathways
old_test_names <- list.files(new_eg_names, full.names = TRUE)

new_test_names <- gsub(" ", "_", old_test_names)# Replace all spaces with underscores

new_test_names <- tolower(new_test_names) # Convert to lowercase

# And again replace the folder names
file.rename(old_test_names, new_test_names)

# Level 4:
# We finally fix the species names for eg1_trichromat_no_uv
list.files("example_data/eg1_trichromat_no_uv/test_data")# Examine folders

# To exclude the text file, filter species folders based on the "Ap" pattern
# in their names
old_species_names<- list.files("example_data/eg1_trichromat_no_uv/test_data", 
                               pattern = "Ap", full.names = TRUE)

new_species_names <- gsub(" ", "_", old_species_names)# Replace spaces with underscores

new_species_names <- tolower(new_species_names) # Convert to lowercase

# And again replace the folder names
file.rename(old_species_names, new_species_names)


## ----animal_ids--------------------------------------------------------------------------------------------------------------------------
# Construct the global data location path. This assumes the R script is in the
# project root
data_location <- file.path(getwd(), "example_data/eg1_trichromat_no_uv/test_data")

# List the species folders in the data location
# These could also be locations depending on your data set
species_folders <- list.files(path = data_location)

# Print the list of species folders and log file with QCPA processing details
print(species_folders)

# To remove the "Batch_QCPA_Log.txt" from our list of folder names,
# we subset the list on the condition that file names do not end in 
# "Log.txt" using the endsWith function 

species <- species_folders[!endsWith(species_folders, "Log.txt")] 

# Print the filtered list of species folders
print(species)


## ----animal_loop-------------------------------------------------------------------------------------------------------------------------
# Initialize an empty data frame
animal_info <- data.frame()

# Iterate over the species folders
for (species_name in species) {
  
  species_path <- file.path(data_location, species_name)
  individual_list <- list.files(path = species_path)
  
  # Iterate over the individuals in each species folder
  for (individual_name in individual_list) {
    
    individual_path <- file.path(species_path, individual_name)
    distances <- list.files(path = individual_path, pattern = "cm")
    
    # Add new rows to the animal_info data frame
    new_rows <- expand.grid(
      Species = species_name,
      Ind = individual_name,
      Dist = distances,
      stringsAsFactors = FALSE
    )
    animal_info <- rbind(animal_info, new_rows)
  }
}

# Rename the column names in animal_info
colnames(animal_info) <- c("Species", "Ind", "Dist")



## ----Check data--------------------------------------------------------------------------------------------------------------------------
# Check data
head(animal_info)

# Convert character columns to factors
animal_info[] <- lapply(animal_info, as.factor)

# Display structure of the data
str(animal_info)

# Check the number of unique species, individuals and viewing distances
sapply(animal_info, nlevels)

# Aggregate data to count individuals by species and distance
aggregate_counts <- aggregate(Ind ~ Species + Dist, data = animal_info, FUN = length)

# Plot bar chart of individual counts by species and distance
barplot(Ind ~ Dist + Species,
  beside = TRUE, 
  ylab = "Count",
  legend.text = TRUE, 
  data = aggregate_counts
)


## ----Check ROIs--------------------------------------------------------------------------------------------------------------------------
# Generate individual animal sub-directories paths
animal_sub_dirs <- apply(animal_info, 1, function(x) paste(x, collapse = "/"))

# Prepend the global file directories to the sub-directory paths
full_animal_dirs <- file.path(data_location, animal_sub_dirs)

# Identify the unique regions of interest. If there are
# more than expected - look for naming inconsistencies.
tif_files <- list.files(full_animal_dirs, pattern = ".tif")
roi_names <- unique(unlist(strsplit(tif_files, "_"))[c(TRUE, FALSE)])
roi_names


## ----simple ROIs-------------------------------------------------------------------------------------------------------------------------
ROI <- c("animal", "animal+background", "background") # Add the names of ROIs




## ----ROI---------------------------------------------------------------------------------------------------------------------------------
# Here, we create a character vector determining whether animal,
# background and/or animal+background options were used
# Using the first row of animal_info to specify the location to examine
# the number and types of _Summary Results.csv files

# Use the first row of animal_info to specify the location to examine
first_animal_path <- file.path(data_location, 
                               paste(unlist(animal_info[1, ]), collapse = "/"))

# Find all the ROI output files for '_Summary Results.csv'
roi_files <- list.files(first_animal_path, pattern = "_Summary Results.csv")

# Extract ROI names by removing the "_Summary Results.csv" suffix
rois <- gsub("_Summary Results.csv", "", roi_files)
rois

# Note: In the QCPA batch script, three ROIs were investigated.


## ----merge ROI---------------------------------------------------------------------------------------------------------------------------
# Expand each unique row of the animal_info data frame to included a unique ROI value
animal_info_roi <- merge(animal_info, rois)

colnames(animal_info_roi)[4] <- "ROI"  # Name column 4 to ROI

animal_info_roi$ROI <- factor(animal_info_roi$ROI)  # Convert to factor

head(animal_info_roi)  # Check data

str(animal_info_roi)  # Check that ROI is a factor


## ----reduced function--------------------------------------------------------------------------------------------------------------------
# Part A:
# Here, we specify what image analysis so that we can use the correct file extension
exnfile <- "_Summary Results.csv"

ani_id_dat <- animal_info_roi # Assign our animal ID data frame

path <- data_location # This is the global location set in section 2.1

# Part B:
# Once the file extension condition is met, we use lapply to extract the data,
# using a nested function within lapply
Sum_Res_list <- lapply(
  1:nrow(ani_id_dat), # for each row in ani_id_dat...
  function(x) { # start of our nested function

    # Extract the individual row information
    tmpLoc <- paste(unlist(ani_id_dat[x, ]), collapse = "/")

    # Create a directory string
    tmpLoc <- paste0(paste(path, tmpLoc, sep = "/"), exnfile)

    # Read in the data using the newly specified location
    data <- read.csv(tmpLoc)

    # Add the specific animal information to the data
    data <- merge(ani_id_dat[x, ], data)

    data # Returns data
  }
)

# Part C:
# We then collapse the Sum_Res_list list into a data frame by
# calling rbind in the function do.call
Sum_Res_df <- do.call(rbind, Sum_Res_list)

# Check the imported data
dim(Sum_Res_df) # 18 rows by 133 columns

# Finally, to prevent any errors we remove the objects ani_id_dat, exnfile, path
# from the global environment because we'll be recycling them in the second function
rm(exnfile)
rm(ani_id_dat)
rm(path)




## ----read_qcpa function------------------------------------------------------------------------------------------------------------------
read_qcpa <- function(ani_id_dat, filetype = "NA", path = data_location) {
  # Default location is assigned, but can be changed

  # Part A: Specify arguments for different image analyses
  file_extensions <- list(
    VCA = "_Summary Results.csv",
    PartAn = "_Cluster Particle Analysis Summary Results.csv",
    Clust = "_Cluster Results.csv",
    IndParticle = "_Individual Particle Results.csv"
  )

  if (!filetype %in% names(file_extensions)) {
    stop('Specify analysis output type: 
         filetype = "VCA", "PartAn", "Clust", or "IndParticle"')
  }

  exnfile <- file_extensions[[filetype]]

  # Part B: Extract data using lapply
  data_list <- lapply(seq_len(nrow(ani_id_dat)), function(x) {
    # Extract row information and create directory string
    dir_components <- unlist(ani_id_dat[x, ])
    tmpLoc <- file.path(path, paste(dir_components[1:3], collapse = "/"), 
                        paste(dir_components[4], exnfile, sep = ""))

    # Check if file exists before trying to read it
    if (!file.exists(tmpLoc)) {
      stop("File does not exist: ", tmpLoc)
    }

    # Read data and add animal information
    data <- read.csv(tmpLoc)
    data <- merge(ani_id_dat[x, ], data)

    return(data)
  })

  # Part C: Combine list into a data frame
  data_df <- do.call(rbind, data_list)

  return(data_df)
}



## ----implementing read_qcpa--------------------------------------------------------------------------------------------------------------
# VCA,BSA,CAA analysis:
# Read in and assign analysis
VCA_analysis <- read_qcpa(animal_info_roi, filetype = "VCA")

# Subset data excluding the column Image and X
VCA_analysis <- VCA_analysis[, !colnames(VCA_analysis) %in% c("X", "Image")]

dim(VCA_analysis) # 18 rows, by 131 columns

VCA_analysis[1:2, 1:6] # Observe the first two rows and six columns of data

# Cluster Particle Analysis:
# Read in and assign analysis
Particle_analysis <- read_qcpa(animal_info_roi, filetype = "PartAn")

dim(Particle_analysis) # 62 rows, by 20 columns
colnames(Particle_analysis)[5]

# Change column name to be consistent with Cluster analysis
colnames(Particle_analysis)[5] <- "ClusterID"

# Cluster Results:
# Read in and assign analysis
Cluster_analysis <- read_qcpa(animal_info_roi, filetype = "Clust")

# Subset data excluding the column Image and X
Cluster_analysis <- Cluster_analysis[, !colnames(Cluster_analysis) %in% c("X", "Image")]

dim(Cluster_analysis) # 62 rows, by 21 columns

# Individual Particle Analysis:
# Read in and assign analysis
IndParticle_analysis <- read_qcpa(animal_info_roi, filetype = "IndParticle")

# Subset data excluding the column X.1
IndParticle_analysis <- IndParticle_analysis[, colnames(IndParticle_analysis) != "X.1"]

dim(IndParticle_analysis) # 214 rows, by 23 columns


## ----LEIA CSV Data-----------------------------------------------------------------------------------------------------------------------
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
  seq_len(nrow(animal_info_roi)),
  function(x) read_leia(x, animal_info_roi, data_location)
)

# Combine list into a data frame
LEIA_Res_analysis <- do.call(rbind, LEIA_Res_list)

# Subset data to exclude specific columns
LEIA_Res_analysis <- LEIA_Res_analysis[, !(colnames(LEIA_Res_analysis) 
                                           %in% c("X", "Image"))]

# Check the resulting data
dim(LEIA_Res_analysis) # Print dimensions of the data
names(LEIA_Res_analysis)



## ----GabRat CSV Data---------------------------------------------------------------------------------------------------------------------
# Because we are not using ROI which adds three levels (rows),
# we trim data frame using unique()
# For each the 6 rows in animal_info_roi

# Import GabRat results and process data

# Function to read and process GabRat results
read_gabrat <- function(data_location, animal_info) {
  gabrat_res_list <- lapply(
    1:nrow(unique(animal_info[, 1:3])),
    function(row_index) {
      # Convert row into a character vector
      tmp_loc <- as.character(unlist(animal_info[row_index, 1:3]))
      tmp_loc <- c(tmp_loc, "GabRat") # Add the new sub folder GabRat
      dir_path <- file.path(data_location, 
                            paste(tmp_loc, collapse = "/"), 
                            "_GabRat_Results.csv")
      
      # Read in the data using the newly specified location
      data <- read.csv(dir_path)
      
      # Add the specific animal information to the data
      data <- merge(animal_info[row_index, 1:3], data)
      data
    }
  )
  
  # Collapse the gabrat_res_list list into a data frame
  gabrat_res_analysis <- do.call(rbind, gabrat_res_list)
  
  # Filter data for rows containing "_dbl" in ID
  gabrat_res_analysis <- gabrat_res_analysis[endsWith(gabrat_res_analysis$ID, "_dbl"), ]
  rownames(gabrat_res_analysis) <- NULL
  
  # Add ROI column by extracting substring after "WholeImage_"
  gabrat_res_analysis$ROI <- sapply(
    strsplit(gabrat_res_analysis$ID, "WholeImage_"), `[`, 2
  )
  
  # Remove '_dbl' suffix from ROI column
  gabrat_res_analysis$ROI <- gsub("_dbl", "", gabrat_res_analysis$ROI)
  
  # Remove unnecessary columns
  gabrat_res_analysis <- gabrat_res_analysis[, !colnames(gabrat_res_analysis) %in% 
                                               c("X", "ID", "Sigma")]
  
  gabrat_res_analysis
}

# Call the function to read and process GabRat results
gabrat_res_analysis <- read_gabrat(data_location, animal_info_roi)

# Check the processed data
dim(gabrat_res_analysis) # Should show 18 rows and 5 columns
print(head(gabrat_res_analysis)) # Displays first six rows of data



## ----merge two particle analyses files---------------------------------------------------------------------------------------------------
# Merging two files together
Cluster_Particle_analysis <- merge(Cluster_analysis, Particle_analysis,
  by = c("Species", "Ind", "Dist", "ROI", "ClusterID")
)

dim(Cluster_Particle_analysis) # 62 rows and 36 columns
names(Cluster_Particle_analysis) # Inspect data


## ----combine VCA LEIA and GabRat---------------------------------------------------------------------------------------------------------
# Merge the first two data frames (VCA_analysis and LEIA_Res_analysis) together
VCA_LEIA_GabRat_analysis <- merge(VCA_analysis, LEIA_Res_analysis,
  by = c("Species", "Ind", "Dist", "ROI")
)

dim(VCA_LEIA_GabRat_analysis) # 18 rows and 164 columns

# Merge the new data frame (VCA_LEIA_GabRat_analysis) with gabrat_res_analysis
VCA_LEIA_GabRat_analysis <- merge(VCA_LEIA_GabRat_analysis, gabrat_res_analysis,
  by = c("Species", "Ind", "Dist", "ROI")
)

dim(VCA_LEIA_GabRat_analysis) # 18 rows and 165 columns
names(VCA_LEIA_GabRat_analysis) # Inspect data structure



## ----Removing NAs------------------------------------------------------------------------------------------------------------------------
# Column names specification
column_names <- c(
  "Species", "Ind", "Dist", "ROI", "BSA.BsL.Hrz", "Lum.mean",
  "CAA.Scpl", "Lum.CoV", "BSA.BML", "Col.mean", "GabRat"
)

# Subset the data based on the specified column names
v.l.gabrat_sub_analysis <- VCA_LEIA_GabRat_analysis[, column_names]

# Inspect the dimensions and provide an initial summary
cat("Dimensions before cleaning:", dim(v.l.gabrat_sub_analysis), "\n")

# Replace infinite values with NA
infinite_indices <- sapply(v.l.gabrat_sub_analysis, is.infinite)
v.l.gabrat_sub_analysis[infinite_indices] <- NA

# Remove NA/NaNs
v.l.gabrat_sub_analysis <- na.omit(v.l.gabrat_sub_analysis)

# Inspect the cleaned data frame
cat("Dimensions after cleaning:", dim(v.l.gabrat_sub_analysis), "\n")

# Examine variance for specified columns
variance_results <- apply(v.l.gabrat_sub_analysis[, 5:11], 2, var)
variance_results



## ----Normalising data--------------------------------------------------------------------------------------------------------------------
# Subset column names for analysis
colnames_subset <- colnames(v.l.gabrat_sub_analysis[, 5:11])

# Part A: Test for normality using the Shapiro-Wilk test
normality_results <- lapply(v.l.gabrat_sub_analysis[colnames_subset], shapiro.test)
print(normality_results)

# Part B: Visualize distribution of the data using histograms
# Set up plot layout to display histograms as 3 rows by 3 columns
par(mfrow = c(3, 3))

# Create histograms for each column in the subset
for (column in colnames_subset) {
  hist(v.l.gabrat_sub_analysis[, column], 
       breaks = 10, 
       main = column, 
       xlab = "")
}



## ----Pairs and correlation plot----------------------------------------------------------------------------------------------------------
# Define function for correlation panel
panel_correlation <- function(x, y) {
  # Set up the plotting area
  par(usr = c(0, 1, 0, 1))
  
  # Calculate and format the Pearson correlation
  correlation <- round(cor(x, y, method = "pearson"), 2)
  correlation_text <- paste0("R = ", correlation)
  
  # Display the correlation on the panel
  text(0.5, 0.5, correlation_text, cex = 1)
}

# Define function for scatter plot panel
panel_scatter <- function(x, y) {
  points(x, y, 
         col = my_cols[v.l.gabrat_sub_analysis$ROI], 
         cex = 1.1, pch = 19)
}

# Define colors
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")

# Create scatter plot matrix for the specified columns
pairs(v.l.gabrat_sub_analysis[, 5:11], 
      main = "Seven colour variables for two viewing distances", 
      upper.panel = panel_correlation, 
      lower.panel = panel_scatter, 
      oma = c(5, 5, 6, 18), gap = 0)

# Create legend data
legend_data <- unique(data.frame(
  ROI_Label = as.character(v.l.gabrat_sub_analysis$ROI), 
  Color = my_cols[v.l.gabrat_sub_analysis$ROI]
))

# Display the legend
legend("right", legend_data$ROI_Label, 
       xpd = TRUE, 
       pch = 19, cex = 1.1, bty = "n", col = legend_data$Color)



## ----Boxplots----------------------------------------------------------------------------------------------------------------------------
# Convert data to long format using base R's 'reshape'
v.l.gabrat_sub_analysis_long <- reshape(v.l.gabrat_sub_analysis, 
                            varying = list(names(v.l.gabrat_sub_analysis)[5:11]), 
                            v.names = "value", 
                            timevar = "variable", 
                            idvar = c("Species", "Ind", "Dist", "ROI"), 
                            direction = "long")

# Reset plot parameters for a single plot
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# Extract variable names for the x-axes labels
box_xlab <- names(v.l.gabrat_sub_analysis)[5:11]

# Create boxplots to inspect for outliers
boxplot(value ~ variable, 
        data = v.l.gabrat_sub_analysis_long, 
        pch = 16, boxwex = 0.5, 
        xlab = "Colour variable", 
        names = box_xlab)



## ----outliers----------------------------------------------------------------------------------------------------------------------------
# Calculate z-scores and look for outliers
apply(v.l.gabrat_sub_analysis[, 5:11], 2, function(x) (x - mean(x)) / sd(x))


## ----Standardising data------------------------------------------------------------------------------------------------------------------
# Function to standardize and inspect data
standardize_data <- function(data) {
  # Standardize data
  standardized_data <- round(scale(data, center = TRUE, scale = TRUE), 3)
  
  # Check and print mean and variance
  cat("Column Means (should be near zero):\n")
  print(colMeans(standardized_data))
  cat("\nColumn Variances (should be 1):\n")
  print(apply(standardized_data, 2, var))
  standardized_data
}

# Update the data frame with standardized values and inspect
v.l.gabrat_sub_analysis[, 5:11] <- standardize_data(v.l.gabrat_sub_analysis[, 5:11])



## ----Mahalanobis-------------------------------------------------------------------------------------------------------------------------
# Using R's Mahalanobis function, add a new column to your data
v.l.gabrat_sub_analysis$mahalanobis <- mahalanobis(
  v.l.gabrat_sub_analysis[, 5:11],

  # Get a vector of column means
  colMeans(v.l.gabrat_sub_analysis[, 5:11]),

  # Get a covaraince matrix
  cov(v.l.gabrat_sub_analysis[, 5:11])
)

# Calculate p-values using k-1 df = 6
v.l.gabrat_sub_analysis$pvalue <- pchisq(v.l.gabrat_sub_analysis$mahalanobis,
  df = 6, lower.tail = FALSE
)

# Inspect Mahalanobis distances and p-values
v.l.gabrat_sub_analysis[, colnames(v.l.gabrat_sub_analysis) %in% 
                          c("mahalanobis", "pvalue")]


## ----Output files------------------------------------------------------------------------------------------------------------------------
# Function to create and return a directory path
create_directory <- function(path_components) {
  directory_path <- paste(path_components, collapse = "/")
  if (!dir.exists(directory_path)) {
    dir.create(directory_path)
  }
  return(directory_path)
}

# Create and get the output directory path
out_path <- create_directory(c(data_location, "output"))

# Save a single data frame to CSV
save_to_csv <- function(data, filename) {
  file_path <- paste0(out_path, "/", filename)
  write.csv(data, file = file_path, row.names = FALSE)
}

# Save the gabrat_res_analysis data frame
save_to_csv(gabrat_res_analysis, "gabrat_res_analysis.csv")

# Get the list of objects with "analysis" in their names
savefile_list <- ls(pattern = "analysis")

# Save each data frame in the list to a CSV file
for (data_name in savefile_list) {
  # Generate file name with date and custom format
  filename <- paste0(
    gsub("_analysis", "_data", data_name), 
    format(Sys.time(), "_%d_%b_%Y"), 
    ".csv"
  )
  
  save_to_csv(get(data_name), filename)
}





## ----data.table--------------------------------------------------------------------------------------------------------------------------
library(data.table)

# Convert animal_info_roi into data.table format
animal_info_roi_DT <- as.data.table(animal_info_roi)

# Create a function to generate the file path for LEIA analysis
generate_filepath <- function(row) {
  paste(data_location, row$Species, row$Ind, row$Dist, "LEIA", row$ROI,
        "_Local Edge Intensity Analysis.csv", sep = "/")
}

# Read data for each row in animal_info_roi_DT
LEIA_analysis_DT <- animal_info_roi_DT[, {
  file_path <- generate_filepath(.SD)
  fread(file_path)
}, by = 1:NROW(animal_info_roi_DT)]

# Remove unwanted columns
unwanted_columns <- c("NROW", "V1", "Image", "Transform")
LEIA_analysis_DT[, (unwanted_columns) := NULL]

# Combine the original and new data
LEIA_analysis_DT <- cbind(animal_info_roi_DT, LEIA_analysis_DT)

# Inspect the combined data
head(LEIA_analysis_DT)

# Define the file name for saving
fname <- paste0(
  out_path, "/", gsub("_analysis", "", "LEIA_analysis_DT"),
  "_data", format(Sys.time(), "_%d_%b_%Y"), ".csv"
)

# Save the data to a CSV file
fwrite(LEIA_analysis_DT, file = fname, row.names = FALSE)


