############################################################
# Analysis for Henning
############################################################

############################################################
# Settings
############################################################

############################################################
# Functions
############################################################

############################################################
# Install packages
############################################################
# ipak function: install and load multiple R packages.
# Check to see if packages are installed. Install them if they are not, then load them into the R session.
.ipak = function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  lapply(pkg, library, character.only = TRUE)
}

# List of packages you want to install/use
packages = c("readxl", "ggplot2", "dplyr")

# Install and load packages
.ipak(packages)

############################################################
# Set paths & read in files
############################################################
# Set WorkDir
WorkDir = "C:/Users/Manuel/Documents/Github_desktop/g4_hunter_plotting/"
setwd(WorkDir)

FilepathRawData = "CGGBP1_TSS_10000-W25-S0.0.txt"

##################################################
# Read in files
##################################################
RawData <- read.table(FilepathRawData, 
                      fill=TRUE,
                      nrows = 50000, # Read in a couple of lines
                      header = F)

############################################################
# Prepare data
############################################################
# Get colnames for Rawdata, stored in second row
cnames = RawData[2,]
# Remove non character stuff from names
cnames = sub("[^a-zA-Z0-9]", "", cnames)
cnames = sub(" ", "", cnames)
# finally rename
colnames(RawData) = cnames

# Get Rowindex for chromosome indicator (chrID)
rowIDs = which(grepl(">",RawData$Start))
# Write into dinfo
dinfo = data.frame(chrID = RawData[rowIDs, 1], srowID = rowIDs, erowID = NA)
# get number of rows
rowcount = nrow(dinfo)
# Get end rowID
dinfo$erowID[1:(rowcount-1)] = dinfo$srowID[-1]-1
# Fill in lats erowID position
dinfo$erowID[rowcount] = nrow(RawData)

# Data for each chrID is stored in the next srowID:erowID rows followingafter chrID row and colname row
# Create a list holding information for each chrID
dlist <- list()
# Populate
for(chrID in dinfo$chrID){
  # Get rowIDs for current chrID
  # +2 to skip first two rows which contain chrID and colnames
  srowID = dinfo$srowID[dinfo == chrID]+2
  # Get end rowID
  erowID = dinfo$erowID[dinfo == chrID]
  # Get data for chrID
  dataset = RawData[srowID:erowID,]
  # Make some columns numeric
  dataset$Start = as.numeric(dataset$Start)
  dataset$End = as.numeric(dataset$End)
  dataset$Length  = as.numeric(dataset$Length )
  dataset$Score = as.numeric(dataset$Score)
  # write into dlist
  dlist[[chrID]] = dataset
}

############################################################
# Analysis
############################################################
# Create df holding Scores per chrID in columns
dfScore = bind_rows(dlist, .id = "chrID")
# Calculate mean(Score) for each Start position
dfmean = dfScore %>%
  group_by(Start) %>%
  summarise(Score_mean = mean(Score, na.rm = TRUE,),
            Score_sd = sd(Score, na.rm = TRUE))

############################################################
# Plots
############################################################
# Define ggplot2 settings
global_theme <- theme_classic()+
  theme(axis.text.x = element_text(angle = 0),
        line = element_line(size = 1/.pt)
  )
# Set global ggplot2 theme
theme_set(global_theme)

##################################################
# Plot Score for first chrID
##################################################
# Select chrID, then plot
temp = dlist[[1]]
ggplot(data = temp , aes(x = Start, y = Score))+
  geom_point(size = 0.1)

##################################################
# Plot Score for first chrID
##################################################
ggplot(data=dfmean, aes(x=Start, y=Score_mean, ymin=Score_mean-Score_sd, ymax=Score_mean+Score_sd))+
  #geom_point(size=0.1)+
  #geom_ribbon(alpha=0.5, fill="lightblue")+
  geom_errorbar(width=0.5, size=1, alpha=0.1, color="grey")+
  geom_line()





