# Change working directory
setwd("EngageSpark Data Sets")

# List files to import
library(xlsx)
files <- list.files(pattern = "FL_")
# Import files
data <- c()
for(i in 1:length(files)) {
      temp <-  read.xlsx(files[i], sheetIndex = 1)
      data <- rbind(data, temp)
      rm(temp)
}

# Generate sample
source("../stratified.R")

# Validate stratification variables
data.frame(table(data$Municipality))

# Generate sample from data
sample <- stratified(df = data, group = "Municipality", size = 3790/nrow(data))


# Check that proportions are the same in population and sample
tables <- function(x) {data.frame(table(select(data, x)))}
library(dplyr)
t.municipality <- tables("Municipality")
t.municipality <- data.frame(t.municipality,
                             proportion = t.municipality$Freq / nrow(data))
t.municipality <- data.frame(t.municipality,
                             sample.size = t.municipality$proportion * 3790)
t.municipality <- data.frame(t.municipality,
                             sample = data.frame(table(sample$Municipality))[2])

# Export sample
write.xlsx(sample, "EWS_sample.xlsx")
