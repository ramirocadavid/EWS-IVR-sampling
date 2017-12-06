
# Calculate sample size -------------------------------------------------------------

# Calculated using this tool: https://www.surveysystem.com/sscalc.htm
# Populations size = 27,554
# Margin of error = 5%
# Confidence level = 95%
# Retention rate = 10%


# Select sample ---------------------------------------------------------------------

# Data source in https://farmerlink.force.com/taroworks/00O58000004NWU2
# Import data and remove other rows (confidentiality message)
farmers <- read.csv("ews_farmers.csv", na.strings = "")
names(farmers) <- c("First.Name", "Last.Name", "Adult.number", "Primary.Phone.Number", 
                    "Mobile", "EWS.Farmer.Name", "Location.captured.by.GPS", "Province",
                    "Municipality", "Baranggay" )
farmers <- farmers[1:27408, ]

# Sample size returned = 379/0.1 = 3,790
sample.size <- 3790
size <- 3790/ nrow(farmers)

# Function and documentation for stratification:
# https://www.rdocumentation.org/packages/fifer/versions/1.0/topics/stratified
source("stratified.R")

# Define groups to stratify on
library(dplyr)
tables <- function(x) {data.frame(table(select(farmers, x)))}
to.table <- names(farmers)[8:10]
for(i in 1:3) {
      print(to.table[i])
      print(tables(to.table[i]))
}
# Stratify by municipality!

# Sample from population
sample <- stratified(df = farmers, group = "Municipality", size = size)

# Sample from farmers with primary and adult number
farmers.primary.adult <- farmers[!is.na(farmers$Primary.Phone.Number) | 
                                       !is.na(farmers$Adult.number), ]
sample.size.primary.adult <- 3790 / nrow(farmers.primary.adult)
sample.primary.adult <- stratified(farmers.primary.adult, "Municipality",
                                   sample.size.primary.adult)

# Sample from farmers with primary number
farmers.primary <- farmers[!is.na(farmers$Primary.Phone.Number), ]
sample.size.primary <- 3790 / nrow(farmers.primary)
sample.primary <- stratified(farmers.primary, "Municipality", sample.size.primary)

# Sample from farmers with mobile number
farmers.mob <- farmers[!is.na(farmers$Mobile), ]
sample.size.mob <- 3790 / nrow(farmers.mob)
sample.mob <- stratified(farmers.mob, "Municipality", sample.size.mob)

# # Check that proportions are the same in population and sample
# t.municipality <- tables("Municipality")
# t.municipality <- data.frame(t.municipality,
#                              proportion = t.municipality$Freq / nrow(farmers))
# t.municipality <- data.frame(t.municipality,
#                              sample.size = t.municipality$proportion * sample.size)
# t.municipality <- data.frame(t.municipality,
#                              sample = data.frame(table(sample$Municipality))[2])

# Export
library(xlsx)
write.xlsx(sample, "Sample_all_farmers.xlsx")
write.xlsx(sample.primary.adult, "Sample_primary_adult.xlsx")
write.xlsx(sample.primary, "Sample_primary.xlsx")
write.xlsx(sample.mob, "Sample_mobile.xlsx")
