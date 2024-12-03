# Run this file to produce Maternity Metric report for each of 4 ICBs

library("rmarkdown")

# this calls the libraries, reads the data, and creates the funnel plot function
source("OneTimeProcessing.r")
source("HIE and Neonatal Charts Working.r")

odscode <- c("QYG")
orgname <- c("Cheshire & Merseyside")

# just for London
# odscode <- c("QMF", "QRV", "QWE", "QKK", "QMJ")  #
# orgname <- c("North East London", "North West London", "South West London", "South East London", "North Central London")  # 

#NEY and NW
# odscode <- c("QOQ", "QHM", "QF7", "QWO", "QE1", "QOP", "QYG")
# orgname <- c("Humber & North Yorkshire", "North East & North Cumbria", "South Yorkshire", "West Yorkshire", 
#              "Lancashire & South Cumbria", "Greater Manchester", "Cheshire & Merseyside")

# in a single for loop
for (icb in 1:length(odscode)){
  ICB_filter <- odscode[icb]
  ICB_Name <- orgname[icb]
  
  source("InLoopProcessing.r")
  
  render("ForEachICB.Rmd", output_file = paste0('Output/Maternity report', ICB_Name, '.html'))    
}
