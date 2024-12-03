# ```{r setup, include=FALSE}

here::i_am("RMDFolder/ForEachICB.Rmd")

library(here)
# library(NHSRplotthedots)
library(tidyr)
library(dplyr)
library(readxl)
library(FunnelPlotR)
library(ggplot2)
library(plotly)
library(ExcelFunctionsR)
library(DT)
library(data.table)
library(binom)
library(zoo)
library(htmltools)
library(stringr)
library(tidymodels)
library(kableExtra)

# remotes::install_github("https://github.com/chrismainey/FunnelPlotR")
# ```


### Read in Data and do some basic formating ###
# MatData <- read_excel(here("Data/Data_Extract_New_Delivery_Method.xlsx"), sheet = "Data 20240104")
MatData <- read_excel(here("Data/Data Extract September 2024.xlsx"), sheet = "Data")
MatDataLabels <- read_excel(here("Data/Data Extract September 2024.xlsx"), sheet = "labels") 
MetaData <- read_excel(here("Data/MetaData.xlsx"), sheet = 1) 

# 2024/01/29 remove "Stillbirth Ratio (SUS)"
  MatData <- MatData %>% filter(Measure != "Stillbirth Ratio" & Measure != "Baby hospital readmission" & Measure !="Breast feeding at 6-8 weeks")
  MatDataLabels <- MatDataLabels %>% filter(Measure != "Stillbirth Ratio" & Measure != "Baby hospital readmission" & Measure !="Breast feeding at 6-8 weeks")
  MetaData <- MetaData %>% filter(Measure != "Stillbirth Ratio" & Measure != "Baby hospital readmission" & Measure !="Breast feeding at 6-8 weeks")
  
Month_Lookup <- read_excel(here("Data/Month_Lookup.xlsx"), sheet = 1) 

#Shorten the Trust names where possible, as they will be column headings
MatData$`Org_Name` <- case_when(RIGHT(MatData$`Org_Name`,30) == "Hospitals NHS Foundation Trust" ~
                                  LEFT(MatData$`Org_Name`,LEN(MatData$`Org_Name`) - 31)
                                ,RIGHT(MatData$`Org_Name`,20) == "NHS Foundation Trust" ~
                                  LEFT(MatData$`Org_Name`,LEN(MatData$`Org_Name`) - 21)
                                ,RIGHT(MatData$`Org_Name`,9) == "NHS Trust" ~
                                  LEFT(MatData$`Org_Name`,LEN(MatData$`Org_Name`) - 10)
                                , TRUE ~ MatData$`Org_Name`)
MatData$`Org_Name` <- str_replace(MatData$`Org_Name`, "and", "&")

MatData$Period_Name2 <- str_replace(MatData$Period_Name, "3 months to ", "")
MatData$`Direction Required` <- str_replace(MatData$`Direction Required`, " is Better", "")

# for ICB, shorten it's name
MatData$`Org_Name` <- str_replace(MatData$`Org_Name`, " Integrated Care Board", "")

### Funnel Plot Function ###
Funnel_Function <-function(Indic, Highlight_Outliers, Limit, Title, Multiplier, YLabel, XLabel) {
  
  #Filter For Metric Required, and also don't include the ICBs
  Input_Data <- MatData %>%
    filter(Measure == Indic) %>%
    filter(!Org_Level == "ICB") %>%
    filter(Exclude_Flag == "0")
  
  Period <- unique(Input_Data$Period_Name)
  Title <- paste(Title, Period, sep = " - ")
  
  # Creates a unique list of ICB Trusts which will be used to highlight outliers in the final chart. If you want to display outlier trusts in an ICB you just need to get the filter to look at the ICB column  instead of the region one
  TrustsInICB2disp <- Input_Data %>%
    select(Org_Name) %>%
    filter(Input_Data$ICB_Code == Highlight_Outliers)
  
  #Changed to as character as was having issues displaying on the final chart
  HighLight <- as.character(TrustsInICB2disp$Org_Name)
  
  # Run the funnel plot a second time - this is the one that will be displayed, with the list of ICB outlier trusts shown
  if (nrow(TrustsInICB2disp) == 0){
    funnel_plot(Input_Data, numerator=Alternative_Numerator, denominator=Denominator, group = Org_Name, 
              title = Title, draw_unadjusted = FALSE, draw_adjusted = TRUE,label = "highlight", data_type="PR",
              highlight=NA, limit=95, multiplier = Multiplier, y_label = YLabel, x_label = XLabel)
  } else {
  funnel_plot(Input_Data, numerator=Alternative_Numerator, denominator=Denominator, group = Org_Name, 
              title = Title, draw_unadjusted = FALSE, draw_adjusted = TRUE,label = "highlight", data_type="PR",
              highlight=HighLight, limit=95, multiplier = Multiplier, y_label = YLabel, x_label = XLabel)
  }  
}




#Create HTML formatting code for header and overall table HTML container
#create header style HTML code
header.style <- "th { font-size: 12px; font-weight: bold; color: white; background-color: #005EB8;}"
#pull header names from the table
header.names <- c(colnames(MetaData))    # removed the " " at start, as I exclude rownames
# The container parameter allows us to design the header of the table using CSS
my.container <- withTags(table(
  style(type = "text/css", header.style),
  thead(
    tr(
      lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
    )
  )
))

# Create Metadata Table
MetaTable <- datatable(MetaData, 
                       rownames = FALSE, extensions = "Buttons", 
                       container = my.container,  
                       options = list(
                         columnDefs = list(
                           list(className = 'dt-center', targets = "_all")),
                         lengthMenu = c(20, 30) ) )   ##Option for how many rows appears in the table (10 or 20) 
