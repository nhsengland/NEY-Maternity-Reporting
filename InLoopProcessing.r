Indicator <- unique(MatData$Measure)

#  Automate the list of multipliers
multip <- MatData %>% select(Measure, `Unit of Measurement`) %>% distinct() %>% select(`Unit of Measurement`)

multip$`Unit of Measurement` <- case_when(
  multip$`Unit of Measurement` == "Percentage" ~ 100,
  multip$`Unit of Measurement` == "Rate per 1,000" ~ 1000
)
# convert from dataframe!
multiplier <- multip[['Unit of Measurement']]

# create value and conditional format frames or required size
Output_values <- MatData %>% select(Org_Name, ICB_Code) %>% distinct()  
Output_cformat <- MatData %>% select(Org_Name, ICB_Code) %>% distinct() 
OutputExtra <- MatData %>% select(`Unit of Measurement`, `Direction Required`, Period_Name2, National_Latest_Rate, ICB_Code) %>% distinct() %>% filter(ICB_Code == ICB_filter) %>% select(-ICB_Code)
Alpha <- c()
Beta <- c()
Beta_test <- c()

# for each indicator, a funnel plot is calculated for trusts.  
# Trust values are saved in Output_values, and conditional formatting info in output_cformat 

# The exclude flag is for the first two indicators, 'tears' and 'apgar scores'.  Trusts with a value of zero 
# mess up the control limits, so are removed.


for (i in 1:length(Indicator)){
  
  Input_Data <- MatData %>%
    filter(Measure == Indicator[i] ) %>%
    filter(Exclude_Flag == 0) %>%
    filter(!Org_Level == "ICB") 
  NotExcluded <- MatData %>%
    filter(Measure == Indicator[i] ) %>%
    filter(!Org_Level == "ICB") 
  
  funnel_details <- funnel_plot(Input_Data, numerator=Alternative_Numerator, denominator=Denominator, 
                                group = Org_Name, title = Indicator[i], draw_unadjusted = FALSE,
                                draw_adjusted = TRUE, label = "highlight", data_type="PR", highlight=NA, limit=95,
                                multiplier = multiplier[i], y_label = Indicator[i], x_label = "xxx") 
  
  # funnel_details <- funnel_plot(numerator=Input_Data$Alternative_Numerator, denominator=Input_Data$Denominator, 
  #                               group = Input_Data$Org_Name, title = Indicator[i], draw_unadjusted = FALSE,
  #                               draw_adjusted = TRUE, label = "highlight", data_type="PR", highlight=NA, limit=95,
  #                               multiplier = multiplier[i], y_label = Indicator[i], x_label = "xxx") 
  
  tomerge <- source_data(funnel_details)

  
  #  
  #  extra bit for Spiegelhalter regression....  
  #
          regr_merge <- tomerge %>% filter(!rr %in% c(0, 1))
          regr_merge$denominator <- log(regr_merge$denominator)
          regr_merge$rr <-   log( regr_merge$rr/ (1 - regr_merge$rr) )
          
          Xmax <- max(regr_merge$denominator)  
          # linear modelling bit
          lm_spec <- linear_reg() %>%
            set_engine("lm")
          lm_wflow <- workflow() %>%
            #add_case_weights(case_wts) %>%
            #  add_recipe(base_recipe) %>%
            add_formula(rr ~ denominator) %>%    # NB  could have log(den)instead of log above!
            add_model(lm_spec)
          
          lm_model <- fit(lm_wflow, data = regr_merge) # , case_weights = ScatPCN$case_wts)
          
          x_range <- seq(0.00, Xmax, length.out = 11)
          x_range <- matrix(x_range, nrow = 11, ncol = 1)
          # something about plot and trace having same dimension to make hoverinfo work!!
          xdf <- data.frame(x_range)
          colnames(xdf) <- c("denominator")
          ydf <- lm_model %>% predict(xdf)
          colnames(ydf) <- c("Val")
          xy <- data.frame(xdf, ydf)
          Coeffs <- tidy(lm_model)
          eqntext <- paste("y = ", round(Coeffs$estimate[1], digits = 3), " + ", round(Coeffs$estimate[2], digits = 3), " x")
          Alpha[i] <- round(Coeffs$estimate[1], digits = 3)
          Beta[i] <- round(Coeffs$estimate[2], digits = 3)
          Beta_test[i] <- Coeffs$p.value[2] < 0.05
           
          #  cat("Beta : ", round(Coeffs$estimate[2], digits = 3), "\n \n")
  #  end of extra bit for Spiegelhalter regression....    
  
  
  
  tomerge$rr <- tomerge$rr * multiplier[i]  #  rr is the trust value on the plot
  
  Dir <- Input_Data$`Direction Required`[1]

#  is rr outside the confidence limits?  If so, higher or lower?
  tomerge$hilo <- case_when(
    (Dir == "Higher" & tomerge$rr > tomerge$OD95UCL) ~ "dark blue",
    (Dir == "Higher" & tomerge$rr < tomerge$OD95LCL) ~ "orange",
    (Dir == "Lower" & tomerge$rr > tomerge$OD95UCL) ~ "orange",
    (Dir == "Lower" & tomerge$rr < tomerge$OD95LCL) ~ "dark blue",
    (Dir == "No direction" & tomerge$rr > tomerge$OD95UCL) ~ "grey",
    (Dir == "No direction" & tomerge$rr < tomerge$OD95LCL) ~ "grey",
  )
  
  Ind <- Indicator[i]
  Out <- paste0(Ind, "_cformat")
  
  tomerge <- tomerge %>%
    select(group, rr, hilo) %>%
    rename( !!Ind := rr, !!Out := hilo )
  
#  Add trusts with excluded flag = 1 back in!
  TheExcluded_ToAdd <- NotExcluded %>% filter(Exclude_Flag == 1) %>% select(Org_Name)
  toAdd <- list(TheExcluded_ToAdd$Org_Name, "N/A", "N/A")
  tomerge <- rbindlist(list(tomerge, toAdd))

# need to add on the ICB as well
  test <- MatData %>% filter(ODS_Code == ICB_filter, Measure == Indicator[i]) %>% select(Rate)
  test$Rate <- ifelse(multiplier[i] == 100, test$Rate * multiplier[i], test$Rate * 1000)
  toAdd <- list(ICB_Name, test$Rate, "NA")
  tomerge <- rbindlist(list(tomerge, toAdd))

  #merge values, outlier with "Output"
  Output_values <- merge(Output_values, tomerge, by.x='Org_Name', by.y='group', all.x=TRUE) %>% select(-all_of(Out))
  Output_cformat <- merge(Output_cformat, tomerge, by.x='Org_Name', by.y='group', all.x=TRUE) %>% select(-all_of(Ind))
  
}

# restrict table of values to ICB of interest!
Output_values <- Output_values %>% filter(ICB_Code == ICB_filter) %>% select(-ICB_Code)
Output_cformat <- Output_cformat %>% filter(ICB_Code == ICB_filter) %>% select(-ICB_Code)

#  Excluded were given a value of zero, formatting of dark blue, now making them minus -1 (see * below), NA
# Output_cformat[Output_cformat == "N/A"] <- "dark blue"
# Output_values[Output_values == "N/A"] <- 0
Output_cformat[Output_cformat == "N/A"] <- NA
Output_values[Output_values == "N/A"] <- -1

# Make any NA values (i.e. missing values) be coloured red
Output_cformat[is.na(Output_values)] <- "red"

ICB_Trusts <- Output_values[['Org_Name']]    # Also includes the ICB
ICB_Trusts_cformat <- paste0(ICB_Trusts, '_cformat')

# few bits of formatting....
# (*)  Note that adding N/A made columns character not numeric...
num_cols <- as.numeric(ncol(Output_values))
Output_values <- Output_values %>% mutate(across(c(4:num_cols), as.numeric, 1))
# round all to 1 dp
Output_values <- Output_values %>% mutate(across(c(4:num_cols), round, 1))

Output_values[Output_values == -1] <- "Suppressed"

# add percentage sign for % indicators! 
# Output_values$`Baby hospital readmission` <- ifelse(Output_values$`Baby hospital readmission` == "Suppressed","Suppressed",paste0(Output_values$`Baby hospital readmission`,' %'))
# Output_values$`Breast feeding at 6-8 weeks` <- ifelse(Output_values$`Breast feeding at 6-8 weeks` == "Suppressed","Suppressed",paste0(Output_values$`Breast feeding at 6-8 weeks`,' %'))
Output_values$`Breast milk at first feed` <- ifelse(Output_values$`Breast milk at first feed` == "Suppressed","Suppressed",paste0(Output_values$`Breast milk at first feed`,' %'))
Output_values$`Births Induced` <- ifelse(Output_values$`Births Induced` == "Suppressed","Suppressed",paste0(Output_values$`Births Induced`,' %'))
Output_values$`Births: Planned caesarean` <- ifelse(Output_values$`Births: Planned caesarean` == "Suppressed","Suppressed",paste0(Output_values$`Births: Planned caesarean`,' %'))
Output_values$`Births: Unplanned caesarean` <- ifelse(Output_values$`Births: Unplanned caesarean` == "Suppressed","Suppressed",paste0(Output_values$`Births: Unplanned caesarean`,' %'))
Output_values$`Births: Forceps and ventouse` <- ifelse(Output_values$`Births: Forceps and ventouse` == "Suppressed","Suppressed",paste0(Output_values$`Births: Forceps and ventouse`,' %'))
Output_values$`Births: Spontaneous vaginal` <- ifelse(Output_values$`Births: Spontaneous vaginal` == "Suppressed","Suppressed",paste0(Output_values$`Births: Spontaneous vaginal`,' %'))
Output_values$`Continuity of Carer Pathway` <- ifelse(Output_values$`Continuity of Carer Pathway` == "Suppressed","Suppressed",paste0(Output_values$`Continuity of Carer Pathway`,' %'))
Output_values$`Pre Term Births` <- ifelse(Output_values$`Pre Term Births` == "Suppressed","Suppressed", paste0(Output_values$`Pre Term Births`,' %'))
Output_values$`Robson group 1 - C-sec rate` <- ifelse(Output_values$`Robson group 1 - C-sec rate` == "Suppressed","Suppressed",paste0(Output_values$`Robson group 1 - C-sec rate`,' %'))
Output_values$`Robson group 2 - C-sec rate` <- ifelse(Output_values$`Robson group 2 - C-sec rate` == "Suppressed","Suppressed",paste0(Output_values$`Robson group 2 - C-sec rate`,' %'))
Output_values$`Robson group 5 - C-sec rate` <- ifelse(Output_values$`Robson group 5 - C-sec rate` == "Suppressed","Suppressed",paste0(Output_values$`Robson group 5 - C-sec rate`,' %'))
Output_values$`Smoking at Booking` <- ifelse(Output_values$`Smoking at Booking` == "Suppressed","Suppressed",paste0(Output_values$`Smoking at Booking`,' %'))
Output_values$`Smoking at Delivery` <- ifelse(Output_values$`Smoking at Delivery` == "Suppressed","Suppressed",paste0(Output_values$`Smoking at Delivery`,' %'))

# replace 'NA %' with a blank!
Output_values[Output_values == "NA %"] <- ""

# add England to table
OutputExtra <- OutputExtra %>% rename(England = National_Latest_Rate, 'Three months_to' = Period_Name2)
# round to 1 dp
# OutputExtra$England[17] <- round(OutputExtra$England[17], digits=1)
OutputExtra$England[num_cols - 2] <- round(OutputExtra$England[num_cols - 2], digits=1)
OutputExtra$England[num_cols - 1] <- round(OutputExtra$England[num_cols - 1], digits=1)
OutputExtra$England <- ifelse(OutputExtra$`Unit of Measurement` == "Percentage",
                              paste0(round(OutputExtra$England *100, digits=1), ' %') ,  round(OutputExtra$England,0))

# Transpose...!
  Outtmp <- data.frame(Output_values)
  Out_val_tp <- transpose(Outtmp)
  Out_val_tp <- data.frame(t(sapply(Outtmp,c)))
  colnames(Out_val_tp) <- Output_values$Org_Name 
  Out_val_tp$Indicator <- colnames(Output_values)
  Out_val_tp <- Out_val_tp[-1,]      # remove first row
  
  for (col in 1:ncol(Output_cformat)){
    colnames(Output_cformat)[col] <-  sub("_.*", "", colnames(Output_cformat)[col])
  }
  
  Outtmp <- data.frame(Output_cformat)
  Out_cf_tp <- transpose(Outtmp)
  Out_cf_tp <- data.frame(t(sapply(Outtmp,c)))
  colnames(Out_cf_tp) <- Output_cformat$Org  #_Name 
  Out_cf_tp <- Out_cf_tp[-1,]
  # add something to end of column names!!
  colnames(Out_cf_tp) <- paste(colnames(Out_cf_tp),"cformat",sep="_")

# then merge two outputs.....
# indicator is last column of Out_val_tp; want it to be first
# ICB name can be anywhere in Out_val_tp; want it to be 6th (after England!)
Output <- cbind(OutputExtra, Out_val_tp, Out_cf_tp) %>%
  relocate(Indicator) 
Output <- Output %>%
  relocate(match(ICB_Name, names(Output)), .after = England)

# this is just to add asterisks to names!
Output$`Indicator` <- case_when(Output$`Indicator` == "Births Induced" ~ "Births Induced*"
                                ,Output$Indicator == "Births: Planned caesarean" ~ "Births: Planned caesarean*"
                                ,Output$Indicator == "Births: Unplanned caesarean" ~ "Births: Unplanned caesarean*"
                                ,Output$Indicator == "Births: Forceps and ventouse" ~ "Births: Forceps and ventouse*"
                                ,Output$Indicator == "Births: Spontaneous vaginal" ~ "Births: Spontaneous vaginal*"
                                ,Output$Indicator == "Breast milk at first feed" ~ "Breast milk at first feed*"
                                ,Output$Indicator == "Breast feeding at 6-8 weeks" ~ "Breast feeding at 6-8 weeks*"
                                ,Output$Indicator == "Continuity of Carer Pathway" ~ "Continuity of Carer Pathway*"
                                ,Output$Indicator == "Pre Term Births" ~ "Pre Term Births*"
                                ,Output$Indicator == "Smoking at Booking" ~ "Smoking at Booking*"
                                ,Output$Indicator == "Smoking at Delivery" ~ "Smoking at Delivery*"
                                , TRUE ~ Output$`Indicator`)


##Formatting for data tables
#Create HTML formatting code for header and overall table HTML container
#create header style HTML code
header.style <- "th { font-size: 12px; font-weight: bold; color: white; background-color: #005EB8;}"
#pull header names from the table
header.names <- c(colnames(Output))    # removed the " " at start,as I exclude rownames
# The container parameter allows us to design the header of the table using CSS
my.container <- withTags(table(
  #  tableHeader(Output),
  tableFooter(Output),
  style(type = "text/css", header.style),
  thead(
    tr(
      lapply(header.names, th, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
    )
  )
))

# need the following to hide the cformat columns
col2 <- ncol(Output) - 1
col1 <- ncol(Output) - length(ICB_Trusts)

ICB_datatable <- datatable(Output, 
          callback=JS('$("button.buttons-copy").css("background", "#5fa2ce"); 
                    $("button.buttons-csv").css("background", "#5fa2ce"); 
                    return table;'),
          rownames = FALSE, extensions = "Buttons", 
          container = my.container,  
          options = list(dom = 'Blfrtip', 
                         columnDefs = list(list(targets = c(col1:col2), visible = F),  #14:22 for NENC, 11:16 for WY
                                           list(className = 'dt-center', targets = "_all")),
                         buttons = list(list(extend = 'csv', exportOptions = list(columns = ':visible')), 
                                        list(extend = 'copy', exportOptions = list(columns = ':visible')), 
                                        list(extend = 'colvis', columns= c(2:col1-1)) ),
                             lengthMenu = c(20, 30) ) ) %>%  ##Option for how many rows appears in the table (10 or 20) 
  formatStyle(
    ICB_Trusts,
    valueColumns = ICB_Trusts_cformat,
    backgroundColor  = styleEqual(c('orange', 'dark blue', 'grey', 'red'), c('#c85200', '#1170aa', '#a3acb9', '#D30000') ) )
