# Check the available IR data exported from uFTIR
# //!\\ Not available for public review //!\\
# IR data is only available on restricted drive : W:drive "//wurnet.nl/dfs-root/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/1_FTIR_rawdata/uFTIR_files"
# or upon justified request 

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(stats)
rm(list=ls()) # cleaning console
graphics.off() # cleaning plots
'%!in%' <- function(x,y)!('%in%'(x,y))

# Set WD in the project: 
setwd("C:/Users/berio001/Documents/MINAGRIS_C/MINAGRIS_Microplastic_Soil_Assessmnent")
wd.out= "Outputs" 

# Load the MINAGRIS_Read_Labels_function.R
source("MINAGRIS_Read_Labels_Function.R") 


# 1. Summarize IR files Available ####

  # * Access IR data folder names ####
    # List of folders produced after uFTIR aquisition
    # Directory IR files
    wd.in.IR="//wurnet.nl/dfs-root/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/1_FTIR_rawdata/uFTIR_files"
    setwd(wd.in.IR)
    
    # Create function to set the depth of loading
      list.dirs.depth.n <- function(p, n) {
        res <- list.dirs(p, recursive = FALSE)
        if (n > 1) {
          add <- list.dirs.depth.n(res, n-1)
          add
        } else {
          res
        }
      } #end function
      
    # Create list of IR files available 
    IR_File_names <-list.dirs.depth.n(wd.in.IR,2)
        
    # Create Data Frame of IR files available
    METADATA_IR=data.frame(File.dir=IR_File_names, n.uP.detected=NA )
    METADATA_IR$IR_File_name <- gsub(".*/", "", METADATA_IR$File.dir)   # Remove the working directory name
        
    # /!\ There should not be Duplicated files! 
    if (any(duplicated( METADATA_IR$IR_File_name))){
      warning("Duplicated IR File names")
      METADATA_IR$File_names[duplicated( METADATA_IR$IR_File_name)]
    }
  
    # Read labels
    METADATA_IR=Read_MINAGRIS_label(METADATA_IR)
        
    # Total number of IR files: 
    nrow(METADATA_IR)
  
  # * Summary Batch ####
    
    Summary_IR_Batch =  METADATA_IR%>% 
      group_by( Batch_id, Batch_name) %>% # Group per CSS
      summarise(N_Files=n(),
                N_filters=length(unique(Filter_name)), 
               # N_Soil_samples=length(unique(Soil_sample)), #Number of extractions 
                Spike_soil=  paste(Filter_name[Sample_type %in% c("s", "s2")], collapse = ", "),
               Soil_sample_names= paste0(unique(Soil_sample), collapse = " ; "),
                .groups = "drop") 
    
    
  # * Number of unique soil samples per CSS ####
  
      # for (css in 1:11) {
      #   print( paste("CSS",css, ";", length(unique(METADATA_IR$Farm[METADATA_IR$CSS==paste(css)])),"Unique Farms", ";", length(unique(METADATA_IR$Soil_sample[METADATA_IR$CSS==paste(css)])), "Unique soils"))
      #   print(sort(unique(METADATA_IR$Soil_sample[METADATA_IR$CSS==paste(css)])))
      # }  
  
    
      Summary_IR_CSS = subset( METADATA_IR, Sample_type =="n" )%>% 
        group_by(  CSS) %>% # Group per CSS
        summarise(N_Files=n(),
                  n_farms= length(unique(Farm)),
                  n_fields= length(unique(Soil_sample)),
                 Sample= paste0(unique(Soil_sample), collapse = " ; ") ) 
                 
  
  
  # * Number of QCs ####
        # - Blank chemicals, bsm
        # - Spikes, s1 or s2
        # - St soil, st
        # - Replicated soil, r
        # - Operator check
  
      Summary_IR_QC = subset(METADATA_IR, Soil_sample %in% c("bcm","pfsr","st") |  Sample_type %in% c("r","s2","s" ) )   %>% 
        group_by(  Soil_sample, Sample_type  ) %>% # Group per CSS
        summarise(N_Files=n(),
                  Batch= unique(paste0(Batch_name, collapse = " ; ") ))
      
      
      
# 2. Export tables ####
  #Set WD in the project: 
    setwd("C:/Users/berio001/Documents/MINAGRIS_C/MINAGRIS_Microplastic_Soil_Assessmnent")
    setwd("C:/Users/berio001/Documents/MINAGRIS_C/MINAGRIS_Microplastic_Soil_Assessmnent")
    
   write.csv(METADATA_IR, paste(wd.out,"IR_METADATA_2025.08.13.csv",sep = "/"))
   write.csv( Summary_IR_Batch, paste(wd.out,"IR_SummaryBatch_2025.08.13.csv",sep = "/"))
   write.csv( Summary_IR_QC, paste(wd.out,"IR_SummaryQC_2025.08.13.csv",sep = "/"))
   write.csv( Summary_IR_CSS, paste(wd.out,"IR_SummaryCSS_2025.08.13.csv",sep = "/"))

