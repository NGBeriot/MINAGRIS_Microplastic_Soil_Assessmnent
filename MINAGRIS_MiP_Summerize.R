# Script to Make summary table of any kind ; create size categories 

# Expected Data Structure: 

# Column name       #Description         	                                                        #Data format
  # File_Names       	Name of the Processed, manually checked IR file	                              TEXT.csv
  # Lab	              Analysis lab	                                                                {"WUR", "Ubern"}
  # Batch 	          Analysis batch	                                                              #Number
  # Preparation_Type	The kind of preparation for the samples: 	                                    {"Blank_chemical", "Spiked", "Reference_Soil", "Standard_Soil", "Field_samples"}
  # Sample_type	      Type of samples taken into account during the analysis                      	{n, s, r, st}
  # Soil_sample	      Soil code (CSS, Farm, Field) used for the preparation	                        {bcm, rs, st, c(#CSS, #Farm, #Field)}
  # Filter_name 	    Name of the extracted filter	                                                Batch_"Soil_sample"_"Sample_type"
  # IR_rep 	          Only if a specific filter is acquired twice in IR	                            {ir1, ir2, ir3}
  # PMF_rep	          Only if a specific acquired IR file is processed twice by the same operator	  {pmf1, pmf2, pmf3}
  # Operator	        Operator who processed the IR file and did the manual checking	              {"EC", "JM", "SR", "AG"}
  # ID	              Particle ID	#Number
  # Q_index	          Matching Quality Index	[-]
  # Polymer.grp,	    Name of best polymer match	txt
  # Polymer.red12,	  Name of best polymer match, in a list of 12 main polymers, rest as others. 	txt
  # Polymer.red3	    Name of best polymer match, PE, PVC or PLA, rest as others. 	txt
  # Area,	            Measured Area	[um2]
  # length,	          Measured length	[um]
  # Width,	          Measured width	[um]
  # Aspect_ratio,	    Calculated aspect ratio	[-]
  # Mass, 	          Calculated mass	[ng]
  # Size_cat.um	      Associated size category 	txt

# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\

# "Other.Plastic" Are showed in the details per polymer and of course conserved in particle tables but 
#  *Excluded* from total counts (e.g. summary per farm or blanks => no blank correction) 

# /!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
    
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(stats)


rm(list=ls()) # cleaning console
graphics.off() # cleaning plots
'%!in%' <- function(x,y)!('%in%'(x,y))

setwd("C:/Users/berio001/Minagris/MINAGRIS_Microplastic_Soil_Assessmnent")

wd.out= "Outputs" # W:/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Purency Microplastic Finder/PMF_Results_Summary"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"


# Initialization 
Data_comb_red_blank=read.csv("Outputs/Corrected_MiP_Particles_20241113.csv")
    

    colnames(Data_comb_red_blank)
    
    # "File_Names"
    # "Lab"          
    # "Batch_Name" 
    # "Preparation_Type"  
    # "Sample_type"  
    # "Soil_sample"          
    # "CSS"
    # "Farm"
    # "Field"
    # "Filter_div"
    # "Filter_name" 
    # "IR_rep"                                     
    # "PMF_rep"                    
    # "Operator" 
    # 
    # "ID"               
    # "Q_index"
    # "Polymer.grp"
    # "Polymer.red12"  
    # "Polymer.red3"    
    # 
    # "N.px"
    # "Area.um2.cor"
    # "Length.um"        
    # "Width.um"         
    # "Aspect_ratio"
    # "Mass.ng"
    # "Size_cat.um" 
    
    # File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample          
    # CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator 
    # ID, Q_index, Polymer.grp, Polymer.red12, Polymer.red3, N.px, Area.um2.cor, Length.um, Width.um, Aspect_ratio, Mass.ng, Size_cat.um 

# For all Files:   
  # 1. Sum per Processed and manually checked results file / "File_Names" <-> "ID". "Q_index" 
  # 2. Mean per multiple IR Acquisition, Processing or manually checking results file <-> "IR_rep", "PMF_rep", "Operator"
  # 3. Sum per filter.div (one 5g sub sample extraction in multiple filters) <-> "Filter_div"
    
# Only for Field Samples:   
  # 4. Mean per soil rep (multiple 5g extractions for the same Soil Sample) <-> "Batch_Name" 
  # 5. Mean per field  <-> "lab"      
  # 6. Mean per CSS
  # 7. Mean MINAGRIS  
        
  # .a: all factors
  # .b: per Polymer.red12 * per Size_cat.um
  # .c: per Polymer.red12
  # .d: sum up all polymers, "Other.Plastic" excluded 
    

# 1. Summaries, processed (PMF) File ####   
    # *1.a Sum up per processed (PMF) File, all factors  ####
    Summary1a_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator,
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  %>%  #
    # Complete the Summary1_File: 
    # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,
                       CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator),
               nesting(Polymer.grp,Polymer.red12, Polymer.red3, Size_cat.um), 
             fill=list(N.particles=0,
                       Num.px=0,
                       Tot.Area.mm2=0,
                       Median.Area.sqrt.um=0,
                       SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.grp!="No.plastic")

    
    # *1.b Sum up per processed (PMF) File, Polymer.red12 * per Size_cat.um  ####
    Summary1b_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator,
               Polymer.red12,  Polymer.red3, Size_cat.um ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%  #
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,
                       CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator),
               nesting(Polymer.red12, Polymer.red3, Size_cat.um), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.red12!="No.plastic") #
    
    
    

    # *1.c Sum up per processed (PMF) File, Polymer.red12 ####
    Summary1c_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator,
               Polymer.red12,  Polymer.red3) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%  #
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,
                       CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator),
               nesting(Polymer.red12, Polymer.red3), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.red12!="No.plastic") #
    
    
    # *1.d Sum up per processed (PMF) sum up all polymers, "Other.Plastic" excluded  ####
    Summary1d_File = Data_comb_red_blank %>% 
      # "Other.Plastic" to 0
      mutate(N.px = if_else(Polymer.red12 == "Other.Plastic", 0, N.px),
             Area.um2.cor= if_else(Polymer.red12 == "Other.Plastic", 0,  Area.um2.cor),
             Mass.ng = if_else(Polymer.red12 == "Other.Plastic", 0,  Mass.ng) ) %>%
      
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator) %>%
      
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  #
    
    # *1.e Sum up per processed (PMF) sum up all polymers, "Other.Plastic" included ####
    Summary1e_File = Data_comb_red_blank %>% 
      
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator) %>%
      
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  #
    
    # *1.f Sum up per processed (PMF) File, all factors  ####
    Summary1f_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator,
               Size_cat.um) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  %>%  #
      # Complete the Summary1_File: 
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,
                       CSS, Farm, Field, Filter_div, Filter_name, IR_rep, PMF_rep, Operator),
               nesting(Size_cat.um), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0))

    
    
    
    
    
    
    
    # Check Total particles numbers
    sum(Summary1e_File$N.particles)
    sum(Summary1d_File$N.particles)
    sum(Summary1c_File$N.particles)
    sum(Summary1b_File$N.particles)
    sum(Summary1a_File$N.particles)
    length(Data_comb_red_blank$N.px[Data_comb_red_blank$N.px>0])
    
# 2. Summaries, IR results files ####     
    # *2a Mean over IR results files, all factors ####
    Summary2a_IRfiles = Summary1a_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, 
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    
    # *2b Mean over IR results files, per Polymer.red12 * per Size_cat.um####
    Summary2b_IRfiles = Summary1b_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, 
               Polymer.red12,  Polymer.red3, Size_cat.um) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    # *2c Mean over IR results files, per Polymer.red12####
    Summary2c_IRfiles = Summary1c_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, 
               Polymer.red12,  Polymer.red3) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    
    # *2d Mean over IR results files, sum up all polymers, "Other.Plastic" excluded ####
    Summary2d_IRfiles = Summary1d_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name) %>%
      
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    # *2e Mean over IR results files, sum up all polymers, "Other.Plastic" included ####
    Summary2e_IRfiles = Summary1e_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name) %>%
      
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    # *2f Mean over IR results files, all factors ####
    Summary2f_IRfiles = Summary1f_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Filter_div, Filter_name, 
               Size_cat.um) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    

    # Check Total particles numbers
    sum(Summary2e_IRfiles$Mean.particles)
    sum(Summary2d_IRfiles$Mean.particles)
    sum(Summary2c_IRfiles$Mean.particles)
    sum(Summary2b_IRfiles$Mean.particles)
    sum(Summary2a_IRfiles$Mean.particles)

    # Check Total files
    max(Summary2e_IRfiles$N.files)
    max(Summary2d_IRfiles$N.files)
    max(Summary2c_IRfiles$N.files)
    max(Summary2b_IRfiles$N.files)
    max(Summary2a_IRfiles$N.files)
    
# 3. Summaries, filter.div ####     
    # * 3a Sum per filter.div, all factors ####
    Summary3a_Filter = Summary2a_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    
    # * 3b Sum per filter.div, Polymer.red12 * per Size_cat.um####
    Summary3b_Filter = Summary2b_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,
               Polymer.red12,  Polymer.red3, Size_cat.um) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    
    # * 3c Sum per filter.div, Polymer.red12 ####
    Summary3c_Filter = Summary2c_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,
               Polymer.red12,  Polymer.red3) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # * 3d Sum per filter.div, sum up all polymers, "Other.Plastic" excluded  ####
    Summary3d_Filter = Summary2d_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field) %>%
      
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # * 3e Sum per filter.div, sum up all polymers, "Other.Plastic" included  ####
    Summary3e_Filter = Summary2e_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field) %>%
      
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # * 3f Sum per filter.div, all factors ####
    Summary3f_Filter = Summary2f_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,
                Size_cat.um) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # Check Total particles numbers
    sum(Summary3e_Filter$Mean.particles)
    sum(Summary3d_Filter$Mean.particles)
    sum(Summary3c_Filter$Mean.particles)
    sum(Summary3b_Filter$Mean.particles)
    sum(Summary3a_Filter$Mean.particles)

    
    # //\\ \\// Preparation_Type=="Field_samples" \\// //\\####

# 4. Summaries, Soil_samples ####     
    # * 4a Mean per Soil_samples, all factors ####
    Summary4a_Soil= subset( Summary3a_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type, Soil_sample,
               CSS, Farm, Field, 
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 

    
    # * 4b Mean per Soil_samples, Polymer.red12 * per Size_cat.um ####
    
    Summary4b_Soil= subset( Summary3b_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type, Soil_sample,
               CSS, Farm, Field, 
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 

    
    # * 4c Mean per Soil_samples, Polymer.red12 ####
    
    Summary4c_Soil= subset( Summary3c_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type,  Soil_sample,
               CSS, Farm, Field, 
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 

    
    # * 4d sum up all polymers, "Other.Plastic" excluded  ####
    
    Summary4d_Soil= subset( Summary3d_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type,  Soil_sample,
               CSS, Farm, Field ) %>% # For each PMF_File_name, get the summary
                 
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 4e sum up all polymers, "Other.Plastic" included ####
    
    Summary4e_Soil= subset( Summary3e_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type,  Soil_sample,
               CSS, Farm, Field ) %>% # For each PMF_File_name, get the summary
      
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 4f sum up all polymers, "Other.Plastic" included ####
    
    Summary4f_Soil= subset( Summary3f_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type, Soil_sample,
               CSS, Farm, Field, 
               Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    
    # Mean per field of the soil samples. 
    
# 5. Summaries, Field ####         
    # * 5a Mean per Field, all factors ####
    Summary5a_Field= Summary4a_Soil%>% 
      group_by(Preparation_Type, 
               CSS, Farm, Field, 
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.F= min( Mean.particles),
                 Max.particles.F= max( Mean.particles),
                 Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 5b Mean per Field_samples, Polymer.red12 * per Size_cat.um ####
    
    Summary5b_Field= Summary4b_Soil%>% 
      group_by(Preparation_Type, 
               CSS, Farm, Field, 
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 5c Mean per Field_samples, Polymer.red12 ####
    
    Summary5c_Field= Summary4c_Soil%>% 
      group_by(Preparation_Type,  
               CSS, Farm, Field, 
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.F= min( Mean.particles),
                 Max.particles.F= max( Mean.particles),
                 Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) ) #  Mean mass per sample and polymer, over the files/operators
       
    
    
    # * 5d sum up all polymers, "Other.Plastic" excluded  ####
    
    Summary5d_Field= Summary4d_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm, Field ) %>% # For each PMF_File_name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.F= min( Mean.particles),
                Max.particles.F= max( Mean.particles),
                Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) ) #  Mean mass per sample and polymer, over the files/operators
      
    
    # * 5e sum up all polymers, "Other.Plastic" excluded  ####
    
    Summary5e_Field= Summary4e_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm, Field ) %>% # For each PMF_File_name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.F= min( Mean.particles),
                Max.particles.F= max( Mean.particles),
                Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
   
    # Mean per CSS of the soil samples. 
    
# 6. Summaries, CSS ####      
    #  * 6a Mean per CSS, all factors #### 
    Summary6a_CSS= Summary4a_Soil %>% 
      group_by(Preparation_Type, 
               CSS, 
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )
    
    
    # * 6b Mean per CSS, Polymer.red12 * per Size_cat.um ####
    
    Summary6b_CSS= Summary4b_Soil %>% 
      group_by(Preparation_Type, 
               CSS,
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )
    
    
    # * 6c Mean per Field_samples, Polymer.red12 ####
    
    Summary6c_CSS= Summary4c_Soil %>% 
      group_by(Preparation_Type,  
               CSS,  
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )
    
    
    
    # * 6d sum up all polymers, "Other.Plastic" excluded  ####
    
    Summary6d_CSS= Summary4d_Soil %>% 
      group_by(Preparation_Type,  
               CSS, ) %>% # For each PMF_File_name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )

    # * 6e sum up all polymers, "Other.Plastic" excluded  ####

    Summary6e_CSS= Summary4e_Soil %>% 
      group_by(Preparation_Type,  
               CSS, ) %>% # For each PMF_File_name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) ) 
    
    
    # * 6f sum up all polymers, "Other.Plastic" excluded  ####
    
    Summary6f_CSS= Summary4f_Soil %>% 
      group_by(Preparation_Type, 
               CSS, 
               Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )
    
    


# 7. Summaries, Project #### 
    #  * 7a Mean all MINAGRIS, all factors #### 
    Summary7a_MINAGRIS= Summary4a_Soil %>%
      group_by(Preparation_Type,
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um ) %>%
      summarise(N.files = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) 
    
    
    # * 7b Mean all MINAGRIS, Polymer.red12 * per Size_cat.um ####
    
    Summary7b_MINAGRIS= Summary4b_Soil%>% 
      group_by(Preparation_Type, 
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise(N.files = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) 
    
    
    # * 7c Mean all MINAGRIS, Polymer.red12 ####
    
    Summary7c_MINAGRIS= Summary4c_Soil%>% 
      group_by(Preparation_Type,  
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_name, get the summary
      summarise(N.files = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) 
    
    
    # * 7d all MINAGRIS, "Other.Plastic" excluded  ####
    
    Summary7d_MINAGRIS= Summary4d_Soil %>% 
      group_by(Preparation_Type ) %>% # For each PMF_File_name, get the summary
      summarise(N.files = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) 
    
    # * 7e all MINAGRIS, "Other.Plastic" excluded  ####
    
    Summary7e_MINAGRIS= Summary4e_Soil %>% 
      group_by(Preparation_Type ) %>% # For each PMF_File_name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) 
    
    #  * 7f Mean all MINAGRIS, all factors #### 
    Summary7f_MINAGRIS= Summary4f_Soil %>%
      group_by(Preparation_Type,
             Size_cat.um ) %>%
      summarise(N.files = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) 
    
    
# Export ####
  
  Date="_20241118.csv"    
   write.csv(Summary1a_File, paste(wd.out,"/Summary1a_File", Date, sep = ""))
   write.csv(Summary1b_File, paste(wd.out,"/Summary1b_File", Date, sep = ""))
   write.csv(Summary1c_File, paste(wd.out,"/Summary1c_File", Date, sep = ""))
   write.csv(Summary1d_File, paste(wd.out,"/Summary1d_File", Date, sep = ""))
    
   write.csv(Summary2a_IRfiles, paste(wd.out,"/Summary2a_IRfiles", Date, sep = ""))
   write.csv(Summary2b_IRfiles, paste(wd.out,"/Summary2b_IRfiles", Date, sep = ""))
   write.csv(Summary2c_IRfiles, paste(wd.out,"/Summary2c_IRfiles", Date, sep = ""))
   write.csv(Summary2d_IRfiles, paste(wd.out,"/Summary2d_IRfiles", Date, sep = ""))
   
   write.csv(Summary3a_Filter, paste(wd.out,"/Summary3a_Filter", Date, sep = ""))
   write.csv(Summary3b_Filter, paste(wd.out,"/Summary3b_Filter", Date, sep = ""))
   write.csv(Summary3c_Filter, paste(wd.out,"/Summary3c_Filter", Date, sep = ""))
   write.csv(Summary3d_Filter, paste(wd.out,"/Summary3d_Filter", Date, sep = ""))
   
   write.csv(Summary4a_Soil, paste(wd.out,"/Summary4a_Soil", Date, sep = ""))
   write.csv(Summary4b_Soil, paste(wd.out,"/Summary4b_Soil", Date, sep = ""))
   write.csv(Summary4c_Soil, paste(wd.out,"/Summary4c_Soil", Date, sep = ""))
   write.csv(Summary4d_Soil, paste(wd.out,"/Summary4d_Soil", Date, sep = ""))
   
   write.csv(Summary5a_Field, paste(wd.out,"/Summary5a_Field", Date, sep = ""))
   write.csv(Summary5b_Field, paste(wd.out,"/Summary5b_Field", Date, sep = ""))
   write.csv(Summary5c_Field, paste(wd.out,"/Summary5c_Field", Date, sep = ""))
   write.csv(Summary5d_Field, paste(wd.out,"/Summary5d_Field", Date, sep = ""))
   
   write.csv(Summary1e_File, paste(wd.out,"/Summary1e_File", Date, sep = ""))
   write.csv(Summary2e_IRfiles, paste(wd.out,"/Summary2e_IRfiles", Date, sep = ""))
   write.csv(Summary3e_Filter, paste(wd.out,"/Summary3e_Filter", Date, sep = ""))
   write.csv(Summary4e_Soil, paste(wd.out,"/Summary4e_Soil", Date, sep = ""))
   write.csv(Summary5e_Field, paste(wd.out,"/Summary5e_Field", Date, sep = ""))
   
   write.csv(Summary6a_CSS, paste(wd.out,"/Summary6a_CSS", Date, sep = ""))
   write.csv(Summary6b_CSS, paste(wd.out,"/Summary6b_CSS", Date, sep = ""))
   write.csv(Summary6c_CSS, paste(wd.out,"/Summary6c_CSS", Date, sep = ""))
   write.csv(Summary6d_CSS, paste(wd.out,"/Summary6d_CSS", Date, sep = ""))
   write.csv(Summary6e_CSS, paste(wd.out,"/Summary6e_CSS", Date, sep = ""))
 
   
   write.csv(Summary7a_MINAGRIS, paste(wd.out,"/Summary7a_MINAGRIS", Date, sep = ""))
   write.csv(Summary7b_MINAGRIS, paste(wd.out,"/Summary7b_MINAGRIS", Date, sep = ""))
   write.csv(Summary7c_MINAGRIS, paste(wd.out,"/Summary7c_MINAGRIS", Date, sep = ""))
   write.csv(Summary7d_MINAGRIS, paste(wd.out,"/Summary7d_MINAGRIS", Date, sep = ""))
   write.csv(Summary7e_MINAGRIS, paste(wd.out,"/Summary7e_MINAGRIS", Date, sep = ""))
  

   
   
    # Check CSS6F6F1 ####
   CSS6F6F1 =subset(Data_comb_red_blank, CSS==6 & Farm ==6 & Field == 1 )
   # write.csv( CSS6F6F1, paste(wd.out,"/CSS6F6F1", Date, sep = ""))
   Summary4a_CSS6F6F1= CSS6F6F1 %>% 
     group_by(Preparation_Type,  Lab,
              CSS, Farm, Field, 
              Polymer.grp,Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_name, get the summary
     summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                Num.px=sum(N.px),           # Number of pixels
                Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                Tot.Mass.ng=sum(Mass.ng),
                Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                SD.Area=sd(Area.um2.cor)) %>%

     # for each file I want all the polymers represented
     ungroup() %>%
     complete(nesting(Preparation_Type, 
                      CSS, Farm, Field),
              nesting(Polymer.grp,Polymer.red12, Polymer.red3), 
              fill=list(N.particles=0,
                        Num.px=0,
                        Tot.Area.mm2=0,
                        Median.Area.sqrt.um=0,
                        SD.Area=0)) %>%
     # Remove the "No.plastic", not needed anymore
     subset(Polymer.grp!="No.plastic")
   
   sum(Summary4a_CSS6F6F1$N.particles)
   # write.csv(  Summary4a_CSS6F6F1, paste(wd.out,"/Summary4a_CSS6F6F1", Date, sep = ""))
   

   CSS6F6F1_Field=subset(Summary5a_Field, CSS==11 & Farm==10 & Field==2) #, select="Mean.particles"
    sum(CSS6F6F1_Field$Mean.particles)
   CSS6F6F1_Soil=subset(Summary4a_Soil, CSS==11 & Farm==10 & Field==2)
    sum( CSS6F6F1_Soil$Mean.particles)
   CSS6F6F1_Filter=subset(Summary3a_Filter, CSS==11 & Farm==10 & Field==2)
    sum( CSS6F6F1_Filter$Mean.particles)
   
    CSS6F6F1_Field=subset(Summary5d_Field, CSS==11 & Farm==10 & Field==2) #, select="Mean.particles"
    sum(CSS6F6F1_Field$Mean.particles)
    CSS6F6F1_Soil=subset(Summary4d_Soil, CSS==11 & Farm==10 & Field==2)
    sum( CSS6F6F1_Soil$Mean.particles)
    CSS6F6F1_Filter=subset(Summary3d_Filter, CSS==11 & Farm==10 & Field==2)
    sum( CSS6F6F1_Filter$Mean.particles)
    
    CSS6F6F1_Field=subset(Summary5c_Field, CSS==11 & Farm==10 & Field==2) #, select="Mean.particles"
    sum(CSS6F6F1_Field$Mean.particles)
    CSS6F6F1_Soil=subset(Summary4c_Soil, CSS==11 & Farm==10 & Field==2)
    sum( CSS6F6F1_Soil$Mean.particles)
    CSS6F6F1_Filter=subset(Summary3c_Filter, CSS==11 & Farm==10 & Field==2)
    sum( CSS6F6F1_Filter$Mean.particles)
   
   
    
    # Check CSS11F10F2 ####
    CSS11F10F2 =subset(Data_comb_red_blank, CSS==11 & Farm ==10 & Field == 2 )
    
    Summary4a_CSS11F10F2= CSS11F10F2 %>% 
      group_by(Preparation_Type,  Lab,
               CSS, Farm, Field, 
               Polymer.grp,Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_name, get the summary
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%
      
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(Preparation_Type, 
                       CSS, Farm, Field),
               nesting(Polymer.grp,Polymer.red12, Polymer.red3), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.grp!="No.plastic")
    
    sum(Summary4a_CSS11F10F2$N.particles)
    # write.csv(  Summary4a_CSS11F10F2, paste(wd.out,"/Summary4a_CSS11F10F2", Date, sep = ""))
    
    
    CSS11F10F2_Field=subset(Summary5a_Field, CSS==11 & Farm==10 & Field==2) #, select="Mean.particles"
    sum(CSS11F10F2_Field$Mean.particles)
    CSS11F10F2_Soil=subset(Summary4a_Soil, CSS==11 & Farm==10 & Field==2)
    sum( CSS11F10F2_Soil$Mean.particles)
    CSS11F10F2_Filter=subset(Summary3a_Filter, CSS==11 & Farm==10 & Field==2)
    sum( CSS11F10F2_Filter$Mean.particles)
    
    CSS11F10F2_Field=subset(Summary5d_Field, CSS==11 & Farm==10 & Field==2) #, select="Mean.particles"
    sum(CSS11F10F2_Field$Mean.particles)
    CSS11F10F2_Soil=subset(Summary4d_Soil, CSS==11 & Farm==10 & Field==2)
    sum( CSS11F10F2_Soil$Mean.particles)
    CSS11F10F2_Filter=subset(Summary3d_Filter, CSS==11 & Farm==10 & Field==2)
    sum( CSS11F10F2_Filter$Mean.particles)
    
    CSS11F10F2_Field=subset(Summary5c_Field, CSS==11 & Farm==10 & Field==2) #, select="Mean.particles"
    sum(CSS11F10F2_Field$Mean.particles)
    CSS11F10F2_Soil=subset(Summary4c_Soil, CSS==11 & Farm==10 & Field==2)
    sum( CSS11F10F2_Soil$Mean.particles)
    CSS11F10F2_Filter=subset(Summary3c_Filter, CSS==11 & Farm==10 & Field==2)
    sum( CSS11F10F2_Filter$Mean.particles)
    
    
    
    
    ####################### Work in progress ####################
    # * Custom summary per CSS, polymer and size category 
    
    # Into a CSS loop to be more efficient? 
    
    css=1
    for (css in 1:11){   
      Summary_CSS_Polymer.red12_SizeCat = subset(Data_comb_red_blank, Preparation_Type=="Field_samples" & CSS==css) %>% 
        # 1. All the particles have to be summed up per sample, per sizecat and per polymer category  : Group by "File_Names" (I just keep other columns for convenience)
        group_by(  File_Names, Soil_sample, Sample_type, Preparation_Type, CSS, Farm, Field, Lab, Polymer.grp, Polymer.red12, Polymer.red3, Size_cat.um  ) %>% # For each PMF_File_name, get the summary
        summarise( N.particles= sum(N.px!=0),      # Count the lines that have at least 1px (leave out the empty lines( empty samples))
                   Tot.Area.um2=sum(Area.um2.cor), # Area are summed up per sample and per polymer category 
                   Tot.Mass.ng=sum( Mass.ng) ) %>% # IN the future we will include an estimation of the mass (or not but at least it is ready, let me dream)
        # 2. Add a 0 in the "empty categories" File_Names * CSS * Polymer.red12 * Size_cat.um
        ungroup() %>% # Ungroup before using complete to avoid issues with full_join
        complete(File_Names,CSS,Polymer.red12,Size_cat.um,
                 fill=list(N.particles=0, 
                           Tot.Area.um2=0,
                           Tot.Mass.ng=0)) %>%
        # 3. The particle results are averaged per CSS, SizeCat and per polymer, over the labs, operators, replicates (I just keep other columns for convenience)
        group_by(  CSS,Polymer.red12,Size_cat.um ) %>%  # Group per polymer cluster
        summarise( 
          Mean.particles= mean(N.particles),    # Mean particle number in this CSS, this sizecat, this polymer category
          Mean.Tot.Area.um2=mean(Tot.Area.um2!=0), # Mean area in this CSS, this sizecat, this polymer category
          Mean.Tot.Mass.ng=mean(Tot.Mass.ng!=0),   # Mean mass in this CSS, this sizecat, this polymer category (one day...)
          sd.particles=sd(N.particles),         # Standard deviation of particle number in this CSS, this sizecat, this polymer category
          sd.Area=sd(Tot.Area.um2!=0)   )     %>%      # Standard deviation of area in this CSS, this sizecat, this polymer category
        # 4. Make sure that SizeCat is in the right order
        mutate(
          Polymer.red12 = factor(Polymer.red12, levels = c("Other.Plastic", "PE", "PP", "PA","PS", "PVC", "PET",  "PLA", "PU", "PMMA", "PC" )), # specify the desired order
          Size_cat.um = factor(Size_cat.um, levels = c("50-300","300-550","550-800", "800-1050","1050-1300","1300-1550","1550-1800")) # specify the desired order
        )
      # write.csv(  Summary_Fields_Polymer12, paste(wd.out,"Summary_Fields_Polymer12.csv",sep = "/"))
      # write.csv(  Summary_Fields_AllPolymer, paste(wd.out,"Summary_Fields_AllPolymer.csv",sep = "/"))
      
      PLOT= ggplot( subset(Summary_CSS_Polymer.red12_SizeCat, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles, fill=Polymer.red12)) +
        geom_bar(position="stack", stat="identity")+ 
        scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                     "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                                     "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                     "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                     "CA"= "#FFD39B"  ) ) +
        ggtitle(paste("Field Samples ; CSS ", css))+
        theme_minimal()
      
      print(PLOT)
    }

    
    
        

