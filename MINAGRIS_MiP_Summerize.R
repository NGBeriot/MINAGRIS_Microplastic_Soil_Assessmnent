# Script to Make summary table of any kind ; create size categories 

# Expected Data Structure: 

# Column name       #Description         	                                                        #Data format
  # File_Names       	Name of the Processed, manually checked IR file	                              TEXT.csv
  # Lab	              Analysis lab	                                                                {"WUR", "Ubern"}
  # Batch 	          Analysis batch	                                                              #Number
  # Preparation_Type	The kind of preparation for the samples: 	                                    {"Blank_chemical", "Spiked", "Reference_Soil", "Standard_Soil", "Field_samples"}
  # Sample_type	      Type of samples taken into account during the analysis                      	{n, s, r, st}
  # Soil_sample	      Soil code (CSS, Farm, Field) used for the preparation	                        {bcm, rs, st, c(#CSS, #Farm, #Field)}
  # Filter_Name 	    Name of the extracted filter	                                                Batch_"Soil_sample"_"Sample_type"
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
    

library(tidyverse)
library(hrbrthemes)
library(stats)


rm(list=ls()) # cleaning console
graphics.off() # cleaning plots
'%!in%' <- function(x,y)!('%in%'(x,y))

# Set WD in the project only needed when RStudio projects are not used.

# Outputs folder
wd.out= "Outputs"

# Initialization 
#MC - see comment in previous script about dates in output file names.
Data_comb_red_blank=read.csv("Outputs/Corrected_MiP_Particles.csv")
    
#MC - this section is unclear - add documentation
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
    # "Filter_Name" 
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
    # CSS, Farm, Field, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator 
    # ID, Q_index, Polymer.grp, Polymer.red12, Polymer.red3, N.px, Area.um2.cor, Length.um, Width.um, Aspect_ratio, Mass.ng, Size_cat.um 

#MC - until here, the documentation below is clear!
    
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
    
# /!\ WORK in Progress: 
    # Replace all the following sections with a unique function
    # that makes summaries with inputs as : 
    # - level  (File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,  CSS, Farm, Field, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator)
    # - factors (Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um)
    # - option include/ exclude "Other.Plastic"
    
# 0. Summarize available data ####
    
    Data_comb_red_blank$CSS_Farm_Field=paste(Data_comb_red_blank$CSS,Data_comb_red_blank$Farm, Data_comb_red_blank$Field, sep="_")
    Data_comb_red_blank$CSS_Farm_Field[ Data_comb_red_blank$Preparation_Type!="Field_samples"]="Other"
    # * List of soil samples ####
    unique(Data_comb_red_blank$CSS_Farm_Field)
    
    # Print the unique Soil codes for each case study site (CSS) 
    for (css in 1:11) {
      print( paste("CSS",css, ";", length(unique(Data_comb_red_blank$Farm[Data_comb_red_blank$CSS==paste(css)])),"Unique Farms", ";", length(unique(Data_comb_red_blank$Soil_sample[Data_comb_red_blank$CSS==paste(css)])), "Unique soils"))
      print(sort(unique(Data_comb_red_blank$Soil_sample[Data_comb_red_blank$CSS==paste(css)])))
    }  
    
    # * List of Fields ####
    unique(Data_comb_red_blank$CSS_Farm_Field)
    
    # Print the unique Soil codes for each case study site (CSS) 
    for (css in 1:11) {
      print( paste("CSS",css, ";", length(unique(Data_comb_red_blank$Farm[Data_comb_red_blank$CSS==paste(css)])),"Unique Farms", ";", length(unique(Data_comb_red_blank$Soil_sample[Data_comb_red_blank$CSS==paste(css)])), "Unique soils"))
      print(sort(unique(Data_comb_red_blank$CSS_Farm_Field[Data_comb_red_blank$CSS==paste(css)])))
    }  
    
    

    
    
# 1. Summaries, processed (PMF) File ####   
    # *1.a Sum up per processed (PMF) File, all factors  ####
    Summary1a_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator,
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
                       CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator),
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
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator,
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
                       CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator),
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
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator,
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
                       CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator),
               nesting(Polymer.red12, Polymer.red3), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.red12!="No.plastic") #
    
    
    # *1.d Sum up per processed (PMF) sum up all polymers, excluding "Other.Plastic"  ####
    Summary1d_File = Data_comb_red_blank %>% 
      # "Other.Plastic" to 0
      mutate(N.px = if_else(Polymer.red12 == "Other.Plastic", 0, N.px),
             Area.um2.cor= if_else(Polymer.red12 == "Other.Plastic", 0,  Area.um2.cor),
             Mass.ng = if_else(Polymer.red12 == "Other.Plastic", 0,  Mass.ng) ) %>%
      
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator) %>%
      
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  #
    
    # *1.e Sum up per processed (PMF) sum up all polymers, including "Other.Plastic"  ####
    Summary1e_File = Data_comb_red_blank %>% 
      
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator) %>%
      
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  #
    
    # *1.f Sum up per processed (PMF) File, Size_cat.um  ####
    Summary1f_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator,
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
                       CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator),
               nesting(Size_cat.um), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0))

    
    # *1.h Sum up per processed (PMF) File, Polymer.red3 * per Size_cat.um  ####
    Summary1h_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator,
              Polymer.red3, Size_cat.um ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%  #
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,
                       CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator),
               nesting(Polymer.red3, Size_cat.um), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0))    %>%    # Remove the "No.plastic", not needed anymore
    subset(Polymer.red3!="No.plastic") #
    
    
    
    # *1.i Sum up per processed (PMF) File, Polymer.red3 ####
    Summary1i_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator,
               Polymer.red3) %>%
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
                       CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator),
               nesting(Polymer.red3), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0))
    
    
    # *1.g Sum up per processed (PMF) File, all Polymer ####
    Summary1g_File = Data_comb_red_blank %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator,
               Polymer.grp, Polymer.red12,  Polymer.red3) %>%
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
                       CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator),
               nesting(Polymer.grp,Polymer.red12, Polymer.red3), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.grp!="No.plastic")
    
    
    
    
    
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
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name,
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
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name,
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
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name,
               Polymer.red12,  Polymer.red3) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    
    # *2d Mean over IR results files, sum up all polymers, excluding "Other.Plastic" ####
    Summary2d_IRfiles = Summary1d_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name) %>%
      
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    # *2e Mean over IR results files, sum up all polymers, including "Other.Plastic" ####
    Summary2e_IRfiles = Summary1e_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name) %>%
      
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
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name,
               Size_cat.um) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) 
    
    # *2g Mean over IR results files, all polymer ####
    Summary2g_IRfiles = Summary1g_File %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name, Filter_div, Filter_Name,
               Polymer.grp, Polymer.red12,  Polymer.red3) %>%
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
               CSS, Farm, Field, Extraction_Name,
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    
    # * 3b Sum per filter.div, Polymer.red12 * per Size_cat.um####
    Summary3b_Filter = Summary2b_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name,
               Polymer.red12,  Polymer.red3, Size_cat.um) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # * 3c Sum per filter.div, Polymer.red12 ####
    Summary3c_Filter = Summary2c_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field, Extraction_Name,
               Polymer.red12,  Polymer.red3) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    

    

    
    # * 3d Sum per filter.div, sum up all polymers, excluding "Other.Plastic"  ####
    Summary3d_Filter = Summary2d_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,Extraction_Name) %>%
      
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # * 3e Sum per filter.div, sum up all polymers, including "Other.Plastic"  ####
    Summary3e_Filter = Summary2e_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,Extraction_Name) %>%
      
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # * 3f Sum per filter.div, size cat ####
    Summary3f_Filter = Summary2f_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,Extraction_Name,
                Size_cat.um) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) 
    
    # * 3g Sum per filter.div, all polymer ####
    Summary3g_Filter = Summary2g_IRfiles %>% 
      group_by(Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,          
               CSS, Farm, Field,Extraction_Name,
               Polymer.grp, Polymer.red12,  Polymer.red3) %>%
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
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.particles= mean( Mean.particles),
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 

    
    # * 4b Mean per Soil_samples, Polymer.red12 * per Size_cat.um ####
    
    Summary4b_Soil= subset( Summary3b_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type, Soil_sample,
               CSS, Farm, Field, 
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
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
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 

    
    # * 4d sum up all polymers, excluding "Other.Plastic"  ####
    
    Summary4d_Soil= subset( Summary3d_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type,  Soil_sample,
               CSS, Farm, Field ) %>% # For each PMF_File_Name, get the summary
                 
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 4e sum up all polymers, including "Other.Plastic" ####
    
    Summary4e_Soil= subset( Summary3e_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type,  Soil_sample,
               CSS, Farm, Field ) %>% # For each PMF_File_Name, get the summary
      
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 4f sum up all polymers, size cat 
    
    Summary4f_Soil= subset( Summary3f_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type, Soil_sample,
               CSS, Farm, Field, 
               Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    
    # * 4g Mean per Soil_samples, Polymer.grp ####
    
    Summary4g_Soil= subset( Summary3g_Filter, Preparation_Type=="Field_samples") %>% 
      group_by(Lab, Preparation_Type,  Soil_sample,
               CSS, Farm, Field, 
               Polymer.grp,  Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
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
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
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
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
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
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
      summarise( N.files = n(),
                 Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.F= min( Mean.particles),
                 Max.particles.F= max( Mean.particles),
                 Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) ) #  Mean mass per sample and polymer, over the files/operators
       
    
    
    # * 5d sum up all polymers, excluding "Other.Plastic" ####
    
    Summary5d_Field= Summary4d_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm, Field ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.F= min( Mean.particles),
                Max.particles.F= max( Mean.particles),
                Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) ) #  Mean mass per sample and polymer, over the files/operators
      
    
    # * 5e sum up all polymers,  including "Other.Plastic"  ####
    
    Summary5e_Field= Summary4e_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm, Field ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.F= min( Mean.particles),
                Max.particles.F= max( Mean.particles),
                Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                Min_Max_diff=Max.particles.F-Min.particles.F,
                Formula=Min_Max_diff-Min.particles.F/2
      ) 
   
    Summary5e_Field$CSS_Farm_Field= paste(Summary5e_Field$CSS,Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
    
    # Mean per CSS of the soil samples. 
    
# 6. Summaries, CSS ####      
    #  * 6a Mean per CSS, all factors #### 
    Summary6a_CSS= Summary4a_Soil %>% 
      group_by(Preparation_Type, 
               CSS, 
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
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
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
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
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )
    
    

    Summary6c_CSS_outlier = subset(Summary4c_Soil, CSS!=11 | Farm !=10 ) %>% 
      group_by(Preparation_Type,  
               CSS,  
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )
    
    
    
    # * 6d sum up all polymers, excluding "Other.Plastic"  ####
    
    Summary6d_CSS= Summary4d_Soil %>% 
      group_by(Preparation_Type,  
               CSS, ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )

    # * 6e sum up all polymers, including "Other.Plastic"  ####

    Summary6e_CSS= Summary4e_Soil %>% 
      group_by(Preparation_Type,  
               CSS) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) ) 
    
    Summary6e_CSS_outlier = subset(Summary4e_Soil, CSS!=11 | Farm !=10 ) %>% 
      group_by(Preparation_Type,  
               CSS ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) ) 
    
    
    
    # * 6f sum up all polymers, size cat  ####
    
    Summary6f_CSS= Summary4f_Soil %>% 
      group_by(Preparation_Type, 
               CSS, 
               Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) )
    
    # * 6g sum up all polymers, Polymer.grp ####
    
    Summary6g_CSS= Summary4g_Soil %>% 
      group_by(Preparation_Type,  
               CSS,
               Polymer.grp,Polymer.red12,Polymer.red3) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) ) 
    
    
    
    Summary6g_CSS_outlier = subset(Summary4g_Soil, CSS!=11 | Farm !=10 ) %>% 
      group_by(Preparation_Type,  
               CSS,
               Polymer.grp,Polymer.red12,Polymer.red3) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) ) 
    
    ####################### Work in progress ####################
    # * Custom summary per CSS, polymer and size category 
    

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
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
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
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
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
    
    
    # * 7d all MINAGRIS, excluding "Other.Plastic"  ####
    
    Summary7d_MINAGRIS= Summary4d_Soil %>% 
      group_by(Preparation_Type ) %>% # For each PMF_File_Name, get the summary
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
    
    # * 7e all MINAGRIS, including "Other.Plastic" ####
    
    Summary7e_MINAGRIS= Summary4e_Soil %>% 
      group_by(Preparation_Type ) %>% # For each PMF_File_Name, get the summary
      
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
    

    
    
# 8. Summary per Farm     ####
    
    # * 8a Mean per Farm, all factors ####
    Summary8a_Farm= Summary4a_Soil%>% 
      group_by(Preparation_Type, 
               CSS, Farm,
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
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
    
    
    # * 8b Mean per Farm, Polymer.red12 * per Size_cat.um ####
    
    Summary8b_Farm= Summary4b_Soil%>% 
      group_by(Preparation_Type, 
               CSS, Farm,
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 8c Mean per Farm, Polymer.red12 ####
    
    Summary8c_Farm= Summary4c_Soil%>% 
      group_by(Preparation_Type,  
               CSS, Farm, 
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
      summarise( N.files = n(),
                 Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.F= min( Mean.particles),
                 Max.particles.F= max( Mean.particles),
                 Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) ) #  Mean mass per sample and polymer, over the files/operators
    
    
    
    # * 8d  Mean per Farm, sum up all polymers, excluding "Other.Plastic"   ####
    
    Summary8d_Farm= Summary4d_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.F= min( Mean.particles),
                Max.particles.F= max( Mean.particles),
                Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng) ) #  Mean mass per sample and polymer, over the files/operators
    
    
    # * 8e Mean per Farm, sum up all polymers, including "Other.Plastic" ####
    
    Summary8e_Farm= Summary4e_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.F= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.F= min( Mean.particles),
                Max.particles.F= max( Mean.particles),
                Mean.px.F=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.F=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.F=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.F=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.F=mean( Mean.Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                Min_Max_diff=Max.particles.F-Min.particles.F,
                Formula=Min_Max_diff-Min.particles.F/2
      ) 
    
    # * 8g  Mean per Farm,  Per polymer.grp  ####
    Summary8g_Farm= Summary4g_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm,
               Polymer.grp,Polymer.red12,Polymer.red3) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) ) 
    
    
    
    Summary8g_Farm_outlier = subset(Summary4g_Soil, CSS!=11 | Farm !=10 ) %>% 
      group_by(Preparation_Type,  
               CSS, Farm,
               Polymer.grp,Polymer.red12,Polymer.red3) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles),
                Min.particles.CSS= min( Mean.particles),
                Max.particles.CSS= max( Mean.particles),
                Mean.px.CSS=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng) ) 
    
    
    
    ####################### Work in progress ####################
    # Export the summary per polymer.grp per soil Custom summary per CSS, polymer and size category 
    
    # 
    # css=1
    # for (css in 1:11){   
    #   
    #   
    #   write.csv(Summary1d_File, paste(wd.out,"/Summary_c_CSS",css, Date, sep = ""))
    # }
    # 
    # 

    
    
    
# Export ####
  
  Date="2028.02.07.csv"    
   write.csv(Summary1a_File, paste(wd.out,"/Summary1a_File", Date, sep = ""))
   write.csv(Summary1b_File, paste(wd.out,"/Summary1b_File", Date, sep = ""))
   write.csv(Summary1c_File, paste(wd.out,"/Summary1c_File", Date, sep = ""))
   write.csv(Summary1d_File, paste(wd.out,"/Summary1d_File", Date, sep = ""))
   write.csv(Summary1g_File, paste(wd.out,"/Summary1g_File", Date, sep = ""))
    
   write.csv(Summary2a_IRfiles, paste(wd.out,"/Summary2a_IRfiles", Date, sep = ""))
   write.csv(Summary2b_IRfiles, paste(wd.out,"/Summary2b_IRfiles", Date, sep = ""))
   write.csv(Summary2c_IRfiles, paste(wd.out,"/Summary2c_IRfiles", Date, sep = ""))
   write.csv(Summary2d_IRfiles, paste(wd.out,"/Summary2d_IRfiles", Date, sep = ""))
   write.csv(Summary2g_IRfiles, paste(wd.out,"/Summary2g_IRfiles", Date, sep = ""))
   
   write.csv(Summary3a_Filter, paste(wd.out,"/Summary3a_Filter", Date, sep = ""))
   write.csv(Summary3b_Filter, paste(wd.out,"/Summary3b_Filter", Date, sep = ""))
   write.csv(Summary3c_Filter, paste(wd.out,"/Summary3c_Filter", Date, sep = ""))
   write.csv(Summary3d_Filter, paste(wd.out,"/Summary3d_Filter", Date, sep = ""))
   write.csv(Summary3f_Filter, paste(wd.out,"/Summary3f_Filter", Date, sep = ""))
   write.csv(Summary3g_Filter, paste(wd.out,"/Summary3g_Filter", Date, sep = ""))
   
   write.csv(Summary4a_Soil, paste(wd.out,"/Summary4a_Soil", Date, sep = ""))
   write.csv(Summary4b_Soil, paste(wd.out,"/Summary4b_Soil", Date, sep = ""))
   write.csv(Summary4c_Soil, paste(wd.out,"/Summary4c_Soil", Date, sep = ""))
   write.csv(Summary4d_Soil, paste(wd.out,"/Summary4d_Soil", Date, sep = ""))
   write.csv(Summary4g_Soil, paste(wd.out,"/Summary4g_Soil", Date, sep = ""))
   
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
   write.csv(Summary6g_CSS, paste(wd.out,"/Summary6g_CSS", Date, sep = ""))
   
   write.csv(Summary7a_MINAGRIS, paste(wd.out,"/Summary7a_MINAGRIS", Date, sep = ""))
   write.csv(Summary7b_MINAGRIS, paste(wd.out,"/Summary7b_MINAGRIS", Date, sep = ""))
   write.csv(Summary7c_MINAGRIS, paste(wd.out,"/Summary7c_MINAGRIS", Date, sep = ""))
   write.csv(Summary7d_MINAGRIS, paste(wd.out,"/Summary7d_MINAGRIS", Date, sep = ""))
   write.csv(Summary7e_MINAGRIS, paste(wd.out,"/Summary7e_MINAGRIS", Date, sep = ""))
  
   write.csv(Summary8e_Farm, paste(wd.out,"/Summary8e_Farm", Date, sep = ""))
   write.csv(Summary8g_Farm, paste(wd.out,"/Summary8g_Farm", Date, sep = ""))
   write.csv(Summary8g_Farm_outlier, paste(wd.out,"/Summary8g_Farm_outlier", Date, sep = ""))
   
   write.csv(Summary6c_CSS_outlier, paste(wd.out,"/Summary6c_CSS_outlier", Date, sep = ""))
   write.csv(Summary6e_CSS_outlier, paste(wd.out,"/Summary6e_CSS_outlier", Date, sep = ""))
   write.csv(Summary6g_CSS_outlier, paste(wd.out,"/Summary6g_CSS_outlier", Date, sep = ""))
   
   
   #########################################################################################
   
   
   # Check samples with a big difference betwwen S1 and S2#### 
   
   # Arbitrary formula : Max > 1.5*Min +10 
   Check_S5e=subset(Summary5e_Field, Min_Max_diff-Min.particles.F/2>10)
   Check_S5e=subset(Summary5e_Field, Formula>10)
   
   Check_S5e= Check_S5e[order( Check_S5e$Formula), levels=  ]
   df$Category <- factor(df$Category, levels = df$Category[order(df$Value)])
   Check_S5e$CSS_Farm_Field= factor( Check_S5e$CSS_Farm_Field, levels =  Check_S5e$CSS_Farm_Field[order( -Check_S5e$Formula)])
   
   Summary5e_Field$CSS_Farm_Field= paste(Summary5e_Field$CSS,Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
   
   Summary4e_Soil$CSS_Farm_Field= paste(Summary4e_Soil$CSS,Summary4e_Soil$Farm, Summary4e_Soil$Field, sep=".")
   
   Check_S4e=subset(Summary4e_Soil, CSS_Farm_Field %in% Check_S5e$CSS_Farm_Field)
   
   
   
   # Plot biggest differneces 
   # P1: Plot vertical bar min-max, from Summary5e_Field
   df_p1= Check_S5e
   
   p1= ggplot()+
     geom_linerange(data=df_p1, aes(x=CSS_Farm_Field, y=Mean.particles.F, ymin = Min.particles.F, ymax = Max.particles.F))+
     ggtitle("Most S1-S2 difference")+
     theme_minimal()
   
   #  P2: Plot Samples S1-S2, min-max with lab color, from Summary4e_Soil
   df_p2=Check_S4e

   
   PLOT=  
     p1+ 
     geom_point(data = df_p2, aes(x=CSS_Farm_Field, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
     scale_size_manual(values = c("WUR"=2,  "Ubern"=3))+
     scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
     ggtitle("Most S1-S2 difference")+
     labs(y = "Average number of plastic particles per soil sample") +
     theme_minimal()+
     theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
           axis.title.x = element_blank())
   
   print( PLOT)
   
   write.csv(Summary3d_Filter, paste(wd.out,"/Summary3d_Filter", Date, sep = ""))
   
   # Check CSS4F7F1 ####
   CSS4F7F1=subset(Data_comb, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_red =subset(Data_comb_red, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_red_blank =subset(Data_comb_red_blank, CSS==4 & Farm == 7 & Field == 1 )
   
   CSS4F7F1_Summary4e= subset(Summary4e_Soil, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_Summary3e= subset(Summary3e_Filter, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_WUR =subset(MiP_wur, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_WUR2 =subset(MiP_wur_cor, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_WUR3 =subset(Data_WUR, CSS==4 & Farm == 7 & Field == 1 )
   
   # Check CSS4F2F1 ####
   CSS4F2F1=subset(Data_comb, CSS==4 & Farm == 2 & Field == 1 )
   CSS4F2F1_red =subset(Data_comb_red, CSS==4 & Farm == 2 & Field == 1 )
   CSS4F2F1_red_blank =subset(Data_comb_red_blank, CSS==4 & Farm == 2 & Field == 1 )
   CSS4F2F1_WUR3 =subset(Data_WUR, CSS==4 & Farm == 2 & Field == 1 )
  
   
   
   
   # Check CSS11F8F1 ####
   CSS11F8F1 =subset(Data_comb_red_blank, CSS==11 & Farm == 8 & Field == 1 )
   CSS11F8F1_wur =subset(Data_WUR, CSS==11 & Farm == 8 & Field == 1 )
   
   
   
   # Check CSS4F6F1 ####
   CSS4F6F1 =subset(Data_comb_red_blank, CSS==4 & Farm == 6& Field == 1 )
   
   # Check CSS6F1F1 ####
   CSS6F1F1 =subset(Data_comb_red_blank, CSS==6 & Farm ==1 & Field == 1 )
   # write.csv( CSS6F6F1, paste(wd.out,"/CSS6F6F1", Date, sep = ""))
   Summary4a_CSS6F1F1= CSS6F1F1 %>% 
     group_by(Preparation_Type,  Lab,
              CSS, Farm, Field, 
              Polymer.grp,Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
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
   
   sum(Summary4a_CSS6F1F1$N.particles)
   # write.csv(  Summary4a_CSS6F6F1, paste(wd.out,"/Summary4a_CSS6F6F1", Date, sep = ""))
   
   
   
   # Check CSS6F2F1 ####
   CSS6F2F1 =subset(Data_comb_red_blank, CSS==6 & Farm ==2 & Field == 1 )
   
   # Check CSS6F3F1 ####
   CSS6F3F1 =subset(Data_comb_red_blank, CSS==6 & Farm ==3 & Field == 1 )
   
   # Check CSS6F8F1 ####
   CSS6F8F1 =subset(Data_comb_red_blank, CSS==6 & Farm ==3 & Field == 1 )
   
   
    # Check CSS6F6F1 ####
   CSS6F6F1 =subset(Data_comb_red_blank, CSS==6 & Farm ==6 & Field == 1 )
   # write.csv( CSS6F6F1, paste(wd.out,"/CSS6F6F1", Date, sep = ""))
   Summary4a_CSS6F6F1= CSS6F6F1 %>% 
     group_by(Preparation_Type,  Lab,
              CSS, Farm, Field, 
              Polymer.grp,Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
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
   
   
    
  
    
    
    
    # Check CSS6F3F2 ####
    CSS6F3F2 =subset(Data_comb_red_blank, CSS==6 & Farm ==3 & Field == 2 )
    # write.csv( CSS6F6F1, paste(wd.out,"/CSS6F6F1", Date, sep = ""))
    Summary4a_CSS6F3F2= CSS6F3F2 %>% 
      group_by(Preparation_Type,  Lab,
               CSS, Farm, Field, 
               Polymer.grp,Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
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
    
    sum(Summary4a_CSS6F1F1$N.particles)
    # write.csv(  Summary4a_CSS6F6F1, paste(wd.out,"/Summary4a_CSS6F6F1", Date, sep = ""))
    
    
    # Check CSS6F6F2 ####
    CSS6F6F2 =subset(Data_comb_red_blank, CSS==6 & Farm ==6 & Field == 2 )
    
    # Check CSS11F10F2 ####
    CSS11F10F2 =subset(Data_comb_red_blank, CSS==11 & Farm ==10 & Field == 2 )
    
    Summary4a_CSS11F10F2= CSS11F10F2 %>% 
      group_by(Preparation_Type,  Lab,
               CSS, Farm, Field, 
               Polymer.grp,Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
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
    
    
    
    
   
    
    
        

