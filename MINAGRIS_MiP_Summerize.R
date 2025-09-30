# Script to Make summary table of any kind ; create size categories 

# Expected Data Structure: 

# Column name       #Description         	                                                        #Data format
  # File_Name       	Name of the Processed, manually checked IR file	                              TEXT.csv
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
wd.out= "Outputs/2025_08"

# 0. Initialization ####
  # Data frame of measured particles
df_MiP=read.csv(paste(wd.out, "/Corrected_MiP_Particles.csv",sep=""))

  # Summary Blank filters per Polymer, per lab
S_pPol_pLab_mFilter= read.csv( paste(wd.out,"Summary_Blanks_pPolymer_pLab_meanFilter.csv",sep = "/"))

  # Available Metadata
#Fields_METADATA=read.csv("Fields_METADATA.csv")
#Field_METADATA_vector=colnames(Fields_METADATA)
# Parameters in Field_METADATA to be included in the average
#Field_METADATA_vector=Field_METADATA_vector[Field_METADATA_vector %!in% c("X", "CSS","Farm","Field")]
Field_METADATA_vector=NA  
  
#MC - this section is unclear - add documentation
    colnames(df_MiP)
  
  # colnames(df_MiP) organized: 
      
    # "File_Name"
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
    # "Size_cat2.um" 
    
    # File_Name, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample          
    # CSS, Farm, Field, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator 
    # ID, Q_index, Polymer.grp, Polymer.red12, Polymer.red3, N.px, Area.um2.cor, Length.um, Width.um, Aspect_ratio, Mass.ng, Size_cat.um 

# For all Files:   
  # 1. Sum per Processed and manually checked results file / "File_Name" <-> "ID". "Q_index" 
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
    # - level  (File_Name, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,  CSS, Farm, Field, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator)
    # - factors (Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um)
    # - option include/ exclude "Other.Plastic"
    
# * Summarize available data ####
    
    # New column CSS_Farm_Field
    df_MiP$CSS_Farm_Field=paste(df_MiP$CSS,df_MiP$Farm, df_MiP$Field, sep="_")
    df_MiP$CSS_Farm_Field[ df_MiP$Preparation_Type!="Field_samples"]="Other"

# * List of soil samples ####
    unique(df_MiP$Soil_sample)
    
    # Print the unique Soil codes for each case study site (CSS) 
    for (css in 1:11) {
      print( paste("CSS",css, ";", length(unique(df_MiP$Farm[df_MiP$CSS==paste(css)])),"Unique Farms", ";", length(unique(df_MiP$Soil_sample[df_MiP$CSS==paste(css)])), "Unique soils"))
      print(sort(unique(df_MiP$Soil_sample[df_MiP$CSS==paste(css)])))
    }  
    
    # * List of Fields ####
    unique(df_MiP$CSS_Farm_Field)
    
    # Print the unique Soil codes for each case study site (CSS) 
    for (css in 1:11) {
      print( paste("CSS",css, ";", length(unique(df_MiP$Farm[df_MiP$CSS==paste(css)])),"Unique Farms", ";", length(unique(df_MiP$Soil_sample[df_MiP$CSS==paste(css)])), "Unique soils"))
      print(sort(unique(df_MiP$CSS_Farm_Field[df_MiP$CSS==paste(css)])))
    }  
    
    
    # Number of Files per lab: 
    length(unique(df_MiP$File_Name[df_MiP$Lab=="WUR"]))
    length(unique(df_MiP$File_Name[df_MiP$Lab=="Ubern"]))
    length(unique(df_MiP$File_Name))
    
    # Number of Files per Fields: 
    length(unique(df_MiP$CSS_Farm_Field[df_MiP$Lab=="WUR"]))
    length(unique(df_MiP$CSS_Farm_Field[df_MiP$Lab=="Ubern"]))
    length(unique(df_MiP$CSS_Farm_Field))
    
    # Number of Field sample Files per lab: 
    length(unique(df_MiP$File_Name[df_MiP$Lab=="WUR" & df_MiP$Preparation_Type=="Field_samples"]))
    length(unique(df_MiP$File_Name[df_MiP$Lab=="Ubern"& df_MiP$Preparation_Type=="Field_samples"]))
    length(unique(df_MiP$File_Name[ df_MiP$Preparation_Type=="Field_samples"]))
    
    # Number of identified particles 
    nrow(df_MiP[df_MiP$N.px>0,])
    
    
    # * Create Summary groups ####
    
    Group1_Files=c("File_Name", "Lab","Batch_Name", "Preparation_Type", "Sample_type", "Soil_sample", # Field_METADATA_vector,     
                  "CSS","Farm", "Field", "Extraction_Name", "Filter_div", "Filter_Name", "IR_rep", "PMF_rep", "Operator")
    
    Group2_IRfiles=c("Lab","Batch_Name", "Preparation_Type", "Sample_type", "Soil_sample", # Field_METADATA_vector,     
                    "CSS","Farm", "Field", "Extraction_Name", "Filter_div", "Filter_Name")
    
    Group3_Filters=c("Lab","Batch_Name", "Preparation_Type", "Sample_type", "Soil_sample", # Field_METADATA_vector,     
                    "CSS","Farm", "Field", "Extraction_Name")
    
    Group4_Soil=c("Lab", "Preparation_Type", "Soil_sample",  #Field_METADATA_vector,     
                 "CSS","Farm", "Field")
    
    Group5_Field= c("Preparation_Type",   "CSS","Farm", "Field") #Field_METADATA_vector,
    Group6_Farm= c("Preparation_Type", "CSS","Farm")
    Group7_CSS= c("Preparation_Type", "CSS")
    Group9_Crop= c("Preparation_Type", "CropDominant4")
    
    GroupP= c("Polymer.grp", "Polymer.red12", "Polymer.red3")
    GroupP12=c( "Polymer.red12",  "Polymer.red3")
    GroupP3=c("Polymer.red3")
    GroupS=c("Size_cat.um", "Size_cat2.um")
    
# 1. Summaries, processed (PMF) File ####   
    # Sum per Processed and manually checked results file
    Group1_Files
    
    # *1.a Sum up per processed (PMF) File, all factors  ####
    Summary1a_File = df_MiP %>% 
      group_by_at( c(Group1_Files, GroupP, GroupS) ) %>%
       # group_by_at(Field_METADATA_vector) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  %>%  #
    # Complete the Summary1_File: 
    # for each file I want all the polymers represented
    # So a new line is created for the missing combinations with a 0 value  
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
              nesting(!!!syms(GroupP) ), 
              nesting(!!!syms(GroupS) ),
             fill=list(N.particles=0,
                       Num.px=0,
                       Tot.Area.mm2=0,
                       Tot.Mass.ng=0,
                       Median.Area.sqrt.um=0,
                       SD.Area=0))%>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.red12!="No.plastic") 

    
    # *1.b Sum up per processed (PMF) File, Polymer.red12 * per Size_cat.um  ####
    Summary1b_File = df_MiP %>% 
      group_by_at( c(Group1_Files, GroupP12, GroupS) ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%  #
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
               nesting(!!!syms(GroupP12) ), 
               nesting(!!!syms(GroupS) ),
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Tot.Mass.ng=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.red12!="No.plastic") #
    
    
    # *1.b2 Sum up per processed (PMF) File, Polymer.red12 * per Size_cat2.um  ####
    Summary1b2_File = df_MiP %>% 
      group_by_at( c(Group1_Files, GroupP12, "Size_cat2.um") ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%  #
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
               nesting(!!!syms(GroupP12) ), 
               nesting(!!!syms("Size_cat2.um") ),
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Tot.Mass.ng=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.red12!="No.plastic") #
    
    
    
    # *1.c Sum up per processed (PMF) File, Polymer.red12 ####
    Summary1c_File = df_MiP %>% 
      group_by_at( c(Group1_Files, GroupP12) ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%  #
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
               nesting(!!!syms(GroupP12) ), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Tot.Mass.ng=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.red12!="No.plastic") #
    
    
    # *1.d Sum up per processed (PMF) sum up all polymers, excluding "Other.Plastic"  ####
    Summary1d_File = df_MiP %>% 
      # "Other.Plastic" to 0
      mutate(N.px = if_else(Polymer.red12 == "Other.Plastic", 0, N.px),
             Area.um2.cor= if_else(Polymer.red12 == "Other.Plastic", 0,  Area.um2.cor),
             Mass.ng = if_else(Polymer.red12 == "Other.Plastic", 0,  Mass.ng) ) %>%
      
      group_by_at( c(Group1_Files) ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%
      ungroup() #
    
    # *1.e Sum up per processed (PMF) sum up all polymers, including "Other.Plastic"  ####
    Summary1e_File = df_MiP %>% 
      
      group_by_at( c(Group1_Files) ) %>%
      
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))%>%
      ungroup()  #
    
    # *1.f Sum up per processed (PMF) File, Size_cat.um  ####
    Summary1f_File = df_MiP %>% 
      group_by_at( c(Group1_Files, GroupS) ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  %>%  #
      # Complete the Summary1_File: 
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
               nesting(!!!syms(GroupS) ),
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Tot.Mass.ng=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0))

    
    # *1.h Sum up per processed (PMF) File, Polymer.red3 * per Size_cat.um  ####
    Summary1h_File = df_MiP %>% 
      group_by_at( c(Group1_Files, "Polymer.red3", GroupS) ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor)) %>%  #
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
               nesting(Polymer.red3 ), 
               nesting(!!!syms(GroupS) ),
                
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Tot.Mass.ng=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0))    %>%    # Remove the "No.plastic", not needed anymore
    subset(Polymer.red3!="No.plastic") #
    
    
    
    # *1.i Sum up per processed (PMF) File, Polymer.red3 ####
    Summary1i_File = df_MiP %>% 

      group_by_at( c(Group1_Files, "Polymer.red3") ) %>%
      
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  %>%  #
      # Complete the Summary1_File: 
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
               nesting(Polymer.red3), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Tot.Mass.ng=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0))
    
    
    # *1.g Sum up per processed (PMF) File, all Polymer ####
    Summary1g_File = df_MiP %>% 
      group_by_at( c(Group1_Files, GroupP) ) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  %>%  #
      # Complete the Summary1_File: 
      # for each file I want all the polymers represented
      ungroup() %>%
      complete(nesting(!!!syms(Group1_Files) ),
               nesting(!!!syms(GroupP) ), 
               fill=list(N.particles=0,
                         Num.px=0,
                         Tot.Area.mm2=0,
                         Tot.Mass.ng=0,
                         Median.Area.sqrt.um=0,
                         SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.grp!="No.plastic")
    
    
    
    
    
    
    # Check Total particles numbers
    sum(Summary1e_File$N.particles)
    sum(Summary1d_File$N.particles) # Excluding other plastics
    sum(Summary1c_File$N.particles)
    sum(Summary1b_File$N.particles)
    sum(Summary1a_File$N.particles)
    length(df_MiP$N.px[df_MiP$N.px>0])
    
    
# 2. Summaries, IR results files ####    
    # Average per multiple IR Acquisition, Processing or manually checking results file
    # Here we average, independently filters aquired several times (IR_rep) and processed several times (Operators)
    # This is a minor approximation for ease of calculation 
    # It can be fixed later on with adding a summary level.
    # The approximation concerns 4 Filters: 
    
    # How many Filters are Acquired and Processed multiple times? 
    unique(df_MiP$IR_rep)
    MultipleIR_Extract=unique(df_MiP$Extraction_Name[df_MiP$IR_rep %in% c ("ir2" ,"ir3")])
    MultipleIR_File= "m24_451_r_PMF_EC.csv"
    
    for (ex in 1: length(MultipleIR_Extract)){
    #print(length(unique(df_MiP$File_Name[df_MiP$Extraction_Name == MultipleIR_Extract[ex]])))
      if (length(unique(df_MiP$File_Name[df_MiP$Extraction_Name == MultipleIR_Extract[ex]])) > 1) {
       # print(MultipleIR_Extract[ex])
        print(unique(df_MiP$File_Name[df_MiP$Extraction_Name ==MultipleIR_Extract[ex]]))
        MultipleIR_File= c(MultipleIR_File,unique(df_MiP$File_Name[df_MiP$Extraction_Name ==MultipleIR_Extract[ex]]) )
      }
     
    }
    MultipleIR_File=unique(MultipleIR_File)
    
    # Plot concerned filters: 
    ggplot(subset( Summary1g_File,  Summary1g_File$File_Name %in% MultipleIR_File ),  aes(x=File_Name, y=N.particles, color=Polymer.grp, shape = Operator)) + 
    facet_grid(~Extraction_Name, scales =  "free", )+
      geom_point()  +
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"="Other Plastic" ) )+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
            panel.border = element_rect(color = "black", fill = NA, size = 1) )
    
    
    # *2a Mean over IR results files, all factors ####
    Summary2a_IRfiles = Summary1a_File %>% 
      group_by_at( c(Group2_IRfiles, GroupP, GroupS) ) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
    
    # *2b Mean over IR results files, per Polymer.red12 * per Size_cat.um####
    Summary2b_IRfiles = Summary1b_File %>% 
      group_by_at( c(Group2_IRfiles, GroupP12, GroupS) ) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
    # *2b2 Mean over IR results files, per Polymer.red12 * per Size_cat2.um####
    Summary2b2_IRfiles = Summary1b2_File %>% 
      group_by_at( c(Group2_IRfiles, GroupP12, "Size_cat2.um") ) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
    # *2c Mean over IR results files, per Polymer.red12####
    Summary2c_IRfiles = Summary1c_File %>% 
      group_by_at( c(Group2_IRfiles, GroupP12) ) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
    
    # *2d Mean over IR results files, sum up all polymers, excluding "Other.Plastic" ####
    Summary2d_IRfiles = Summary1d_File %>% 
      group_by_at( c(Group2_IRfiles) ) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
    # *2e Mean over IR results files, sum up all polymers, including "Other.Plastic" ####
    Summary2e_IRfiles = Summary1e_File %>% 
      group_by_at( c(Group2_IRfiles) ) %>%
      
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
    # *2f Mean over IR results files, all factors ####
    Summary2f_IRfiles = Summary1f_File %>% 
      group_by_at( c(Group2_IRfiles, GroupS) ) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
    # *2g Mean over IR results files, all polymer ####
    Summary2g_IRfiles = Summary1g_File %>% 
      group_by_at( c(Group2_IRfiles, GroupP) ) %>%
      summarise( N.files = n(),
                 Operators= paste0(Operator, collapse = " ; "),
                 Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
                 sd.particles.operator=sd(N.particles),
                 sd.Area.operator=sd(Tot.Area.mm2)   ) %>%
      ungroup()
    
  
    
    
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
   # Sum per filter.div (one 5g sub sample extraction in multiple filters) 
    # And correction of blank contamination  - Option 2 (Average) 
    # We generate two summaries: 
    # -without correction "Summary3a"
    # -with correction "Summary3a_cor"
    
    Group3_Filters
    
    # * 3a Sum per filter.div, all factors ####
    Summary3a_Filter = Summary2a_IRfiles %>% 
      group_by_at( c(Group3_Filters, GroupP, GroupS) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    
    # Blank Correction - Option 2 (Average) 
    # /!\ In some filters, values will be negative.
    # That is alright and should be canceled out with the number of filters. 
    # Before presentation negative results are put at 0
    
    #Characterize blanks: 
    
    Summary3a_Filter_bcm=subset(  Summary3a_Filter, Soil_sample=="bcm" )
    n_bcm_files=length(unique(   Summary3a_Filter_bcm$Extraction_Name))
    
    # How does an average bcm File look like per lab? 
    
    Summary10a_Lab_bcm= Summary3a_Filter_bcm %>% 
      group_by_at( c("Lab", GroupP, GroupS) ) %>%
      summarise( N.div = n(),
                 Mean.particles= mean(Mean.particles), #
                 Mean.px=mean(Mean.px),              # 
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=mean(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    # Check
    sum(Summary10a_Lab_bcm$Mean.particles[Summary10a_Lab_bcm$Lab=="WUR" & Summary10a_Lab_bcm$Polymer.grp == "PP"])
    
    #Only keep the non null rows to apply the correction : 
    Summary10a_Lab_bcm_cor=subset(Summary10a_Lab_bcm, Mean.particles>0)
    
    sum( Summary10a_Lab_bcm_cor$Mean.particles[ Summary10a_Lab_bcm_cor$Lab=="WUR" &  Summary10a_Lab_bcm_cor$Polymer.grp == "PP"])
    
    write.csv(Summary10a_Lab_bcm_cor, paste(wd.out,"Summary_Blanks_meanFilter_4correction.csv",sep = "/"))
    
    #Corrected summary:
    Summary3a_Filter_cor= Summary3a_Filter 
      
      # Start For loop per Summary10a_Lab_bcm:
      for (b in 1:nrow (Summary10a_Lab_bcm_cor)) {
        Summary3a_Filter_cor[  Summary3a_Filter_cor$Lab ==Summary10a_Lab_bcm_cor$Lab[b]&
                               Summary3a_Filter_cor$Polymer.grp ==Summary10a_Lab_bcm_cor$Polymer.grp[b] &
                               Summary3a_Filter_cor$Size_cat.um ==Summary10a_Lab_bcm_cor$Size_cat.um[b]  ,
                               c("Mean.particles","Mean.px","Mean.Tot.Area.mm2","Mean.Tot.Mass.ng")] =
          sweep( 
          Summary3a_Filter_cor[  Summary3a_Filter_cor$Lab ==Summary10a_Lab_bcm_cor$Lab[b]&
                                 Summary3a_Filter_cor$Polymer.grp ==Summary10a_Lab_bcm_cor$Polymer.grp[b] &
                                 Summary3a_Filter_cor$Size_cat.um ==Summary10a_Lab_bcm_cor$Size_cat.um[b]  ,
                                 c("Mean.particles","Mean.px","Mean.Tot.Area.mm2","Mean.Tot.Mass.ng")] ,
          MARGIN =2, #subtraction column-wise (MARGIN = 2)
          as.numeric(Summary10a_Lab_bcm_cor[b, c("Mean.particles","Mean.px","Mean.Tot.Area.mm2","Mean.Tot.Mass.ng")]),
          FUN = "-")
       
      } # Summary10a_Lab_bcm

 
    # * 3b Sum per filter.div, Polymer.red12 * per Size_cat.um####
    Summary3b_Filter = Summary2b_IRfiles %>% 
      group_by_at( c(Group3_Filters, GroupP12, GroupS) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    

    
    ## Apply blank correction -OPTION 2 (Sum) From lower level summary 
    Summary3b_Filter_cor= Summary3a_Filter_cor %>% 
      group_by_at( c(Group3_Filters, GroupP12, GroupS) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    # * 3b2 Sum per filter.div, Polymer.red12 * per Size_cat2.um####
    Summary3b2_Filter = Summary2b2_IRfiles %>% 
      group_by_at( c(Group3_Filters, GroupP12, "Size_cat2.um") ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    
    ## Apply blank correction -OPTION 2 (Sum) From lower level summary 
    Summary3b2_Filter_cor = Summary3b_Filter_cor %>% 
      group_by_at( c(Group3_Filters, GroupP12, "Size_cat2.um") ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    
    
    # * 3c Sum per filter.div, Polymer.red12 ####
    Summary3c_Filter = Summary2c_IRfiles %>% 
      group_by_at( c(Group3_Filters, GroupP12) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    

    
    
    ## Apply blank correction -OPTION 2 (Sum) From lower level summary 
    Summary3c_Filter_cor=  Summary3b_Filter_cor %>% 
      group_by_at( c(Group3_Filters, GroupP12) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
   
    
    # * 3d Sum per filter.div, sum up all polymers, excluding "Other.Plastic"  ####
    Summary3d_Filter = Summary2d_IRfiles %>% 
      group_by_at( c(Group3_Filters) ) %>%
      
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    
    ## Apply blank correction -OPTION 2 (Substracion) From same level summary   
    # Select bcm
    Summary3d_Filter_bcm=subset(  Summary3d_Filter, Soil_sample=="bcm" )
    n_bcm_files=length(unique(   Summary3d_Filter_bcm$Extraction_Name))
    sum(Summary3d_Filter_bcm$Mean.particles)
    length(unique(   Summary3d_Filter_bcm$Extraction_Name))
    
    # How does an average bcm File look like per lab? 
    
    Summary10d_Lab_bcm= Summary3d_Filter_bcm %>% 
      group_by_at( c("Lab") ) %>%
      summarise( N.div = n(),
                 Mean.particles= mean(Mean.particles), #
                 Mean.px=mean(Mean.px),              # 
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=mean(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    # Check
    sum(Summary10d_Lab_bcm$Mean.particles[Summary10d_Lab_bcm$Lab=="WUR" ])
    
    #Only keep the non null rows to apply the correction : 
    Summary10d_Lab_bcm_cor=subset(Summary10d_Lab_bcm, Mean.particles>0)
    
    sum( Summary10d_Lab_bcm_cor$Mean.particles[ Summary10d_Lab_bcm_cor$Lab=="WUR"])
    
    #write.csv(Summary10a_Lab_bcm_cor, paste(wd.out,"Summary_Blanks_meanFilter_4correction.csv",sep = "/"))
    
    #Corrected summary:
    Summary3d_Filter_cor= Summary3d_Filter 
    
    # Start For loop per Summary10a_Lab_bcm:
    for (b in 1:nrow (Summary10d_Lab_bcm_cor)) {
      Summary3d_Filter_cor[  Summary3d_Filter_cor$Lab ==Summary10d_Lab_bcm_cor$Lab[b],
                             c("Mean.particles","Mean.px","Mean.Tot.Area.mm2","Mean.Tot.Mass.ng")] =
        sweep( 
          Summary3d_Filter_cor[  Summary3d_Filter_cor$Lab == Summary10d_Lab_bcm_cor$Lab[b]  , 
                                 c("Mean.particles","Mean.px","Mean.Tot.Area.mm2","Mean.Tot.Mass.ng")] ,
          MARGIN =2, #subtraction column-wise (MARGIN = 2)
          as.numeric(Summary10d_Lab_bcm_cor[b, c("Mean.particles","Mean.px","Mean.Tot.Area.mm2","Mean.Tot.Mass.ng")]),
          FUN = "-")
      
    } # Summary10b_Lab_bcm
    
  
    
    # * 3e Sum per filter.div, sum up all polymers, including "Other.Plastic"  ####
    Summary3e_Filter = Summary2e_IRfiles %>% 
      group_by_at( c(Group3_Filters) ) %>%
      
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    ## Apply blank correction -OPTION 2 (Sum) From lower level summary 
    Summary3e_Filter_cor=  Summary3c_Filter_cor %>% 
      group_by_at( c(Group3_Filters) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    
    # * 3f Sum per filter.div, size cat ####
    Summary3f_Filter = Summary2f_IRfiles %>% 
      group_by_at( c(Group3_Filters, GroupS) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
  
    ## Apply blank correction -OPTION 2 (Sum) From lower level summary 
    Summary3f_Filter_cor=  Summary3b_Filter_cor %>% 
      group_by_at( c(Group3_Filters, GroupS) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    
    # * 3g Sum per filter.div, all polymer ####
    Summary3g_Filter = Summary2g_IRfiles %>% 
      group_by_at( c(Group3_Filters, GroupP) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    

    ## Apply blank correction -OPTION 2 (Sum) From lower level summary 
    Summary3g_Filter_cor=  Summary3a_Filter_cor %>% 
      group_by_at( c(Group3_Filters, GroupP) ) %>%
      summarise( N.div = n(),
                 Mean.particles= sum(Mean.particles), #
                 Mean.px=sum(Mean.px),              # 
                 Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
                 Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
   

    
    # //\\ \\// Preparation_Type=="Field_samples" \\// //\\####
    #OR Preparation_Type %in% c("Field_samples", "Standard_Soil") ? 
    
    
    
    
    
# 4. Summaries, Soil_samples ####    
    Group4_Soil
   
    
    # * 4a Mean per Soil_samples, all factors ####
    Summary4a_Soil= subset( Summary3a_Filter_cor, Preparation_Type == "Field_samples" )  %>% #Preparation_Type %in% c("Field_samples", "Standard_Soil")
      group_by_at( c( Group4_Soil, GroupP, GroupS) ) %>% # For each PMF_File_Name, get the summary
      summarise( N.extract = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.particles= mean( Mean.particles),
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()

    
    
    
    # How many Soil Samples are extracted multiple times? 
    unique(df_MiP$IR_rep)
    # list of soils with multiple extractions
    MultipleSoil_Extract=unique(Summary4a_Soil$Soil_sample[ Summary4a_Soil$N.extract>1])
    
    MultipleExtract_Name=  "m15_8.8.1_S1_n" 
    
    # For each of this soil 
    for (ex in 1: length(MultipleSoil_Extract)){
      #print(length(unique(df_MiP$File_Name[df_MiP$Extraction_Name == MultipleIR_Extract[ex]])))
      if (length(unique(df_MiP$Extraction_Name[df_MiP$Soil_sample == MultipleSoil_Extract[ex]])) > 1) { # If there is more than one 
        # print(MultipleIR_Extract[ex])
        print(unique(df_MiP$Extraction_Name[df_MiP$Soil_sample == MultipleSoil_Extract[ex]])) 
        MultipleExtract_Name= c( MultipleExtract_Name,unique(df_MiP$Extraction_Name[df_MiP$Soil_sample == MultipleSoil_Extract[ex]]) )
      }
      
    }
    MultipleExtract_Name=unique( MultipleExtract_Name)
    
    # Plot concerned filters: 
    ggplot(subset( Summary3c_Filter, Extraction_Name %in%  MultipleExtract_Name  ),  aes(x=Extraction_Name, y=Mean.particles, color=Polymer.red12)) + 
      facet_wrap(~Soil_sample + Preparation_Type, scales =  "free" , nrow = 5)+
      geom_point()  +
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                    "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                    "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                    "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                    "CA"= "#FFD39B") , 
                         # Relabel  "Other.Plastic"                 
                         labels = c( "Other.Plastic"="Other Plastic" ) )+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
            panel.border = element_rect(color = "black", fill = NA, size = 1) )
    
    
    # * 4b Mean per Soil_samples, Polymer.red12 * per Size_cat.um ####
    
    Summary4b_Soil= subset( Summary3b_Filter_cor, Preparation_Type == "Field_samples" ) %>% 
      group_by_at( c( Group4_Soil, GroupP12, GroupS) ) %>% # For each PMF_File_Name, get the summary
      summarise( N.extract = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()

    # * 4b2 Mean per Soil_samples, Polymer.red12 * per Size_cat2.um ####
    
    Summary4b2_Soil= subset( Summary3b2_Filter_cor, Preparation_Type == "Field_samples" ) %>% 
      group_by_at( c( Group4_Soil, GroupP12, "Size_cat2.um") ) %>% # For each PMF_File_Name, get the summary
      summarise( N.extract = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
   
    
    # * 4c Mean per Soil_samples, Polymer.red12 ####
    
    Summary4c_Soil= subset( Summary3c_Filter_cor, Preparation_Type == "Field_samples" ) %>% 
      group_by_at( c( Group4_Soil, GroupP12) ) %>%# For each PMF_File_Name, get the summary
      summarise( N.extract = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()

    
    # * 4d sum up all polymers, excluding "Other.Plastic"  ####
    
    Summary4d_Soil= subset( Summary3d_Filter_cor, Preparation_Type == "Field_samples" ) %>% 
      group_by_at( c( Group4_Soil) ) %>% # For each PMF_File_Name, get the summary
                 
      summarise( N.extract = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    
    # * 4e sum up all polymers, including "Other.Plastic" ####
    
    Summary4e_Soil= subset( Summary3e_Filter_cor, Preparation_Type == "Field_samples" ) %>% 
      group_by_at( c( Group4_Soil) ) %>% # For each PMF_File_Name, get the summary
      
      summarise( N.extract = n(),
                 Mean.particles.S= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.S= min( Mean.particles),
                 Max.particles.S= max( Mean.particles),
                 Mean.px.S=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.S=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.S=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.S=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.S=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    
    # * 4f sum up all polymers, size cat ####
    
    Summary4f_Soil= subset( Summary3f_Filter_cor, Preparation_Type == "Field_samples" ) %>% 
      group_by_at( c( Group4_Soil, GroupS) ) %>% # For each PMF_File_Name, get the summary
      
      summarise( N.extract = n(),
                 Mean.particles.S= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.S= min( Mean.particles),
                 Max.particles.S= max( Mean.particles),
                 Mean.px.S=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.S=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.S=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.S=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.S=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    
    # * 4g Mean per Soil_samples, Polymer.grp ####
    
    Summary4g_Soil= subset( Summary3g_Filter_cor, Preparation_Type == "Field_samples" ) %>% 
      group_by_at( c( Group4_Soil, GroupP) ) %>% # For each PMF_File_Name, get the summary
      summarise( N.extract = n(),
                 Mean.particles.S= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.S= min( Mean.particles),
                 Max.particles.S= max( Mean.particles),
                 Mean.px.S=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.S=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.S=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.S=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.S=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    
    
    # Mean per field of the soil samples. 
    
# 5. Summaries, Field ####       
    Group5_Field
    
    # * 5a Mean per Field, all factors ####
    Summary5a_Field= Summary4a_Soil%>% 
      group_by_at( c(Group5_Field, GroupP, GroupS) ) %>% # For each PMF_File_Name, get the summary
      summarise( N.soils = n(),
                 Mean.particles.Fd= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.Fd= min( Mean.particles),
                 Max.particles.Fd= max( Mean.particles),
                 Mean.px.Fd=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    
    # * 5b Mean per Field_samples, Polymer.red12 * per Size_cat.um ####
    
    Summary5b_Field= Summary4b_Soil%>% 
      group_by_at( c(Group5_Field, GroupP12, GroupS) ) %>% # For each PMF_File_Name, get the summary
      summarise( N.soils = n(),
                 Mean.particles.Fd= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.Fd= min( Mean.particles),
                 Max.particles.Fd= max( Mean.particles),
                 Mean.px.Fd=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    # * 5b2 Mean per Field_samples, Polymer.red12 * per Size_cat2.um ####
    
    Summary5b2_Field= Summary4b2_Soil%>% 
      group_by_at( c(Group5_Field, GroupP12, "Size_cat2.um" ) ) %>% # For each PMF_File_Name, get the summary
      summarise( N.soils = n(),
                 Mean.particles.Fd= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.Fd= min( Mean.particles),
                 Max.particles.Fd= max( Mean.particles),
                 Mean.px.Fd=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    # * 5c Mean per Field_samples, Polymer.red12 ####
    
    Summary5c_Field= Summary4c_Soil%>% 
      group_by_at( c(Group5_Field, GroupP12) ) %>%# For each PMF_File_Name, get the summary
      summarise( N.soils = n(),
                 Mean.particles.Fd= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.Fd= min( Mean.particles),
                 Max.particles.Fd= max( Mean.particles),
                 Mean.px.Fd=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng) ) %>% #  Mean mass per sample and polymer, over the files/operators
      ungroup() 
       
    
    
    # * 5d sum up all polymers, excluding "Other.Plastic" ####
    
    Summary5d_Field= Summary4d_Soil %>% 
      group_by_at( c(Group5_Field) ) %>%# For each PMF_File_Name, get the summary
      
      summarise(N.soils = n(),
                Mean.particles.Fd= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.Fd= min( Mean.particles),
                Max.particles.Fd= max( Mean.particles),
                Mean.px.Fd=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng) ) %>% #  Mean mass per sample and polymer, over the files/operators
      ungroup() 
      
    
    # * 5e sum up all polymers,  including "Other.Plastic"  ####
    
    Summary5e_Field= Summary4e_Soil %>% 
      group_by_at( c(Group5_Field) ) %>%# For each PMF_File_Name, get the summary
      
      summarise(N.soils = n(),
                Mean.particles.Fd= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.Fd= min( Mean.particles.S),
                Max.particles.Fd= max( Mean.particles.S),
                Mean.px.Fd=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2.S),
                Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2.S),
                Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng.S), #  Mean mass per sample and polymer, over the files/operators
                Min.Tot.Mass.ng.Fd=min( Mean.Tot.Mass.ng.S),
                Max.Tot.Mass.ng.Fd=max( Mean.Tot.Mass.ng.S),
                Min_Max_diff=Max.particles.Fd-Min.particles.Fd,
                Formula=Min_Max_diff-Min.particles.Fd/2
      ) %>%
      ungroup()
   
    Summary5e_Field$CSS_Farm_Field= paste(Summary5e_Field$CSS,Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
    

 
    # * 5f sum up all polymers,  including "Other.Plastic", Per size.cat ####
    
    Summary5f_Field= Summary4f_Soil %>% 
      group_by_at( c(Group5_Field, GroupS) ) %>%# For each PMF_File_Name, get the summary
      
      summarise(N.soils = n(),
                Mean.particles.Fd= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.Fd= min( Mean.particles.S),
                Max.particles.Fd= max( Mean.particles.S),
                Mean.px.Fd=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2.S),
                Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2.S),
                Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng.S), #  Mean mass per sample and polymer, over the files/operators
                Min_Max_diff=Max.particles.Fd-Min.particles.Fd,
                Formula=Min_Max_diff-Min.particles.Fd/2
      ) %>%
      ungroup()
    
    Summary5e_Field$CSS_Farm_Field= paste(Summary5e_Field$CSS,Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
    
    
    # * 5g  Mean per Farm,  Per polymer.grp  ####
    Summary5g_Field= Summary4g_Soil %>%
      group_by_at( c(Group5_Field, GroupP) ) %>% # For each PMF_File_Name, get the summary

      
      summarise(N.soils = n(),
                Mean.particles.Fd= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.Fd= min( Mean.particles.S),
                Max.particles.Fd= max( Mean.particles.S),
                Mean.px.Fd=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.Fd=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.Fd=min(Mean.Tot.Area.mm2.S),
                Max.Tot.Area.mm2.Fd=max(Mean.Tot.Area.mm2.S),
                Mean.Tot.Mass.ng.Fd=mean( Mean.Tot.Mass.ng.S), #  Mean mass per sample and polymer, over the files/operators
                Min_Max_diff=Max.particles.Fd-Min.particles.Fd,
                Formula=Min_Max_diff-Min.particles.Fd/2
      ) %>%
      ungroup()

    
  #Check
    sum(subset(Summary5f_Field, CSS==6 & Farm==6 & Field==1, select=Mean.particles.Fd))
    sum(subset(Summary5e_Field, CSS==6& Farm==6 & Field==1, select=Mean.particles.Fd))
    sum(subset(Summary5c_Field, CSS==6& Farm==6 & Field==1, select=Mean.particles.Fd))
    sum(subset(Summary5b_Field, CSS==6& Farm==6 & Field==1, select=Mean.particles.Fd))
    sum(subset(Summary5a_Field, CSS==6& Farm==6 & Field==1, select=Mean.particles.Fd))
    
    sum(subset(Summary5f_Field, CSS==6 & Farm==6 & Field==1 , select=Mean.particles.Fd))
    sum(subset(Summary5e_Field, CSS==6& Farm==6 & Field==1, select=Mean.particles.Fd))
    sum(subset(Summary5c_Field, CSS==6& Farm==6 & Field==1& Polymer.red12=="PE", select=Mean.particles.Fd))
    sum(subset(Summary5b_Field, CSS==6& Farm==6 & Field==1& Polymer.red12=="PE", select=Mean.particles.Fd))
    sum(subset(Summary5a_Field, CSS==6& Farm==6 & Field==1& Polymer.red12=="PE", select=Mean.particles.Fd))
    
    (subset(Summary5b_Field, CSS==6& Farm==6 & Field==1 &Polymer.red12=="PE"))
    (subset(Summary5b2_Field, CSS==6& Farm==6 & Field==1 &Polymer.red12=="PE"))
    
    # 6. Summary per Farm     ####
    # Mean (Field)  per farm
    Group6_Farm
    
    # * 6a Mean per Farm, all factors ####
    Summary6a_Farm= Summary5a_Field%>% 
      group_by_at( c(Group6_Farm, GroupP, GroupS) )  %>% # For each PMF_File_Name, get the summary
      summarise( N.fields = n(),
                 Mean.particles.Fm= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.Fm= min( Mean.particles.Fd),
                 Max.particles.Fm= max( Mean.particles.Fd),
                 Mean.px.Fm=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.Fm=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.Fm=min(Mean.Tot.Area.mm2.Fd),
                 Max.Tot.Area.mm2.Fm=max(Mean.Tot.Area.mm2.Fd),
                 Mean.Tot.Mass.ng.Fm=mean( Mean.Tot.Mass.ng.Fd) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    
    # * 6b Mean per Farm, Polymer.red12 * per Size_cat.um ####
    
    Summary6b_Farm= Summary5b_Field%>% 
      group_by_at( c(Group6_Farm, GroupP12, GroupS) )  %>% # For each PMF_File_Name, get the summary
      summarise( N.fields = n(),
                 Mean.particles.Fm= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.Fm= min( Mean.particles.Fd),
                 Max.particles.Fm= max( Mean.particles.Fd),
                 Mean.px.Fm=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.Fm=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.Fm=min(Mean.Tot.Area.mm2.Fd),
                 Max.Tot.Area.mm2.Fm=max(Mean.Tot.Area.mm2.Fd),
                 Mean.Tot.Mass.ng.Fm=mean( Mean.Tot.Mass.ng.Fd) #  Mean mass per sample and polymer, over the files/operators
      ) %>%
      ungroup()
    
    
    # * 6c Mean per Farm, Polymer.red12 ####
    
    Summary6c_Farm= Summary5c_Field%>% 
      group_by_at( c(Group6_Farm, GroupP12) )  %>% # For each PMF_File_Name, get the summary
      summarise( N.fields = n(),
                 Mean.particles.Fm= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles.Fm= min( Mean.particles.Fd),
                 Max.particles.Fm= max( Mean.particles.Fd),
                 Mean.px.Fm=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2.Fm=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2.Fm=min(Mean.Tot.Area.mm2.Fd),
                 Max.Tot.Area.mm2.Fm=max(Mean.Tot.Area.mm2.Fd),
                 Mean.Tot.Mass.ng.Fm=mean( Mean.Tot.Mass.ng.Fd) ) %>%
      ungroup()#  Mean mass per sample and polymer, over the files/operators
    
    
    
    # * 6d  Mean per Farm, sum up all polymers, excluding "Other.Plastic"   ####
    
    Summary6d_Farm= Summary5d_Field %>% 
      group_by_at( c(Group6_Farm) ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.fields = n(),
                Mean.particles.Fm= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.Fm= min( Mean.particles.Fd),
                Max.particles.Fm= max( Mean.particles.Fd),
                Mean.px.Fm=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.Fm=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.Fm=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.Fm=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.Fm=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup() #  Mean mass per sample and polymer, over the files/operators
    
    
    # * 6e Mean per Farm, sum up all polymers, including "Other.Plastic" ####
    
    Summary6e_Farm= Summary5e_Field %>% 
      group_by_at( c(Group6_Farm) ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.fields = n(),
                Mean.particles.Fm= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.Fm= min( Mean.particles.Fd),
                Max.particles.Fm= max( Mean.particles.Fd),
                Mean.px.Fm=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.Fm=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.Fm=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.Fm=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.Fm=mean( Mean.Tot.Mass.ng.Fd), #  Mean mass per sample and polymer, over the files/operators
                Min_Max_diff=Max.particles.Fm-Min.particles.Fm,
                Formula=Min_Max_diff-Min.particles.Fm/2
      ) %>%
      ungroup()
    
    # # * 6g  Mean per Farm,  Per polymer.grp  ####
    # Summary6g_Farm= Summary5g %>% 
    #   group_by_at( c(Group6_Farm, GroupP) ) %>% # For each PMF_File_Name, get the summary
    #   
    #   summarise(N.fields = n(),
    #             Mean.particles.CSS= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.CSS= median( Mean.particles.S),
    #             Min.particles.CSS= min( Mean.particles.S),
    #             Max.particles.CSS= max( Mean.particles.S),
    #             Mean.px.CSS=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    # 
    # 
    # Summary6g_Farm_outlier = subset(Summary4g_Soil, CSS!=11 | Farm !=10 ) %>% 
    #   group_by_at( c(Group6_Farm, GroupP) ) %>% # For each PMF_File_Name, get the summary
    #   
    #   summarise(N.fields = n(),
    #             Mean.particles.CSS= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.CSS= median( Mean.particles.S),
    #             Min.particles.CSS= min( Mean.particles.S),
    #             Max.particles.CSS= max( Mean.particles.S),
    #             Mean.px.CSS=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    # 
       
# 7. Summaries, CSS#### 
  #  Mean (Field) per CSS
   Group7_CSS 
    
   
    #  * 7a Mean per CSS, all factors #### 
    Summary7a_CSS= Summary5a_Field %>% 
      group_by_at( c(Group7_CSS , GroupP, GroupS) )  %>% # For each PMF_File_Name, get the summary
      summarise(N.files = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()
    
    
    # * 7b Mean per CSS, Polymer.red12 * per Size_cat.um ####
    
    Summary7b_CSS= Summary5b_Field %>% 
      group_by_at( c(Group7_CSS , GroupP12, GroupS) )  %>% # For each PMF_File_Name, get the summary
        summarise(N.fields = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()
    
    
    # * 7c Mean per Field_samples, Polymer.red12 ####
    
    Summary7c_CSS= Summary5c_Field %>% 
      group_by_at( c(Group7_CSS , GroupP12) )  %>% # For each PMF_File_Name, get the summary
      summarise(N.fields = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()
    
    

    Summary7c_CSS_outlier = subset(Summary5c_Field, CSS!=11 | Farm !=10 ) %>% 
      group_by_at( c(Group7_CSS , GroupP12) ) %>% # For each PMF_File_Name, get the summary
      summarise(N.fields = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()
    
    
    
    # * 7d sum up all polymers, excluding "Other.Plastic"  ####
    
    Summary7d_CSS= Summary5d_Field %>% 
      group_by(Preparation_Type,  
               CSS) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.fields = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()

    # * 7e sum up all polymers, including "Other.Plastic"  ####

    Summary7e_CSS= Summary5e_Field%>% 
      group_by(Preparation_Type,  
               CSS) %>% # For each PMF_File_Name, get the summary
      summarise(N.fields = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()
    
    Summary7e_CSS_outlier = subset(Summary5e_Field, CSS!=11 | Farm !=10 ) %>% 
      group_by(Preparation_Type,  
               CSS ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.fields = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()
    
    
    
    # * 7f sum up all polymers, size cat  ####
    
    Summary7f_CSS= Summary5f_Field   %>% 
      group_by_at( c(Group7_CSS , GroupS) )  %>% # For each PMF_File_Name, get the summary
      summarise(N.fields = n(),
                Mean.particles.CSS= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Median.particles.CSS= median( Mean.particles.Fd),
                Min.particles.CSS= min( Mean.particles.Fd),
                Max.particles.CSS= max( Mean.particles.Fd),
                Mean.px.CSS=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.Fd) )%>%
      ungroup()
    
    # # * 7g sum up all polymers, Polymer.grp ####
    # 
    # Summary7g_CSS= Summary4g_Soil %>% 
    #   group_by_at( c(Group7_CSS , GroupP) )%>% # For each PMF_File_Name, get the summary
    #   
    #   summarise(N.files = n(),
    #             Mean.particles.CSS= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.CSS= median( Mean.particles.S),
    #             Min.particles.CSS= min( Mean.particles.S),
    #             Max.particles.CSS= max( Mean.particles.S),
    #             Mean.px.CSS=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    # 
    # 
    # Summary7g_CSS_outlier = subset(Summary4g_Soil, CSS!=11 | Farm !=10 ) %>% 
    #   group_by_at( c(Group7_CSS , GroupP) ) %>% # For each PMF_File_Name, get the summary
    #   
    #   summarise(N.files = n(),
    #             Mean.particles.CSS= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.CSS= median( Mean.particles.S),
    #             Min.particles.CSS= min( Mean.particles.S),
    #             Max.particles.CSS= max( Mean.particles.S),
    #             Mean.px.CSS=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.CSS=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.CSS=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.CSS=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.CSS=max(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.CSS=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    ####################### Work in progress ####################
    # * Custom summary per CSS, polymer and size category 
    #############################################################   
    

# 8. Summaries, Project #### 
    # Mean (Field) All MINAGRIS project
    
    #  * 8a Mean all MINAGRIS, all factors #### 
    Summary8a_MINAGRIS= Summary5a_Field %>%
      group_by(Preparation_Type,
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um ) %>%
      summarise(N.fields = n(),
                Mean.particles.MM= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.Fd),
                Max.particles.MM= max( Mean.particles.Fd),
                Median.particles.MM= median( Mean.particles.Fd),
                sd.particles.MM= sd( Mean.particles.Fd),
                Mean.px.MM=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.Fd),
                sd.Tot.Area.mm2.MM=sd(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.Fd) ) %>%
      ungroup()
    
    
    # * 8b Mean all MINAGRIS, Polymer.red12 * per Size_cat.um ####
    
    Summary8b_MINAGRIS= Summary5b_Field%>% 
      group_by(Preparation_Type, 
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise(N.fields = n(),
                Mean.particles.MM= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.Fd),
                Max.particles.MM= max( Mean.particles.Fd),
                Median.particles.MM= median( Mean.particles.Fd),
                sd.particles.MM= sd( Mean.particles.Fd),
                Mean.px.MM=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.Fd),
                sd.Tot.Area.mm2.MM=sd(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.Fd) ) %>%
      ungroup()
    
    
    # * 8c Mean all MINAGRIS, Polymer.red12 ####
    
    Summary8c_MINAGRIS= Summary5c_Field%>% 
      group_by(Preparation_Type,  
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
      summarise(N.fields = n(),
                Mean.particles.MM= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.Fd),
                Max.particles.MM= max( Mean.particles.Fd),
                Median.particles.MM= median( Mean.particles.Fd),
                sd.particles.MM= sd( Mean.particles.Fd),
                Mean.px.MM=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.Fd),
                sd.Tot.Area.mm2.MM=sd(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.Fd) ) %>%
      ungroup()
    
    
    # * 8d all MINAGRIS, excluding "Other.Plastic"  ####
    
    Summary8d_MINAGRIS= Summary5d_Field %>% 
      group_by(Preparation_Type ) %>% # For each PMF_File_Name, get the summary
      summarise(N.fields = n(),
                Mean.particles.MM= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.Fd),
                Max.particles.MM= max( Mean.particles.Fd),
                Median.particles.MM= median( Mean.particles.Fd),
                sd.particles.MM= sd( Mean.particles.Fd),
                Mean.px.MM=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.Fd),
                sd.Tot.Area.mm2.MM=sd(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.Fd) ) %>%
      ungroup()
    
    # * 8e all MINAGRIS, including "Other.Plastic" ####
    
    Summary8e_MINAGRIS= Summary5e_Field %>% 
      group_by(Preparation_Type ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.fields = n(),
                Mean.particles.MM= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.Fd),
                Max.particles.MM= max( Mean.particles.Fd),
                Median.particles.MM= median( Mean.particles.Fd),
                sd.particles.MM= sd( Mean.particles.Fd),
                Mean.px.MM=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.Fd),
                sd.Tot.Area.mm2.MM=sd(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.Fd) ) %>%
      ungroup()
    
    #  * 8f Mean all MINAGRIS, all factors #### 
    Summary8f_MINAGRIS= Summary5f_Field %>%
      group_by(Preparation_Type,
             Size_cat.um ) %>%
      summarise(N.fields = n(),
                Mean.particles.MM= mean( Mean.particles.Fd), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.Fd),
                Max.particles.MM= max( Mean.particles.Fd),
                Median.particles.MM= median( Mean.particles.Fd),
                sd.particles.MM= sd( Mean.particles.Fd),
                Mean.px.MM=mean( Mean.px.Fd),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.Fd), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.Fd),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.Fd),
                sd.Tot.Area.mm2.MM=sd(Mean.Tot.Area.mm2.Fd),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.Fd) ) %>%
      ungroup()

    # * 8g sum up all polymers, Polymer.grp ####
    
    Summary8g_MINAGRIS= Summary4g_Soil %>% 
      group_by_at( c(GroupP) )%>% # For each PMF_File_Name, get the summary
      
      summarise(N.fields = n(),
                Mean.particles.MM= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.S),
                Max.particles.MM= max( Mean.particles.S),
                Median.particles.MM= median( Mean.particles.S),
                sd.particles.MM= sd( Mean.particles.S),
                Mean.px.MM=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.S),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.S),
                sd.Tot.Area.mm2.MM=sd(Mean.Tot.Area.mm2.S),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.S),
                Min.Tot.Mass.ng.MM=min( Mean.Tot.Mass.ng.S),
                Max.Tot.Mass.ng.MM=max( Mean.Tot.Mass.ng.S) ) %>%
      ungroup()
    

    # # 9. Summaries, Cropping System ####      
    # Group9_Crop
    # 
    # Summary4e_Soil %>% 
    #   group_by(CropDominant3)%>% 
    # summarise(N_Soils = n()) %>%
    #   ungroup()
    # 
    # Summary4e_Soil %>% 
    #   group_by(CropDominant4)%>% 
    #   summarise(N_Soils = n()) %>%
    #   ungroup()
    # 
    # 
    # 
    # #  * 9a Mean per Crop, all factors #### 
    # Summary9a_Crop= Summary4a_Soil %>% 
    #   group_by_at( c(Group9_Crop , GroupP, GroupS) )  %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles),
    #             Min.particles.Crop= min( Mean.particles),
    #             Max.particles.Crop= max( Mean.particles),
    #             SD.particles.Crop= sd( Mean.particles),
    #             Mean.px.Crop=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng) )%>%
    #   ungroup()
    # 
    # 
    # # * 9b Mean per Crop, Polymer.red12 * per Size_cat.um ####
    # 
    # Summary9b_Crop= Summary4b_Soil %>% 
    #   group_by_at( c(Group9_Crop , GroupP12, GroupS) )  %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles),
    #             Min.particles.Crop= min( Mean.particles),
    #             Max.particles.Crop= max( Mean.particles),
    #             SD.particles.Crop= sd( Mean.particles),
    #             Mean.px.Crop=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng) ) %>%
    #   ungroup()
    # 
    # 
    # # * 9c Mean per Field_samples, Polymer.red12 ####
    # 
    # Summary9c_Crop= Summary4c_Soil %>% 
    #   group_by_at( c(Group9_Crop , GroupP12) )  %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles),
    #             Min.particles.Crop= min( Mean.particles),
    #             Max.particles.Crop= max( Mean.particles),
    #             SD.particles.Crop= sd( Mean.particles),
    #             Mean.px.Crop=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng) ) %>%
    #   ungroup()
    # 
    # 
    # 
    # Summary9c_Crop_outlier = subset(Summary4c_Soil, CSS!=11 | Farm !=10 ) %>% 
    #   group_by_at( c(Group9_Crop , GroupP12) ) %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles),
    #             Min.particles.Crop= min( Mean.particles),
    #             Max.particles.Crop= max( Mean.particles),
    #             SD.particles.Crop= sd( Mean.particles),
    #             Mean.px.Crop=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng) ) %>%
    #   ungroup()
    # 
    # 
    # 
    # # * 9d sum up all polymers, excluding "Other.Plastic"  ####
    # 
    # Summary9d_Crop= Summary4d_Soil %>% 
    #   group_by_at(Group9_Crop) %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles),
    #             Min.particles.Crop= min( Mean.particles),
    #             Max.particles.Crop= max( Mean.particles),
    #             SD.particles.Crop= sd( Mean.particles),
    #             Mean.px.Crop=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng) ) %>%
    #   ungroup()
    # 
    # # * 9e sum up all polymers, including "Other.Plastic"  ####
    # 
    # Summary9e_Crop= Summary4e_Soil %>% 
    #   group_by_at(Group9_Crop) %>%  # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles.S),
    #             Min.particles.Crop= min( Mean.particles.S),
    #             Max.particles.Crop= max( Mean.particles.S),
    #             SD.particles.Crop= sd( Mean.particles.S),
    #             Mean.px.Crop=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2.S),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    # Summary9e_Crop_outlier = subset(Summary4e_Soil, CSS!=11 | Farm !=10 ) %>% 
    #   group_by_at(Group9_Crop) %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles.S),
    #             Min.particles.Crop= min( Mean.particles.S),
    #             Max.particles.Crop= max( Mean.particles.S),
    #             SD.particles.Crop= sd( Mean.particles.S),
    #             Mean.px.Crop=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2.S),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    # 
    # 
    # # * 9f sum up all polymers, size cat  ####
    # 
    # Summary9f_Crop= Summary4f_Soil %>% 
    #   group_by_at( c(Group9_Crop , GroupS) )  %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles),
    #             Min.particles.Crop= min( Mean.particles),
    #             Max.particles.Crop= max( Mean.particles),
    #             SD.particles.Crop= sd( Mean.particles),
    #             Mean.px.Crop=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng) ) %>%
    #   ungroup()
    # 
    # # * 9g sum up all polymers, Polymer.grp ####
    # 
    # Summary9g_Crop= Summary4g_Soil %>% 
    #   group_by_at( c(Group9_Crop , GroupP) )%>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles.S),
    #             Min.particles.Crop= min( Mean.particles.S),
    #             Max.particles.Crop= max( Mean.particles.S),
    #             SD.particles.Crop= sd( Mean.particles.S),
    #             Mean.px.Crop=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2.S),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    # 
    # 
    # Summary9g_Crop_outlier = subset(Summary4g_Soil, CSS!=11 | Farm !=10 ) %>% 
    #   group_by_at( c(Group9_Crop , GroupP) ) %>% # For each PMF_File_Name, get the summary
    #   summarise(N.files = n(),
    #             Mean.particles.Crop= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
    #             Median.particles.Crop= median( Mean.particles.S),
    #             Min.particles.Crop= min( Mean.particles.S),
    #             Max.particles.Crop= max( Mean.particles.S),
    #             SD.particles.Crop= sd( Mean.particles.S),
    #             Mean.px.Crop=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
    #             Mean.Tot.Area.mm2.Crop=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Median.Tot.Area.mm2.Crop=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
    #             Min.Tot.Area.mm2.Crop=min(Mean.Tot.Area.mm2.S),
    #             Max.Tot.Area.mm2.Crop=max(Mean.Tot.Area.mm2.S),
    #             SD.Tot.Area.mm2.Crop=sd(Mean.Tot.Area.mm2.S),
    #             Mean.Tot.Mass.ng.Crop=mean( Mean.Tot.Mass.ng.S) ) %>%
    #   ungroup()
    # 
    # 
    # 
    # 10. Per lab ####
    #  * 10a Mean all Lab, all factors #### 
    Summary10a_Lab= Summary4a_Soil %>%
      group_by(Preparation_Type, Lab,
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um ) %>%
      summarise(N.soils = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    
    # * 10b Mean all Lab, Polymer.red12 * per Size_cat.um ####
    
    Summary10b_Lab= Summary4b_Soil%>% 
      group_by(Preparation_Type, Lab,
               Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_Name, get the summary
      summarise(N.soils = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) %>%
      ungroup() 
    
    
    # * 10c Mean all Lab, Polymer.red12 ####
    
    Summary10c_Lab= Summary4c_Soil%>% 
      group_by(Preparation_Type,  Lab,
               Polymer.red12,  Polymer.red3)  %>% # For each PMF_File_Name, get the summary
      summarise(N.soils = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) )  %>%
      ungroup()
    
    
    # * 10d all Lab, excluding "Other.Plastic"  ####
    
    Summary10d_Lab= Summary4d_Soil %>% 
      group_by(Lab,Preparation_Type ) %>% # For each PMF_File_Name, get the summary
      summarise(N.soils = n(),
                Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles),
                Max.particles.MM= max( Mean.particles),
                Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) %>%
      ungroup()
    
    # * 10e all Lab, including "Other.Plastic" ####
    
    Summary10e_Lab= Summary4e_Soil %>% 
      group_by(Lab,Preparation_Type ) %>% # For each PMF_File_Name, get the summary
      
      summarise(N.soils = n(),
                Mean.particles.MM= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.S),
                Max.particles.MM= max( Mean.particles.S),
                Mean.px.MM=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.S),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.S),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.S),
                Min.Tot.Mass.ng.MM=min( Mean.Tot.Mass.ng.S),
                Max.Tot.Mass.ng.MM=max( Mean.Tot.Mass.ng.S) ) %>%
      ungroup()
    
    #  * 10f Mean all Lab, all factors #### 
    Summary10f_Lab= Summary4f_Soil %>%
      group_by(Preparation_Type,Lab,
               Size_cat.um ) %>%
      summarise(N.soils = n(),
                Mean.particles.MM= mean( Mean.particles.S), # Mean particle number per sample and polymer, over the files/operators 
                Min.particles.MM= min( Mean.particles.S),
                Max.particles.MM= max( Mean.particles.S),
                Mean.px.MM=mean( Mean.px.S),              # Mean Number of pixels per sample and polymer, over the files/operators
                Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2.S), #  Mean area per sample and polymer, over the files/operators
                Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.S),
                Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.S),
                Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.S),
                Min.Tot.Mass.ng.MM=min( Mean.Tot.Mass.ng.S),
                Max.Tot.Mass.ng.MM=max( Mean.Tot.Mass.ng.S)) %>%
      ungroup() 
    
    
    
    
    
    
    # 11. Make a field table ####
    # - With the Polymer12 (10+1)
    # - 2 size categories  (2)
    # - Number & Area (2)
    # -> +44 columns 
    colnames(Summary5b2_Field)

    
    Summary5b2_Field_wide=  Summary5b2_Field%>% 
      ungroup()%>%
      select(-c(Polymer.red3, N.soils, Mean.px.Fd,  Min.particles.Fd, Max.particles.Fd , Min.Tot.Area.mm2.Fd, Max.Tot.Area.mm2.Fd, Mean.Tot.Mass.ng.Fd)) %>% 
       pivot_wider( 
        names_from = c(Polymer.red12, Size_cat2.um),
        values_from = c(Mean.particles.Fd, Mean.Tot.Area.mm2.Fd)
      )
    
    colnames(Summary5b2_Field_wide)
    Summary5b2_Field_wide$CSS.Farm.Field= paste( Summary5b2_Field_wide$CSS, Summary5b2_Field_wide$Farm, Summary5b2_Field_wide$Field)
   # Add total uP
    col_MiPtotNum=c("Mean.particles.Fd_PA_300-2000"  , "Mean.particles.Fd_PA_90-300", "Mean.particles.Fd_PC_300-2000"   ,"Mean.particles.Fd_PC_90-300" ,  "Mean.particles.Fd_PE_300-2000" , "Mean.particles.Fd_PE_90-300" ,"Mean.particles.Fd_PET_300-2000", "Mean.particles.Fd_PET_90-300" , "Mean.particles.Fd_PLA_300-2000"     ,"Mean.particles.Fd_PLA_90-300" , "Mean.particles.Fd_PMMA_300-2000" , "Mean.particles.Fd_PMMA_90-300"      ,"Mean.particles.Fd_PP_300-2000", "Mean.particles.Fd_PP_90-300" ,"Mean.particles.Fd_PS_300-2000"     , "Mean.particles.Fd_PS_90-300","Mean.particles.Fd_PU_300-2000" ,"Mean.particles.Fd_PU_90-300", "Mean.particles.Fd_PVC_300-2000", "Mean.particles.Fd_PVC_90-300",  "Mean.particles.Fd_Other.Plastic_300-2000","Mean.particles.Fd_Other.Plastic_90-300")
    col_MiPtotArea=c("Mean.Tot.Area.mm2.Fd_PA_300-2000"  , "Mean.Tot.Area.mm2.Fd_PA_90-300", "Mean.Tot.Area.mm2.Fd_PC_300-2000"   ,"Mean.Tot.Area.mm2.Fd_PC_90-300" ,  "Mean.Tot.Area.mm2.Fd_PE_300-2000" , "Mean.Tot.Area.mm2.Fd_PE_90-300" ,"Mean.Tot.Area.mm2.Fd_PET_300-2000", "Mean.Tot.Area.mm2.Fd_PET_90-300" , "Mean.Tot.Area.mm2.Fd_PLA_300-2000"     ,"Mean.Tot.Area.mm2.Fd_PLA_90-300" , "Mean.Tot.Area.mm2.Fd_PMMA_300-2000" , "Mean.Tot.Area.mm2.Fd_PMMA_90-300"      ,"Mean.Tot.Area.mm2.Fd_PP_300-2000", "Mean.Tot.Area.mm2.Fd_PP_90-300" ,"Mean.Tot.Area.mm2.Fd_PS_300-2000"     , "Mean.Tot.Area.mm2.Fd_PS_90-300","Mean.Tot.Area.mm2.Fd_PU_300-2000" ,"Mean.Tot.Area.mm2.Fd_PU_90-300", "Mean.Tot.Area.mm2.Fd_PVC_300-2000", "Mean.Tot.Area.mm2.Fd_PVC_90-300",  "Mean.Tot.Area.mm2.Fd_Other.Plastic_300-2000","Mean.Tot.Area.mm2.Fd_Other.Plastic_90-300")
    
    Summary5b2_Field_wide$MiPtotNum=rowSums( Summary5b2_Field_wide[, col_MiPtotNum])
    Summary5b2_Field_wide$MiPtotArea=rowSums( Summary5b2_Field_wide[,  col_MiPtotArea])
    
    write.csv(Summary5b2_Field_wide, paste(wd.out,"/Summary5b2_Field_wide.csv", sep = ""))
      
    # Checks Summary5b2_Field_wide
   unique( abs(Summary5b2_Field_wide$MiPtotNum-Summary5e_Field$Mean.particles.Fd)<0.000000001)
    
    
    # Checks Plots
    
    ggplot( Summary5b2_Field_wide)+
      geom_point(aes(x=CSS.Farm.Field, y=`Mean.particles.Fd_PVC_90-300`*200))+
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 90) )
    
    ggplot( Summary5b2_Field_wide)+
      geom_point(aes(x=CSS.Farm.Field, y=`Mean.particles.Fd_PE_300-2000` *200))+
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 90) )
    
    ggplot( Summary5b2_Field_wide)+
      geom_point(aes(x=CSS.Farm.Field, y=`Mean.particles.Fd_PE_90-300`*200))+
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 90) )
      
    
    
    # Analusis test 
    
    #test=  scale( Summary5b2_Field_wide)

    
    
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
  
  Date=".csv"     # 2025.03.13
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
   
   write.csv(Summary7a_CSS, paste(wd.out,"/Summary7a_CSS", Date, sep = ""))
   write.csv(Summary7b_CSS, paste(wd.out,"/Summary7b_CSS", Date, sep = ""))
   write.csv(Summary7c_CSS, paste(wd.out,"/Summary7c_CSS", Date, sep = ""))
   write.csv(Summary7d_CSS, paste(wd.out,"/Summary7d_CSS", Date, sep = ""))
   write.csv(Summary7e_CSS, paste(wd.out,"/Summary7e_CSS", Date, sep = ""))
   #write.csv(Summary7g_CSS, paste(wd.out,"/Summary7g_CSS", Date, sep = ""))
   
   write.csv(Summary8a_MINAGRIS, paste(wd.out,"/Summary8a_MINAGRIS", Date, sep = ""))
   write.csv(Summary8b_MINAGRIS, paste(wd.out,"/Summary8b_MINAGRIS", Date, sep = ""))
   write.csv(Summary8c_MINAGRIS, paste(wd.out,"/Summary8c_MINAGRIS", Date, sep = ""))
   write.csv(Summary8d_MINAGRIS, paste(wd.out,"/Summary8d_MINAGRIS", Date, sep = ""))
   write.csv(Summary8e_MINAGRIS, paste(wd.out,"/Summary8e_MINAGRIS", Date, sep = ""))
   write.csv(Summary8f_MINAGRIS, paste(wd.out,"/Summary8f_MINAGRIS", Date, sep = ""))
   write.csv(Summary8g_MINAGRIS, paste(wd.out,"/Summary8g_MINAGRIS", Date, sep = ""))
   
   write.csv(Summary6e_Farm, paste(wd.out,"/Summary6e_Farm", Date, sep = ""))
  #write.csv(Summary6g_Farm, paste(wd.out,"/Summary8g_Farm", Date, sep = ""))
   #write.csv(Summary6g_Farm_outlier, paste(wd.out,"/Summary8g_Farm_outlier", Date, sep = ""))
   
   #write.csv(Summary7c_CSS_outlier, paste(wd.out,"/Summary7c_CSS_outlier", Date, sep = ""))
  # write.csv(Summary7e_CSS_outlier, paste(wd.out,"/Summary7e_CSS_outlier", Date, sep = ""))
   #write.csv(Summary7g_CSS_outlier, paste(wd.out,"/Summary7g_CSS_outlier", Date, sep = ""))
   
   
   # write.csv(Summary9a_Crop, paste(wd.out,"/Summary9a_Crop", Date, sep = ""))
   # write.csv(Summary9b_Crop, paste(wd.out,"/Summary9b_Crop", Date, sep = ""))
   # write.csv(Summary9c_Crop, paste(wd.out,"/Summary9c_Crop", Date, sep = ""))
   # write.csv(Summary9d_Crop, paste(wd.out,"/Summary9d_Crop", Date, sep = ""))
   # write.csv(Summary9e_Crop, paste(wd.out,"/Summary9e_Crop", Date, sep = ""))
   # write.csv(Summary9g_Crop, paste(wd.out,"/Summary9g_Crop", Date, sep = ""))
   # 
   
   
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
   CSS4F7F1_red_blank =subset(df_MiP, CSS==4 & Farm == 7 & Field == 1 )
   
   CSS4F7F1_Summary4e= subset(Summary4e_Soil, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_Summary3e= subset(Summary3e_Filter, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_WUR =subset(MiP_wur, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_WUR2 =subset(MiP_wur_cor, CSS==4 & Farm == 7 & Field == 1 )
   CSS4F7F1_WUR3 =subset(Data_WUR, CSS==4 & Farm == 7 & Field == 1 )
   
   # Check CSS4F2F1 ####
   CSS4F2F1=subset(Data_comb, CSS==4 & Farm == 2 & Field == 1 )
   CSS4F2F1_red =subset(Data_comb_red, CSS==4 & Farm == 2 & Field == 1 )
   CSS4F2F1_red_blank =subset(df_MiP, CSS==4 & Farm == 2 & Field == 1 )
   CSS4F2F1_WUR3 =subset(Data_WUR, CSS==4 & Farm == 2 & Field == 1 )
  
   
   
   
   # Check CSS11F8F1 ####
   CSS11F8F1 =subset(df_MiP, CSS==11 & Farm == 8 & Field == 1 )
   CSS11F8F1_wur =subset(Data_WUR, CSS==11 & Farm == 8 & Field == 1 )
   
   
   
   # Check CSS4F6F1 ####
   CSS4F6F1 =subset(df_MiP, CSS==4 & Farm == 6& Field == 1 )
   
   # Check CSS6F1F1 ####
   CSS6F1F1 =subset(df_MiP, CSS==6 & Farm ==1 & Field == 1 )
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
                        Tot.Mass.ng=0,
                        Median.Area.sqrt.um=0,
                        SD.Area=0)) %>%
     # Remove the "No.plastic", not needed anymore
     subset(Polymer.grp!="No.plastic")
   
   sum(Summary4a_CSS6F1F1$N.particles)
   # write.csv(  Summary4a_CSS6F6F1, paste(wd.out,"/Summary4a_CSS6F6F1", Date, sep = ""))
   
   
   
   # Check CSS6F2F1 ####
   CSS6F2F1 =subset(df_MiP, CSS==6 & Farm ==2 & Field == 1 )
   
   # Check CSS6F3F1 ####
   CSS6F3F1 =subset(df_MiP, CSS==6 & Farm ==3 & Field == 1 )
   
   # Check CSS6F8F1 ####
   CSS6F8F1 =subset(df_MiP, CSS==6 & Farm ==3 & Field == 1 )
   
   
    # Check CSS6F6F1 ####
   CSS6F6F1 =subset(df_MiP, CSS==6 & Farm ==6 & Field == 1 )
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
                        Tot.Mass.ng=0,
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
    CSS6F3F2 =subset(df_MiP, CSS==6 & Farm ==3 & Field == 2 )
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
    CSS6F6F2 =subset(df_MiP, CSS==6 & Farm ==6 & Field == 2 )
    
    # Check CSS11F10F2 ####
    CSS11F10F2 =subset(df_MiP, CSS==11 & Farm ==10 & Field == 2 )
    
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
    
    
    
    
   
    
    
        

