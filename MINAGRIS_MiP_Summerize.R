# Script to  Load result table from Ubern and WUR ; Merge data of both labs ; Blank correction ;  ; Make summary table of any kind ; create size categories 

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

wd.out="W:/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Purency Microplastic Finder/PMF_Results_Summary"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"

# 1. Load MiP tables ####

  # * From WUR ####
    wd.in.WUR="W:/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Purency Microplastic Finder/PMF_Results_Summary"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"
    wd.in=wd.in.WUR
    setwd(wd.in)
  
    Data_WUR=read.csv("WUR_MiP_Particles_2024.11.13.csv")

  # * From Ubern ####
    wd.in.Ubern="//wurnet.nl/dfs-root/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/5_UbernData_092024"
    wd.in=wd.in.Ubern
    setwd(wd.in)
  
    Data_Ubern=read.csv("data_1029.csv")# "results_1024.csv")

    

# 2. Merge MiP tables ####
    
    # Expected Column names
    uP_Colnames=c("File_Names", "Lab", "Batch_Name", "Preparation_Type", "Sample_type", "Soil_sample", "Filter_name", "IR_rep", "PMF_rep", "Operator",
                  "ID", "Q_index",	"Polymer.grp", "Polymer.red12", "Polymer.red3", "Area.um2.cor", "Length.um" , "Width.um", "Aspect_ratio", "Mass.ng", "Size_cat.um")

    # Completing WUR data: 
    uP_Colnames[uP_Colnames %!in% colnames(Data_WUR)]
    
    Data_WUR$File_Names=Data_WUR$PMF_File_name
    Data_WUR$PMF_rep="pmf"
    Data_WUR$Q_index=Data_WUR$Similarity
    
    # Completing Ubern data: 
    uP_Colnames[uP_Colnames %!in% colnames(Data_Ubern)]
    
    Data_Ubern$File_Names=Data_Ubern$source_file
    Data_Ubern$Area.um2.cor=Data_Ubern$area
    Data_Ubern$Filter_name=NA
    Data_Ubern$Q_index=Data_Ubern$index_results
    Data_Ubern$Soil_sample=Data_Ubern$Soil.sample
    Data_Ubern$Lab="Ubern"
    Data_Ubern$Operator="AG"
    Data_Ubern$IR_rep="ir"
    Data_Ubern$PMF_rep="pmf"
    Data_Ubern$Width.um =Data_Ubern$width             
    Data_Ubern$Length.um=Data_Ubern$height
    Data_Ubern$Mass.ng=0
    Data_Ubern$Aspect_ratio=Data_Ubern$Length.um/Data_Ubern$Width.um
    Data_Ubern$Filter_div="0" 
    
    # Polymers
    Polymer.red12= c("PP", "PE",  "PS", "PET", "PVC", "PU", "PMMA", "PLA", "PC", "PA", "No.plastic") # 12 plastics , +Others  #"CA","silicone",
    Polymer.red3= c("PE", "PLA", "PVC","No.plastic") # PE, PLA, PVC, +Others
    Data_Ubern$Polymer.grp=Data_Ubern$New_identity
    Data_Ubern$N.px=0
    Data_Ubern$N.px[Data_Ubern$area>0]=1
    Data_Ubern$Polymer.grp[Data_Ubern$area==0]="No.plastic"
    Data_Ubern$Polymer.red12=Data_Ubern$Polymer.grp
    Data_Ubern$Polymer.red12[Data_Ubern$Polymer.grp %!in% Polymer.red12]="Other.Plastic"
    Data_Ubern$Polymer.red3=Data_Ubern$Polymer.grp
    Data_Ubern$Polymer.red3[Data_Ubern$Polymer.grp %!in% Polymer.red3]="Other.Plastic"

    # * Merge ####
    Data_comb=rbind(subset(Data_WUR, select = colnames(Data_WUR)[colnames(Data_WUR)%in% colnames(Data_Ubern)]),
                    subset(Data_Ubern, select = colnames(Data_Ubern)[colnames(Data_Ubern)%in% colnames(Data_WUR)]  ) )
    Data_comb$Preparation_Type="Not"
    Data_comb$Preparation_Type[ Data_comb$Sample_type %in% c("n","r")]="Field_samples"
    Data_comb$Preparation_Type[ Data_comb$Sample_type %in% c("s","s2")] = "Spiked"
    Data_comb$Preparation_Type[ Data_comb$Soil_sample=="bcm"]="Blank_chemical"
    Data_comb$Preparation_Type[ Data_comb$Soil_sample %in% c( "RS", "pfsr","rs")  ]="Reference_Soil"
    Data_comb$Preparation_Type[ Data_comb$Soil_sample %in% c( "st")  ]="Standard_Soil"
    
    Data_comb[Data_comb$Preparation_Type=="Not",]
    
    # Re-label Soil_sample:
    nrow( subset(Data_comb, Soil_sample=="bcm"))
    nrow( subset(Data_WUR, Soil_sample=="bcm"))
    nrow( subset(Data_Ubern, Soil_sample=="bcm"))
    nrow( subset(Data_comb, Preparation_Type=="Blank_chemical"))
    
    Soil_sample_x= Data_comb$Soil_sample
    Data_comb$Soil_sample[Data_comb$Lab=="WUR"]=paste( Data_comb$CSS[Data_comb$Lab=="WUR"], ".",
                                                       Data_comb$Farm[Data_comb$Lab=="WUR"],".",
                                                       Data_comb$Field[Data_comb$Lab=="WUR"],
                                                       "_S1", sep = "")
    Data_comb$Soil_sample[Data_comb$Lab=="Ubern"]=paste( Data_comb$CSS[Data_comb$Lab=="Ubern"], ".",
                                                       Data_comb$Farm[Data_comb$Lab=="Ubern"],".",
                                                       Data_comb$Field[Data_comb$Lab=="Ubern"],
                                                       "_S2", sep = "")
    
    Data_comb$Soil_sample[Data_comb$Preparation_Type=="Blank_chemical"]= "bcm"
    Data_comb$Soil_sample[Data_comb$Preparation_Type=="Reference_Soil"]= "rs"
    Data_comb$Soil_sample[Data_comb$Preparation_Type=="Standard_Soil"]= "st"
    
    nrow( subset(Data_comb, Soil_sample=="bcm"))
    
    
    # Re-label Filter_name:
    Data_comb$Filter_name=paste(Data_comb$Batch_Name, Data_comb$Soil_sample, Data_comb$Sample_type, Data_comb$Filter_div, sep = "_")
    
    
    # Add a unique ID per particle 
    Data_comb$ID=seq_along(Data_comb$File_Names)
    
    # * Create size categories ####
      # Custom size categories [um]:          
      cat.min=50
      cat.max=1550#1800
      cat.bin=250
      Cat.um=seq(cat.min, cat.max, by=cat.bin)
      Cat.um.txt=c("50-300", "300-550", "550-800",
                   "800-1050", "1050-1300", "1300-1550", "1550-1800")
      
      Data_comb$Size_cat.um="Too small"
      # Build categories by succesive replacement "upward"
      for (c in 1:length(Cat.um)){
        Data_comb$Size_cat.um[Data_comb$Area.um2.cor>Cat.um[c]^2]=Cat.um.txt[c]
      }
      
      # Exclude the "Too small" and "Too big"
      Data_comb$Size_cat.um[Data_comb$Area.um2.cor<cat.min^2]="Too small"
      Data_comb$Size_cat.um[Data_comb$Area.um2.cor>cat.max^2]="Too big"
      
    
      uP_Colnames[uP_Colnames %!in% colnames(Data_comb)]
      
      
# 3. Mass estimations ####
    # Create a separate function ?
    
    

    
    
# 4. Analyse blanks ####
    # Blank data frame (WUR) 
    df_Blanks=subset(Data_comb, Soil_sample=="bcm"  ) # & Polymer.red12 != "Other.Plastic"
    
    # /!\ "Other.Plastic" = No.plastic? 
    # df_Blanks[df_Blanks$Polymer.red12=="Other.Plastic", ]
    # df_Blanks[df_Blanks$Polymer.red12=="Other.Plastic", c("Polymer.grp","Polymer.red3","Polymer.red12")] <- "No.plastic"
    # df_Blanks$N.px[df_Blanks$Polymer.red12== "No.plastic"]=0
    
    # Summary / Blanks / Polymer
    
    # 1. Sum up per IR File 
    Summary_Blanks1_File = df_Blanks %>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_name, IR_rep, PMF_rep, Operator,  Polymer.grp, Polymer.red12,  Polymer.red3  ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 #Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))
    
    # 2. Mean per Filter (if in one batch, one filter, multiple IR files (IR_rep or Operator_rep))
    Summary_Blanks2_Filter =  Summary_Blanks1_File %>% 
      group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_name, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
      summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
                 Num.px=mean(Num.px),           # Number of pixels
                 Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
                 #Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
                 SD.Area=mean( SD.Area) )
      
    # 3. Mean per Batch (if in one batch, multiple  filter, (Extract_rep))    
      Summary_Blanks3_Batch =   Summary_Blanks2_Filter %>%    
    group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
        summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
                 Num.px=mean(Num.px),           # Number of pixels
                 Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
                 #Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
                 SD.Area=mean( SD.Area) )
    
      # write.csv(Summary_Blanks3_Batch, paste(wd.out,"Summary_Blanks_Batch_2024.11.13.csv",sep = "/"))
      
      
    # 4. Sum all particles, from all polymers per Batch (if in one batch, multiple  filter, (Extract_rep))
      # /!\ "Other.Plastic" excluded /!\
      
      
      Summary_Blanks4_Batch_SumPol12 =   Summary_Blanks3_Batch %>%
        # "Other.Plastic" to 0
        mutate(N.particles = if_else(Polymer.red12 == "Other.Plastic", 0, N.particles),
               Tot.Area.mm2= if_else(Polymer.red12 == "Other.Plastic", 0,  Tot.Area.mm2),
               #Mass.ng = if_else(Polymer.red12 == "Other.Plastic", 0,  Tot.Area.mm2),
               Median.Area.sqrt.um= if_else(Polymer.red12 == "Other.Plastic", 0,  Median.Area.sqrt.um),
               SD.Area= if_else(Polymer.red12 == "Other.Plastic", 0,  SD.Area) ) %>%
        # Group
        group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
        summarise( N.particles= sum(N.particles),  # Number of particles (Sum of Binary)
                   Num.px=sum(Num.px),           # Number of pixels
                   Tot.Area.mm2=sum( Tot.Area.mm2), # Total plastic area 
                   #Tot.Mass.ng=sum( Mass.ng),
                   Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
                   SD.Area=mean( SD.Area) )
    # write.csv( Summary_Blanks4_Batch_SumPol12, paste(wd.out,"Summary_Blanks_Batch_2024.11.13.csv",sep = "/"))
    

# 5. Blank correction ####

    # * Remove batches with more than 5 particles #### 
    
      Summary_Blanks4_Batch_SumPol12$Batch_Name[ Summary_Blanks4_Batch_SumPol12$N.particles>5]
    
    Data_comb_red=subset(Data_comb, Batch_Name %!in%  Summary_Blanks4_Batch_SumPol12$Batch_Name[ Summary_Blanks4_Batch_SumPol12$N.particles>5]  )
    
    # * Characterize the remaining contamination #### 
    df_Blanks_red=subset(Data_comb_red, Soil_sample=="bcm" )
    
   
    # Summary / Blanks / Polymer
    
    # 1. Sum up per IR File 
    Summary_Blanks1_File_red =  df_Blanks_red%>% 
      group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_name, IR_rep, PMF_rep, Operator,  Polymer.grp, Polymer.red12,  Polymer.red3  ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
                 #Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))
    
    # 2. Mean per Filter (if in one batch, one filter, multiple IR files (IR_rep or Operator_rep))
    Summary_Blanks2_Filter_red =  Summary_Blanks1_File_red %>% 
      group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_name, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
      summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
                 Num.px=mean(Num.px),           # Number of pixels
                 Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
                 #Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
                 SD.Area=mean( SD.Area) )
    
    # 3. Mean per Batch (if in one batch, multiple  filter, (Extract_rep))    
    Summary_Blanks3_Batch_red =   Summary_Blanks2_Filter_red %>%    
      group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
      summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
                 Num.px=mean(Num.px),           # Number of pixels
                 Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
                 #Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
                 SD.Area=mean( SD.Area) )
    
    # 4. Sum all particles, all polymers, per Batch (if in one batch, multiple  filter, (Extract_rep))  
    # /!\ "Other.Plastic" excluded /!\
    
    Summary_Blanks4_Batch_SumPol12_red=   Summary_Blanks3_Batch_red %>%
      # "Other.Plastic" to 0
      mutate(N.particles = if_else(Polymer.red12 == "Other.Plastic", 0, N.particles),
             Tot.Area.mm2= if_else(Polymer.red12 == "Other.Plastic", 0,  Tot.Area.mm2),
             #Mass.ng = if_else(Polymer.red12 == "Other.Plastic", 0,  Tot.Area.mm2),
             Median.Area.sqrt.um= if_else(Polymer.red12 == "Other.Plastic", 0,  Median.Area.sqrt.um),
             SD.Area= if_else(Polymer.red12 == "Other.Plastic", 0,  SD.Area) ) %>%
      # Group
      group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
      summarise( N.particles= sum(N.particles),  # Number of particles (Sum of Binary)
                 Num.px=sum(Num.px),           # Number of pixels
                 Tot.Area.mm2=sum( Tot.Area.mm2), # Total plastic area 
                 #Tot.Mass.ng=sum( Mass.ng),
                 Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
                 SD.Area=mean( SD.Area) )
    
    #\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
    
    
    # * Plots blanks
    
    # All batches, Polymer12
    ggplot( Summary_Blanks3_Batch, aes(Lab, N.particles)) +
      geom_jitter(height=0,width=0.25, aes(color=Polymer.red12 ))+
      ggtitle("Particles summ per Polymer.red12 per batch")+
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                                   "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B" , "No.plastic"= "black"   ) ) +
      theme_minimal()
    
    # All batches, Polymer12
      Summary_Blanks3_Batch$Lab_Batch=paste(Summary_Blanks3_Batch$Lab, Summary_Blanks3_Batch$Batch_Name)
      ggplot(  Summary_Blanks3_Batch, aes( Lab_Batch, N.particles)) +
      geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
      ggtitle("Particles summ per Polymer.red12 per batch")+
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                    "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                                    "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                    "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                    "CA"= "#FFD39B", "No.plastic"= "black"  ) ) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    
    # per Lab
      ggplot( Summary_Blanks4_Batch_SumPol12, aes( Lab, N.particles) ) +
      geom_jitter(height=0,width=0.25)+
      ggtitle("All Polymer.red12 summed per batch")+
      theme_minimal()

    # Number and median area of plastic particle per blank:

    # Both lab: 
      # Number of Plastic particle per blank:
      mean(Summary_Blanks4_Batch_SumPol12_red$N.particles) 
      
      # Number and median area of PP particle per blank:
      sum(subset(Summary_Blanks3_Batch_red,Polymer.grp %in% c("PP") , select="N.particles") ) / length(unique(df_Blanks_red$File_Names))
      
      PP_Areamedian=median(df_Blanks_red$Area.um2.cor[grep("PP",df_Blanks_red$Polymer.grp)])
      
      
      # Number and median area of PE particle per blank:
      sum(subset(Summary_Blanks3_Batch_red,Polymer.grp %in% c("PE") , select="N.particles") ) / length(unique(df_Blanks_red$File_Names))
      PE_Areamedian=median(df_Blanks_red$Area.um2.cor[grep("PE",df_Blanks_red$Polymer.red12)])     
      median(subset(df_Blanks_red, Polymer.red12 %in% c("PE") , select="Area.um2.cor")[[1]])
      
      
      # Number rest of plastic particle per blank:
      sum(subset(Summary_Blanks3_Batch_red,Polymer.red12  %!in% c("Other.Plastic", "PE", "PP", "No.plastic") , select="N.particles") ) / length(unique(df_Blanks_red$File_Names))
    
      
    # WUR:  
      # Number of Plastic particle per blank:
      mean(Summary_Blanks4_Batch_SumPol12_red$N.particles[Summary_Blanks4_Batch_SumPol12_red$Lab=="WUR"]) 
  
      
      # Number and median area of PP particle per blank:
      sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %in% c("PP") & Lab=="WUR", select="N.particles") ) / nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="WUR"))
      PP_WUR_Areamedian=median(subset(df_Blanks_red, Polymer.red12 %in% c("PP") & Lab=="WUR", select="Area.um2.cor")[[1]])
      
      # Number and median area of PE particle per blank:
      sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %in% c("PE") & Lab=="WUR", select="N.particles") ) / nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="WUR"))
      PE_WUR_Areamedian=median(subset(df_Blanks_red, Polymer.red12 %in% c("PE") & Lab=="WUR", select="Area.um2.cor")[[1]])
      
      
      # Number rest of plastic particle per blank:
      sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %!in% c("Other.Plastic", "PE", "PP", "No.plastic")& Lab=="WUR", select="N.particles" ) )/ nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="WUR"))
      
      
    # UBern:  
      # Number of Plastic particle per blank:
      mean(Summary_Blanks4_Batch_SumPol12_red$N.particles[Summary_Blanks4_Batch_SumPol12_red$Lab=="Ubern"]) 
      
      
      # Number and median area of PP particle per blank:
      sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %in% c("PP") & Lab=="Ubern", select="N.particles") ) / nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="Ubern"))
      PP_Ubern_Areamedian=median(subset(df_Blanks_red, Polymer.red12 %in% c("PP") & Lab=="Ubern", select="Area.um2.cor")[[1]])
      
      # Number and median area of PE particle per blank:
      sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %in% c("PE") & Lab=="Ubern", select="N.particles") ) / nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="Ubern"))
      PE_Ubern_Areamedian=median(subset(df_Blanks_red, Polymer.red12 %in% c("PE") & Lab=="Ubern", select="Area.um2.cor")[[1]])
      
      
      # Number rest of plastic particle per blank:
      sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %!in% c("Other.Plastic", "PE", "PP", "No.plastic")& Lab=="Ubern", select="N.particles" ) )/ nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="Ubern"))
      
    
    
    # * Calculate correction ####
    Data_comb_red_blank= Data_comb_red
    # For Ubern: 
      # 1. If there is PE, remove 1 PE particle with the closest area from the median:
      # 2. If no PE, remove one PP particle with the closest area from the median:
      # 3. If no PE, no PP don't do any thing
      
    # For WUR: 
      # 1. If there is PP, remove 1 PP particle with the closest area from the median:
      # 2. If no PP, remove one PE particle with the closest area from the median:
      # 3. If no PE, no PP don't do any thing
      
      
    # Start For loop per sample: 
    for (s in unique(Data_comb_red_blank$File_Names)){
      # Find all the particles in sample s: 
      Sample=Data_comb_red_blank[Data_comb_red_blank$File_Names==s,]

      # For Ubern: 
      if (unique(Sample$Lab)=="Ubern"){
      # 1. If there is a PE particle, remove a PE,          
        if ("PE" %in% Sample$Polymer.grp){
          # Find the PE particle with the closest area from the median.
          # Create a df of all PE particles
          PE=Sample[Sample$Polymer.grp=="PE",]
          # Calculate the absolute difference with PE_Ubern_Areamedian for each PE particle 
          PE$Median_diff=0
          for (p in 1:nrow(PE)){
            PE$Median_diff[p]=abs(PE$Area.um2.cor[p]-PE_Ubern_Areamedian)
          }
          # ID of the FIRST[1] particle with the min Median_diff:
          ID_PE=PE$ID[PE$Median_diff==min(PE$Median_diff)][1]
          # remove this particle  
          Data_comb_red_blank=subset(Data_comb_red_blank, ID!= ID_PE ) 
          
      # 2. If there is no PE particle but PP, remove a PP,    
        } else if  ("PP" %in% Sample$Polymer.grp){ 
          #Find the PE particle with the closest area from the median:
          PP=Sample[Sample$Polymer.grp=="PP",]
          # Calculate the absolute difference with PP_Areamedian for each PE particle 
          PP$Median_diff=0
          for (p in 1:nrow(PP)){
            PP$Median_diff[p]=abs(PP$Area.um2.cor[p]- PP_Ubern_Areamedian)
          }
          # ID of the FIRST[1] particle with the min Median_diff:
          ID_PP=PP$ID[PP$Median_diff==min(PP$Median_diff)][1]
          # remove this particle  
          Data_comb_red_blank=subset(Data_comb_red_blank, ID!= ID_PP ) 
        }
      # 3. If no PE, no PP don't do any thing
      } # end if UBern
      
      
      # For WUR:  
      if (unique(Sample$Lab)=="WUR"){
      # 1. If there is a PP particle, remove a PP,      
        if ("PP" %in% Sample$Polymer.grp){
          #Find the PP particle with the closest area from the median:
          PP=Sample[Sample$Polymer.grp=="PP",]
          # Calculate the absolute difference with PP_Areamedian for each PP particle 
          PP$Median_diff=0
          for (p in 1:nrow(PP)){
            PP$Median_diff[p]=abs(PP$Area.um2.cor[p]-PP_WUR_Areamedian)
          }
          # ID of the FIRST[1] particle with the min Median_diff:
          ID_PP=PP$ID[PP$Median_diff==min(PP$Median_diff)][1]
          # remove this particle  
          Data_comb_red_blank=subset(Data_comb_red_blank, ID!= ID_PP ) 
   
      # 2. If there is no PP particle but PE, remove a PE,        
        } else if  ("PE" %in% Sample$Polymer.grp){ # If there is no PP particle, but a PE 
          #Find the PE particle with the closest area from the median:
          PE=Sample[Sample$Polymer.grp=="PE",]
          # Calculate the absolute difference with PE_Areamedian for each PE particle 
          PE$Median_diff=0
          for (p in 1:nrow(PE)){
            PE$Median_diff[p]=abs(PE$Area.um2.cor[p]-PE_WUR_Areamedian)
          }
          # ID of the FIRST[1] particle with the min Median_diff:
          ID_PE=PE$ID[PE$Median_diff==min(PE$Median_diff)][1]
          # remove this particle  
          Data_comb_red_blank=subset(Data_comb_red_blank, ID!= ID_PE ) 
        }
      # 3. If no PE, no PP don't do any thing    
      } # end if WUR
    } # end loop Samples
    
   
    
# 6. Export table ####  
    length(unique(Data_comb_red_blank$File_Names))
    uP_Colnames[uP_Colnames %!in% colnames(Data_comb_red_blank)]
    length(unique(Data_comb_red_blank$File_Names[Data_comb_red_blank$Polymer.grp=="No.plastic"]))
    length(unique(Data_comb$File_Names[Data_comb$Polymer.grp=="No.plastic"]))
    length(unique(Data_comb$File_Names))
    # write.csv(Data_comb_red_blank, paste(wd.out,"Corrected_MiP_Particles_20241113.csv",sep = "/"))
    
    


    
# 7. Summaries ####
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

        
  # .a: all factors
  # .b: per Polymer.red12 * per Size_cat.um
  # .c: per Polymer.red12
  # .d: sum up all polymers, "Other.Plastic" excluded 
    

    
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
    
    
    
    # Check Total particles numbers
    sum(Summary1e_File$N.particles)
    sum(Summary1d_File$N.particles)
    sum(Summary1c_File$N.particles)
    sum(Summary1b_File$N.particles)
    sum(Summary1a_File$N.particles)
    length(Data_comb_red_blank$N.px[Data_comb_red_blank$N.px>0])
    
    
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
    
    # Check Total particles numbers
    sum(Summary3e_Filter$Mean.particles)
    sum(Summary3d_Filter$Mean.particles)
    sum(Summary3c_Filter$Mean.particles)
    sum(Summary3b_Filter$Mean.particles)
    sum(Summary3a_Filter$Mean.particles)

    
    # //\\ \\// Preparation_Type=="Field_samples" \\// //\\####

    
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
    
    
    
    
    
    
    # * 5a Mean per Field, all factors ####
    Summary5a_Field= Summary4a_Soil%>% 
      group_by(Preparation_Type, 
               CSS, Farm, Field, 
               Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  )  %>% # For each PMF_File_name, get the summary
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
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
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles= min( Mean.particles),
                 Max.particles= max( Mean.particles),
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) ) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
    
    # * 5d sum up all polymers, "Other.Plastic" excluded  ####
    
    Summary5d_Field= Summary4d_Soil %>% 
      group_by(Preparation_Type,  
               CSS, Farm, Field ) %>% # For each PMF_File_name, get the summary
      
      summarise( N.files = n(),
                 Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
                 Min.particles= min( Mean.particles),
                 Max.particles= max( Mean.particles),
                 Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
                 Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
                 Min.Tot.Area.mm2=min(Mean.Tot.Area.mm2),
                 Max.Tot.Area.mm2=max(Mean.Tot.Area.mm2),
                 Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
      ) 
    
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
   
    
   # * Export ####
  
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
   write.csv(Summary2e_File, paste(wd.out,"/Summary2e_File", Date, sep = ""))
   write.csv(Summary3e_File, paste(wd.out,"/Summary3e_File", Date, sep = ""))
   write.csv(Summary4e_File, paste(wd.out,"/Summary4e_File", Date, sep = ""))
   write.csv(Summary5e_File, paste(wd.out,"/Summary5e_File", Date, sep = ""))
   
   
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

    
    
        

