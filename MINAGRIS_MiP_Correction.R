# Script to  Load result table from Ubern and WUR ; Merge data of both labs ; Blank correction ;  Mass estimation ; "Other plastic correction"  create size categories 

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

# only use when RStudio project is not used
setwd("C:/Users/berio001/Minagris/MINAGRIS_Microplastic_Soil_Assessmnent")


wd.out= "Outputs" # W:/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Purency Microplastic Finder/PMF_Results_Summary"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"


# 1. Load MiP tables ####

# * From WUR ####
#MC - again I would remove dates from file names (and the dots in the date :)
#MC - alternative is to make a 'initialization file' where you update all your file names everytime, than the code stays flexible.
Data_WUR=read.csv("Outputs/WUR_MiP_Particles_20241128.csv")

# * From Ubern ####
wd.in.Ubern="UBern_Data"
Data_Ubern=read.csv(paste(wd.in.Ubern,"/Ubern_data_1124.csv", sep = "")) # "results_1024.csv")

# 2. Merge MiP tables ####

# Expected Column names
uP_Colnames=c("File_Names", "Lab", "Batch_Name", "Preparation_Type", "Sample_type", "Soil_sample", "Filter_name", "IR_rep", "PMF_rep", "Operator",
              "ID", "Q_index",	"Polymer.grp", "Polymer.red12", "Polymer.red3", "Area.um2.cor", "Length.um" , "Width.um", "Aspect_ratio", "Mass.ng", "Size_cat.um")

# Completing WUR data: 
uP_Colnames[uP_Colnames %!in% colnames(Data_WUR)]

Data_WUR$File_Names=Data_WUR$PMF_File_name
Data_WUR$PMF_rep="pmf"
Data_WUR$Q_index=Data_WUR$Relevance #OR Similarity
Data_WUR$Width.um =Data_WUR$Width.um.cor         
Data_WUR$Length.um=Data_WUR$Length.um.cor

# Completing Ubern data: 
uP_Colnames[uP_Colnames %!in% colnames(Data_Ubern)]

Data_Ubern$File_Names=Data_Ubern$source_file
Data_Ubern$Area.um2.cor=Data_Ubern$area
Data_Ubern$Filter_name=NA
Data_Ubern$Q_index=Data_Ubern$index_results/1000 # normalize over 1
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
#MC - unclear what the printed numbers below mean - add comment
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

# * Size range
min(subset (Data_comb, N.px >=1, select="Area.um2.cor"))
min(subset (Data_comb, N.px >=1, select="Length.um"))
  

# * Create size categories ####

# Custom size categories [um]:          
cat.min=90
cat.max=1350#1400 #1800
cat.bin=210
Cat.um=seq(cat.min, cat.max, by=cat.bin)
# Cat.um.txt=c("50-300", "300-550", "550-800",
#              "800-1050", "1050-1300", "1300-1550", "1550-1800")
# Cat.um.txt=c("80-300", "300-520", "520-740",
#              "740-960", "960-1180", "1180-1400")
 Cat.um.txt=c("90-300", "300-510", "510-720",
              "720-930", "930-1140", "1140-1350")



Data_comb$Size_cat.um="Too small"
# Build categories by successive replacement "upward"
for (c in 1:length(Cat.um)){
  Data_comb$Size_cat.um[Data_comb$Area.um2.cor>Cat.um[c]^2]=Cat.um.txt[c]
}

# Label the "Too small"
Data_comb[Data_comb$N.px>=1 & Data_comb$Length.um<86 & Data_comb$Width.um<86,]

# Extend the size categories to include until 86um in "90-300"
Data_comb$Size_cat.um[Data_comb$Size_cat.um=="Too small" & (Data_comb$Length.um>86 | Data_comb$Width.um>86) ]="90-300"
Data_comb$Size_cat.um[Data_comb$N.px>=1 & Data_comb$Length.um<86 & Data_comb$Width.um<86]="Too small"

# Label the "Too big"
Data_comb[Data_comb$N.px>=1 & Data_comb$Length.um>2000 & Data_comb$Width.um>2000,]
Data_comb$Size_cat.um[Data_comb$Area.um2.cor>cat.max^2]="Too big"

Data_comb[Data_comb$N.px>=1 & Data_comb$Size_cat.um %in% c("Too small", "Too big") ,]

 
# 3. Remove "MYSP" ####
# Remove the mysterious polymers 

Data_comb=subset(Data_comb, Polymer.grp != "MYSP")


# 3. Mass estimations ####

# /!\ Work in progress /!\
# Create a separate function ?





# 4. Analyse blanks ####
# Blank data frame (WUR) 
df_Blanks=subset(Data_comb, Soil_sample=="bcm"  ) # & Polymer.red12 != "Other.Plastic"

# /!\ "Other.Plastic" = No.plastic? 
# df_Blanks[df_Blanks$Polymer.red12=="Other.Plastic", ]
# df_Blanks[df_Blanks$Polymer.red12=="Other.Plastic", c("Polymer.grp","Polymer.red3","Polymer.red12")] <- "No.plastic"
# df_Blanks$N.px[df_Blanks$Polymer.red12== "No.plastic"]=0

# Summary / Blanks / Polymer

# *a. Sum up per IR File ####
Summary_Blanks1_File = df_Blanks %>% 
  group_by(File_Names, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_name, IR_rep, PMF_rep, Operator,  Polymer.grp, Polymer.red12,  Polymer.red3  ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
             Num.px=sum(N.px),           # Number of pixels
             Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
             SD.Area=sd(Area.um2.cor))

# *b. Mean per Filter (if in one batch, one filter, multiple IR files (IR_rep or Operator_rep)) ####
Summary_Blanks2_Filter =  Summary_Blanks1_File %>% 
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_name, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
             Num.px=mean(Num.px),           # Number of pixels
             Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             SD.Area=mean( SD.Area) )

# *c. Mean per Batch (if in one batch, multiple  filter, (Extract_rep))  ####   
Summary_Blanks3_Batch =   Summary_Blanks2_Filter %>%    
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
             Num.px=mean(Num.px),           # Number of pixels
             Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             SD.Area=mean( SD.Area) )

# write.csv(Summary_Blanks3_Batch, paste(wd.out,"Summary_Blanks_Batch_2024.11.13.csv",sep = "/"))


# *d. Sum all particles, from all polymers per Batch (if in one batch, multiple  filter, (Extract_rep)) ####
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


# *e. Sum all particles, from all polymers per Batch (if in one batch, multiple  filter, (Extract_rep)) ####
# /!\ "Other.Plastic" excluded /!\


Summary_Blanks5_Batch_SumPol12 =   Summary_Blanks3_Batch %>%
  # Group
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.particles),  # Number of particles (Sum of Binary)
             Num.px=sum(Num.px),           # Number of pixels
             Tot.Area.mm2=sum( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             SD.Area=mean( SD.Area) )
# write.csv( Summary_Blanks4_Batch_SumPol12, paste(wd.out,"Summary_Blanks_Batch_2024.11.13.csv",sep = "/"))



# * f. Overview of Matching quality of "Other.plastic" in Blanks ####
# "Other.plastics" are more rare, less known, not tested for recovery, and more susceptible to be mistaken to other polymers
# Therefore, we add a matching quality filter to reduce the risk of mis-identification and over estimation
# Set up for a matching quality filter at 0.35, which is reasonable from expert judgement and 
# conveniently (ad-hoc) removes two EVAc particles that would otherwise bring the m16 blank over the 5 particles threshold (ad-hoc). 

Out=subset(Data_comb, Q_index !=0 & Q_index <0.35 & Polymer.red12 =="Other.Plastic" )




# 5. Blank correction ####

# * Remove batches with more than 5 particles #### 

Summary_Blanks4_Batch_SumPol12$Batch_Name[ Summary_Blanks4_Batch_SumPol12$N.particles>5]

Data_comb_red=subset(Data_comb, Batch_Name %!in%  Summary_Blanks4_Batch_SumPol12$Batch_Name[ Summary_Blanks4_Batch_SumPol12$N.particles>5]  )


# * Remove "Other.plastics" with low Q_index (>0.35) ####
Data_comb_red=subset(Data_comb_red, Q_index ==0 | Q_index >0.35 | Polymer.red12 !="Other.Plastic")


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
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.particles),  # Number of particles (Sum of Binary)
             Num.px=sum(Num.px),           # Number of pixels
             Tot.Area.mm2=sum( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             SD.Area=mean( SD.Area) )

#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\


# * Plots blanks ####

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
Summary_Blanks3_Batch_red$Lab_Batch=paste(Summary_Blanks3_Batch_red$Lab, Summary_Blanks3_Batch_red$Batch_Name)
ggplot(  Summary_Blanks3_Batch_red, aes( Lab_Batch, N.particles)) +
  geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
  ggtitle("Particles summ per Polymer.red12 per batch")+
  scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                                "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                "CA"= "#FFD39B", "No.plastic"= "black"  ) ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
        axis.title.x = element_blank())


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
sum(subset(Summary_Blanks3_Batch_red,Polymer.red12  %!in% c( "PE", "PP", "No.plastic") , select="N.particles") ) / length(unique(df_Blanks_red$File_Names))


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
sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %!in% c( "PE", "PP", "No.plastic")& Lab=="WUR", select="N.particles" ) )/ nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="WUR"))


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
sum(subset(Summary_Blanks3_Batch_red, Polymer.red12 %!in% c("PE", "PP", "No.plastic")& Lab=="Ubern", select="N.particles" ) )/ nrow(subset(Summary_Blanks4_Batch_SumPol12_red, Lab=="Ubern"))



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


# 7. Export table ####  
length(unique(Data_comb_red_blank$File_Names))
uP_Colnames[uP_Colnames %!in% colnames(Data_comb_red_blank)]
length(unique(Data_comb_red_blank$File_Names[Data_comb_red_blank$Polymer.grp=="No.plastic"]))
length(unique(Data_comb$File_Names[Data_comb$Polymer.grp=="No.plastic"]))
length(unique(Data_comb$File_Names))


 write.csv(Data_comb_red_blank, paste(wd.out,"Corrected_MiP_Particles_20241127.csv",sep = "/"))

 write.csv(df_Blanks, paste(wd.out,"Blanks_Particles_20241127.csv",sep = "/"))








