# Script to  Load result table from Ubern and WUR ; Merge data of both labs ; Blank correction ;  Mass estimation ; "Other plastic correction"  create size categories 

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

# 1. Load MiP tables ####

# * From WUR ####
#MC - again I would remove dates from file names (and the dots in the date :)
#MC - alternative is to make a 'initialization file' where you update all your file names everytime, than the code stays flexible.
Data_WUR=read.csv(paste("WUR_MiP_Particles.csv", sep=""))
# Number of particles: 
nrow(Data_WUR[Data_WUR$N.px>0,])
# Number of files: 
length(unique(Data_WUR$PMF_File_name))

# * From Ubern ####
wd.in.Ubern="UBern_Data"
# Data_Ubern=read.csv(paste(wd.in.Ubern,"/Ubern_data_1124.csv", sep = "")) # "results_1024.csv")
Data_Ubern=read.csv(paste(wd.in.Ubern,"/Ubern_data_2504.csv", sep = "")) # "results_1024.csv")
head(Data_Ubern)
# Number of particles: 
nrow(Data_Ubern[Data_Ubern$area>0,])
# Number of files: 
length(unique(Data_Ubern$source_file))
(unique(Data_Ubern$source_file))
colnames(Data_Ubern)



# 2. Merge MiP tables ####

# Expected Column names
uP_Colnames=c("File_Name", "Lab", "Batch_Name", "Preparation_Type", "Sample_type", "Soil_sample", "Filter_name", "IR_rep", "PMF_rep", "Operator",
              "ID", "Q_index",	"Polymer.grp", "Polymer.red12", "Polymer.red3", "Area.um2.cor", "Length.um" , "Width.um", "Aspect_ratio", "Mass.ng", "Size_cat.um")

# * Completing from WUR data #### 
uP_Colnames[uP_Colnames %!in% colnames(Data_WUR)]


#MC - in the file you send me these names were already correct, and running these lines caused an error!!
colnames(Data_WUR)

Data_WUR$PMF_rep="pmf"
Data_WUR$Q_index=Data_WUR$Relevance #OR Similarity
Data_WUR$Width.um =Data_WUR$Width.um.cor         
Data_WUR$Length.um=Data_WUR$Length.um.cor

Data_WUR$File_Name= Data_WUR$PMF_File_name
unique(Data_WUR$Filter_Name[Data_WUR$Soil_sample=="rs"])
unique(Data_WUR$Filter_name[Data_WUR$Soil_sample=="rs"])

unique(Data_WUR$Filter_Name)
unique(Data_WUR$Filter_div)
unique(Data_WUR$Soil_sample)


# * Completing Ubern data ####
uP_Colnames[uP_Colnames %!in% colnames(Data_Ubern)]

Data_Ubern$File_Name=Data_Ubern$source_file
Data_Ubern$Area.um2.cor=Data_Ubern$area

#Filter names 
  Data_Ubern$Filter_Name=Data_Ubern$FileName # /!\ Assuming no IR_rep and no Operator_rep
  
  # Find the samples associated with multiple filters:
  unique(Data_Ubern$source_file)[grep("_F", unique(Data_Ubern$source_file))]
  # "M22111001" and "M22111002" /!\ Manual selection!
  Data_Ubern$Filter_div="0" 
  for (sample in c("M22111001", "M22111002")){
    f=1
    for ( file in (unique( Data_Ubern$source_file[grep(sample,Data_Ubern$source_file)])) ){
      Data_Ubern$Filter_div[Data_Ubern$source_file==file]=f
      f=f+1
    }
  }
  Data_Ubern$Filter_div[grep("M22111001",Data_Ubern$source_file)]
  Data_Ubern$Filter_div[grep("M22111002",Data_Ubern$source_file)]
  
Data_Ubern$Q_index=Data_Ubern$rsq


Data_Ubern$Soil_sample=Data_Ubern$Soil.sample
Data_Ubern$Lab="Ubern"
Data_Ubern$Operator="AG"
Data_Ubern$IR_rep="ir"
Data_Ubern$PMF_rep="pmf"
Data_Ubern$Width.um = pmin(Data_Ubern$height, Data_Ubern$width)         
Data_Ubern$Length.um= pmax(Data_Ubern$height, Data_Ubern$width) 
Data_Ubern$Mass.ng=0
Data_Ubern$Aspect_ratio=Data_Ubern$Width.um/Data_Ubern$Length.um

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

# Replacing NA with 0px for "no plastic 
subset(Data_Ubern,Polymer.grp=="No.plastic")

Data_Ubern = Data_Ubern %>%
  mutate(N.px =         if_else( Polymer.grp=="No.plastic", 0, N.px),
         Area.um2.cor = if_else(Polymer.grp=="No.plastic", 0, Area.um2.cor),
         Mass.ng =      if_else( Polymer.grp=="No.plastic", 0, Mass.ng),
         Length.um =if_else( Polymer.grp=="No.plastic", 0, Length.um),
         Width.um = if_else( Polymer.grp=="No.plastic", 0, Width.um ))

nrow(subset(Data_Ubern, is.na(Area.um2.cor) ))

# * Filtering Ubern particles ####
nrow(subset(Data_Ubern, N.px !=0 ))

# Filter out Small particles  
Filtered_out_Data_Ubern=subset(Data_Ubern, N.px !=0 & Width.um<86 )
nrow(subset(Data_Ubern, N.px !=0 & Width.um<86 ) )

# Percentage removed 
nrow(subset(Data_Ubern, N.px !=0 & Width.um<86 ) ) / nrow(subset(Data_Ubern, N.px !=0 ) ) *100


# Mutate the N.px to 0 if the Length.um.cor<86 um
Data_Ubern_cor = Data_Ubern %>%
  mutate(N.px =         if_else( Width.um <86, 0, N.px),
         Area.um2.cor = if_else( Width.um <86, 0, Area.um2.cor),
         Mass.ng =      if_else( Width.um <86, 0, Mass.ng),
         Length.um =if_else( Width.um <86, 0, Length.um),
         Width.um = if_else( Width.um <86, 0, Width.um ))

nrow(subset(Data_Ubern_cor, N.px !=0 ))

# Filter out big particles  
# Based on the smallest dimension (Width.um>2000)
nrow(subset(Data_Ubern_cor, Width.um>2000 ) )

Filtered_out_Data_Ubern=rbind(Filtered_out_Data_Ubern, subset(Data_Ubern_cor, Width.um>2000 ) )


# Mutate the N.px to 0 if the Length.um.cor<86 um
Data_Ubern_cor = Data_Ubern_cor %>%
  mutate(N.px =    if_else( Width.um>2000, 0, N.px),
         Area.um2.cor = if_else( Width.um>2000, 0, Area.um2.cor),
         Mass.ng =      if_else( Width.um>2000, 0, Mass.ng),
         Length.um =if_else( Width.um>2000, 0, Length.um),
         Width.um = if_else( Width.um>2000, 0, Width.um ))

nrow(subset(Data_Ubern_cor, N.px !=0 ))
nrow(subset(Data_Ubern, N.px !=0 ))

# * Merge (Data_comb) ####
Data_comb=rbind(subset(Data_WUR, select = colnames(Data_WUR)[colnames(Data_WUR)%in% colnames(Data_Ubern_cor)]),
                subset(Data_Ubern_cor, select = colnames(Data_Ubern_cor)[colnames(Data_Ubern_cor)%in% colnames(Data_WUR)]  ) )

# Number of particles: 
nrow(Data_comb[Data_comb$Area.um2.cor>0,])
# Number of files: 
length(unique(Data_comb$File_Name))

# Add Preparation Type description
Data_comb$Preparation_Type="Not"
Data_comb$Preparation_Type[ Data_comb$Sample_type %in% c("n","r")]="Field_samples"
Data_comb$Preparation_Type[ Data_comb$Sample_type %in% c("s","s2")] = "Spiked"
Data_comb$Preparation_Type[ Data_comb$Soil_sample=="bcm"]="Blank_chemical"
Data_comb$Preparation_Type[ Data_comb$Soil_sample %in% c( "RS", "pfsr","rs")  ]="Reference_Soil"
Data_comb$Preparation_Type[ Data_comb$Soil_sample %in% c( "st")  ]="Standard_Soil"

Data_comb[Data_comb$Preparation_Type=="Not",]


# Re-label Soil_sample:
# MC - unclear what the printed numbers below mean - add comment
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

# Add the CSS in Soil sample description 
Data_comb$Soil_sample[Data_comb$Preparation_Type=="Reference_Soil"]=paste("rs", "_", Data_comb$CSS[Data_comb$Preparation_Type=="Reference_Soil"], sep = "")
Data_comb$Soil_sample[Data_comb$Soil_sample=="rs_-1" ]="pfsr"
Data_comb$Preparation_Type[Data_comb$Soil_sample=="pfsr" ]="Blank_soil"
Data_comb$Sample_type[Data_comb$Preparation_Type=="Reference_Soil"]="n"

# Re-label Filter_Name:
Data_comb$Filter_Name=paste(Data_comb$Batch_Name, "_", Data_comb$Soil_sample, "_", Data_comb$Sample_type, "_", Data_comb$Filter_div, sep = "")


# Add a unique ID per particle 
Data_comb$ID=seq_along(Data_comb$File_Name)

# Add Extraction_Name: 
  # Name of the extracted soil (note: one extracted soil can be divided in several filters when needs be, e.g CSS11.10.1 )
  # Filter_Name = Batch_"Soil_sample"_"Sample_type"_"Filter_div"
  # Extraction_Name = Batch_"Soil_sample"_"Sample_type"

Data_comb$Extraction_Name= paste(Data_comb$Batch_Name, "_",
                                 Data_comb$Soil_sample,"_",
                                 Data_comb$Sample_type, sep="")

# * Size range ####
min(subset (Data_comb, N.px >=1, select="Area.um2.cor"))
min(subset (Data_comb, N.px >=1, select="Length.um"))

SMALL=subset (Data_comb, N.px >=1)[with(subset (Data_comb, N.px >=1),order(Width.um)),] [1:20,]
BIG= Data_comb[with(Data_comb,order(-Width.um)),] [1:20,]
BIG= Data_comb[with(Data_comb,order(-Width.um)),] [1:20,]

write.csv(SMALL, paste(wd.out,"SMALL_comb_Particles.csv",sep = "/"))
write.csv(BIG, paste(wd.out,"BIG_comb_Particles.csv",sep = "/"))

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
              "720-930", "930-1140", "1140-2000")

Data_comb$Size_cat.um="Too small"
# Build categories by successive replacement "upward"
for (c in 1:length(Cat.um)){
  Data_comb$Size_cat.um[Data_comb$Area.um2.cor>Cat.um[c]^2]=Cat.um.txt[c]
}

# Label the "Too small"
Data_comb[Data_comb$N.px>=1 & Data_comb$Width.um<86,]

# Extend the size categories to include until 86um in "90-300"
Data_comb$Size_cat.um[Data_comb$Size_cat.um=="Too small" &  Data_comb$Width.um>86 ]="90-300"
Data_comb$Size_cat.um[Data_comb$N.px>=1 & Data_comb$Width.um<86]="Too small"

# Extend the size categories to include until 2000 um in "1140-2000"
Data_comb$Size_cat.um[Data_comb$Area.um2.cor>Cat.um[c]^2  &  Data_comb$Width.um<2000 ]="1140-2000"

# Label the "Too big"
Data_comb[Data_comb$Width.um>2000,]
Data_comb$Size_cat.um[Data_comb$Width.um>2000]="Too big"

Data_comb[Data_comb$N.px>=1 & Data_comb$Size_cat.um %in% c("Too small", "Too big") ,]

# check for NA
subset(Data_comb, is.na(Size_cat.um))


# * Add binary size categories ####
Cat.um.txt2=c("90-300", "300-2000")

Data_comb$Size_cat2.um="300-2000"
Data_comb$Size_cat2.um[Data_comb$Size_cat.um == "90-300"]="90-300"


# 3. Mass estimations ####

# /!\ Work in progress /!\
# Create a separate function ?
# https://pubs.acs.org/doi/full/10.1021/acs.est.4c01031 


 # *** 3.1 Polymer Density table [g/cm3] ####
Polymer_density=read.csv("Polymer_density.csv")

Data_comb$Pol_Density=1
for (pol in Polymer_density$Polymer ){
  Data_comb$Pol_Density[Data_comb$Polymer.grp==pol]=Polymer_density$Density_med_g.cm3[Polymer_density$Polymer==pol]
}

is.na(Data_comb$pol_density)

# *** 3.2 Volume / Simon (2018) ####
# https://pubs-acs-org.ezproxy.library.wur.nl/doi/10.1021/acs.est.4c01031
# https://pubs.acs.org/doi/10.1021/acs.est.3c03620?goto=supporting-info
# https://www.sciencedirect.com/science/article/pii/S0043135418303877?getft_integrator=acs&pes=vor&utm_source=acs
# The most used model is that of Simon et al. (2018). (22)
#Three assumptions are at the core of the model. 
# First, the particles are assumed to have an ellipsoidal shape, with the major and minor axes on the XY plane calculated from the ellipse that best fits the particle projection on the XY plane (equivalent ellipse from the 2D area obtained by image analysis).
# Second, the particles are assumed to lie at their lowest energy state (therefore the Z-direction axis is the smallest dimension).
# Third, the asymmetry of the ellipsoid in the ZY plane is assumed proportional to that in the XY plane hence the axis in the Z direction (i.e., particle height “H”) is assumed to be in the same ratio to the 2D minor axis (“W” for width), as the 2D minor axis is to the 2D major axis (“L” for length) (i.e., H/W = W/L). The model was developed for mass balance in a wastewater treatment plant, for particles ranging from 10–500 μm. No validation was run.

Data_comb$Aspect_ratio2=min( Data_comb$Aspect_ratio , Data_comb$Width.um /Data_comb$Length.um)
Data_comb$Aspect_ratio2[is.na(Data_comb$Aspect_ratio2)]=0.667
Data_comb$height.um=Data_comb$Width.um * Data_comb$Aspect_ratio2  


Data_comb$volumeS18.um3=4* pi * Data_comb$height.um * Data_comb$Width.um * Data_comb$Length.um /(3*2*2*2)


# *** 3.3 Mass (select) ####
Data_comb$Mass.ng=Data_comb$volumeS18.um3 *Data_comb$Pol_Density/1000


# 4. Analyse blanks ####
# Blank data frame (WUR) 
df_Blanks=subset(Data_comb, Soil_sample=="bcm"  ) # & Polymer.red12 != "Other.Plastic"
length(unique(df_Blanks$File_Name))

nrow( subset(Data_comb, Soil_sample=="bcm"))
nrow( subset(Data_WUR, Soil_sample=="bcm"))
nrow( subset(Data_Ubern_cor, Soil_sample=="bcm"))

nrow( unique( subset(Data_comb, Soil_sample=="bcm", select = "File_Name")))
nrow( unique( subset(Data_WUR, Soil_sample=="bcm", select = "File_Name")))
nrow( unique( subset(Data_Ubern_cor, Soil_sample=="bcm", select = "File_Name")))

nrow( subset(Data_comb, Preparation_Type=="Blank_chemical"))
nrow( unique( subset(Data_comb, Preparation_Type=="Blank_chemical", select = "File_Name")))



# number of blanks
length(unique(df_Blanks$File_Name))
length(unique(df_Blanks$Filter_Name))

length(unique(df_Blanks$File_Name[df_Blanks$Lab=="WUR"]))
length(unique(df_Blanks$File_Name[df_Blanks$Lab=="Ubern"]))

# number of blanks with no particules 
length(unique(df_Blanks$File_Name[df_Blanks$N.px==0] ))
length(unique(df_Blanks$File_Name[df_Blanks$Lab=="WUR" & df_Blanks$N.px==0]))
length(unique(df_Blanks$File_Name[df_Blanks$Lab=="Ubern"& df_Blanks$N.px==0]))

# list of polymers 
unique(df_Blanks$Polymer.grp)

Summary_Blanks_polymers = df_Blanks %>% 
  group_by( Polymer.grp, Polymer.red12,  Polymer.red3  ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
             Num.px=sum(N.px),           # Number of pixels
             Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
             SD.Area=sd(Area.um2.cor))


# Summary / Blanks / Polymer

# *a. Sum up per IR File, Polymer.grp ####
Summary_Blanks1_File = df_Blanks %>% 
  group_by(File_Name, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_Name, IR_rep, PMF_rep, Operator,  Polymer.grp, Polymer.red12,  Polymer.red3  ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
             Num.px=sum(N.px),           # Number of pixels
             Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
             SD.Area=sd(Area.um2.cor),
             )%>%
  ungroup()

# *b. Mean per Filter (if in one batch, one filter, multiple IR files (IR_rep or Operator_rep)) ####
Summary_Blanks2_Filter =  Summary_Blanks1_File %>% 
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_Name, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
             Num.px=mean(Num.px),           # Number of pixels
             Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             SD.Area=mean( SD.Area),
             N.operator=length(unique(Operator)), 
             Operators= paste0(unique(Operator), collapse = " ; "))%>%
  ungroup()

# *c. Mean per Filter sum polymers (if in one batch, multiple  filter, (Extract_rep))  #### 
Summary_Blanks3_Filter_sumPol =  Summary_Blanks2_Filter %>% 
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Filter_Name) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.particles),  # Number of particles (Sum of Binary)
             Num.px= sum(Num.px),           # Number of pixels
             Tot.Area.mm2= sum( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             N.operator=max(N.operator), 
             Operators= paste0(unique(Operators), collapse = " ; "))%>%
  ungroup()

 #write.csv(Summary_Blanks3_Filter_sumPol , paste(wd.out,"Summary_Blanks3_Filter_sumPol_2025.08.csv",sep = "/"))

# *d. Mean per Batch (if in one batch, multiple  filter, (Extract_rep))  ####   
Summary_Blanks4_Batch =   Summary_Blanks2_Filter %>%    
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= mean(N.particles),  # Number of particles (Sum of Binary)
             Num.px=mean(Num.px),           # Number of pixels
             Tot.Area.mm2=mean( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             SD.Area=mean( SD.Area) )%>%
  ungroup()

# write.csv(Summary_Blanks3_Batch, paste(wd.out,"Summary_Blanks_Batch_2024.11.13.csv",sep = "/"))


# *e. Sum all particles, from all polymers per Batch (if in one batch, multiple  filter, (Extract_rep)) ####

Summary_Blanks5_Batch_SumPol12 =   Summary_Blanks4_Batch %>%
  # Group
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.particles),  # Number of particles (Sum of Binary)
             Num.px=sum(Num.px),           # Number of pixels
             Tot.Area.mm2=sum( Tot.Area.mm2), # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=mean(Median.Area.sqrt.um),
             SD.Area=mean( SD.Area) )%>%
  ungroup()
# write.csv( Summary_Blanks4_Batch_SumPol12, paste(wd.out,"Summary_Blanks_Batch_2024.11.13.csv",sep = "/"))



# * f. Overview of Matching quality of "Other.plastic" in Blanks ####
# "Other.plastics" are more rare, less known, not tested for recovery, and more susceptible to be mistaken to other polymers
# Therefore, we add a matching quality filter to reduce the risk of mis-identification and over estimation
# Set up for a matching quality filter at 0.35, which is reasonable from expert judgement and 
# conveniently (ad-hoc) removes two EVAc particles that would otherwise bring the m16 blank over the 5 particles threshold (ad-hoc). 

Out=subset(Data_comb, Q_index !=0 & Q_index <0.35 & Polymer.red12 =="Other.Plastic" )




# 5. Blank correction - OPTION Particle ####

# * Remove samples with more than one particle in batches with blank of more than 5 particles ####
# if the batch has more than 5 particles we consider something went wrong in this batch so other samples might be contaminated. 
# However if a samples has <=1 particle it is likely not affected by the contamination

Batch_bcm_over5=Summary_Blanks5_Batch_SumPol12$Batch_Name[Summary_Blanks5_Batch_SumPol12$N.particles>5]

# Create the reduced data frame Data_comb_red
nrow (subset(Data_comb, Batch_Name %in% Batch_bcm_over5  )) # 515 particles detected in batch "m29" and "m9" that are potentially contamination 
nrow (subset(Data_comb, Batch_Name %in% Batch_bcm_over5 & N.px==0   )) # 9 samples are in Batch_bcm_over5 BUT wit h0 particles detected so we keep them. 

# Remove particles 
Data_comb_red=subset(Data_comb, Batch_Name %!in% Batch_bcm_over5 |  N.px==0 ) 

# Number of particles, Files, Soils 
nrow (subset(Data_comb_red, N.px!=0 ))
length (unique(Data_comb_red$File_Name))
length (unique(Data_comb_red$Soil_sample))


# * Characterize the remaining bcm contamination #### 

# Create dataframe 
df_bcm=subset(Data_comb_red, Soil_sample=="bcm" )
nrow(subset(df_bcm, N.px !=0))
unique(df_bcm$Polymer.grp)
#list of unique files 
unique(df_bcm$File_Name)

# number of blanks
length(unique(df_bcm$File_Name))
length(unique(df_bcm$Filter_Name))
length(unique(df_bcm$Batch_Name))

length(unique(df_bcm$File_Name[df_bcm$Lab=="WUR"]))
length(unique(df_bcm$File_Name[df_bcm$Lab=="Ubern"]))

# number of blanks with no particules 
length(unique(df_bcm$File_Name[df_bcm$N.px==0] ))
length(unique(df_bcm$File_Name[df_bcm$Lab=="WUR" & df_bcm$N.px==0]))
length(unique(df_bcm$File_Name[df_bcm$Lab=="Ubern"& df_bcm$N.px==0]))

#Number of UP: 
  nrow(subset(df_bcm, N.px!=0 ))

# list of polymers 
unique(df_bcm$Polymer.grp)

Summary_Blanks_polymers = df_bcm %>% 
  group_by( Polymer.grp, Polymer.red12,  Polymer.red3  ) %>%              # Group per file, Filter.name,Sample_type,Operator for info
  summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
             Num.px=sum(N.px),           # Number of pixels
             Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
             #Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
             SD.Area=sd(Area.um2.cor))


# Summary per File, Polymer all :  

S1c_bcm = df_bcm %>% 
  group_by( File_Name, Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,    
              CSS,Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%
  summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
             Num.px=sum(N.px),           # Number of pixels
             Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area 
             Tot.Mass.ng=sum( Mass.ng),
             Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
             SD.Area=sd(Area.um2.cor)) %>%  #
  # for each file I want all the polymers represented
  ungroup() %>%
  complete(nesting(File_Name, Lab,Batch_Name, Preparation_Type, Sample_type, Soil_sample,    
                   CSS,Farm, Field, Extraction_Name, Filter_div, Filter_Name, IR_rep, PMF_rep, Operator ),
           nesting(Polymer.grp, Polymer.red12,  Polymer.red3 ), 
           fill=list(N.particles=0,
                     Num.px=0,
                     Tot.Area.mm2=0,
                     Median.Area.sqrt.um=0,
                     SD.Area=0)) %>%
  # Remove the "No.plastic", not needed anymore becasue all other polymers are reported. 
  subset(Polymer.grp!="No.plastic") #

nrow(S1c_bcm)/6
length(unique(S1c_bcm$File_Name))
sum(S1c_bcm$N.particles)

# Summary per Filter, Polymer all :  
S2c_bcm= S1c_bcm  %>% 
  group_by( Lab, Batch_Name, Preparation_Type, Sample_type, Soil_sample,     
                CSS,Farm, Field, Extraction_Name, Filter_div, Filter_Name, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%
  summarise( N.files = n(),
             Operators= paste0(Operator, collapse = " ; "),
             Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
             Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
             Mean.Tot.Area.mm2=mean(Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
             Mean.Tot.Mass.ng=mean(Tot.Mass.ng), #  Mean mass per sample and polymer, over the files/operators
             sd.particles.operator=sd(N.particles),
             sd.Area.operator=sd(Tot.Area.mm2)   )%>%
  ungroup() 
 
nrow(S2c_bcm)/6
length(unique(S2c_bcm$Filter_Name))
sum(S2c_bcm$Mean.particles)



# Summary per Batch, Polymer all :   

S3c_bcm= S2c_bcm%>% 
  group_by( Batch_Name, Lab,Preparation_Type, Sample_type, Soil_sample,      
                 CSS,Farm, Field, Extraction_Name, Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%
  summarise( N.div = n(),
             Mean.particles= sum(Mean.particles), #
             Mean.px=sum(Mean.px),              # 
             Mean.Tot.Area.mm2=sum(Mean.Tot.Area.mm2), #  
             Mean.Tot.Mass.ng=sum(Mean.Tot.Mass.ng) ) %>%
  ungroup()

nrow(S3c_bcm)/6
length(unique(S3c_bcm$Batch_Name))
sum(S3c_bcm$Mean.particles)

# Summary per Lab, Polymer all :  

S7c_bcm= S3c_bcm %>%
  group_by(Preparation_Type, Lab,
           Polymer.grp,, Polymer.red12,  Polymer.red3 ) %>%
  summarise(N.files = n(),
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

nrow(S7c_bcm)/6
length(unique(S7c_bcm$Lab))
sum(S7c_bcm$Mean.particles.MM)

# Summary per Polymer, per lab Mean filter :  

S_pPol_pLab_mFilter= S3c_bcm %>%
  group_by(Preparation_Type, Lab,
           Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%
  summarise(N.filters = n(),
            Mean.particles.MM= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
            Min.particles.MM= min( Mean.particles),
            Max.particles.MM= max( Mean.particles),
            Mean.px.MM=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
            Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
            Median.Tot.Area.mm2.MM=median(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
            Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2),
            Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2),
            Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng) ) %>%
  group_by(Lab) %>%
  mutate(
    MiP_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100) %>%
  ungroup()

# Summary per Polymer, per lab Mean filter :  

S_pPol_mFilter= S3c_bcm %>%
  group_by(Preparation_Type,
           Polymer.grp, Polymer.red12,  Polymer.red3 ) %>%
  summarise(N.filters = n(),
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


# *** PLOT Polymer composition per blank filter####

# Order plot per polymer   
S7c_bcm$Polymer.red12 = factor(S7c_bcm$Polymer.red12,
                               levels = unique(S7c_bcm$Polymer.red12[order(-S7c_bcm$Mean.particles.MM)] ))

# Add percentages in the plot
S7c_bcm = S7c_bcm %>%
  group_by(Lab) %>%
  mutate(
    MiP_ntxt=paste0("(", round(Mean.particles.MM, 2), ")") ,
    MiP_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
    MiP_perc_text=paste0(round(MiP_perc, 0), "%"), # Creat percentage as text  
    MiP_Area_txt=paste0("(", round(Mean.Tot.Area.mm2.MM, 3), ")") ,
    MiP_Area_perc = Mean.Tot.Area.mm2.MM / sum(Mean.Tot.Area.mm2.MM) * 100,
    MiP_Area_perc_text=paste0(round(MiP_Area_perc, 0), "%") ) # Creat percentage as text  
    


# Do not show percentages < 3.5% 
#S7c_bcm$MiP_perc_text [S7c_bcm$MiP_perc < 3.5]=NA 

S7c_bcm=subset(S7c_bcm, Mean.particles.MM!=0)

# Number 
ggplot(S7c_bcm, aes(x=Polymer.red12, y=Mean.particles.MM, fill= Polymer.grp ))+
  facet_grid(~Lab, scales =  "free", )+
  geom_bar(stat="summary", width=1, color="white") +
  #coord_polar("y", start=0) +
  scale_fill_manual(values = c("PE"="#377EB8",  "EVAc"="#E41A1C", "PU"="#F781BF",
                               "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                               "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B") , 
                    # Relabel  "Other.Plastic"                 
                    labels = c( "Other.Plastic"= "Other Plastics" ) ) +
  theme_minimal() + 
  ggtitle("Chemical blanks, Polymer composition")+
  # geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
  geom_text(aes(label =  S7c_bcm$MiP_perc_text), vjust = 1, nudge_y = 0.045)+
  geom_text(aes(label =  S7c_bcm$MiP_ntxt), vjust = 1, nudge_y = -0.005)+
  scale_y_continuous(breaks = seq(0, 1, 0.2) )+  # Have a break for each gap # y_max*200/8
                   #  labels = label_at(1000), #,                                        # label every second break
                   #  limits = c(-y_max*200/40, y_max*202))+ 
  #ggtitle(paste("Field Samples ; CSS ", css))+
  # guides( color  = "none")+
  labs( y = "Average number of MiP per sample",
    fill = "Polymers identified") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

dev.size(units = "cm") # 15 x 9 cm 
##  dev.size(units = "cm") gives the size of ggplot actual zoom

# Area 
ggplot(S7c_bcm, aes(x=Polymer.red12, y=Mean.Tot.Area.mm2.MM, fill= Polymer.grp ))+
  facet_grid(~Lab, scales =  "free", )+
  geom_bar(stat="summary", width=1, color="white") +
  #coord_polar("y", start=0) +
  scale_fill_manual(values = c("PE"="#377EB8",  "EVAc"="#E41A1C", "PU"="#F781BF",
                               "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                               "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B") , 
                    # Relabel  "Other.Plastic"                 
                    labels = c( "Other.Plastic"= "Other Plastics" ) ) +
  theme_minimal() + 
  ggtitle("Chemical blanks, Polymer composition")+
  # geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
  geom_text(aes(label =  S7c_bcm$MiP_Area_perc_text), vjust = 1, nudge_y = 0.0045)+
  geom_text(aes(label =  S7c_bcm$MiP_Area_txt),       vjust = 1, nudge_y = -0.0005)+
  scale_y_continuous(breaks = seq(0, 1, 0.1) )+  # Have a break for each gap # y_max*200/8
  #  labels = label_at(1000), #,                                        # label every second break
  #  limits = c(-y_max*200/40, y_max*202))+ 
  #ggtitle(paste("Field Samples ; CSS ", css))+
  # guides( color  = "none")+
  labs(y = expression("Average surface of plastic per sample [mm" ^ 2*"]"),
        fill = "Polymers identified") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())

dev.size(units = "cm") # 15 x 9 cm 
##  dev.size(units = "cm") gives the size of ggplot actual zoom



# *** Both lab: ####
# Number of Plastic particle per blank filter:
sum(S2c_bcm$Mean.particles ) / length(unique(S2c_bcm$Filter_Name))

# Average number of PP particle per blank filter:
sum(subset(S2c_bcm ,Polymer.grp %in% c("PP") , select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name))

# Median area of PP particle per blank File (no aggreagation per filter considered)
PP_Areamedian=median(df_bcm$Area.um2.cor[grep("PP",df_bcm$Polymer.grp)])

# Average number of PE particle per blank filter:
sum(subset(S2c_bcm ,Polymer.grp %in% c("PE") , select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name))

# Median area of PE particle per blank File (no aggreagation per filter considered)
PE_Areamedian=median(df_bcm$Area.um2.cor[grep("PE",df_bcm$Polymer.grp)])

# Number rest of plastic particle per blank:
sum(subset(S2c_bcm, Polymer.red12  %!in% c( "PE", "PP", "No.plastic") , select="Mean.particles") ) / length(unique(df_bcm$File_Name))


# *** WUR: ####  

# Number of Plastic particle per blank filter:
sum(S2c_bcm$Mean.particles[S2c_bcm$Lab=="WUR"] ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="WUR"] ))

# Average number of PP particle per blank filter:
sum(subset(S2c_bcm ,Polymer.grp %in% c("PP") & Lab=="WUR" , select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="WUR"]))

# Median area of PP particle per blank File (no aggreagation per filter considered)
PP_Areamedian=median(df_bcm$Area.um2.cor[df_bcm$Polymer.grp =="PP" & df_bcm$Lab=="WUR"])

# Average number of PE particle per blank filter:
sum(subset(S2c_bcm ,Polymer.grp %in% c("PE") & Lab=="WUR" , select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="WUR"]))

# Median area of PE particle per blank File (no aggreagation per filter considered)
PE_Areamedian=median(df_bcm$Area.um2.cor[df_bcm$Polymer.grp =="PE" & df_bcm$Lab=="WUR"])

# Number rest of plastic particle per blank:
sum(subset(S2c_bcm, Polymer.red12  %!in% c( "PE", "PP", "No.plastic")  & Lab=="WUR", select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="WUR"]))


# *** UBern: #### 
# Number of Plastic particle per blank filter:
sum(S2c_bcm$Mean.particles[S2c_bcm$Lab=="Ubern"] ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="Ubern"] ))

# Average number of PP particle per blank filter:
sum(subset(S2c_bcm ,Polymer.grp %in% c("PP") & Lab=="Ubern" , select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="Ubern"]))

# Median area of PP particle per blank File (no aggreagation per filter considered)
PP_Areamedian=median(df_bcm$Area.um2.cor[df_bcm$Polymer.grp =="PP" & df_bcm$Lab=="Ubern"])

# Average number of PE particle per blank filter:
sum(subset(S2c_bcm ,Polymer.grp %in% c("PE") & Lab=="Ubern" , select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="Ubern"]))

# Median area of PE particle per blank File (no aggreagation per filter considered)
PE_Areamedian=median(df_bcm$Area.um2.cor[df_bcm$Polymer.grp =="PE" & df_bcm$Lab=="Ubern"])

# Number rest of plastic particle per blank:
sum(subset(S2c_bcm, Polymer.red12  %!in% c( "PE", "PP", "No.plastic")  & Lab=="Ubern", select="Mean.particles") ) / length(unique(S2c_bcm$Filter_Name[S2c_bcm$Lab=="Ubern"]))

#MC- you print all these number to the terminal, but don't save them anywhere?

# * Apply correction ####
Data_comb_red_blank= Data_comb_red
# For Ubern: 
# 1. If there is PE, remove 1 PE particle with the closest area from the median:
# 2. If no PE, remove one PP particle with the closest area from the median:
# 3. If no PE, no PP don't do any thing

# For WUR: 
# 1. If there is PP, remove 1 PP particle with the closest area from the median:
# 2. If no PP, remove one PE particle with the closest area from the median:
# 3. If no PE, no PP don't do any thing

#??????????????????????

# 
# # Start For loop per sample: 
# for (s in unique(Data_comb_red_blank$File_Name)){
#   # Find all the particles in sample s: 
#   # test with s="M16040701_S2_results.csv"
#   Sample_i=Data_comb_red_blank[Data_comb_red_blank$File_Name==s,]
#   
#   # Assign the expected bcm distribution per Lab
#   bcm_filter_i= subset(S_pPol_pLab_mFilter, Lab==unique(Sample_i$Lab))
#   
#   # Recalculate occurrence percentage based on the list of available polymers in Sample_i
#   bcm_filter_i$MiP_perc_i=bcm_filter_i$Mean.particles.MM/ 
#       sum(bcm_filter_i$Mean.particles.MM[ bcm_filter_i$Polymer.grp %in% unique(Sample_i$Polymer.grp)])
#   
#   bcm_filter_i$MiP_percumul_i=cumsum(bcm_filter_i$MiP_perc_i)
#   
#  
# } # end loop Samples
# 
# 
# 
# #???????????????????
# 
# # Start For loop per sample: 
# for (s in unique(Data_comb_red_blank$File_Name)){
#   # Find all the particles in sample s: 
#   # test with s="M16040701_S2_results.csv"
#   Sample=Data_comb_red_blank[Data_comb_red_blank$File_Name==s,]
#   
#   # For Ubern: 
#   if (unique(Sample$Lab)=="Ubern"){
#     # 1. If there is a PE particle, remove a PE,          
#     if ("PE" %in% Sample$Polymer.grp){
#       # Find the PE particle with the closest area from the median.
#       # Create a df of all PE particles
#       PE=Sample[Sample$Polymer.grp=="PE",]
#       # Calculate the absolute difference with PE_Ubern_Areamedian for each PE particle 
#       PE$Median_diff=0
#       for (p in 1:nrow(PE)){
#         PE$Median_diff[p]=abs(PE$Area.um2.cor[p]-PE_Ubern_Areamedian)
#       }
#       # ID of the FIRST[1] particle with the min Median_diff:
#       ID_PE=PE$ID[PE$Median_diff==min(PE$Median_diff)][1]
#       # remove this particle  
#       Data_comb_red_blank = Data_comb_red_blank %>%
#         mutate(N.px =     if_else(ID== ID_PE, 0, N.px),
#                Length.um =if_else(ID== ID_PE, 0, Length.um),
#                Width.um = if_else(ID== ID_PE, 0, Width.um),
#                Area.um2.cor = if_else(ID== ID_PE, 0,  Area.um2.cor),
#                Mass.ng =      if_else(ID== ID_PE, 0,  Mass.ng),
#                Polymer.grp=   if_else(ID== ID_PE, "No.plastic", Polymer.grp) )
#       
#       # 2. If there is no PE particle but PP, remove a PP,    
#     } else if  ("PP" %in% Sample$Polymer.grp){ 
#       #Find the PE particle with the closest area from the median:
#       PP=Sample[Sample$Polymer.grp=="PP",]
#       # Calculate the absolute difference with PP_Areamedian for each PE particle 
#       PP$Median_diff=0
#       for (p in 1:nrow(PP)){
#         PP$Median_diff[p]=abs(PP$Area.um2.cor[p]- PP_Ubern_Areamedian)
#       }
#       # ID of the FIRST[1] particle with the min Median_diff:
#       ID_PP=PP$ID[PP$Median_diff==min(PP$Median_diff)][1]
#       # remove this particle  
#       Data_comb_red_blank=Data_comb_red_blank %>%
#         mutate(N.px =     if_else(ID== ID_PP, 0, N.px),
#                Length.um =if_else(ID== ID_PP, 0, Length.um),
#                Width.um = if_else(ID== ID_PP, 0, Width.um),
#                Area.um2.cor = if_else(ID== ID_PP, 0,  Area.um2.cor),
#                Mass.ng =      if_else(ID== ID_PP, 0,  Mass.ng),
#                Polymer.grp=   if_else(ID== ID_PP, "No.plastic", Polymer.grp) )
# 
#     }
#     # 3. If no PE, no PP don't do any thing
#   } # end if UBern
#   
#   
#   # For WUR:  
#   if (unique(Sample$Lab)=="WUR"){
#     # 1. If there is a PP particle, remove a PP,      
#     if ("PP" %in% Sample$Polymer.grp){
#       #Find the PP particle with the closest area from the median:
#       PP=Sample[Sample$Polymer.grp=="PP",]
#       # Calculate the absolute difference with PP_Areamedian for each PP particle 
#       PP$Median_diff=0
#       for (p in 1:nrow(PP)){
#         PP$Median_diff[p]=abs(PP$Area.um2.cor[p]-PP_WUR_Areamedian)
#       }
#       # ID of the FIRST[1] particle with the min Median_diff:
#       ID_PP=PP$ID[PP$Median_diff==min(PP$Median_diff)][1]
#       # remove this particle  
#       Data_comb_red_blank=Data_comb_red_blank %>%
#         mutate(N.px =     if_else(ID== ID_PP, 0, N.px),
#                Length.um =if_else(ID== ID_PP, 0, Length.um),
#                Width.um = if_else(ID== ID_PP, 0, Width.um),
#                Area.um2.cor = if_else(ID== ID_PP, 0,  Area.um2.cor),
#                Mass.ng =      if_else(ID== ID_PP, 0,  Mass.ng),
#                Polymer.grp=   if_else(ID== ID_PP, "No.plastic", Polymer.grp) )
#       
#       # 2. If there is no PP particle but PE, remove a PE,        
#     } else if  ("PE" %in% Sample$Polymer.grp){ # If there is no PP particle, but a PE 
#       #Find the PE particle with the closest area from the median:
#       PE=Sample[Sample$Polymer.grp=="PE",]
#       # Calculate the absolute difference with PE_Areamedian for each PE particle 
#       PE$Median_diff=0
#       for (p in 1:nrow(PE)){
#         PE$Median_diff[p]=abs(PE$Area.um2.cor[p]-PE_WUR_Areamedian)
#       }
#       # ID of the FIRST[1] particle with the min Median_diff:
#       ID_PE=PE$ID[PE$Median_diff==min(PE$Median_diff)][1]
#       # remove this particle  
#       Data_comb_red_blank=Data_comb_red_blank %>%
#         mutate(N.px =     if_else(ID== ID_PE, 0, N.px),
#                Length.um =if_else(ID== ID_PE, 0, Length.um),
#                Width.um = if_else(ID== ID_PE, 0, Width.um),
#                Area.um2.cor = if_else(ID== ID_PE, 0,  Area.um2.cor),
#                Mass.ng =      if_else(ID== ID_PE, 0,  Mass.ng),
#                Polymer.grp=   if_else(ID== ID_PE, "No.plastic", Polymer.grp) )
#     }
#     # 3. If no PE, no PP don't do any thing    
#   } # end if WUR
# } # end loop Samples
# 
# 
# # Number of particles:
# nrow(Data_comb[Data_comb$N.px>0,])
# nrow(Data_comb_red[Data_comb_red$N.px>0,])
# nrow(Data_comb_red_blank[Data_comb_red_blank$N.px>0,])
# 
# 
# # Number of files: 
# length(unique(Data_comb$File_Name))
# length(unique(Data_comb_red$File_Name))
# length(unique(Data_comb_red_blank$File_Name))
# 


# 6. Add the field METADATA ####

Fields_METADATA=read.csv("Fields_METADATA.csv")

head(Fields_METADATA)

Field_METADATA_vector=colnames(Fields_METADATA)
Field_METADATA_vector=Field_METADATA_vector[Field_METADATA_vector %!in% c("X", "CSS","Farm","Field")]

Data_comb_meta=merge(Data_comb, Fields_METADATA, by=c("CSS","Farm","Field"), all.x = TRUE)
Data_comb_red_blank_meta=merge(Data_comb_red_blank, Fields_METADATA, by=c("CSS","Farm","Field"), all.x = TRUE)

# Data_comb_red_blank_meta$Preparation_Type=Data_comb_red_blank_meta$Preparation_Type.x
# Data_comb_red_blank_meta=subset(Data_comb_red_blank_meta, select = -c(Preparation_Type.x, Preparation_Type.y))
# head(Data_comb_red_blank_meta)
# 7. Create Table per field ###



# 8. Export table ####  
length(unique(Data_comb_red_blank$File_Name))
uP_Colnames[uP_Colnames %!in% colnames(Data_comb_red_blank)]
length(unique(Data_comb_red_blank$File_Name[Data_comb_red_blank$Polymer.grp=="No.plastic"]))
length(unique(Data_comb$File_Name[Data_comb$Polymer.grp=="No.plastic"]))
length(unique(Data_comb$File_Name))


 # write.csv(Data_comb_red_blank_meta, paste(wd.out,"Corrected_MiP_Particles.csv",sep = "/"))
 write.csv(Data_comb_red, paste(wd.out,"Corrected_MiP_Particles.csv",sep = "/"))
 
 #write.csv(Data_comb_meta, paste(wd.out,"MiP_Particles.csv",sep = "/"))

 write.csv(df_bcm, paste(wd.out,"Blanks_Particles.csv",sep = "/"))

 write.csv(S_pPol_pLab_mFilter, paste(wd.out,"Summary_Blanks_pPolymer_pLab_meanFilter.csv",sep = "/"))






