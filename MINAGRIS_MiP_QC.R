# Create specific table for blanks, Reference Soils, and spikes, Calculate recovery,

# Start script to analyse QCs: 
#   -Blank chemicals
#   -Blank soils
#   -Standard soils
#   -Spiked soils
# 
# ...for their Particle number, Area, size distribution and polymer composition. 

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(stats)


rm(list=ls()) # cleaning console
graphics.off() # cleaning plots
'%!in%' <- function(x,y)!('%in%'(x,y))

# Set WD in the project only needed when RStudio projects are not used.

# Outputs folder
wd.out= "Outputs"

# Initialization 
#MC - see comment in previous script about dates in output file names.
Data_comb_red_blank=read.csv("Outputs/Corrected_MiP_Particles_20241128.csv")

Summary3a_Filter=read.csv("Outputs/Summary3a_Filter_20241217.csv")
Summary3c_Filter=read.csv("Outputs/Summary3c_Filter_20241217.csv")
Summary3e_Filter=read.csv("Outputs/Summary3e_Filter_20241217.csv")



# Chemical Blanks ####

df_st=subset(Data_comb_red_blank, Soil_sample=="st")

unique(df_st$File_Names)

S3a_st=subset(Summary3a_Filter, Soil_sample=="st")
S3e_st=subset(Summary3e_Filter, Soil_sample=="st")
S3c_st=subset(Summary3c_Filter, Soil_sample=="st")

unique(df_st$File_Names)
unique(df_st$CSS)[order(unique(df_st$CSS))]

# * All filters, Sum of all polymers  ####
ggplot( S3e_st, aes(x=Extraction_Name, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
  scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
  scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
  geom_point()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title.x = element_blank())

# * Dots,  All filters,  polymer 12 ####
S3c_st$Lab_Batch=paste( S3c_st$Lab, "CSS", S3c_st$Batch_Name )
ggplot(  S3c_st, aes( Extraction_Name, Mean.particles)) +
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


# * Size distribution  ####






# * Polymer composition ####




ggplot(S3c_st, aes(x=Polymer.red12, y=Mean.particles, fill= Polymer.red12 ))+
  facet_wrap(~Lab)+
  geom_bar(stat="summary", width=1, color="white") +
  #coord_polar("y", start=0) +
  scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                               "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                               "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B") , 
                    # Relabel  "Other.Plastic"                 
                    labels = c( "Other.Plastic"= "Other Plastics" ) ) +
  theme_minimal() + 
  geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
  geom_text(aes(label = paste0(round(MiP_perc, 1), "%")), vjust = 1, nudge_y = 0.2)+
  #ggtitle(paste("Field Samples ; CSS ", css))+
  # guides( color  = "none")+
  labs(# y = "Average number of plastic particles per kg of soil",
    fill = "Polymers identified") +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank())




# Soil Blanks ####
  # Samples of burned soil brought to the field and CSS lab to assess contamination durin sampling.  I expect 
  # Blank data frame (WUR) 
  df_SoilBlanks=subset(Data_comb_red_blank, Soil_sample=="rs")
  S3a_SoilBlanks=subset(Summary3a_Filter, Soil_sample=="rs")
  S3e_SoilBlanks=subset(Summary3e_Filter, Soil_sample=="rs")
  S3c_SoilBlanks=subset(Summary3c_Filter, Soil_sample=="rs")

  unique(df_SoilBlanks$File_Names)
  unique(df_SoilBlanks$CSS)[order(unique(df_SoilBlanks$CSS))]
  
  # * All filters, Sum of all polymers ####
  ggplot( S3e_SoilBlanks, aes(x=Extraction_Name, y=Mean.particles ) )+
    geom_point()
  
  # * Dots, All filters,  polymer 12 ####
  S3c_SoilBlanks$Lab_CSS=paste( S3c_SoilBlanks$Lab, "CSS", S3c_SoilBlanks$CSS )
  ggplot(  S3c_SoilBlanks, aes( Lab_CSS, Mean.particles)) +
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
  
  # * Size distribution  ####
  
  
  
  
  
  
  # * Polymer composition ####
    ggplot(S3c_st, aes(x=Polymer.red12, y=Mean.particles, fill= Polymer.red12 ))+
    facet_wrap(~Lab)+
    geom_bar(stat="summary", width=1, color="white") +
    #coord_polar("y", start=0) +
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"= "Other Plastics" ) ) +
    theme_minimal() + 
    geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
    geom_text(aes(label = paste0(round(MiP_perc, 1), "%")), vjust = 1, nudge_y = 0.2)+
    #ggtitle(paste("Field Samples ; CSS ", css))+
    # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
      fill = "Polymers identified") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  
# Standard soils ####
  df_st=subset(Data_comb_red_blank, Soil_sample=="st")
  
  unique(df_st$File_Names)
  
  S3a_st=subset(Summary3a_Filter, Soil_sample=="st")
  S3e_st=subset(Summary3e_Filter, Soil_sample=="st")
  S3c_st=subset(Summary3c_Filter, Soil_sample=="st")
  
  unique(df_st$File_Names)
  unique(df_st$CSS)[order(unique(df_st$CSS))]
  
  # * All filters, Sum of all polymers  ####
  ggplot( S3e_st, aes(x=Extraction_Name, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
    scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
    scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
    geom_point()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
          axis.title.x = element_blank())
  
  # * Dots,  All filters,  polymer 12 ####
  S3c_st$Lab_Batch=paste( S3c_st$Lab, "CSS", S3c_st$Batch_Name )
  ggplot(  S3c_st, aes( Extraction_Name, Mean.particles)) +
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
  
  
  # * Size distribution  ####
  
  
  
  
  
  
  # * Polymer composition ####
  
  
  
  
  ggplot(S3c_st, aes(x=Polymer.red12, y=Mean.particles, fill= Polymer.red12 ))+
    facet_wrap(~Lab)+
    geom_bar(stat="summary", width=1, color="white") +
    #coord_polar("y", start=0) +
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"= "Other Plastics" ) ) +
    theme_minimal() + 
    geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
    geom_text(aes(label = paste0(round(MiP_perc, 1), "%")), vjust = 1, nudge_y = 0.2)+
    #ggtitle(paste("Field Samples ; CSS ", css))+
    # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
      fill = "Polymers identified") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  
  
  
# Spiking ####
  
  
  