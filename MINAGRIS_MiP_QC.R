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

source("MINAGRIS_Read_Labels_Function.R") 

# Set WD in the project only needed when RStudio projects are not used.

# Outputs folder
wd.out= "Outputs/2025_03"

# 0. Load Data ####

Cat.um.txt=c("90-300", "300-510", "510-720",
             "720-930", "930-1140", "1140-1350")



#MC - see comment in previous script about dates in output file names.
Data_comb_red_blank=read.csv(paste(wd.out, "/Corrected_MiP_Particles.csv",sep=""))

# Summary per PMF File: 
  Summary1e_File=read.csv("Outputs/Summary1e_File_20241217.csv") # 3e, Overall mean: NULL


# Summary per Filter : 
  Summary3a_Filter=read.csv("Outputs/Summary3a_Filter_20241217.csv") # 3a, All factors: Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um
  Summary3c_Filter=read.csv("Outputs/Summary3c_Filter_20241217.csv") # 3c, Polymer12: Polymer.red12,  Polymer.red3
  Summary3e_Filter=read.csv("Outputs/Summary3e_Filter_20241217.csv") # 3e, Overall mean: NULL

  Summary3f_Filter=read.csv("Outputs/Summary3f_Filter_20241217.csv") # 3e, SizeCat

# 1. Chemical Blanks ####
  
  # Create dataframe 
  df_bcm=subset(Data_comb_red_blank, Soil_sample=="bcm")
  
  #list of unique files 
  unique(df_bcm$File_Names)
  
  # Summary per Filter :  

  S3a_bcm=subset(Summary3a_Filter, Soil_sample=="bcm")
  S3e_bcm=subset(Summary3e_Filter, Soil_sample=="bcm")
  S3c_bcm=subset(Summary3c_Filter, Soil_sample=="bcm")
  
  S3f_bcm=subset(Summary3f_Filter, Soil_sample=="bcm")
  
  S7a_bcm= S3a_bcm %>%
    group_by(Preparation_Type,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  
  S7a_bcm2= S3a_bcm %>%
    group_by(Preparation_Type, Lab,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  S7c_bcm= S3c_bcm %>%
    group_by(Preparation_Type, Lab,
            Polymer.red12,  Polymer.red3 ) %>%
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
  
  S7f_bcm =S3f_bcm %>%
    group_by(Preparation_Type, 
              Size_cat.um  ) %>%
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
    
  S7f_bcm2 =S3f_bcm %>%
    group_by(Preparation_Type, Lab,
             Size_cat.um  ) %>%
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
  
  
  
  # * PLOT All filters, Sum of all polymers  ####
    ggplot( S3e_bcm, aes(x=Extraction_Name, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
      scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
      scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
      ggtitle("Chemical blanks, sum all Polymers ")+
      geom_point()+
      theme_minimal()+
     theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
  
  # * PLOT All filters,  polymer 12 ####
  S3c_bcm$Lab_Batch=paste( S3c_bcm$Lab, "CSS", S3c_bcm$Batch_Name )
  ggplot(  S3c_bcm, aes( Extraction_Name, Mean.particles)) +
    geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
    ggtitle("Chemical blanks, Polymer.red12")+
    scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"= "Other Plastics" ) ) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
          axis.title.x = element_blank())
  
  
  # * PLOT Size distribution  ####
   # Per Polymer12
  # One value per Size_cat.um
  
  S7a_bcm$Size_cat.um= factor(S7a_bcm$Size_cat.um,
                                         levels =  Cat.um.txt )
  # Add percentages in the plot
  S7f_bcm = S7f_bcm %>%
    #group_by(Lab) %>%
    mutate(
      MiP_cat_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
      MiP_cat_perc_text=paste0(round(MiP_cat_perc, 0), "%") ) # Creat percentage as text  
  
  S7f_bcm=subset(S7f_bcm, Size_cat.um %in%  Cat.um.txt)
  
  
  S7a_bcm=subset(S7a_bcm, Size_cat.um %in%  Cat.um.txt)
  S7a_bcm$Size_cat.um= factor(S7a_bcm$Size_cat.um,
                              levels =  Cat.um.txt )
  
 ggplot( ) +
    geom_bar(data=S7a_bcm, aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12), position="stack", stat="identity")+ 
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastics") ) +
   geom_text(data= S7f_bcm, aes(x=Size_cat.um,  y=Mean.particles.MM*200, label =  MiP_cat_perc_text), vjust = 1, nudge_y = 5)+
    ggtitle("Chemical blanks, Micoplastic size distribution")+
    theme_minimal()+
    labs(y = "Average number of plastic particles per kg of soil",
         x= "Size categories [µm]",
         fill =  "Polymers identified") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
  
  
  # per lab
 
 S7a_bcm2$Size_cat.um= factor(S7a_bcm2$Size_cat.um,
                             levels =  Cat.um.txt )
 # Add percentages in the plot
 S7a_bcm2=subset(S7a_bcm2, Size_cat.um %in%  Cat.um.txt)
 S7f_bcm2=subset(S7f_bcm2, Size_cat.um %in%  Cat.um.txt)
 
 S7f_bcm2 = S7f_bcm2 %>%
   group_by(Lab) %>%
   mutate(
     MiP_cat_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
     MiP_cat_perc_text=paste0(round(MiP_cat_perc, 0), "%") ) # Creat percentage as text  
 
 
 
 
 
 S7a_bcm2$Size_cat.um= factor(S7a_bcm2$Size_cat.um,
                             levels =  Cat.um.txt )
 
 ggplot( ) +
   facet_wrap(~Lab)+
   geom_bar(data=S7a_bcm2, aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12), position="stack", stat="identity")+ 
   scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                "CA"= "#FFD39B") , 
                     # Relabel  "Other.Plastic"                 
                     labels = c( "Other.Plastic"="Other Plastics") ) +
   geom_text(data= S7f_bcm2, aes(x=Size_cat.um,  y=Mean.particles.MM*200, label =  MiP_cat_perc_text), vjust = 1, nudge_y = 5)+
   ggtitle("Chemical blanks, Micoplastic size distribution")+
   theme_minimal()+
   labs(y = "Average number of plastic particles per kg of soil",
        x= "Size categories [µm]",
        fill =  "Polymers identified") +
   theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
  
  
  
  # * PLOT Polymer composition ####
  
 # Order plot per polymer   
 S7c_bcm$Polymer.red12 = factor(S7c_bcm$Polymer.red12,
                                           levels = unique(S7c_bcm$Polymer.red12[order(-S7c_bcm$Mean.particles.MM)] ))
 
 # Add percentages in the plot
 S7c_bcm = S7c_bcm %>%
   group_by(Lab) %>%
 mutate(
   MiP_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
   MiP_perc_text=paste0(round(MiP_perc, 0), "%") ) # Creat percentage as text  
 
 
 # Do not show percentages < 3.5% 
 S7c_bcm$MiP_perc_text [S7c_bcm$MiP_perc < 3.5]=NA 
 
 S7c_bcm=subset(S7c_bcm, Mean.particles.MM!=0)
 
  ggplot(S7c_bcm, aes(x=Polymer.red12, y=Mean.particles.MM, fill= Polymer.red12 ))+
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
    ggtitle("Chemical blanks, Polymer composition")+
   # geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
    geom_text(aes(label =  S7c_bcm$MiP_perc_text), vjust = 1, nudge_y = 0.05)+
    #ggtitle(paste("Field Samples ; CSS ", css))+
    # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
      fill = "Polymers identified") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  



# 2. Soil Blanks ####

  
  # Create dataframe 
  df_SoilBlanks=subset(Data_comb_red_blank, Soil_sample=="rs")
  
  #list of unique files 
  unique(df_SoilBlanks$File_Names)
  
  # Summary per Filter :  
  S3a_SoilBlanks=subset(Summary3a_Filter, Soil_sample=="rs")
  S3e_SoilBlanks=subset(Summary3e_Filter, Soil_sample=="rs")
  S3c_SoilBlanks=subset(Summary3c_Filter, Soil_sample=="rs")
  
  S3f_SoilBlanks=subset(Summary3f_Filter, Soil_sample=="rs")
  
  S7a_SoilBlanks= S3a_SoilBlanks %>%
    group_by(Preparation_Type,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  
  S7a_SoilBlanks2= S3a_SoilBlanks %>%
    group_by(Preparation_Type, Lab,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  S7c_SoilBlanks= S3c_SoilBlanks %>%
    group_by(Preparation_Type, Lab,
             Polymer.red12,  Polymer.red3 ) %>%
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
  
  S7f_SoilBlanks =S3f_SoilBlanks %>%
    group_by(Preparation_Type, 
             Size_cat.um  ) %>%
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
  
  S7f_SoilBlanks2 =S3f_SoilBlanks %>%
    group_by(Preparation_Type, Lab,
             Size_cat.um  ) %>%
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
  
  
  
  # * PLOT All filters, Sum of all polymers  ####
  
  S3e_SoilBlanks$Lab_Batch_CSS=paste( S3e_SoilBlanks$Lab, S3e_SoilBlanks$Batch_Name, "CSS", S3e_SoilBlanks$CSS )
  
  ggplot( S3e_SoilBlanks, aes(x=Lab_Batch_CSS, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
    scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
    scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
    ggtitle("Soil Blanks, sum all Polymers ")+
    geom_point()+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
          axis.title.x = element_blank())
  
  # * PLOT All filters,  polymer 12 ####
  S3c_SoilBlanks$Lab_Batch_CSS=paste( S3c_SoilBlanks$Lab, S3c_SoilBlanks$Batch_Name, "CSS", S3c_SoilBlanks$CSS )
  ggplot(  S3c_SoilBlanks, aes( Lab_Batch_CSS, Mean.particles)) +
    geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
    ggtitle("Soil Blanks, Polymer.red12")+
    scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                  "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                  "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                  "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                  "CA"= "#FFD39B") , 
                       # Relabel  "Other.Plastic"                 
                       labels = c( "Other.Plastic"= "Other Plastics" ) ) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
          axis.title.x = element_blank())
  
  

  # * PLOT Size distribution  ####
  # Per Polymer12
  # One value per Size_cat.um
  
  S7a_SoilBlanks$Size_cat.um= factor(S7a_SoilBlanks$Size_cat.um,
                              levels =  Cat.um.txt )
  # Add percentages in the plot
  S7f_SoilBlanks=subset(S7f_SoilBlanks, Size_cat.um %in%  Cat.um.txt) 
  
  S7f_SoilBlanks = S7f_SoilBlanks %>%
    #group_by(Lab) %>%
    mutate(
      MiP_cat_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
      MiP_cat_perc_text=paste0(round(MiP_cat_perc, 0), "%") ) # Creat percentage as text  
  
 
  
  
  S7a_SoilBlanks=subset(S7a_SoilBlanks, Size_cat.um %in%  Cat.um.txt)
  S7a_SoilBlanks$Size_cat.um= factor(S7a_SoilBlanks$Size_cat.um,
                              levels =  Cat.um.txt )
  
  ggplot( ) +
    geom_bar(data=S7a_SoilBlanks, aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12), position="stack", stat="identity")+ 
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastics") ) +
    geom_text(data= S7f_SoilBlanks, aes(x=Size_cat.um,  y=Mean.particles.MM*200, label =  MiP_cat_perc_text), vjust = 1, nudge_y = 10)+
    ggtitle("Soil blanks, Micoplastic size distribution")+
    theme_minimal()+
    labs(y = "Average number of plastic particles per kg of soil",
         x= "Size categories [µm]",
         fill =  "Polymers identified") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
  
  
  # per lab
  
  S7a_SoilBlanks2$Size_cat.um= factor(S7a_SoilBlanks2$Size_cat.um,
                               levels =  Cat.um.txt )
  # Add percentages in the plot
  S7a_SoilBlanks2=subset(S7a_SoilBlanks2, Size_cat.um %in%  Cat.um.txt)
  S7f_SoilBlanks2=subset(S7f_SoilBlanks2, Size_cat.um %in%  Cat.um.txt)
  
  S7f_SoilBlanks2 = S7f_SoilBlanks2 %>%
    group_by(Lab) %>%
    mutate(
      MiP_cat_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
      MiP_cat_perc_text=paste0(round(MiP_cat_perc, 0), "%") ) # Creat percentage as text  
  
  
  
  
  
  S7a_SoilBlanks2$Size_cat.um= factor(S7a_SoilBlanks2$Size_cat.um,
                               levels =  Cat.um.txt )
  
  ggplot( ) +
    facet_wrap(~Lab)+
    geom_bar(data=S7a_SoilBlanks2, aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12), position="stack", stat="identity")+ 
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastics") ) +
    geom_text(data= S7f_SoilBlanks2, aes(x=Size_cat.um,  y=Mean.particles.MM*200, label =  MiP_cat_perc_text), vjust = 1, nudge_y =10)+
    ggtitle("Soil Banks, Micoplastic size distribution")+
    theme_minimal()+
    labs(y = "Average number of plastic particles per kg of soil",
         x= "Size categories [µm]",
         fill =  "Polymers identified") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
  
  
  
  # * PLOT Polymer composition ####
  
  # Order plot per polymer   
  S7c_SoilBlanks$Polymer.red12 = factor(S7c_SoilBlanks$Polymer.red12,
                                 levels = unique(S7c_SoilBlanks$Polymer.red12[order(-S7c_SoilBlanks$Mean.particles.MM)] ))
  
  # Add percentages in the plot
  S7c_SoilBlanks = S7c_SoilBlanks %>%
    group_by(Lab) %>%
    mutate(
      MiP_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
      MiP_perc_text=paste0(round(MiP_perc, 0), "%") ) # Creat percentage as text  
  
  
  # Do not show percentages < 3.5% 
  S7c_SoilBlanks$MiP_perc_text [S7c_SoilBlanks$MiP_perc < 3.5]=NA 
  
  S7c_SoilBlanks=subset(S7c_SoilBlanks, Mean.particles.MM!=0)
  
  ggplot(S7c_SoilBlanks, aes(x=Polymer.red12, y=Mean.particles.MM, fill= Polymer.red12 ))+
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
    ggtitle("Soil Blanks, Polymer composition")+
    # geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
    geom_text(aes(label =  S7c_SoilBlanks$MiP_perc_text), vjust = 1, nudge_y = 0.05)+
    #ggtitle(paste("Field Samples ; CSS ", css))+
    # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
      fill = "Polymers identified") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  
  

  
# 3. Standard soils ####
  # Create dataframe 
  df_st=subset(Data_comb_red_blank, Soil_sample=="st")
  
  #list of unique files 
  unique(df_st$File_Names)
  
  # Summary per Filter :  
  S3a_st=subset(Summary3a_Filter, Soil_sample=="st")
  S3e_st=subset(Summary3e_Filter, Soil_sample=="st")
  S3c_st=subset(Summary3c_Filter, Soil_sample=="st")
  
  S3f_st=subset(Summary3f_Filter, Soil_sample=="st")
  
  S7a_st= S3a_st %>%
    group_by(Preparation_Type,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  
  S7a_st2= S3a_st %>%
    group_by(Preparation_Type, Lab,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  S7c_st= S3c_st %>%
    group_by(Preparation_Type, Lab,
             Polymer.red12,  Polymer.red3 ) %>%
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
  
  S7f_st =S3f_st %>%
    group_by(Preparation_Type, 
             Size_cat.um  ) %>%
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
  
  S7f_st2 =S3f_st %>%
    group_by(Preparation_Type, Lab,
             Size_cat.um  ) %>%
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
  
  
  
  # * PLOT All filters, Sum of all polymers  ####
  ggplot( S3e_st, aes(x=Extraction_Name, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
    scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
    scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
    ggtitle("Soil standard, sum all Polymers ")+
    geom_point()+
    theme_minimal()+
    scale_y_continuous(breaks = seq(0, max(S3e_st$Mean.particles), by = 2),
            minor_breaks = seq(0, max(S3e_st$Mean.particles), 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
          axis.title.x = element_blank())
  
  # * PLOT All filters,  polymer 12 ####
  S3c_st$Lab_Batch=paste( S3c_st$Lab, "CSS", S3c_st$Batch_Name )
  ggplot(  S3c_st, aes( Extraction_Name, Mean.particles)) +
    facet_wrap(~Lab,scales="free_x",nrow=1)+
    geom_jitter(height=0,width=0.33, aes(color=Polymer.red12, shape=Lab ), size=3)+
    ggtitle("Soil standard, Polymer.red12")+
    scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                  "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                  "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                  "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                  "CA"= "#FFD39B") , 
                       # Relabel  "Other.Plastic"                 
                       labels = c( "Other.Plastic"= "Other Plastics" ) ) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
          axis.title.x = element_blank())
  
  
  # * PLOT Size distribution  ####
  # Per Polymer12
  # One value per Size_cat.um
  
  S7a_st$Size_cat.um= factor(S7a_st$Size_cat.um,
                              levels =  Cat.um.txt )
  # Add percentages in the plot
  S7f_st = S7f_st %>%
    #group_by(Lab) %>%
    mutate(
      MiP_cat_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
      MiP_cat_perc_text=paste0(round(MiP_cat_perc, 0), "%") ) # Creat percentage as text  
  
  S7f_st=subset(S7f_st, Size_cat.um %in%  Cat.um.txt)
  
  
  S7a_st=subset(S7a_st, Size_cat.um %in%  Cat.um.txt)
  S7a_st$Size_cat.um= factor(S7a_st$Size_cat.um,
                              levels =  Cat.um.txt )
  
  ggplot( ) +
    geom_bar(data=S7a_st, aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12), position="stack", stat="identity")+ 
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastics") ) +
    geom_text(data= S7f_st, aes(x=Size_cat.um,  y=Mean.particles.MM*200, label =  MiP_cat_perc_text), vjust = 1, nudge_y = 10)+
    ggtitle("Soil standard, Micoplastic size distribution")+
    theme_minimal()+
    labs(y = "Average number of plastic particles per kg of soil",
         x= "Size categories [µm]",
         fill =  "Polymers identified") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
  
  
  # per lab
  
  S7a_st2$Size_cat.um= factor(S7a_st2$Size_cat.um,
                               levels =  Cat.um.txt )
  # Add percentages in the plot
  S7a_st2=subset(S7a_st2, Size_cat.um %in%  Cat.um.txt)
  S7f_st2=subset(S7f_st2, Size_cat.um %in%  Cat.um.txt)
  
  S7f_st2 = S7f_st2 %>%
    group_by(Lab) %>%
    mutate(
      MiP_cat_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
      MiP_cat_perc_text=paste0(round(MiP_cat_perc, 0), "%") ) # Creat percentage as text  
  
  
  
  
  
  S7a_st2$Size_cat.um= factor(S7a_st2$Size_cat.um,
                               levels =  Cat.um.txt )
  
  ggplot( ) +
    facet_wrap(~Lab)+
    geom_bar(data=S7a_st2, aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12), position="stack", stat="identity")+ 
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastics") ) +
    geom_text(data= S7f_st2, aes(x=Size_cat.um,  y=Mean.particles.MM*200, label =  MiP_cat_perc_text), vjust = 1, nudge_y = 10)+
    ggtitle("Soil standard, Micoplastic size distribution")+
    theme_minimal()+
    labs(y = "Average number of plastic particles per kg of soil",
         x= "Size categories [µm]",
         fill =  "Polymers identified") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
  
  
  
  # * PLOT Polymer composition ####
  
  # Order plot per polymer   
  S7c_st$Polymer.red12 = factor(S7c_st$Polymer.red12,
                                 levels = unique(S7c_st$Polymer.red12[order(-S7c_st$Mean.particles.MM)] ))
  
  # Add percentages in the plot
  S7c_st = S7c_st %>%
    group_by(Lab) %>%
    mutate(
      MiP_perc = Mean.particles.MM / sum(Mean.particles.MM) * 100,
      MiP_perc_text=paste0(round(MiP_perc, 0), "%") ) # Creat percentage as text  
  
  
  # Do not show percentages < 3.5% 
  S7c_st$MiP_perc_text [S7c_st$MiP_perc < 3.5]=NA 
  
  S7c_st=subset(S7c_st, Mean.particles.MM!=0)
  
  ggplot(S7c_st, aes(x=Polymer.red12, y=Mean.particles.MM, fill= Polymer.red12 ))+
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
    ggtitle("Soil standard, Polymer composition")+
    # geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
    geom_text(aes(label =  S7c_st$MiP_perc_text), vjust = 1, nudge_y = 0.05)+
    #ggtitle(paste("Field Samples ; CSS ", css))+
    # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
      fill = "Polymers identified") +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank())
  
  
  
# 4.  Soil samples replicats ####
    # Create dataframe 
  
    
   # Get all the soils that appear more than one in the IR files list: 

  list_replicat=unique(Summary2e_IRfiles$Soil_sample[duplicated(Summary2e_IRfiles$Soil_sample)])
  length(list_replicat)
  
  list_replicat2=table(Summary2e_IRfiles$Soil_sample)[order(table(Summary2e_IRfiles$Soil_sample))]
  
  list_replicat2=list_replicat2[list_replicat2>1]
  
  list_replicat2_n=table(Summary2e_IRfiles$Soil_sample[Summary2e_IRfiles$Preparation_Type!="Spiked"])[order(table(Summary2e_IRfiles$Soil_sample[Summary2e_IRfiles$Preparation_Type!="Spiked"]))]
  list_replicat2_n=list_replicat2_n[list_replicat2_n>1]
  
  #list of unique files 
  unique(df_Replicat$File_Names)
  length(unique(df_Replicat$File_Names))
  
  unique(df_Replicat2$File_Names)
  length(unique(df_Replicat2$File_Names))
  
  unique(df_Replicat2$File_Names[df_Replicat2$File_Names %in% list_replicat & df_Replicat2$Preparation_Type!="Spiked"])
  
  # Summary per PMF File: 
  S1h_r=subset(Summary1h_File, Soil_sample %in% list_replicat & Preparation_Type!="Spiked")
  S1i_r=subset(Summary1i_File, Soil_sample %in% list_replicat & Preparation_Type!="Spiked")
  
  # Summary per Filter :  
  S3a_r=subset(Summary3a_Filter, Soil_sample %in% list_replicat & Preparation_Type!="Spiked")
  S3e_r=subset(Summary3e_Filter, Soil_sample %in% list_replicat & Preparation_Type!="Spiked")
  S3c_r=subset(Summary3c_Filter, Soil_sample %in% list_replicat & Preparation_Type!="Spiked")
  S3c_r$Lab_Batch=paste( S3c_r$Lab, "CSS", S3c_st$Batch_Name )
  
  S3f_r=subset(Summary3f_Filter, Soil_sample %in% list_replicat & Preparation_Type!="Spiked")
  
  S7a_r= S3a_r %>%
    group_by(Preparation_Type,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  
  S7a_r2= S3a_r %>%
    group_by(Preparation_Type, Lab,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  S7c_r= S3c_r %>%
    group_by(Preparation_Type, Lab,
             Polymer.red12,  Polymer.red3 ) %>%
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
  
  S7f_r =S3f_r %>%
    group_by(Preparation_Type, 
             Size_cat.um  ) %>%
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
  
  S7f_r2 =S3f_r %>%
    group_by(Preparation_Type, Lab,
             Size_cat.um  ) %>%
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
  
  
  
  # Plot the results 
  for (s in  list_replicat){
    S3e_plot= subset(S3e_r, Soil_sample==s )
    S3c_plot= subset(S3c_r, Soil_sample==s )
  
    # * PLOT All filters, Sum of all polymers  ####

    PLOT=ggplot( S3e_plot, aes(x=Extraction_Name, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
      scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
      scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
      ggtitle(paste(s, "Sum all Polymers"))+
      geom_point()+
      theme_minimal()+
      scale_y_continuous(breaks = seq(0, max(S3e_plot$Mean.particles), by = 5),
                         minor_breaks = seq(0, max(S3e_plot$Mean.particles), 1)) +
      expand_limits(y=0)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
    
    print(PLOT)
    
    
    # * PLOT Dots,  All filters,  polymer 12 ####
    
    PLOT=ggplot( S3c_plot, aes( Extraction_Name, Mean.particles)) +
      geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
      ggtitle("Particles summ per Polymer.red12 per batch")+
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                    "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                    "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                    "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                    "CA"= "#FFD39B") , 
                         # Relabel  "Other.Plastic"                 
                         labels = c( "Other.Plastic"= "Other Plastics" ) ) +
      ggtitle(paste(s, "Sum all Polymers"))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
    
    print(PLOT)
    
    
  }
  
  
  

# 5. Spiked soils ####
  
  Polymer_Spiked_list=
  
  # Create dataframe 
  df_Spiked=subset(Data_comb_red_blank, Preparation_Type=="Spiked")
  
  Soil_Spiked_list=unique(df_Spiked$Soil_sample)

  #list of unique files 
  unique(df_Spiked$File_Names)
  
  # Summary per PMF File: 
  S1h_s=subset(Summary1h_File, Preparation_Type=="Spiked")
  S1i_s=subset(Summary1i_File, Preparation_Type=="Spiked")
  
  unique(S1h_s$File_Names[S1h_s$Lab=="WUR"])
  length(unique(S1h_s$File_Names[S1h_s$Lab=="WUR"]))
  length(unique(S1i_s$File_Names[S1i_s$Lab=="WUR"]))
  
  
  
  # Summary per Filter :
  
  S1e_s=subset(Summary1e_File, Preparation_Type=="Spiked")
  S1c_s=subset(Summary1c_File, Preparation_Type=="Spiked")
  
  S2e_s=subset(Summary2e_IRfiles, Preparation_Type=="Spiked")
  
  S3a_s=subset(Summary3a_Filter, Preparation_Type=="Spiked")
  S3e_s=subset(Summary3e_Filter, Preparation_Type=="Spiked")

  
  
  S3c_s=subset(Summary3c_Filter, Preparation_Type=="Spiked")
  S3c_s$Lab_Batch=paste( S3c_s$Lab, "CSS", S3c_s$Batch_Name )
  
  S3f_s=subset(Summary3f_Filter, Preparation_Type=="Spiked")
  
  S7a_s= S3a_s %>%
    group_by(Preparation_Type,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  
  S7a_s2= S3a_s %>%
    group_by(Preparation_Type, Lab,
             Polymer.grp, Polymer.red12,  Polymer.red3, Size_cat.um  ) %>%
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
  
  S7c_s= S3c_s %>%
    group_by(Preparation_Type, Lab,
             Polymer.red12,  Polymer.red3 ) %>%
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
  
  S7f_s =S3f_s %>%
    group_by(Preparation_Type, 
             Size_cat.um  ) %>%
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
  
  S7f_s2 =S3f_s %>%
    group_by(Preparation_Type, Lab,
             Size_cat.um  ) %>%
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
  
  
  
  
  
  
  # Plot the results 
  for (s in  Soil_Spiked_list){
    S3e_plot= subset(S3e_s, Soil_sample==s )
    S2e_plot= subset(S2e_s, Soil_sample==s )
    
    S3c_plot= subset(S3c_s, Soil_sample==s )
    
    S1e_plot=subset(S1e_s, Soil_sample==s )
    S1c_plot=subset(S1c_s, Soil_sample==s )
    
    # * PLOT PMF files, Sum of all polymers  ####
    
    ggplot( S1e_plot, aes(x=File_Names, y=N.particles, color=Lab, shape=Lab, size=Lab ))+
      scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
      scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
      ggtitle(paste("Spiked", s, "Sum all Polymers"))+
      geom_point()+
      theme_minimal()+
      scale_y_continuous(limits = c(0, max(S1e_plot$N.particles)),
                           breaks = seq(0, max(S1e_plot$N.particles), by = 20),
                         minor_breaks = seq(0, max(S1e_plot$N.particles), by=10)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
    
    # * PLOT PMF files,  polymer 12 ####
    
   ggplot( S1c_plot, aes( x=File_Names, y=N.particles)) +
      geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
      ggtitle("Particles summ per Polymer.red12 per batch")+
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                    "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                    "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                    "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                    "CA"= "#FFD39B") , 
                         # Relabel  "Other.Plastic"                 
                         labels = c( "Other.Plastic"= "Other Plastics" ) ) +
      ggtitle(paste("Spiked", s, "Sum all Polymers"))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
    
    
    
    # * PLOT All filters, Sum of all polymers  ####
    
    PLOT=ggplot( S3e_plot, aes(x=Extraction_Name, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
      scale_size_manual(values = c("WUR"=2.5,  "Ubern"=3))+
      scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
      ggtitle(paste("Spiked", s, "Sum all Polymers"))+
      geom_point()+
      theme_minimal()+
      scale_y_continuous(breaks = seq(0, max(S3e_plot$Mean.particles), by = 20),
                         minor_breaks = seq(0, max(S3e_plot$Mean.particles), by=10)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
    
    print(PLOT)
    
    
    # * PLOT  All filters,  polymer 12 ####
    
    PLOT=ggplot( S3c_plot, aes( Extraction_Name, Mean.particles)) +
      geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
      ggtitle("Particles summ per Polymer.red12 per batch")+
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                    "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                    "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                    "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                    "CA"= "#FFD39B") , 
                         # Relabel  "Other.Plastic"                 
                         labels = c( "Other.Plastic"= "Other Plastics" ) ) +
      ggtitle(paste("Spiked", s, "Sum all Polymers"))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
    
    print(PLOT)
    
    
  }
  
  
  
  
  
  
  
  
  
  
  # 6. Spiked in ####
  
  # 6.1. Load and harmonize ####
  Spike_in_WUR= read.csv("WUR_Data/Spike_in_20240722_IJResults.csv")
  Spike_in_WUR$File_Names=Spike_in_WUR$Label
  Spike_in_WUR= Read_MINAGRIS_label(Spike_in_WUR)
  Spike_in_WUR$Lab="WUR"
  Spike_in_WUR$Batch_Name=tolower(Spike_in_WUR$Batch_Name)
  Spike_in_WUR$Area.um2.cor= Spike_in_WUR$Area
  Spike_in_WUR$N.px=0
  Spike_in_WUR$N.px[Spike_in_WUR$Area.um2.cor>0]=1
  Spike_in_WUR$Mass.ng=0
  
  # * Create Polymer categories ####
    Spike_in_WUR$Polymer.grp="Polymer"
    Spike_in_WUR$Polymer.grp[grep("PE", Spike_in_WUR$File_Names, ignore.case = T)]="PE"
    Spike_in_WUR$Polymer.grp[grep("PLA", Spike_in_WUR$File_Names, ignore.case = T)]="PLA"
    Spike_in_WUR$Polymer.grp[grep("PVC", Spike_in_WUR$File_Names, ignore.case = T)]="PVC"
    Spike_in_WUR$Polymer.grp[grep("MP1", Spike_in_WUR$File_Names, ignore.case = T)]="MP1"
    Spike_in_WUR$Polymer.grp[grep("MP2", Spike_in_WUR$File_Names, ignore.case = T)]="MP2"
    length(  Spike_in_WUR$Polymer.grp[Spike_in_WUR$Polymer.grp=="PE"])
    length(  Spike_in_WUR$Polymer.grp[Spike_in_WUR$Polymer.grp=="PLA"])
    length(  Spike_in_WUR$Polymer.grp[Spike_in_WUR$Polymer.grp=="PVC"])
    length(  Spike_in_WUR$Polymer.grp[Spike_in_WUR$Polymer.grp=="Polymer"])
    
    # /!\ Rename Spike_in_WUR Polymer.grp 
    Spike_in_WUR$Polymer.grp[grep("MP1", Spike_in_WUR$File_Names, ignore.case = T)]="PE"
    Spike_in_WUR$Polymer.grp[grep("MP2", Spike_in_WUR$File_Names, ignore.case = T)]="PLA"
    
    
  Spike_in_WUR$Polymer.red12= Spike_in_WUR$Polymer.grp
  Spike_in_WUR$Polymer.red3=Spike_in_WUR$Polymer.grp

  
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
  
  
  
  Spike_in_WUR$Size_cat.um="Too small"
  # Build categories by successive replacement "upward"
  for (c in 1:length(Cat.um)){
    Spike_in_WUR$Size_cat.um[Spike_in_WUR$Area.um2.cor>Cat.um[c]^2]=Cat.um.txt[c]
  }
  
  # Label the "Too small"
  Spike_in_WUR[Spike_in_WUR$N.px>=1 & Spike_in_WUR$Length.um<86 & Spike_in_WUR$Width.um<86,]
  
  # Extend the size categories to include until 86um in "90-300"
  Spike_in_WUR$Size_cat.um[Spike_in_WUR$Size_cat.um=="Too small" & (Spike_in_WUR$Length.um>86 | Spike_in_WUR$Width.um>86) ]="90-300"
  Spike_in_WUR$Size_cat.um[Spike_in_WUR$N.px>=1 & Spike_in_WUR$Length.um<86 & Spike_in_WUR$Width.um<86]="Too small"
  
  # Label the "Too big"
  Spike_in_WUR[Spike_in_WUR$N.px>=1 & Spike_in_WUR$Length.um>2000 & Spike_in_WUR$Width.um>2000,]
  Spike_in_WUR$Size_cat.um[Spike_in_WUR$Area.um2.cor>cat.max^2]="Too big"
  
  nrow(Spike_in_WUR[Spike_in_WUR$N.px>=1 & Spike_in_WUR$Size_cat.um %in% c("Too small", "Too big") ,])
  Spike_in_WUR[Spike_in_WUR$N.px>=1 & Spike_in_WUR$Size_cat.um %in% c("Too small", "Too big") ,]
  
  
  head(Spike_in_WUR )
  
  # ImageJ summary : 
  # just for internal checking
  
  Spike_in_WUR_Sum= read.csv("WUR_Data/Spike_in_20240722_IJSummary.csv")
  Spike_in_WUR_Sum$File_Names=Spike_in_WUR_Sum$Slice
  Spike_in_WUR_Sum= Read_MINAGRIS_label(Spike_in_WUR_Sum)

    Spike_in_WUR_Sum$Polymer.grp="Polymer"
    Spike_in_WUR_Sum$Polymer.grp[grep("PE", Spike_in_WUR_Sum$File_Names, ignore.case = T)]="PE"
    Spike_in_WUR_Sum$Polymer.grp[grep("PLA", Spike_in_WUR_Sum$File_Names, ignore.case = T)]="PLA"
    Spike_in_WUR_Sum$Polymer.grp[grep("PVC", Spike_in_WUR_Sum$File_Names, ignore.case = T)]="PVC"
    Spike_in_WUR_Sum$Polymer.grp[grep("MP1", Spike_in_WUR_Sum$File_Names, ignore.case = T)]="MP1"
    Spike_in_WUR_Sum$Polymer.grp[grep("MP2", Spike_in_WUR_Sum$File_Names, ignore.case = T)]="MP2"
    length(  Spike_in_WUR_Sum$Polymer.grp[Spike_in_WUR_Sum$Polymer.grp=="PE"])
    length(  Spike_in_WUR_Sum$Polymer.grp[Spike_in_WUR_Sum$Polymer.grp=="PLA"])
    length(  Spike_in_WUR_Sum$Polymer.grp[Spike_in_WUR_Sum$Polymer.grp=="PVC"])

    length(  Spike_in_WUR_Sum$Polymer.grp[Spike_in_WUR_Sum$Polymer.grp=="Polymer"])
    
    unique(Spike_in_WUR$Size_cat.um)
    # /!\ "720-930", "930-1140",  "1140-1350", "Too big"  not in  Spike_in_WUR$Size_cat.um
    
  
  # * Polymer_Spiked_list ####
    Polymer_Spiked_list= unique(Spike_in_WUR_Sum$Polymer.grp)
    
    
  # 6.2. Summarize ####
    # * (3h)For each spike_in (filter), for each polymer 3 , for each size category ####
    Spike_in_WUR_3h= Spike_in_WUR %>% 
    group_by(File_Names, Lab, Batch_Name,   Sample_type, Soil_sample,          
             CSS, Farm, Field,     # IR_rep, PMF_rep to be included /!\
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
    complete(nesting(File_Names, Lab, Batch_Name,   Sample_type, Soil_sample,
                     CSS, Farm, Field ,
                     Polymer.grp,Polymer.red12, Polymer.red3), #, IR_rep, PMF_rep, to be included /!\,
             nesting( Size_cat.um), 
             fill=list(N.particles=0,
                       Num.px=0,
                       Tot.Area.mm2=0,
                       Median.Area.sqrt.um=0,
                       SD.Area=0)) %>%
    # Remove the "No.plastic", not needed anymore
    subset(Polymer.grp!="No.plastic")
  
  
  sum(  Spike_in_WUR_3h$N.particles)
  
   # * (3i) For each Spike_in (filter) , for each polymer 3 ####
    Spike_in_WUR_3i= Spike_in_WUR %>% 
      group_by(File_Names, Lab, Batch_Name,   Sample_type, Soil_sample,          
               CSS, Farm, Field,     # IR_rep, PMF_rep to be included /!\
               Polymer.grp, Polymer.red12,  Polymer.red3) %>%
      summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
                 Num.px=sum(N.px),           # Number of pixels
                 Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
                 Tot.Mass.ng=sum(Mass.ng),
                 Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
                 SD.Area=sd(Area.um2.cor))  %>%  #
      # Complete the Summary1_File: 
      # for each file I want all the polymers represented
      # ungroup() %>%
      # complete(nesting(File_Names, Lab, Batch_Name,   Sample_type, Soil_sample,
      #                  CSS, Farm, Field   ), #, IR_rep, PMF_rep, to be included /!\,
      #          nesting(Polymer.grp,Polymer.red12, Polymer.red3), 
      #          fill=list(N.particles=0,
      #                    Num.px=0,
      #                    Tot.Area.mm2=0,
      #                    Median.Area.sqrt.um=0,
      #                    SD.Area=0)) %>%
      # Remove the "No.plastic", not needed anymore
      subset(Polymer.grp!="No.plastic")
    
  
  # * (3i) For each Spike_in (filter) , for each polymer 3 with size filter  ####
  Spike_in_WUR_3i_red= subset(Spike_in_WUR, Area.um2.cor>60^2) %>% 
    group_by(File_Names, Lab, Batch_Name,   Sample_type, Soil_sample,          
             CSS, Farm, Field,     # IR_rep, PMF_rep to be included /!\
             Polymer.grp, Polymer.red12,  Polymer.red3) %>%
    summarise( N.particles= sum(N.px!=0),  # Number of particles (Sum of Binary)
               Num.px=sum(N.px),           # Number of pixels
               Tot.Area.mm2=sum(Area.um2.cor)/1000000, # Total plastic area converted in mm2
               Tot.Mass.ng=sum(Mass.ng),
               Median.Area.sqrt.um=sqrt(median(Area.um2.cor)),
               SD.Area=sd(Area.um2.cor))  %>%  #
    # Complete the Summary1_File: 
    # for each file I want all the polymers represented
    # ungroup() %>%
    # complete(nesting(File_Names, Lab, Batch_Name,   Sample_type, Soil_sample,
    #                  CSS, Farm, Field   ), #, IR_rep, PMF_rep, to be included /!\,
    #          nesting(Polymer.grp,Polymer.red12, Polymer.red3), 
    #          fill=list(N.particles=0,
    #                    Num.px=0,
    #                    Tot.Area.mm2=0,
    #                    Median.Area.sqrt.um=0,
    #                    SD.Area=0)) %>%
    # Remove the "No.plastic", not needed anymore
    subset(Polymer.grp!="No.plastic")
  
    
  
    # 6.3.  PLOT ####
    
    # * PLOT Size distribution  ####
    
    # Per Polymer3
    # One value per Size_cat.um
    
    # Add percentages in the plot
    
    # /!\ 1h is not the average /!\
    
    
    Spike_in_WUR_3h = Spike_in_WUR_3h %>%
      group_by(Polymer.red3) %>%
      mutate(
        MiP_cat_perc = N.particles / sum(N.particles) * 100,
        MiP_cat_perc_text=paste0(round(MiP_cat_perc, 0), "%") ) # Creat percentage as text  
    
    Spike_in_WUR_3h$Size_cat.um= factor(Spike_in_WUR_3h$Size_cat.um,
                                        levels =  c("Too small",Cat.um.txt, "Too big") )
    
    
    ggplot( data= Spike_in_WUR_3h, aes(x=Size_cat.um, y=N.particles, fill=Polymer.red12)) +
      facet_wrap(~Polymer.red3)+
      geom_bar( position="stack", stat="identity")+ 
      scale_fill_manual(values = c("PE"="#377EB8",  "PLA"="#A65628", "PVC"="#4DAF4A",           
                                   "MP1"="#999999",   "MP2"="#FFF8DC",  "Other.Plastic"="#E41A1C") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"="Other Plastics") ) +
      #geom_text(data= Spike_in_WUR_1h, aes(x=Size_cat.um,  y=N.particles, label =  MiP_cat_perc_text), vjust = 1, nudge_y = 10)+
      ggtitle("Spike-in, Micoplastic size distribution")+
      theme_minimal()+
      labs(y = "Average number of plastic particles per Sample",
           x= "Size categories [µm]",
           fill =  "Polymers identified") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )   
    
  
    
    
    
  # 7. Calculate Recovery ####
    # Recovery = ( Spike_out - Mean_Soil_out ) / Spike_in
    # for particle numbers and for Area
    # Spike_out = uFTIR measure of the spike sample per PMF file (Summary 1)
    # Mean_Soil_out = Average from the correponding soil replicat (Summary 4)
    # Spike_in = Stereo-microscope of the spiked plastic per prepared filter (Summary 3)
    
    
    
    # 7.1. For each File, for each polymer, for each size category ####
    
    # So We need to build a summary For each sample, for each polymer, (for each size category) 
    # => Recovery_WUR_1h=cbind( S1h_r, S1h_s, Spike_in_WUR_1h)
    
    # * 1. Add the Spike out (Summary1h_File) ####
    Recovery_WUR_1h=subset(Summary1h_File, Lab=="WUR" & Preparation_Type == "Spiked" & Polymer.red3 %in% Polymer_Spiked_list)
  
    
    # * 2. Add the Mean env soil (4h) ####
      # for each Soil_sample (4h), Polymer red 3, Size cat
    
    Recovery_WUR_1h$N.particles_Field=0
    Recovery_WUR_1h$Tot.Area.mm2_Field=0
    q=1
    
    # soil = Soil_Spiked_list[1]
    # pol = Polymer_Spiked_list[1]
    # cat = Cat.um.txt[1]
    
    for (soil in Soil_Spiked_list){
      for (pol in Polymer_Spiked_list){
        for (cat in Cat.um.txt){
          # A=subset(Summary4b_Soil, Soil_sample==soil & Polymer.red12==pol & Size_cat.um==cat, select = Mean.particles )
           AA=subset(Summary4b_Soil, Soil_sample==soil & Polymer.red12==pol & Size_cat.um==cat, select = Mean.Tot.Area.mm2)[[1]]
          # print(soil)
          # print(pol)
          # print(cat)
           print(AA)
          # print(q)
          # q=q+1
          
          Recovery_WUR_1h$N.particles_Field [Recovery_WUR_1h$Soil_sample==soil & Recovery_WUR_1h$Polymer.red3==pol & Recovery_WUR_1h$Size_cat.um==cat]=
            subset(Summary4b_Soil, Soil_sample==soil & Polymer.red12==pol & Size_cat.um==cat, select = Mean.particles )[[1]]
          
          Recovery_WUR_1h$Tot.Area.mm2_Field[Recovery_WUR_1h$Soil_sample==soil & Recovery_WUR_1h$Polymer.red3==pol & Recovery_WUR_1h$Size_cat.um==cat]=
            subset(Summary4b_Soil, Soil_sample==soil & Polymer.red12==pol & Size_cat.um==cat, select = Mean.Tot.Area.mm2 ) [[1]]
        }
      }
    }
    
    A=subset( Recovery_WUR_1h, N.particles_Field==-1)
    AA=subset( Recovery_WUR_1h, Tot.Area.mm2_Field==-1)
    min(Summary4b_Soil$Mean.Tot.Area.mm2)
  
    #  * 3. Add the Mean spiked in (3h) ####
    # *** for each filter (3h), Polymer red 3, Size cat ####
    Recovery_WUR_1h$N.particles_in=0
    Recovery_WUR_1h$Tot.Area.mm2_in=0
    
    # f = unique(Spike_in_WUR_3h$File_Names)[1]
    # pol = Polymer_Spiked_list[1]
    # cat = Cat.um.txt[1]
    q=1
    for (f in unique(Spike_in_WUR_3h$File_Names)){ # *60
      for (pol in Polymer_Spiked_list){            # *5
        for (cat in c(Cat.um.txt, "Too small", "Too big")){                   # *6
          # R=subset( Recovery_WUR_1h, Batch_Name == Spike_in_WUR_3h$Batch_Name[Spike_in_WUR_3h$File_Names==f] &
          #              Sample_type==Spike_in_WUR_3h$Sample_type[Spike_in_WUR_3h$File_Names==f] &
          #             CSS==Spike_in_WUR_3h$CSS[Spike_in_WUR_3h$File_Names==f] &
          #             Farm==Spike_in_WUR_3h$Farm[Spike_in_WUR_3h$File_Names==f] &
          #             Field==Spike_in_WUR_3h$Field[Spike_in_WUR_3h$File_Names==f] &
          #               Polymer.red3==pol & Size_cat.um==cat, select =  N.particles_in )
          # B= subset( Spike_in_WUR_3h, File_Names == f & Polymer.red3==pol & Size_cat.um==cat, select = Tot.Area.mm2 )[[1]]
          # # print(soil)
          # # print(pol)
          # # print(cat)
          #   print(B)
          #   # print(q)
          #   # q=q+1
           
          if(length(subset( Spike_in_WUR_3h, File_Names == f & Polymer.red3==pol & Size_cat.um==cat, select = Tot.Area.mm2 )[[1]])>0){
          Recovery_WUR_1h$N.particles_in[Recovery_WUR_1h$Batch_Name == unique(Spike_in_WUR_3h$Batch_Name[Spike_in_WUR_3h$File_Names==f]) &
                                         Recovery_WUR_1h$Sample_type==unique(Spike_in_WUR_3h$Sample_type[Spike_in_WUR_3h$File_Names==f]) &
                                         Recovery_WUR_1h$CSS==unique(Spike_in_WUR_3h$CSS[Spike_in_WUR_3h$File_Names==f]) &
                                         Recovery_WUR_1h$Farm==unique(Spike_in_WUR_3h$Farm[Spike_in_WUR_3h$File_Names==f]) &
                                         Recovery_WUR_1h$Field==unique(Spike_in_WUR_3h$Field[Spike_in_WUR_3h$File_Names==f]) &
                                         Recovery_WUR_1h$Polymer.red3==pol &
                                         Recovery_WUR_1h$Size_cat.um==cat]=
            as.numeric(subset( Spike_in_WUR_3h, File_Names == f & Polymer.red3==pol & Size_cat.um==cat, select =  N.particles )[[1]])
          
            Recovery_WUR_1h$Tot.Area.mm2_in[Recovery_WUR_1h$Batch_Name ==  unique(Spike_in_WUR_3h$Batch_Name[Spike_in_WUR_3h$File_Names==f]) &
                                              Recovery_WUR_1h$Sample_type==unique(Spike_in_WUR_3h$Sample_type[Spike_in_WUR_3h$File_Names==f]) &
                                              Recovery_WUR_1h$CSS==unique(Spike_in_WUR_3h$CSS[Spike_in_WUR_3h$File_Names==f]) &
                                              Recovery_WUR_1h$Farm==unique(Spike_in_WUR_3h$Farm[Spike_in_WUR_3h$File_Names==f]) &
                                              Recovery_WUR_1h$Field==unique(Spike_in_WUR_3h$Field[Spike_in_WUR_3h$File_Names==f]) &
                                              Recovery_WUR_1h$Polymer.red3==pol &
                                              Recovery_WUR_1h$Size_cat.um==cat]=
              subset( Spike_in_WUR_3h, File_Names == f & Polymer.red3==pol & Size_cat.um==cat, select = Tot.Area.mm2 )[[1]]
          }
        }
      }
    }
    
    # B=subset( Recovery_WUR_1h, N.particles_in==-1)
    # B=subset( Recovery_WUR_1h, N.particles_in==-1 & Size_cat.um %!in% c("1140-1350", "720-930", "930-1140" , "Too big"))
    # unique(B$File_Names)
    # 
    # f="M10_451_s2_PE-HDx15.png"
    # pol="PE"
    # cat="Too small"
    # 
    # subset( Spike_in_WUR_3h, CSS==7 & Farm==1 &Field==1 & Batch_Name=="m10" & Polymer.red3=="PE" & Size_cat.um=="510-720" )
    # C=subset( Spike_in_WUR_3h, CSS==7 & Farm==1 &Field==1 & Batch_Name=="m15" & Polymer.red3=="PE")
    
    #  * 4. Calculate recovery ####

    Recovery_WUR_1h$n_Recovery=(Recovery_WUR_1h$N.particles-Recovery_WUR_1h$N.particles_Field)/Recovery_WUR_1h$N.particles_in
    Recovery_WUR_1h$Area_Recovery=(Recovery_WUR_1h$Tot.Area.mm2-Recovery_WUR_1h$Tot.Area.mm2_Field)/Recovery_WUR_1h$Tot.Area.mm2_in
    
  
    # 7.2. For each File, for each polymer ####
    
    # So We need to build a summary For each sample, for each polymer
    # => Recovery_WUR_1i=cbind( S1h_r, S1h_s, Spike_in_WUR_1h)
    
    # * 1. Add the Spike out (Summary1h_File) ####
    Recovery_WUR_1i=subset(Summary1i_File, Lab=="WUR" & Preparation_Type == "Spiked" & Polymer.red3 %in% Polymer_Spiked_list)
    
    
    # * 2. Add the Mean env soil (4h) ####
    # for each Soil_sample (4h), Polymer red 3, Size cat
    
    Recovery_WUR_1i$N.particles_Field=0
    Recovery_WUR_1i$Tot.Area.mm2_Field=0
    q=1
    
    # soil = Soil_Spiked_list[1]
    # pol = Polymer_Spiked_list[1]
    # cat = Cat.um.txt[1]
    
    for (soil in Soil_Spiked_list){
      for (pol in Polymer_Spiked_list){
          # A=subset(Summary4b_Soil, Soil_sample==soil & Polymer.red12==pol, select = Mean.particles )
          #AA=subset(Summary4b_Soil, Soil_sample==soil & Polymer.red12==pol, select = Mean.Tot.Area.mm2)[[1]]
          # print(soil)
          # print(pol)
          #print(AA)
          # print(q)
          # q=q+1
          
          Recovery_WUR_1i$N.particles_Field [Recovery_WUR_1i$Soil_sample==soil & Recovery_WUR_1i$Polymer.red3==pol ]=
            subset(Summary4c_Soil, Soil_sample==soil & Polymer.red12==pol , select = Mean.particles )[[1]]
          
          Recovery_WUR_1i$Tot.Area.mm2_Field[Recovery_WUR_1i$Soil_sample==soil & Recovery_WUR_1i$Polymer.red3==pol ]=
            subset(Summary4c_Soil, Soil_sample==soil & Polymer.red12==pol , select = Mean.Tot.Area.mm2 ) [[1]]
        }
      }
    
    
    # A=subset( Recovery_WUR_1i, N.particles_Field==-1)
    # AA=subset( Recovery_WUR_1i, Tot.Area.mm2_Field==-1)
    min(Summary4b_Soil$Mean.Tot.Area.mm2)
    
    #  * 3. Add the Mean spiked in (3h) ####
    # for each filter (3h), Polymer red 3, Size cat
    Recovery_WUR_1i$N.particles_in=0
    Recovery_WUR_1i$Tot.Area.mm2_in=0
    
    # f = unique(Spike_in_WUR_3i$File_Names)[1]
    # pol = Polymer_Spiked_list[1]

    
    q=1
    for (f in unique(Spike_in_WUR_3i$File_Names)){ # *60
      for (pol in Polymer_Spiked_list){            # *5            
          # R=subset( Recovery_WUR_1i, Batch_Name == Spike_in_WUR_3i$Batch_Name[Spike_in_WUR_3i$File_Names==f] &
          #              Sample_type==Spike_in_WUR_3i$Sample_type[Spike_in_WUR_3i$File_Names==f] &
          #             CSS==Spike_in_WUR_3i$CSS[Spike_in_WUR_3i$File_Names==f] &
          #             Farm==Spike_in_WUR_3i$Farm[Spike_in_WUR_3i$File_Names==f] &
          #             Field==Spike_in_WUR_3i$Field[Spike_in_WUR_3i$File_Names==f] &
          #               Polymer.red3==pol , select =  N.particles_in )
          # B= subset( Spike_in_WUR_3i, File_Names == f & Polymer.red3==pol & Size_cat.um==cat, select = Tot.Area.mm2 )[[1]]
          # # print(soil)
          # # print(pol)
          # # print(cat)
          #   print(B)
          #   # print(q)
          #   # q=q+1
          
          if(length(subset( Spike_in_WUR_3i, File_Names == f & Polymer.red3==pol, select = Tot.Area.mm2 )[[1]])>0){
            Recovery_WUR_1i$N.particles_in[Recovery_WUR_1i$Batch_Name == unique(Spike_in_WUR_3i$Batch_Name[Spike_in_WUR_3i$File_Names==f]) &
                                             Recovery_WUR_1i$Sample_type==unique(Spike_in_WUR_3i$Sample_type[Spike_in_WUR_3i$File_Names==f]) &
                                             Recovery_WUR_1i$CSS==unique(Spike_in_WUR_3i$CSS[Spike_in_WUR_3i$File_Names==f]) &
                                             Recovery_WUR_1i$Farm==unique(Spike_in_WUR_3i$Farm[Spike_in_WUR_3i$File_Names==f]) &
                                             Recovery_WUR_1i$Field==unique(Spike_in_WUR_3i$Field[Spike_in_WUR_3i$File_Names==f]) &
                                             Recovery_WUR_1i$Polymer.red3==pol ]=
              as.numeric(subset( Spike_in_WUR_3i, File_Names == f & Polymer.red3==pol , select =  N.particles )[[1]])
            
            Recovery_WUR_1i$Tot.Area.mm2_in[Recovery_WUR_1i$Batch_Name ==  unique(Spike_in_WUR_3i$Batch_Name[Spike_in_WUR_3i$File_Names==f]) &
                                              Recovery_WUR_1i$Sample_type==unique(Spike_in_WUR_3i$Sample_type[Spike_in_WUR_3i$File_Names==f]) &
                                              Recovery_WUR_1i$CSS==unique(Spike_in_WUR_3i$CSS[Spike_in_WUR_3i$File_Names==f]) &
                                              Recovery_WUR_1i$Farm==unique(Spike_in_WUR_3i$Farm[Spike_in_WUR_3i$File_Names==f]) &
                                              Recovery_WUR_1i$Field==unique(Spike_in_WUR_3i$Field[Spike_in_WUR_3i$File_Names==f]) &
                                              Recovery_WUR_1i$Polymer.red3==pol]=
              subset( Spike_in_WUR_3i, File_Names == f & Polymer.red3==pol, select = Tot.Area.mm2 )[[1]]
          }
        }
    }
    
    # B=subset( Recovery_WUR_1i, N.particles_in==-1)
    # unique(B$File_Names)
    # 
    # f="M10_451_s2_PE-HDx15.png"
    # pol="PE"
    # 
    # subset( Spike_in_WUR_3i, CSS==7 & Farm==1 &Field==1 & Batch_Name=="m10" & Polymer.red3=="PE" )
    # C=subset( Spike_in_WUR_3i, CSS==7 & Farm==1 &Field==1 & Batch_Name=="m15" & Polymer.red3=="PE")
    
    #  * 3. Add the Mean spiked in (3h_red) ####
    # for each filter (3h), Polymer red 3, Size cat
    Recovery_WUR_1i$N.particles_in_red=0
    Recovery_WUR_1i$Tot.Area.mm2_in_red=0
    
    # f = unique(Spike_in_WUR_3i$File_Names)[1]
    # pol = Polymer_Spiked_list[1]
    
    
    q=1
    for (f in unique(Spike_in_WUR_3i_red$File_Names)){ # *60
      for (pol in Polymer_Spiked_list){            # *5            
        # R=subset( Recovery_WUR_1i, Batch_Name == Spike_in_WUR_3i$Batch_Name[Spike_in_WUR_3i$File_Names==f] &
        #              Sample_type==Spike_in_WUR_3i$Sample_type[Spike_in_WUR_3i$File_Names==f] &
        #             CSS==Spike_in_WUR_3i$CSS[Spike_in_WUR_3i$File_Names==f] &
        #             Farm==Spike_in_WUR_3i$Farm[Spike_in_WUR_3i$File_Names==f] &
        #             Field==Spike_in_WUR_3i$Field[Spike_in_WUR_3i$File_Names==f] &
        #               Polymer.red3==pol , select =  N.particles_in )
        # B= subset( Spike_in_WUR_3i, File_Names == f & Polymer.red3==pol & Size_cat.um==cat, select = Tot.Area.mm2 )[[1]]
        # # print(soil)
        # # print(pol)
        # # print(cat)
        #   print(B)
        #   # print(q)
        #   # q=q+1
        
        if(length(subset( Spike_in_WUR_3i_red, File_Names == f & Polymer.red3==pol, select = Tot.Area.mm2 )[[1]])>0){
          Recovery_WUR_1i$N.particles_in_red[Recovery_WUR_1i$Batch_Name == unique(Spike_in_WUR_3i_red$Batch_Name[Spike_in_WUR_3i_red$File_Names==f]) &
                                           Recovery_WUR_1i$Sample_type==unique(Spike_in_WUR_3i_red$Sample_type[Spike_in_WUR_3i_red$File_Names==f]) &
                                           Recovery_WUR_1i$CSS==unique(Spike_in_WUR_3i_red$CSS[Spike_in_WUR_3i_red$File_Names==f]) &
                                           Recovery_WUR_1i$Farm==unique(Spike_in_WUR_3i_red$Farm[Spike_in_WUR_3i_red$File_Names==f]) &
                                           Recovery_WUR_1i$Field==unique(Spike_in_WUR_3i_red$Field[Spike_in_WUR_3i_red$File_Names==f]) &
                                           Recovery_WUR_1i$Polymer.red3==pol ]=
            as.numeric(subset( Spike_in_WUR_3i_red, File_Names == f & Polymer.red3==pol , select =  N.particles )[[1]])
          
          Recovery_WUR_1i$Tot.Area.mm2_in_red[Recovery_WUR_1i$Batch_Name ==  unique(Spike_in_WUR_3i_red$Batch_Name[Spike_in_WUR_3i_red$File_Names==f]) &
                                            Recovery_WUR_1i$Sample_type==unique(Spike_in_WUR_3i_red$Sample_type[Spike_in_WUR_3i_red$File_Names==f]) &
                                            Recovery_WUR_1i$CSS==unique(Spike_in_WUR_3i_red$CSS[Spike_in_WUR_3i_red$File_Names==f]) &
                                            Recovery_WUR_1i$Farm==unique(Spike_in_WUR_3i_red$Farm[Spike_in_WUR_3i_red$File_Names==f]) &
                                            Recovery_WUR_1i$Field==unique(Spike_in_WUR_3i_red$Field[Spike_in_WUR_3i_red$File_Names==f]) &
                                            Recovery_WUR_1i$Polymer.red3==pol]=
            subset( Spike_in_WUR_3i_red, File_Names == f & Polymer.red3==pol, select = Tot.Area.mm2 )[[1]]
        }
      }
    }
    
    # B=subset( Recovery_WUR_1i, N.particles_in==-1)
    # unique(B$File_Names)
    # 
    # f="M10_451_s2_PE-HDx15.png"
    # pol="PE"
    # 
    # subset( Spike_in_WUR_3ii, CSS==7 & Farm==1 &Field==1 & Batch_Name=="m10" & Polymer.red3=="PE" )
    # C=subset( Spike_in_WUR_3ii, CSS==7 & Farm==1 &Field==1 & Batch_Name=="m15" & Polymer.red3=="PE")
    # 
    # 
    
    #  * 4. Calculate recovery ####
    
    Recovery_WUR_1i=subset( Recovery_WUR_1i, Recovery_WUR_1i$N.particles_in_red!=0)
    
    Recovery_WUR_1i$n_Recovery=(Recovery_WUR_1i$N.particles-Recovery_WUR_1i$N.particles_Field)/Recovery_WUR_1i$N.particles_in
    Recovery_WUR_1i$Area_Recovery=(Recovery_WUR_1i$Tot.Area.mm2-Recovery_WUR_1i$Tot.Area.mm2_Field)/Recovery_WUR_1i$Tot.Area.mm2_in
    
    Recovery_WUR_1i$n_Recovery_red=(Recovery_WUR_1i$N.particles-Recovery_WUR_1i$N.particles_Field)/Recovery_WUR_1i$N.particles_in_red
    Recovery_WUR_1i$Area_Recovery_red=(Recovery_WUR_1i$Tot.Area.mm2-Recovery_WUR_1i$Tot.Area.mm2_Field)/Recovery_WUR_1i$Tot.Area.mm2_in_red
  
    
    # * 5. Average ####
    
    Recovery_WUR_1i_mean_soil= Recovery_WUR_1i %>%
      group_by(Soil_sample, Polymer.red3, Sample_type) %>%
      summarise(nfiles=n(),
                N.particles_Field_M=mean(N.particles_Field),
                N.particles_in_red_M=mean(N.particles_in_red),
                n_Recovery_M= mean(n_Recovery), 
                Area_Recovery_M= mean(Area_Recovery),
                n_Recovery_red_M= mean(n_Recovery_red), 
                Area_Recovery_red_M= mean(Area_Recovery_red))
    
    Recovery_WUR_1i_mean= Recovery_WUR_1i %>%
      group_by( Polymer.red3, Sample_type) %>%
      summarise(nfiles=n(),
                N.particles_Field_M=mean(N.particles_Field),
                N.particles_in_red_M=mean(N.particles_in_red),
                n_Recovery_M= mean(n_Recovery), 
                Area_Recovery_M= mean(Area_Recovery),
                n_Recovery_red_M= mean(n_Recovery_red), 
                Area_Recovery_red_M= mean(Area_Recovery_red))
    
    
    # 6. Export #### 
    write.csv( Recovery_WUR_1i, paste(wd.out,"/Recovery_WUR_1i", Date, sep = ""))
    write.csv( Recovery_WUR_1i_mean_soil, paste(wd.out,"/Recovery_WUR_1i_mean_soil", Date, sep = ""))
    write.csv(Recovery_WUR_1i_mean, paste(wd.out,"/Recovery_WUR_1i_mean", Date, sep = ""))
    
    
    
    
    
    
    # 9. Operator checks #### 
    
    
    # Get all the Filters that appear more than one in the PMF files list (Summary1e_File): 
    
    list_replicat_operators=table(Summary1e_File$Extraction_Name)[order(table(Summary1e_File$Extraction_Name))]
    list_replicat_operators= list_replicat_operators[ list_replicat_operators>1]  
    list_replicat_operators= as.data.frame(list_replicat_operators)
    
    write.csv(list_replicat_operators, paste(wd.out,"/Recovery_WUR_1i_mean", Date, sep = ""))

    
    # Bin 
    A= Summary1e_File$Extraction_Name[duplicated(Summary1e_File$Extraction_Name)]
    B= unique(Summary1e_File$Extraction_Name[duplicated(Summary1e_File$Extraction_Name)])
    C= Summary1e_File[ Summary1e_File$Extraction_Name %in% A ,]
    
    B %in% A
    A %in% B
    
    list_replicat_operators= as.data.frame(list_replicat_operators)
    
    D=list_replicat_operators$Var1 [list_replicat_operators$Var1 %in% B]
    
    C
    unique(C$ File_Names)
   
    
    
    
    # End Bin 
    
    
    list_replicat_operators_WUR= 
    
    
    Summary1e_File$File_Names[duplicated(Summary1e_File$Extraction_Name)]
    
    
    Summary1e_File$Extraction_Name[duplicated(Summary1e_File$Extraction_Name)]
    
    
    Summary1e_File$File_Names[duplicated(Summary1e_File$Extraction_Name)]
    
    
    
    
    
    length(unique(Summary1e_File$Soil_sample[duplicated(Summary1e_File$Soil_sample)]))
    
    
    
    list_replicat2=table(Summary2e_IRfiles$Soil_sample)[order(table(Summary2e_IRfiles$Soil_sample))]
    
    list_replicat2=list_replicat2[list_replicat2>1]
    
    list_replicat2_n=table(Summary2e_IRfiles$Soil_sample[Summary2e_IRfiles$Preparation_Type!="Spiked"])[order(table(Summary2e_IRfiles$Soil_sample[Summary2e_IRfiles$Preparation_Type!="Spiked"]))]
    list_replicat2_n=list_replicat2_n[list_replicat2_n>1]
    
    
    
    # Get all the soils that appear more than one in the IR files list: 
    
    list_replicat=unique(Summary2e_IRfiles$Soil_sample[duplicated(Summary2e_IRfiles$Soil_sample)])
    length(list_replicat)
    
    list_replicat2=table(Summary2e_IRfiles$Soil_sample)[order(table(Summary2e_IRfiles$Soil_sample))]
    
    list_replicat2=list_replicat2[list_replicat2>1]
    
    list_replicat2_n=table(Summary2e_IRfiles$Soil_sample[Summary2e_IRfiles$Preparation_Type!="Spiked"])[order(table(Summary2e_IRfiles$Soil_sample[Summary2e_IRfiles$Preparation_Type!="Spiked"]))]
    list_replicat2_n=list_replicat2_n[list_replicat2_n>1]
    
    #list of unique files 
    unique(df_Replicat$File_Names)
    length(unique(df_Replicat$File_Names))
    
    unique(df_Replicat2$File_Names)
    length(unique(df_Replicat2$File_Names))
    
    unique(df_Replicat2$File_Names[df_Replicat2$File_Names %in% list_replicat & df_Replicat2$Preparation_Type!="Spiked"])
    
    
    
    
    # 5. Operator comparison #####
    
    
    
    # List of samples analysed by 3 Operators: 

    
    
    # CSS 11 Farm 10 checks ####
    
    S1c_plot= subset(Summary1c_File, CSS==11 &Farm==10 )
    
       # * PLOT Dots,  All filters,  polymer 12 ####
    
    PLOT=ggplot( S1c_plot, aes( x=File_Names, N.particles)) +
      geom_jitter(height=0,width=0.25, aes(color=Polymer.red12, shape=Lab ))+
      ggtitle("Particles summ per Polymer.red12 per batch")+
      scale_color_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                    "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                    "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                    "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                    "CA"= "#FFD39B") , 
                         # Relabel  "Other.Plastic"                 
                         labels = c( "Other.Plastic"= "Other Plastics" ) ) +
      ggtitle(paste("CSS11 Farm10 ", "Sum all Polymers"))+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0),
            axis.title.x = element_blank())
    
    print(PLOT)
    
    
    
    Filters_d.Operator=
      
      
      unique(Summary2d_IRfiles$ Extraction_Name[Summary3a_Filter$N.div>2])
    
    
    head (Summary3d_Filter)
    
    
    
    df_d.Operator=subset(df_data_cor, Filter.name %in% Filters_d.Operator )
    
    unique(df_d.Operator$Operator)
    
    # Total number of particles to be compared: 
    length(df_d.Operator$N.px[ df_d.Operator$N.px!=0])
    
    # Compare Overall, per polymer3 and per operator, the number of plastic found, the area found, the size distribution:
    
    # Sumerize per Operator per Polymer, overall analysed filtered (10)
    Summary_d.Operator.Overall.Polymer3= df_d.Operator %>%
      group_by(Operator, Polymer.red3) %>% 
      summarise(n.pos.filter=length(unique(Filter.name)),
                Tot.N.particles= sum(N.px!=0) ,
                Tot.Area=sum(Area.um2)
      )
    
    # Sumerize per Operator per Polymer3, per analysed filtered (10)
    Summary_d.Operator.Filters.Polymer3= df_d.Operator %>%
      group_by(Filter.name, Polymer.red12, Operator ) %>% 
      summarise(n.pos.filter=length(unique(Filter.name)),
                Tot.N.particles= sum(N.px!=0) ,
                Tot.Area=sum(Area.um2) )
    
    # Sumerize per Polymer12, per analysed filtered (10)
    Summary_d.Op.Filters.Polymer12= df_d.Operator %>%
      group_by(Filter.name, Polymer.red12, Operator ) %>% 
      summarise(n.pos.filter=length(unique(Filter.name)),
                Tot.N.particles= sum(N.px!=0) ,
                Tot.Area.um2=sum(Area.um2) ) %>% 
      
      group_by(Filter.name, Polymer.red12) %>% 
      summarise(n.pos.op=length(unique(Operator)),
                Mean.N.particles= mean(Tot.N.particles) , # Mean over operators
                Mean.Area.mm2=mean(Tot.Area.um2)/1000000,
                Min.N.particles= min(Tot.N.particles),
                Max.N.particles= max(Tot.N.particles),
                Min.Area.mm2=min(Tot.Area.um2)/1000000,
                Max.Area.mm2=max(Tot.Area.um2)/1000000,
                SD.N.particles= sd(Tot.N.particles) , 
                SD.Area.mm2=sd(Tot.Area.um2)/1000000  )
    
    # write.csv(Summary_d.Operator.Polymer3,  paste(wd.out,"Summary_d.Operator.Polymer3_2024.02.21.csv",sep = "/"))
    # write.csv(Summary_d.Op.Filters.Polymer12,  paste(wd.out,"SSummary_d.Op.Filters.Polymer12_2024.02.21.csv",sep = "/"))
    
    # Total number of particles compared:  
    sum( Summary_d.Operator.Filters.Polymer3$Tot.N.particles)
    
    
    # Create new data frame
    
    
    ggplot(aes(x=Summary_d.Operator.Filters.Polymer3$Tot.N.particles[Summary_d.Operator.Filters.Polymer3$Operator=="SR"]))
    geom_point()
    
    ggplot(Summary_d.Operator.Filters.Polymer3)+
      geom_point(aes(x=Tot.N.particles, y = Operator))
    
    ggplot(Summary_d.Operator.Filters.Polymer3[Summary_d.Operator.Filters.Polymer3$Polymer.red12=="PE",])+
      geom_point(aes(x= Filter.name, y =Tot.Area , color= Operator, shape=Operator,  size=5))+
      theme_ipsum()
    theme_minimal()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ##################################  
# * (1h) For each PMF file, for each polymer 3 , for each size category ####
   # Recovery_WUR_1h=cbind( S1h_r, S1h_s, Spike_in_WUR_1h)
   
    # 1. Initialize Recovery_WUR_1h with S1h_s (Spike out files)
    # = one line / PMF File / Polumer 3/ size category 
    Recovery_WUR_1h=S1h_s[S1h_s$Lab=="WUR",]

    
    # 2. Initialize N.particles_Env and Tot.Area.mm2_Env
    # = results from Replicats 
    # = one line / Soil / Polymer 3/ size category 
    
    
    # 3. Initialize N.particles_IN and Tot.Area.mm2_IN
    # The input file fro m
    Recovery_WUR_1h$N.particles_Env=-1
    
      unique(Recovery_WUR_1h$Soil_sample)
      Polymer_Spiked_WUR=c("PE", "PLA", "PVC")
      
      
      Recovery_WUR_1h$N.particles_In=-1
      
      Recovery_WUR_1h$Tot.Area.mm2_In=-1
      Recovery_WUR_1h$Tot.Area.mm2_Env=-1
      
      
    for (soil in unique(Recovery_WUR_1h$Soil_sample)){
      for (p in Polymer_Spiked_WUR) {
        for (cat in Cat.um.txt) {
      Recovery_WUR_1h$N.particles_In[Recovery_WUR_1h$Soil_sample ==soil &
                                       Recovery_WUR_1h$Polymer.red3== p &
                                       Recovery_WUR_1h$Size_cat.um==cat] =
        
        Recovery_WUR_1h$Tot.Area.mm2_In[Recovery_WUR_1h$Soil_sample ==soil &
                                         Recovery_WUR_1h$Polymer.red3== p &
                                         Recovery_WUR_1h$Size_cat.um==cat] =
        
        Recovery_WUR_1h$N.particles_Env[Recovery_WUR_1h$Soil_sample ==soil &
                                         Recovery_WUR_1h$Polymer.red3== p &
                                         Recovery_WUR_1h$Size_cat.um==cat] =
        
        Recovery_WUR_1h$Tot.Area.mm2_Env[Recovery_WUR_1h$Soil_sample ==soil &
                                         Recovery_WUR_1h$Polymer.red3== p &
                                         Recovery_WUR_1h$Size_cat.um==cat] =
      
    }
        
      }
      
      
      Recovery_WUR_1h      %>% 
        mutate (
          N.Recovery = (N.particles - N.particles_Env ) / N.particles_In
          Area.Recovery = (Area.particles -Area.particles_Env ) / Area.particles_In
          
    # * (1i) For each PMF file, for each polymer 3 ####
    Recovery_in_WUR_1i= (S1i_r, S1i_s,, Spike_in_WUR_1i) 
    
    
    
    
  
  # * Spike in / spike out size distribution comparison 
  
  # * 
  
  
  
  
  
  