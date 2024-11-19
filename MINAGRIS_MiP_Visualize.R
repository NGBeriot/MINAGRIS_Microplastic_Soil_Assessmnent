
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(stats)

rm(list=ls()) # cleaning console
graphics.off() # cleaning plots
'%!in%' <- function(x,y)!('%in%'(x,y))

wd.out="W:/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Purency Microplastic Finder/PMF_Results_Summary"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"


# 1. Load MiP table ####

  wd.in="W:/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Purency Microplastic Finder/PMF_Results_Summary"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"
  setwd(wd.in)
  
  Data=read.csv("Corrected_MiP_Particles_20241113.csv")
  
  
  Summary5a_Field=read.csv("Summary5a_Field_20241118.csv") # Mean per soil sample, All factors
  Summary5b_Field=read.csv("Summary5b_Field_20241118.csv") # Mean per soil sample, Polymer.red12 * Size_cat.um
  Summary5c_Field=read.csv("Summary5c_Field_20241118.csv") # Mean per soil sample, Polymer.red12
  Summary5d_Field=read.csv("Summary5c_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 


# 2. Size distribution histogram ####
Cat.um.txt=c("50-300", "300-550", "550-800",
             "800-1050", "1050-1300", "1300-1550", "1550-1800")

MINAGRIS_Custom_Histogram(Data)


# 3. Bar Plot per CSS, uP per kg soil per field  ####

# Average number of particles per kg soil per field

# Duplicate the data to create calculate an overall mean. 

A=Summary5c_Field
B=Summary5c_Field
A$CSS=as.character(A$CSS)
A$Farm=as.character(A$Farm)
A$Field=as.character(A$Field)
B$CSS="MEAN"
B$Farm="ME"
B$Field="AN"

C=bind_rows(A,B) 


Summary_Field_Polymer12_bar= subset(C, Preparation_Type=="Field_samples") %>% 
  group_by(  Preparation_Type, CSS, Farm, Field, Polymer.red12 ) %>%  # Group per polymer cluster
  summarise( N.files = n(),
             Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
             Min.particles= min( Mean.particles),
             Max.particles= max( Mean.particles),
             Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
             Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
             Min.Tot.Area.mm2=min(Mean.Tot.Area.mm2),
             Max.Tot.Area.mm2=max(Mean.Tot.Area.mm2),
             Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng)  #  Mean mass per sample and polymer, over the files/operators
  ) 

#Summary_Field_Polymer12_bar=C
Summary_Field_Polymer12_bar$Farm.Field=paste(Summary_Field_Polymer12_bar$Farm, Summary_Field_Polymer12_bar$Field, sep=".")
Summary_Field_Polymer12_bar$Farm.Field[Summary_Field_Polymer12_bar$Farm.Field=="ME.AN"]="MEAN"
unique(Summary_Field_Polymer12_bar$Farm.Field)  

Summary_Field_Polymer12_bar$Farm.Field= factor(Summary_Field_Polymer12_bar$Farm.Field, 
                                               levels= c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2", "5.1", "5.2", "6.1", "6.2",
                                                         "7.1", "7.2", "8.1","8.2", "9.1", "9.2", "10.1", "10.2", "11.1", "11.2", "12.1", "12.2","12.3","12.4", "MEAN"))




A=Summary4e_Soil
B=Summary4e_Soil
A$CSS=as.character(A$CSS)
A$Farm=as.character(A$Farm)
A$Field=as.character(A$Field)
B$CSS="MEAN"
B$Farm="ME"
B$Field="AN"

C=bind_rows(A,B) 
C=A


Summary_Soil_PolymerALL_dot= subset(C, Preparation_Type=="Field_samples") %>% 
  group_by(  Preparation_Type, CSS, Farm, Field, Soil_sample, Lab) %>%  # Group per polymer cluster
  summarise( N.files = n(),
             Mean.particles= mean( Mean.particles), # Mean particle number per sample and polymer, over the files/operators 
             Min.particles= min( Mean.particles),
             Max.particles= max( Mean.particles),
             Mean.px=mean( Mean.px),              # Mean Number of pixels per sample and polymer, over the files/operators
             Mean.Tot.Area.mm2=mean(Mean.Tot.Area.mm2), #  Mean area per sample and polymer, over the files/operators
             Min.Tot.Area.mm2=min(Mean.Tot.Area.mm2),
             Max.Tot.Area.mm2=max(Mean.Tot.Area.mm2),
             Mean.Tot.Mass.ng=mean( Mean.Tot.Mass.ng)  #  Mean mass per sample and polymer, over the files/operators
  ) 

#Summary_Soil_PolymerALL_dot=C
Summary_Soil_PolymerALL_dot$Farm.Field=paste(Summary_Soil_PolymerALL_dot$Farm, Summary_Soil_PolymerALL_dot$Field, sep=".")
Summary_Soil_PolymerALL_dot$Farm.Field[Summary_Soil_PolymerALL_dot$Farm.Field=="ME.AN"]="MEAN"
unique(Summary_Soil_PolymerALL_dot$Farm.Field)  

Summary_Soil_PolymerALL_dot$Farm.Field= factor(Summary_Soil_PolymerALL_dot$Farm.Field, 
                                      levels= c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2", "5.1", "5.2", "6.1", "6.2",
                                                 "7.1", "7.2", "8.1","8.2", "9.1", "9.2", "10.1", "10.2", "11.1", "11.2", "12.1", "12.2","12.3","12.4", "MEAN"))

for (css in 1:11){  
df_plot_bar=subset(Summary_Field_Polymer12_bar, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
df_plot_dot=subset(Summary_Soil_PolymerALL_dot, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")

PLOT= ggplot(df_plot_bar) +
  # Stack bars with Polymer 12 
  geom_bar( aes(x=Farm.Field, y=Mean.particles*200, fill=Polymer.red12), position="stack", stat="identity")+ 
  # Custum color palette
  scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                               "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                               "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B"  ),
  # Relabel  "Other.Plastic"                 
                    labels = c( "Other.Plastic"="Other Plastic" ) ) +
  # Add box around the bars: transparent for all fields, black for the MINAGRIS MEAN
  geom_bar( aes(x=Farm.Field, y=Mean.particles*200, group=Farm.Field, color = Farm.Field), position="stack", stat="summary", fun=sum, fill = "transparent",
            size = 1.5) + 
  scale_color_manual(values = c("1.1"="NA", "1.2"="NA", "2.1"="NA", "2.2"="NA", "3.1"="NA", "3.2"="NA", "4.1"="NA", "4.2"="NA", "5.1"="NA", "5.2"="NA", "6.1"="NA", "6.2"="NA",
                                "7.1"="NA", "7.2"="NA", "8.1"="NA","8.2"="NA", "9.1"="NA", "9.2"="NA", "10.1"="NA", "10.2"="NA", "11.1"="NA", "11.2"="NA", "12.1"="NA", "12.2"="NA","12.3"="NA","12.4"="NA", "MEAN"="Black" ))+ 
  # Add a min max line, from summary soil, polymer 12,   
  # geom_point(data = df_plot_dot, aes(x=Farm.Field, y=Mean.particles*200)) +
  geom_errorbar(aes(x=Farm.Field ,ymin = Min.particles*200, ymax = Max.particles*200))+
  # Titles
  ggtitle(paste("Field Samples ; CSS ", css))+
  theme_minimal()+
  guides( color  = "none")+
  labs(y = "Average number of plastic particles per kg of soil",
       fill = "Plastics identified") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title.x = element_blank())


print(PLOT)
# Export plots ####
# ggsave(filename = paste(wd.out,"/Bar_MiP_number_CSS", css, ".png",sep =""), plot = PLOT, width = 8, height = 4, units = "in", dpi = 300)

} 


# x. Points S1-S2 per Field, min-max with lab color and range bar  ####
Summary5e_Field$Farm.Field=paste(Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
Summary4e_Soil$Farm.Field=paste( Summary4e_Soil$Farm, Summary4e_Soil$Field, sep=".")

for (css in 1:11){
# P1: Plot vertical bar min-max, from Summary5e_Field
  df_p1=subset(Summary5e_Field, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  df_p1$Farm.Field= factor( df_p1$Farm.Field, 
                            levels= c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2", "5.1", "5.2", "6.1", "6.2",
                                      "7.1", "7.2", "8.1","8.2", "9.1", "9.2", "10.1", "10.2", "11.1", "11.2", "12.1", "12.2","12.3","12.4", "MEAN"))
  
  p1=ggplot(df_p1, aes(x=Farm.Field, y=Mean.particles.F))+
    geom_linerange( aes(ymin = Min.particles.F, ymax = Max.particles.F))+
    ggtitle(paste("Field Samples ; CSS ", css))

  
 #  P2: Plot Samples S1-S2, min-max with lab color, from Summary4e_Soil
   df_p2=subset(Summary4e_Soil, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
   df_p2$Farm.Field= factor( df_p2$Farm.Field, 
                       levels= c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2", "5.1", "5.2", "6.1", "6.2",
                                 "7.1", "7.2", "8.1","8.2", "9.1", "9.2", "10.1", "10.2", "11.1", "11.2", "12.1", "12.2","12.3","12.4", "MEAN"))
      
   p2= ggplot()+
      geom_point(data = df_p2, aes(x=Farm.Field, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
      scale_size_manual(values = c("WUR"=2,  "Ubern"=3))+
      scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
      ggtitle(paste("Field Samples ; CSS ", css))+
      theme_minimal()

   PLOT=  
p1+ 
  geom_point(data = df_p2, aes(x=Farm.Field, y=Mean.particles, color=Lab, shape=Lab, size=Lab ))+
  scale_size_manual(values = c("WUR"=2,  "Ubern"=3))+
  scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
  ggtitle(paste("Field Samples ; CSS ", css))+
  theme_minimal()
   
    ggsave(filename = paste(wd.out,"/Dot_MiP_number_CSS", css, ".png",sep =""), plot = PLOT, width = 8, height = 4, units = "in", dpi = 300)
} #end for css  

# 4. Bar Plot CSS Area ####

##########33

for (css in 1:11){  
  df_plot=subset(Summary_Field_Polymer12_bar, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  
  
  PLOT= ggplot(df_plot ) +
    geom_bar( aes(x=Farm.Field, y=Mean.Tot.Area.mm2, fill=Polymer.red12), position="stack", stat="identity")+ 
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                                 "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B"  ) ) +
    geom_bar( aes(x=Farm.Field, y=Mean.Tot.Area.mm2, group=Farm.Field, color = Farm.Field), position="stack", stat="summary", fun=sum, fill = "transparent",
              size = 1.5) + 
    scale_color_manual(values = c("1.1"="NA", "1.2"="NA", "2.1"="NA", "2.2"="NA", "3.1"="NA", "3.2"="NA", "4.1"="NA", "4.2"="NA", "5.1"="NA", "5.2"="NA", "6.1"="NA", "6.2"="NA",
                                  "7.1"="NA", "7.2"="NA", "8.1"="NA","8.2"="NA", "9.1"="NA", "9.2"="NA", "10.1"="NA", "10.2"="NA", "11.1"="NA", "11.2"="NA", "12.1"="NA", "12.2"="NA","12.3"="NA","12.4"="NA", "MEAN"="Black" ))+ 
    
    
    ggtitle(paste("Field Samples ; CSS ", css))+
    theme_minimal()+
    guides( color  = "none")+
    labs(y = "Mean Area of plastic particles detected per 5g of soil [mm2]") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
          axis.title.x = element_blank())
  
  
  ggsave(filename = paste(wd.out,"/Bar_Area_CSS", css, ".png",sep =""), plot = PLOT, width = 8, height = 6, units = "in", dpi = 300)
}


ggplot(df_plot ) +
  
  
  
# lab 
  Summary_Field_Polymer12_bar2 = subset(Data, Preparation_Type=="Field_samples") %>% 
  group_by( File_Names, Polymer.red12, Sample_type, Preparation_Type, CSS, Farm, Field, Lab   ) %>% # For each PMF_File_name, get the summary
  summarise( N.particles= sum(N.px!=0),           # Number of particles
             Num.px=sum(N.px),           # Number of pixels
             Tot.Area.um2=sum(Area.um2.cor), # Total plastic area 
             Tot.Mass.ng=sum( Mass.ng) ) %>% 
  group_by( Sample_type, Preparation_Type, Lab , CSS, Farm, Field, Polymer.red12 ) %>%  # Group per polymer cluster
  summarise( N.files = n(),
             Mean.particles= mean(N.particles), # Mean particle number per sample and polymer, over the files/operators 
             Mean.px=mean(Num.px),              # Mean Number of pixels per sample and polymer, over the files/operators
             Mean.Tot.Area.um2=mean(Tot.Area.um2), #  Mean area per sample and polymer, over the files/operators
             Mean.Tot.Mass.ng=mean(Tot.Mass.ng) #  Mean mass per sample and polymer, over the files/operators
  ) 
  
  
  
  PLOT= ggplot(Summary_Field_Polymer12_bar2  ) +
  geom_bar( aes(x=Lab, y=Mean.Tot.Area.um2, fill=Polymer.red12), position="stack", stat="identity")+ 
  scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                               "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                               "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B"  ) ) +
  scale_color_manual(values = c("1.1"="NA", "1.2"="NA", "2.1"="NA", "2.2"="NA", "3.1"="NA", "3.2"="NA", "4.1"="NA", "4.2"="NA", "5.1"="NA", "5.2"="NA", "6.1"="NA", "6.2"="NA",
                                "7.1"="NA", "7.2"="NA", "8.1"="NA","8.2"="NA", "9.1"="NA", "9.2"="NA", "10.1"="NA", "10.2"="NA", "11.1"="NA", "11.2"="NA", "12.1"="NA", "12.2"="NA","12.3"="NA","12.4"="NA", "MEAN"="Black" ))+ 
  
  theme_minimal()+
  guides( color  = "none")+
  labs(y = "Mean Area of plastic particles detected per 5g of soil [um2]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title.x = element_blank())

print(PLOT)

ggplot(Summary_Field_Polymer12_bar2  ) +
  geom_bar( aes(x=Lab, y=Mean.particles, fill=Polymer.red12), position="stack", stat="identity")+ 
  scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                               "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
                               "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B"  ) ) +
  scale_color_manual(values = c("1.1"="NA", "1.2"="NA", "2.1"="NA", "2.2"="NA", "3.1"="NA", "3.2"="NA", "4.1"="NA", "4.2"="NA", "5.1"="NA", "5.2"="NA", "6.1"="NA", "6.2"="NA",
                                "7.1"="NA", "7.2"="NA", "8.1"="NA","8.2"="NA", "9.1"="NA", "9.2"="NA", "10.1"="NA", "10.2"="NA", "11.1"="NA", "11.2"="NA", "12.1"="NA", "12.2"="NA","12.3"="NA","12.4"="NA", "MEAN"="Black" ))+ 
  
  theme_minimal()+
  guides( color  = "none")+
  labs(y = "Mean number of plastic particles detected per 5g of soil [um2]") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title.x = element_blank())
