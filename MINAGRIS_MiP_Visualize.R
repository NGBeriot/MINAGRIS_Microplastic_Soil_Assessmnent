
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(stats)

rm(list=ls()) # cleaning console
graphics.off() # cleaning plots
'%!in%' <- function(x,y)!('%in%'(x,y))

setwd("C:/Users/berio001/Minagris/MINAGRIS_Microplastic_Soil_Assessmnent")
wd.out="Outputs"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"

# Nicolas Color Palette 
# values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
#            "PP"="#999999",  "PLA"="#FF7F00",           "PS"="#FFFF33",
#            "PET"="#A65628", "PVC"="#4DAF4A",           "PA"="#984EA3",
#            "PMMA"="#a1d99b",   "PC"="#FFF8DC",
#            "CA"= "#FFD39B"  

# MaP Color Palette 
# values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
#            "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
#            "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
#            "PMMA"="#a1d99b",   "PC"="#FFF8DC",
#            "CA"= "#FFD39B")



# 1. Load MiP table ####

 
  Data=read.csv("Outputs/Corrected_MiP_Particles_20241113.csv")

# Summary5a_Field=read.csv("Outputs/Summary5a_Field_20241118.csv") # Mean per soil sample, All factors
# Summary5b_Field=read.csv("Outputs/Summary5b_Field_20241118.csv") # Mean per soil sample, Polymer.red12 * Size_cat.um
Summary4c_Soil=read.csv("Outputs/Summary4c_Soil_20241118.csv") # Mean per soil sample, Polymer.red12
# Summary5d_Field=read.csv("Outputs/Summary5d_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 
# Summary5e_Field=read.csv("Outputs/Summary5e_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 
  
  Summary5a_Field=read.csv("Outputs/Summary5a_Field_20241118.csv") # Mean per soil sample, All factors
  Summary5b_Field=read.csv("Outputs/Summary5b_Field_20241118.csv") # Mean per soil sample, Polymer.red12 * Size_cat.um
  Summary5c_Field=read.csv("Outputs/Summary5c_Field_20241118.csv") # Mean per soil sample, Polymer.red12
  Summary5d_Field=read.csv("Outputs/Summary5d_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 
  Summary5e_Field=read.csv("Outputs/Summary5e_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 

  Summary6c_CSS=read.csv("Outputs/Summary7c_MINAGRIS_20241118.csv")
    
    
  Summary7c_MINAGRIS=read.csv("Outputs/Summary7c_MINAGRIS_20241118.csv")

  # 2. Size distribution histogram ####
  
  
  A=subset(Data_comb_red_blank, Area.um2.cor !=0 & Area.um2.cor <7885.44 & Length.um<88.042)
  B=subset(Data_comb_red_blank, Area.um2.cor > 1180^2)
  
  Cat.um.txt=c("80-300", "300-520", "520-740",
               "740-960", "960-1180", "1180-1400")

# source("MINAGRIS_Custom_Histogram.R") Work in progress
# MINAGRIS_Custom_Histogram(Data) 

  
  # * All CSS All Polymers #### 
    # One value per Size_cat.um
    Summary7f_MINAGRIS$Size_cat.um= factor(Summary7f_MINAGRIS$Size_cat.um,
                                           levels =  Cat.um.txt )
    
    
    PLOT= ggplot( subset(Summary7f_MINAGRIS, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles.MM*200)) +
      geom_bar(position="stack", stat="identity", fill="steelblue4")+ 
      ggtitle("MINAGRIS, Micoplastic size distribution")+
      theme_minimal()+
      labs(y = "Average number of plastic particles per kg of soil",
           x= "Size categories [µm]") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT
    
    ggsave(filename = paste(wd.out,"/Hist_MiP_SizeDistribution_AllCSS_AllPolymers", ".png",sep =""), plot = PLOT, width = 4, height = 4, units = "in", dpi = 300)
    

  
  # * All CSS Per Polymers   #####
    # One value per {CSS Polymer per Size_cat.um
    
    Summary7a_MINAGRIS$Size_cat.um= factor(Summary7a_MINAGRIS$Size_cat.um,
                               levels =  Cat.um.txt )
  
    
    PLOT= ggplot( subset(Summary7a_MINAGRIS, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12)) +
      geom_bar(position="stack", stat="identity")+ 
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"="Other Plastic" ) ) +
      ggtitle("MINAGRIS, Micoplastic size distribution")+
      theme_minimal()+
      labs(y = "Average number of plastic particles per kg of soil",
           x= "Size categories [µm]",
           fill = "Polymers identified") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT
    
    ggsave(filename = paste(wd.out,"/Hist_MiP_SizeDistribution_ALLCSS_Polymer12", ".png",sep =""), plot = PLOT, width = 4, height = 4, units = "in", dpi = 300)
    
  
  
  # * Per CSS All Polymers #### 
  # One value per CSS* Size_cat.um
  
  # Order per Size_cat.um
  Summary6f_CSS$Size_cat.um= factor(Summary6f_CSS$Size_cat.um,
                                    levels =  Cat.um.txt )
  
  for (css in 1:11){  
    
    PLOT= ggplot( subset(Summary6f_CSS, Size_cat.um %in%  Cat.um.txt & CSS == css), aes(x=Size_cat.um, y=Mean.particles.CSS*200)) +
      geom_bar(position="stack", stat="identity", fill="steelblue4")+ 
      ggtitle(paste("CSS ", css, ", Microplastic size distribution", sep = ""))+
      theme_minimal()+
      labs(y = "Average number of plastic particles per kg of soil",
           x= "Size categories [µm]") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT
    
    ggsave(filename = paste(wd.out,"/Hist_MiP_SizeDistribution_CSS", css, "_AllPolymers.png",sep =""), plot = PLOT, width = 4, height = 4, units = "in", dpi = 300)
    
  }
  
 
  
  # * Per CSS Per Polymers   #####
  # One value per CSS*Polymer*Size_cat.um
  
  
  # Order per Size_cat.um

  
  Summary6a_CSS$Size_cat.um= factor(Summary6a_CSS$Size_cat.um,
                                    levels =  Cat.um.txt )
  
  
  for (css in 1:11){  
      
    PLOT= ggplot( subset(Summary6a_CSS, Size_cat.um %in%  Cat.um.txt & CSS == css), aes(x=Size_cat.um, y=Mean.particles.CSS*200, fill=Polymer.red12)) +
      geom_bar(position="stack", stat="identity")+ 
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"="Other Plastic" ) ) +
      ggtitle(paste("CSS ", css, ", Microplastic size distribution", sep = ""))+
      theme_minimal()+
      labs(y = "Average number of plastic particles per kg of soil",
           x= "Size categories [µm]",
           fill = "Polymers identified") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT
    
    ggsave(filename = paste(wd.out,"/Hist_MiP_SizeDistribution_AllCSS", css, "_Polymer12.png",sep =""), plot = PLOT, width = 4, height = 4, units = "in", dpi = 300)
  }
  
   
  
  
# 3. Content Particles All CSS ####
  
  # Data frame for the bars 
  df_plot_bar=Summary6c_CSS
   df_plot_bar_css=subset( df_plot_bar,  Preparation_Type=="Field_samples")
   
  # Data frame for the min-max 
  df_plot_dot_css=subset(Summary6e_CSS,  Preparation_Type=="Field_samples")
   
  # Truncation over 50 MiP/ sample <-> 10 000 MiP/ kg soil
    y_max=50
    
    # text if the max is met
    df_plot_dot_css$Trunc=NA
    df_plot_dot_css$Trunc[ df_plot_dot_css$Max.particles.CSS> y_max]= paste( "max=",
                                                                             df_plot_dot_css$Max.particles.CSS[ df_plot_dot_css$Max.particles.CSS> y_max] *200,
                                                                             sep = "" )
   
    
    # Cut at y_max
    df_plot_dot_css$Max.particles.CSS[ df_plot_dot_css$Max.particles.CSS> y_max]= y_max
  
  # Add "CSS"abreviation in x-axis   
    df_plot_bar_css$CSS=factor(paste("CSS", df_plot_bar_css$CSS),
                               levels =  c("CSS 1", "CSS 2", "CSS 3", "CSS 4", "CSS 5", "CSS 6",
                                           "CSS 7", "CSS 8", "CSS 9", "CSS 10", "CSS 11") )
    
    
  # Initialise Plot
    PLOT= ggplot( df_plot_bar_css) +
      # Stack bars with Polymer 12 
      geom_bar( aes(x=CSS, y=Mean.particles.CSS*200, fill=Polymer.red12), position="stack", stat="identity") + 
      # Custum color palette
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"="Other Plastic" ) ) +
      # Add box around the bars: transparent for all fields, black for the MINAGRIS MEAN
      geom_bar( aes(x=CSS, y=Mean.particles.CSS*200, group=CSS, color = CSS), position="stack", stat="summary", fun=sum, fill = "transparent",
                size = 1.5) +
      scale_color_manual(values = c("CSS 1"="NA", "CSS 2"="NA", "CSS 3"="NA", "CSS 4"="NA", "CSS 5"="NA", "CSS 6"="NA", 
                                    "CSS 7"="NA", "CSS 8"="NA", "CSS 9"="NA", "CSS 10"="NA", "CSS 11"="NA", "CSS 12"="NA", "MEAN"="Black" )) +
      # Add a min max line,   
      geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
      # Add text when y max is reached
      geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max*205, label=Trunc )) +
      
      
      # Titles
      ggtitle(paste("All MINAGRIS, Microplastic particles per field"))+
      theme_minimal()+
      guides( color  = "none")+
      labs(y = "Average number of plastic particles per kg of soil",
           fill = "Polymers identified") +
      theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
            axis.title.x = element_blank())
    
  
    PLOT
    ggsave(filename = paste(wd.out,"/Bar_MiP_number_AllCSS", ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
    
  
# 4. Polymers all project ####  
  
  # Order plot per polymer   
  Summary7c_MINAGRIS$Polymer.red12 = factor(Summary7c_MINAGRIS$Polymer.red12,
                                            levels = Summary7c_MINAGRIS$Polymer.red12[order(-Summary7c_MINAGRIS$Mean.particles.MM)] )
  
  # Add percentages in the plot
  Summary7c_MINAGRIS$MiP_perc <- Summary7c_MINAGRIS$Mean.particles.MM / sum(Summary7c_MINAGRIS$Mean.particles.MM) * 100
  # Creat percentage as text 
  Summary7c_MINAGRIS$MiP_perc_text=paste0(round( Summary7c_MINAGRIS$MiP_perc, 0), "%")
  
  # Do not show percentages < 3.5% 
  Summary7c_MINAGRIS$MiP_perc_text [Summary7c_MINAGRIS$MiP_perc < 3.5]=NA
  
  # * Pie chart ####
  
  PLOT= ggplot(Summary7c_MINAGRIS, aes(x="", y=Mean.particles.MM, fill= Polymer.red12 ))+
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastic" ) ) +
    geom_text(aes(x=1.3, label = MiP_perc_text ), 
              position = position_stack(vjust = 0.5), size=5  )+
    coord_polar("y", start=0) +
    theme_void() + 
  ggtitle("All MINAGRIS, Microplastic polymer composition" )+
   # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
         fill = "Polymers identified") #+
    #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
       #   axis.title.x = element_blank() )
  
  PLOT
  
  ggsave(filename = paste(wd.out, "/Pie_MiP_number_Polymer12_ALL.png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  

  # details of "other polymers" #####
  A=subset(Summary7a_MINAGRIS, Polymer.red12=="Other.Plastic")
  ggplot(data=A, aes(x=Polymer.grp, y=Mean.particles.MM, fill=Size_cat.um))+
    geom_bar(stat="identity", position = "stack", width=1, color="white") 
  
  
  # 5. Polymers per CSS ####  
  
   # * Pie chart ####
  
  for (css in 1:11){ 
    df_plot_pie_css=subset(Summary6c_CSS, CSS %in% c(css,"MEAN") )
    
    # Order plot per polymer   
    df_plot_pie_css$Polymer.red12 = factor(df_plot_pie_css$Polymer.red12,
                                              levels = df_plot_pie_css$Polymer.red12[order(-df_plot_pie_css$Mean.particles.CSS)] )
    
    
    # Add percentages in the plot
    df_plot_pie_css$MiP_perc <- df_plot_pie_css$Mean.particles.CSS / sum( df_plot_pie_css$Mean.particles.CSS) * 100
    # Creat percentage as text 
    df_plot_pie_css$MiP_perc_text=paste0(round( df_plot_pie_css$MiP_perc, 0), "%")
    
    # Do not show percentages < 3.5% 
    df_plot_pie_css$MiP_perc_text [ df_plot_pie_css$MiP_perc < 3.5]=NA
    
    
    
  
  PLOT= ggplot(df_plot_pie_css, aes(x="", y=Mean.particles.CSS, fill= Polymer.red12 ))+
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastic" ) ) +
    geom_text(aes(x=1.3, label = MiP_perc_text ), 
              position = position_stack(vjust = 0.5), size=5  )+
    coord_polar("y", start=0) +
    theme_void() + 
    ggtitle( paste("CSS ", css, ", Microplastic polymer composition", sep=""  ))+
    # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
      fill = "Polymers identified") #+
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
  #   axis.title.x = element_blank() )
  
  PLOT
  
  ggsave(filename = paste(wd.out, "/Pie_MiP_number_Polymer12_CSS", css, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  }
  
  
  
# * Bar chart ####


ggplot(Summary7c_MINAGRIS, aes(x=Polymer.red12, y=Mean.particles.MM*200, fill= Polymer.red12 ))+
  geom_bar(stat="identity", width=1, color="white") +
  #coord_polar("y", start=0) +
  scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                               "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                               "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B") , 
                    # Relabel  "Other.Plastic"                 
                    labels = c( "Other.Plastic"="Other Plastic" ) ) +
  theme_minimal() + 
  geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
  geom_text(aes(label = paste0(round(MiP_perc, 1), "%")), vjust = 1, nudge_y = 0.2)+
  #ggtitle(paste("Field Samples ; CSS ", css))+
  # guides( color  = "none")+
  labs(# y = "Average number of plastic particles per kg of soil",
    fill = "Polymers identified") +
theme(axis.text.x = element_blank(),
      axis.title.x = element_blank())




  
  # * Bar chart
  

# 3. Bar Plot per CSS, uP per kg soil per field  ####
  
  # Average number of particles per kg soil per field
  Summary5e_Field$Farm.Field=paste("F",Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
  Summary5e_Field$Farm.Field= factor(Summary5e_Field$Farm.Field, 
                                           levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                                     "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
  Summary5c_Field$Farm.Field=paste("F",Summary5c_Field$Farm, Summary5c_Field$Field, sep=".")
  Summary5c_Field$Farm.Field= factor(Summary5c_Field$Farm.Field, 
                                     levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                               "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
  
  # *  IF PROJECT MEAN bar: ####
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
  
  
  Summary5c_Field_MMEAN= subset(C, Preparation_Type=="Field_samples") %>% 
    group_by(  Preparation_Type, CSS, Farm, Field, Farm.Field, Polymer.red12 ) %>%  # Group per polymer cluster
    summarise( N.files = n(),
               Mean.particles.MM= mean( Mean.particles.F), # Mean particle number per sample and polymer, over the files/operators 
               Min.particles.MM= min( Mean.particles.F),
               Max.particles.MM= max( Mean.particles.F),
               Mean.px.MM=mean( Mean.px.F),              # Mean Number of pixels per sample and polymer, over the files/operators
               Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.F), #  Mean area per sample and polymer, over the files/operators
               Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.F),
               Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.F),
               Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.F)  #  Mean mass per sample and polymer, over the files/operators
    ) 
  
    Summary5c_Field_MMEAN$Farm.Field[Summary5c_Field_MMEAN$Farm.Field=="ME.AN"]="MEAN"
    
  
  Summary5c_Field_MMEAN$Farm.Field= factor(Summary5c_Field_MMEAN$Farm.Field, 
                                           levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                                     "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
  
  
  #Adding the MEAN 
  
      A=Summary5e_Field
      B=Summary5e_Field
      A$CSS=as.character(A$CSS)
      A$Farm=as.character(A$Farm)
      A$Field=as.character(A$Field)
      B$CSS="MEAN"
      B$Farm="ME"
      B$Field="AN"
      
      C=bind_rows(A,B) 
      
      Summary5e_Field_MMEAN= subset(C, Preparation_Type=="Field_samples") %>% 
        group_by(  Preparation_Type, CSS, Farm, Field, Farm.Field) %>%  # Group per polymer cluster
        summarise( N.files = n(),
                   Mean.particles.MM= mean( Mean.particles.F), # Mean particle number per sample and polymer, over the files/operators 
                   Min.particles.MM= min( Mean.particles.F),
                   Max.particles.MM= max( Mean.particles.F),
                   Mean.px.MM=mean( Mean.px.F),              # Mean Number of pixels per sample and polymer, over the files/operators
                   Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.F), #  Mean area per sample and polymer, over the files/operators
                   Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.F),
                   Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.F),
                   Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.F)  #  Mean mass per sample and polymer, over the files/operators
        ) 
      
      #Summary5e_Field_MMEAN=C
      Summary5e_Field_MMEAN$Farm.Field=paste( "F",Summary5e_Field_MMEAN$Farm, Summary5e_Field_MMEAN$Field, sep=".")
      Summary5e_Field_MMEAN$Farm.Field[Summary5e_Field_MMEAN$Farm.Field=="ME.AN"]="MEAN"
      unique(Summary5e_Field_MMEAN$Farm.Field)  
      
      Summary5e_Field_MMEAN$Farm.Field= factor(Summary5e_Field_MMEAN$Farm.Field, 
                                               levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                                         "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
      
      df_plot_bar=Summary5e_Field_MMEAN
      
      # * IF NO PROJECT MEAN bar:  ####
      
      df_plot_bar=Summary5c_Field
  
      
      for (css in 1:11){  
  #df_plot_bar_css=subset(Summary5c_Field_MMEAN, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  # df_plot_dot=subset(Summary5e_Field_MMEAN, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  
  #df_p1=subset(Summary5e_Field, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  #df_p2=subset(Summary4e_Soil, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  
  df_plot_dot_css=subset(Summary5e_Field, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  df_plot_bar_css=subset( df_plot_bar, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  
  PLOT= ggplot( df_plot_bar_css) +
    # Stack bars with Polymer 12 
    geom_bar( aes(x=Farm.Field, y=Mean.particles.F*200, fill=Polymer.red12), position="stack", stat="identity")+ 
    # Custum color palette
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
    # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"="Other Plastic" ) ) +
    # Add box around the bars: transparent for all fields, black for the MINAGRIS MEAN
    geom_bar( aes(x=Farm.Field, y=Mean.particles.F*200, group=Farm.Field, color = Farm.Field), position="stack", stat="summary", fun=sum, fill = "transparent",
              size = 1.5) + 
    scale_color_manual(values = c("F.1.1"="NA", "F.1.2"="NA", "F.2.1"="NA", "F.2.2"="NA", "F.3.1"="NA", "F.3.2"="NA", "F.4.1"="NA", "F.4.2"="NA", "F.5.1"="NA", "F.5.2"="NA", "F.6.1"="NA", "F.6.2"="NA",
                                  "F.7.1"="NA", "F.7.2"="NA", "F.8.1"="NA","F.8.2"="NA", "F.9.1"="NA", "F.9.2"="NA", "F.10.1"="NA", "F.10.2"="NA", "F.11.1"="NA", "F.11.2"="NA", "F.12.1"="NA", "F.12.2"="NA","F.12.3"="NA","F.12.4"="NA", "MEAN"="Black" ))+ 
    # Add a min max line, from summary soil, polymer 12,   
    geom_linerange(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, ymin = Min.particles.F*200, ymax = Max.particles.F*200))+
    # Titles
    ggtitle(paste("CSS ", css, ", Microplastic particles per field", sep = ""))+
    theme_minimal()+
    guides( color  = "none")+
    labs(y = "Average number of plastic particles per kg of soil",
         fill = "Polymers identified") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
          axis.title.x = element_blank())
  
  
  print(PLOT)
  # Export plots ####
   ggsave(filename = paste(wd.out,"/Bar_MiP_number_CSS", css, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  
  } 
  
  
  # 4. Points S1-S2 per Field, min-max with lab color and range bar  ####
  Summary5e_Field$Farm.Field=paste(Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
  Summary4e_Soil$Farm.Field=paste( Summary4e_Soil$Farm, Summary4e_Soil$Field, sep=".")
  
  for (css in 1:11){
  # P1: Plot vertical bar min-max, from Summary5e_Field
    df_p1=subset(Summary5e_Field, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
    df_p1$Farm.Field= factor( df_p1$Farm.Field, 
                              levels= c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2", "5.1", "5.2", "6.1", "6.2",
                                        "7.1", "7.2", "8.1","8.2", "9.1", "9.2", "10.1", "10.2", "11.1", "11.2", "12.1", "12.2","12.3","12.4", "MEAN"))
    
    p1= ggplot()+
      geom_linerange(data=df_p1, aes(x=Farm.Field, y=Mean.particles.F, ymin = Min.particles.F, ymax = Max.particles.F))+
      ggtitle(paste("Field Samples ; CSS ", css))+
      theme_minimal()
    
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
       labs(y = "Average number of plastic particles per soil sample") +
    theme_minimal()+
       theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
             axis.title.x = element_blank())
     
     PLOT
     
      ggsave(filename = paste(wd.out,"/Dot_MiP_number_CSS", css, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  } #end for css  


  

  
  
  
  
  
  # 5. Bar Plot CSS Area ####



for (css in 1:11){  
  df_plot=subset(Summary5c_Field_MMEAN, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
  
  
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
  
  
  ggsave(filename = paste(wd.out,"/Bar_Area_CSS", css, ".png",sep =""), plot = PLOT, width = 10.5, height = 6, units = "in", dpi = 300)
}


ggplot(df_plot ) +
  
  
  
# lab 
  Summary5c_Field_MMEAN2 = subset(Data, Preparation_Type=="Field_samples") %>% 
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
  
  
  
  PLOT= ggplot(Summary5c_Field_MMEAN2  ) +
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

ggplot(Summary5c_Field_MMEAN2  ) +
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
