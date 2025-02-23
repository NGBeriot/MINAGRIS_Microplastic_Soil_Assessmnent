

library(tidyverse)
library(hrbrthemes)
library(stats)
library(readxl)
library(ggnewscale)
library(ggthemes)

rm(list=ls()) # cleaning console
graphics.off() # cleaning plots

'%!in%' <- function(x,y)!('%in%'(x,y))
label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")

#setwd("C:/Users/berio001/Minagris/MINAGRIS_Microplastic_Soil_Assessmnent")
wd.out="Outputs"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"

# set encoding to read csv files correctly
options(encoding = "UTF-8") #"UTF-8" "latin-1"

# Set seed for jitter
set.seed(42^2)

# 1. Load data####

  # # * MiP table ####
  #   Data_comb_red_blank=read.csv("Outputs/Corrected_MiP_Particles.csv")
  # 
  # 
  # # * Summary tables ####
  # 
  # 
  # 
  # # Summary5a_Field=read.csv("Outputs/Summary5a_Field_20241118.csv") # Mean per soil sample, All factors
  # # Summary5b_Field=read.csv("Outputs/Summary5b_Field_20241118.csv") # Mean per soil sample, Polymer.red12 * Size_cat.um
  # Summary4c_Soil=read.csv("Outputs/Summary4c_Soil_20241118.csv") # Mean per soil sample, Polymer.red12
  # # Summary5d_Field=read.csv("Outputs/Summary5d_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 
  # # Summary5e_Field=read.csv("Outputs/Summary5e_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 
  #   
  #   Summary5a_Field=read.csv("Outputs/Summary5a_Field_20241118.csv") # Mean per soil sample, All factors
  #   Summary5b_Field=read.csv("Outputs/Summary5b_Field_20241118.csv") # Mean per soil sample, Polymer.red12 * Size_cat.um
  #   Summary5c_Field=read.csv("Outputs/Summary5c_Field_20241118.csv") # Mean per soil sample, Polymer.red12
  #   Summary5d_Field=read.csv("Outputs/Summary5d_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 
  #   Summary5e_Field=read.csv("Outputs/Summary5e_Field_20241118.csv") # Mean per soil sample, Sum up all polymers, "Other.Plastic" excluded 
  # 
  #   Summary6c_CSS=read.csv("Outputs/Summary7c_MINAGRIS_20241118.csv")
  #     
  #     
  #   Summary7c_MINAGRIS=read.csv("Outputs/Summary7c_MINAGRIS_20241118.csv")
  #   
    
# 2. Outline Format ####
    
    # * Titles and translations #### 
    
   Txt_translation=read_excel("Txt_translation_table.xlsx")%>%
      mutate(across(everything(), ~ str_replace_all(., "\\\\n", "\n")))
    
    Cat.um.txt=c("90-300", "300-510", "510-720",
                 "720-930", "930-1140", "1140-1350")
    
    Date="2025.02.12"
    
    # * Color palette ####   
    
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
    
    
    # # Linda's palette: 
    # paletteer_d("ggthemr::flat_dark") 
    

    
# 3. Hist. Size distribution ####
  
  # Check Available sizes 
  A=subset(Data_comb_red_blank, Area.um2.cor !=0 & Area.um2.cor <7885.44 & Length.um<88.042)
  B=subset(Data_comb_red_blank, Area.um2.cor > 1180^2)

  # source("MINAGRIS_Custom_Histogram.R") Work in progress
  # MINAGRIS_Custom_Histogram(Data) 

  # For loop per version: # (Maybe not the most eleguant coding but at least it is more organized)
  
  # * 3.1. Per CSS, Mean Farms, Sum Polymers #### 
    # One value per Size_cat.um
    Summary7f_MINAGRIS$Size_cat.um= factor(Summary7f_MINAGRIS$Size_cat.um,
                                           levels =  Cat.um.txt )
    
  # Translation for each css: 
  for (css in 1:11){  
    
    PLOT= ggplot( subset(Summary7f_MINAGRIS, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles.MM*200)) +
      geom_bar(position="stack", stat="identity", fill="steelblue4")+ 
      ggtitle(Txt_translation$Hist_Title_All[Txt_translation$CSS==css])+
      theme_minimal()+
        labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==css]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT

    ggsave(filename = paste(wd.out,"/CSS",css,"/3.1.Hist_MiP_SizeDistribution_AllCSS_AllPolymers_txtCSS", css, ".png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
    
  }
  
  # * 3.2. Per CSS, Mean Farms, Per Polymer   #####
    # One value per {CSS Polymer per Size_cat.um
    
    Summary7a_MINAGRIS$Size_cat.um= factor(Summary7a_MINAGRIS$Size_cat.um,
                               levels =  Cat.um.txt )
  
  # Translation for each css: 
  for (css in 1:11){  
    PLOT= ggplot( subset(Summary7a_MINAGRIS, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12)) +
      geom_bar(position="stack", stat="identity")+ 
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css] ) ) +
      ggtitle(Txt_translation$Hist_Title_All[Txt_translation$CSS==css])+
      theme_minimal()+
      labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==css],
           fill =  Txt_translation$Polymers_identified[ Txt_translation$CSS==css]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT
    
    ggsave(filename = paste(wd.out,"/CSS",css,"/3.2.Hist_MiP_SizeDistribution_ALLCSS_Polymer12_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
  }
  
  
  # * 3.3. Per CSS, Per Farm, Sum Polymers #### 
  # One value per CSS* Size_cat.um
  
  # Order per Size_cat.um
  Summary6f_CSS$Size_cat.um= factor(Summary6f_CSS$Size_cat.um,
                                    levels =  Cat.um.txt )
  
  for (css in 1:11){  
    
    PLOT= ggplot( subset(Summary6f_CSS, Size_cat.um %in%  Cat.um.txt & CSS == css), aes(x=Size_cat.um, y=Mean.particles.CSS*200)) +
      geom_bar(position="stack", stat="identity", fill="steelblue4")+ 
      ggtitle(paste("CSS ", css, ", ", Txt_translation$Hist_Title_CSS[Txt_translation$CSS==css], sep = ""))+
      theme_minimal()+
      labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==css]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )

    ggsave(filename = paste(wd.out,"/CSS",css,"/3.3.Hist_MiP_SizeDistribution_CSS", css, "_AllPolymers.png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
    
  }
  
 
  
  # * 3.4. Per CSS, Per Farm, Per Polymer   #####
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
                        labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ) ) +
      ggtitle(paste("CSS ", css, ", ", Txt_translation$Hist_Title_CSS[Txt_translation$CSS==css], sep = ""))+
      theme_minimal()+
      labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==css],
           fill =  Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    ggsave(filename = paste(wd.out,"/CSS",css,"/3.4.Hist_MiP_SizeDistribution_CSS", css, "_Polymer12_", Date, ".png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
  }
  


# 4. BarPlot, Number MiP per kg soil ####
  #For loop per version: # (Maybe not the most eleguant coding but at least it is more organized)
  
  # * 4.1. Per CSS, Sum Farms, Sum Polymers ####
  # -> Average (and median), jitter dots, no min-max, max out of bound as label

  
  # Initialise Plot
  # Data frame for the bars 
  df_plot_bar=  Summary6c_CSS_outlier 
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
  
  # Translation for each css: 
  for (css in 1:11){ 
    # *** MEAN #####
    PLOT= ggplot() +
      # Stack bars with Polymer 12 
      geom_bar(data=df_plot_bar_css, aes(x=CSS, y=Mean.particles.CSS*200,fill="Average all farms"),  stat="identity") + 
      scale_fill_manual(values = "#20b8bd", labels=Txt_translation$Average_Farms[Txt_translation$CSS==css])+
      
      # # Add a min max line,
      # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
      # Add text when y max is reached
      geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max*201.9, label=Trunc )) +
      
      # Add the x-axis line
      geom_segment(aes(x = 0.4, xend = nrow(Summary6d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
      
      # Add the y-axis 
      geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
      
      # Add jitter
      #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
      geom_point(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
      scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
      
      ylim(0,10500)+
      # Titles
      ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
      
      
      # custom y scale 
      scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
                         labels = label_at(1000), #,                                        # label every second break
                         limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
      
      
      theme_minimal()+
      #guides( color  = "none")+
      labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
           color=element_blank() ) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_line(color = "black"),
            axis.line.y = element_blank(),
            legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
            panel.grid.minor.y = element_blank())+ # Remove the minor grid 
      
      # Add the axis.text.x at the rite position
      annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
      # Clip the graph to the right size
      coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
    
    
    print( PLOT)
    ggsave(filename = paste(wd.out,"/CSS",css,"/4.1.Bar_Mean_MiP_SumPolymer_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
  } # end Translation for each css, 4.1
  
  
  
  
  # * 4.2. Per CSS, Sum Farms, Per Polymer ####
  # -> Average (and median), no dot, no min-max, no max
  
  # Data frame for the bars 
  df_plot_bar_css=subset( Summary6c_CSS_outlier ,  Preparation_Type=="Field_samples")
  
  # Data frame for the min-max 
  df_plot_dot_css=subset(Summary6e_CSS,  Preparation_Type=="Field_samples")
  
  # Truncation over 30 MiP/ sample <-> 6 000 MiP/ kg soil # /!\ Ad Hoc
  y_max=30
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
  
  # Translation for each css: 
  for (css in 1:11){  
    # *** MEAN #####
    PLOT= ggplot( df_plot_bar_css) +
      # Stack bars with Polymer 12 
      geom_bar( aes(x=CSS, y=Mean.particles.CSS*200, fill=Polymer.red12), position="stack", stat="identity") + 
      # Custom color palette
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css] ) ) +
      # # Add box around the bars: transparent for all fields, black for the MINAGRIS MEAN
      # geom_bar( aes(x=CSS, y=Mean.particles.CSS*200, group=CSS, color = CSS), position="stack", stat="summary", fun=sum, fill = "transparent",
      #           size = 1.5) +
      # scale_color_manual(values = c("CSS 1"="NA", "CSS 2"="NA", "CSS 3"="NA", "CSS 4"="NA", "CSS 5"="NA", "CSS 6"="NA", 
      #                               "CSS 7"="NA", "CSS 8"="NA", "CSS 9"="NA", "CSS 10"="NA", "CSS 11"="NA", "CSS 12"="NA", "MEAN"="Black" )) +
      # # Add a min max line,   
      # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
      # # Add text when y max is reached
      # geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max*201.9, label=Trunc )) +
      # # Add jitter
      # geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
      
      # Titles
      ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
      
      # Add the x-axis line
      geom_segment(aes(x = 0.4, xend = nrow(Summary6d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
      
      # Add the y-axis 
      geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
      
      # custom y scale 
      scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap # y_max*200/8
                         labels = label_at(1000), #,                                        # label every second break
                         limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
      
      # Theme 
      theme_minimal()+
      guides( color  = "none")+
      labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_line(color = "black"),
            axis.line.y = element_blank(),
            panel.grid.minor.y = element_blank())+
      
      # Add the axis.text.x at the rite position
      annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
      # Clip the graph to the right size
      coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
    
    
    print( PLOT)
    ggsave(filename = paste(wd.out,"/CSS",css,"/4.2.Bar_Mean_MiP_Polymer12_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
    # # *** MEDIAN #####
    # PLOT= ggplot( df_plot_bar_css) +
    #   # Stack bars with Polymer 12 
    #   geom_bar( aes(x=CSS, y=Median.particles.CSS*200, fill=Polymer.red12), position="stack", stat="identity") + 
    #   # Custom color palette
    #   scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
    #                                "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
    #                                "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
    #                                "PMMA"="#a1d99b",   "PC"="#FFF8DC",
    #                                "CA"= "#FFD39B") , 
    #                     # Relabel  "Other.Plastic"                 
    #                     labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css] ) ) +
    #   # # Add box around the bars: transparent for all fields, black for the MINAGRIS MEAN
    #   # geom_bar( aes(x=CSS, y=Median.particles.CSS*200, group=CSS, color = CSS), position="stack", stat="summary", fun=sum, fill = "transparent",
    #   #           size = 1.5) +
    #   # scale_color_manual(values = c("CSS 1"="NA", "CSS 2"="NA", "CSS 3"="NA", "CSS 4"="NA", "CSS 5"="NA", "CSS 6"="NA", 
    #   #                               "CSS 7"="NA", "CSS 8"="NA", "CSS 9"="NA", "CSS 10"="NA", "CSS 11"="NA", "CSS 12"="NA", "MEAN"="Black" )) +
    #   # # Add a min max line,   
    #   # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Median.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
    #   # # Add text when y max is reached
    #   # geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max*201.9, label=Trunc )) +
    #   # # Add jitter
    #   # geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
    #   
    #   # Titles
    #   ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
    #   
    #   # Add the x-axis line
    #   geom_segment(aes(x = 0.4, xend = nrow(Summary6d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
    #   
    #   # Add the y-axis 
    #   geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
    #   
    #   # custom y scale 
    #   scale_y_continuous(breaks = seq(0, y_max*200, y_max*200/8), # Have a break for each gap
    #                      labels = label_at(y_max*200/4), #,                                        # label every second break
    #                      limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
    #   
    #   # Theme 
    #   theme_minimal()+
    #   guides( color  = "none")+
    #   labs(y = Txt_translation$y_nMiP_med[Txt_translation$CSS==css],
    #        fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
    #   theme(axis.text.x = element_blank(),
    #         axis.title.x = element_blank(),
    #         axis.ticks.y = element_line(color = "black"),
    #         axis.line.y = element_blank())+
    #   
    #   # Add the axis.text.x at the rite position
    #   annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
    #   # Clip the graph to the right size
    #   coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
    # 
    # 
    # print( PLOT)
    # ggsave(filename = paste(wd.out,"/CSS",css,"/Bar_Median_MiP_Polymer12_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
  } # end Translation for each css, 4.2
  
  
  
  
  
  
 
  # * 4.3. Per CSS, Per Farm, Sum Polymers (WinP) ####
    # /!\ Work in progress /!\ #
  
  # * 4.4. Per CSS, Per Farm, Per Polymer ####
    # Bar Plot per CSS, uP per kg soil per Field
    
    # Initialize plot df
    # Average number of particles per kg soil per Field
    Summary5e_Field$Farm.Field=paste("F",Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
    Summary5e_Field$Farm.Field= factor(Summary5e_Field$Farm.Field, 
                                       levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                                 "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
    
    Summary5c_Field$Farm.Field=paste("F",Summary5c_Field$Farm, Summary5c_Field$Field, sep=".")
    Summary5c_Field$Farm.Field= factor(Summary5c_Field$Farm.Field, 
                                       levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                                 "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
    
    # # *** IF PROJECT MEAN bar: ####
    # # Duplicate the data to create calculate an overall mean. 
    # # For the bars
    # A=Summary5c_Field
    # B=Summary5c_Field
    # A$CSS=as.character(A$CSS)
    # A$Farm=as.character(A$Farm)
    # A$Field=as.character(A$Field)
    # B$CSS="MEAN"
    # B$Farm="ME"
    # B$Field="AN"
    # 
    # C=bind_rows(A,B) 
    # 
    # 
    # Summary5c_Field_MMEAN= subset(C, Preparation_Type=="Field_samples") %>% 
    #   group_by(  Preparation_Type, CSS, Farm, Field, Farm.Field, Polymer.red12 ) %>%  # Group per polymer cluster
    #   summarise( N.files = n(),
    #              Mean.particles.MM= mean( Mean.particles.F), # Mean particle number per sample and polymer, over the files/operators 
    #              Min.particles.MM= min( Mean.particles.F),
    #              Max.particles.MM= max( Mean.particles.F),
    #              Mean.px.MM=mean( Mean.px.F),              # Mean Number of pixels per sample and polymer, over the files/operators
    #              Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.F), #  Mean area per sample and polymer, over the files/operators
    #              Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.F),
    #              Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.F),
    #              Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.F)  )#  Mean mass per sample and polymer, over the files/operators
    # 
    # Summary5c_Field_MMEAN$Farm.Field[Summary5c_Field_MMEAN$Farm.Field=="ME.AN"]="MEAN"
    # 
    # Summary5c_Field_MMEAN$Farm.Field= factor(Summary5c_Field_MMEAN$Farm.Field, 
    #                                          levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
    #                                                    "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
    # # For the dots
    # A=Summary5e_Field
    # B=Summary5e_Field
    # A$CSS=as.character(A$CSS)
    # A$Farm=as.character(A$Farm)
    # A$Field=as.character(A$Field)
    # B$CSS="MEAN"
    # B$Farm="ME"
    # B$Field="AN"
    # 
    # C=bind_rows(A,B) 
    # 
    # Summary5e_Field_MMEAN= subset(C, Preparation_Type=="Field_samples") %>% 
    #   group_by(  Preparation_Type, CSS, Farm, Field, Farm.Field) %>%  # Group per polymer cluster
    #   summarise( N.files = n(),
    #              Mean.particles.MM= mean( Mean.particles.F), # Mean particle number per sample and polymer, over the files/operators 
    #              Min.particles.MM= min( Mean.particles.F),
    #              Max.particles.MM= max( Mean.particles.F),
    #              Mean.px.MM=mean( Mean.px.F),              # Mean Number of pixels per sample and polymer, over the files/operators
    #              Mean.Tot.Area.mm2.MM=mean(Mean.Tot.Area.mm2.F), #  Mean area per sample and polymer, over the files/operators
    #              Min.Tot.Area.mm2.MM=min(Mean.Tot.Area.mm2.F),
    #              Max.Tot.Area.mm2.MM=max(Mean.Tot.Area.mm2.F),
    #              Mean.Tot.Mass.ng.MM=mean( Mean.Tot.Mass.ng.F) ) #  Mean mass per sample and polymer, over the files/operators
    # 
    # Summary5e_Field_MMEAN$Farm.Field=paste( "F",Summary5e_Field_MMEAN$Farm, Summary5e_Field_MMEAN$Field, sep=".")
    # Summary5e_Field_MMEAN$Farm.Field[Summary5e_Field_MMEAN$Farm.Field=="ME.AN"]="MEAN"
    # unique(Summary5e_Field_MMEAN$Farm.Field)  
    # 
    # Summary5e_Field_MMEAN$Farm.Field= factor(Summary5e_Field_MMEAN$Farm.Field, 
    #                                          levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
    #                                                    "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
    # df_plot_bar=Summary5e_Field_MMEAN
    
    # *** IF NO PROJECT MEAN bar:  ####

      # Remove outlier (CSS11 Farm 10) 
      df_plot_bar=subset(Summary5c_Field, CSS!=11 | Farm !=10 )
      Summary5e_Field_outlier=subset(Summary5e_Field, CSS!=11 | Farm !=10 )
      
      for (css in 1:11){  
        
        # **** MEAN ####
        df_plot_dot_css=subset(Summary5e_Field_outlier, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
        df_plot_bar_css=subset( df_plot_bar, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
        
        
        # Add vertical ticks between farms: 
        # finds the separations between the farms  
        Farms_csum <- df_plot_dot_css %>%
          group_by(Farm) %>%
          summarise(
            n_farm=n()) %>%
          mutate( sum_farm=cumsum(n_farm)) 
        #Farm_boundaries <-  Farms_csum$sum_farm[1:nrow( Farms_csum)-1]+0.5 # Removing the last tick 
        Farm_boundaries <-  Farms_csum$sum_farm+0.5 # including the last tick 
        
        
        # Calculate gap between breaks: 
        gap= round(max(df_plot_dot_css$Mean.particles.F)/5)*100
        if (gap>1000){
          gap=round(gap/500)*500
        }
        
        # plot number of files 
        ggplot()+
          geom_point(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, color=factor(N.files)), shape = 1)
        
        # Plot 
        PLOT= ggplot( ) +
          # Stack bars with Polymer 12 
          geom_bar( data=df_plot_bar_css,aes(x=Farm.Field, y=Mean.particles.F*200, fill=Polymer.red12), position="stack", stat="identity")+ 
          # Custom color palette
          scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                       "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                       "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                       "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                       "CA"= "#FFD39B") , 
                            # Relabel  "Other.Plastic"                 
                            labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ) ) +
          
          # custom y scale 
          scale_y_continuous(breaks = seq(0, max(df_plot_dot_css$Mean.particles.F)*200, gap), # Have a break for each gap  
                             labels = label_at(gap*2),                                        # label every second break  
                             limits = c(-3*gap/4, max(df_plot_dot_css$Mean.particles.F)*200))+ #limits goes beyond to allow the vertical ticks between farms
          
          # Titles
          ggtitle(paste("CSS ", css, ", ", Txt_translation$Bar_Title_CSS[Txt_translation$CSS==css]," ",Txt_translation$Field[Txt_translation$CSS==css], sep = ""))+
          
          # Add the x-axis line
          geom_segment(aes(x = 0.4, xend = max(Farm_boundaries), y = 0, yend = 0), color = "black", size = 1) + #geom_hline(yintercept = 0, color = "black", size = 1) +
          
          # Add the y-axis 
          geom_segment(aes(x = 0.4, xend = 0.4, y = max(df_plot_dot_css$Mean.particles.F)*200, yend = -gap/2), color = "black", size = 1) +
          
          # Add vertical ticks between farms: 
          geom_segment(data = data.frame(x = Farm_boundaries), aes(x = x, xend = x, y = 0, yend = -gap/2),
                       color = "black", size = .8) +
          
          # # Add the min-max bar 
          # geom_linerange(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, ymin = Min.particles.F*200, ymax = Max.particles.F*200))+
          
          # # Add a dot when Min.particles.F == Max.particles.F and n>1
          #  geom_point(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, color=factor(N.files)))+
          #  scale_color_manual(values = c("1"="NA","2"="Black" ))+
          # Guides
          guides( color  = "none", )+
          
          # # Add a star when n<=1
          # new_scale_color() +
          # geom_point(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, color=factor(N.files)), shape = 8,size=2)+
          # scale_color_manual(values = c("1"="Black","2"="NA"))+
          
          # Guides
          guides( color  = "none", )+
          # Translate axis and legend
          labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],                
               fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
          # Theme
          theme_minimal()+
          theme(
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.length.y = unit(0.15, "cm"),
            axis.ticks.y = element_line(color = "black"), 
            axis.line.y = element_blank() )+
          
          # Add the axis.text.x at the right position
          annotate("text", x = df_plot_dot_css$Farm.Field, y = -gap/10, label = df_plot_dot_css$Farm.Field,angle=90,  hjust=1) +
          # Clip the graph to the right size
          coord_cartesian(ylim = c(-gap/2, NA), clip = "off")
        
        print(PLOT)
        # Export plots 
        ggsave(filename = paste(wd.out,"/CSS",css,"/4.4.Bar_MiP_number_nobar_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 5, units = "in", dpi = 300)
        
      } # End for CSS loop, 4.4
  
  
  
  # * 4.5. Per CSS, Per Sample, Per Polymer ####
  
    # Plot, one bar per Sample  #
      # Prepare the data all sum all per Soil Sample
      # Add a line for the field with a missing soil
     Summary4e_Soilcomp=   Summary4e_Soil %>%
        ungroup() %>%
        complete(Lab, nesting(CSS, Farm, Field, Preparation_Type),
                 fill=list(Mean.particles=0,
                           Mean.px=0,
                           Mean.Tot.Area.mm2=0,
                           Mean.Tot.Mass.ng=0,
                           SD.Area=0,
                           N.files=0))
      
      
      
      
      # Add a Farm.Field.Lab column per Soil Sample 
      Summary4e_Soilcomp$Farm.Field.Lab=paste("F", Summary4e_Soilcomp$Farm, Summary4e_Soilcomp$Field, Summary4e_Soilcomp$Lab, sep=".")
      # /!\ There is repetition among CSS /!\
      
      # Add a column Farm.Field per Field
      Summary4e_Soilcomp$Farm.Field=paste("F",Summary4e_Soilcomp$Farm, Summary4e_Soilcomp$Field, sep=".")
      Summary4e_Soilcomp$Farm.Field= factor(Summary4e_Soilcomp$Farm.Field, 
                                         levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                                   "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
      
      

      # Prepare the data all sum per Polymer12 per Soil Sample
      
      # Add a line for the field with a missing soil
      Summary4c_Soilcomp=   Summary4c_Soil %>%
        ungroup() %>%
        complete(Lab, nesting(CSS, Farm, Field, Preparation_Type, Polymer.red12),
                 fill=list(Mean.particles=0,
                           Mean.px=0,
                           Mean.Tot.Area.mm2=0,
                           Mean.Tot.Mass.ng=0,
                           SD.Area=0,
                           N.files=0))
      
            # Add a Farm.Field.Lab column per Soil Sample 
        Summary4c_Soilcomp$Farm.Field.Lab=paste("F",   Summary4c_Soilcomp$Farm,   Summary4c_Soilcomp$Field,   Summary4c_Soilcomp$Lab, sep=".")
      # /!\ There is repetition among CSS /!\
      
      # Add a column Farm.Field per Field
        Summary4c_Soilcomp$Farm.Field=paste("F",  Summary4c_Soilcomp$Farm,   Summary4c_Soilcomp$Field, sep=".")
        Summary4c_Soilcomp$Farm.Field= factor(  Summary4c_Soilcomp$Farm.Field, 
                                        levels= c("F.1.1", "F.1.2", "F.2.1", "F.2.2", "F.3.1", "F.3.2", "F.4.1", "F.4.2", "F.5.1", "F.5.2", "F.6.1", "F.6.2",
                                                  "F.7.1", "F.7.2", "F.8.1","F.8.2", "F.9.1", "F.9.2", "F.10.1", "F.10.2", "F.11.1", "F.11.2", "F.12.1", "F.12.2","F.12.3","F.12.4", "MEAN"))
      
      
      # Prepare the data for the plot
      # Remove outlier (CSS11 Farm 10) 
      Plot_Summary4c_Soil=subset(  Summary4c_Soilcomp, CSS!=11 | Farm !=10 )
      Plot_Summary4e_Soil=subset(Summary4e_Soilcomp, CSS!=11 | Farm !=10 )
      Plot_Summary8e_Farm=subset(Summary8e_Farm, CSS!=11 | Farm !=10 )
      
      
      for (css in 1:11){  
        df_plot_dot_css=subset(Plot_Summary4e_Soil, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
        df_plot_bar_Sample_css=subset(Plot_Summary4c_Soil, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
        df_plot_dot_farm=subset(Plot_Summary8e_Farm, CSS %in% c(css,"MEAN") & Preparation_Type=="Field_samples")
        
        # Add vertical ticks between farms: 
        # finds the separations between the farms  
        Farms_csum <- df_plot_dot_css %>%
          group_by(Farm) %>%
          summarise(
            n_farm=n()) %>%
          mutate( sum_farm=cumsum(n_farm)) %>%
          mutate( mean_farm = (lead(sum_farm) + sum_farm)/2)
        
        Farm.Field_csum <- df_plot_dot_css %>%
          group_by(Farm.Field) %>%
          summarise(
            n_sample=n()) %>%
          mutate( sum_Farm.Field=cumsum(n_sample)) %>%
          mutate( mean_farm = (lead(sum_Farm.Field) + sum_Farm.Field)/2)
        
        Field_csum <- df_plot_dot_css %>%
          group_by(Farm.Field, Field) %>%
          summarise(
            n_sample=n()) %>%
          mutate( sum_Farm.Field=cumsum(n_sample)) %>%
          mutate( mean_farm = (lead(sum_Farm.Field) + sum_Farm.Field)/2)
        
        
          
        #Farm_boundaries <-  Farms_csum$sum_farm[1:nrow( Farms_csum)-1]+0.5 # Removing the last tick 
        Farm_boundaries <-  Farms_csum$sum_farm+0.5 # including the last tick 
        Farm_name <- c(2,Farms_csum$mean_farm[1:nrow( Farms_csum)-1])+0.5 # Removing the last tick 
        
        Farm.Field_boundaries= Farm.Field_csum$sum_Farm.Field+0.5
        Farm.Field_name= c(1, Farm.Field_csum$mean_farm[1:nrow( Farm.Field_csum)-1])+0.5
        
        # Calculate gap between breaks: 
        gap= round(max(df_plot_dot_css$Mean.particles)/5)*100
        if (gap>1000){
          gap=round(gap/500)*500
        }


        # Order plot_dot by Farm.Field.Lab
        df_plot_dot_css$Farm.Field.Lab=factor(df_plot_dot_css$Farm.Field.Lab,
                               levels = df_plot_dot_css$Farm.Field.Lab[order(df_plot_dot_css$Farm.Field)] )
        
        PLOT=ggplot( ) +
          # Stack bars with Polymer 12 
          geom_bar( data=  df_plot_bar_Sample_css,aes(x=Farm.Field.Lab, y=Mean.particles*200,   fill=Polymer.red12), position="stack", stat="identity") + 
          # Custom color palette
          scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                       "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                       "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                       "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                       "CA"= "#FFD39B") , 
                            # Relabel  "Other.Plastic"                 
                            labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ),
                            guide = guide_legend(reverse = F)) +
          # Add a dot when Min.particles.F == Max.particles.F and n>1
          geom_point(data=df_plot_dot_css, aes(x=Farm.Field.Lab, y=Mean.particles*200+gap/5, shape= factor(N.files)))+
          scale_shape_manual(values = c("1"=NA,"0"=8), labels = c( "0"=Txt_translation$Ongoing_Analysis[Txt_translation$CSS==css], "1"="" ), name=NULL)+ # star for the missing sample, NA for the rest) 
                             
          # Add vertical ticks between farms: 
          geom_segment(data = data.frame(x = Farm_boundaries), aes(x = x, xend = x, y = 0, yend = -3*gap/4),
                       color = "black", size = .8) +
          
          # Add vertical ticks between fields: 
          geom_segment(data = data.frame(x = Farm.Field_boundaries), aes(x = x, xend = x, y = 0, yend = -gap/4),
                       color = "black", size = .6) +

          # Titles
          ggtitle(paste("CSS ", css, ", ", Txt_translation$Bar_Title_CSS[Txt_translation$CSS==css]," ",Txt_translation$Sample[Txt_translation$CSS==css], sep = ""))+
          
          # Add the x-axis line
          geom_segment(aes(x = 0.4, xend = max(Farm_boundaries), y = 0, yend = 0), color = "black", size = 1) + #geom_hline(yintercept = 0, color = "black", size = 1) +
          
          # Add the y-axis 
          geom_segment(aes(x = 0.4, xend = 0.4, y = max(df_plot_dot_css$Mean.particles)*200, yend = -gap/2), color = "black", size = 1) +
          
        # # Add the axis.text.x for the Farm.fields
        # annotate("text", x = Farm.Field_name, y = -gap/4, label = unique(df_plot_dot_css$Farm.Field) ,  hjust=0.5) +
          
        # # Add the axis.text.x for the fields
        # annotate("text", x = Farm.Field_name, y = -gap/4, label = paste(".", Field_csum$Field, sep = "") ,  hjust=0.5) +
        # 
        # # Add the axis.text.x for the Farms
        # annotate("text", x = Farm_name, y = -0.75*gap, label = paste("F.",unique(df_plot_dot_farm$Farm), sep ="") ,  hjust=0.5)+
          
        # Add the axis.text.x for the Farm.fields
          annotate("text", x = Farm.Field_name, y = -gap/10, label = Field_csum$Farm.Field ,angle=90,  hjust=1) +
        
          
        # custom y scale 
        scale_y_continuous(breaks = seq(0, max(df_plot_dot_css$Mean.particles)*200, gap), # Have a break for each gap  
                             labels = label_at(gap*2),                                        # label every second break  
                             limits = c(-gap, max(df_plot_dot_css$Mean.particles)*200))+ #limits goes beyond to allow the vertical ticks between farms
          
          
        # Theme
          guides(fill=guide_legend(order=1), shape=guide_legend(order=2))+
          labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
               fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
        theme_minimal()+
          theme(
            axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.length.y = unit(0.15, "cm"),
            axis.ticks.y = element_line(color = "black"), 
            axis.line.y = element_blank() )+
        # Clip the graph to the right size
        coord_cartesian(ylim = c(-gap/2, NA), clip = "off")

        #print(PLOT)
        # Export plots 
        ggsave(filename = paste(wd.out,"/CSS",css,"/4.5.Bar_MiP_number_Sample_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 5, units = "in", dpi = 300)
        
      }
      
      # # / PLOT outlier (CSS11 Farm 10)  ####
      # 
      # 
      # # Remove outlier (CSS11 Farm 10) 
      # df_plot_bar_css11f10=subset(Summary5c_Field, CSS==11 & Farm ==10 )
      # Summary5e_Field_css11f10=subset(Summary5e_Field, CSS==11 & Farm ==10)
      # 
      # df_plot_dot_css=subset(Summary5e_Field_css11f10,  Preparation_Type=="Field_samples")
      #   df_plot_bar_css=subset(  df_plot_bar_css11f10,  Preparation_Type=="Field_samples")
      #   
      #   # OR Exclude extreme case in CSS11: 
      #   #df_plot_dot_css=subset(Summary5e_Field, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples" & Farm.Field %!in% c("F.10.1", "F.10.2") ) 
      #   #df_plot_bar_css=subset( df_plot_bar, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples"& Farm.Field %!in% c("F.10.1", "F.10.2") )
      #   
      #   ggplot()+
      #     geom_point(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, color=factor(N.files)))
      #   
      #   PLOT= ggplot( df_plot_bar_css) +
      #     # Stack bars with Polymer 12 
      #     geom_bar( aes(x=Farm.Field, y=Mean.particles.F*200, fill=Polymer.red12), position="stack", stat="identity")+ 
      #     # Custom color palette
      #     scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
      #                                  "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
      #                                  "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
      #                                  "PMMA"="#a1d99b",   "PC"="#FFF8DC",
      #                                  "CA"= "#FFD39B") , 
      #                       # Relabel  "Other.Plastic"                 
      #                       labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ) ) +
      #     # Add box around the bars: transparent for all fields, black for the MINAGRIS MEAN
      #     geom_bar( aes(x=Farm.Field, y=Mean.particles.F*200, group=Farm.Field, color = Farm.Field), position="stack", stat="summary", fun=sum, fill = "transparent",
      #               size = 1.5) + 
      #     scale_color_manual(values = c("F.1.1"="NA", "F.1.2"="NA", "F.2.1"="NA", "F.2.2"="NA", "F.3.1"="NA", "F.3.2"="NA", "F.4.1"="NA", "F.4.2"="NA", "F.5.1"="NA", "F.5.2"="NA", "F.6.1"="NA", "F.6.2"="NA",
      #                                   "F.7.1"="NA", "F.7.2"="NA", "F.8.1"="NA","F.8.2"="NA", "F.9.1"="NA", "F.9.2"="NA", "F.10.1"="NA", "F.10.2"="NA", "F.11.1"="NA", "F.11.2"="NA", "F.12.1"="NA", "F.12.2"="NA","F.12.3"="NA","F.12.4"="NA", "MEAN"="Black" ))+ 
      #     # Add a min max line, from summary soil, polymer 12,   
      #     geom_linerange(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, ymin = Min.particles.F*200, ymax = Max.particles.F*200))+
      #     guides( color  = "none", )+
      #     # Add a dot when Min.particles.F == Max.particles.F and n>1
      #     new_scale_color() +
      #     geom_point(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.F*200, color=factor(N.files)))+
      #     scale_color_manual(values = c("1"="NA","2"="Black" ))+ 
      #     
      #     
      #     # Titles
      #     ggtitle(paste("CSS ", css, ", ", Txt_translation$Bar_Title_CSS[Txt_translation$CSS==css], sep = ""))+
      #     theme_minimal()+
      #     guides( color  = "none", )+
      #     labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
      #          fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
      #     theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
      #           axis.title.x = element_blank())
      #   
      #   
      #   print(PLOT)
      #   # Export plots 
      #   ggsave(filename = paste(wd.out,"/CSS",css,"/Bar_MiP_number_CSS11Farm10_",Date, ".png",sep =""), plot = PLOT, width = 3.5, height = 4, units = "in", dpi = 300)
      #   
      # 
      
      
  
# 5. Pie. Polymer composition ####
  # * All CSS ####
  
  # Order plot per polymer   
  Summary7c_MINAGRIS$Polymer.red12 = factor(Summary7c_MINAGRIS$Polymer.red12,
                                            levels = Summary7c_MINAGRIS$Polymer.red12[order(-Summary7c_MINAGRIS$Mean.particles.MM)] )
  
  # Add percentages in the plot
  Summary7c_MINAGRIS$MiP_perc <- Summary7c_MINAGRIS$Mean.particles.MM / sum(Summary7c_MINAGRIS$Mean.particles.MM) * 100
  # Creat percentage as text 
  Summary7c_MINAGRIS$MiP_perc_text=paste0(round( Summary7c_MINAGRIS$MiP_perc, 0), "%")
  
  # Do not show percentages < 3.5% 
  Summary7c_MINAGRIS$MiP_perc_text [Summary7c_MINAGRIS$MiP_perc < 3.5]=NA
  
  
  # Translation for each css: 
  for (css in 1:11){  
  
  PLOT= ggplot(Summary7c_MINAGRIS, aes(x="", y=Mean.particles.MM, fill= Polymer.red12 ))+
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                 "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                 "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                 "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                 "CA"= "#FFD39B") , 
                      # Relabel  "Other.Plastic"                 
                      labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css] ) ) +
    geom_text(aes(x=1.3, label = MiP_perc_text ), 
              position = position_stack(vjust = 0.5), size=5  )+
    coord_polar("y", start=0) +
    theme_void() + 
  ggtitle(Txt_translation$Pie_Title_All[Txt_translation$CSS==css] )+
   # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
         fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) #+
    #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
       #   axis.title.x = element_blank() )
  
  PLOT
  
  ggsave(filename = paste(wd.out,"/CSS",css, "/5.1.Pie_MiP_number_Polymer12_ALLCSS_txtCSS", css,"_",Date, ".png", sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  }
  

  # * details of "other polymers" #####
  A=subset(Summary7a_MINAGRIS, Polymer.red12=="Other.Plastic")
  ggplot(data=A, aes(x=Polymer.grp, y=Mean.particles.MM, fill=Size_cat.um))+
    geom_bar(stat="identity", position = "stack", width=1, color="white") 
  
  
  # * Per CSS ####  
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
                      labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ) ) +
    geom_text(aes(x=1.3, label = MiP_perc_text ), 
              position = position_stack(vjust = 0.5), size=5  )+
    coord_polar("y", start=0) +
    theme_void() + 
    ggtitle( paste("CSS ", css, ", ", Txt_translation$Pie_Title_CSS[Txt_translation$CSS==css], sep=""  ))+
    # guides( color  = "none")+
    labs(# y = "Average number of plastic particles per kg of soil",
      fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) #+
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
  #   axis.title.x = element_blank() )
  
  PLOT
  
  ggsave(filename = paste(wd.out,"/CSS",css, "/5.2.Pie_MiP_number_Polymer12_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  }
  
  
  
# 6. Bar.  Polymer composition ####
  # * All CSS ####

ggplot(Summary7c_MINAGRIS, aes(x=Polymer.red12, y=Mean.particles.MM*200, fill= Polymer.red12 ))+
  geom_bar(stat="identity", width=1, color="white") +
  #coord_polar("y", start=0) +
  scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                               "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                               "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                               "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                               "CA"= "#FFD39B") , 
                    # Relabel  "Other.Plastic"                 
                    labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ) ) +
  theme_minimal() + 
  geom_text(aes(label=Polymer.red12) , vjust = -0.5, hjust = 0 , nudge_x = -.5) +
  geom_text(aes(label = paste0(round(MiP_perc, 1), "%")), vjust = 1, nudge_y = 0.2)+
  #ggtitle(paste("Field Samples ; CSS ", css))+
  # guides( color  = "none")+
  labs(# y = "Average number of plastic particles per kg of soil",
    fill = "Polymers identified") +
theme(axis.text.x = element_blank(),
      axis.title.x = element_blank())




  
  # * Per CSS ####
  
  


  
# 7. Dots. S1-S2 per Field ####
  # = min-max with lab color and range bar
  
  # * Per CSS ####
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
     p2
     
  
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
     
    print( PLOT)
     
      ggsave(filename = paste(wd.out,"/CSS",css,"/7.Dot_MiP_number_CSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  } #end for css  
  

  

  
  
  
  
  
# 8. Bar. Area MiP ####

  # * Per CSS #### 

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
  
  
  ggsave(filename = paste(wd.out,"/CSS",css,"/Bar_Area_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 6, units = "in", dpi = 300)
}


ggplot(df_plot ) +
  
  
  
# lab 
  Summary5c_Field_MMEAN2 = subset(Data, Preparation_Type=="Field_samples") %>% 
  group_by( File_Names, Polymer.red12, Sample_type, Preparation_Type, CSS, Farm, Field, Lab   ) %>% # For each PMF_File_Name, get the summary
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
