

library(tidyverse)
library(hrbrthemes)
library(stats)
library(readxl)
library(ggnewscale)
library(ggthemes)
library(scales)

rm(list=ls()) # cleaning console
graphics.off() # cleaning plots

'%!in%' <- function(x,y)!('%in%'(x,y))
label_at <- function(n) function(x) ifelse(x %% n == 0, x, "")

#setwd("C:/Users/berio001/Minagris/MINAGRIS_Microplastic_Soil_Assessmnent")
wd.out="Outputs/2025_08"  #//\\ #wd.out="//WURNET.NL/Homes/berio001/My Documents/R"

# set encoding to read csv files correctly
options(encoding = "UTF-8") #"UTF-8" "latin-1"

# Set seed for jitter
set.seed(42^2)

# 1. Load data####

  # # * MiP table ####
  #   df_MiP=read.csv("Outputs/Corrected_MiP_Particles.csv")
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
  #   Summary7c_CSS=read.csv("Outputs/Summary8c_MINAGRIS_20241118.csv")
  #     
  #     
  #   Summary8c_MINAGRIS=read.csv("Outputs/Summary8c_MINAGRIS_20241118.csv")
  #   
    
# 2. Outline Format ####
    
    # * Titles and translations #### 
# To generate graphs in the css language :     
   Txt_translation=read_excel("Txt_translation_table.xlsx")%>%
      mutate(across(everything(), ~ str_replace_all(., "\\\\n", "\n")))

# To generate all graph with english text: 
Txt_translation=read_excel("Txt_translation_table_eng.xlsx")%>%
  mutate(across(everything(), ~ str_replace_all(., "\\\\n", "\n")))


    Cat.um.txt=c("90-300", "300-510", "510-720",
                 "720-930", "930-1140", "1140-2000")
    
    Cat.um.txt2=c("90-300", "300-2000")
    
    Date="2025.08_eng"
    Date=""
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
    
#
    
# 3. Hist. Size distribution ####
  
  # Check Available sizes 
  A=subset(df_MiP, Area.um2.cor !=0 & Area.um2.cor <7885.44 & Length.um<88.042)
  B=subset(df_MiP, Area.um2.cor > 1180^2)

  # source("MINAGRIS_Custom_Histogram.R") Work in progress
  # MINAGRIS_Custom_Histogram(Data) 

  # For loop per version: # (Maybe not the most eleguant coding but at least it is more organized)
  
  # * 3.1. Number Per CSS, Mean Farms, Sum Polymers ####
  Summary8f_MINAGRIS$Size_cat.um
    # One value per Size_cat.um
    Summary8f_MINAGRIS$Size_cat.um= factor(Summary8f_MINAGRIS$Size_cat.um,
                                           levels =  Cat.um.txt )
  #check for NAs
    Summary8f_MINAGRIS[is.na(Summary8f_MINAGRIS$Size_cat.um),] # potentially  1 NA: "Too small"
    
  # Translation for each css: 
  for (css in 1:11){  
    
    PLOT= ggplot( subset(Summary8f_MINAGRIS, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles.MM*200)) +
      geom_bar(position="stack", stat="identity", fill="steelblue4")+ 
      ggtitle(Txt_translation$Hist_Title_All[Txt_translation$CSS==css])+
      theme_minimal()+
        labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==css]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT

    ggsave(filename = paste(wd.out,"/CSS",css,"/3.1.Hist_MiP_SizeDistribution_AllCSS_AllPolymers_txtCSS", css, ".png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
    
  }
  
  # * 3.2. Number Per CSS, Mean Farms, Per Polymer   #####
    # One value per {CSS Polymer per Size_cat.um
    
    Summary8b_MINAGRIS %>% 
      group_by(Size_cat.um)%>% 
      summarise( Mean.particles=sum(Mean.particles.MM))
    
    Summary8b_MINAGRIS$Size_cat.um= factor(Summary8b_MINAGRIS$Size_cat.um,
                               levels =  Cat.um.txt )
    #check for NAs
    Summary8b_MINAGRIS[is.na(Summary8b_MINAGRIS$Size_cat.um) & Summary8b_MINAGRIS$Mean.particles.MM> 0,] # potentially  1 NA: "Too small"
    
    #check big particles 
    subset(Summary8b_MINAGRIS, Size_cat.um=="1140-2000" & Mean.particles.MM != 0)
    
  
  # Translation for each css: 
  for (css in 1:11){  
    PLOT= ggplot( subset(Summary8b_MINAGRIS, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles.MM*200, fill=Polymer.red12)) +
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
  
  
  # * 3.3. Number Per CSS, Per Farm, Sum Polymers #### 
  # One value per CSS* Size_cat.um
  
  # Order per Size_cat.um
  Summary7f_CSS$Size_cat.um= factor(Summary7f_CSS$Size_cat.um,
                                    levels =  Cat.um.txt )
  
  for (css in 1:11){  
    
    PLOT= ggplot( subset(Summary7f_CSS, Size_cat.um %in%  Cat.um.txt & CSS == css), aes(x=Size_cat.um, y=Mean.particles.CSS*200)) +
      geom_bar(position="stack", stat="identity", fill="steelblue4")+ 
      ggtitle(paste("CSS ", css, ", ", Txt_translation$Hist_Title_CSS[Txt_translation$CSS==css], sep = ""))+
      theme_minimal()+
      labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==css]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )

    ggsave(filename = paste(wd.out,"/CSS",css,"/3.3.Hist_MiP_SizeDistribution_CSS", css, "_AllPolymers.png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
    
  }
  
 
  
  # * 3.4. Number Per CSS, Per Farm, Per Polymer   #####
  # One value per CSS*Polymer*Size_cat.um
    Summary7b_CSS %>% 
      group_by(Size_cat.um)%>% 
      summarise( Mean.particles=sum(Mean.particles.CSS))
    
    # Order per Size_cat.um
    Summary7b_CSS$Size_cat.um= factor(Summary7b_CSS$Size_cat.um,
                                      levels =  Cat.um.txt )
    
    #check for NAs
    Summary7b_CSS[is.na(Summary7b_CSS$Size_cat.um) & Summary7b_CSS$Mean.particles.CSS> 0,] # potentially  1 NA: "Too small"
    
    #check big particles 
    subset(Summary7b_CSS, Size_cat.um=="1140-2000" & Mean.particles.CSS != 0)
    
 
  
  for (css in 1:11){  
    PLOT= ggplot( subset(Summary7b_CSS, Size_cat.um %in%  Cat.um.txt & CSS == css), aes(x=Size_cat.um, y=pmax(Mean.particles.CSS,0)*200, fill=Polymer.red12)) +
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
  
  # * 3.5. Number Per lab, Mean Farms, Sum Polymers #### 
  # One value per Size_cat.um
    Summary10f_Lab%>% 
      group_by(Size_cat.um)%>% 
      summarise( Mean.particles=sum(Mean.particles.MM))
    
    
  Summary10f_Lab$Size_cat.um= factor(Summary10f_Lab$Size_cat.um,
                                         levels =  Cat.um.txt )
  
    PLOT= ggplot( subset(Summary10f_Lab, Size_cat.um %in%  Cat.um.txt), aes(x=Size_cat.um, y=Mean.particles.MM*200, fill = Lab)) +
    facet_wrap(~Lab)+
      geom_bar(position="stack", stat="identity", )+ 
      scale_fill_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
      ggtitle(Txt_translation$Hist_Title_All[Txt_translation$CSS==css])+
      theme_minimal()+
      labs(y = "Average number of MiP per kg soil",
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==3]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    
    PLOT
    ggsave(filename = paste(wd.out,"/3.5.Hist_MiP_SizeDistribution_Lab_AllPolymers.png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
    
    # * 3.6. Number Per lab, Mean Farms, per Polymers #### 
    # One value per Size_cat.um
    Summary10b_Lab$Size_cat.um= factor(Summary10b_Lab$Size_cat.um,
                                       levels =  Cat.um.txt )
    
    PLOT= ggplot( subset(Summary10b_Lab, Size_cat.um %in%  Cat.um.txt ), aes(x=Size_cat.um, y=pmax(Mean.particles.MM,0)*200, fill=Polymer.red12)) +
      facet_wrap(~Lab)+
      geom_bar(position="stack", stat="identity")+ 
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==3]  ) ) +
      ggtitle(Txt_translation$Hist_Title_All[Txt_translation$CSS==3])+
      theme_minimal()+
      labs(y = "Average number of MiP per kg soil",
           x= Txt_translation$Size_categories_txt[Txt_translation$CSS==3],
           fill =  Txt_translation$Polymers_identified[Txt_translation$CSS==3]) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5) )
    PLOT 
    ggsave(filename = paste(wd.out,"/3.6.Hist_MiP_SizeDistribution_Lab_PerPolymer12.png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
    
    

# 4. BarPlot, Number MiP per kg soil ####
    
  # ////////////////////////////////////////////////////////////////////////////////////////////
  # Show Field average 
  # pmax(Mean.particles, 0) sets all negative Mean.particles values to 0
  # ////////////////////////////////////////////////////////////////////////////////////////////
    
  #For loop per version: # (Maybe not the most eleguant coding but at least it is more organized)
  
  # * 4.1. Per CSS, Sum Farms, Sum Polymers ####
  # -> Average (and median), jitter dots, no min-max, max out of bound as label

  
  # Initialise Plot
  # Data frame for the bars 
  df_plot_bar=  Summary7c_CSS_outlier 
  df_plot_bar_css=subset( df_plot_bar,  Preparation_Type=="Field_samples")
  
  # Data frame for the min-max 
  df_plot_dot_css=subset(Summary7e_CSS,  Preparation_Type=="Field_samples")
  
  # Truncation over 50 MiP/ sample <-> 10 000 MiP/ kg soil
  y_max=50
  y_max_mm2=3.5
  # text if the max is met
  df_plot_dot_css$Trunc=NA
  df_plot_dot_css$Trunc[ df_plot_dot_css$Max.particles.CSS> y_max]= paste( "max=",
                                                                           round(df_plot_dot_css$Max.particles.CSS[ df_plot_dot_css$Max.particles.CSS> y_max] *200,digits = 0),
                                                                           sep = "" )
  df_plot_dot_css$Trunc_mm2=NA
  df_plot_dot_css$Trunc_mm2[ df_plot_dot_css$Max.Tot.Area.mm2.CSS> y_max_mm2]= paste( "max=",
                                                                           round(df_plot_dot_css$Max.Tot.Area.mm2.CSS[ df_plot_dot_css$Max.Tot.Area.mm2.CSS> y_max_mm2] *200,digits = 0),
                                                                           " mm2", sep = ""  )
  
  # Cut at y_max
  df_plot_dot_css$Max.particles.CSS[ df_plot_dot_css$Max.particles.CSS> y_max]= y_max
  df_plot_dot_css$Max.Tot.Area.mm2.CSS[ df_plot_dot_css$Max.Tot.Area.mm2.CSS> y_max_mm2]= y_max_mm2
  
  # Add "CSS"abreviation in x-axis   
  df_plot_bar_css$CSS=factor(paste("CSS", df_plot_bar_css$CSS),
                             levels =  c("CSS 1", "CSS 2", "CSS 3", "CSS 4", "CSS 5", "CSS 6",
                                         "CSS 7", "CSS 8", "CSS 9", "CSS 10", "CSS 11") )
  
  # Translation for each css: 
  for (css in 1:11){ 
    # *** Mean Number #####
    PLOT= ggplot() +
      # Stack bars with Polymer 12 
      geom_bar(data=df_plot_bar_css, aes(x=CSS, y=Mean.particles.CSS*200,fill="Average all farms"),  stat="identity") + 
      scale_fill_manual(values = "#20b8bd", labels=Txt_translation$Average_Farms[Txt_translation$CSS==css])+
      
      # # Add a min max line,
      # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
      # Add text when y max is reached
      geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max*201.9, label=Trunc )) +
      
      # Add the x-axis line
      geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
      
      # Add the y-axis 
      geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
      
      # Add jitter
      #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
      geom_point(data= Summary5e_Field, aes(x=CSS, y=pmax(Mean.particles.Fd,0)*200, color="Individual field" ), alpha = 0.25, position = position_jitter(seed=43^3))+
      scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
      
      ylim(0,10500)+
      # Titles
      ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
      
      
      # custom y scale 
      scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
                         labels = label_at(1000), #,                                        # label every second break
                         limits = c(-y_max*200/40, y_max*202+20))+ #limits goes beyond to allow the vertical ticks between farms
      
      
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
      
      # Add the axis.text.x at the right position
      annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
      # Clip the graph to the right size
      coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
    
    
    print( PLOT)
    ggsave(filename = paste(wd.out,"/CSS",css,"/4.1.Bar_Mean_MiP_SumPolymer_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
    
    # *** Mean Area #####
    
    
    PLOT= ggplot() +
      # Stack bars with Polymer 12 
      geom_bar(data=df_plot_bar_css, aes(x=CSS, y=Mean.Tot.Area.mm2.CSS*200,fill="Average all farms"),  stat="identity") + 
      scale_fill_manual(values = "#20b8bd", labels=Txt_translation$Average_Farms[Txt_translation$CSS==css])+
      
      # # Add a min max line,
      # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
      # Add text when y max is reached
      geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max_mm2*(201.9), label=Trunc_mm2 )) +
      
      # Add the x-axis line
      geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
      
      # Add the y-axis 
      geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max_mm2*200, yend = 0), color = "black", size = 1) +
      
      # Add jitter
      #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
      geom_point(data= Summary5e_Field, aes(x=CSS, y=pmax(Mean.Tot.Area.mm2.Fd,0)*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
      scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
      
      ylim(0,700)+
      # Titles
      ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
      
      
      # custom y scale 
      scale_y_continuous(breaks = seq(0, y_max_mm2*200, 100), # Have a break for each gap
                         labels = label_at(200), #,                                        # label every second break
                         limits = c(-y_max_mm2*200/40, y_max_mm2*202+20))+ #limits goes beyond to allow the vertical ticks between farms
      
      
      theme_minimal()+
      #guides( color  = "none")+
      labs(y = expression("Average surface of plastic per kg of soil  [mm" ^ 2*"]"),
           fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
           color=element_blank() ) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_line(color = "black"),
            axis.line.y = element_blank(),
            legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
            panel.grid.minor.y = element_blank())+ # Remove the minor grid 
      
      # Add the axis.text.x at the right position
      annotate("text", x = df_plot_dot_css$CSS, y = -y_max_mm2*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
      # Clip the graph to the right size
      coord_cartesian(ylim = c(-y_max_mm2*200/40, NA), clip = "off")
    
    
    #print( PLOT)
    ggsave(filename = paste(wd.out,"/CSS",css,"/4.1.Bar_MeanArea_MiP_SumPolymer_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
    
    
    
  } # end Translation for each css, 4.1
  
  
  
  
  # * 4.2. Per CSS, Sum Farms, Per Polymer ####
  # -> Average (and median), no dot, no min-max, no max
  
  # Data frame for the bars 
  df_plot_bar_css=subset( Summary7c_CSS_outlier ,  Preparation_Type=="Field_samples")
  
  # Data frame for the min-max 
  df_plot_dot_css=subset(Summary7e_CSS,  Preparation_Type=="Field_samples")
  
  # Truncation over 50 MiP/ sample <-> 10 000 MiP/ kg soil
  y_max=30
  y_max_mm2=2
  # text if the max is met
  df_plot_dot_css$Trunc=NA
  df_plot_dot_css$Trunc[ df_plot_dot_css$Max.particles.CSS> y_max]= paste( "max=",
                                                                           round(df_plot_dot_css$Max.particles.CSS[ df_plot_dot_css$Max.particles.CSS> y_max] *200,digits = 0),
                                                                           sep = "" )
  df_plot_dot_css$Trunc_mm2=NA
  df_plot_dot_css$Trunc_mm2[ df_plot_dot_css$Max.Tot.Area.mm2.CSS> y_max_mm2]= paste( "max=",
                                                                                      round(df_plot_dot_css$Max.Tot.Area.mm2.CSS[ df_plot_dot_css$Max.Tot.Area.mm2.CSS> y_max_mm2] *200,digits = 0),
                                                                                      " mm2", sep = ""  )
  
  # Cut at y_max
  df_plot_dot_css$Max.particles.CSS[ df_plot_dot_css$Max.particles.CSS> y_max]= y_max
  df_plot_dot_css$Max.Tot.Area.mm2.CSS[ df_plot_dot_css$Max.Tot.Area.mm2.CSS> y_max_mm2]= y_max_mm2
  
  
  # Add "CSS"abreviation in x-axis   
  df_plot_bar_css$CSS=factor(paste("CSS", df_plot_bar_css$CSS),
                             levels =  c("CSS 1", "CSS 2", "CSS 3", "CSS 4", "CSS 5", "CSS 6",
                                         "CSS 7", "CSS 8", "CSS 9", "CSS 10", "CSS 11") )
  
  # Translation for each css: 
  for (css in 1:11){  
    # *** Mean Number #####
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
      # Add text when y max is reached
      geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max*201.9, label=Trunc )) +
      # # Add jitter
      # geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
      
      # Titles
      ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
      
      # Add the x-axis line
      geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
      
      # Add the y-axis 
      geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
      
      # custom y scale 
      scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap # y_max*200/8
                         labels = label_at(1000), #,                                        # label every second break
                         limits = c(-y_max*200/40, y_max*202+20))+ #limits goes beyond to allow the vertical ticks between farms
      
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
      
      # Add the axis.text.x at the right position
      annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
      # Clip the graph to the right size
      coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
    
    
    #print( PLOT)
    ggsave(filename = paste(wd.out,"/CSS",css,"/4.2.Bar_Mean_MiP_Polymer12_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
    # *** Mean Area #####
    PLOT= ggplot( df_plot_bar_css) +
      # Stack bars with Polymer 12 
      geom_bar( aes(x=CSS, y=pmax(Mean.Tot.Area.mm2.CSS,0)*200, fill=Polymer.red12), position="stack", stat="identity") + 
      # Custom color palette
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css] ) ) +
      
      # Add text when y max is reached
      geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max_mm2*201.9, label=Trunc_mm2 )) +
      # # Add jitter
      # geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
      
      # Titles
      ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
      
      # Add the x-axis line
      geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
      
      # Add the y-axis 
      geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max_mm2*200, yend = 0), color = "black", size = 1) +
      
      # custom y scale 
      scale_y_continuous(breaks = seq(0, y_max_mm2*200, 100), # Have a break for each gap
                         labels = label_at(200), #,                                        # label every second break
                         limits = c(-y_max_mm2*200/40, y_max_mm2*202+20))+ #limits goes beyond to allow the vertical ticks between farms
      
      # Theme 
      theme_minimal()+
      guides( color  = "none")+
      labs(y = expression("Average surface of plastic per kg of soil  [mm" ^ 2*"]"),
           fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.y = element_line(color = "black"),
            axis.line.y = element_blank(),
            panel.grid.minor.y = element_blank())+
      
      # Add the axis.text.x at the right position
      annotate("text", x = df_plot_dot_css$CSS, y = -y_max_mm2*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
      # Clip the graph to the right size
      coord_cartesian(ylim = c(-y_max_mm2*200/40, NA), clip = "off")
    
   # print( PLOT)
    ggsave(filename = paste(wd.out,"/CSS",css,"/4.2.Bar_MeanArea_MiP_Polymer12_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
    
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
    #   geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
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
    #   # Add the axis.text.x at the right position
    #   annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
    #   # Clip the graph to the right size
    #   coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
    # 
    # 
    # print( PLOT)
    # ggsave(filename = paste(wd.out,"/CSS",css,"/Bar_Median_MiP_Polymer12_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
    
  } # end Translation for each css, 4.2
  
  
  PLOT
  
  
  
 
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
        gap= round(max(df_plot_dot_css$Mean.particles.Fd)/5)*100
        if (gap>1000){
          gap=round(gap/500)*500
        }
        
        # plot number of files 
        ggplot()+
          geom_point(data=df_plot_dot_css, aes(x=Farm.Field, y=Mean.particles.Fd*200, color=factor(N.files)), shape = 1)
        
        # Plot 
        PLOT= ggplot( ) +
          # Stack bars with Polymer 12 
          geom_bar( data=df_plot_bar_css,aes(x=Farm.Field, y=pmax(Mean.particles.Fd,0)*200, fill=Polymer.red12), position="stack", stat="identity")+ 
          # Custom color palette
          scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                       "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                       "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                       "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                       "CA"= "#FFD39B") , 
                            # Relabel  "Other.Plastic"                 
                            labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ) ) +
          
          # custom y scale 
          scale_y_continuous(breaks = seq(0, max(df_plot_dot_css$Mean.particles.Fd)*200, gap), # Have a break for each gap  
                             labels = label_at(gap*2),                                        # label every second break  
                             limits = c(-3*gap/4, max(df_plot_dot_css$Mean.particles.Fd)*202+80))+ #limits goes beyond to allow the vertical ticks between farms
          
          # Titles
          ggtitle(paste("CSS ", css, ", ", Txt_translation$Bar_Title_CSS[Txt_translation$CSS==css]," ",Txt_translation$Field[Txt_translation$CSS==css], sep = ""))+
          
          # Add the x-axis line
          geom_segment(aes(x = 0.4, xend = max(Farm_boundaries), y = 0, yend = 0), color = "black", size = 1) + #geom_hline(yintercept = 0, color = "black", size = 1) +
          
          # Add the y-axis 
          geom_segment(aes(x = 0.4, xend = 0.4, y = max(df_plot_dot_css$Mean.particles.Fd)*200+80, yend = -gap/2), color = "black", size = 1) +
          
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
        
       # print(PLOT)
        # Export plots 
        ggsave(filename = paste(wd.out,"/CSS",css,"/4.4.Bar_MiP_number_nobar_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 5, units = "in", dpi = 300)
        
      } # End for CSS loop, 4.4
  
      
      
      # *** PLOT outlier farm (CSS11 Farm 10)  ####


      # Select outlier (CSS11 Farm 10)
      df_plot_bar_css11f10=subset(Summary5c_Field, CSS==11 & Farm ==10 )
      Summary5e_Field_css11f10=subset(Summary5e_Field, CSS==11 & Farm ==10)

      df_plot_dot_css=subset(Summary5e_Field_css11f10,  Preparation_Type=="Field_samples")
      df_plot_bar_css=subset(df_plot_bar_css11f10,  Preparation_Type=="Field_samples")

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
        gap= round(max(df_plot_dot_css$Mean.particles.Fd)/5)*100
        if (gap>1000){
          gap=round(gap/500)*500
        }
        
        gap=10000
        
        
        PLOT_11.10= ggplot( ) +
          # Stack bars with Polymer 12 
          geom_bar( data=df_plot_bar_css,aes(x=Farm.Field, y=Mean.particles.Fd*200, fill=Polymer.red12), position="stack", stat="identity")+ 
          # Custom color palette
          scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                       "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                       "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                       "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                       "CA"= "#FFD39B") , 
                            # Relabel  "Other.Plastic"                 
                            labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css]  ) ) +
          
          # Titles
          # ggtitle(paste("CSS ", css, ", ", Txt_translation$Bar_Title_CSS[Txt_translation$CSS==css]," ",Txt_translation$Field[Txt_translation$CSS==css], sep = ""))+
          
          # Add the x-axis line
          geom_segment(aes(x = 0.4, xend = max(Farm_boundaries), y = 0, yend = 0), color = "black", size = 1) + #geom_hline(yintercept = 0, color = "black", size = 1) +
          
          # Add the y-axis 
          geom_segment(aes(x = 0.4, xend = 0.4, y = 80000, yend = -gap/2), color = "black", size = 1) +
          
          # Add vertical ticks between farms: 
          geom_segment(data = data.frame(x = Farm_boundaries), aes(x = x, xend = x, y = 0, yend = -gap/2),
                       color = "black", size = .8) +
          
          # custom y scale 
          # scale_y_continuous(breaks = seq(0, max(df_plot_dot_css$Mean.particles.F)*200, gap), # Have a break for each gap  
          #                    labels = label_at(gap*2),                                        # label every second break  
          #                    limits = c(-3*gap/4, max(df_plot_dot_css$Mean.particles.F)*200))+ #limits goes beyond to allow the vertical ticks between farms
          
          scale_y_continuous(
            breaks = seq(0, 80000, 10000),  # Ticks every 10,000
            labels = function(x) ifelse(x %% 20000 == 0, label_number()(x), ""),  # Labels only for multiples of 20,000
            limits = c(-3*gap/4, 80000)
          ) +
          
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
        
        
        print(PLOT_11.10)
        
        # Export plots
        ggsave(filename = paste(wd.out,"/CSS",css,"/4.4.Bar_MiP_number_nobar_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 5, units = "in", dpi = 300)
        ggsave(filename = paste(wd.out,"/CSS",css,"/4.4.Bar_MiP_number_CSS11Farm10_",Date, ".png",sep =""), plot = PLOT_11.10, width = 3.35, height = 5, units = "in", dpi = 300)
        
        
        
        # grid.arrange(PLOT, PLOT_11.10, ncol = 2) 
        # ggarrange(PLOT, PLOT_11.10, ncol = 2)

       
      
      
      
  
  # * 4.5. Per CSS, Per Sample, Per Polymer ???? ####
  
    # Plot, one bar per Sample  #
      # Prepare the data all sum all per Soil Sample
      # Add a line for the field with a missing soil
     Summary4e_Soilcomp=   Summary4e_Soil %>%
        ungroup() %>%
        complete(Lab, nesting(CSS, Farm, Field, Preparation_Type),
                 fill=list(Mean.particles.S=0,
                           Mean.px.S=0,
                           Mean.Tot.Area.mm2.S=0,
                           Mean.Tot.Mass.ng.S=0,
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
      Plot_Summary6e_Farm=subset(Summary6e_Farm, CSS!=11 | Farm !=10 )
      
      
      for (css in 1:11){  
        df_plot_dot_css=subset(Plot_Summary4e_Soil, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
        df_plot_bar_Sample_css=subset(Plot_Summary4c_Soil, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
        df_plot_dot_farm=subset(Plot_Summary6e_Farm, CSS %in% c(css,"MEAN") & Preparation_Type=="Field_samples")
        
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
        gap= round(max(df_plot_dot_css$Mean.particles.S)/5)*100
        if (gap>1000){
          gap=round(gap/500)*500
        }


        # Order plot_dot by Farm.Field.Lab
        df_plot_dot_css$Farm.Field.Lab=factor(df_plot_dot_css$Farm.Field.Lab,
                               levels = df_plot_dot_css$Farm.Field.Lab[order(df_plot_dot_css$Farm.Field)] )
        
        PLOT=ggplot( ) +
          # Stack bars with Polymer 12 
          geom_bar( data=  df_plot_bar_Sample_css,aes(x=Farm.Field.Lab, y=pmax(Mean.particles,0)*200,   fill=Polymer.red12), position="stack", stat="identity") + 
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
          geom_point(data=df_plot_dot_css, aes(x=Farm.Field.Lab, y=Mean.particles.S*200, shape= factor(N.extract)))+
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
          geom_segment(aes(x = 0.4, xend = 0.4, y = max(df_plot_dot_css$Mean.particles.S)*200, yend = -gap/2), color = "black", size = 1) +
          
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
        scale_y_continuous(breaks = seq(0, max(df_plot_dot_css$Mean.particles.S)*200, gap), # Have a break for each gap  
                             labels = label_at(gap*2),                                        # label every second break  
                             limits = c(-gap, max(df_plot_dot_css$Mean.particles.S)*202+20))+ #limits goes beyond to allow the vertical ticks between farms
          
          
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

        print(PLOT)
        # Export plots 
        ggsave(filename = paste(wd.out,"/CSS",css,"/4.5.Bar_MiP_number_Sample_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 5, units = "in", dpi = 300)
        
      }
      
    

      # *** PLOT outlier farm (CSS11 Farm 10)  ####
      
      

      
      # Select outlier (CSS11 Farm 10)
        df_plot_dot_css=subset(Summary4e_Soilcomp, CSS==11 & Farm ==10   & Preparation_Type=="Field_samples")
        df_plot_bar_Sample_css=subset( Summary4c_Soilcomp, CSS==11 & Farm ==10  & Preparation_Type=="Field_samples")
        df_plot_dot_farm=subset(Summary6e_Farm, CSS==11 & Farm ==10  & Preparation_Type=="Field_samples")
        
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
        gap= round(max(df_plot_dot_css$Mean.particles.S)/5)*100
        if (gap>1000){
          gap=round(gap/500)*500
        }
        
        
        # Order plot_dot by Farm.Field.Lab
        df_plot_dot_css$Farm.Field.Lab=factor(df_plot_dot_css$Farm.Field.Lab,
                                              levels = df_plot_dot_css$Farm.Field.Lab[order(df_plot_dot_css$Farm.Field)] )
        
        PLOT_11.10=ggplot( ) +
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
          geom_point(data=df_plot_dot_css, aes(x=Farm.Field.Lab, y=Mean.particles.S*200+gap/5, shape= factor(N.extract)))+
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
          geom_segment(aes(x = 0.4, xend = 0.4, y = max(df_plot_dot_css$Mean.particles.S)*200, yend = -gap/2), color = "black", size = 1) +
          
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
          scale_y_continuous(breaks = seq(0, max(df_plot_dot_css$Mean.particles.S)*200, gap), # Have a break for each gap  
                             labels = label_at(gap*2),                                        # label every second break  
                             limits = c(-gap, max(df_plot_dot_css$Mean.particles.S)*202+20))+ #limits goes beyond to allow the vertical ticks between farms
          
          
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
        
       
        # Export plots 
      
      print(PLOT_11.10)
      
      # Export plots
      ggsave(filename = paste(wd.out,"/CSS",css,"/4.4.Bar_MiP_number_nobar_CSS", css, "_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 5, units = "in", dpi = 300)
      ggsave(filename = paste(wd.out,"/CSS",css,"/4.5.Bar_MiP_number_CSS11Farm10_",Date, ".png",sep =""), plot = PLOT_11.10, width = 3.35, height = 5, units = "in", dpi = 300)
      
      
      
      
  #     
  #     # * 4.6. Per cropping system, Sum Polymers (CSS translation?) #####     
  #     # -> Average (and median), jitter dots, no min-max, max out of bound as label
  #     
  #     unique(Summary9c_Crop_outlier$CropDominant4)
  #     Summary9c_Crop_outlier$CropDominant4[is.na(Summary9c_Crop_outlier$CropDominant4)]="na"
  #     
  #     Summary4e_Soil$CropDominant4[is.na(Summary4e_Soil$CropDominant4)]="na"
  #     
  #     Summary9e_Crop$CropDominant4[is.na( Summary9e_Crop$CropDominant4)]="na"
  #     
  #     Summary9d_Crop$CropDominant4[is.na(  Summary9d_Crop$CropDominant4)]="na"
  #     
  #     # Initialise Plot
  #     # Data frame for the bars 
  #     df_plot_bar=  Summary9c_Crop_outlier
  #     df_plot_bar_css=subset( df_plot_bar,  Preparation_Type=="Field_samples")
  #     
  #     # Data frame for the min-max 
  #     df_plot_dot_css=subset(Summary9e_Crop,  Preparation_Type=="Field_samples")
  # 
  #      # Truncation over 50 MiP/ sample <-> 10 000 MiP/ kg soil
  # y_max=50
  # y_max_mm2=3.5
  # # text if the max is met
  # df_plot_dot_css$Trunc=NA
  # df_plot_dot_css$Trunc[ df_plot_dot_css$Max.particles.Crop> y_max]= paste( "max=",
  #                                                                          df_plot_dot_css$Max.particles.Crop[ df_plot_dot_css$Max.particles.Crop> y_max] *200,
  #                                                                          sep = "" )
  # df_plot_dot_css$Trunc_mm2=NA
  # df_plot_dot_css$Trunc_mm2[ df_plot_dot_css$Max.Tot.Area.mm2.Crop> y_max_mm2]= paste( "max=",
  #                                                                          round(df_plot_dot_css$Max.Tot.Area.mm2.Crop[ df_plot_dot_css$Max.Tot.Area.mm2.Crop> y_max_mm2] *200,digits = 0),
  #                                                                          " mm2", sep = ""  )
  # 
  # # Cut at y_max
  # df_plot_dot_css$Max.particles.Crop[ df_plot_dot_css$Max.particles.Crop> y_max]= y_max
  # df_plot_dot_css$Max.Tot.Area.mm2.Crop[ df_plot_dot_css$Max.Tot.Area.mm2.Crop> y_max_mm2]= y_max_mm2
  #     
  #     # Add "Crop"abreviation in x-axis   
  #     #df_plot_bar_css$CSS=factor(paste("CSS", df_plot_bar_css$CSS),
  #                                # levels =  c("CSS 1", "CSS 2", "CSS 3", "CSS 4", "CSS 5", "CSS 6",
  #                                #             "CSS 7", "CSS 8", "CSS 9", "CSS 10", "CSS 11") )
  #     
  #     # Translation for each css: 
  #     #for (css in 1:11){ 
  #     css=3
  #       # *** MEAN number #####
  #       PLOT= ggplot() +
  #         # Stack bars with Polymer 12 
  #         geom_bar(data=df_plot_bar_css, aes(x=CropDominant4, y=Mean.particles.Crop*200, fill="Average all Soils"),  stat="identity") + 
  #         scale_fill_manual(values = "brown", labels="Average all Soils")+ # labels=Txt_translation$Average_Soil[Txt_translation$CSS==css]
  #         
  #         # # Add a min max line,
  #         # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
  #         # Add text when y max is reached
  #         geom_text(data=df_plot_dot_css, aes(x=CropDominant4, y=y_max*201.9, label=Trunc )) +
  #         
  #         # Add the x-axis line
  #         geom_segment(aes(x = 0.4, xend = nrow(Summary9d_Crop)-0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  #         
  #         # Add the y-axis 
  #         geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
  #         
  #         # Add jitter
  #         #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  #         geom_point(data= Summary4e_Soil, aes(x=CropDominant4, y=Mean.particles*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
  #         scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
  #         
  #         ylim(0,10500)+
  #         # Titles
  #         ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  #         
  #         
  #         # custom y scale 
  #         scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
  #                            labels = label_at(1000), #,                                        # label every second break
  #                            limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
  #         
  #         theme_minimal()+
  #         #guides( color  = "none")+
  #         labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
  #              #fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
  #              color=element_blank() ) +
  #         theme(#axis.text.x = element_blank(),
  #               axis.title.x = element_blank(),
  #               axis.ticks.y = element_line(color = "black"),
  #               axis.line.y = element_blank(),
  #               legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
  #               panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
  #         
  #         # Add the axis.text.x at the right position
  #         #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  #         # Clip the graph to the right size
  #         #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  #       
  #       
  #       print( PLOT)
  #     
  #       
  #     #} # end Translation for each css, 4.6
  #       ggsave(filename = paste(wd.out,"/4.6.Bar_Mean_MiP_SumPolymer_Crop_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
  #     
  #       
  #       # *** Box plot number2 #####
  #       
  #      
  #       
  #       df_plot_box= subset(Summary4e_Soil, CropDominant4 %!in% c("na", "fallow"))
  #       #df_plot_box= subset(Summary4e_Soil, CropDominant4 != "fallow")
  #       
  #       # Add "Crop"abreviation in x-axis   
  # 
  #       
  #       PLOT= ggplot() +
  #         # Stack bars with Polymer 12 
  #         geom_boxplot(data=df_plot_box, aes(x=CropDominant4, y=Mean.particles.S*200, fill="Average all Soils"), outlier.shape = NA) + 
  #         scale_fill_manual(values = "brown", labels="Average all Soils")+ # labels=Txt_translation$Average_Soil[Txt_translation$CSS==css]
  #         scale_x_discrete( labels = c( "arable_crops" = "Arable", "greenhouse_vegetable"="Greenhouse vegetables", "mixed" = "Mixed rotations", "orchards"= "Orchards", "other_vegetable" = "Other Vegetables" ) ) +
  #         # # Add a min max line,
  #         # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
  #         # Add text when y max is reached
  #         geom_text(data=subset(df_plot_dot_css, CropDominant4 %!in% c("na", "fallow")), aes(x=CropDominant4, y=y_max*201.9, label=Trunc )) +
  #         
  #         # Add the x-axis line
  #        # geom_segment(aes(x = 0.4, xend = nrow(Summary9d_Crop)-0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  #         
  #         # Add the y-axis 
  #         geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
  #         
  #         # Add jitter
  #         #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  #         geom_point(data= subset(Summary4e_Soil, CropDominant4 %!in% c("na", "fallow") ), aes(x=CropDominant4, y=Mean.particles.S*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
  #         scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
  #         
  #         ylim(0,10500)+
  #         # Titles
  #         ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  #         
  #         
  #         # custom y scale 
  #         scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
  #                            labels = label_at(1000), #,                                        # label every second break
  #                            limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
  #         
  #         theme_minimal()+
  #         #guides( color  = "none")+
  #         labs(y = "Average number MiP per kg of soil",
  #              #fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
  #              color=element_blank() ) +
  #         theme(#axis.text.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.ticks.y = element_line(color = "black"),
  #           axis.line.y = element_blank(),
  #           legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
  #           panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
  #       
  #       # Add the axis.text.x at the right position
  #       #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  #       # Clip the graph to the right size
  #       #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  #       
  #       
  #       print( PLOT)
  #       
  #       
  #       #} # end Translation for each css, 4.6
  #       ggsave(filename = paste(wd.out,"/4.6.Box_Mean_MiP_SumPolymer_Crop_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
  #       
  #       
  #       # *** Box plot area2 #####
  #       
  #       
  #       
  #       df_plot_box= subset(Summary4e_Soil, CropDominant4 %!in% c("na", "fallow"))
  #       #df_plot_box= subset(Summary4e_Soil, CropDominant4 != "fallow")
  #       
  #       # Add "Crop"abreviation in x-axis   
  #       
  #       
  #       PLOT= ggplot() +
  #         # Stack bars with Polymer 12 
  #         geom_boxplot(data=df_plot_box, aes(x=CropDominant4, y=Mean.Tot.Area.mm2.S*200, fill="Average all Soils"), outlier.shape = NA) + 
  #         scale_fill_manual(values = "brown", labels="Average all Soils")+ # labels=Txt_translation$Average_Soil[Txt_translation$CSS==css]
  #         scale_x_discrete( labels = c( "arable_crops" = "Arable", "greenhouse_vegetable"="Greenhouse vegetables", "mixed" = "Mixed rotations", "orchards"= "Orchards", "other_vegetable" = "Other Vegetables" ) ) +
  #         # # Add a min max line,
  #         # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
  #         # Add text when y max is reached
  #         geom_text(data=subset(df_plot_dot_css, CropDominant4 %!in% c("na", "fallow")), aes(x=CropDominant4, y=y_max_mm2*201.9, label=Trunc_mm2 )) +
  #         
  #         # Add the x-axis line
  #         # geom_segment(aes(x = 0.4, xend = nrow(Summary9d_Crop)-0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  #         
  #         # Add the y-axis 
  #         geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max_mm2*200, yend = 0), color = "black", size = 1) +
  #         
  #         # Add jitter
  #         #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  #         geom_point(data= subset(Summary4e_Soil, CropDominant4 %!in% c("na", "fallow") ), aes(x=CropDominant4, y=Mean.Tot.Area.mm2.S*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
  #         scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
  #         
  #         ylim(0,700)+
  #         # Titles
  #         ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  #         
  #         
  #         # custom y scale 
  #         scale_y_continuous(breaks = seq(0, y_max_mm2*200, 100), # Have a break for each gap
  #                            labels = label_at(200), #,                                        # label every second break
  #                            limits = c(-y_max_mm2*200/40, y_max_mm2*202))+ #limits goes beyond to allow the vertical ticks between farms
  #         
  #         theme_minimal()+
  #         #guides( color  = "none")+
  #         labs(y = expression("Average area of MiP [mm" ^ 2*"/kg soil]"),
  #              #fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
  #              color=element_blank() ) +
  #         theme(#axis.text.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.ticks.y = element_line(color = "black"),
  #           axis.line.y = element_blank(),
  #           legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
  #           panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
  #       
  #       # Add the axis.text.x at the right position
  #       #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  #       # Clip the graph to the right size
  #       #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  #       
  #       
  #       print( PLOT)
  #       
  #       
  #       #} # end Translation for each css, 4.6
  #       ggsave(filename = paste(wd.out,"/4.6.Box_Mean_MiP_SumPolymer_Crop_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
  #       
  #       
  #       
  #       
  #       # *** Box plot Area #####
  #       
  #       PLOT= ggplot() +
  #         # Stack bars with Polymer 12 
  #         geom_boxplot(data=Summary4e_Soil, aes(x=CropDominant4, y=Mean.Tot.Area.mm2.S*200, fill="Average all Soils"), outlier.shape = NA) + 
  #         scale_fill_manual(values = "brown", labels="Average all Soils")+ # labels=Txt_translation$Average_Soil[Txt_translation$CSS==css]
  #         
  #         # # Add a min max line,
  #         # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
  #         # Add text when y max is reached
  #         geom_text(data=df_plot_dot_css, aes(x=CropDominant4, y=y_max_mm2*(194+7.9*(1 %% 2)), label=Trunc_mm2 )) +
  #         
  #         # Add the x-axis line
  #         geom_segment(aes(x = 0.4, xend = nrow(Summary9d_Crop)-0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  #         
  #         # Add the y-axis 
  #         geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max_mm2*200, yend = 0), color = "black", size = 1) +
  #         
  #         # Add jitter
  #         #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  #         geom_point(data= Summary4e_Soil, aes(x=CropDominant4, y=Mean.Tot.Area.mm2.S*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
  #         scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
  #         
  #         ylim(0,700)+
  #         # Titles
  #         ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  #         
  #         
  #         # custom y scale 
  #         scale_y_continuous(breaks = seq(0, y_max_mm2*200, 100), # Have a break for each gap
  #                            labels = label_at(200), #,                                        # label every second break
  #                            limits = c(-y_max_mm2*200/40, y_max_mm2*202))+ #limits goes beyond to allow the vertical ticks between farms
  #         
  #         theme_minimal()+
  #         #guides( color  = "none")+
  #         labs(y = expression("Average surface of plastic per kg of soil  [mm" ^ 2*"]"),
  #              #fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
  #              color=element_blank() ) +
  #         theme(#axis.text.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.ticks.y = element_line(color = "black"),
  #           axis.line.y = element_blank(),
  #           legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
  #           panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
  #       
  #       # Add the axis.text.x at the right position
  #       #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  #       # Clip the graph to the right size
  #       #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  #       
  #       
  #       print( PLOT)
  #       
  #       
  #       #} # end Translation for each css, 4.6
  #       ggsave(filename = paste(wd.out,"/4.6.Box_MeanArea_MiP_SumPolymer_Crop_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
  #       
  #       
  #       # *** Box plot Mean Area #####
  #       
  #       PLOT= ggplot() +
  #         # Stack bars with Polymer 12 
  #         geom_boxplot(data=Summary4e_Soil, aes(x=CropDominant4, y=sqrt(Mean.Tot.Area.mm2/Mean.particles)*1000, fill="Average all Soils"), outlier.shape = NA) + 
  #         scale_fill_manual(values = "brown", labels="Average all Soils")+ # labels=Txt_translation$Average_Soil[Txt_translation$CSS==css]
  #         
  #         # # Add a min max line,
  #         # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
  #         # Add text when y max is reached
  #         #geom_text(data=df_plot_dot_css, aes(x=CropDominant4, y=y_max_mm2/y_max, label=Trunc_mm2 )) +
  #         
  #         # Add the x-axis line
  #         geom_segment(aes(x = 0.4, xend = nrow(Summary9d_Crop)-0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  #         
  #         # Add the y-axis 
  #         geom_segment(aes(x = 0.4, xend = 0.4, y = 1000, yend = 0), color = "black", size = 1) +
  #         
  #         # Add jitter
  #         #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  #         geom_point(data= Summary4e_Soil, aes(x=CropDominant4, y=sqrt(Mean.Tot.Area.mm2/Mean.particles)*1000, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
  #         scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
  #         
  #         ylim(0,1000)+
  #         # Titles
  #         ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  #         
  #         
  #         # custom y scale 
  #         scale_y_continuous(breaks = seq(0, 1000, 100), # Have a break for each gap
  #                            labels = label_at(200), #,                                        # label every second break
  #                            limits = c(-y_max_mm2*200/40, 1000))+ #limits goes beyond to allow the vertical ticks between farms
  #         
  #         theme_minimal()+
  #         #guides( color  = "none")+
  #         labs(y = expression("Average particle size  [um" ^ 2*"]"),
  #              #fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
  #              color=element_blank() ) +
  #         theme(#axis.text.x = element_blank(),
  #           axis.title.x = element_blank(),
  #           axis.ticks.y = element_line(color = "black"),
  #           axis.line.y = element_blank(),
  #           legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
  #           panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
  #       
  #       # Add the axis.text.x at the right position
  #       #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  #       # Clip the graph to the right size
  #       #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  #       
  #       
  #       print( PLOT)
  #       
  #       
  #       #} # end Translation for each css, 4.6
  #       ggsave(filename = paste(wd.out,"/4.6.Box_MeanArea_MiP_SumPolymer_Crop_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
  #       
  #   
  #     # * 4.7. Per cropping system, Per Polymers (CSS translation?) #####   
  #     # -> Average (and median), no dot, no min-max, no max
  #     
  #     # Data frame for the bars 
  #     df_plot_bar_css=subset( Summary9c_Crop_outlier ,  Preparation_Type=="Field_samples" )
  #     
  #     # Data frame for the min-max 
  #     df_plot_dot_css=subset(Summary9e_Crop,  Preparation_Type=="Field_samples")
  #     
  #     # Truncation over 30 MiP/ sample <-> 6 000 MiP/ kg soil # /!\ Ad Hoc
  #     y_max=30
  #     # text if the max is met
  #     df_plot_dot_css$Trunc=NA
  #     df_plot_dot_css$Trunc[ df_plot_dot_css$Max.particles.Crop> y_max]= paste( "max=",
  #                                                                              df_plot_dot_css$Max.particles.Crop[ df_plot_dot_css$Max.particles.Crop> y_max] *200,
  #                                                                              sep = "" )
  #     # Cut at y_max
  #     df_plot_dot_css$Max.particles.Crop[ df_plot_dot_css$Max.particles.Crop> y_max]= y_max
  #     
  #     # # Add "CSS"abreviation in x-axis   
  #     # df_plot_bar_css$CSS=factor(paste("CSS", df_plot_bar_css$CSS),
  #     #                            levels =  c("CSS 1", "CSS 2", "CSS 3", "CSS 4", "CSS 5", "CSS 6",
  #     #                                        "CSS 7", "CSS 8", "CSS 9", "CSS 10", "CSS 11") )
  #     
  #     # Translation for each css: 
  #    # for (css in 1:11){  
  #     css=3
  #       # *** MEAN #####
  #       PLOT= ggplot( df_plot_bar_css) +
  #         # Stack bars with Polymer 12 
  #         geom_bar( aes(x= CropDominant4, y=Mean.particles.Crop*200, fill=Polymer.red12), position="stack", stat="identity") + 
  #         # Custom color palette
  #         scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
  #                                      "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
  #                                      "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
  #                                      "PMMA"="#a1d99b",   "PC"="#FFF8DC",
  #                                      "CA"= "#FFD39B") , 
  #                           # Relabel  "Other.Plastic"                 
  #                           labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css] ) ) +
  #         # # Add box around the bars: transparent for all fields, black for the MINAGRIS MEAN
  #         # geom_bar( aes(x=CSS, y=Mean.particles.CSS*200, group=CSS, color = CSS), position="stack", stat="summary", fun=sum, fill = "transparent",
  #         #           size = 1.5) +
  #         # scale_color_manual(values = c("CSS 1"="NA", "CSS 2"="NA", "CSS 3"="NA", "CSS 4"="NA", "CSS 5"="NA", "CSS 6"="NA", 
  #         #                               "CSS 7"="NA", "CSS 8"="NA", "CSS 9"="NA", "CSS 10"="NA", "CSS 11"="NA", "CSS 12"="NA", "MEAN"="Black" )) +
  #         # # Add a min max line,   
  #         # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
  #         # # Add text when y max is reached
  #         # geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max*201.9, label=Trunc )) +
  #         # # Add jitter
  #         # geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  #         
  #         # Titles
  #         ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  #         
  #         # Add the x-axis line
  #         geom_segment(aes(x = 0.4, xend = nrow(Summary9d_Crop)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  #         
  #         # Add the y-axis 
  #         geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200/2, yend = 0), color = "black", size = 1) +
  #         
  #         # # custom y scale 
  #         # scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap # y_max*200/8
  #         #                    labels = label_at(1000), #,                                        # label every second break
  #         #                    limits = c(-y_max*200/40, y_max*202/2))+ #limits goes beyond to allow the vertical ticks between farms
  #         
  #         # Theme 
  #         theme_minimal()+
  #         guides( color  = "none")+
  #         labs(y = Txt_translation$y_nMiP_av[Txt_translation$CSS==css],
  #              fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css]) +
  #         theme(#axis.text.x = element_blank(),
  #               axis.title.x = element_blank(),
  #               axis.ticks.y = element_line(color = "black"),
  #               axis.line.y = element_blank(),
  #               panel.grid.minor.y = element_blank())+
  #         
  #         # Add the axis.text.x at the right position
  #         #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  #         # Clip the graph to the right size
  #         coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  #       
  #       
  #       print( PLOT)
  #       ggsave(filename = paste(wd.out,"/4.7.Bar_Mean_MiP_Polymer12_Crop_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)
  #       
  #     
  #  
  # 
# 5. Pie. Polymer composition ####
  # * All CSS ####
  
  # Order plot per polymer   
  Summary8c_MINAGRIS$Polymer.red12 = factor(Summary8c_MINAGRIS$Polymer.red12,
                                            levels = Summary8c_MINAGRIS$Polymer.red12[order(-Summary8c_MINAGRIS$Mean.particles.MM)] )
  
  # Add percentages in the plot
  Summary8c_MINAGRIS$MiP_perc <- Summary8c_MINAGRIS$Mean.particles.MM / sum(Summary8c_MINAGRIS$Mean.particles.MM) * 100
  # Creat percentage as text 
  Summary8c_MINAGRIS$MiP_perc_text=paste0(round( Summary8c_MINAGRIS$MiP_perc, 0), "%")
  
  # Do not show percentages < 3.5% 
  Summary8c_MINAGRIS$MiP_perc_text [Summary8c_MINAGRIS$MiP_perc < 3.5]=NA
  
  
  # Translation for each css: 
  for (css in 1:11){  
  
  PLOT= ggplot(Summary8c_MINAGRIS, aes(x="", y=Mean.particles.MM, fill= Polymer.red12 ))+
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
  A=subset(Summary8a_MINAGRIS, Polymer.red12=="Other.Plastic")
  ggplot(data=A, aes(x=Polymer.grp, y=Mean.particles.MM, fill=Size_cat.um))+
    geom_bar(stat="identity", position = "stack", width=1, color="white") 
  
  
  # * Per CSS ####  
  for (css in 1:11){ 
    df_plot_pie_css=subset(Summary7c_CSS, CSS %in% c(css,"MEAN") )
    
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
  
  

PLOT=ggplot(Summary8c_MINAGRIS, aes(x=Polymer.red12, y=Mean.particles.MM*200, fill= Polymer.red12 ))+
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
  geom_text(aes(label=Polymer.red12) ,                     vjust =0.5, hjust = 0.5, nudge_x = 0,  nudge_y = 20) +
  geom_text(aes(label = paste0(round(MiP_perc, 1), "%")),  vjust =0.5, hjust = 0.5, nudge_x =0 ,  nudge_y = -20)+
  #ggtitle(paste("Field Samples ; CSS ", css))+
  # guides( color  = "none")+
  labs( y = "Average number of plastic particles per kg of soil",
    fill = "Polymers identified") +
theme(axis.text.x = element_blank(),
      axis.title.x = element_blank())
  PLOT
  dev.size(units = "in") # 15 x 9 cm 
  ##  dev.size(units = "cm") gives the size of ggplot actual zoom

  ggsave(filename = paste(wd.out,"/6.1.Bar_MiP_composition_Polymer12_AllCSS.png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
  


  
  # * Per CSS ####
  
  # * Par Lab ####
 
  # Order plot per polymer   
  Summary10c_Lab$Polymer.red12 = factor(Summary10c_Lab$Polymer.red12,
                                            levels = Summary10c_Lab$Polymer.red12[order(-Summary8c_MINAGRIS$Mean.particles.MM)] )
  
  # Add percentages in the plot
  Summary10c_Lab$MiP_perc=0
  Summary10c_Lab$MiP_perc[Summary10c_Lab$Lab=="WUR"] <- Summary10c_Lab$Mean.particles.MM[Summary10c_Lab$Lab=="WUR"]  / sum(Summary10c_Lab$Mean.particles.MM[Summary10c_Lab$Lab=="WUR"] ) * 100
  Summary10c_Lab$MiP_perc[Summary10c_Lab$Lab=="Ubern"] <- Summary10c_Lab$Mean.particles.MM[Summary10c_Lab$Lab=="Ubern"]  / sum(Summary10c_Lab$Mean.particles.MM[Summary10c_Lab$Lab=="Ubern"] ) * 100
  
  
   # Creat percentage as text 
  Summary10c_Lab$MiP_perc_text=paste0(round( Summary10c_Lab$MiP_perc, 0), "%")
  
  # Do not show percentages < 3.5% 
  Summary10c_Lab$MiP_perc_text [Summary10c_Lab$MiP_perc < 3.5]=NA
  
  
  # Translation for each css: 

    
    PLOT= ggplot(Summary10c_Lab, aes(x=Polymer.red12, y=Mean.particles.MM*200, fill= Polymer.red12 ))+
      facet_wrap(~Lab, scales = "fixed", ncol =1)+
      geom_bar(stat="identity", width=1, color="white") +
      #coord_polar("y", start=0) +
      scale_fill_manual(values = c("PE"="#377EB8",  "Other.Plastic"="#E41A1C", "PU"="#F781BF",
                                   "PP"="#FF7F00",  "PLA"="#A65628",           "PS"="#999999",
                                   "PET"="#FFD700", "PVC"="#4DAF4A",           "PA"="#984EA3",
                                   "PMMA"="#a1d99b",   "PC"="#FFF8DC",
                                   "CA"= "#FFD39B") , 
                        # Relabel  "Other.Plastic"                 
                        labels = c( "Other.Plastic"=Txt_translation$Other_Plastic[Txt_translation$CSS==css] ) ) +
      # geom_text(aes(x=Polymer.red12, label = MiP_perc_text ), 
      #           position = position_stack(vjust = 1.1), size=5  )+
      #coord_polar("y", start=0) +
      theme_minimal() + 
      geom_text(aes(label=Polymer.red12) ,                     vjust = -0.5, hjust = 0.5, nudge_x = 0,  nudge_y = 0.2) +
      geom_text(aes(label = paste0(round(MiP_perc, 1), "%")),  vjust = 1,    hjust = 0.5, nudge_x =0 ,  nudge_y = -0.2)+
      ggtitle(Txt_translation$Pie_Title_All[Txt_translation$CSS==css] )+
      # guides( color  = "none")+
      labs( y = "Average number of MiP per Kg soil",
        fill = "Polymer identified") #+
    #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
    #   axis.title.x = element_blank() )
    
    dev.size(units = "in") # 15 x 9 cm 
    ##  dev.size(units = "cm") gives the size of ggplot actual zoom
    
    PLOT
    ggsave(filename = paste(wd.out,"/6.3.Bar_MiP_composition_Polymer12_AllCSS_PerLab.png",sep =""), plot = PLOT, width = 10.5, height = 8.5, units = "in", dpi = 300)
    
  
  
  # * details of "other polymers" #####
  A=subset(Summary8a_MINAGRIS, Polymer.red12=="Other.Plastic")
  ggplot(data=A, aes(x=Polymer.grp, y=Mean.particles.MM, fill=Size_cat.um))+
    geom_bar(stat="identity", position = "stack", width=1, color="white") 
  
  
  
# 7. Dots. S1-S2 per Field ####
  # = min-max with lab color and range bar
  
  # * Number Per CSS ####
  Summary5e_Field$Farm.Field=paste(Summary5e_Field$Farm, Summary5e_Field$Field, sep=".")
  Summary4e_Soil$Farm.Field=paste( Summary4e_Soil$Farm, Summary4e_Soil$Field, sep=".")
  
  for (css in 1:11){
  # P1: Plot vertical bar min-max, from Summary5e_Field
    df_p1=subset(Summary5e_Field, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
    df_p1$Farm.Field= factor( df_p1$Farm.Field, 
                              levels= c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2", "5.1", "5.2", "6.1", "6.2",
                                        "7.1", "7.2", "8.1","8.2", "9.1", "9.2", "10.1", "10.2", "11.1", "11.2", "12.1", "12.2","12.3","12.4", "MEAN"))
    
    
    
    
    
    p1= ggplot()+
      geom_linerange(data=df_p1, aes(x=Farm.Field, y=Mean.particles.F*200, ymin = Min.particles.F*200, ymax = Max.particles.F*200))+
      ggtitle(paste("Field Samples ; CSS ", css))+
      theme_minimal()
    
   #  P2: Plot Samples S1-S2, min-max with lab color, from Summary4e_Soil
     df_p2=subset(Summary4e_Soil, CSS %in% c(css,"MEAN")  & Preparation_Type=="Field_samples")
     df_p2$Farm.Field= factor( df_p2$Farm.Field, 
                         levels= c("1.1", "1.2", "2.1", "2.2", "3.1", "3.2", "4.1", "4.2", "5.1", "5.2", "6.1", "6.2",
                                   "7.1", "7.2", "8.1","8.2", "9.1", "9.2", "10.1", "10.2", "11.1", "11.2", "12.1", "12.2","12.3","12.4", "MEAN"))
        
     p2= ggplot()+
        geom_point(data = df_p2, aes(x=Farm.Field, y=Mean.particles.S*200, color=Lab, shape=Lab, size=Lab ))+
        scale_size_manual(values = c("WUR"=2,  "Ubern"=3))+
        scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
        ggtitle(paste("Field Samples ; CSS ", css))+
        theme_minimal()
     p2
     
  
     PLOT=  p1+
    geom_point(data = df_p2, aes(x=Farm.Field, y=Mean.particles.S*200, color=Lab, shape=Lab, size=Lab ))+
    scale_size_manual(values = c("WUR"=2,  "Ubern"=3))+
    scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
    ggtitle(paste("Field Samples ; CSS ", css))+
       labs(y = "Average number of MiP per kg soil") +
    theme_minimal()+
       theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
             axis.title.x = element_blank())
     
    print( PLOT)
     
     # ggsave(filename = paste(wd.out,"/CSS",css,"/7.Dot_MiP_number_CSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
      ggsave(filename = paste(wd.out,"/7.Dot_MiP_Number_CSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
      
      
    # * Area Per CSS #### 
      PLOT=  
        ggplot()+
        geom_linerange(data=df_p1, aes(x=Farm.Field, y=Mean.Tot.Area.mm2.F, ymin = Min.Tot.Area.mm2.F, ymax = Max.Tot.Area.mm2.F))+
        geom_point(data = df_p2, aes(x=Farm.Field, y=Mean.Tot.Area.mm2, color=Lab, shape=Lab, size=Lab ))+
        scale_size_manual(values = c("WUR"=2,  "Ubern"=3))+
        scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
        ggtitle(paste("Field Samples ; CSS ", css))+
        labs(y = expression("Average surface of plastic per soil sample [mm" ^ 2*"]")) +
        theme_minimal()+
        theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
              axis.title.x = element_blank())
      
      print( PLOT)
      
      # ggsave(filename = paste(wd.out,"/CSS",css,"/7.Dot_MiP_number_CSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
      ggsave(filename = paste(wd.out,"/7.Dot_MiP_Area_CSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
      
  } #end for css  
  
  # Area per CSS****
  
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
    
    # ggsave(filename = paste(wd.out,"/CSS",css,"/7.Dot_MiP_number_CSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
    ggsave(filename = paste(wd.out,"/7.Dot_MiP_number_CSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 10.5, height = 4, units = "in", dpi = 300)
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


# 9. BoxPlot  ####

# # *** Box plot number, per crop #####
# df_plot_box= subset(Summary4e_Soil, CropDominant4 != "na")
# 
# max(df_plot_box$Mean.particles.S[df_plot_box$Lab=="Ubern"])*200
# # Add "Crop"abreviation in x-axis   
# 
# 
# PLOT= ggplot() +
#   # Stack bars with Polymer 12 
#   geom_boxplot(data=df_plot_box, aes(x=Lab, y=Mean.particles.S*200, fill="Average all Soils"), outlier.shape = NA) + 
#   scale_fill_manual(values = "brown", labels="Average all Soils")+ # labels=Txt_translation$Average_Soil[Txt_translation$CSS==css]
#   #scale_x_discrete( labels = c( "arable_crops" = "Arable", "fallow"= "Fallow", "greenhouse_vegetable"="Greenhouse vegetables", "mixed" = "Mixed rotations", "orchards"= "Orchards", "other_vegetable" = "Other Vegetables" ) ) +
#   # # Add a min max line,
#   # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
#   # Add text when y max is reached
#   geom_text(data=subset(df_plot_dot_css, CropDominant4 != "na"), aes(x=CropDominant4, y=y_max*201.9, label=Trunc )) +
#   
#   # Add the x-axis line
#   geom_segment(aes(x = 0.4, xend = nrow(Summary9d_Crop)-0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
#   
#   # Add the y-axis 
#   geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
#   
#   # Add jitter
#   #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
#   geom_point(data= subset(Summary4e_Soil, CropDominant4 != "na"), aes(x=CropDominant4, y=Mean.particles.S*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
#   scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
#   
#   ylim(0,10500)+
#   # Titles
#   ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
#   
#   
#   # custom y scale 
#   scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
#                      labels = label_at(1000), #,                                        # label every second break
#                      limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
#   
#   theme_minimal()+
#   #guides( color  = "none")+
#   labs(y = "Average number MiP per kg of soil",
#        #fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
#        color=element_blank() ) +
#   theme(#axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.ticks.y = element_line(color = "black"),
#     axis.line.y = element_blank(),
#     legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
#     panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
# 
# # Add the axis.text.x at the right position
# #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
# # Clip the graph to the right size
# #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
# 
# 
# print( PLOT)
# 
# 
# #} # end Translation for each css, 4.6
# ggsave(filename = paste(wd.out,"/4.6.Box_Mean_MiP_SumPolymer_Crop_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)



# * 9.1. Per lab, Soils ####
# -> Average (and median), jitter dots, no min-max, max out of bound as label


# Initialise Plot
# Data frame for the bars 
df_plot_box=  subset(Summary4e_Soil,   Preparation_Type=="Field_samples")

nrow(df_plot_box)
nrow(df_plot_box[df_plot_box$Mean.particles.S>3,])/nrow(df_plot_box)
median(df_plot_box$Mean.particles.S)

# Truncation over 50 MiP/ sample <-> 10 000 MiP/ kg soil
  df_plot_box_lab=subset(Summary10e_Lab,   Preparation_Type=="Field_samples")

y_max=50
y_max_mm2=3.5
# text if the max is met
df_plot_box_lab$Trunc=NA
df_plot_box_lab$Trunc[ df_plot_box_lab$Max.particles.MM> y_max]= paste( "max=",
                                                                        round(df_plot_box_lab$Max.particles.MM[ df_plot_box_lab$Max.particles.MM> y_max] *200,digits = 0),
                                                                         sep = "" )
df_plot_box_lab$Trunc_mm2=NA
df_plot_box_lab$Trunc_mm2[ df_plot_box_lab$Max.Tot.Area.mm2.MM> y_max_mm2]= paste( "max=",
                                                                                    round(df_plot_box_lab$Max.Tot.Area.mm2.MM[ df_plot_box_lab$Max.Tot.Area.mm2.MM> y_max_mm2] *200,digits = 0),
                                                                                    " mm2", sep = ""  )
# Cut at y_max
df_plot_box$Max.particles.S[ df_plot_box$Max.particles.S> y_max]= y_max
df_plot_box$Max.Tot.Area.mm2.S[ df_plot_box$Max.Tot.Area.mm2.S> y_max_mm2]= y_max_mm2


# Translation for each css: 
  # *** MEAN #####
  PLOT= ggplot() +
    # Stack bars with Polymer 12 
    geom_boxplot(data=df_plot_box, aes(x=Lab, y=Mean.particles.S*200), outlier.shape = NA) + 

    # # Add a min max line,
    # geom_linerange(data=df_plot_box, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
    
  # Add text when y max is reached
   geom_text(data=df_plot_box_lab, aes(x=Lab, y=y_max*201.9, label=Trunc ) )+
    
    # Add the x-axis line
    # geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
    
    # Add the y-axis 
   # geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
    
    # Add jitter
    #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
    geom_point(data= Summary4e_Soil, aes(x=Lab, y=Mean.particles.S*200, color=Lab, shape=Lab), alpha = 0.25, position = position_jitter(seed=43^3))+
    
  scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+

  stat_summary(fun.y=mean, geom="point", shape=3, size=9, color="blue", fill="blue") +
   ylim(0,10500)+
    # Titles
    ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==3])+
    
    
    # custom y scale 
    # scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
    #                    labels = label_at(1000), #,                                        # label every second break
    #                    limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
    # 
    
    theme_minimal()+
    #guides( color  = "none")+
    labs(y = "Average number of Mip per Kg of Soil"      ) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_line(color = "black"),
          axis.line.y = element_blank(),
          legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
          panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
    
    # Add the axis.text.x at the right position
    #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
    # Clip the graph to the right size
    #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  
  
  print( PLOT)
  ggsave(filename = paste(wd.out,"/9.1.Box_MeanNumber_MiP_SumPolymer_Lab.png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
  
  
  # *** Mean Area #####
  
  PLOT= ggplot() +
    # Stack bars with Polymer 12 
    geom_boxplot(data=df_plot_box, aes(x=Lab, y=Mean.Tot.Area.mm2.S*200), outlier.shape = NA) + 
    
    # # Add a min max line,
    # geom_linerange(data=df_plot_box, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
    
    # Add text when y max is reached
    geom_text(data=df_plot_box_lab, aes(x=Lab, y=y_max_mm2*201.9, label=Trunc_mm2 ) )+
    
    # Add the x-axis line
    # geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
    
    # Add the y-axis 
    # geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
    
    # Add jitter
    #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
    geom_point(data= Summary4e_Soil, aes(x=Lab, y=Mean.Tot.Area.mm2.S*200, color=Lab, shape=Lab), alpha = 0.25, position = position_jitter(seed=43^3))+
    
    scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
    
    stat_summary(fun.y=mean, geom="point", shape=3, size=9, color="blue", fill="blue") +
    ylim(0,y_max_mm2*202)+
    # Titles
    ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==3])+
    
    
    # custom y scale 
    # scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
    #                    labels = label_at(1000), #,                                        # label every second break
    #                    limits = c(-y_max*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
    # 
    
    theme_minimal()+
    #guides( color  = "none")+
    labs(y =expression("Average surface of plastic per soil sample [mm" ^ 2*"]")) +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.y = element_line(color = "black"),
          axis.line.y = element_blank(),
          legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
          panel.grid.minor.y = element_blank())#+ # Remove the minor grid 
  
  # Add the axis.text.x at the right position
  #annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  # Clip the graph to the right size
  #coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")
  
  
  print( PLOT)
  ggsave(filename = paste(wd.out,"/9.1.Box_MeanArea_MiP_SumPolymer_Lab.png",sep =""), plot = PLOT, width = 5.5, height = 4, units = "in", dpi = 300)
  



# * 9.2. All project, Fields ####
# -> Average (and median), jitter dots, no min-max, max out of bound as label


# Initialise Plot
# Data frame for the bars 
df_plot_box=  subset(Summary5e_Field,   Preparation_Type=="Field_samples")
# Check big particles 
sort(df_plot_box$Mean.particles.Fd[df_plot_box$Mean.particles.Fd>3000/200]*200)


nrow(df_plot_box)
nrow(df_plot_box[df_plot_box$Mean.particles.Fd>3,])/nrow(df_plot_box)
median(df_plot_box$Mean.particles.Fd)

# Truncation over 50 MiP/ sample <-> 10 000 MiP/ kg soil
y_max=25
y_max_mm2=3.5
# text if the max is met
df_plot_box$Trunc=NA
  df_plot_box$Trunc[ df_plot_box$Max.particles.Fd == max(df_plot_box$Max.particles.Fd)]= paste( "max=",
                                                  round(df_plot_box$Mean.particles.Fd[ df_plot_box$Max.particles.Fd  == max(df_plot_box$Max.particles.Fd)] *200,digits = 0),
                                                               sep = "" )
df_plot_box$Trunc_mm2=NA
df_plot_box$Trunc_mm2[ df_plot_box$Max.Tot.Area.mm2.Fd== max(df_plot_box$Max.Tot.Area.mm2.Fd)]= paste( "max=",
                                                             round(df_plot_box$Mean.Tot.Area.mm2.Fd[ df_plot_box$Max.Tot.Area.mm2.Fd== max(df_plot_box$Max.Tot.Area.mm2.Fd)] *200,digits = 0),
                                                                          " mm2", sep = ""  )
# Cut at y_max
# df_plot_box$Max.particles.Fd[ df_plot_box$Max.particles.S> y_max]= y_max
# df_plot_box$Max.Tot.Area.mm2.Fd[ df_plot_box$Max.Tot.Area.mm2.S> y_max_mm2]= y_max_mm2


# Translation for each css: 
# *** MEAN #####
PLOT= ggplot() +
  # Stack bars with Polymer 12 
  geom_boxplot(data=df_plot_box, aes(x=Preparation_Type, y=pmax(Mean.particles.Fd,0)*200), outlier.shape = NA, fill="lightsteelblue") + #  "steelblue4"
  
  # Add a min max line,
  #geom_linerange(data=df_plot_box, aes(x=Preparation_Type, y=Mean.particles.Fd*200, ymin = Min.particles.S*200, ymax = Max.particles.S*200)) +
  #Add text when y max is reached
  geom_text(data=df_plot_box, aes(x=Preparation_Type, y=y_max*201.9, label=Trunc )) +
  
  # Add the x-axis line
  # geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  
  # Add the y-axis 
  # geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
  
  # Add jitter
  #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  geom_point(data= Summary5e_Field, aes(x=Preparation_Type, y=pmax(Mean.particles.Fd,0)*200),  alpha = 0.25, position = position_jitter(seed=43^3))+
  
  #scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
  
  stat_summary(fun.y=mean, geom="point", shape=3, size=9, color="blue", fill="blue") +
  # ylim(0,10500)+
  # Titles
  ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==3])+
  
  
  # custom y scale 
  scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
                     labels = label_at(1000), #,                                        # label every second break
                     limits = c(-y_max*200/40, y_max*202+20))+ #limits goes beyond to allow the vertical ticks between farms

  
  theme_minimal()+
  #guides( color  = "none")+
  labs(y = "Average number of Mip per Kg of Soil"      ) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "black"),
        axis.line.y = element_blank(),
        legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
        panel.grid.minor.y = element_blank())#+ # Remove the minor grid 

# Add the axis.text.x at the right position
#annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
# Clip the graph to the right size
#coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")


print( PLOT)
ggsave(filename = paste(wd.out,"/9.2.Box_Mean_MiP_AllPolymer_AllCSS","_",Date, ".png",sep =""), plot = PLOT, width = 4, height = 4, units = "in", dpi = 300)


# *** Mean Area #####

PLOT= ggplot() +
  # Stack bars with Polymer 12 
  geom_boxplot(data=df_plot_box, aes(x=Preparation_Type, y=pmax(Mean.Tot.Area.mm2.Fd,0)*200), outlier.shape = NA, fill="lightsteelblue") + #  "steelblue4"
  
  # Add a min max line,
  #geom_linerange(data=df_plot_box, aes(x=Preparation_Type, y=Mean.particles.Fd*200, ymin = Min.particles.S*200, ymax = Max.particles.S*200)) +
  #Add text when y max is reached
  geom_text(data=df_plot_box, aes(x=Preparation_Type, y=y_max_mm2*201.9, label=Trunc_mm2 )) +
  
  # Add the x-axis line
  # geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  
  # Add the y-axis 
  # geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
  
  # Add jitter
  #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  geom_point(data= Summary5e_Field, aes(x=Preparation_Type, y=pmax(Mean.Tot.Area.mm2.Fd,0)*200),  alpha = 0.25, position = position_jitter(seed=43^3))+
  
  #scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
  
  stat_summary(fun.y=mean, geom="point", shape=3, size=9, color="blue", fill="blue") +
  ylim(0,y_max_mm2*202)+
  # Titles
  ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==3])+
  
  
  # # custom y scale 
  # scale_y_continuous(breaks = seq(0, y_max_mm2*200, 500), # Have a break for each gap
  #                    labels = label_at(1000), #,                                        # label every second break
  #                    limits = c(-y_max_mm2*200/40, y_max*202))+ #limits goes beyond to allow the vertical ticks between farms
  # 
  # 
  theme_minimal()+
  #guides( color  = "none")+
  labs(y = expression("Average surface of plastic per soil sample [mm" ^ 2*"]")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "black"),
        axis.line.y = element_blank(),
        legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
        panel.grid.minor.y = element_blank())#+ # Remove the minor grid 

# Add the axis.text.x at the right position
#annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
# Clip the graph to the right size
#coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")


print( PLOT)
ggsave(filename = paste(wd.out,"/9.2.Box_MeanArea_MiP_AllPolymer_AllCSS","_",Date, ".png",sep =""), plot = PLOT, width = 4, height = 4, units = "in", dpi = 300)




# * 9.3. All project, FSoil ####
# -> Average (and median), jitter dots, no min-max, max out of bound as label


# Initialise Plot
# Data frame for the bars 
df_plot_box=  subset(Summary4e_Soil,   Preparation_Type=="Field_samples")
sort(df_plot_box$Mean.particles.S[df_plot_box$Mean.particles.S>3000/200]*200)
sort(df_plot_box$Mean.particles.S[df_plot_box$Mean.particles.S>3000/200]*200)

nrow(df_plot_box)
nrow(df_plot_box[df_plot_box$Mean.particles.S>3,])/nrow(df_plot_box)
median(df_plot_box$Mean.particles.S)

# Truncation over 50 MiP/ sample <-> 10 000 MiP/ kg soil
y_max=25
y_max_mm2=3.5
# text if the max is met
df_plot_box$Trunc=NA
df_plot_box$Trunc[ df_plot_box$Max.particles.S == max(df_plot_box$Max.particles.S)]= paste( "max=",
                                                                                            df_plot_box$Max.particles.S[ df_plot_box$Max.particles.S  == max(df_plot_box$Max.particles.S)] *200,
                                                                                            sep = "" )
df_plot_box$Trunc_mm2=NA
df_plot_box$Trunc_mm2[ df_plot_box$Max.Tot.Area.mm2.S== max(df_plot_box$Max.Tot.Area.mm2.S)]= paste( "max=",
                                                                                                     round(df_plot_box$Max.Tot.Area.mm2.S[ df_plot_box$Max.Tot.Area.mm2.S== max(df_plot_box$Max.Tot.Area.mm2.S)] *200,digits = 0),
                                                                                                     " mm2", sep = ""  )
# Cut at y_max
df_plot_box$Max.particles.S[ df_plot_box$Max.particles.S> y_max]= y_max
df_plot_box$Max.Tot.Area.mm2.S[ df_plot_box$Max.Tot.Area.mm2.S> y_max_mm2]= y_max_mm2


# Translation for each css: 
# *** MEAN #####
PLOT= ggplot() +
  # Stack bars with Polymer 12 
  geom_boxplot(data=df_plot_box, aes(x=Preparation_Type, y=Mean.particles.S*200), outlier.shape = NA, fill="lightsteelblue") + #  "steelblue4"
  
  # Add a min max line,
  geom_linerange(data=df_plot_box, aes(x=Preparation_Type, y=Mean.particles.S*200, ymin = Min.particles.S*200, ymax = Max.particles.S*200)) +
  #Add text when y max is reached
  geom_text(data=df_plot_box, aes(x=Preparation_Type, y=y_max*201.9, label=Trunc )) +
  
  # Add the x-axis line
  # geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  
  # Add the y-axis 
  # geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max*200, yend = 0), color = "black", size = 1) +
  
  # Add jitter
  #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  geom_point(data= Summary4e_Soil, aes(x=Preparation_Type, y=Mean.particles.S*200),  alpha = 0.25, position = position_jitter(seed=43^3))+
  
  #scale_color_manual(values = c("WUR"="dark green",  "Ubern"="red"))+
  
  stat_summary(fun.y=mean, geom="point", shape=3, size=9, color="blue", fill="blue") +
  # ylim(0,10500)+
  # Titles
  ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  
  
  # custom y scale 
  scale_y_continuous(breaks = seq(0, y_max*200, 500), # Have a break for each gap
                     labels = label_at(1000), #,                                        # label every second break
                     limits = c(-y_max*200/40, y_max*202+20))+ #limits goes beyond to allow the vertical ticks between farms
  
  
  theme_minimal()+
  #guides( color  = "none")+
  labs(y = "Average number of Mip per Kg of Soil"      ) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "black"),
        axis.line.y = element_blank(),
        legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
        panel.grid.minor.y = element_blank())#+ # Remove the minor grid 

# Add the axis.text.x at the right position
#annotate("text", x = df_plot_dot_css$CSS, y = -y_max*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
# Clip the graph to the right size
#coord_cartesian(ylim = c(-y_max*200/40, NA), clip = "off")


print( PLOT)
ggsave(filename = paste(wd.out,"/9.2.Box_Mean_MiP_AllPolymer_AllCSS","_",Date, ".png",sep =""), plot = PLOT, width = 4, height = 4, units = "in", dpi = 300)


# *** Mean Area #####


PLOT= ggplot() +
  # Stack bars with Polymer 12 
  geom_bar(data=df_plot_bar_css, aes(x=CSS, y=Mean.Tot.Area.mm2.CSS*200,fill="Average all farms"),  stat="identity") + 
  scale_fill_manual(values = "#20b8bd", labels=Txt_translation$Average_Farms[Txt_translation$CSS==css])+
  
  # # Add a min max line,
  # geom_linerange(data=df_plot_dot_css, aes(x=CSS, y=Mean.particles.CSS*200, ymin = Min.particles.CSS*200, ymax = Max.particles.CSS*200)) +
  # Add text when y max is reached
  geom_text(data=df_plot_dot_css, aes(x=CSS, y=y_max_mm2*(194+7.9*(CSS %% 2)), label=Trunc_mm2 )) +
  
  # Add the x-axis line
  geom_segment(aes(x = 0.4, xend = nrow(Summary7d_CSS)+0.5, y = 0, yend = 0), color = "black", size = 0.5) + #geom_hline(yintercept = 0, color = "black", size = 1) +
  
  # Add the y-axis 
  geom_segment(aes(x = 0.4, xend = 0.4, y =  y_max_mm2*200, yend = 0), color = "black", size = 1) +
  
  # Add jitter
  #geom_jitter(data= Summary4e_Soil, aes(x=CSS, y=Mean.particles*200), alpha = 0.25)+
  geom_point(data= Summary4e_Soil, aes(x=CSS, y=Mean.Tot.Area.mm2*200, color="Individual field sample" ), alpha = 0.25, position = position_jitter(seed=43^3))+
  scale_color_manual(values = "black", labels=Txt_translation$Field_sample[Txt_translation$CSS==css])+
  
  ylim(0,700)+
  # Titles
  ggtitle(Txt_translation$Bar_Title_All[Txt_translation$CSS==css])+
  
  
  # custom y scale 
  scale_y_continuous(breaks = seq(0, y_max_mm2*200, 100), # Have a break for each gap
                     labels = label_at(200), #,                                        # label every second break
                     limits = c(-y_max_mm2*200/40, y_max_mm2*202+20))+ #limits goes beyond to allow the vertical ticks between farms
  
  
  theme_minimal()+
  #guides( color  = "none")+
  labs(y = expression("Average surface of plastic per kg of soil  [mm" ^ 2*"]"),
       fill = Txt_translation$Polymers_identified[Txt_translation$CSS==css],
       color=element_blank() ) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_line(color = "black"),
        axis.line.y = element_blank(),
        legend.title = element_text(color = "NA"), # Add an invisible text to make sure the size is the same as the plot 4.2 
        panel.grid.minor.y = element_blank())+ # Remove the minor grid 
  
  # Add the axis.text.x at the right position
  annotate("text", x = df_plot_dot_css$CSS, y = -y_max_mm2*200/40, label = paste("CSS ", df_plot_dot_css$CSS,sep = ""), angle=0,  hjust=0.5, vjust=1) +
  # Clip the graph to the right size
  coord_cartesian(ylim = c(-y_max_mm2*200/40, NA), clip = "off")


print( PLOT)
ggsave(filename = paste(wd.out,"/CSS",css,"/4.1.Bar_MeanArea_MiP_SumPolymer_AllCSS_txtCSS", css,"_",Date, ".png",sep =""), plot = PLOT, width = 11, height = 4, units = "in", dpi = 300)




} # end Translation for each css, 4.1


