# Check the available processed and visually checked results (PMF data) from WUR,
# Create METADATA and combined result table
# PMF data is available in the R project. 

library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(dplyr)
library(stats)

rm(list=ls()) # cleaning console
graphics.off() # cleaning plots
'%!in%' <- function(x,y)!('%in%'(x,y))

# Set WD in the project: 
setwd("C:/Users/berio001/Minagris/MINAGRIS_Microplastic_Soil_Assessmnent")
wd.out= "Outputs" 

# Load the MINAGRIS_Read_Labels_function.R
source("MINAGRIS_Read_Labels_Function.R") 


# 1. Summarize available PMF results ####
  # = results.csv produced after PMF processing and visual checking. 
  # There can be more than IR folders because some are analysed by several operators
  # There can be less than IR folders when all IR folders have not been PMF processed yet. 
  
  # Directory of the Manually checked IR files 
  wd.in="WUR_Data"
  
  # * Create a list of available PMF files  ####
    PMF_File_names <- dir(wd.in, full.names = F, recursive = T)
    PMF_File_names <- PMF_File_names[grepl("*.csv$", PMF_File_names)] # select only .csv files
    
    PMF_File_names <- PMF_File_names[grepl("m.*", PMF_File_names, ignore.case = F)] # select only files starting with 'm' // I would have liked to select files with PMF in the name but not all results follow this rule.
    # Excluded file, for checking: 
    PMF_File_names[PMF_File_names %!in% PMF_File_names[grep("m.*", PMF_File_names, ignore.case = F)] ]
    
    PMF_File_names <- PMF_File_names[!grepl("Excluded_Analysis", PMF_File_names, ignore.case = F)] # select only files not in 'Excluded_Analysis'
    
    # /!\ There should not be Duplicated files! 
    if (any(duplicated( PMF_File_names))){
      warning("Duplicated PMF File names")
      PMF_File_names[duplicated( PMF_File_names)]
    }
    
  # * Create Data Frame of PMF files available
    METADATA=data.frame(File.dir=PMF_File_names, col.sep=NA, dec.sep=NA, skip=NA, n.uP.detected=NA )
    METADATA$PMF_File_name <- gsub(".*/", "", METADATA$File.dir)   # Remove the working directory name
    METADATA_PMF=Read_MINAGRIS_label(METADATA)
    
    # Total number of files: 
    nrow(METADATA_PMF)
    
    
  
  # * Number of unique soil samples per CSS ####
 
    # Number of CSS
    unique(METADATA_PMF$CSS)
    
    for (css in 1:11) {
      print( paste("CSS",css, ";", length(unique(METADATA_PMF$Farm[METADATA_PMF$CSS==paste(css)])),"Unique Farms", ";", length(unique(METADATA_PMF$Soil_sample[METADATA_PMF$CSS==paste(css)])), "Unique soils"))
      print(sort(unique(METADATA_PMF$Soil_sample[METADATA_PMF$CSS==paste(css)])))
    }  
    
    Summary_Data.PMF.CSS.n = subset( METADATA_PMF, Soil_sample %!in% c("pfsr","st", "bcm", "RS") & Sample_type =="n")   %>% 
      group_by(  CSS, Sample_type  ) %>% # Group per CSS
      summarise(N_Files=n(),
                Farms= unique(paste0(Farm, collapse = " ; ") ),
                Fields=unique(paste0(Field, collapse = " ; ") )) 
    
    
    
  # * Number of QCs
  Summary_Data.PMF.QC = subset(METADATA_PMF, Soil_sample %in% c("bcm","pfsr","st") |  Sample_type %in% c("r","s2","s" ) )   %>% 
    group_by(  Sample_type, Soil_sample ) %>% # Group per CSS
    summarise(N_Files=n(),
              Batch= unique(paste0(Batch_Name, collapse = " ; ") ), 
              Operators= unique(paste0(Operator, collapse = " ; ")))
  
  

#2. Load the data From PMF ####
  # Expected Header:   
  Header=c("X","Particle.id","N.px","Area.um2","Class.Idx","Polymer.grp","XPos.um","YPos.um",
           "LeftEdge.um","RightEdge.um","BottomEdge.um","TopEdge.um","CenterX.um","CenterY.um",
           "PC1LdX","PC1LdY","Length.um","Width.um","Aspect_ratio","Direction.deg","Relevance","Similarity","Done")
  
  # Create a list to load all the PMF results files
  Loading_list <- vector("list", length = length(PMF_File_names))  
  
  # Start For loop across all PMF files available
  for( i in seq_along(PMF_File_names)) {
    file_i_dir= paste(wd.in, PMF_File_names[i], sep="/")
    # * Check the column and decimal separators format ####
    # The expected format is [col.sep = "," ; dec.sep="."]
    
    # Column separator is read from the "metada description of the PMF exported file
    METADATA_PMF$col.sep[i] = substr(readLines(file_i_dir)[8],1,1)
    METADATA_PMF$dec.sep[i] = substr(sub(";Minimum Relevance:;0",replacement = "", readLines(file_i_dir)[grep("Minimum Relevance", readLines(file_i_dir))]),1,1)
    
    # Check the number of Metadata lines to skip before reading the table. 
    METADATA_PMF$skip[i]=grep( "Particle Properties:", readLines(file_i_dir))
    
    # If the column and decimal separator are different, read the data table:
    if (METADATA_PMF$col.sep[i] !=   METADATA_PMF$dec.sep[i]) {  
      # create a temporary data frame with the specific loaded file 
      file_i <-read.table(text = gsub(",", ".", readLines(file_i_dir)), sep = METADATA_PMF$col.sep[i], header = T, skip =  METADATA_PMF$skip[i], col.names =Header  )
      METADATA_PMF$n.uP.detected[i]= nrow( file_i ) # Number of particles identified in the sample is the number of lines in the file
      
      #/!\ If no particules are idemtified in this file, there have been no import
      # But we still want to record the result file 
      # So we add lines for Files with no plastic detected: 
      if (nrow( file_i )==0){ 
        file_i <- data.frame(
          X = 0, 
          Particle.id = 0,
          N.px = 0,
          Area.um2 = 0,
          Class.Idx = 0,
          Polymer.grp = "No.plastic",
          XPos.um = 0,
          YPos.um = 0,
          LeftEdge.um = 0,
          RightEdge.um = 0,
          BottomEdge.um = 0,
          TopEdge.um = 0,
          CenterX.um = 0,
          CenterY.um = 0,
          PC1LdX = 0,
          PC1LdY = 0,
          Length.um = 0,
          Width.um = 0,
          Aspect_ratio = 0,
          Direction.deg = 0,
          Relevance = 0,
          Similarity = 0,
          Done = NA,
          Filter_name = NA
        )
      } # end if no particles detected
      
    } #End If the column and decimal separator are different
    
    # If the column and decimal separator are the same !
    #/////// WORK IN PROGRESS \\\\\\\\\\\\\\\\\
    
    # * Extract info from the file name: ####
    # Filter_name, in between the working directory and "PMF" 
    file_i$PMF_File_name <- file_i_dir # PMF File name
    file_i$PMF_File_name <- gsub(".*/", "",  file_i_dir)   # Remove the working directory name
    
    file_i$Filter_name <- gsub(".*/", "",  file_i_dir)   # Remove the working directory name
    file_i$Filter_name <- gsub(".*/", "",  file_i$Filter_name)   # Remove the working directory name bis
    file_i$Filter_name <- gsub(".csv", "", file_i$Filter_name) # Remove 'the working directory name'.csv'
    
    file_i$Filter_name <- gsub("_PMF_compleate_.*", "", ignore.case = T, file_i$Filter_name)
    file_i$Filter_name <- gsub("_PMF_.*", "", ignore.case = T, file_i$Filter_name)
    file_i$Filter_name <- gsub("_compleate_.*", "", ignore.case = T, file_i$Filter_name)
    file_i$Filter_name <- gsub("_manual_.*", "", ignore.case = T, file_i$Filter_name)
    file_i$Filter_name <- gsub("_ir2", "", ignore.case = T, file_i$Filter_name)
    file_i$Filter_name <- gsub("_ir3", "", ignore.case = T, file_i$Filter_name)
    
    
    # Make sure that numerical parameters are numerical values:
    # Vector of numerical column names:
    num=c("Particle.id","N.px","Area.um2","Class.Idx","XPos.um","YPos.um",
          "LeftEdge.um","RightEdge.um","BottomEdge.um","TopEdge.um","CenterX.um","CenterY.um",
          "PC1LdX","PC1LdY","Length.um","Width.um","Aspect_ratio","Direction.deg","Relevance","Similarity")
    
    #file_i[,num]=as.data.frame(apply(file_i[,num], 2, function(x) {as.numeric(sub(",", ".", x, fixed = TRUE))}))  # Commented because not needed anymore?!
    file_i[,"Polymer.grp"]=as.character(file_i[,"Polymer.grp"])
    
    
    file_i$Mass.ng=0
    Loading_list[[i]] <- file_i
    
    
  } # End Files 'for loop' 
  
  METADATA_PMF$PMF_File_name[METADATA_PMF$col.sep==METADATA_PMF$dec.sep] # /!\ if col.sep==dec.sep
  METADATA_PMF$IR_File_name=  gsub("_PMF.*", "", METADATA_PMF$PMF_File_name)
  
 
  
  
# 3. Merge uP data frame and add labels #### 
  # Bind the Loading_list into a data frame of all microplastics identified at WUR:
  MiP_wur <- bind_rows(Loading_list)
  
  # Read labels 
  MiP_wur <- Read_MINAGRIS_label(MiP_wur)
  unique(MiP_wur$CSS)
  
  # Some re labeling: 
  METADATA_PMF$IR_File_name[METADATA_PMF$PMF_File_name=="m4_281_rs_n_PMF_JM.csv"]="m4_RS2_n"
  MiP_wur$Filter_name[MiP_wur$Filter_name=="m4_281_rs_n"]="m4_RS2_n"
  
  MiP_wur$IR_File_name=  gsub("_PMF.*", "", MiP_wur$PMF_File_name)
  MiP_wur$IR_File_name[MiP_wur$PMF_File_name=="m4_281_rs_n_PMF_JM.csv"]="m4_RS2_n"
  
  # * Add WUR lab ####
  MiP_wur$Lab="WUR"
  
  # Check Names NAs
  MiP_wur[is.na( MiP_wur$Soil_sample),]
  MiP_wur[is.na( MiP_wur$Sample_type),]
  
  # * Add Polymer groups ####
  # List of detected polymers
  Polymers.PMFclassifiers=unique(MiP_wur$Polymer.grp) # 21 classifiers + MYSP {Mysterious Polymer}
  
  # Reduce the polymer groups to 10 main and rest in other plastic
  Polymer.red12= c("PP", "PE",  "PS", "PET", "PVC", "PU", "PMMA", "PLA", "PC", "PA", "No.plastic") # 12 plastics , +Others  #"CA","silicone",
  
  # Reduce the polymer groups to 3 spiked ones: PE, PLA, PVC
  Polymer.red3= c("PE", "PLA", "PVC","No.plastic") # PE, PLA, PVC, +Others
  
  # Name polymers not in Polymer.red as "Other.Plastic"
  df_plastic.pol=MiP_wur
  df_plastic.pol$Polymer.red12=df_plastic.pol$Polymer.grp
  df_plastic.pol$Polymer.red12[df_plastic.pol$Polymer.grp %!in% Polymer.red12]="Other.Plastic"
  df_plastic.pol$Polymer.red3=df_plastic.pol$Polymer.grp
  df_plastic.pol$Polymer.red3[df_plastic.pol$Polymer.grp %!in% Polymer.red3]="Other.Plastic"
  
  MiP_wur=df_plastic.pol
  
  
  
  # 4. Correct the scale of px size -> MiP_wur_cor #####
  
  # * Open the table Tiles_per_sample.csv. ####
  Tile_per_sample=read.csv("//wurnet.nl/dfs-root/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Tiles_per_sample.csv") 
  
  # * Check if all samples are already in the Tiles_per_sample.csv. ####
  
  length(unique(MiP_wur$PMF_File_name) )
  length(unique(MiP_wur$Filter_name) )
  length(unique(MiP_wur$Soil_sample) )
  
  unique(unique(MiP_wur$PMF_File_name) %in% METADATA_PMF$PMF_File_name) # Check all MiP_wur$PMF_File_name are in METADATA_PMF$PMF_File_name
  
  unique(METADATA_PMF$IR_File_name %in% Tile_per_sample$File_Names)
  
  METADATA_PMF$PMF_File_name[METADATA_PMF$IR_File_name %!in% Tile_per_sample$File_Names]
  
  # Re-name samples that contain an error: 
  
  Tile_per_sample$File_Names[Tile_per_sample$File_Names=="m2_121_n2"]="m2_121_n_ir2"
  Tile_per_sample$File_Names[Tile_per_sample$File_Names=="m2_161_r2"]="m2_161_r_ir2"
  Tile_per_sample$File_Names[Tile_per_sample$File_Names=="m2_1_n_RS1"]="m2_122_n_RS1"
  Tile_per_sample$File_Names[Tile_per_sample$File_Names=="m5_311_n_ir2"]="m5_311_n_ir3"
  Tile_per_sample$File_Names[Tile_per_sample$File_Names=="m5_css3_rs_n"]="m5_RS3_n"
  
  
  METADATA_PMF$PMF_File_name[METADATA_PMF$IR_File_name %!in% Tile_per_sample$File_Names]
  
  # * If not, create the table again.####
  
  if (length(METADATA_PMF$PMF_File_name[METADATA_PMF$IR_File_name %!in% Tile_per_sample$File_Names])!=0){
    METADATA_PMF$IR_File_name[METADATA_PMF$IR_File_name %!in% Tile_per_sample$File_Names]
    warning("recreate/ update Tiles_per_sample.csv.")
    
    # Load the FTIR_tiles_per_IRsample_Function.R: 
    source("W:/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/FTIR_tiles_per_IRsample_Function.R") 
    
    #Directory of raw IR data folder:
    wd.raw="//wurnet.nl/dfs-root/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/1_FTIR_rawdata/uFTIR_files"
    
    Tile_per_sample=uFTIR_n_tiles(wd.raw)
  } #End if 
  
  # * If yes, load tile numbers ####
  
  
  #METADATA_PMF2=merge(METADATA_PMF, Tile_per_sample, by.x = "IR_File_name",  by.y = "File_Names", all.x = TRUE)
  
  unique(MiP_wur$IR_File_name[(MiP_wur$IR_File_name %!in% Tile_per_sample$File_Names)])
  
  unique(Tile_per_sample$File_Names[(Tile_per_sample$File_Names %!in% MiP_wur$IR_File_name )])
  
  # Add Tile_per_sample in MiP_wur
  MiP_wur_cor=merge(Tile_per_sample, MiP_wur, by.x="File_Names", by.y ="IR_File_name", all.y = TRUE)
  
  
  #Check NAs
  MiP_wur_cor[is.na( MiP_wur_cor$Tile_Numbers),]
  
  
  # * Assign number of px per tiles: ####
  
  # METADATA_PMF2$px_size=0
  # METADATA_PMF2$px_size[METADATA_PMF2$Tile_Numbers<560]= 44.4 # 16 px_per_tile
  # METADATA_PMF2$px_size[METADATA_PMF2$Tile_Numbers>575]= 88.8 # 8 px_per_tile
  # 
  # METADATA_PMF2$PMF_File_name[ METADATA_PMF2$px_size==0]
  
  
  unique(MiP_wur$PMF_File_name[(MiP_wur$IR_File_name %!in% Tile_per_sample$File_Names)])
  
  # Create new variable: px_size 
  
  # Px size not measured in these range:
  MiP_wur_cor$File_Names[MiP_wur_cor$Tile_Numbers<182]
  MiP_wur_cor$px_size[ MiP_wur_cor$Tile_Numbers>560 & MiP_wur_cor$Tile_Numbers<575]
  
  # Assign 
  MiP_wur_cor$px_size =0
  
  MiP_wur_cor$px_size[ MiP_wur_cor$Tile_Numbers>180 & MiP_wur_cor$Tile_Numbers<560]= 44.4 # 16 px_per_tile
  MiP_wur_cor$px_size[ MiP_wur_cor$Tile_Numbers>575 & MiP_wur_cor$Tile_Numbers<2240]= 88.8 # 8 px_per_tile
  
  MiP_wur_cor$File_Names[  MiP_wur_cor$px_size==0]
  
  # Calculate corrected Area:  
  MiP_wur_cor$Area.um2.cor= (MiP_wur_cor$N.px) * (MiP_wur_cor$px_size)^2
  
  
  max(MiP_wur_cor$Area.um2.cor)
  
  
  # Area.cor.ratio
  MiP_wur_cor$Area.cor.ratio=MiP_wur_cor$Area.um2/MiP_wur_cor$Area.um2.cor
  MiP_wur_cor$Area.cor.ratio[MiP_wur_cor$Area.um2.cor==0]=-1
  
  Ratio_out=MiP_wur_cor[MiP_wur_cor$Area.cor.ratio>1 & MiP_wur_cor$Polymer.grp!="No.plastic"  ,]
  
  unique(is.na(MiP_wur_cor$ Class.Idx))
  unique(is.na(MiP_wur_cor$File_Names))
  
  unique(MiP_wur_cor$PMF_File_name=="<NA>")
  
  
  # Plot
  ggplot(  data = MiP_wur_cor, aes(Area.um2, Area.um2.cor, colour= factor(px_size ) ) )+
    geom_abline(intercept = 0, slope = 1)+ # slope x=y
    geom_abline(intercept = 0, slope = 0.25)+ # slope x=y/4
    geom_point() +
    theme_minimal()
  
  
  
  
  # Filter out big particles (7)
  MiP_wur_cor=subset(MiP_wur_cor, MiP_wur_cor$Area.um2.cor<2000*2000)
  
  
  #Data_cor=subset(Data_cor, Area.cor.ratio>0.95 & Area.cor.ratio<1.05)
  
  # * Correct Area and lengths  ####
  # unique (Data$PMF_File_name %in% METADATA_PMF2$PMF_File_name)
  # 
  # Data_cor=merge(METADATA_PMF2, Data, by="PMF_File_name", all.y = TRUE)
  # 
  # Data_cor$Area.um2.cor= Data_cor$N.px * Data_cor$px_size^2
  # 
  # ggplot(  data = Data_cor, aes(Area.um2, Area.um2.cor) )+
  #   geom_point()
  # 
  # 
  # 
  # Data_cor[Data_cor$Area.um2==max( Data_cor$Area.um2),]
  
  ###############
  
  
  # Data$mean.px.length= Data$Length.um/ Data$N.px
  # min(df_plastic.red$Length.um)
  # 
  # A=Data [ Data$mean.px.length<=1,]
  # Files2corect=unique(A$PMF_File_name)
  # 
  # d.factors<-c("LeftEdge.um","RightEdge.um","BottomEdge.um","TopEdge.um","CenterX.um","CenterY.um","Length.um","Width.um")
  # dd.factors=c("Area.um2")
  # 
  # Data[Data$PMF_File_name %in% Files2corect, d.factors]=Data[Data$PMF_File_name %in% Files2corect, d.factors]*88
  # Data[Data$PMF_File_name %in% Files2corect, dd.factors]=Data[Data$PMF_File_name %in% Files2corect, dd.factors]*88*88
  # 
  # Data$Area.um2.cor = Data$N.px * 88.3 *88.3
  
  # 4. Export Results ####
  
  write.csv(Summary_Data.PMF.CSS.n, paste(wd.out,"PMF_SummaryCSS_2024.11.13.csv",sep = "/"))
  write.csv( Summary_Data.PMF.QC, paste(wd.out,"PMF_SummaryQC_2024.11.13.csv",sep = "/"))
  write.csv(METADATA_PMF, paste(wd.out,"PMF_METADATA_2024.11.13.csv",sep = "/"))
  write.csv(MiP_wur_cor, paste(wd.out,"WUR_MiP_Particles_2024.11.13.csv",sep = "/"))
  
