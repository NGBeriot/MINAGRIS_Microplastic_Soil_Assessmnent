# List the number of tiles in an uFTIR file

uFTIR_n_tiles<-function(wd.raw){

  start.time <- Sys.time()
  
  Sample_names="A"
  Tile_numbers=0
  c=0 # Counter of folders
  
  # L1 is the first folder level in "1_FTIR_rawdata/uFTIR_files" <=> "Batch"
  L1 <- dir(wd.raw, pattern = "", full.names = T)
  for (i in 1:length(L1) ) { # For each Bach i
    # L2 is the Second folder level in "1_FTIR_rawdata/uFTIR_files" <=> "IR Folder"
    L2 <- dir(L1[i], pattern = "", full.names = T)
    for (j in 1:length(L2)) {  # For each IR Folder j, in Batch i
      drd <- length(dir(L2[j], pattern = ".drd$")) # number of .drd files = number of tiles
      c=c+1 # next folder
      #print(c)
      Tile_numbers[c]=drd
      Sample_names[c]=L2[j]
    } # end loop Folders of Batch i
    #Show progress of Batches 
  print(sub(".*/", "", L1[i]))
  
  } # end loop Batches
  
  Tile_per_sample= data.frame( File_Names=Sample_names, Tile_Numbers=Tile_numbers)
  
  #Tile_per_sample$File_Names=gsub(wd.raw, "", Tile_per_sample$File_Names)
  Tile_per_sample$File_Names=gsub(".*/", "", Tile_per_sample$File_Names)
  
  
   # write.csv(Tile_per_sample,"//wurnet.nl/dfs-root/ESG/DOW_SLM/Data_archive/Minagris/MINAGRIS_Soil_Assessment/2_MP_results/Tiles_per_sample.csv")  
  
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,2)
  time.taken
  
  # Time difference of 50.69 secs  
  # Time difference of 58.83 secs

  return(Tile_per_sample)
} # End function 


