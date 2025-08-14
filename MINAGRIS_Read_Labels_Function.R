# Read_MINAGRIS_label() is a function to read the codes commonly used in MINAGRIS label: 
# It creates new columns from the following codes: 



Read_MINAGRIS_label<-function(Data, Colname_label){
# Description ####
  # Function Inputs: 
  # Data =  DataFrame or vector containing the labels to read
  # colname_label [OPTIONAL]= Column or vector name containing the labels to read, default will look for [Ignore.case]  "File.Name", "Filter.Name", "Sample.Name", or choose the first column with characters. Disp thec column name
  # select.output= additional descriptive columns required to be created
  
  # Function Outputs: 
  # New data frame with additional descriptive columns
  # success message: "[Names of the columns] have been created" or Warning "No MINAGRIS code detected" 
  
# Initialisation ####  
  # If Colname_label not supplied
  
  if(missing(Colname_label)) {
  Colname_label=c(colnames(Data),colnames(Data))[ grep("File.Name|File_Name|Slice|Label" ,colnames(Data), ignore.case = T)][1] # Select the first column name that looks like a colname_label
  }
  Data$Colname_label=Data[,Colname_label]
  



# Reading Data$Colname_label ####  
  # m16_bcm_n_PMF_SR.csv
  # m2_161_r_ir2_PMF_SR.csv
  # m27_11102_n_f3_ir2_PMF_SR.csv
  
  # * Batch_name {1:28} ####
  # Extract the characters starting with the first 'm' and finishing before the next '_'. e.g. "test_m27_11102_n_f3_ir2_P_SR.csv" should be "m27" 
    Data$Batch_name=  gsub("_.*","", str_extract(  Data$Colname_label,("(?i)m.*"))) #
  
    Data$Batch_id=as.numeric(gsub("m|a|b","", Data$Batch_name))
    # gsub("_","",str_extract("test_m27_11102_n_f3_ir2_P_SR.csv", "m\\s*(.*?)\\s*_" ) )
  
    sort(unique(Data$Batch_name ) )
    length(unique(Data$Batch_name ) )
    
   # * Soil_sample {sequence of >3 digits, bcm, rs, st} ####
   # Extract the first sequence of, at least 3 digits, in betwwen two '_'
    Data$Soil_sample = gsub("_","", str_extract(Data$Colname_label, "_\\d{3}\\s*(.*?)\\s*\\_" ) )# Detect the 1st sequence of 3 digits
    
    Data$Soil_sample[grep("bcm",Data$Colname_label, ignore.case = T)]="bcm"
    Data$Soil_sample[grep("st", Data$Colname_label, ignore.case = T)]="st"
    Data$Soil_sample[grep("RS", Data$Colname_label, ignore.case = T)]="rs"
    Data$Soil_sample[grep("pfsr", Data$Colname_label, ignore.case = T)]="rs"
    Data$Soil_sample[grep("bsl",Data$Colname_label, ignore.case = T)]="rs"
    
    Data$Soil_sample[grep("rpc",Data$Colname_label, ignore.case = T)]="rpc"
    
    
    sort(unique( Data$Soil_sample) )
    length(unique( Data$Soil_sample ) )
  
    length(( Data$Soil_sample ) )
    
  # * Sample_type {"n", "r", "rs1", "s2", "s"} ####
    Data$Sample_type=NA
    Data$Sample_type[grep("_r",Data$Colname_label, ignore.case = F)]="r"
    Data$Sample_type[grep("_s",Data$Colname_label, ignore.case = F)]="s"
    Data$Sample_type[grep("_s2",Data$Colname_label, ignore.case = F)]="s2"
    Data$Sample_type[grep("_n",Data$Colname_label, ignore.case = F)]="n"
  
  # * CSS, Farm, Field ####
    # CSS {1 or two digits}, Farm{1 or two digits}, Field{1,2} 
    Data$CSS=0
    Data$Farm=0
    Data$Field=0
    
    # /!\ Confusing labelling: 
    # "1101", "1102", "1111",  "1112",   "1121",    "1122"  
    # Not realy but ok: "1131"  "1132"  "1141"  "1142"  "1151"  "1152"  "1161"  "1162"  "1171"  "1172"  "1181"  "1182"  "1191"  "1192" 
    
    for (s in seq_along(Data$Colname_label)){
      # If the the Soil_sample code has digit, is not a character string among: 
      if (Data$Soil_sample[s] %!in% c("bcm", "st", "rs", "pfsr", "bsl" ) ){ 
          # If the  Soil_sample code has 3 digits, The 1st is the CSS, the 2nd is the Farm, the 3rd is the Field
          if (nchar(Data$Soil_sample[s])==3) {     
            Data$CSS[s]=substr(Data$Soil_sample[s],1,1)
            Data$Farm[s]=substr(Data$Soil_sample[s],2,2)
            Data$Field[s]=substr(Data$Soil_sample[s],3,3)
          } # End if 3 digits
        
        # If the  Soil_sample code has 5 digits, 1-2 is the CSS, 3-4 is the Farm, 5 is the Field
          if (nchar(Data$Soil_sample[s])==5) {
            Data$CSS[s]=substr(Data$Soil_sample[s],1,2)
            Data$Farm[s]=substr(Data$Soil_sample[s],3,4)
            Data$Field[s]=substr(Data$Soil_sample[s],5,5)
          } # End if 5 digits
        
        # If the Soil_sample code has 4 digits, "1-2 is the CSS, 3 is the Farm, 4 is the Field" OR  "1 is the CSS, 2-3 is the Farm, 4 is the Field"
          if (nchar(Data$Soil_sample[s])==4) {
            # If the 1st digit is =! 1, then 2-3 are farm codes because CSS < 20, and 1st digit is not 0 
            if (as.numeric(substr(Data$Soil_sample[s],1,1))!=1){
              Data$CSS[s]=substr(Data$Soil_sample[s],1,1)
              Data$Farm[s]=substr(Data$Soil_sample[s],2,3)
              Data$Field[s]=substr(Data$Soil_sample[s],4,4)
            } else # 1st digit is 1 
            #  If the 1st digit is 1 and the 2nd digit 0 then it is CSS 10 because Farm in 1:12, not 0:12
            if (as.numeric(substr(Data$Soil_sample[s],2,2))==0){   
                Data$CSS[s]=substr(Data$Soil_sample[s],1,2)
                Data$Farm[s]=substr(Data$Soil_sample[s],3,3)
                Data$Field[s]=substr(Data$Soil_sample[s],4,4)
            } else # 2nd digit is not 0 
            #  If the 1st digit is 1 and the 2nd digit is not 0 then it is "CSS 11 farm 1:9" OR "CSS 1, farm 10:12" 
            # The best way to solve it is through batch number 
            if (Data$Batch_name[s] %in% c("m3")) {    # "CSS 1, farm 10:12" <=> Batch m3
              Data$CSS[s]=substr(Data$Soil_sample[s],1,1)
              Data$Farm[s]=substr(Data$Soil_sample[s],2,3)
              Data$Field[s]=substr(Data$Soil_sample[s],4,4)
            } else
            if (Data$Batch_name[s] %in% c("m20","m21", "m23","m26")) {    # "CSS 11 farm 1:9" <=> Batch m20","m21", "m23","m26"
              Data$CSS[s]=substr(Data$Soil_sample[s],1,2)
              Data$Farm[s]=substr(Data$Soil_sample[s],3,3)
              Data$Field[s]=substr(Data$Soil_sample[s],4,4)
            }
          } # End if 4 digits
        } 
        
      if (Data$Soil_sample[s] %in% c("bcm", "st", "rs", "pfsr", "bsl" ) ){
        Data$CSS[s]=-1
        Data$Farm[s]=-1
        Data$Field[s]=-1
      }
    }
    
Data$Colname_label[Data$CSS==0]
sort(unique( Data$CSS) )


    
  # * IR_rep {ir1, ir2, ir3} ####
    Data$IR_rep="ir1"
    Data$IR_rep[grep("ir2", Data$Colname_label, ignore.case = T)]="ir2"
    Data$IR_rep[grep("ir3", Data$Colname_label, ignore.case = T)]="ir3"
    
    Data$Operator=NA
    Data$Operator[grep("SR", Data$Colname_label, ignore.case = T)]="SR"
    Data$Operator[grep("JM", Data$Colname_label, ignore.case = T)]="JM"
    Data$Operator[grep("EC", Data$Colname_label, ignore.case = T)]="EC"
    
   
  # * Filter rep ####
  # When a soil sample (5g) is divided over several filters: 
    Data$Filter_div="0"
    Data$Filter_div[grep("_f1", Data$Colname_label, ignore.case = T)]="f1"
    Data$Filter_div[grep("_f2", Data$Colname_label, ignore.case = T)]="f2"
    Data$Filter_div[grep("_f3", Data$Colname_label, ignore.case = T)]="f3"
    Data$Filter_div[grep("_f4", Data$Colname_label, ignore.case = T)]="f4"
    Data$Filter_div[grep("_f5", Data$Colname_label, ignore.case = T)]="f5"
    Data$Filter_div[grep("_f6", Data$Colname_label, ignore.case = T)]="f6"
    Data$Filter_div[grep("_f7", Data$Colname_label, ignore.case = T)]="f7"
    Data$Filter_div[grep("_f8", Data$Colname_label, ignore.case = T)]="f8"
    Data$Filter_div[grep("_f9", Data$Colname_label, ignore.case = T)]="f9"
    Data$Filter_div[grep("_f10", Data$Colname_label, ignore.case = T)]="f10"
    Data$Filter_div[grep("_f11", Data$Colname_label, ignore.case = T)]="f11"
    
    # * Extract info from the file name: ####
    # Filter_name, in between the working directory and "PMF" 
    Data$Filter_name <- Data$Colname_label # PMF File name
    Data$Filter_name <- gsub(".*/", "",Data$Filter_name)   # Remove the working directory name

    Data$Filter_name <- gsub(".*/", "",  Data$Filter_name)   # Remove the working directory name bis
    Data$Filter_name <- gsub(".csv", "", Data$Filter_name) # Remove 'the working directory name'.csv'
    
    Data$Filter_name <- gsub("_PMF_compleate_.*", "", ignore.case = T, Data$Filter_name)
    Data$Filter_name <- gsub("_PMF_.*", "", ignore.case = T, Data$Filter_name)
    Data$Filter_name <- gsub("_compleate_.*", "", ignore.case = T, Data$Filter_name)
    Data$Filter_name <- gsub("_manual_.*", "", ignore.case = T, Data$Filter_name)
    Data$Filter_name <- gsub("_ir2", "", ignore.case = T, Data$Filter_name)
    Data$Filter_name <- gsub("_ir3", "", ignore.case = T, Data$Filter_name)
    
    
    # # * IR_file ####
    # 
    # # IR_file_name, in between the working directory and "PMF" 
    # 
    # Data$IR_File_name <- gsub(".*/", "", Data$Colname_label)   # Remove the working directory name
    # Data$IR_File_name <- gsub(".*/", "",  Data$IR_File_name)   # Remove the working directory name bis
    # Data$IR_File_name <- gsub(".csv", "", Data$IR_File_name) # Remove 'the working directory name'.csv'
    # 
    # Data$IR_File_name <- gsub("_PMF_compleate_.*", "", ignore.case = T, Data$IR_File_name)
    # Data$IR_File_name <- gsub("_PMF_.*", "", ignore.case = T, Data$IR_File_name)
    # Data$IR_File_name <- gsub("_compleate_.*", "", ignore.case = T, Data$IR_File_name)
    # Data$IR_File_name <- gsub("_manual_.*", "", ignore.case = T, Data$IR_File_name)
    
    
    # * Add Reference soils ####
    # = blank soils = Plastic Free soils
    # /!\ Manual assignment of CSS for the RS /!\ 

  Data$CSS[grep("RS1",Data$Colname_label )]=1
  Data$CSS[grep("m4_RS2_n",Data$Colname_label )]=2
  Data$CSS[grep("m4_281_rs_n",Data$Colname_label )]=2
  Data$CSS[grep("rs3",Data$Colname_label )]=3
  Data$CSS[grep("RS3",Data$Colname_label )]=3
  Data$CSS[grep("m5_css3_rs_n",Data$Colname_label )]=3
    #Data$CSS[grep("",Data$Colname_label )]=4
  Data$CSS[grep("rs5",Data$Colname_label )]=5
  Data$CSS[grep("rs6",Data$Colname_label )]=6
  Data$CSS[grep("rs7",Data$Colname_label )]=7
  Data$CSS[grep("rs8",Data$Colname_label )]=8
  Data$CSS[grep("rs9",Data$Colname_label )]=9
    #Data$CSS[grep("",Data$Colname_label )]=10
  Data$CSS[grep("rs11",Data$Colname_label )]=11
  Data$CSS[grep("pfsr",Data$Colname_label )]=-1
    
  
  
  # Some checks ####
    Long=Data[nchar(Data$Colname_label)>20,]
  
    NoCSS=subset(Data, CSS==-1 & Soil_sample %!in%  c("bcm", "st") | Soil_sample =="rs") 
    
  # Return data   
    return(Data)
}

