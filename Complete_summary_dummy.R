
DF=data.frame(File_name1= c("A1","A1","A1", "A2", "A2",   "B",  "B",  "B",  "B",    "C",  "C", "C"), #c(A1,A2,B,C)
              File_name2= c("A1","A1","A1", "A2", "A2",   "B",  "B",  "B",  "B",    "C",  "C", "C"), #c(A1,A2,"B","C)
              Soil=       c("A", "A", "A",  "A",  "A",    "B",  "B",  "B",  "B",    "C",  "C", "C"), #c(A","B","C)
              Polymer1=   c("pa","pa","pab","pac","pa",   "pab","pab","pab","pab",  "pac","pc","pc"),     #c(pa"," pab"," pac"," pc)
              Polymer2=   c("pa","pa","pa", "pa", "pa",   "pa", "pa", "pa", "pa",   "pa", "pc","pc"),      #c(pa"," pc)
              SizeCat=    c("s1","s2","s1", "s1", "s1",   "s1", "s1", "s2", "s2",   "s1", "s1","s2"),        #c(s1","s2) 
              Area.px=    c(1:12)
)

# 4 files x (3 soils) x 4 polymers1 x(2 polymers2) x 2 sizes 
# available data: 12 rows
# soil is nested in file and polymers2 is nested in Polymer1
# There 2 double particules in "B" 
# full relevant combinations:4x4x2 +2 =  34 rows
# 

# Sumerize per soil#### 

#1.1 first complete the dataset  ####
# 4x4x2 +2 =  34 rows

DF_complete11= DF %>%
  complete(nesting(File_name1, File_name2, Soil),
           nesting(Polymer1, Polymer2),
           nesting(SizeCat),
          fill=list( Area.px=0))

# wrong ways to do it: ####
# e.g. 1 Do not nest SizeCat separatly ####
  DF_complete11_w1= DF %>%
    complete(nesting(File_name1, File_name2, Soil),
             nesting(Polymer1, Polymer2, SizeCat),
             fill=list( Area.px=0))
  
  # SizeCat is nested with polymers so if a combination polymer x Polymer does not appear in the data set then it is not added
  # => pac x S2 is missing form all files 4x4x2 +2 -4 =  30 rows


# e.g. 2 Do not nest SizeCat at all ####
  DF_complete11_w2= DF %>%
    complete(nesting(File_name1, File_name2, Soil),
             nesting(Polymer1, Polymer2),
             fill=list( Area.px=0))
  
  # SizeCat is missing: 
  # =>  4x4 +2 +3 (already present double size cated polymers "s2") =  21 rows:  for e.g. Sample A misses pab x S2

# 1.2. then do the Sum per file  ####
  DF_complete11_File= DF_complete11 %>%
    group_by(File_name1,File_name2,Soil, Polymer1, Polymer2, SizeCat)%>%
      summarise( N.particles= sum(Area.px!=0),
                 Tot.Area.px= sum(Area.px))
                 
  # 4x4x2 =  32 rows   
  
  # 1.3. then do the Mean per soil  ####
  DF_complete11_Soil= DF_complete11_File %>%
    group_by(Soil, Polymer1, Polymer2, SizeCat) %>%
    summarise( Mean.particles= mean(N.particles),
               Mean.Tot.Area.px= mean(Tot.Area.px) )
  
  # 3x4x2 =  24 rows 
  
                 
# 2.1. First do the sum per file ####
 DF_File21= DF %>%
   group_by(File_name1, File_name2, Soil, Polymer1, Polymer2, SizeCat)%>%
   summarise( N.particles= sum(Area.px!=0),
              Tot.Area.px= sum(Area.px))
 # 12-2 = 10 rows   
  
  
# 2.2 Then complete the DF ####
 DF_File21_complete= DF_File %>%
   ungroup() %>%
   complete(nesting(File_name1, File_name2, Soil),
            nesting(Polymer1, Polymer2),
            nesting(SizeCat),
            fill=list( N.particles=0,
                       Tot.Area.px=0))
  # 4x4x2 =  32 rows
  
# 2.3 Then do the Mean per Soil  ####  
 DF_complete23_Soil= DF_File21_complete %>%
   group_by(Soil, Polymer1, Polymer2, SizeCat) %>%
   summarise( Mean.particles= mean(N.particles),
              Mean.Tot.Area.px= mean(Tot.Area.px) )
 # 3x4x2 =  24 rows 
 
 
 DF_File21_complete== DF_complete11_File
 DF_complete23_Soil==DF_complete11_Soil
 