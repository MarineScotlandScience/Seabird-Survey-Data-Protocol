###############################################
## Offshore Ornithology and Mammal Survey Data Protocol Review Script ##
## V1.2
##
## If running from R-Studio use: control|shift|s to run the complete script
###########

rm(list=ls())
## load required packages
## install the following package if necessary (only needs to be done on first use of this script)
#install.packages("readxl")
require(readxl)

############################################
## Enter folder name and excel file name for survey data - assumes a single excel file with multiple sheets. 
############################################
## The 'root.folder' is the top-level folder containting this script, and these sub-folders: 'data requirement info' and 'example data' 
## Note this needs to match the name given to the extracted zip folder 
root.folder="C:\\Users\\mtrinder\\WFH folder\\1 PROJECT WORK\\MS\\Survey data protocols\\MS Seabird Data check\\"

## If testing the script with the built in examples, uncomment one of the following (example data from APEM, HiDef and JNCC)
data.file.name = "Digital Aerial Still Example 27_01_2017.xlsx"
#data.file.name = "Digital Aerial Video Example 27_01_2017.xlsx"
#data.file.name = "ESAS Example 27_01_2017.xlsx"

## If using to review data comment out all of the previous three lines and ensure that 'data.file.name' below is 
## uncommented and an appropriate file name is provided.
#data.file.name=


## If the following variable is set to '1' then the first 5 and last 5 rows of each data sheet are printed to the console and saved to text files with named 'Summary_info_' and the sheet name located in the same folder as the data sheet. The format is csv, so they are opened by defualt in Excel
save.top.tail=0
############################################

############################################
## When the zip folder is extracted it should maintain the necessary structure to find all the necessary files.
## The unzipped folders should correspond to the following:
data.folder = paste(root.folder,"example data\\",sep="")
## In use any folder location can be entered for data.folder but the 


## compulsory field names - text files for each sheet 
data.requirement.folder=paste(root.folder,"data requirement info\\",sep="")
############################################

############################################
cat("LOADING DATA FOR REVIEW...\n")
cat("############################\n")

exp.summary.names=read.table(paste(data.requirement.folder,"expected.summary.fieldnames.txt",sep=""),sep=",",header=T)
exp.observation.names=read.table(paste(data.requirement.folder,"expected.observation.fieldnames.txt",sep=""),sep=",",header=T)
exp.track.names=read.table(paste(data.requirement.folder,"expected.track.fieldnames.txt",sep=""),sep=",",header=T)

## extract sheet names from excel file
Sheet.Names = excel_sheets(paste(data.folder,data.file.name,sep=""))

## load each sheet into a data.frame with the sheet name
t.load=1
while(t.load<=length(Sheet.Names)){
  assign(Sheet.Names[t.load],suppressWarnings(read_excel(path=paste(data.folder,data.file.name,sep=""),sheet=Sheet.Names[t.load])))
  t.load=t.load+1
}
############################################

############################################
#compare expected fieldnames to those in the data
cat("CHECKING FIELD NAMES IN EACH DATA SHEET...\n")

exp.summary.names$in.data = as.character(tolower(exp.summary.names$fieldnames)) %in% tolower(names(Summary))
missing.summary.fields=exp.summary.names[exp.summary.names$in.data=="FALSE",]
if(dim(missing.summary.fields)[1]==0) {
  cat("all summary fields present\n")
  cat("\n")
} else {
  cat("the following summary fields appear to be missing:\n")
  print(missing.summary.fields[,1],max.levels=0)
  cat("\n")
}

exp.observation.names$in.data = as.character(tolower(exp.observation.names$fieldnames)) %in% tolower(names(Observations))
missing.observation.fields=exp.observation.names[exp.observation.names$in.data=="FALSE",]
if(dim(missing.observation.fields)[1]==0) {
  cat("all observation fields present\n")
  cat("\n")
} else {
  cat("the following observation fields appear to be missing:\n")
  print(missing.observation.fields[,1],max.levels=0)
  cat("\n")
}

exp.track.names$in.data = as.character(tolower(exp.track.names$fieldnames)) %in% tolower(names(Track))
missing.track.fields=exp.track.names[exp.track.names$in.data=="FALSE",]
if(dim(missing.track.fields)[1]==0) {
  cat("all track fields present\n")
  cat("\n")
} else {
  cat("the following track fields appear to be missing:\n")
  print(missing.track.fields[,1],max.levels=0)  
  cat("\n")
}

cat("############################\n")
############################################

############################################
## check Euring codes
cat("CHECKING OBJECT CODES AGAINST ESAS CODES...\n")

## load latin and common names for conversion
species.list=read_excel(paste(data.requirement.folder,"full.species.list.xlsx",sep=""))

##load esas species codes
esas.codes=read_excel(paste(data.requirement.folder,"ESAS.species.codes.xlsx",sep=""))
esas.codes$common.name=trimws(esas.codes$common.name)

## list of all objects seen (birds, mammals, fish etc.)
all.objects.recorded =sort(unique(Observations$Object.Name))

## list of species recorded which are identifiable by euring/esas code
#euring.species.recorded =sort(unique(Observations$Object.Name[Observations$Object.ID %in% euring.codes$Code]))
esas.species.recorded = sort(unique(Observations$Object.Name[Observations$Object.ID %in% esas.codes$VOOUS.code]))

## list of euring/esas species names for recorded species - check this against last
#recorded.euring.species.names =sort(euring.codes$Common.name[euring.codes$Code %in% Observations$Object.ID])
recorded.esas.species.names =sort(esas.codes$common.name[esas.codes$VOOUS.code %in% Observations$Object.ID])

## table of objects and euring codes
objects.and.codes = unique(Observations[,which(names(Observations) %in% c("Object.Name","Object.ID"))])

cat("\n")
cat("Names of all objects in data:\n")
cat("\n")
print(all.objects.recorded)
cat("\n")
cat("Names of all objects in data which have an esas code:\n")
cat("\n")
print(esas.species.recorded)
cat("\n")
cat("Official esas designation for objects in data - does this match previous output?:\n")
cat("\n")
print(recorded.esas.species.names)
cat("\n")

if(all(esas.species.recorded %in% recorded.esas.species.names)==FALSE) {
  cat("If two lists are different then ESAS code or object name may have been incorrectly entered,\n")
  cat("check this table of unique objects and codes from the data:\n")
  cat("\n")
  print(objects.and.codes)
  cat("\n")
}
cat("############################\n")
############################################

############################################
##check to make sure field contents match expected
cat("CHECKING FIELD CONTENT AGAINST EXPECTED TYPE:\n")
cat("These data are identified as: \n")
cat("\n")
print(Summary$Survey.type)
cat("\n")
cat("Given this survey type are the following mismatched field types a concern?\n")
cat("\n")

Summary.fieldtypes=data.frame(names(Summary))
names(Summary.fieldtypes)="field.names"
Summary.fieldtypes$data.field.type=unlist(lapply(Summary, class))

Summary.fieldtypes = merge(Summary.fieldtypes,exp.summary.names,by.x="field.names",by.y="fieldnames",all.x=TRUE,sort=FALSE)

cat("The following SUMMARY field types do not have the expected format (numeric, character, etc.) and may need to be checked:\n")
cat("\n")
mismatched.summary.fields=Summary.fieldtypes[which(Summary.fieldtypes$data.field.type!= trimws(as.character(Summary.fieldtypes$expected.field.type))),c(1:3)]
row.names(mismatched.summary.fields)=NULL
print(mismatched.summary.fields)
cat("\n")

Observations.fieldtypes=data.frame(names(Observations))
names(Observations.fieldtypes)="field.names"
Observations.fieldtypes$data.field.type=unlist(lapply(Observations, class))

Observations.fieldtypes = merge(Observations.fieldtypes,exp.observation.names,by.x="field.names",by.y="fieldnames",all.x=TRUE,sort=FALSE)

cat("The following OBSERVATION field types do not have the expected format (numeric, character, etc.) and may need to be checked:\n")
cat("\n")
mismatched.observation.fields=Observations.fieldtypes[which(Observations.fieldtypes$data.field.type!= trimws(as.character(Observations.fieldtypes$expected.field.type))),c(1:3)]
row.names(mismatched.observation.fields)=NULL
print(mismatched.observation.fields)
cat("\n")

Track.fieldtypes=data.frame(names(Track))
names(Track.fieldtypes)="field.names"
Track.fieldtypes$data.field.type=unlist(lapply(Track, class))

Track.fieldtypes = merge(Track.fieldtypes,exp.track.names,by.x="field.names",by.y="fieldnames",all.x=TRUE,sort=FALSE)

cat("The following TRACK field types do not have the expected format (numeric, character, etc.) and may need to be checked:\n")
cat("\n")
mismatched.track.fields=Track.fieldtypes[which(Track.fieldtypes$data.field.type!= trimws(as.character(Track.fieldtypes$expected.field.type))),c(1:3)]
row.names(mismatched.track.fields)=NULL
print(mismatched.track.fields)
cat("\n")
cat("############################\n")
############################################

############################################
## EXTRACT UNIQUE VALUES FOR KEY FIELDS AND OUTPUT TO TEXT FILE
## remove image ID, latitude, longitude, object.height.estimate, 
exclude.list=c("Image.ID","Object.ID.Confidence","No.individuals","Latitude","Longitude","Object.height.estimate","Object.height.min","Object.height.max","height.method","body.length","wing.span","association","association.note", "notes","Object.heading") 

unique.observations=lapply(Observations[,-(which(names(Observations) %in% exclude.list))],unique)

sink(paste(data.folder,"Summary_unique_observations.txt",sep=""),append=FALSE) 
print(paste("Data file: ", data.file.name))
print("Unique records in each field:")
print(unique.observations)#, print) 
sink() 
############################################

############################################
##check to make sure field contents match expected
cat("CHECKING IF FLIGHT HEIGHT HAS BEEN RECORDED FOR BIRDS ON THE SEA:\n")

if(all(is.na(suppressWarnings(as.numeric(Observations$Object.height.estimate[Observations$On.sea=="y"]))))==TRUE){
  cat("...no flight heights entered for birds recorded as 'on.sea'\n")
} else {
  
  on.sea.height.rows = which(is.na((suppressWarnings(as.numeric(Observations$Object.height.estimate[Observations$On.sea=="y"]))))=="FALSE")
  cat(paste("There are",length(on.sea.height.rows),"occasions when flight height was entered for a bird on the sea.\n"))
  cat("check these rows of the Observations data:\n")
  print(on.sea.height.rows)
  cat("\n")
  cat("############################\n")                                            
}
############################################

############################################
## EXTRACT UNIQUE VALUES FOR KEY FIELDS AND OUTPUT TO TEXT FILE
## remove image ID, latitude, longitude, object.height.estimate, 
exclude.list=c("Image.ID","Object.ID.Confidence","No.individuals","Latitude","Longitude","Object.height.estimate","Object.height.min","Object.height.max","height.method","body.length","wing.span","association","association.note", "notes","Object.heading") 

unique.observations=lapply(Observations[,-(which(names(Observations) %in% exclude.list))],unique)

sink(paste(data.folder,"Summary_unique_observations.csv",sep=""),append=FALSE) 
print(paste("Data file: ", data.file.name))
print("Unique records in each field:")
print(unique.observations)#, print) 
sink() 

############################################





#############################################
## Saves first 5 and last 5 rows to csv files for inspepction
## Only operates if save.top.tail=1
############################################

if(save.top.tail==1){
  #### display the first and last rows of each table in the R console.
  t.display=1
  cat("########################################")
  cat("\n")
  while(t.display<=length(Sheet.Names)){
    temp.object=get(Sheet.Names[t.display])
    cat(paste("Data object ",t.display, ": ",Sheet.Names[t.display],sep=""))
    cat("\n")
    cat(paste("Number of columns =",dim(temp.object)[2]))
    cat("\n")
    cat(paste("Number of rows =",dim(temp.object)[1]))
    cat("\n")
    if(dim(temp.object)[1]>=10){
      cat("First 5 rows: ")
      cat("\n")
      print(head(temp.object))
      cat("\n")
      cat("Last 5 rows:")
      cat("\n")
      print(tail(temp.object))
    } else {
      cat(paste(Sheet.Names[t.display],": ",sep=""))
      cat("\n")
      print(temp.object)
    }
    cat("\n")
    cat("########################################")
    cat("\n")
    t.display=t.display+1
  }
  
  ## write summary output to text files
  t.output=1
  cat("saving summary info (first 5 rows and last 5 rows) to text files with sheet names in the same folder location...")
  while(t.output<=length(Sheet.Names)){
    temp.object=get(Sheet.Names[t.output])
    temp.object2=rbind(head(temp.object),tail(temp.object))
    write.table(temp.object2,file=paste(data.folder,"Summary_info_",Sheet.Names[t.output],".csv",sep=""),quote=FALSE,sep=",",row.names=FALSE)
    
    t.output=t.output+1
  }
 
}
############################################
############# End of Script ################
############################################