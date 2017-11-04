#install_github("ropensci/datapack")

# This script creates a DataPackage from the HydroCarbon dataset and uploads it to the
# specified member node. The `datapack::addRunProv` function is used to add the provenance relationships
#
# The resolveURI is used to construct the DataONE object URIs. The default is to use the 
# DataONE 'resolve' service URL, but the URL for a test environment can be used instead, for example,.
# "https://cn-dev-2.test.dataone.org/cn/v2/resolve".
# This script version uses the newest R datapack package as of 2017 01 20
library(dataone)
library(datapack)
library(EML)

uploadPkg <- function() {
  
 # INPUTS/OUTPUTS:
 #  evosArchResults.csv --> evosArchAnalysis.Rmd --> evosArchAnalysis.pdf
 # 
 #  evosArchResults.csv --> evosArchPlots.Rmd --> evosArchPlots.pdf
 # 
  
  dp <- new("DataPackage")
  dataDir <- "filesToArchive"
  saveDir <- getwd()
  setwd(dataDir)
  emlFile <- "dataArchEML.xml"
  EMLdoc <- read_eml(emlFile)
  
  # The 'resolveURI' CN MN in 'D1Client' must agree
  # resolveURI <- "https://cn-dev-2.test.dataone.org/cn/v2/resolve"
  # d1c <- D1Client("STAGING2", "urn:node:mnTestKNB")
  #d1c <- D1Client("DEV2", "urn:node:mnDevUCSB1")
  
  # resolveURI <- "https://cn.dataone.org/cn/v2/resolve"
  # d1c <- D1Client("STAGING2", "urn:node:mnTestKNB")
  
  resolveURI <- "https://cn.dataone.org/cn/v2/resolve"
  d1c <- D1Client("PROD", "urn:node:KNB") #real KNB
  
  
  #
  #----- Execution #1
  #
  message("Adding package objects for execution #1...")
  inputs <- list()
  outputs <- list()
  # Create a DataObject to hold the script file
  progObj <- new("DataObject", format="text/plain", filename="evosArchAnalysis.Rmd",  mediaType="text/markdown") 
                 #,suggestedFilename="evosArchAnalysis.Rmd")
  # Add the script DataObject to the DataPackage
  dp <- addMember(dp, progObj)
  EMLdoc <- updateEMLdistURL(EMLdoc, entityName="Data analysis R markdown script", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(progObj)))
  
  doIn <- new("DataObject", format="text/csv", filename="evosArchResults.csv")#, suggestedFilename="evosArchResults.csv")
  dp <- addMember(dp, doIn)
  inputs[[length(inputs)+1]] <- doIn
  EMLdoc <- updateEMLdistURL(EMLdoc, entityName="evosArchResults.csv", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(doIn)))
  
  doOut <- new("DataObject", format="application/pdf", filename="evosArchAnalysis.pdf")#, suggestedFilename="dataAnalysis.pdf")
  dp <- addMember(dp, doOut)
  outputs[[length(outputs)+1]] <- doOut
  EMLdoc <- updateEMLdistURL(EMLdoc, entityName="Data analysis PDF", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(doOut)))
  
  # Now add the provenance relationships for this script, and it's inputs and outputs
  dp <- describeWorkflow(dp, sources=inputs, program=progObj, derivations=outputs) 
  
  #
  #----- Execution #2
  #
  
  message("Adding package objects for execution #2...")
  inputs <- list()
  outputs <- list()
  # Create a DataObject to hold the script file
  progObj <- new("DataObject", format="text/plain", filename="evosArchPlots.Rmd",  mediaType="text/markdown") 
                 #,suggestedFilename="evosArchPlots.Rmd")
  # Add the script DataObject to the DataPackage
  dp <- addMember(dp, progObj)
  EMLdoc <- updateEMLdistURL(EMLdoc, entityName="MS figures R markdown script", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(progObj)))
  
  # Already created a DataObject for the input file, so reuse it.
  inputs[[length(inputs)+1]] <- doIn
  
  doOut <- new("DataObject", format="application/pdf", filename="evosArchPlots.pdf")#, suggestedFilename="ecoinfoPlots.pdf")
  dp <- addMember(dp, doOut)
  outputs[[length(outputs)+1]] <- doOut
  EMLdoc <- updateEMLdistURL(EMLdoc, entityName="MS figures PDF", resolveUrl=sprintf("%s/%s", resolveURI, getIdentifier(doOut)))
  
  # Now add the provenance relationships for this script, and it's inputs and outputs
  dp <- describeWorkflow(dp, sources=inputs, program=progObj, derivations=outputs) 
  
  message("Writing EML document...")
  write_eml(EMLdoc, emlFile)
  message("Done writing EML document.")
  # Read in the newly updated metadata file
  metadataObj <- new("DataObject", format="eml://ecoinformatics.org/eml-2.1.1", filename=emlFile)
  dp <- addMember(dp, metadataObj)
  
  # Associate the metadata object with all objects in the package
  pids <- getIdentifiers(dp)
  for(iPid in 1:length(pids)) {
    thisPid <- pids[[iPid]]
    dp <- insertRelationship(dp, subjectID=getIdentifier(metadataObj), objectIDs=thisPid)
  }
  
  # Upload to DataONE member node
  resourceMapId <- uploadDataPackage(d1c, dp, replicate=TRUE, public=TRUE, quiet=F, resolveURI=resolveURI)
  
  setwd(saveDir)
}

# Update the distribution url in the EML object with the DataONE 
updateEMLdistURL <- function(EMLdoc, entityName, resolveUrl) {
  # Search for the entity among the 'otherEntity' elements
  found <- FALSE
  for (iEntity in 1:length(EMLdoc@dataset@otherEntity@.Data)) {
    thisEntityName <- EMLdoc@dataset@otherEntity@.Data[[iEntity]]@entityName
    if(thisEntityName == entityName) {
      message(sprintf("Updating otherEntity %s in EML\n", thisEntityName))
      EMLdoc@dataset@otherEntity@.Data[[iEntity]]@physical[[1]]@distribution[[1]]@online@url@.Data <- resolveUrl
      found <- TRUE
    }
  }
  if(found) return(EMLdoc)
  # If not already found, search for the entity among the 'dataTable' elements
  for (iEntity in 1:length(EMLdoc@dataset@dataTable@.Data)) {
    thisEntityName <- EMLdoc@dataset@dataTable@.Data[[iEntity]]@entityName
    if(thisEntityName == entityName) {
      message(sprintf("Update dataTable %s in EML\n", thisEntityName))
      EMLdoc@dataset@dataTable@.Data[[iEntity]]@physical[[1]]@distribution[[1]]@online@url@.Data <- resolveUrl
      found <- TRUE
    }
  }
  if(!found) {
    message(sprintf("Warning! Did not find entity name \"%s\"\n", entityName))
  }
  return(EMLdoc)
} 

uploadPkg()
