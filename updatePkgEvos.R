## Code from Jeanette to update packages using arcticdatautils

library(devtools)
library(dataone)
library(datapack)
library(EML)
library(XML)
#devtools::install_github("nceas/arcticdatautils")
library(arcticdatautils)
#devtools::install_github("nceas/datamgmt")
library(datamgmt)



#options(dataone_test_token = "...")

# cn <- dataone::CNode("STAGING2")
# mn <- dataone::getMNode(cn, "urn:node:mnTestKNB")

cn <- dataone::CNode("PROD")
mn <- dataone::getMNode(cn, "urn:node:KNB")

### Update package

# get the package you want to update into your R envt (use resource map id here)
rm_pid <- "resource_map_doi:10.5063/F1XS5SKS"


pkg <- arcticdatautils::get_package(mn,
                                    rm_pid,
                                    file_names = TRUE)


### update objects

#1 analysis RMD

anlRmdPath<-"evosArchAnalysis.Rmd"

newID1 <- update_object(mn,
                             pid <- pkg$data[2],
                             path <- anlRmdPath,
                             format_id = "text/plain",
                             new_pid = NULL,
                             sid = NULL)

physicalOE3 <- arcticdatautils::pid_to_eml_physical(mn, newID1)

#2 Plots RMD

pltRmdPath<-"filesToArchive/evosArchPlots.Rmd"
newID2 <- update_object(mn,
                             pid <- pkg$data[1],
                             path <- pltRmdPath,
                             format_id = "text/plain",
                             new_pid = NULL,
                             sid = NULL)

physicalOE1 <- arcticdatautils::pid_to_eml_physical(mn, newID2)

#3 evosArch data
evosDatPath<-"filesToArchive/evosArchResults.csv"

newID3 <- update_object(mn,
                             pid <- pkg$data[3],
                             path <- evosDatPath,
                             format_id = "text/csv",
                             new_pid = NULL,
                             sid = NULL)

physicalDT1 <- arcticdatautils::pid_to_eml_physical(mn, newID3)
#eml@dataset@dataTable[[1]]@physical<-physicalDT1[[1]]

#4 analysis pdf

anlPdfPath<-"filesToArchive/evosArchAnalysis.pdf"

newID4 <- update_object(mn,
                             pid <- pkg$data[5],
                             path <- anlPdfPath,
                             format_id = "application/pdf",
                             new_pid = NULL,
                             sid = NULL)
physicalOE4 <- arcticdatautils::pid_to_eml_physical(mn, newID4)


#5 plots pdf

pltPdfPath<-"filesToArchive/evosArchPlots.pdf"

newID5 <- update_object(mn,
                             pid <- pkg$data[4],
                             path <- pltPdfPath,
                             format_id = "application/pdf",
                             new_pid = NULL,
                             sid = NULL)
physicalOE2 <- arcticdatautils::pid_to_eml_physical(mn, newID5)

################ STOP... #########################################################################################
##################################################################################################################
#### manually update the urls in the physical of the metadata, then use saved EML for the publish_update call: ###


# arcticdatautils::publish_update(mn = mn,
#                                 resource_map_pid = rm_pid,
#                                 metadata_pid = pkg$metadata[1],
#                                 metadata_path = "filesToArchive/dataArchEML.xml",
#                                 data_pids = c(newID1,newID2,newID3,newID4,newID5),
#                                 check_first = T,
#                                 use_doi = T)
