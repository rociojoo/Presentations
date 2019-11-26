# Adding false DOIs (or identifiers) to the papers that don't have them:

# calling packages that we will need


# path
# path.metadata <- "ProcessedQueries/PapersPerJournalGroup/"
# path.fulltext <- "fulltext/"
# paper.title <- 'all_papers_all.csv'
# path.pmc <- "doi/"
new_doi <- function(data_all ,pm.doi, path.fulltext){
  require(tidyverse)
  require(generator)

# only do this if we haven't done it before to this data:
if (!any(colnames(data_all) == "doi2")){

####### extracting pseudo doi and generating it for those missing:
   
data_all$doi2 <- str_replace_all(data_all$DI,"[[:punct:]]", '')
doi_missing <- which(data_all$doi2 == "" | is.na(data_all$doi2))
data_all$doi2[doi_missing] <- paste0("nodoi",r_phone_numbers(length(doi_missing)))
# format(data_all,trim=FALSE)
data_all$doi2 <- as.factor(data_all$doi2)

#write_csv(data_all,paste0(path.metadata,"all_papers_all-doi.csv"))
# write_csv(data_all,paste0(path.metadata,"all_papers_Journals-2000-2008.csv"))
}

## adding methods info

if (any(colnames(data_all) == "doi2")){
# extracting the names of the files with methods sections:
files_methods <- dir(path.fulltext,pattern="methods.RData")
str_sub(files_methods,-14,-1)

doi_methods <- substring(files_methods,1,nchar(files_methods)-14)
doi_meta <- data_all$DI
doi_transf <- gsub("([[:punct:]])", "_", doi_meta)
pmcid <- substr(pm.doi$PMCID,4,nchar(pm.doi$PMCID))

doi <- sapply(1:length(doi_transf),function(x){
  if (!is.na(doi_transf[x])){
    ind_doi <- grep(doi_transf[x],doi_methods)
    if (length(ind_doi) == 0){
      ind_doi <- grep(doi_meta[x],pm.doi$DOI)
      if (length(ind_doi) == 0){
        doi3 <- NA
      }else{
        doi3 <- pmcid[ind_doi]
      }
    }else{
      doi3 <- doi_transf[x]
    }
  }else{doi3 <- NA}
})

data_all$doi_methods <- doi
}


return(data_all)
}
# out<-new_doi(path.metadata, path.fulltext, path.pmc)
# write_csv(out,paste0(path.metadata,"all_papers_all-do11.csv"))
