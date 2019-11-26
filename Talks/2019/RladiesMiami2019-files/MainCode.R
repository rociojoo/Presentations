# devtools::install_github('embruna/refsplitr')
################################################################
# Import the querries using the refsplittr package
# This package imports and manipulates WoK references. 
# It can bulk import our querries for us.
# Read in the references directory and combine into one file

library(refsplitr)
references <- references_read('Data/Query-Results-WoK',dir=T, include_all=T)
write.csv(references, 'Data/references_raw.csv',row.names=F)

#######################################################################

# Re-clean our querries
# This largely is a second pass of the same querry we used on the Web of Knowledge website, to catch mistakes and lower the amount of incorrect matches
# We’re going to use the clearing querries function, that cleans querries and runs some stats. If you want to run this on different years all you have to do is do a subset querry and change the name to the specific years. 
# We will use the reference output from references_reads().

# path.fulltext: Path to the folder with the full text downloads
# path.pmc: Path to the doi folder
# path.save.stats <- ‘ProcessedQueries/Stats/’ # place to store a summary stats text file
# path.save.reviews <- ‘ProcessedQueries/Reviews/’ # place to store a file of papers to review
# path.save.query <- ‘Matt/from_scratch/data_output/’ # a place to save the final output
# name <- ‘cleaned_papers_all_years’

path <- "./Data/references_raw.csv"
path.fulltext <- "fulltext/"
path.pmc <- "./Data/doi/"
path.save.reviews <- path.save.query <- path.save.stats <- './Data/' # place to store a file of papers to review
name <- 'all'

source('CleaningQueries.R')
source('FalseDOIs.R')
cleaning.query(path,name,path.save.reviews,path.save.query,path.save.stats, path.pmc, path.fulltext)

