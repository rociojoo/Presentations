#devtools::install_github('embruna/refsplitr')
library(refsplitr)
references <- references_read('Data/Query-Results-WoK',dir=T, include_all=T)
write.csv(references, 'Data/ProcessedQueries/References/references_raw.csv',row.names=F)