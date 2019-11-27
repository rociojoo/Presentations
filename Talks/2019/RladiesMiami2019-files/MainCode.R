
# LIBRARIES
# devtools::install_github('embruna/refsplitr')
library(refsplitr)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm) 
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load_gh("trinker/textstem")
library(textstem) # lemmatizing # to install textstem, error when installing statnet.common because my R version is 3.4, so had to work with 4.1.4 statnet.common
library(topicmodels)
library(gridExtra)
library(grid)
library(parallel)
library(RColorBrewer)
library(ggwordcloud)

################################################################
# IMPORT QUERY RESULTS USING REFSPLITR PACKAGE
# This package imports and manipulates WoK references. 
# It can bulk import our querries for us.
# Read in the references directory and combine into one file

references <- references_read('Data/Query-Results-WoK',dir=T, include_all=T)
head(references)
write.csv(references, 'Data/references_raw.csv',row.names=F)

#######################################################################

# CLEAN QUERY RESULTS
# This largely is a second pass of the same query we used on the Web of Knowledge website, to catch mistakes and lower the amount of incorrect matches
# We’re going to use the clearing queries function, that cleans querries and runs some stats. If you want to run this on different years all you have to do is do a subset querry and change the name to the specific years. 
# We will use the reference output from references_reads().

# path.fulltext: Path to the folder with the full text downloads
# path.pmc: Path to the doi folder
# path.save.stats:  place to store a summary stats text file
# path.save.reviews: place to store a file of papers to review
# path.save.query: a place to save the final output
# name: of the file to be saved

path <- "./Data/references_raw.csv"
path.fulltext <- "fulltext/"
path.pmc <- "./Data/doi/"
path.save.reviews <- path.save.query <- path.save.stats <- './Data/' # place to store a file of papers to review
name <- 'all'

source('CleaningQueries.R')
source('FalseDOIs.R')
cleaning.query(path,name,path.save.reviews,path.save.query,path.save.stats, path.pmc, path.fulltext)

#############################################################
# PRE-PROCESSING ABSTRACTS FOR TOPIC ANALYSIS 

# PATH
path_dict_tools <- "./Data/AuxiliaryTextMining/"

# calling auxiliary functions
source("./Americanizing.R")
source("./cleaning_words_abstract.R")

original_data <- read.csv("./Data/all_simple.csv", stringsAsFactors = FALSE)
 
# 1.Filter columns of interest
test <- original_data %>% 
  mutate(paper = doi2) %>% 
  select(paper, abstract) 
# now using functions to clean abstracts:
df_words <- cleaning_words_abstract(test, path_dict_tools)

# let's see how many of each there are and 
# filter out the ones that only happen once in the corpus
word_freq <- df_words %>% count(word_am_lem)
word_freq <- word_freq %>% mutate(prop = n/sum(n))

terms_extract <- data.frame(word_am_lem = 
                              word_freq$word_am_lem[which(word_freq$prop < round(min(word_freq$prop),10) + 10^{-10})],
                            stringsAsFactors = FALSE)
df_words <- df_words %>% 
  anti_join(terms_extract)

# Select only columns of interest
final_data <- df_words %>% 
  select(paper, word_am_lem)
names(final_data) <- c("paper","word")

write.csv(x = final_data, file = "./Data/cleaned_filtered_TidyData_TopicModeling.csv", row.names = FALSE)

#############################################################
# TOPIC MODELING

# final_data <- read.csv("./Data/cleaned_filtered_TidyData_TopicModeling.csv", stringsAsFactors = FALSE)

# Shaping the data to use the topicmodel package
datosFreq <- final_data %>%
  # Obtain word frequencies
  count(paper, word) 
# Document Term Matrix
datos_dtm <- datosFreq %>%
  # Specify the token
  cast_dtm(document = paper, term = word, value = n)

# arguments for LDA
N_topics <- 10
alpha_par <- 1 # 0.25
method_par <- "VEM"

LDAModel <- LDA(x = datos_dtm, k = N_topics, method = method_par, 
            control = list(alpha = alpha_par)) #, delta= 0.1))
saveRDS(LDAModel, file=paste0('./Data/LDAModel',N_topics,
                               '_alpha_',alpha_par,"_method_",method_par,'.rds'))


#######################################################
# ANALYZING TOPIC MODELING RESULTS

path_plots <- "./Plots/" # where we'll save the wordcloud file

# LDAModel <- readRDS(file = paste0("./Data/LDAModel",N_topics,"_alpha_",alpha_par,"_method_",method_par,".rds"))

# Topic as a mixture of words
# beta: the probability of that term being generated from that topic.
papers_beta <- tidytext::tidy(LDAModel, matrix = "beta")
# papers_beta
head(papers_beta)

# wordcloud function
word_clouds_ggsave <- function(data, values_thresh, indiv_angle = FALSE, color_pal = "D", max_size_area = 30, 
                               height_file_par = 26, width_file_par = 26, path_plot, filename, 
                               rm_outside_par = FALSE, eccentricity_par = 1){
  
  # data : df with words (as term column) and frequencies (as beta column) by topic (as topic column)
  # values_tresh : lower limit for frequencies of words
  # indiv_angle : TRUE if each word would have a different angle which could be (-90, -45, 0, 45, 90). 
  #               FALSE if every word is horizontal
  # color_pal : from viridis, check viridis help
  # max_size_area : to plot each wordcloud. Depends on number of words. If too large, there could be a lot of
  #                 space between wordclouds. If too small, some words may not appear in the wordcloud
  # height_file_par and width_file_par : height and width of file where we save the plots (in inches)
  # filename : for the pdf with wordclouds
  # eccentricity_par : proportion of horizontal display of wordcloud (respect to vertical). 
  #                     Default 1 (sphere or square like shape)
  # rm_outside_par : removes text that would not be fitted. TRUE to avoid overlap. But try not to loose text. Default FALSE.
  
  topic_sample <- data %>% filter(beta > values_thresh) #%>%  select(term,beta)
  
  if (indiv_angle == TRUE){
    table_topic <- topic_sample %>%
      mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 2, 4, 2, 1)))
  }else{
    table_topic <- topic_sample %>%
      mutate(angle = rep(0,nrow(topic_sample)))
  }
  # 
  color_pallete <- c('deepskyblue','dodgerblue4')
  color_pallete <- c(low='#ffffcc',high='#253494')
 
  plot_w <- ggplot(table_topic, aes(label = term, size = beta, angle = angle, 
                                    color = beta)) +
    geom_text_wordcloud_area(rm_outside = rm_outside_par, eccentricity = eccentricity_par,grid_margin = 0) + #area_corr_power = 1,
    scale_size_area(max_size = max_size_area) +
    theme_bw() +scale_colour_gradientn(colors=c('#253494','palegoldenrod','orangered'),values=c(0,.25,1))+
    facet_wrap(~topic, scales = "free",shrink = F) +
    theme(strip.text = element_text(size = 45),
          plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
          strip.background = element_rect(fill = 'white'))
   
  ggsave(plot=plot_w,filename=paste0(path_plot, filename), height=height_file_par, width = width_file_par, units = "in")
  
}

filename <- paste0("wordcloud_topics_",N_topics,".pdf")

word_clouds_ggsave(data = papers_beta, values_thresh = 0.003, indiv_angle = FALSE, color_pal = color_pal, max_size_area = 30, 
                   height_file_par = 26, width_file_par = 26, path_plot = path_plots, filename = filename,
                   rm_outside_par = FALSE, eccentricity_par = 1)


