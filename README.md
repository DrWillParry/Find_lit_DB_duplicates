# Find duplicate entries in a literature database extract using R by (fuzzy) matching on Title

This script (available in [find_duplicates.R](find_duplicates.R)) can be used to find titles in an extract from a literature database (such as PubMed) and identify any entries that are either duplicated exactly (i.e. the same title is included more than once) or likely to be duplicated (the match is not exact, but close). In the example file ([csv-longtermco-set.csv](csv-longtermco-set.csv)), there is one exact match and two inexact matches (i.e. one duplicated paper, one patient comment and one erratum).

The results of the script can then be used to manually check and clean database extracts when conducting systematic reviews.

```r
#Author: Dr Will Parry 
#Web: http://willparry.net

#Load packages
pkgs <- c('data.table', 'purrr', 'stringr', 'stringdist')
sapply(pkgs, require, character.only = TRUE)

#Read database extract
db_extract <- 'csv-longtermco-set.csv' #example extract from pubmed
#NB: must have a Title field on which to match

#Read extract into data table and add a string for title matching
db_table <- fread(db_extract) %>%
  .[, match_string := toupper(str_remove_all(Title, '[:punct:]| '))]
#string is made from Title field, made upper case, with all punctuation and spaces removed

#Create table of 'distances' between pairs of match_string values using {stringdist} package
dist_table <- db_table[['match_string']] %>% #using just the vector of match_strings 
  combn(2) %>% t() %>% #get all combinations and transpose result to long format
  data.table() %>% #make into a data table
  setnames(c('V1', 'V2'), c('title1', 'title2')) %>% #rename columns
  .[, dist := stringdist(title1, title2)] %>% #add string distance metric
  .[order(dist)] #order in increasing distance (top of table will be most similar)

View(head(dist_table)) #view the most similar results

#Distance of zero is an exact match
#In the example, there is:
# - one exact match (duplicated paper)
# - one patient comment on a listed paper
# - one erratum for a listed paper

```
