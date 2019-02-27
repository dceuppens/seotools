
#autoinstall packages

packages <- c("fuzzywuzzyR","writexl","readxl","tidyr" ,"dplyr", "stringi", "stringr","reshape2","tools","ggplot2")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)
library(urltools)
library(tools)
library(ggplot2)
library(reshape2)
library(fuzzywuzzyR)

# set working directory - replace with your working directory

setwd("~/Documents/compare")

# Internal_all export - original site - replace with full path to site if not in working directory
urls_old_file <- "internal_all_old.xlsx"

# Internal_all export - new site / test site - replace with full path to site if not in working directory

urls_new_file <- "internal_all_new.xlsx"

# Crawl new site based on urls old site - replace with full path to site if not in working directory
#Internal_all export
match_file <- "internal_all_oud_nieuw.xlsx"
# Redirect & Canonical chains report
redirects_file <- "redirect_and_canonical_chains.xlsx"


# If you are using a different domains for production & test site - enter the domain names
old_domain <- "www.currentdomain.com"
test_domain <- "test.domain.com"

# reading the different files into dataframe - 1st row contains useless info & is skipped
# If you get warnings increase the guess_max value

urls_old <-  read_excel(urls_old_file, 
                    sheet = 1, 
                    col_names = TRUE, 
                    na = "",
                    skip=1,
                    guess_max = 10000
                    )

urls_new <-  read_excel(urls_new_file, 
                        sheet = 1, 
                        col_names = TRUE, 
                        na = "",
                        skip=1,
                        guess_max = 10000)

redirects <-  read_excel(redirects_file, 
                          sheet = 1, 
                          col_names = TRUE, 
                          na = "",
                          skip=1,
                          guess_max = 10000
                          )

urls_match <-  read_excel(match_file, 
                          sheet = 1, 
                          col_names = TRUE, 
                          na = "",
                          skip=1,
                          guess_max = 10000
                          )


# Only keep columns that are needed

urls_old <- urls_old[,c("Address","Content","Status Code","Indexability","Indexability Status",	"Title 1",
                        "Title 1 Length","Meta Description 1","Meta Description 1 Length","H1-1","H1-1 length",
                        "Canonical Link Element 1","Size", "Word Count","Text Ratio", "Crawl Depth", "Link Score",
                        "Unique Inlinks", "Unique Outlinks","Unique External Outlinks",	"Response Time",
                        "Redirect URI",	"Redirect Type","GA Sessions", "GA New Users", "GA Bounce Rate",
                        "GA Page Views Per Session",	"GA Avg Session Duration", "GA Goal Conversion Rate All",	
                        "GA Goal Completions All",	"GA Goal Value All","GA Page Value")]
urls_new <- urls_new[,c("Address","Content","Status Code","Indexability","Indexability Status",	"Title 1",
                        "Title 1 Length","Meta Description 1","Meta Description 1 Length","H1-1","H1-1 length",
                        "Canonical Link Element 1",	"Size",	"Word Count","Text Ratio", "Crawl Depth", "Link Score",
                        "Unique Inlinks",	"Unique Outlinks","Unique External Outlinks",	"Response Time",
                        "Redirect URI",	"Redirect Type")]


redirects <- redirects[,c("Address","Final Address","Final Status Code","Number of Redirects","Redirect Loop","Chain Type","Temp Redirect In Chain")]

urls_match <- urls_match[,c("Address","Content","Status Code","Indexability","Indexability Status",	
                            "Title 1","Title 1 Length","Meta Description 1","Meta Description 1 Length","H1-1",
                            "H1-1 length","Canonical Link Element 1",	"Size",	"Word Count","Text Ratio",	
                            "Crawl Depth",	"Link Score",	"Unique Inlinks",	"Unique Outlinks","Unique External Outlinks",	
                            "Response Time","Redirect URI",	"Redirect Type")]

urls_match$`Unique Inlinks`<- 0
urls_match$`Crawl Depth`<- NA
# To make sure we capture the data for all urls from the old domain - even if they are not found using the crawl of the new site we marge the 
# crawl based on the old urls and the 'spider' crawl from the new domain - we only need to check the URLs that have a status 200 
# Other status codes do not contain data on titles, size,...etc and are already included in the redirect chain report

urls_new_all <-rbind(urls_new,urls_match[urls_match$`Status Code`=="200", ])

# Remove duplicates - if duplicates exist the one with the highest number of Inlinks will be selected (by default - when crawling based on lists all
# urls have Inlinks=0)

urls_new_all <- urls_new_all %>% 
  group_by(Address) %>%
  top_n(n = 1, wt = `Unique Inlinks`)

urls_new_all <- as.data.frame(urls_new_all)
#urls_new_all_test <- urls_new_all %>% 
#  group_by(Address) %>% 
#  slice(which.min(`Crawl Depth`))

#urls_new_all_test <- urls_new_all_test %>% 
#  group_by(Address) %>%
#  top_n(n = 1, wt = `Unique Inlinks`)


# Replacing the 'test' domain with the real domain to enable comparing the files

urls_new_all$Address <- gsub(test_domain,old_domain, urls_match$Address)
urls_new_all$`Canonical Link Element 1` <- gsub(test_domain,old_domain, urls_new_all$`Canonical Link Element 1`)

redirects$Address <- gsub(test_domain,old_domain, redirects$Address)
redirects$`Final Address`<- gsub(test_domain,old_domain,redirects$`Final Address` )

# Creating the file with all the URL data comparison
# First - Merging the original crawl with the redirects
# Adds the number of redirects & the final address + status.code
# The join is made based on the URL (=Address) - contains all the data from the original crawl & only the data from the redirects 
# that matches the original address

comparison <- merge(urls_old, redirects, by = c("Address"), all.x=TRUE)

# Replacing the columnames 


colnames(comparison)[colnames(comparison)=="Address"] <- "Original Address"
colnames(comparison)[colnames(comparison)=="Content"] <- "Original Content type"
colnames(comparison)[colnames(comparison)=="Status Code"] <- "Original Status Code"
colnames(comparison)[colnames(comparison)=="Indexability"] <- "Original Indexability"
colnames(comparison)[colnames(comparison)=="Indexability Status"] <- "Original Indexability Status"
colnames(comparison)[colnames(comparison)=="Title 1"] <- "Original Title"
colnames(comparison)[colnames(comparison)=="Title 1 Length"] <- "Original Title Length"
colnames(comparison)[colnames(comparison)=="Meta Description 1"] <- "Original Meta Description"
colnames(comparison)[colnames(comparison)=="Meta Description 1 Length"] <- "Original Meta Description Length"
colnames(comparison)[colnames(comparison)=="H1-1"] <- "Original H1"
colnames(comparison)[colnames(comparison)=="H1-1 length"] <- "Original H1 length"
colnames(comparison)[colnames(comparison)=="Canonical Link Element 1"] <- "Original Canonical URL"
colnames(comparison)[colnames(comparison)=="Size"] <- "Original Size"
colnames(comparison)[colnames(comparison)=="Word Count"] <- "Original Wordcount"
colnames(comparison)[colnames(comparison)=="Text Ratio"] <- "Original Text Ratio"
colnames(comparison)[colnames(comparison)=="Crawl Depth"] <- "Original Crawl Depth"
colnames(comparison)[colnames(comparison)=="Link Score"] <- "Original Link Score"
colnames(comparison)[colnames(comparison)=="Unique Inlinks"] <- "Original Unique Inlinks"
colnames(comparison)[colnames(comparison)=="Unique Outlinks"] <- "Original Unique Outlinks"
colnames(comparison)[colnames(comparison)=="Unique External Outlinks"] <- "Original Unique External links"
colnames(comparison)[colnames(comparison)=="Response Time"] <- "Original Response Time"
colnames(comparison)[colnames(comparison)=="Redirect URI"] <- "Original Redirect URI"
colnames(comparison)[colnames(comparison)=="Redirect Type"] <- "Original Redirect Type"

# This is faster :)

colnames(urls_new_all) <- paste("New", colnames(urls_new_all), sep = " ")

# Adding the data from the new crawl based on the 'final address' (after redirects) and 
# the address of the new crawl. 
# Includes all the data from the crawl of the original site & the crawl of the new site


comparison <- merge (comparison, urls_new_all, by.x=c("Final Address"), by.y=c("New Address"),all =TRUE)

# writing the full table to disk
write_xlsx(comparison,"comparison.xlsx")


# Defining standardized Crawl Dept - replace >10 by 10+
comparison$OriginalLevel <- comparison$`Original Crawl Depth`
comparison$OriginalLevel[comparison$OriginalLevel >= 10] <- "10+"

comparison$NewLevel <- comparison$`New Crawl Depth`
comparison$NewLevel[comparison$NewLevel >= 10] <- "10+"

# Only consider HTML type of urls
urls_html_original <- comparison[grep("html", comparison$`Original Content type`), ]
urls_html_new <- comparison[grep("html", comparison$`New Content`), ]

# Only consider Indexable 
urls_html_original <- urls_html_original[urls_html_original$`Original Indexability`=="Indexable", ]
urls_html_new <- urls_html_new[which(urls_html_new$`New Indexability`=="Indexable" & !is.na(urls_html_new$`New Indexability`) & (urls_html_new$`Original Indexability` !="Non-Indexable" | is.na(urls_html_new$`Original Indexability`))), ]


urls_html_new$NewLevel[is.na(urls_html_new$NewLevel)] <- "Orphan"
# Dedup urls 

urls_html_new <- urls_html_new %>%
  group_by(`Final Address`) %>%
  slice(1:1)
  
urls_html_new <- as.data.frame(urls_html_new)
# Count the number of urls per level 
original_cat_level <- group_by(urls_html_original,OriginalLevel) %>%
  summarise(count = n())

new_cat_level <- group_by(urls_html_new,NewLevel) %>%
  summarise(count = n())

# make sure all possible values ( 0->10+) exist
original_level = data.frame(ID="Original",OriginalLevel=0:11)
original_level$OriginalLevel[11]<- "10+"
original_level$OriginalLevel[12]<- "Orphan"
new_level = data.frame(ID="New",NewLevel=0:11)
new_level$NewLevel[11]<- "10+"
new_level$NewLevel[12]<- "Orphan"
new_cat_level <- merge(new_level, new_cat_level, by = c("NewLevel"), all =TRUE)
original_cat_level <- merge(original_level, original_cat_level, by = c("OriginalLevel"), all =TRUE)
colnames(original_cat_level)[colnames(original_cat_level)=="OriginalLevel"] <- "Level"
colnames(new_cat_level)[colnames(new_cat_level)=="NewLevel"] <- "Level"
cat_level <- rbind(new_cat_level,original_cat_level)

# reorganiseer data & save in xlsx
cat_level_cast <- dcast(cat_level, ID ~ Level, value.var = "count")
                                                           
cat_level_cast <- cat_level_cast[,c(1:3,5:12,4,13)]

write_xlsx(cat_level_cast,"crawl_depth_old_new.xlsx")

# Function to compare 2 strings - based on common words - part of Fuzzywuzzy
textcompare <- function(s1, s2){
  init = SequenceMatcher$new(string1 = s1, string2 = s2)
  similarity <- init$ratio()
  
  return(similarity)
}


# Select only HTML pages 
collist = c("New Content","Original Content type")
selection <- apply(comparison[,collist],1,function(row) length(grep("html",row))>0)


# Subset containing only H1 & URL
H1 <- comparison[selection,c("Original Address","Final Address","Original H1","New H1-1","Original H1 length","New H1-1 length","New Content","Original Content type")]
H1 <- H1[!is.na(H1$`Original Address`), ]

# Compare old & new H1
H1$similarity <- apply(H1, 1, function(x) textcompare(x[3],x[4]))


# Subset containing only Title & URL
Titles <- comparison[selection,c("Original Address","Final Address","Original Title","New Title 1","Original Title Length","New Title 1 Length","New Content","Original Content type")]
Titles <- Titles[!is.na(Titles$`Original Address`), ]
# Compare old & new Title
Titles$similarity <- apply(Titles, 1, function(x) textcompare(x[3],x[4]))


# Compare old & new Title
H1$similarity <- apply(H1, 1, function(x) textcompare(x[3],x[4]))
                                
test <- urls_html_new[is.na(urls_html_new$`NewLevel`), ]
s1 = 'We kunnen langer leven met minder kwalen, volgens verouderings-onderzoeker Andrea Maier. De truc is om spierballen te kweken.'
s2 = ' Dit is absoluut niet relevant'
init = SequenceMatcher$new(string1 = s1, string2 = s2)
init$ratio()
