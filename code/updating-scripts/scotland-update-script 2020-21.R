################################################################################
## SCOTLAND UPDATE TEMPLATE ####################################################
################################################################################
## This is the script used to add new data to the master data file and        ##
## produce the report for Scotland for any year available in the file         ##
################################################################################
## Instructions (detailed instructions are in /docs/technical):
## 1. mandatory input: input (update) the current year and whether or not you
##      are producing a new report, or recompiling an exisitng one.
## 2. If you are producing a new report, add input the required metadata
##      for all the files you've downloaded into data/01-raw. 
## 3. Run through the rest of the script that imports the data, cleans it,  
##      creates an .Rmd file and produces the compiled .pdf report.
################################################################################
################################################################################
##  MANDATORY INPUTS                                                          ##
################################################################################

# which fiscal year do you want to produce a report for?
# NB: the current year is the year in which the fiscal year starts
current.year <- 2020

# if you want to produce a report based on current data - but for a previous year
# set add.new.data to FALSE. Run the rest of the script.
# If you want to add new data from GS, TS and Aberdeen council, then change to 
# TRUE and proceed through the script. Alyways make sure the data you are entering 
# matches the current.year variable. 
add.new.data <- TRUE

# If you have already produced an .Rmd file by running this script, and have 
# made changes to the .Rmd file and just want to recompile it switch to TRUE.
# If you want to produce a fresh copy of the template for this year switch
# to FALSE
recompile.rmd <- FALSE

# number of decimal places in text and tables:
dp.text <- 1
dp.tables <- 1



################################################################################
## MANUAL DATA INPUT ###########################################################
################################################################################
if (add.new.data){
  ## after dowloading the Scotland files into the data/01-raw folder, enter their
  ## metadata here:
  
  # Scottish Local Government Finance Statistics (Income and Expenditure data)####
  ################################################################################
  # title as it will appear in the references:
  sco.i.e.title <- "Scottish Local Government Finance Statistics 2010-21: Local Authority Level Analysis - Net Revenue Expenditure by Subservice"
  
  # url of the file:
  sco.i.e.url <- "https://www.gov.scot/binaries/content/documents/govscot/publications/statistics/2022/03/scottish-local-government-finance-statistics-slgfs-2020-21/documents/scottish-local-government-finance-statistics-slgfs-2020-21-la-level-net-revenue-expenditure-subservice/scottish-local-government-finance-statistics-slgfs-2020-21-la-level-net-revenue-expenditure-subservice/govscot%3Adocument/scottish-local-government-finance-statistics-slgfs-2020-21-la-level-net-revenue-expenditure-subservice.xlsx"
  # year published, as it will appear in the references:
  sco.i.e.year.published <- 2022
  
  ## replace with date of access to data:
  sco.i.e.date.accessed <- "26.04.2023"
  
  # path and name of file where you have saved it:
  sco.i.e.file <- "data/01-raw/orig.sco-20-21.xlsx"
  
  # which sheet has the first LA (Aberdeen city) on it?
  sco.i.e.start.sh <- 4
  
  # which sheet has the last LA (West Lothian) on it?
  sco.i.e.end.sh <- 35
  
  # which cell has the Parking "Gross Expenditure on a Funding Basis"?
  sco.i.e.exp.cell <- "B37"
  
  # which cell has the Parking "Gross Income on a Funding Basis"?
  sco.i.e.inc.cell <- "C37"
  
  # which cell has the Total Roads and Transport "Net revenue Expenditure on a funding basis"?
  sco.i.e.transp.cell <- "D43"
  
  
  # Aberdeen city - separate data source if available? ###########################
  ################################################################################

    # title as it will appear in the references:
  sco.aberdeen.title <- "{Aberdeen City Council Annual Accounts 2020-21}"
  
  # url of the file:
  sco.aberdeen.url <- "https://www.aberdeencity.gov.uk/sites/default/files/2021-09/Aberdeen%20City%20Council%20Audited%20Annual%20Accounts%20202021.pdf"
  
  # year published, as it will appear in the references:
  sco.aberdeen.year.published <- 2021
  
  ## replace with date of access to data:
  sco.aberdeen.date.accessed <- "28.4.2023"
  
  # How much parking Income did Aberdeen City report this year?? (£,000s)  Section 18
  sco.aberdeen.income.total <- 3717
  
  # How much parking Expenditure did Aberdeen City report this year?? (£,000s)  Section 18
  sco.aberdeen.expend.total <- 2814
  
  
  # Transport Scotland (PCN) data  ###############################################
  ################################################################################
  # title as it will appear in the references:   N.B. THIS IS A YEAR BEHIND THE ACCOUNTS!!!
  sco.pdf.title <- "Decriminalised Parking Enforcement: Local Authorites’ Income
and Expenditure: 2020 to 2021"
  
  # url of the file:
  sco.pdf.url <- "https://www.transport.gov.scot/media/51930/decriminalised-parking-enforcement-income-and-expenditure-2020-to-2021-revised-report-19-july-2022.pdf"
  
  # year published, as it will appear in the references:
  sco.pdf.year.published <- 2022
  
  ## replace with date of access to data:
  sco.pdf.date.accessed <- "26.06.2023"
  
  # path and name of file where you have saved it:
  sco.pdf.file <- "data/01-raw/orig.sco-20-21-pcn.pdf"
  
  # which page in the report is the DPE table on?
  sco.pdf.dpe.tab <- 4
  
  # which page in the report is the number of PCNs table on?
  sco.pdf.pcn.tab <- 8
  
  # which page in the report is the income/expenditure table on?
  sco.pdf.i.e.tab <- 10
} 

################################################################################
################################################################################
##                                                                            ##
##     THE REST OF THIS SCRIPT IS TO BE RUN ONLY -- NO MODIFICATIONS!         ##
##                                                                            ##
################################################################################
################################################################################
################################################################################
## LOAD PACKAGES AND DATA ######################################################
################################################################################
source("code/do-not-touch-scripts/functions.R")
if (!require("pacman")) install.packages("pacman")
#tabulizer removed as no longer available on CRAN downloaded from Github
pacman::p_load( tidyr,dplyr,tibble,RefManageR,googlesheets4,knitr,here,kableExtra,showtext,sf,viridis,tabulizer,bookdown,readxl,tcltk2)

options(stringsAsFactors = FALSE)

# load existing master file, bib.master and name lookup table
master <- readRDS("data/03-processed/master.rds")
master%>%   mutate(auth.name= stringr::str_replace(auth.name, " & "," and " )) ->master
bib.master <- readRDS("data/03-processed/bib.master.rds")
orig.sco.name.lookup <- readRDS("data/01-raw/orig.sco.name.lookup.rds")

# new report name
report.name <- paste0("scotland-report-", current.year, "-",
                      current.year - 1999)

if (add.new.data){  
  ################################################################################
  ## AUTOMATIC DATA IMPORT AND CLEANING
  ################################################################################
  # loop through each sheet of the excel file to extract the income/expenditure data
  file.name = sco.i.e.file
   FunScotlandLoopIE(year = current.year,
                    file.name = sco.i.e.file,
                    start.sh =  sco.i.e.start.sh,
                    end.sh =  sco.i.e.end.sh,
                    exp.cell =  sco.i.e.exp.cell,
                    inc.cell =  sco.i.e.inc.cell,
                    transp.cell = sco.i.e.transp.cell) -> scotland.i.e.
  
  aberdeen <- data.frame(auth.name = "Aberdeen City",
                         year = current.year,
                         income.total = sco.aberdeen.income.total,
                         expend.total =  sco.aberdeen.expend.total)
  
  # slot in manual aberdeen city data
  scotland.i.e. %>% 
    filter(auth.name == "Aberdeen City") %>% 
    select(auth.name, transport.total, year) %>% 
    full_join(aberdeen) -> full.aberdeen
  
  # merge back with scotland i.e.
  scotland.i.e. %>% 
    filter(auth.name != "Aberdeen City") %>%
    bind_rows(full.aberdeen) -> scotland.i.e.
  
  # extract the dpe table from the pdf
  #This has changed format in 2020-21 - DPE Table now in new columns
  #N.B. Tabulizer has been removed from CRAN - have a look at Github
  
  scotland.dpe.1 <- extract_tables(sco.pdf.file, pages = sco.pdf.dpe.tab)[[1]]
  scotland.dpe.2 <- extract_tables(sco.pdf.file, pages = sco.pdf.dpe.tab+1)[[1]]
  
  scotland.dpe <- rbind(scotland.dpe.1,scotland.dpe.2)
  
  # clean DPE type table
  scotland.dpe <- FunScotlandDPE2020(scotland.dpe, current.year)
  
  scotland.dpe %>% mutate(auth.name=replace(auth.name,auth.name=="Shetland","Shetland Islands"))%>% 
                    mutate(auth.name=replace(auth.name,auth.name=="Comhairle nan Eilean Sar","Na h-Eileanan an Iar"))->scotland.dpe
  
  # extract PCN type table
  scotland.pcn <- extract_tables(sco.pdf.file,
                                 pages = sco.pdf.pcn.tab,
                                 output = "data.frame")[[1]]
  
  #Check for empty rows
  var<-paste0("X",current.year,".",current.year-1999)
 scotland.pcn %>% filter( get(var) == "")   ->scot.pcn.empty
    if(current.year>2020 & nrow(scot.pcn.empty)!=0){stop("Blank Rows from PDF - Check Data")}
  
#Fix for 2018 and 2020 Data 
 names(scotland.pcn)[1]<-"Local.authority"
 if((current.year==2018 | current.year==2020) & nrow(scot.pcn.empty)!=0){
   scotland.pcn %>% filter(get(var) != "") ->scotland.pcn 
  scotland.pcn$Local.authority<-gsub("^$|^ $","East Dunbartonshire", scotland.pcn$Local.authority)
 }
 
  
  # clean PCN table
  scotland.pcn <- FunScotlandPCN(scotland.pcn, current.year)
  
  # extract TfS i.e. type table directly into a data.frame
  scotland.tfs.i.e <- extract_tables(sco.pdf.file,
                                     pages = sco.pdf.i.e.tab,
                                     output = "data.frame",row.names=NULL)[[1]]
  
  # clean TFS income expenditure table
    scotland.tfs.i.e[,7:8]<-NULL
  names(scotland.tfs.i.e)<-c("Local.authority","PCN.income","Pay.&.display/other.income","Total.income","Expenditure","Annual.balance")
  scotland.tfs.i.e <- FunScotlandTFSIE(scotland.tfs.i.e, current.year)
  
  # join all 3 tables from the pdf
  scotland.pdf <- full_join(full_join(scotland.dpe,
                                      scotland.pcn,by = c("auth.name", "year")),
                            scotland.tfs.i.e,  by = c("auth.name", "year"))
  
  # now merge income exp data from the Excel files with the pdf data
  update <- full_join(scotland.i.e., scotland.pdf)
  
  # add Scotland data and calculate surplus
  update %>%
    mutate(country = "Scotland",
           auth.type = "LA",
           surplus.total = income.total - expend.total) -> master.update
  
#Take some extra lines out - not sure wherte they come from (Tim)
#  master.update %>% filter(!auth.name%like% "DPE") %>% filter(auth.name!="")  %>% filter(auth.name!="Dunbartonshire") %>% filter(auth.name!="Eilean Siar") -> master.update
 
#Some old names cropping up - just get rid of these (Tim)   
#master %>% filter(country=="Scotland") %>% select(auth.name) %>% distinct() ->scot.names
#master.update %>% filter(auth.name %in% scot.names)->master.update
  
   # double check the update is OK: 
  if (nrow(master.update) != 32) {
    paste("Something is wrong. The update should have 32 rows, but it has",
          nrow(master.update), "instead.")} else {
            "Everything checks out, the update has 32 rows"}
  
  ################################################################################
  ## Add (or overwrite) new rows to master #######################################
  ################################################################################
  
  # add update - if that year already exists, it will be overwritten!!!
  if (exists("master.update")){
    master %>%
      anti_join(master.update, by = c("country", "auth.name", "year")) %>%
      bind_rows(master.update) -> master}
  
  
  # save updated datafile to master
  saveRDS(master, "data/03-processed/master.rds")
  write.csv(master, "outputs/csv-tables/master.csv")
  
  ## IMPORT AND CLEAN RPI DATA ###################################################
  # if RPI file doesn't exist, or if it doesn't have today's date, download it again. 
  if (!file.exists("data/01-raw/rpi.csv") | 
      format(file.mtime("data/01-raw/rpi.csv"), "%d.%m.%Y") != 
      format(Sys.Date(), "%d.%m.%Y")) {
    url <- paste0("https://docs.google.com/spreadsheets/d/",
                  "1joRISS6YV3eusMNgFPaHfrtsiPN1skM-mpEZJqD3FUg/",
                  "edit?pli=1#gid=1538913200")
    
    rpi<-read_sheet(url, sheet = "Inflation, last 10 years")
    names(rpi)[2]<-"Cost.of.Living"
    write.csv(rpi,"data/01-raw/rpi.csv")
  }
  
  # update RPI data acces date and year of publication in the bibliography
  bib.master %>%
    mutate(urldate = ifelse(content == "rpi",
                            as.character(format(Sys.Date(), "%d.%m.%Y")), urldate),
           year = ifelse(content == "rpi",
                         as.numeric(format(Sys.Date(), "%Y")), year)) ->
    bib.master
  ################################################################################
  # add new files to bibliography master #########################################
  ################################################################################
  # add new rows to bibliography #################################################
  # add i.e. source:
    sco.i.e.bib <- data.frame(fiscyear = current.year,
                            url = sco.i.e.url,
                            country = "Scotland",
                            content = "i.e",
                            bibtype = "misc",
                            year = sco.i.e.year.published,
                            author = "{Scottish Government}",
                            urldate = sco.i.e.date.accessed,
                            title = sco.i.e.title,
                            key = paste0("Scotland.i.e.", current.year))
  
  # add it to bib.master (overwriting if already exists)
  bib.master %>%
    anti_join(sco.i.e.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(sco.i.e.bib) -> bib.master
  
  # add aberdeen data source:
  
#Not sure why Aberdeen is seperate for now - reports are a mess so have left out   
  sco.aberdeen.bib <- data.frame(fiscyear = current.year,
                                 url = sco.aberdeen.url,
                                 country = "Scotland",
                                 content = "i.e.",
                                 bibtype = "misc",
                                 year = sco.aberdeen.year.published,
                                 author = "{Aberdeen City Council}",
                                 urldate = sco.aberdeen.date.accessed,
                                 title = sco.aberdeen.title,
                                 key = paste0("Scotland.abd.", current.year))
  
  
  # if exists add it to bib.master (overwriting if already exists)
 bib.master %>%
 anti_join(sco.aberdeen.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(sco.aberdeen.bib) -> bib.master
  
  # add pdf source:
  sco.pdf.bib <- data.frame(fiscyear = current.year,
                            url = sco.pdf.url,
                            country = "Scotland",
                            content = "pcn",
                            bibtype = "misc",
                            year = sco.pdf.year.published,
                            author = "{Transport Scotland}",
                            urldate = sco.pdf.date.accessed,
                            title = sco.pdf.title,
                            key = paste0("Scotland.pdf.", current.year))
  
  # add it to bib.master (overwriting if already exists)
  bib.master %>%
    anti_join(sco.pdf.bib, by = c("fiscyear", "country", "content")) %>%
    bind_rows(sco.pdf.bib) -> bib.master
  

  
  # save updated datafile to master
  saveRDS(bib.master, "data/03-processed/bib.master.rds")
}

  # select scotland only bibliograpy #############################################
  # select a bibliography for the scotland report - only the rows needed
  bib.master %>%
    filter(fiscyear > current.year - 5, !content %in% c("budget", "wpl")) %>%
   group_by(country) %>%
  # filter(country %in% c("Wales", "Scotland") | country == "England" & fiscyear == max(fiscyear)) %>%
  filter(country %in% c("GB","Scotland") | country %in% c("England","Wales") & fiscyear == max(fiscyear)) %>%
    mutate(refs = paste0("@", key)) %>%
    column_to_rownames("key") -> bib.scotland

#Get original DPE report reference added
bib.master %>%   filter(country == "Scotland", fiscyear == 2013, content == "pcn") %>% mutate(refs = paste0("@", key))%>%
  column_to_rownames("key")-> orig.DPE.2013
bib.scotland<-rbind(bib.scotland,orig.DPE.2013)
  
  # create bib file
  bib.scotland %>%
    as.BibEntry() %>%
    WriteBib(file = "code/report-rmds/scotland.bib",
             biblatex = FALSE, verbose = FALSE)
  
  # also save the data.frame
   saveRDS(bib.scotland, paste0("data/03-processed/", report.name, "-bib.rds"))



################################################################################
## COMPILE REPORT 
################################################################################
# check if master data is available for current year?

if(nrow(filter(master, country == "Scotland", year == current.year)) == 0) {
  warning("There are no records for the year ", current.year) } else {
    if(nrow(filter(master, country == "Scotland", year == current.year)) !=32) {
      warning("Something has gone wrong. There should be 32 rows for ", 
             current.year, " but there are not. I suggest you revert to a ",
             "previous version of the repository and try again.")} else {
               
               
               if(!recompile.rmd){
                 # create a fresh copy of the scotland report template
                 file.copy("code/report-templates/scotland-report-template.Rmd",
                           paste0("code/report-rmds/", report.name, ".Rmd"),
                           overwrite = TRUE)
                 }
               
               
               # compile the report (but check if file exists first)
               if(recompile.rmd & !file.exists(paste0("code/report-rmds/", 
                                                     report.name, ".Rmd"))){
                 warning("The Rmd file does not exist. Rerun this script with ",
                       "recompile.rmd swithced to FALSE.")} else { 
                         
                         suppressWarnings(rm(params))
                         knitr::opts_chunk$set(
                           warning = TRUE, # show warnings
                           message = TRUE, # show messages
                           error = TRUE, # do not interrupt generation in case of errors,
                           echo = TRUE  # show R code
                         )
                         rmarkdown::render(paste0("code/report-rmds/", report.name, ".Rmd"),
                                           output_file = paste0(report.name, ".pdf"),
                                           output_dir = "outputs/reports",
                                           params = list(current.year = current.year,
                                                         dp.text = dp.text,
                                                         dp.tables = dp.tables))
                         
                         # remove empty folder that the compilation creates
                         unlink(paste0("outputs/reports/", report.name, "_files"), recursive=TRUE)
                         
                         # remove log file (comment this out if there are issues and look at the log
                         # file for clues?
                         suppressWarnings(file.remove(paste0("code/report-rmds/", report.name, ".log")))
                       }
             }
  }
# # the report are saved to /outputs/reports/
################################################################################
################################################################################