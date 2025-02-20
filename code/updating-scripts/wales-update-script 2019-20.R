################################################################################
## WALES UPDATE TEMPLATE #######################################################
################################################################################
## This is the script used to add new data to the master data file and        ##
## produce the report for Wales for any year available in the file            ##
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

################################################################################
##  MANDATORY INPUTS                                                          ##
################################################################################
# which fiscal year do you want to produce a report for?
# NB: the current year is the year in which the fiscal year starts
current.year <- 2019

# if you want to produce a report based on current data - but for a previous year
# set add.new.data to FALSE. Run the rest of the script.
# If you want to add new data that has been publihsed then change to 
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
#This is the Data Table https://statswales.gov.wales/Catalogue/Local-Government/Finance/Revenue/Transport/roadsandtransportrevenueexpenditure-by-authority
#wal.income.file = Total Income : Parking of Vehicles
#wal.expenditure.file =  Net Current Expenditure :v
#wal.transport.file = Net Current Expenditure : Total transport planning, highways, roads and transport

if (add.new.data){
  ## after dowloading the Wales files into the data/01-raw folder, enter their
  ## correct filenames here:
  wal.income.file <-"orig.wal.inc.22.csv"
  wal.expenditure.file <-"orig.wal.exp.22.csv"
  wal.transport.file <-"orig.wal.trans.22.csv"
  
  ## replace with date of access to data:
  new.date.accessed <- "20.04.2023"
}
################################################################################
################################################################################
##                                                                            ##
##   THE REST OF THIS SCRIPT IS TO BE RUN ONLY -- NO MODIFICATIONS!           ##
##                                                                            ##
################################################################################
################################################################################
################################################################################
## LOAD PACKAGES AND DATA ######################################################
################################################################################
source("code/do-not-touch-scripts/functions.R")
if (!require("pacman")) install.packages("pacman")
pacman::p_load( tidyr,dplyr,tibble,RefManageR,googlesheets4,knitr,here,kableExtra,showtext,sf,viridis,bookdown,tabulizer)

# load existing master file
master <- readRDS("data/03-processed/master.rds")
bib.master <- readRDS("data/03-processed/bib.master.rds")
# new report name
report.name <- paste0("wales-report-", current.year, "-",
                      current.year - 1999)

## IMPORT AND CLEAN RPI DATA #################################################
# if RPI file doesn't exist, or if it doesn't have today's date, download it 
# again. This happens even if you don't add new data.


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

if (add.new.data){ 
  ################################################################################
  ## AUTOMATIC DATA IMPORT AND CLEANING 
  ################################################################################
  path <- "data/01-raw/"
  # read all expenditure data, remove extra row and column
  wal.income.total <- read.csv(paste0(path, wal.income.file))[-1,-1]
  # read all income data, remove extra row and column
  wal.expend.total <- read.csv(paste0(path, wal.expenditure.file))[-1,-1]
  # read all transport total data, remove extra row and column
  wal.transport.total <- read.csv(paste0(path, wal.transport.file))[-1,-1]
  
  # reshape all three dfs - you can ignore the warnigns here!
  wal.expend.total<- FunWalesReshape(wal.expend.total)
  
  wal.income.total<- FunWalesReshape(wal.income.total)
  wal.transport.total<- FunWalesReshape(wal.transport.total)
  
  # join them together and calculate surplus
  wal.expend.total %>% 
    left_join(wal.income.total) %>% 
    left_join(wal.transport.total) %>% 
    mutate(income.total = -income.total,
           surplus.total = income.total - expend.total) %>% 
    filter(year == current.year) -> update
  
  # add Wales specific data
  update %>% 
    mutate(country = "Wales",
           auth.type = "LA")  -> update
  
  # double check the update is OK:
  if (nrow(update) != 22) {
    paste("Something is wrong. The update should have 22 rows, but it has",
          nrow(update), "instead.")} else {
            "Everything checks out, the update has 22 rows"}
  
  ##############################################################################
  ## Add (or overwrite) new rows to master #####################################
  ##############################################################################
  
  # add update for Wales - if that year already exists, it will be overwritten!!!
  master %>% 
    anti_join(update, by = c("country", "auth.name", "year")) %>% 
    bind_rows(update) -> master
  
  # save updated datafile to master
  saveRDS(master, "data/03-processed/master.rds")
  write.csv(master, "outputs/csv-tables/master.csv")
  
  ##############################################################################
  # add new files to bibliography master #######################################
  ##############################################################################
  # add new date.accessed to bibliography master
  bib.master %>% 
    mutate(urldate = ifelse(country == "Wales", new.date.accessed,
                            urldate),
           year = ifelse(country == "Wales", 
                         as.numeric(format(Sys.Date(), "%Y")), year)) -> bib.master
  
  # update RPI data access date and year of publication
  bib.master %>% 
    mutate(urldate = ifelse(content == "rpi", 
                            as.character(format(Sys.Date(), "%d.%m.%Y")), urldate),
           year = ifelse(content == "rpi", 
                         as.numeric(format(Sys.Date(), "%Y")), year)) ->
    bib.master
  
  # save updated datafile to master
  saveRDS(bib.master, "data/03-processed/bib.master.rds")
  
}

# update RPI data acces date and year of publication
bib.master %>% 
  mutate(urldate = ifelse(content == "rpi", 
                          as.character(format(Sys.Date(), "%d.%m.%Y")), urldate),
         year = ifelse(content == "rpi", 
                       as.numeric(format(Sys.Date(), "%Y")), year)) ->
  bib.master

# save updated datafile to master
saveRDS(bib.master, "data/03-processed/bib.master.rds")

# create a bibliography for the wales report
bib.master %>% 
  filter(fiscyear > current.year - 5, !content %in% c("budget", "pcn", "wpl")) %>% 
 group_by(country) %>% 
 filter(country %in% c("GB","Wales") | country %in% c("England","Scotland") & fiscyear == max(fiscyear)) %>%
  mutate(refs = paste0("@", key)) %>% 
  column_to_rownames("key") -> bib.wales

# create the wales bibliography
bib.wales %>% 
  as.BibEntry() %>% 
  WriteBib(file = "code/report-rmds/wales.bib", 
           biblatex = FALSE, verbose = FALSE)

# also save the data frame
saveRDS(bib.wales, paste0("data/03-processed/", report.name, "-bib.rds"))


################################################################################
## COMPILE REPORT 
################################################################################
# check if master data is available for current year?

if(nrow(filter(master, country == "Wales", year == current.year)) == 0) {
  warning("There are no records for the year ", current.year) } else {
    if(nrow(filter(master, country == "Wales", year == current.year)) != 22) {
      warning("Something has gone wrong. There should be 22 rows for ", 
             current.year, " but there are not. I suggest you revert to a ",
             "previous version of the repository and try again.")} else {
               
               if(!recompile.rmd){        
                 # create a fresh copy of the wales report template
                 file.copy("code/report-templates/wales-report-template.Rmd",
                           paste0("code/report-rmds/", report.name, ".Rmd"),
                           overwrite = TRUE)}
               
               # compile the report (but check if file exists first)
               if(recompile.rmd & !file.exists(paste0("code/report-rmds/", 
                                                      report.name, ".Rmd"))){
                 warning("The Rmd file does not exist. Rerun this script with ",
                       "recompile.rmd swithced to FALSE.")} else { 
                         
                         
                         # compile the report - you can repeat this as many times as you like after 
                         # updating the .Rmd file 

#I have taken these out and replaced with the render command                         
#           params = list("current.year" = current.year,
#                                       "dp.text" = dp.text,
#                                       "dp.tables" = dp.tables)
#                       
#                         
#            knitr::knit(input = paste0("code/report-rmds/", report.name, ".Rmd"), 
#                                     output= paste0("outputs/reports/",report.name, "TEST.pdf"))
            
            
 #New Render Command instead of Knit - this needs to create process files in different directory
                         rm(params)
            rmarkdown::render(input = paste0("code/report-rmds/", report.name, ".Rmd"),
                              params = list("current.year" = current.year,
                                            "dp.text" = dp.text,
                                            "dp.tables" = dp.tables),
                              output_file= paste0(report.name, ".pdf"))
            
            
                        #Move Finished Report to Report Outputs    
                         file.copy(from=paste0("code/report-rmds/",report.name, ".pdf"),
                                   to=paste0("outputs/reports/", report.name,".pdf"))
                         file.remove(paste0("code/report-rmds/",report.name, ".pdf"))
                         
                         # remove empty folder that the compilation creates
                         #unlink(paste0("outputs/reports/", report.name, "_files"), recursive=TRUE)
                         unlink(paste0("code/report-rmds/", report.name, "_files"), recursive=TRUE)
                         
                         # remove log file (comment this out if there are issues and look at the log
                         # file for clues?
                         suppressWarnings(file.remove(paste0("code/report-rmds/", report.name, ".log")))
                         suppressWarnings(file.remove(paste0("code/report-rmds/", report.name, ".tex")))
                       }
             }
  }

# the report are saved to /outputs/reports/
################################################################################
################################################################################

