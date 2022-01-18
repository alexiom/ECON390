###########################################################################
# Title:       Compiling HCRIS 
# Author:      Alex Marsh
# Date:        October 22, 2021
# Description: This script will download and clean the HCRIS data for renal facilities
#
# Notes:       - Make sure you have csv files with the correct codes. 
#              - Do not load dtplyr package. It causes an error later on.
##########################################################################
##### Load libraries
library(data.table)
library(stringr)
library(dplyr)
library(tidyr)

##### Set project directory
setwd("/Users/alexmarsh/Documents/School/RA_work/Duke/dialysis_mergers/")

##### Set input and output directories
str.indir  = "data/hcris/raw/"
str.outdir = "data/hcris/cleaned/"

##### Set output filenames
str.outfile_panel = "hcris_panel.csv"

##### Set controls for which years to do
k.min_year        = 1998
k.max_year        = 2010
log.download_data = FALSE

##### Load codes I want to extract
dt.codes_to_extract = fread("data/hcris/codes_non_ownership.csv",
                            colClasses = "character")

#dt.codes_ownership<-fread("data/hcris/codes_ownership.csv",
#                          colClasses = "character")

# Create a key
dt.codes_to_extract[,`:=`(key=paste0(worksheet,line,column),
                          worksheet=NULL,
                          line=NULL,
                          column=NULL)
]

dt.codes_to_extract[,`:=`(variable=as.character(variable),key=as.character(key))]

##### Download data from NBER
if (log.download_data) {
  for (year in k.min_year:k.max_year) {
    for (type in c("rpt","nmrc","alpha")) {
      str.url =
        paste0("http://www.nber.org/hcris/265-94/rnl_",type,"265_94_",as.character(year),
               ifelse(type!="rpt","_long",""),".csv")
      
      str.outfile<-paste0("hcris_",type,"_",year,".csv")
      
      download.file(str.url,
                    paste0(str.indir,str.outfile))
    }
  }
}

##### Load data and clean it
for (year in k.min_year:k.max_year) {
  for (type in c("rpt","nmrc","alpha")) { 
    
    str.infile   = paste0("hcris_",type,"_",as.character(year),".csv")
    str.outfile  = paste0("hcris_",as.character(year),".csv")
    str.var_name = paste0("dt.hcris_",type)
    
    assign(str.var_name,
           fread(paste0(str.indir,str.infile))
    )
    
    if (type=="rpt") {
      setnames(get(str.var_name),
               old=c("rpt_rec_num"),
               new=c("report_number")
      )
      
    } else {
      setnames(get(str.var_name),
               old=c("rpt_rec_num","wksht_cd","clmn_num"),
               new=c("report_number","worksheet","column")
      )
    }
    
    
  }
  
  dt.hcris_nmrc[,`:=`(line_num = str_pad(line_num,5,"left","0"),
                      column = str_pad(column,4,"left","0"))]
  dt.hcris_alpha[,`:=`(line_num = str_pad(line_num,5,"left","0"),
                       column = str_pad(column,4,"left","0"))]
  
  dt.hcris_nmrc[,`:=`(key=paste0(worksheet,line_num,column))]
  dt.hcris_alpha[,`:=`(key=paste0(worksheet,line_num,column))]  
  
  # First get all of the non-owner variables
  dt.hcris =
    dt.hcris_nmrc %>%
    filter(key %chin% dt.codes_to_extract$key) %>%
    left_join(dt.codes_to_extract,by="key") %>% 
    select(-key,-worksheet,-line_num,-column) %>%
    group_by(report_number) %>%
    spread(variable,itm_val_num)
  
  dt.hcris =
    dt.hcris_alpha %>%
    filter(key %chin% dt.codes_to_extract$key) %>%
    left_join(dt.codes_to_extract,by="key") %>% 
    select(-key,-worksheet,-line_num,-column) %>%
    group_by(report_number) %>%
    spread(variable,alphnmrc_itm_txt) %>%
    left_join(dt.hcris,.,by="report_number")
  
  dt.hcris = merge(dt.hcris,dt.hcris_rpt[,.(report_number,fy_bgn_dt,fy_end_dt)],by="report_number")
  
  dt.hcris$year = year
  fwrite(dt.hcris,
         file=paste0(str.outdir,str.outfile),
         quote=TRUE,
         row.names = FALSE
  )
}

list.hcris_cleaned_files = list.files(path=str.outdir,
                                      pattern="hcris_\\d+.csv",
                                      full.names = TRUE)

# Load all reports
dt.hcris = rbindlist(lapply(list.hcris_cleaned_files,fread),fill=TRUE,use.names = TRUE)

fwrite(dt.hcris,paste0(str.indir,"hcris_panel_raw.csv"))

dt.hcris = dt.hcris[!is.na(prvdr_num)]

# replace all epo variables with absolute values
dt.hcris[,
         `:=`(epo_cost=abs(epo_cost),epo_net_cost=abs(epo_net_cost),epo_rebates=abs(epo_rebates))
]

# replace NA rebates with 0
dt.hcris[is.na(epo_rebates),epo_rebates:=0]

# initial clean of epo variables
dt.hcris[,epo_cost := as.numeric(epo_cost)]
dt.hcris[is.na(epo_cost) & !is.na(epo_net_cost) & epo_rebates==0,epo_cost := epo_net_cost]
dt.hcris[is.na(epo_cost) & !is.na(epo_net_cost) & epo_rebates != 0,epo_cost := epo_net_cost + epo_rebates]
dt.hcris[is.na(epo_cost) & is.na(epo_net_cost) & epo_rebates == 0,`:=` (epo_cost=0,epo_net_cost=0)]
dt.hcris[is.na(epo_cost) & is.na(epo_net_cost) & epo_rebates != 0 ,`:=` (epo_cost=0,epo_net_cost=0)]
dt.hcris[!is.na(epo_cost) & is.na(epo_net_cost),epo_net_cost := epo_cost - epo_rebates]

dt.hcris[epo_net_cost>epo_cost,`:=`(epo_cost=epo_net_cost,epo_net_cost=epo_cost)]

# fix one incorrect provider number
dt.hcris[prvdr_num ==" ",prvdr_num:="342664"]

#clean dates
dt.hcris[,`:=`(report_start_date=NULL,report_end_date=NULL)]
dt.hcris[,`:=`(fy_bgn_dt=mdy(fy_bgn_dt),
               fy_end_dt=mdy(fy_end_dt))]

dt.hcris[,zip_code:= as.numeric(substr(trimws(zip_code),1,5))]
zip_codes = unique(dt.hcris[,.(prvdr_num,zip_code)])
prvdr_num_NA = zip_codes[is.na(zip_code),prvdr_num]
zip_codes = zip_codes[is.na(zip_code)]
zip_codes[,n := .N,by="prvdr_num"]
prvdr_num_2 = unique(zip_codes[prvdr_num %in% prvdr_num_NA & n == 2,prvdr_num])
prvdr_num_3 = unique(zip_codes[prvdr_num %in% prvdr_num_NA & n == 3,prvdr_num])
prvdr_num_2 = prvdr_num_2[!is.na(prvdr_num_2)]
dt.hcris[prvdr_num %in% prvdr_num_2,zip_code := max(zip_code,na.rm = TRUE),by=prvdr_num]

dt.hcris[prvdr_num==12509,zip_code := 35801] #typo
dt.hcris[prvdr_num == 12507 & is.na(zip_code), zip_code := 36604] # weird missing case
dt.hcris[prvdr_num == 522536, zip_code := 53226]

dt.hcris[state=="",state:=NA]
dt.hcris[chain_indicator=="",chain_indicator:=NA]

dt.hcris[grepl("DAVITA",chain_identity,ignore.case = TRUE),
         chain_identity:="DaVita"]
dt.hcris[grepl("FRESENUIS",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FRESNIOUS",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FRESNEIUS",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FRESENIUS",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FRESINIUS",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FRESEN",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FRESI",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FREN",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FRES",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]
dt.hcris[grepl("FESE",chain_identity,ignore.case = TRUE),
         chain_identity:="Fresenius"]

dt.hcris[chain_identity=="DACITA  INC",chain_identity:="DaVita"]
dt.hcris[chain_identity=="DANITA  INC.",chain_identity:="DaVita"]
dt.hcris[chain_identity=="DANITA  INC.",chain_identity:="DaVita"]
dt.hcris[chain_identity=="DVITA  INC.",chain_identity:="DaVita"]
dt.hcris[chain_identity=="DATIVA  INC." ,chain_identity:="DaVita"]
dt.hcris[chain_identity=="DAVIATA  INC" ,chain_identity:="DaVita"]
dt.hcris[chain_identity=="VITA INC." ,chain_identity:="DaVita"]


test_v  = c(NA,NA,2,1)
test_v1 = c(-1,-2,3,4)
test_v2 = c(1,2,-3,-4)

ids = which(ifelse(is.na(test_v),TRUE,FALSE))
dt.hcris[ids,"epo_cost"]     = 0
dt.hcris[ids,"epo_net_cost"] = 0

dt.hcris[is.na(epo_cost) & is.na(epo_net_cost) & epo_rebates == 0,`:=` (epo_cost=0,epo_net_cost=0)]
