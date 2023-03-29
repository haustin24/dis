library(tidyverse)
library(here)
library(lubridate)
library(readxl)



# BoE ---------------------------------------------------------------------

#read in files
gilt_operations_raw <- read_csv(here('input', "gilt_purchase_operational_results.csv"))

#get headers 
gilt_operations_clean <- gilt_operations_raw[-1,] %>%
  set_names(gsub("[\r\n]", "",gilt_operations_raw[1,]))

#remove excess columns
gilt_operations_clean <- gilt_operations_clean %>% 
  select(`Operation date`:Tail)

#set date format
gilt_operations_clean <- gilt_operations_clean %>% 
  mutate(`Operation date` = dmy(gilt_operations_clean$`Operation date`),
         `Settlement date` = dmy(gilt_operations_clean$`Settlement date`))

gilt_operations_clean <- gilt_operations_clean %>% 
  mutate(across(c(`Total offers received (proceeds £mn)`:"Tail"), as.numeric))

#find maturity 
gilt_operations_clean <- gilt_operations_clean %>% 
  mutate(maturity = dmy(str_sub(Bond, start = -6L)))

#find residual maturity 
gilt_operations_clean <- gilt_operations_clean %>% 
  mutate(res_mat =
           time_length(as.Date(maturity) - as.Date(`Operation date`), unit = "years"))

boe_purchases <- gilt_operations_clean %>% 
  select(`Operation date`,ISIN,`Total offers received (proceeds £mn)`,
         `Total allocation (proceeds £mn)`,`Total allocation (nominal £mn)`,Tail,
         res_mat, maturity)

boe_purchases <- boe_purchases %>% 
  set_names("operation_date", "ISIN", "proceeds_offers",
            "proceeds_purchases","nominal_purchases","tail",
            "res_mat","maturity")
#set numeric format

#order by date
boe_purchases <- boe_purchases %>% 
  arrange(`operation_date`)
# DMO issuance ---------------------------------------------------------------------

#read in dmo issuance history data 
dmo_raw <- read_csv(here('input', 'Gilt_Issuance_History.csv'))

#set headers
dmo_clean <- dmo_raw[-c(1:2),] %>%
  set_names(gsub("[\r\n]", "",dmo_raw[1,]))

#remove NA's from first row 
dmo_clean <- dmo_clean[!is.na(dmo_clean$`Gilt Name`), ]

#set as date
dmo_clean <- dmo_clean %>% 
  mutate(`Issue Date` = dmy(dmo_clean$`Issue Date`))

#remove commas for as.numeric
dmo_clean <- dmo_clean %>% 
  mutate(across(c(`Clean Price at Issue (£)`:`Cumulative Total Amount in Issue (£ million)`),
                funs(as.numeric(str_replace(., ",","")))))

#remove and rename dupliacte col names from above cleaning
dmo_clean <- dmo_clean[,-c(5:8)] %>% 
  set_names(colnames(dmo_clean[,c(1:4,5:8)]))

dmo_clean$`Issuance Type` %>% unique()

#filter out to include only outright issuance of adjustments
dmo_clean <- dmo_clean %>% 
  filter(`Issuance Type` %in% c("Outright", "Reverse Auction", "Conversion",
                                "Syndication", "Switch Auction"))

dmo_clean <- dmo_clean %>%
  select(`ISIN Code`,`Issue Date`,`Issuance Type`,
         `Clean Price at Issue (£)`,`Yield at Issue (%)`,`Nominal Amount Issued (£ million)`,
         `Cumulative Total Amount in Issue (£ million)`) %>% 
  set_names("ISIN", "issue_date", "issuance_type",
            "clean_price_at_issue", "yield_at_issue", "nom_issued",
            "cumulative_total_in_issue")

dmo_issuance <- dmo_clean %>% 
  select(ISIN,issue_date,nom_issued,cumulative_total_in_issue)

#arrange by date
dmo_issuance <- dmo_issuance %>%
  arrange(issue_date)

# saveRDS(dmo_issuance, here('output',"clean_dmo_issuance.RDS"))
# DMO price data ----------------------------------------------------------

#function to clean dmo prices data 

clean_dmo_prices_function <- function(filepath){
  
  #read in file
  df <- read_xlsx(filepath)
  
  #remove headers
  df2 <- df[-c(1:4),] %>% 
    set_names(df[4,])
  
  #clean dates
  df2 <- df2 %>% 
    mutate(`Redemption Date` = as.Date(as.numeric(`Redemption Date`), origin = "1900-01-01"),
           `Close of Business Date` = as.Date(as.numeric(`Close of Business Date`), origin = "1900-01-01"))
  
  #remove clutter to show only conventional gilt price
  df2 <- df2 %>% 
    filter(grepl('Treasury Gilt', `Gilt Name`)) %>% 
    filter(!grepl('Index-linked | WI', `Gilt Name`))
  
  df2 <- df2[!is.na(df2$`Gilt Name`), ]
  
  #fix white space in col name
  df2 <- df2 %>% 
    set_names(gsub("[\r\n]", "",colnames(df2)))
  
  #format as numeric
  df2 <- df2 %>% 
    mutate(across(c(`Clean Price(£)`:`Modified Duration`),as.numeric))
  
  df2
  
}

#find file paths for all dmo price spreadsheets
price_paths <- list.files(here('input','dmo_gilt_prices'), full.names = TRUE)

#apply filepaths to cleaning function
# dmo_prices_list <- lapply(price_paths, clean_dmo_prices_function)

saveRDS(dmo_prices_list, here('output', 'dmo_prices_list.RDS'))

# BBG FULL dataset  ------------------------------------------------------

bbg_gilt_data <- read_xlsx(here('input', 'bbg_gilt_data.xlsx'), sheet = 'data (values)')

#split by ISIN names 
#loop to split up data by bond
bbg_gilt_list <- list()

for(i in seq(2,395,5)) {
  
  #select data
  df <- bbg_gilt_data %>%
    select(1, i:(i + 4))
  
  #pull ISIN 
  name <- df[3,2] %>% 
    str_replace(" Corp", "")
  
  #rename and remove rows
  df <- df %>% 
    set_names(df[5,]) %>% 
    slice(-c(1:5))
  
  #reformat
  df <- df %>% 
    mutate(Dates = as.Date(as.numeric(Dates), origin = "1899-12-30", tz = "GMT"),
           across(c(YLD_YTM_MID:PX_LOW),as.numeric),
           ISIN = name)
  
  bbg_gilt_list[[i]] <- df
  
}

bbg_gilts_long <- bind_rows(bbg_gilt_list)


#add bond maturities and remove data past maturity to make sure dead bond data not involved
boe_maturities <- read_xlsx(here('input','bbg_gilt_data.xlsx'), sheet = "boe_maturities")

boe_maturities <- boe_maturities %>% 
  mutate(maturity = dmy(Maturity),
         ISIN = str_sub(ISIN, end = -6L))


bbg_isins <- bbg_gilts_long %>%
  select(ISIN) %>% 
  unique() %>% 
  pull()

mat_check_function <- function(isin) {
  mat_check_isin <- isin
  
  bond_maturity <- boe_maturities %>%
    filter(ISIN == mat_check_isin) %>%
    select(maturity) %>%
    pull()
  
  bbg_gilts_long %>%
    filter(ISIN == mat_check_isin &
             Dates <= bond_maturity)
}

mat_checked_bbg_gilts <- bind_rows(lapply(bbg_isins, mat_check_function))

#check to make sure no observation of a bond is greater than its maturity 
bbg_gilts_long <- bbg_gilts_long %>% 
  left_join(boe_maturities %>% 
              select(ISIN, maturity),
            by = "ISIN")

bbg_gilts_long <- mat_checked_bbg_gilts

bbg_gilts_long <- bbg_gilts_long %>% 
  mutate(res_mat =
           time_length(as.Date(maturity) - as.Date(Dates), unit = "years"))


##JUMP TO 'BBG price data' to run tidying loop for prices
#left join price data
bbg_full_dataset <- bbg_full_dataset %>% 
  left_join(bbg_price_list_long,
            by = c("Dates", "ISIN"))


saveRDS(bbg_gilts_long, here('output','bbg_full_dataset.RDS'))
saveRDS(bbg_full_dataset, here('output','bbg_full_dataset.RDS'))

# Combining data ----------------------------------------------------------

#function for unique ISINS to see % of free-float following each purchase operation

#find all unique bonds purchased by boe 
boe_isins <- boe_purchases$ISIN %>% 
  unique()


free_float_ownership_function <- function(x){
  
  #filter by bond
df <- boe_purchases %>% 
  filter(ISIN == x) 

#cumulative sum of purchases 
df <- df %>% 
  mutate(cuml_nom_purch = cumsum(df$nominal_purchases))

#issuance for given bond
df2 <- dmo_issuance %>% 
  filter(ISIN == x)

#loop to find nearest cumulative amount in issue at operation date 
nearest_issuance_list <- list()

for(i in 1:nrow(df)){
#find purchase operation date
v1 <- df[i,] %>% 
  select(operation_date) %>% 
  pull()

#find nearest issuance date to v1 and find cumul amount in issue 
v2 <- df2 %>% 
  mutate(date_diff = v1 - issue_date) %>% 
  filter(date_diff >= 0) %>%
  filter(date_diff == min(date_diff)) %>% 
  select(ISIN, issue_date, cumulative_total_in_issue)

#merge dmo and boe data to calculate % of ownership at period 
merged_data <- left_join(df[i,],
          v2,
          by = "ISIN")

nearest_issuance_list[[i]] <- merged_data %>% 
  mutate(percent_of_freefloat = cuml_nom_purch/cumulative_total_in_issue)
}

#merged list to show complete dataframe of boe ownership of bond at all points in time following purchases
df3 <- bind_rows(nearest_issuance_list)

df3
}

#takes approx. 5 mins to run 
# boe_market_freefloat <- lapply(boe_isins, free_float_ownership_function)
# # 
# # #save to avoid having to run function again 
# saveRDS(boe_market_freefloat, here('output',"boe_freefloat_ownership_list.RDS"))


# OIS rates ---------------------------------------------------------------

ois_raw <- read_xlsx(here('input','ois_yields.xlsx'), sheet = "ois_mid_yields_values")

ois_clean <- ois_raw[-c(1:3),] %>% 
  set_names(c("date",ois_raw[3,-1]))

ois_clean <- ois_clean %>% 
  mutate(date = as.Date(as.numeric(ois_clean$date), origin = "1899-12-30", tz = "GMT"))

ois_clean <- ois_clean %>% 
  mutate(across(c(2:length(ois_clean)), as.numeric))

#temporary cleaner col names 
ois_clean <- ois_clean %>%
  set_names(c(
    "date",
    colnames(ois_clean[-1]) %>%
      str_replace(" BGN Curncy", "") %>%
      paste("OIS", sep = "_")
  ))

ticker_abrv <- colnames(ois_clean[-1])

#pivot longer 
ois_clean <- ois_clean %>% 
  pivot_longer(c(-1)) %>% 
  set_names("date","ticker_abrv", "ois_yield")


## get tenors for OIS rates to workout which maturities are which. 


ticker_decode <- tibble("ticker_abrv"= ticker_abrv)

#trim "BPSWS..."  prefix from string
ticker_decode <- ticker_decode %>% 
  mutate("no_prefix" = ticker_abrv %>% 
           str_sub(start = 6L))

#remove "_OIS" suffix 
ticker_decode <- ticker_decode %>% 
  mutate("no_suffix" = no_prefix %>% 
           str_replace("_OIS", ""))

ticker_decode <- ticker_decode %>% 
  mutate("number" = str_extract(no_suffix, "\\d+"))

ticker_decode <- ticker_decode %>% 
  mutate("letter" = str_extract(no_suffix, "[A-Z]+"))

#assume letters are months
# z = 7 days i.e. 1 week 
#average of 30.437 days in a month
letter_decode <- tibble("letter" = LETTERS[1:11],
       "letter_year_frac" = seq(1:11)/12)

ticker_decode <- ticker_decode %>% 
  left_join(letter_decode)

#case when mutate to add in week fractions for first two OIS contracts 
#turn NA's in number and letter_year_frac to 0
ticker_decode <- ticker_decode %>% 
  mutate(letter_year_frac = case_when(letter == "Z"~ (7/365),
                               TRUE ~ letter_year_frac),
         number = case_when(is.na(number) ~ "0",
                            TRUE ~ number),
         letter_year_frac = case_when(is.na(letter_year_frac) ~ 0,
                                      TRUE ~ letter_year_frac))

ticker_decode <- ticker_decode %>% 
  mutate(number = as.numeric(number))


#mutate to workout tenor of OIS as fraction of a year
#conditional on Z being multiplicative rather than all else being in addition
ticker_decode <- ticker_decode %>% 
  mutate(ois_tenor = case_when(letter == "Z" ~ number*letter_year_frac,
                               TRUE ~ number + letter_year_frac))

ticker_decode_small <- ticker_decode %>% 
  select(ticker_abrv, ois_tenor)



#add OIS tenors to clean OIS data

ois_clean <- ois_clean %>% 
  left_join(ticker_decode_small, by = "ticker_abrv")

#save as RDS file 
# saveRDS(ois_clean, here('output', "ois_clean.RDS"))
# write_csv(ois_clean, here('output', "ois_clean.csv"))

#BOE dataset: bid-ask spreads and yields ------------------------------------

bid_ask_function <- function(row_no) {
  freefloat_1 <- freefloat_boe[row_no, ]
  
  isin_1 <- freefloat_boe[row_no, ] %>%
    select(ISIN) %>%
    pull()
  
  date_1 <- freefloat_boe[row_no, ] %>%
    select(operation_date) %>%
    pull()
  
  #tail to avoid chance where there is no bond data observation for date
  bond_data_1 <- bbg_data %>%
    filter(ISIN == isin_1 &
             Dates <= date_1) %>%
    tail(1)
  
  bond_data_2 <- bond_data_1 %>%
    mutate(YLD_BID_ASK = YLD_YTM_BID - YLD_YTM_ASK,
           date_dif = date_1 - Dates) %>%
    select(Dates, ISIN, YLD_YTM_MID, YLD_BID_ASK, date_dif) %>%
    set_names("operation_date",
              "ISIN",
              "YLD_YTM_MID",
              "YLD_BID_ASK",
              "date_dif")
  
  #change date back to date of operation rather than nearest rounded from bbg data
  bond_data_2$operation_date <- date_1
  
  left_join(freefloat_1, bond_data_2,
            by = c("operation_date", "ISIN"))
  
}

bond_dataset <- bind_rows(lapply(1:nrow(freefloat_boe), bid_ask_function))


bond_dataset <- bond_dataset %>% 
  arrange(operation_date)



#BOE dataset: bid-ask spread day change; less than,  greater than -----------------------------------------------

onedchng_bid_ask_function <- function(row_no) {
  df_1 <- bond_dataset[row_no,]
  
  #find ISIN
  isin_1 <- df_1 %>%
    select(ISIN) %>%
    pull()
  
  #find date of operation
  date_1 <- df_1 %>%
    select(operation_date) %>%
    pull()
  
  #filter bond data by t-1 and t+1 or operation dates
  
  bond_data_tminus1 <- bbg_data %>%
    filter(ISIN == isin_1 &
             Dates < date_1) %>%
    tail(1)
  
  bond_data_tplus1 <- bbg_data %>%
    filter(ISIN == isin_1 &
             Dates > date_1) %>%
    head(1)
  
  
  #find bid-ask spreads for each date
  bond_data_tminus1_2 <- bond_data_tminus1 %>%
    mutate(YLD_BID_ASK = YLD_YTM_BID - YLD_YTM_ASK) %>%
    select(Dates, ISIN, YLD_BID_ASK) %>%
    set_names("date",
              "ISIN",
              "YLD_BID_ASK")
  
  bond_data_tplus1_2 <- bond_data_tplus1 %>%
    mutate(YLD_BID_ASK = YLD_YTM_BID - YLD_YTM_ASK) %>%
    select(Dates, ISIN, YLD_BID_ASK) %>%
    set_names("date",
              "ISIN",
              "YLD_BID_ASK")
  
  #create df with bid-ask day change
  bond_data_2 <- tibble(
    "operation_date" = date_1,
    "ISIN" = isin_1,
    "1DCHNG_YIELD_BID_ASK" =
      bond_data_tplus1_2$YLD_BID_ASK - bond_data_tminus1_2$YLD_BID_ASK,
    "1DCHNG_date_dif" =
      bond_data_tplus1_2$date - bond_data_tminus1_2$date
  )
  
  
  #left join to main dataframe
  df_1 %>%
    left_join(bond_data_2,
              by = c("operation_date", "ISIN"))
}

updated_bond_dataset <- bind_rows(lapply(1:nrow(bond_dataset), onedchng_bid_ask_function))

updated_bond_dataset <- updated_bond_dataset %>% 
  arrange(operation_date)

bond_dataset <- updated_bond_dataset

#BOE dataset: bid-ask spread day change; less than, end of day -------------------------

onedchng_EOD_bid_ask_function_2 <- function(row_no) {
  df_1 <- bond_dataset[row_no,]
  
  #find ISIN
  isin_1 <- df_1 %>%
    select(ISIN) %>%
    pull()
  
  #find date of operation
  date_1 <- df_1 %>%
    select(operation_date) %>%
    pull()
  
  #filter bond data by t-1 and t+1 or operation dates
  
  bond_data_tminus1 <- bbg_data %>%
    filter(ISIN == isin_1 &
             Dates < date_1) %>%
    tail(1)
  
  bond_data_tplus1 <- bbg_data %>%
    filter(ISIN == isin_1 &
             Dates >= date_1) %>%
    head(1)
  
  
  #find bid-ask spreads for each date
  bond_data_tminus1_2 <- bond_data_tminus1 %>%
    mutate(YLD_BID_ASK = YLD_YTM_BID - YLD_YTM_ASK) %>%
    select(Dates, ISIN, YLD_BID_ASK) %>%
    set_names("date",
              "ISIN",
              "YLD_BID_ASK")
  
  bond_data_tplus1_2 <- bond_data_tplus1 %>%
    mutate(YLD_BID_ASK = YLD_YTM_BID - YLD_YTM_ASK) %>%
    select(Dates, ISIN, YLD_BID_ASK) %>%
    set_names("date",
              "ISIN",
              "YLD_BID_ASK")
  
  #create df with bid-ask day change
  bond_data_2 <- tibble(
    "operation_date" = date_1,
    "ISIN" = isin_1,
    "1DCHNG_EOD_YIELD_BID_ASK" =
      bond_data_tplus1_2$YLD_BID_ASK - bond_data_tminus1_2$YLD_BID_ASK,
    "1DCHNG_EOD_date_dif" =
      bond_data_tplus1_2$date - bond_data_tminus1_2$date
  )
  
  
  #left join to main dataframe
  df_1 %>%
    left_join(bond_data_2,
              by = c("operation_date", "ISIN"))
}

updated_bond_dataset <- bind_rows(lapply(1:nrow(bond_dataset), onedchng_EOD_bid_ask_function_2))

updated_bond_dataset <- updated_bond_dataset %>% 
  arrange(operation_date)

bond_dataset <- updated_bond_dataset

#change percent freefloat to holding ratio
colnames(boe_dataset)[colnames(boe_dataset) == 'percent_of_freefloat'] <- "holding_ratio"

# BBG dataset: With holding ratios ----------------------------------------------


#avoiding a loop switch to func-ception

bbg_hr_function <- function(isin){
  
  df1 <- bbg_data %>% 
    filter(ISIN == isin)
  
  df_rows <- 1:nrow(df1)
  
  bbg_hr_row_function <- function(row_no) {
    
    date_1 <- df1[row_no, ] %>%
      select(Dates) %>%
      pull()
    
    if(date_1 < freefloat_boe[1,1]){
      
      bond_holding_ratio <- tibble(percent_of_freefloat = 0)
      
    }else{
      
      bond_holding_ratio <- bond_dataset %>%
        filter(ISIN == isin &
                 operation_date <= date_1) %>%
        tail(1) %>%
        select(percent_of_freefloat)
    }
    
    merge(df1 %>%
            filter(Dates == date_1),
          bond_holding_ratio)
    
  }
  
  bind_rows(lapply(df_rows, bbg_hr_row_function))
  
}

bbg_isins <- bbg_data %>% 
  select(ISIN) %>% 
  unique() %>% 
  pull()

test_isins <- bbg_isins[1:4]

rest_isins <- bbg_isins[-c(1:4)]


start_time <- Sys.time()

# bbg_hr_rest <- lapply(rest_isins, bbg_hr_function)

end_time <- Sys.time()
end_time - start_time


bbg_hr <- append(bbg_hr_test,
                 bbg_hr_rest)

bbg_hr_dataset <- bind_rows(bbg_hr)

# saveRDS(bbg_hr_dataset, here('output', 'bbg_hr_dataset.RDS'))

#BOE dataset: QE programme dates ------------------------------------------------------
qe_prog_df <- read_excel(here('input', 'qe_prog_data.xlsx'),
                         sheet = 'programme')


qe_maturity_sectors <- read_excel(here('input', 'qe_prog_data.xlsx'),
                                  sheet = 'maturity_sectors') %>% 
  mutate(across(c(2:7), as.numeric))%>%
  mutate(def_range = row_number()) %>%
  select(-date) %>% 
  pivot_longer(c(-7,-8))

bond_dataset <- bond_dataset %>% 
  mutate(
    prog = case_when(
      operation_date >= qe_prog_df$start_date[1] &
        operation_date <= qe_prog_df$end_date[1] ~
        qe_prog_df$qe_programme[1],
      operation_date >= qe_prog_df$start_date[2] &
        operation_date <= qe_prog_df$end_date[2] ~
        qe_prog_df$qe_programme[2],
      operation_date >= qe_prog_df$start_date[3] &
        operation_date <= qe_prog_df$end_date[3] ~
        qe_prog_df$qe_programme[3],
      operation_date >= qe_prog_df$start_date[4] &
        operation_date <= qe_prog_df$end_date[4] ~
        qe_prog_df$qe_programme[4],
      operation_date >= qe_prog_df$start_date[5] &
        operation_date <= qe_prog_df$end_date[5] ~
        qe_prog_df$qe_programme[5]
    ))

# bond_dataset_2 <- read_excel(here('output', 'bond_dataset_v2.xlsx'),
#                              sheet = 'bond_dataset (value)')
# 
# 
# write.dta(bond_dataset_2, here('stata','bond_dataset_v3.dta') )
# 
# bond_dataset_2 <- bond_dataset_2 %>% 
#   mutate(maturity = as.Date.POSIXct(maturity),
#          issue_date = as.Date.POSIXct(issue_date))
bbg_subset <- bbg_full_dataset %>% 
  select(Dates,ISIN,YLD_YTM_BID,
         YLD_YTM_ASK,PX_MID,PX_ASK,
         PX_BID,bid_ask_v1,bid_ask_v2,
         bid_ask_v3)

colnames(bbg_subset)[1] <- "operation_date"

boe_dataset <- boe_dataset %>% 
  select(-c(`1DCHNG_YIELD_BID_ASK`,`1DCHNG_date_dif`,`1DCHNG_EOD_YIELD_BID_ASK`,
            `1DCHNG_EOD_date_dif`, date_dif)) %>% 
  left_join(bbg_subset,
            by = c("operation_date", "ISIN"))



#BBG dataset -------------------------------------------------------------

bbg_hr_dataset <- readRDS(here('output', 'bbg_hr_dataset.RDS'))

bbg_hr_dataset <- bbg_hr_dataset %>% 
  select(-`y[FALSE, ]`)

#add maturity date
bond_maturities <- read_excel(here('input','bbg_gilt_data.xlsx'),
                              sheet = "boe_maturities")

bond_maturities <- bond_maturities %>% 
  mutate(ISIN = gsub(" Corp","",ISIN),
         maturity = as.Date.POSIXct(dmy(Maturity), tz = 'GMT')) %>% 
  select(-Maturity)

bbg_hr_dataset <- bbg_hr_dataset %>% 
  left_join(bond_maturities,
            by = "ISIN")

#calculate residual maturity
bbg_hr_dataset <- bbg_hr_dataset %>% 
  mutate(res_mat =
           time_length(as.Date(maturity) - as.Date(Dates), unit = "years"))

#filter out residual maturity <90 days??

#add QE programme date 

bbg_hr_dataset <- bbg_hr_dataset %>%
  mutate(
    prog = case_when(
      Dates >= qe_prog_df$start_date[1] &
        Dates <= qe_prog_df$end_date[1] ~
        qe_prog_df$qe_programme[1],
      Dates >= qe_prog_df$start_date[2] &
        Dates <= qe_prog_df$end_date[2] ~
        qe_prog_df$qe_programme[2],
      Dates >= qe_prog_df$start_date[3] &
        Dates <= qe_prog_df$end_date[3] ~
        qe_prog_df$qe_programme[3],
      Dates >= qe_prog_df$start_date[4] &
        Dates <= qe_prog_df$end_date[4] ~
        qe_prog_df$qe_programme[4],
      Dates >= qe_prog_df$start_date[5] &
        Dates <= qe_prog_df$end_date[5] ~
        qe_prog_df$qe_programme[5]
    )
  ) 


# BBG dataset: bid-ask spreads ---------------------------------------------------------

# v1: bid - ask v2: bid - ask as percentage of security (from JGB study)
bbg_hr_dataset <- bbg_hr_dataset %>% 
  mutate(bid_ask_v1 =
           YLD_YTM_BID - YLD_YTM_ASK,
         bid_ask_v2 =
           (YLD_YTM_BID - YLD_YTM_ASK)/YLD_YTM_MID)

#worth attempting with price data to be inline with JGB study?? 
#JGB study filters out res mat < 90days due to impact on YTM 
bbg_hr_dataset <- bbg_hr_dataset %>% 
  filter(res_mat >= (90/365))


# BBG price data ----------------------------------------------------------


bbg_price_data <- read_excel(here('input', 'bbg_gilt_data.xlsx'),
                             sheet = "data_prices (values)")

bbg_hr_dataset <- readRDS(here('output','bbg_hr_dataset.RDS'))


# work out for loop i 

#split by ISIN names 
#loop to split up data by bond
bbg_price_list <- list()

for(i in seq(2,238,3)) {
  
  #select data
  df <- bbg_price_data %>%
    select(1, i:(i + 2))
  
  #pull ISIN 
  name <- df[3,2] %>% 
    str_replace(" Corp", "")
  
  #rename and remove rows
  df <- df %>% 
    set_names(df[5,]) %>% 
    slice(-c(1:5))
  
  #reformat
  df <- df %>% 
    mutate(Dates = as.Date(as.numeric(Dates), origin = "1899-12-30", tz = "GMT"),
           across(c(PX_MID:PX_BID),as.numeric),
           ISIN = name)
  
  bbg_price_list[[i]] <- df
  
}

bbg_price_list_long <- bind_rows(bbg_price_list)


bbg_hr_dataset <- bbg_hr_dataset %>% 
  left_join(bbg_price_list_long,
            by = c("Dates","ISIN"))

#testing if JGB study with prices = yield version 

bbg_hr_dataset <- bbg_hr_dataset %>% 
  mutate(bid_ask_v3 =
           ((PX_ASK - PX_BID)/rowMeans(bbg_hr_dataset[,c("PX_ASK","PX_BID")])))


# BOE maturity sectors ----------------------------------------------------

mat_sec <- read_excel(here('output','boe_dataset_mat_sec.xlsx'),
           sheet = "boe_dataset (2)") %>% 
  select(operation_date, ISIN, maturity_sector)


boe_dataset_mat_sec <- boe_dataset %>% 
  left_join(mat_sec,
            by = c("operation_date", "ISIN"))

boe_dataset_mat_sec <- boe_dataset_mat_sec %>% 
  mutate(operation_date = as.Date.POSIXct(operation_date))

identical(boe_dataset,
          boe_dataset_mat_sec %>% 
            select(-maturity_sector))

boe_dataset <- boe_dataset_mat_sec


# BBG FULL maturity sectors ----------------------------------------------------

write.csv(bbg_full_dataset, here('output', 'bbg_full_dataset.csv'))


bbg_full_dataset_mat_sec <- read_excel(here('output', 'bbg_full_dataset.xlsx'),
                                       sheet = 'bbg_full_dataset (values)')

bbg_full_dataset_mat_sec <- bbg_full_dataset_mat_sec %>% 
  select(Dates,ISIN,maturity_sector)

bbg_full_dataset <- bbg_full_dataset %>% 
  left_join(bbg_full_dataset_mat_sec,
            by = c('Dates', 'ISIN'))


# BBG BOE maturity sectors ------------------------------------------------

bbg_dataset <- bbg_dataset %>% 
  left_join(bbg_full_dataset_mat_sec,
            by = c('Dates', 'ISIN'))




# BBG controls ------------------------------------------------------------

controls_raw <- read_xlsx(here('input','bbg_controls.xlsx'),
                          sheet = "controls (values)",
                          skip = 3) %>% 
  select(1:15)

controls <- controls_raw %>% 
  set_names(c("Dates",
              gsub("\\...*","",colnames(controls_raw[-1]))))


controls <- controls %>% 
  mutate(Dates = as.Date((Dates), origin = "1899-12-30", tz = "GMT"))


# BBG OIS GILT dataset ----------------------------------------------------------

ois_bench_raw <- read_excel(here('input', 'gilt_ois_data.xlsx'),
                            sheet = 'ois (values)',
                            skip = 2) %>% 
  select(-1)

ois_bench <- ois_bench_raw %>% 
  mutate(Dates = as.Date(...2,origin = "1899-12-30", tz = "GMT"),
         across(c(`1Y`:`50Y`), as.numeric)) %>% 
  select(-1)

gilt_bench_raw <- read_excel(here('input', 'gilt_ois_data.xlsx'),
                            sheet = 'bench_gilts (values)',
                            skip = 2) %>% 
  select(-1)

gilts_bench <- gilt_bench_raw %>% 
  mutate(Dates = as.Date(...2,origin = "1899-12-30", tz = "GMT"),
         across(c(`1Y`:`50Y`), as.numeric)) %>% 
  select(-1)

#remove OIS contracts that arent available for bench gilts
ois_bench <- ois_bench %>% 
  select(-c(`9Y`,`12Y`,`25Y`))

#remove unavailable bench gilts
gilts_bench <- gilts_bench %>% 
  select(-c(`9Y`,`12Y`,`25Y`))

gilts_bench_long <- gilts_bench %>% 
  pivot_longer(c(-15))

bench_isins_raw <- read_excel(here('input','gilt_ois_data.xlsx'),
                          sheet = 'bench_isins_daily')



bench_isins_long <- list()
for(i in 1:nrow(bench_isins_raw)){

  test <- bench_isins_raw[i,] %>% 
  select(ISIN, Tenor, From, To)

  bench_isins_long[[i]] <- tibble(Dates = seq(test$From, test$To, 'days'),
       ISIN = test$ISIN,
       Tenor = test$Tenor)
}

#Big list of dates with each tenor and isin
bench_isins <- bind_rows(bench_isins_long)

#df with timeseries of benchmark gilts with ISINs
gilts_bench_named <- gilts_bench_long %>% 
  set_names("Dates","Tenor", "value") %>% 
  left_join(bench_isins,
            by = c("Dates", "Tenor"))


# WRITE:  ------------------------------------------------------

###BOE dataset

saveRDS(boe_dataset, here('output','boe_dataset.RDS'))

write.dta(boe_dataset, here('stata','boe_dataset.dta') )

write.csv(boe_dataset, here('output','boe_dataset.csv'))

###BBG dataset w/BOE 
write.dta(bbg_dataset, here('stata','bbg_dataset.dta'))

saveRDS(bbg_dataset, here('output', 'bbg_dataset.RDS'))

write.csv(bbg_dataset, here('output','bbg_dataset.csv'))


###BBG ONLY dataset 
write.dta(bbg_full_dataset, here('stata','bbg_full_dataset.dta'))

saveRDS(bbg_full_dataset, here('output', 'bbg_full_dataset.RDS'))

###Controls dataset
write.dta(controls, here('stata','controls.dta'))

saveRDS(controls, here('output', 'controls.RDS'))
