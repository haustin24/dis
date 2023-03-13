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

# Read in gilt data  ------------------------------------------------------

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
mat_checked_bbg_gilts %>% 
  left_join(boe_maturities %>% 
              select(ISIN, maturity),
            by = "ISIN") %>% 
  mutate(maturity - Dates) %>% 
  skim()

bbg_gilts_long <- mat_checked_bbg_gilts


saveRDS(bbg_gilts_long, here('output','bbg_gilts_long.RDS'))

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
  mutate(date = as.Date(as.numeric(ois_clean$date), origin = "1900-01-01"))

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




