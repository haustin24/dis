#analysis script 
library(tidyverse)
library(here)
library(lubridate)
library(readxl)
library(skimr)
library(bizdays)
library(timeDate)
library(cowplot)
library(foreign)

create.calendar("UKCal", holidays = holidayLONDON(2008:2021), weekdays = c("saturday", "sunday"))

#read in RDS files from data loading script
boe_market_freefloat_list <- readRDS(here('output', 'boe_freefloat_ownership_list.RDS'))
# dmo_prices_list <- readRDS(here('output', 'dmo_prices_list.RDS'))

freefloat_boe <- bind_rows(boe_market_freefloat_list)


freefloat_boe <- freefloat_boe %>% 
  mutate(bid_cover = proceeds_offers/proceeds_purchases)

# dmo_prices <- bind_rows(dmo_prices_list)
# freefloat_boe %>% 
#   skim()

# Adding yields t-1, tcob and t+endofweek ------------------------------------
# 
# bond_yield_function <- function(bond_df) {
#   #starting with one bond e.g. 34
#   df1 <- bond_df
#   
#   #find ISIN to filter prices df
#   v1 <- df1 %>%
#     select(ISIN) %>%
#     unique() %>%
#     pull()
#   
#   #filter dmo prices to bond
#   df2 <- dmo_prices %>%
#     filter(`ISIN Code` == v1) %>% 
#     arrange(`Close of Business Date`)
#   
#   #loop for each operation to find; yield at tcob tcob-1
#   bond_yield_row_list <- list()
#   for (i in 1:nrow(df1)) {
#     #single row of operation
#     df3 <- df1[i, ]
#     
#     #date of operation
#     v2 <- df3 %>%
#       select(operation_date) %>%
#       pull()
#     
#     #filter df2 for tcob and tcob-1 yield
#     df4 <- df2 %>%
#       filter(`Close of Business Date` <= v2) %>%
#       arrange(`Close of Business Date`) %>% 
#       tail(n = 2L) %>%
#       select(`Yield (%)`) %>%
#       mutate(period = c('tcob-1', 'tcob')) %>%
#       pivot_wider(names_from = period, values_from = 'Yield (%)')
#     
#     #find period change
#     df4 <- df4 %>%
#       mutate(period_change = tcob - `tcob-1`)
#     
#     #combine yield data back with original observation
#     bond_yield_row_list[[i]] <- bind_cols(df3, df4)
#     
#   }
#   
#   return(bind_rows(bond_yield_row_list))
# }
# 
# test <- bond_yield_function(boe_market_freefloat_list[[34]] %>% 
#                               filter(operation_date < ymd("2017-07-01")))
# 
# test %>% 
#   ggplot(aes(x = percent_of_freefloat, y = period_change))+
#   geom_point()+
#   geom_smooth(method=lm, level=0.95)

# linear_model <- lm(test$percent_of_freefloat ~ test$period_change)
# 
# summary(linear_model)

boe_isins <- freefloat_boe$ISIN %>% unique()

# write.csv(boe_isins, "output/boe_isins.CSV")


# Ownership plot ----------------------------------------------------------


ggplot(data = freefloat_boe)+
  geom_line(aes(x = operation_date, y = percent_of_freefloat, colour = ISIN))+
  theme(legend.position = 'none')

percentiles_owership <- freefloat_boe$percent_of_freefloat %>% 
  quantile(probs = seq(0,1,0.05))

# Benchmark data ----------------------------------------------------------

#read in benchmark dates spreadsheet
benchmark_dates <- read_xlsx(here('input', 'gilt_benchmark_dates.xlsx'))

#filter freefloat data by benchmark ISINS
benchmark_freefloat <- freefloat_boe %>% 
  filter(ISIN %in% benchmark_dates$ISIN)


# Read in long gilt data from bbg -----------------------------------------

bbg_data <- readRDS(here('output','bbg_gilts_long.RDS'))

# Filtering for bonds with high ownership ---------------------------------

boe_70_plus <- freefloat_boe %>% 
  filter(percent_of_freefloat >= 0.7) %>% 
  select(ISIN) %>% 
  unique() %>% 
  pull()

boe_60_plus <- freefloat_boe %>% 
  filter(percent_of_freefloat >= 0.6) %>% 
  select(ISIN) %>% 
  unique() %>% 
  pull()

#testing with one bond that reaches >60% boe ownership 
test_bond <- boe_60_plus[1]

bond_test_1 <- freefloat_boe %>% 
  filter(ISIN == test_bond)

bond_test_1_bbg <- bbg_data %>% 
  filter(ISIN == test_bond)

bond_test_1_bbg <- bond_test_1_bbg %>% 
  mutate(BID_ASK = YLD_YTM_BID - YLD_YTM_ASK,
         REAL_VOL = PX_HIGH - PX_LOW)

p1 <- ggplot(bond_test_1_bbg)+
  geom_line(aes(x = Dates, y = REAL_VOL))

p2 <- ggplot(bond_test_1)+
  geom_line(aes(x = operation_date, y = percent_of_freefloat))

plot_grid(p1,p2, ncol = 1)



# OIS - gilt spreads ------------------------------------------------------

#PLAN:
#For each observation of every bond (maybe too much?):
#Find the residual maturity from freefloat_boe
#Construct a yield curve for date of observation
#Linear interpolate OIS rate for corresponding residual maturity
#Calculate spread 
#BOSH


boe_maturities <- read_xlsx(here('input','bbg_gilt_data.xlsx'), sheet = "boe_maturities")

boe_maturities <- boe_maturities %>% 
  mutate(maturity = dmy(Maturity),
         ISIN = str_sub(ISIN, end = -6L))

#add back maturity dates of bonds for reference
freefloat_boe <- freefloat_boe %>% 
  left_join(boe_maturities %>% 
              select(ISIN, maturity))


ois_clean <- readRDS(here('output', 'ois_clean.RDS'))


#for start of loop (test) 
lin_interp_list <- list()

for (i in 1:nrow(freefloat_boe)) {
  df <- freefloat_boe[i, ]
  
  #date of operation
  #do we need to consider operation date + 1 ?
  v0 <- df$operation_date
  isin <- df$ISIN
  
  #find MID_YTM at date
  #find MIT_YTM at less than or equal v0 (date), take final observation
  ##ERROR: if yield data isn't available: Change to find closest 
  
  isin_ytm <- bbg_data %>%
    filter(ISIN == isin &
             Dates <= v0) %>%
    tail(1) %>%
    select(YLD_YTM_MID)
  
  #add ytm to dataframe
  df <- df %>%
    mutate(isin_ytm)
  
  #residual maturity at date of operation
  #OR CLOSEST IF EXACT DATE NOT AVAILABLE
  x <- df$res_mat
  
  #filter OIS date observations, sort and final date
  v0a <- ois_clean %>% 
    filter(date <= v0) %>% 
    select(date) %>% 
    unique() %>% 
    pull() %>% 
    sort() %>% 
    last()
  
  #filter OIS data by nearest date obtained above ^
  df2 <- ois_clean %>%
    filter(date == v0a)
  
  #linear interpolation
  # y = y1+[(x-x1)(y2-y1)]/(x2-x1)
  
  #retrieve closest tenors to residual maturity
  df2 <- df2 %>%
    mutate(tenor_dif = ois_tenor - x)
  
  #lower bound
  v1 <- df2 %>%
    filter(tenor_dif < 0) %>%
    filter(tenor_dif == max(tenor_dif))
  
  #upper bound
  v2 <- df2 %>%
    filter(tenor_dif >= 0) %>%
    filter(tenor_dif == min(tenor_dif))
  
  #ois yields
  y1 <- v1$ois_yield
  y2 <- v2$ois_yield
  
  #ois tenors
  x1 <- v1$ois_tenor
  x2 <- v2$ois_tenor
  
  #linear interpolation
  y <-  y1 + ((x - x1) * (y2 - y1)) / (x2 - x1)
  
  #add back to original df
  df <- df %>%
    mutate(ois_rate_interp = y)
  
  lin_interp_list[[i]] <- df
}


# Single bond test (Bid Ask) -------------------------------------------------------

tdf1 <- freefloat_boe %>% 
  filter(ISIN == "GB0008881541")

gilt <- tdf1$ISIN %>% 
  unique()

tdf2 <- bbg_data %>% 
  filter(ISIN == gilt)


#be aware spread widens significantly after bond maturity, data glitch? 
tdf2 <- tdf2 %>% 
  select(Dates, YLD_YTM_BID, YLD_YTM_ASK) %>% 
  mutate(YLD_BID_ASK = YLD_YTM_BID - YLD_YTM_ASK)


tdf3 <- tdf2 %>% 
  select(Dates, YLD_BID_ASK) %>% 
  set_names("operation_date", "YLD_BID_ASK")

tdf1 %>% 
  left_join(tdf3) %>% 
  ggplot()+
  geom_line(aes(x = percent_of_freefloat, y = YLD_BID_ASK))



# Holding ratio Price/yield -----------------------------------------------

freefloat_yields <- freefloat_boe %>% 
  left_join(bbg_data %>% 
              select(Dates, YLD_YTM_MID, ISIN) %>% 
              set_names("operation_date", "YLD_YTM_MID", "ISIN"),
            by = c("operation_date", "ISIN"))

ggplot(freefloat_yields)+
  geom_point(aes(x = percent_of_freefloat, y = YLD_YTM_MID, color = ISIN))+ 
  theme(legend.position = "none")
  

freefloat_yields %>% 
  filter(ISIN == "GB00B3KJDQ49") %>% 
  ggplot()+
  geom_point(aes(x = operation_date, y = YLD_YTM_MID, color = ISIN))+ 
  theme(legend.position = "none")



# bid-ask spreads and yields ------------------------------------

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



# bid-ask spread day change; less than,  greater than -----------------------------------------------

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

# bid-ask spread day change; less than, end of day -------------------------

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



cor.test(updated_bond_dataset$percent_of_freefloat,
         updated_bond_dataset$YLD_YTM_MID)

ggplot(updated_bond_dataset)+
  geom_point(aes(x = percent_of_freefloat, y = `1DCHNG_EOD_YIELD_BID_ASK`))


ggplot(updated_bond_dataset)+
  geom_point(aes(x = percent_of_freefloat, y = `1DCHNG_YIELD_BID_ASK`))

lm(updated_bond_dataset$percent_of_freefloat ~
     updated_bond_dataset$`1DCHNG_EOD_YIELD_BID_ASK`) %>% 
  summary()

lm(updated_bond_dataset$percent_of_freefloat ~
     updated_bond_dataset$`1DCHNG_YIELD_BID_ASK`) %>% 
  summary()
# Write bond dataset ------------------------------------------------------


saveRDS(bond_dataset, here('output','bond_dataset.RDS'))

write.dta(bond_dataset, here('stata','bond_dataset.dta') )

# Analysis ----------------------------------------------------------------

p <- ggplot(bond_dataset)+
  geom_point(aes(x = percent_of_freefloat, y = `1DCHNG_YIELD_BID_ASK`))

p + geom_smooth(method = 'lm', se = FALSE)

model1 <- lm(bond_dataset$percent_of_freefloat ~
               bond_dataset$`1DCHNG_YIELD_BID_ASK`)

summary(model1)

model2 <- lm(bond_dataset$percent_of_freefloat ~
               bond_dataset$YLD_BID_ASK)

summary(model2)

model3 <- lm(bond_dataset$percent_of_freefloat ~
               bond_dataset$YLD_YTM_MID)

summary(model3)

ggplot(bond_dataset)+
  geom_point(aes(x = percent_of_freefloat, y = YLD_YTM_MID))

# Alternative with timeseries bond data  ----------------------------------
#starting with one bond's timeseries from bbg data and adding free-float to the data 
#additional things to add; residual maturity, on/off the run (shadow bonds)

bond_1_isin <- "GB0002404191"

bond_1 <- bbg_data %>% 
  filter(ISIN == bond_1_isin)

bond_1_list <- list()
for(i in 1:nrow(bond_1)){
  
  date_1 <- bond_1[i,] %>% 
    select(Dates) %>% 
    pull()
  
  freefloat_bond_1 <- freefloat_boe%>% 
    filter(ISIN == bond_1_isin&
             operation_date <= date_1) %>% 
    tail(1) %>% 
    select(percent_of_freefloat)
  
  bond_1_list [[i]] <- merge(bond_1 %>% 
                               filter(Dates == date_1),
                             freefloat_bond_1)
  
}

bond_1_test <- bind_rows(bond_1_list)

bond_1_test <- bond_1_test %>% 
  mutate(BID_ASK = YLD_YTM_BID -YLD_YTM_ASK)

ggplot(bond_1_test, aes(x = percent_of_freefloat, y = BID_ASK))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = 'red')

model1_test <- lm(bond_1_test$BID_ASK~bond_1_test$percent_of_freefloat)

summary(model1_test)

bond_1_test_2 <- bond_dataset %>% 
  filter(ISIN == bond_1_isin)

ggplot(bond_1_test_2, aes(x = percent_of_freefloat, y = YLD_BID_ASK))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = 'red')

model1_test_2 <- lm(bond_1_test_2$YLD_BID_ASK~bond_1_test_2$percent_of_freefloat)

summary(model1_test_2)


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


# QE programme dates ------------------------------------------------------
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


# bbg dataset -------------------------------------------------------------

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


# bid-ask spreads ---------------------------------------------------------

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

write.dta(bbg_hr_dataset, here('stata','bbg_dataset.dta'))

saveRDS(bbg_hr_dataset, here('output', 'bbg_hr_dataset.RDS'))

# -------------------------------------------------------------------------

#get amount outstanding from DMO
dmo_issuance <- bind_rows(readRDS(here('output','clean_dmo_issuance.RDS')))






