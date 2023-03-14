#analysis script 

# Libraries ---------------------------------------------------------------

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


# READ:  ------------------------------------------------------------------

# boe_market_freefloat_list <- readRDS(here('output', 'boe_freefloat_ownership_list.RDS'))
bbg_dataset <- readRDS(here('output', 'bbg_dataset.RDS'))

boe_dataset <- readRDS(here('output', 'boe_dataset.RDS'))

boe_maturities <- read_xlsx(here('input','bbg_gilt_data.xlsx'), sheet = "boe_maturities")

ois_clean <- readRDS(here('output', 'ois_clean.RDS'))

dmo_issuance <- bind_rows(readRDS(here('output','clean_dmo_issuance.RDS')))

benchmark_dates <- read_xlsx(here('input', 'gilt_benchmark_dates.xlsx'))


# OLD: DMO merge ----------------------------------------------------------------


#read in RDS files from data loading script
# dmo_prices_list <- readRDS(here('output', 'dmo_prices_list.RDS'))

# boe_dataset <- bind_rows(boe_market_freefloat_list)
# 
# 
# boe_dataset <- boe_dataset %>% 
#   mutate(bid_cover = proceeds_offers/proceeds_purchases)

# dmo_prices <- bind_rows(dmo_prices_list)
# boe_dataset %>% 
#   skim()

#OLD: Adding yields t-1, tcob and t+endofweek ------------------------------------
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
#   ggplot(aes(x = holding_ratio, y = period_change))+
#   geom_point()+
#   geom_smooth(method=lm, level=0.95)

# linear_model <- lm(test$holding_ratio ~ test$period_change)
# 
# summary(linear_model)

boe_isins <- boe_dataset$ISIN %>% unique()

# write.csv(boe_isins, "output/boe_isins.CSV")


# Ownership plot ##


ggplot(data = boe_dataset)+
  geom_line(aes(x = operation_date, y = holding_ratio, colour = ISIN))+
  theme(legend.position = 'none')

percentiles_owership <- boe_dataset$holding_ratio %>% 
  quantile(probs = seq(0,1,0.05))

# Benchmark dataset ----------------------------------------------------------

#filter freefloat data by benchmark ISINS
benchmark_dataset <- boe_dataset %>% 
  filter(ISIN %in% benchmark_dates$ISIN)


#High ownership ---------------------------------

boe_70_plus <- boe_dataset %>% 
  filter(holding_ratio >= 0.7) %>% 
  select(ISIN) %>% 
  unique() %>% 
  pull()

boe_60_plus <- boe_dataset %>% 
  filter(holding_ratio >= 0.6) %>% 
  select(ISIN) %>% 
  unique() %>% 
  pull()

#testing with one bond that reaches >60% boe ownership 
test_bond <- boe_60_plus[1]

bond_test_1 <- boe_dataset %>% 
  filter(ISIN == test_bond)

bond_test_1_bbg <- bbg_data %>% 
  filter(ISIN == test_bond)

bond_test_1_bbg <- bond_test_1_bbg %>% 
  mutate(BID_ASK = YLD_YTM_BID - YLD_YTM_ASK,
         REAL_VOL = PX_HIGH - PX_LOW)

p1 <- ggplot(bond_test_1_bbg)+
  geom_line(aes(x = Dates, y = REAL_VOL))

p2 <- ggplot(bond_test_1)+
  geom_line(aes(x = operation_date, y = holding_ratio))

plot_grid(p1,p2, ncol = 1)



# OIS - gilt spreads ------------------------------------------------------

#PLAN:
#For each observation of every bond (maybe too much?):
#Find the residual maturity from boe_dataset
#Construct a yield curve for date of observation
#Linear interpolate OIS rate for corresponding residual maturity
#Calculate spread 
#BOSH

boe_maturities <- boe_maturities %>% 
  mutate(maturity = dmy(Maturity),
         ISIN = str_sub(ISIN, end = -6L))

#add back maturity dates of bonds for reference
boe_dataset <- boe_dataset %>% 
  left_join(boe_maturities %>% 
              select(ISIN, maturity))


#for start of loop (test) 
lin_interp_list <- list()

for (i in 1:nrow(boe_dataset)) {
  df <- boe_dataset[i, ]
  
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


#OLD: Single bond test (Bid Ask) -------------------------------------------------------

tdf1 <- boe_dataset %>% 
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
  geom_line(aes(x = holding_ratio, y = YLD_BID_ASK))



#OLD: Holding ratio Price/yield -----------------------------------------------

# freefloat_yields <- boe_dataset %>% 
#   left_join(bbg_data %>% 
#               select(Dates, YLD_YTM_MID, ISIN) %>% 
#               set_names("operation_date", "YLD_YTM_MID", "ISIN"),
#             by = c("operation_date", "ISIN"))
# 
# ggplot(freefloat_yields)+
#   geom_point(aes(x = holding_ratio, y = YLD_YTM_MID, color = ISIN))+ 
#   theme(legend.position = "none")
#   
# 
# freefloat_yields %>% 
#   filter(ISIN == "GB00B3KJDQ49") %>% 
#   ggplot()+
#   geom_point(aes(x = operation_date, y = YLD_YTM_MID, color = ISIN))+ 
#   theme(legend.position = "none")
# 
# 
# cor.test(updated_bond_dataset$holding_ratio,
#          updated_bond_dataset$YLD_YTM_MID)
# 
# ggplot(updated_bond_dataset)+
#   geom_point(aes(x = holding_ratio, y = `1DCHNG_EOD_YIELD_BID_ASK`))
# 
# 
# ggplot(updated_bond_dataset)+
#   geom_point(aes(x = holding_ratio, y = `1DCHNG_YIELD_BID_ASK`))
# 
# lm(updated_bond_dataset$holding_ratio ~
#      updated_bond_dataset$`1DCHNG_EOD_YIELD_BID_ASK`) %>% 
#   summary()
# 
# lm(updated_bond_dataset$holding_ratio ~
#      updated_bond_dataset$`1DCHNG_YIELD_BID_ASK`) %>% 
#   summary()


#Linear pooled OLS regression ----------------------------------------------------------------

p <- ggplot(boe_dataset)+
  geom_point(aes(x = holding_ratio, y = `1DCHNG_YIELD_BID_ASK`))

p + geom_smooth(method = 'lm', se = FALSE)

model1 <- lm(boe_dataset$holding_ratio ~
               boe_dataset$`1DCHNG_YIELD_BID_ASK`)

summary(model1)

model2 <- lm(boe_dataset$holding_ratio ~
               boe_dataset$YLD_BID_ASK)

summary(model2)

model3 <- lm(boe_dataset$holding_ratio ~
               boe_dataset$YLD_YTM_MID)

summary(model3)

ggplot(boe_dataset)+
  geom_point(aes(x = holding_ratio, y = YLD_YTM_MID))

# TEST: Alternative with timeseries bond data  ----------------------------------
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
  
  freefloat_bond_1 <- boe_dataset%>% 
    filter(ISIN == bond_1_isin&
             operation_date <= date_1) %>% 
    tail(1) %>% 
    select(holding_ratio)
  
  bond_1_list [[i]] <- merge(bond_1 %>% 
                               filter(Dates == date_1),
                             freefloat_bond_1)
  
}

bond_1_test <- bind_rows(bond_1_list)

bond_1_test <- bond_1_test %>% 
  mutate(BID_ASK = YLD_YTM_BID -YLD_YTM_ASK)

ggplot(bond_1_test, aes(x = holding_ratio, y = BID_ASK))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = 'red')

model1_test <- lm(bond_1_test$BID_ASK~bond_1_test$holding_ratio)

summary(model1_test)

bond_1_test_2 <- boe_dataset %>% 
  filter(ISIN == bond_1_isin)

ggplot(bond_1_test_2, aes(x = holding_ratio, y = YLD_BID_ASK))+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE, colour = 'red')

model1_test_2 <- lm(bond_1_test_2$YLD_BID_ASK~bond_1_test_2$holding_ratio)

summary(model1_test_2)







