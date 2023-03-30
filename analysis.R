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
library(RColorBrewer)
library(ggsci)
library(ggthemes)
library(ggpubr)


create.calendar("UKCal", holidays = holidayLONDON(2008:2021), weekdays = c("saturday", "sunday"))


# READ:  ------------------------------------------------------------------

# boe_market_freefloat_list <- readRDS(here('output', 'boe_freefloat_ownership_list.RDS'))

#with info form boe operations e.g. freefloat and bid ask spreads
bbg_dataset <- readRDS(here('output', 'bbg_dataset.RDS'))

boe_dataset <- readRDS(here('output', 'boe_dataset.RDS'))

#just bbg data 
bbg_full_dataset <- readRDS(here('output','bbg_full_dataset.RDS'))

boe_maturities <- read_xlsx(here('input','bbg_gilt_data.xlsx'), sheet = "boe_maturities")

ois_clean <- readRDS(here('output', 'ois_clean.RDS'))

dmo_issuance <- bind_rows(readRDS(here('output','clean_dmo_issuance.RDS')))

benchmark_dates <- read_xlsx(here('input', 'gilt_benchmark_dates.xlsx'))

apf <- read_xlsx(here('input', 'apf.xlsx'),
                 sheet = 'apf_values')

qe_prog <- read_xlsx(here('input', 'qe_prog_data.xlsx'),
                     sheet = 'programme')

qe_announcements <- read_excel(here('input', 'qe_prog_data.xlsx'),
                               sheet = 'announcements')

ois_gilt_hr <- readRDS(here('output','ois_gilt_hr.RDS'))

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

lin_interp_list <- list()

for(i in 1:nrow(boe_dataset)){
  test_boe <- boe_dataset[i, ]
  
  test_ois <- ois_clean %>%
    filter(date == test_boe$operation_date)
  
  #ois tenor - boe res mat
  test_ois <- test_ois %>%
    mutate(tenor_dif = ois_tenor - test_boe$res_mat)

  #linear interpolation
  # y = y1+[(x-x1)(y2-y1)]/(x2-x1)
  
  #nearest < tenor
  ois_v1 <- test_ois %>%
    filter(tenor_dif < 0) %>%
    filter(tenor_dif == max(tenor_dif))
  
  #nearest > tenor
  ois_v2 <- test_ois %>%
    filter(tenor_dif >= 0) %>%
    filter(tenor_dif == min(tenor_dif))
  
  if(nrow(ois_v1) == 0|
     nrow(ois_v2) == 0){

    lin_interp_list[[i]] <- test_boe %>%
      mutate(lin_interp_ois_yield = NA)
  }else{
    
  #tenors
  x1 <- ois_v1$ois_tenor
  x2 <- ois_v2$ois_tenor
  
  #ois yields
  y1 <- ois_v1$ois_yield
  y2 <- ois_v2$ois_yield
  
  #boe res mat
  x <- test_boe$res_mat
  
  #linear interpolation
  y <- y1 + (x - x1) * ((y2 - y1) / (x2 - x1))
  
  lin_interp_list[[i]] <- test_boe %>%
    mutate(lin_interp_ois_yield = y)
  }
  
}

boe_lin_interp <- bind_rows(lin_interp_list)
# 
# #for start of loop (test) 
# lin_interp_list <- list()
# 
# for (i in 1:nrow(boe_dataset)) {
#   df <- boe_dataset[i, ]
#   
#   #date of operation
#   #do we need to consider operation date + 1 ?
#   v0 <- df$operation_date
#   isin <- df$ISIN
#   
#   #find MID_YTM at date
#   #find MIT_YTM at less than or equal v0 (date), take final observation
#   ##ERROR: if yield data isn't available: Change to find closest 
#   
#   isin_ytm <- bbg_data %>%
#     filter(ISIN == isin &
#              Dates <= v0) %>%
#     tail(1) %>%
#     select(YLD_YTM_MID)
#   
#   #add ytm to dataframe
#   df <- df %>%
#     mutate(isin_ytm)
#   
#   #residual maturity at date of operation
#   #OR CLOSEST IF EXACT DATE NOT AVAILABLE
#   x <- df$res_mat
#   
#   #filter OIS date observations, sort and final date
#   v0a <- ois_clean %>% 
#     filter(date <= v0) %>% 
#     select(date) %>% 
#     unique() %>% 
#     pull() %>% 
#     sort() %>% 
#     last()
#   
#   #filter OIS data by nearest date obtained above ^
#   df2 <- ois_clean %>%
#     filter(date == v0a)
#   
#   #linear interpolation
#   # y = y1+[(x-x1)(y2-y1)]/(x2-x1)
#   
#   #retrieve closest tenors to residual maturity
#   df2 <- df2 %>%
#     mutate(tenor_dif = ois_tenor - x)
#   
#   #lower bound
#   v1 <- df2 %>%
#     filter(tenor_dif < 0) %>%
#     filter(tenor_dif == max(tenor_dif))
#   
#   #upper bound
#   v2 <- df2 %>%
#     filter(tenor_dif >= 0) %>%
#     filter(tenor_dif == min(tenor_dif))
#   
#   #ois yields
#   y1 <- v1$ois_yield
#   y2 <- v2$ois_yield
#   
#   #ois tenors
#   x1 <- v1$ois_tenor
#   x2 <- v2$ois_tenor
#   
#   #linear interpolation
#   y <-  y1 + ((x - x1) * (y2 - y1)) / (x2 - x1)
#   
#   #add back to original df
#   df <- df %>%
#     mutate(ois_rate_interp = y)
#   
#   lin_interp_list[[i]] <- df
# }
# 
# 



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










# DESCRIPTIVE: APF  -------------------------------------------------------

apf <- apf %>% 
  mutate(APF_CUML_PURCH = as.numeric(APF_CUML_PURCH),
         Date = as.Date.POSIXct(Date))


apf <- apf %>% 
  mutate(
    prog = case_when(
      Date >= qe_prog$start_date[1] &
        Date <= qe_prog$end_date[1] ~
        qe_prog$qe_programme[1],
      Date >= qe_prog$start_date[2] &
        Date <= qe_prog$end_date[2] ~
        qe_prog$qe_programme[2],
      Date >= qe_prog$start_date[3] &
        Date <= qe_prog$end_date[3] ~
        qe_prog$qe_programme[3],
      Date >= qe_prog$start_date[4] &
        Date <= qe_prog$end_date[4] ~
        qe_prog$qe_programme[4],
      Date >= qe_prog$start_date[5] &
        Date <= qe_prog$end_date[5] ~
        qe_prog$qe_programme[5]
    ))

apf_colours <- apf %>% 
  filter(!is.na(prog))



plot_apf <- ggplot(apf)+
  geom_line(aes(x = Date, y = APF_CUML_PURCH/1000))+
  geom_ribbon(data = apf_colours, aes(x = Date, ymin = 0, ymax= APF_CUML_PURCH/1000, fill = prog))+
  theme_hc()+
  scale_fill_manual(values=brewer.pal(6, "Dark2"), name="Programme")+
  scale_y_continuous(limits = c(0,900),breaks = seq(0,875,125), expand = c(0,0))+
  scale_x_date(date_breaks = '2 years', date_labels = '%Y')+
  labs(y = "Cumulative purchase proceeds (£bn)")+
  theme(legend.position = 'top',
        legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
        axis.text.x = element_text(vjust = 1),
        axis.text.y = element_text(hjust = 1),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))

ggsave("apf_plot.png", plot_apf, path = here('plots'))
  

ggplot(boe_dataset)+
  geom_line(aes(x = operation_date, y = cuml_nom_purch, colour = ISIN))+
  theme_hc()+
  theme(legend.position = "none")



# DESCRIPTIVE: Yield Curves -----------------------------------------------

qe1_start <- qe_prog %>% 
  filter(qe_programme == "QE1") %>% 
  select(start_date) %>% 
  pull()

yc_1 <- bbg_full_dataset %>% 
  filter(Dates == qe1_start - days(1)&
           res_mat > 90/365&
           !is.na(YLD_YTM_MID)) %>% 
  arrange(res_mat)

ggplot(yc_1)+
  geom_line(aes(x = res_mat, y = YLD_YTM_MID))

yc_2 <- bbg_full_dataset %>% 
  filter(Dates == qe1_start + days(2)&
           !is.na(YLD_YTM_MID)) %>% 
  arrange(res_mat)

ggplot(yc_2)+
  geom_line(aes(x = res_mat, y = YLD_YTM_MID))


ggplot(yc_1)+
  geom_line(aes(x = res_mat, y = YLD_YTM_MID))+
  geom_line(data = yc_2,aes(x = res_mat, y = YLD_YTM_MID), linetype = 'dotted')

yc_change <- yc_1 %>% 
  left_join(yc_2 %>% 
              select(ISIN, YLD_YTM_MID) %>% 
              set_names("ISIN", "YLD_YTM_1DCHNG"),
            by = c("ISIN")) %>% 
  select(Dates, ISIN, res_mat,
         YLD_YTM_MID,YLD_YTM_1DCHNG) %>% 
  set_names("Dates","ISIN", "res_mat",
            "Yield t-1 announcement",
            "Yield t+1 announcement")%>% 
  pivot_longer(c(4,5))

ggplot(yc_change)+
  geom_line(aes(x = res_mat, y = value, colour = name))


yield_curve_day_change_function <- function(date){
  
  yc_1 <- bbg_full_dataset %>% 
    filter(Dates == date - days(1)&
             res_mat > 90/365&
             !is.na(YLD_YTM_MID)) %>% 
    arrange(res_mat)
  
  yc_2 <- bbg_full_dataset %>% 
    filter(Dates == date + days(1)&
             !is.na(YLD_YTM_MID)) %>% 
    arrange(res_mat)
  
  yc_change <- yc_1 %>% 
    left_join(yc_2 %>% 
                select(ISIN, YLD_YTM_MID) %>% 
                set_names("ISIN", "YLD_YTM_1DCHNG"),
              by = c("ISIN")) %>% 
    select(Dates, ISIN, res_mat,
           YLD_YTM_MID,YLD_YTM_1DCHNG) %>% 
    set_names("Dates","ISIN", "res_mat",
              "t-1 announcement",
              "t+1 announcement")%>% 
    pivot_longer(c(4,5))
  
  announcement <- qe_announcements %>% 
    filter(announcement_date == date) %>% 
    select(announcement) %>% 
    pull()
  
  plot <- ggplot(yc_change)+
    geom_line(aes(x = res_mat, y = value, colour = name, linetype = name))+
    theme_hc()+
    scale_colour_manual(values=brewer.pal(3,"Dark2"), name="Yield Curve")+
    scale_linetype_manual(values= c("solid","dashed"), name="Yield Curve")+
    scale_y_continuous(limits = c(-0.15,5),breaks = seq(0,5,0.5), expand = c(0,0))+
    labs(y = "Mid Yield (year-to-maturity)",
         x = "Tenor",
         caption = paste0(announcement," announcement: ",
                          format(as.Date(date), "%d %B")))+
    theme(legend.position = 'top',
          legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          axis.text.x = element_text(vjust = 1),
          axis.text.y = element_text(hjust = 1),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"))
  
  return(list(plot, yc_change))
}

yield_curve_day_change_function(as.Date("2009-03-05"))

yc_announcement_reaction <- lapply(qe_announcements$announcement_date, yield_curve_day_change_function)

yc_react_qe1a <- yc_announcement_reaction[[1]][[1]]
yc_react_qe1b <- yc_announcement_reaction[[2]][[1]]
yc_react_qe1c <- yc_announcement_reaction[[3]][[1]]
yc_react_qe1d <- yc_announcement_reaction[[4]][[1]]
yc_react_qe2a <- yc_announcement_reaction[[5]][[1]]
yc_react_qe2b <- yc_announcement_reaction[[6]][[1]]
yc_react_qe3a <- yc_announcement_reaction[[7]][[1]]
yc_react_qe4a <- yc_announcement_reaction[[8]][[1]]
yc_react_qe5a <- yc_announcement_reaction[[9]][[1]]
yc_react_qe5b <- yc_announcement_reaction[[10]][[1]]
yc_react_qe5c <- yc_announcement_reaction[[11]][[1]]

ggarrange(yc_react_qe1a,
          yc_react_qe1b,
          yc_react_qe1c,
          yc_react_qe1d,
          common.legend = TRUE)

####REMEMBER THE Y LIMITS CHANGE WITH EACH PROGRAMME###
yield_curve_2day_change_function <- function(date){
  
  yc_1 <- bbg_full_dataset %>% 
    filter(Dates == date - days(1)&
             res_mat > 90/365&
             !is.na(YLD_YTM_MID)) %>% 
    arrange(res_mat)
  
  yc_2 <- bbg_full_dataset %>% 
    filter(Dates == date&
             !is.na(YLD_YTM_MID)) %>% 
    arrange(res_mat)
  
  yc_3 <- bbg_full_dataset %>% 
    filter(Dates == date + days(1)&
             !is.na(YLD_YTM_MID)) %>% 
    arrange(res_mat)
  
  yc_change <- yc_1 %>% 
    left_join(yc_2 %>% 
                select(ISIN, YLD_YTM_MID) %>% 
                set_names("ISIN", "YLD_YTM_COB"),
              by = c("ISIN")) %>%
    left_join(yc_3 %>% 
                select(ISIN, YLD_YTM_MID) %>% 
                set_names("ISIN", "YLD_YTM_1D"),
              by = c("ISIN")) %>% 
    select(Dates, ISIN, res_mat,
           YLD_YTM_MID,YLD_YTM_COB, YLD_YTM_1D) %>% 
    set_names("Dates","ISIN", "res_mat",
              "t-1",
              "COB",
              "t+1")%>% 
    pivot_longer(c(4,5,6))
  
  yc_change$name <- factor(yc_change$name,levels = c("t-1","COB","t+1"))
  
  announcement <- qe_announcements %>% 
    filter(announcement_date == date) %>% 
    select(announcement) %>% 
    pull()
  
  plot <- ggplot(yc_change)+
    geom_line(aes(x = res_mat, y = value, colour = name, linetype = name))+
    theme_hc()+
    scale_colour_manual(values=brewer.pal(3,"Dark2"), name="Yield Curve")+
    scale_linetype_manual(values= c("solid","dashed","longdash"), name="Yield Curve")+
    scale_y_continuous(limits = c(-0.15,1.5),breaks = seq(0,5,0.5), expand = c(0,0))+
    labs(y = "Mid Yield (YTM)",
         x = "Tenor",
         caption = paste0(announcement," announcement: ",
                          format(as.Date(date), "%d %B")))+
    theme(legend.position = 'top',
          legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"),
          axis.text.x = element_text(vjust = 1),
          axis.text.y = element_text(hjust = 1),
          axis.line.x = element_line(colour = "black"),
          axis.line.y = element_line(colour = "black"))
  
  return(list(plot, yc_change))
}


yc_announcement_reaction_qe1 <- lapply(qe_announcements$announcement_date[1:4], yield_curve_2day_change_function)

#qe1 reaction
qe1_grid <- ggarrange(yc_announcement_reaction_v2[[1]][[1]],
          yc_announcement_reaction_v2[[2]][[1]],
          yc_announcement_reaction_v2[[3]][[1]],
          yc_announcement_reaction_v2[[4]][[1]],
          common.legend = TRUE)

yc_announcement_reaction_qe2_3_4 <- lapply(qe_announcements$announcement_date[5:8], yield_curve_2day_change_function)


qe234_grid <- ggarrange(yc_announcement_reaction_qe2_3_4[[1]][[1]],
          yc_announcement_reaction_qe2_3_4[[2]][[1]],
          yc_announcement_reaction_qe2_3_4[[3]][[1]],
          yc_announcement_reaction_qe2_3_4[[4]][[1]],
          common.legend = TRUE)

yc_announcement_reaction_qe5 <- lapply(qe_announcements$announcement_date[9:11], yield_curve_2day_change_function)


qe5_grid <- ggarrange(yc_announcement_reaction_qe5[[1]][[1]],
          yc_announcement_reaction_qe5[[2]][[1]],
          yc_announcement_reaction_qe5[[3]][[1]],
          common.legend = TRUE)

#### DATA TABLE FOR YC Reaction

#all yc reactions 2 days (t-1 COB t+1) JUST FOR DATA NOT PLOTS AS LIMS NOT CHANGED
yc_announcement_reaction_v2 <- lapply(qe_announcements$announcement_date, yield_curve_2day_change_function)
yc_announcement_reaction_data_list <- list()
for(i in 1:11){
  
  yc_announcement_reaction_data_list[[i]] <- yc_announcement_reaction_v2[[i]][[2]] %>% 
    mutate(announcement_date = qe_announcements$announcement_date[i],
           announcement = qe_announcements$announcement[i])
  
}

#dataframe of reaction data 
yc_announcement_reaction_df <- bind_rows(yc_announcement_reaction_data_list)

yc_react_wide <- yc_announcement_reaction_df %>% 
  pivot_wider(names_from = name, values_from = value)

yc_react_wide <- yc_react_wide %>% 
  mutate(COB_change = COB - `t-1`,
         `t+1_change` = `t+1` - `t-1`)

yc_react_change <- yc_react_wide %>%
  select(-c(`t-1`, "COB", `t+1`)) %>% 
  pivot_longer(c(6,7))

#Add in maturity sectors from bbg_full_dataset
bbg_maturity_sectors <- bbg_full_dataset %>% 
  select(Dates, ISIN, maturity_sector)

yc_react_change <- yc_react_change %>% 
  left_join(bbg_maturity_sectors,
            by = c('Dates', 'ISIN'))

#summary table of reaction to qe announcements 
#mean, sd, IQR, max, min 
#by announcement, COB change/t+1 change, maturity sector
sum_yc_react_change<- yc_react_change %>% 
  group_by(announcement, name, maturity_sector) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            iqr = IQR(value),
            max = max(value),
            min = min(value))

write.csv(sum_yc_react_change, here('output', 'yc_react_sum_table.csv'))
# DESCRIPTIVE: YC BARCHART REACTION ---------------------------------------
#RUN DESCRIPTIVE: YIELD CURVES FIRST 

sum_yc_react_change$name[sum_yc_react_change$name==
                                   "COB_change"]="Change at COB"

sum_yc_react_change$name[sum_yc_react_change$name==
                                   "t+1_change"]="Change at t+1"

sum_yc_react_change$maturity_sector <- factor(sum_yc_react_change$maturity_sector,
                                              levels = c("untargeted_short","short","medium",
                                                         "long","untargeted_long"))

ggplot(sum_yc_react_change) +
  geom_col(aes(x = announcement, y = mean*100, fill = maturity_sector),
           position = 'dodge') +
  facet_wrap( ~ name, nrow = 2) +
  theme_hc() +
  scale_fill_manual(values = brewer.pal(5, "Set1"), name = "Maturity sector") +
  labs(y = "Basis points",
       x = "Announcement") +
  theme(
    legend.position = 'top',
    legend.background = element_rect(
      fill = "gray90",
      size = .5,
      linetype = "dotted"
    ),
    axis.text.x = element_text(vjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )


# DESCRIPTIVE: Liquidity reactions ----------------------------------------

bbg_full_dataset <- bbg_full_dataset %>% 
  mutate(bid_ask_v1 = YLD_YTM_BID - YLD_YTM_ASK,
         #bid_ask_v2 = (YLD_YTM_BID - YLD_YTM_ASK)/YLD_YTM_MID,
         bid_ask_v3 = (PX_ASK - PX_BID)/PX_MID)
  

# date <- qe_announcements$announcement_date[1]
# 
# bid_ask_levels_reaction_function <- function(date) {
#   
#   v1 <- bbg_full_dataset %>%
#     filter(Dates == as.Date(date) - days(1)) %>%
#     select(Dates, ISIN,
#            bid_ask_v1, bid_ask_v2, bid_ask_v3)
#   
#   v1 <- v1 %>%
#     set_names(c(colnames(v1)[1:2],
#                 paste0(colnames(v1[3:5]), "_t-1")))
#   
#   v2 <- bbg_full_dataset %>%
#     filter(Dates == as.Date(date)) %>%
#     select(Dates, ISIN,
#            bid_ask_v1, bid_ask_v2, bid_ask_v3)
#   
#   v2 <- v2 %>%
#     set_names(c("Date_COB",
#                 paste0(colnames(v2[2])),
#                 paste0(colnames(v2[3:5]), "_COB")))
#   
#   v3 <- bbg_full_dataset %>%
#     filter(Dates == as.Date(date) + days(1)) %>%
#     select(Dates, ISIN, res_mat, maturity_sector,
#            bid_ask_v1, bid_ask_v2, bid_ask_v3)
#   
#   v3 <- v3 %>%
#     set_names(c("Date_t+1",
#                 colnames(v3)[2:4],
#                 paste0(colnames(v3[5:7]), "_t+1")))
#   
#   z1 <- v1 %>%
#     left_join(v2,
#               by = c("ISIN")) %>%
#     left_join(v3,
#               by = c("ISIN")) %>% 
#     na.omit()
#   
#   y1 <- z1 %>%
#     select(Dates,
#            ISIN,
#            res_mat,
#            `bid_ask_v1_t-1`,
#            bid_ask_v1_COB,
#            `bid_ask_v1_t+1`) %>% 
#     pivot_longer(c(4:6))
#   
#   y1$name <- factor(y1$name,
#                     levels = c("bid_ask_v1_t-1",
#                                "bid_ask_v1_COB",
#                                "bid_ask_v1_t+1"))
#   
#   y2 <- z1 %>%
#     select(Dates,
#            ISIN,
#            res_mat,
#            `bid_ask_v2_t-1`,
#            bid_ask_v2_COB,
#            `bid_ask_v2_t+1`) %>% 
#     pivot_longer(c(4:6))
#   
#   y2$name <- factor(y2$name,
#                     levels = c("bid_ask_v2_t-1",
#                                "bid_ask_v2_COB",
#                                "bid_ask_v2_t+1"))
#   
#   y3 <- z1 %>%
#     select(Dates,
#            ISIN,
#            res_mat,
#            `bid_ask_v3_t-1`,
#            bid_ask_v3_COB,
#            `bid_ask_v3_t+1`) %>% 
#     pivot_longer(c(4:6))
#   
#   y3$name <- factor(y3$name,
#                     levels = c("bid_ask_v3_t-1",
#                                "bid_ask_v3_COB",
#                                "bid_ask_v3_t+1"))
#   
#   return(list(y1, y2, y3))
#   
# }
# 
# bid_ask_qe1a <- lapply(qe_announcements$announcement_date[1],
#                       bid_ask_reaction_function)
# 
# ggplot(bid_ask_qe1a[[1]][[1]])+
#   geom_col(aes(x = ISIN, y = value, fill = name),
#            position = 'dodge', width = 0.5)
# 
# ggplot(bid_ask_qe1a[[1]][[2]])+
#   geom_col(aes(x = ISIN, y = value, fill = name),
#            position = 'dodge', width = 0.5)
# 
# bid_ask_change_reaction_function <- function(date) {
#   
#   v1 <- bbg_full_dataset %>%
#     filter(Dates == as.Date(date) - days(1)) %>%
#     select(Dates, ISIN, res_mat,
#            bid_ask_v1, bid_ask_v2, bid_ask_v3)
#   
#   v1 <- v1 %>%
#     set_names(c(colnames(v1)[1:3],
#                 paste0(colnames(v1[4:6]), "_t-1")))
#   
#   v2 <- bbg_full_dataset %>%
#     filter(Dates == as.Date(date)) %>%
#     select(ISIN, Dates,
#            bid_ask_v1, bid_ask_v2, bid_ask_v3)
#   
#   v2 <- v2 %>%
#     set_names(c(colnames(v2)[1],
#                 "Date_COB",
#                 paste0(colnames(v2[3:5]), "_COB")))
#   
#   v3 <- bbg_full_dataset %>%
#     filter(Dates == as.Date(date) + days(1)) %>%
#     select(ISIN, Dates,
#            bid_ask_v1, bid_ask_v2, bid_ask_v3)
#   
#   v3 <- v3 %>%
#     set_names(c(colnames(v3)[1],
#                 "Date_t+1",
#                 paste0(colnames(v3[3:5]), "_t+1")))
#   
#   z1 <- v1 %>%
#     left_join(v2,
#               by = c("ISIN")) %>%
#     left_join(v3,
#               by = c("ISIN")) %>% 
#     na.omit()
#   
#   y1 <- z1 %>%
#     select(Dates,
#            ISIN,
#            res_mat,
#            `bid_ask_v1_t-1`,
#            bid_ask_v1_COB,
#            `bid_ask_v1_t+1`) %>%
#     mutate(bid_ask_v1_COB_chng =
#              bid_ask_v1_COB - `bid_ask_v1_t-1`,
#            `bid_ask_v1_t+1_chng` =
#              `bid_ask_v1_t+1` - `bid_ask_v1_t-1`) %>% 
#     select(Dates, ISIN, res_mat, 7,8) %>% 
#     pivot_longer(c(4:5))
#   
#   y1$name <- factor(y1$name,
#                     levels = c("bid_ask_v1_COB_chng",
#                                "bid_ask_v1_t+1_chng"))
#   
#   y2 <- z1 %>%
#     select(Dates,
#            ISIN,
#            res_mat,
#            `bid_ask_v2_t-1`,
#            bid_ask_v2_COB,
#            `bid_ask_v2_t+1`) %>%
#     mutate(bid_ask_v2_COB_chng =
#              bid_ask_v2_COB - `bid_ask_v2_t-1`,
#            `bid_ask_v2_t+1_chng` =
#              `bid_ask_v2_t+1` - `bid_ask_v2_t-1`) %>% 
#     select(Dates, ISIN, res_mat, 7,8) %>% 
#     pivot_longer(c(4:5))
#   
#   y2$name <- factor(y2$name,
#                     levels = c("bid_ask_v2_COB_chng",
#                                "bid_ask_v2_t+1_chng"))
#   
#   y3 <- z1 %>%
#     select(Dates,
#            ISIN,
#            res_mat,
#            `bid_ask_v3_t-1`,
#            bid_ask_v3_COB,
#            `bid_ask_v3_t+1`) %>%
#     mutate(bid_ask_v3_COB_chng =
#              bid_ask_v3_COB - `bid_ask_v3_t-1`,
#            `bid_ask_v3_t+1_chng` =
#              `bid_ask_v3_t+1` - `bid_ask_v3_t-1`) %>% 
#     select(Dates, ISIN, res_mat, 7,8) %>% 
#     pivot_longer(c(4:5))
#   
#   y3$name <- factor(y3$name,
#                     levels = c("bid_ask_v3_COB_chng",
#                                "bid_ask_v3_t+1_chng"))
#   
#   
#   return(list(y1, y2, y3))
#   
# }
# 
# bid_ask_qe1a_chng <- lapply(qe_announcements$announcement_date[1],
#                             bid_ask_change_reaction_function)
# 
# ggplot(bid_ask_qe1a_chng[[1]][[1]])+
#   geom_col(aes(x = ISIN, y = value, fill = name, shape = name),
#            position = 'dodge')
# 
# ggplot(bid_ask_qe1a_chng[[1]][[2]])+
#   geom_col(aes(x = ISIN, y = value, fill = name, shape = name),
#            position = 'dodge')
# 
# ggplot(bid_ask_qe1a_chng[[1]][[3]])+
#   geom_col(aes(x = ISIN, y = value*100, fill = name, shape = name),
#            position = 'dodge')
# 
# 



#In use 
bid_ask_change_reaction_function <- function(date) {
  
  announcement_name <- qe_announcements %>% 
    filter(announcement_date == date) %>% 
    select(announcement) %>% 
    pull()
  
  v1 <- bbg_full_dataset %>%
    filter(Dates == as.Date(date) - days(1)) %>%
    select(Dates, ISIN,
           bid_ask_v1, bid_ask_v2, bid_ask_v3)
  
  v1 <- v1 %>%
    set_names(c(colnames(v1)[1:2],
                paste0(colnames(v1[3:5]), "_t-1")))
  
  v2 <- bbg_full_dataset %>%
    filter(Dates == as.Date(date)) %>%
    select(Dates, ISIN,
           bid_ask_v1, bid_ask_v2, bid_ask_v3)
  
  v2 <- v2 %>%
    set_names(c("Date_COB",
                paste0(colnames(v2[2])),
                paste0(colnames(v2[3:5]), "_COB")))
  
  v3 <- bbg_full_dataset %>%
    filter(Dates == as.Date(date) + days(1)) %>%
    select(Dates, ISIN, res_mat, maturity_sector,
           bid_ask_v1, bid_ask_v2, bid_ask_v3)
  
  v3 <- v3 %>%
    set_names(c("Date_t+1",
                colnames(v3)[2:4],
                paste0(colnames(v3[5:7]), "_t+1")))
  
  z1 <- v1 %>%
    left_join(v2,
              by = c("ISIN")) %>%
    left_join(v3,
              by = c("ISIN")) %>% 
    na.omit()
  
  z1 <- z1 %>% 
    select(Dates,ISIN,res_mat,maturity_sector,
           `bid_ask_v1_t-1`,bid_ask_v1_COB,`bid_ask_v1_t+1`,
           `bid_ask_v2_t-1`,bid_ask_v2_COB,`bid_ask_v2_t+1`,
           `bid_ask_v3_t-1`,bid_ask_v3_COB,`bid_ask_v3_t+1`) %>% 
    mutate(
      bid_ask_v1_COB_change = bid_ask_v1_COB - `bid_ask_v1_t-1`,
      `bid_ask_v1_t+1_change` = `bid_ask_v1_t+1` - `bid_ask_v1_t-1`,
      bid_ask_v2_COB_change = bid_ask_v2_COB - `bid_ask_v2_t-1`,
      `bid_ask_v2_t+1_change` = `bid_ask_v2_t+1` - `bid_ask_v2_t-1`,
      bid_ask_v3_COB_change = bid_ask_v3_COB - `bid_ask_v3_t-1`,
      `bid_ask_v3_t+1_change` = `bid_ask_v3_t+1` - `bid_ask_v3_t-1`
    )
  
  y1 <- z1 %>% 
    select(Dates, ISIN, res_mat, maturity_sector,
           bid_ask_v1_COB_change,`bid_ask_v1_t+1_change`,
           bid_ask_v2_COB_change,`bid_ask_v2_t+1_change`,
           bid_ask_v3_COB_change, `bid_ask_v3_t+1_change`) %>% 
    mutate(announcement_date = date,
           announcement = announcement_name)
  
  y1
  
}

bid_ask_reaction <- bind_rows(lapply(qe_announcements$announcement_date, bid_ask_change_reaction_function))


sum_bid_ask_react_change <- bid_ask_reaction %>%
  pivot_longer(c(bid_ask_v1_COB_change:`bid_ask_v3_t+1_change`)) %>% 
  group_by(announcement, name, maturity_sector) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            iqr = IQR(value),
            max = max(value),
            min = min(value))


ggplot(sum_bid_ask_react_change)+
  geom_col(aes(x = announcement, y = mean, fill = maturity_sector), position = 'dodge')+
  facet_grid(~name)

#v3 bid-asks (Exact JGB study with prices)
sum_bid_ask_react_change_v3 <- sum_bid_ask_react_change %>% 
  filter(name == "bid_ask_v3_COB_change"|
           name == "bid_ask_v3_t+1_change")

sum_bid_ask_react_change_v3$maturity_sector <- factor(sum_bid_ask_react_change_v3$maturity_sector,
                                           levels = c("untargeted_short","short","medium",
                                                      "long", "untargeted_long"))
sum_bid_ask_react_change_v3$name[sum_bid_ask_react_change_v3$name==
                                   "bid_ask_v3_COB_change"]="Change at COB"

sum_bid_ask_react_change_v3$name[sum_bid_ask_react_change_v3$name==
                                   "bid_ask_v3_t+1_change"]="Change at t+1"

ggplot(sum_bid_ask_react_change_v3) +
  geom_col(aes(x = announcement, y = mean * 100, fill = maturity_sector),
           position = 'dodge') +
  facet_wrap( ~ name, nrow = 2) +
  theme_hc() +
  scale_fill_manual(values = brewer.pal(5, "Set1"), name = "Maturity sector") +
  labs(y = "Basis points",
       x = "Announcement",
       caption = "V3") +
  theme(
    legend.position = 'top',
    legend.background = element_rect(
      fill = "gray90",
      size = .5,
      linetype = "dotted"
    ),
    axis.text.x = element_text(vjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )
  
#v1 classic bid ask spread
sum_bid_ask_react_change_v1 <- sum_bid_ask_react_change %>% 
  filter(name == "bid_ask_v1_COB_change"|
           name == "bid_ask_v1_t+1_change")

sum_bid_ask_react_change_v1$maturity_sector <- factor(sum_bid_ask_react_change_v1$maturity_sector,
                                                      levels = c("untargeted_short","short","medium",
                                                                 "long", "untargeted_long"))
sum_bid_ask_react_change_v1$name[sum_bid_ask_react_change_v1$name==
                                   "bid_ask_v1_COB_change"]="Change at COB"

sum_bid_ask_react_change_v1$name[sum_bid_ask_react_change_v1$name==
                                   "bid_ask_v1_t+1_change"]="Change at t+1"

ggplot(sum_bid_ask_react_change_v1) +
  geom_col(aes(x = announcement, y = mean * 100, fill = maturity_sector),
           position = 'dodge') +
  facet_wrap(~ name, nrow = 2) +
  theme_hc() +
  scale_fill_manual(values = brewer.pal(5, "Set1"), name = "Maturity sector") +
  labs(y = "Basis points",
       x = "Announcement",
       caption = "v1") +
  theme(
    legend.position = 'top',
    legend.background = element_rect(
      fill = "gray90",
      size = .5,
      linetype = "dotted"
    ),
    axis.text.x = element_text(vjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )


write.csv(sum_bid_ask_react_change, here('output','bid_ask_react_sum_table.csv'))




# DESCRIPTIVE: Liquidity robust check -------------------------------------
#check how bid ask spreads change over the week of an announcement

date <- qe_announcements$announcement_date[2]

bis_days <- bbg_full_dataset %>% 
  select(Dates) %>% 
  unique() %>% 
  arrange()

#t-1 announcement and 1 business week days 
week_window <- bis_days %>% 
  filter(Dates >= date - days(1)&
           Dates < date + days(7))

#add reference for day since announcement
week_window <- week_window %>% 
  mutate(since_announcement =
           c("t_minus_1", "COB",
             paste0("t_plus_",seq(1,4,1))))

#filter dataset to dates
v1 <- bbg_full_dataset %>% 
  filter(Dates %in% week_window$Dates) %>% 
  select(Dates, ISIN,
         maturity_sector, bid_ask_v1,bid_ask_v3) %>% 
  left_join(week_window)


#daily change
v1 <- v1 %>%
  group_by(ISIN) %>%
  mutate(v1_daily_change = c(NA,diff(bid_ask_v1)),
         v3_daily_change = c(NA,diff(bid_ask_v3)))


#group by maturity sector
v1_sum <- v1 %>% 
  group_by(maturity_sector, since_announcement) %>%
  na.omit() %>% 
  summarise(mean_v1 = mean(v1_daily_change),
            mean_v3 = mean(v3_daily_change)) %>% 
  pivot_longer(c(3,4))

ggplot(v1_sum)+
  geom_col(aes(x = since_announcement, y = value, fill = maturity_sector),
           position = 'dodge')+
  facet_wrap(~name)




# DESCRIPTIVE: GILT-OIS Spreads -------------------------------------------
gilt_ois_change_reaction_function <- function(date) {
  
  announcement_name <- qe_announcements %>% 
    filter(announcement_date == date) %>% 
    select(announcement) %>% 
    pull()
  
  v1 <- ois_gilt_hr %>%
    filter(Dates == as.Date(date) - days(1)) %>%
    select(Dates, ISIN, gilt_ois_spread)
  
  v1 <- v1 %>%
    set_names(c(colnames(v1)[1:2],
                paste0(colnames(v1[3]), "_t-1")))
  
  v2 <- ois_gilt_hr %>%
    filter(Dates == as.Date(date)) %>%
    select(Dates, ISIN, gilt_ois_spread)
  
  v2 <- v2 %>%
    set_names(c("Date_COB",
                paste0(colnames(v2[2])),
                paste0(colnames(v2[3]), "_COB")))
  
  v3 <- ois_gilt_hr %>%
    filter(Dates == as.Date(date) + days(1)) %>%
    select(Dates, ISIN, Tenor,
           mat_sec, gilt_ois_spread)
  
  v3 <- v3 %>%
    set_names(c("Date_t+1",
                colnames(v3)[2:4],
                paste0(colnames(v3[5]), "_t+1")))
  
  z1 <- v1 %>%
    left_join(v2,
              by = c("ISIN")) %>%
    left_join(v3,
              by = c("ISIN")) %>% 
    na.omit()
  
  z1 <- z1 %>% 
    select(Dates,ISIN,Tenor,mat_sec,
           `gilt_ois_spread_t-1`, gilt_ois_spread_COB, `gilt_ois_spread_t+1`) %>% 
    mutate(
      gilt_ois_spread_COB_change = gilt_ois_spread_COB - `gilt_ois_spread_t-1`,
      `gilt_ois_spread_t+1_change` = `gilt_ois_spread_t+1` - `gilt_ois_spread_t-1`)
  
  y1 <- z1 %>% 
    select(Dates, ISIN, Tenor, mat_sec,
           gilt_ois_spread_COB_change,`gilt_ois_spread_t+1_change`)%>% 
    mutate(announcement_date = date,
           announcement = announcement_name)
  
  y1
  
}

gilt_ois_reaction <- bind_rows(lapply(qe_announcements$announcement_date, gilt_ois_change_reaction_function))

sum_gilt_ois_react_change <- gilt_ois_reaction %>%
  pivot_longer(c(gilt_ois_spread_COB_change:`gilt_ois_spread_t+1_change`)) %>% 
  group_by(announcement, name, mat_sec) %>% 
  summarise(mean = mean(value),
            sd = sd(value),
            iqr = IQR(value),
            max = max(value),
            min = min(value))

sum_gilt_ois_react_change$mat_sec <- factor(sum_gilt_ois_react_change$mat_sec,
                                              levels = c("untargeted_short","short","medium",
                                                         "long","untargeted_long"))


ggplot(sum_gilt_ois_react_change) +
  geom_col(aes(x = announcement, y = mean * 100, fill = mat_sec),
           position = 'dodge') +
  facet_wrap( ~ name, nrow = 2) +
  theme_hc() +
  scale_fill_manual(values = brewer.pal(5, "Set1"), name = "Maturity sector") +
  labs(y = "Basis points",
       x = "Announcement",
       caption = "V3") +
  theme(
    legend.position = 'top',
    legend.background = element_rect(
      fill = "gray90",
      size = .5,
      linetype = "dotted"
    ),
    axis.text.x = element_text(vjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )

write.csv(sum_gilt_ois_react_change, here('output','gilt_ois_react_sum_table.csv'))



# DESCRIPTIVE: HR Boxplots by programme ---------------------------------------------------

hr_histo_data <- boe_dataset %>%
  select(holding_ratio, prog)

hr_histo_data %>% 
ggplot(aes(x = holding_ratio, fill = prog))+
  geom_density()+
  facet_wrap(~prog)

#histogram of holding ratio per programme 
plot_hr_histo <- hr_histo_data %>% 
  ggplot(aes(x = holding_ratio, fill = prog))+
  geom_histogram()+
  theme_hc()+
  scale_fill_manual(values=brewer.pal(5,"Dark2"), name="Programme")+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  theme(legend.position = 'none',
        axis.text.x = element_text(vjust = 1),
        axis.text.y = element_text(hjust = 1),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"))+
  facet_wrap(~prog)

#density plot of holding ratio by programe
plot_hr_density <- hr_histo_data %>% 
  ggplot(aes(x = holding_ratio, fill = prog))+
  geom_density()+
  theme_hc()+
  labs(x = "Holding ratio",
       y = "Density")+
  scale_fill_manual(values=brewer.pal(5,"Dark2"), name="Programme")+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  theme(legend.position = 'none',
        axis.text.x = element_text(vjust = 1),
        axis.text.y = element_text(hjust = 1),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(
          size = 10, face = "bold"))+
  facet_wrap(~prog)


#Box plot of holding ratio by programme
plot_hr_boxplot <- ggplot(hr_histo_data)+
  geom_boxplot(aes(x = prog, y = holding_ratio, fill = prog))+
  theme_hc()+
  labs(x = "Programme",
       y = "Holding ratio")+
  scale_fill_manual(values=brewer.pal(5,"Dark2"), name="Programme")+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,0.7,0.1))+
  theme(legend.position = 'none',
        axis.text.x = element_text(vjust = 1),
        axis.text.y = element_text(hjust = 1),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        strip.text.x = element_text(
          size = 10, face = "bold"))


# DESCRIPTIVE: Holding ratio by maturity split -------------------------------------------

hr_mat_sec <- boe_dataset %>% 
  group_by(prog, maturity_sector) %>% 
  summarise(mean = mean(holding_ratio))

hr_mat_sec$maturity_sector <- factor(hr_mat_sec$maturity_sector,
                                     levels = c("short","medium","long"))

  ggplot(hr_mat_sec)+
  geom_col(aes(x = prog, y = mean, fill = maturity_sector), position = 'dodge')+
  theme_hc()+
  labs(x = "Programme",
       y = "Holding ratio")+
  scale_fill_manual(values=brewer.pal(4,"Set1")[2:4], name="Maturity sector")+
  scale_y_continuous(labels = scales::percent, breaks = seq(0,0.4,0.1))+
  theme(
    legend.position = 'top',
    legend.background = element_rect(
      fill = "gray90",
      size = .5,
      linetype = "dotted"
    ),
    axis.text.x = element_text(vjust = 1),
    axis.text.y = element_text(hjust = 1),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black")
  )






