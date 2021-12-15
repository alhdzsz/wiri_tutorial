# Install the packages below 
# We load our main library
library(tidyverse)
library(WDI)
library(stargazer)
library(lubridate)
`%notin%` = Negate(`%in%`)
n_areas = 10


# Creating Loading and Inspecting Source Data  --------------------------------

# Reading in our sample procurement data
df = read_csv("Inputs/KE_example_data.csv")

# Reading Pipe Length Data (DUMMY!)
pipelength = read_csv("Inputs/KE_pipelength.csv")

# Read PPP Conversion Factor Data
bf_wb_ppp = WDI(country="KE", indicator=c("PA.NUS.PRVT.PP")) %>%
  rename(bf_wb_ppp=PA.NUS.PRVT.PP) %>%
  select(year, bf_wb_ppp, country) %>% 
  # For this example, we take the most recent conversion factor
  filter(year == 2020) %>% 
  select(country, bf_wb_ppp)

# Read Survey Data 
survey_data = read_csv("Inputs/KE_survey.csv")

# Read Water Contract Classifications
water_cpvs = read_csv("Inputs/water_cpvs.csv")

# Declare Pillar CPV Codes (Substring)
investment = c(16,31,32,34,35,42,43,48,44,45,71) 
operation = c(03,09,15,18,19,22,24,30,33,37,38,39,41,70,50,51,60,63,64,65,66,72,73,75,77,79,80,85,90,92,98,55)
pipes = c(45232150,44162500,45232411,45232440,45232100,45232121,45232130,45231300,44134000,44163130,44160000,44161000,45231100,45231110,45231112)


# Declare Main Procurement Variables
wiri_proc_vars = c("tender_title", 
                   "buyer_name", 
                   "bidder_name", 
                   "tender_cpvs",
                   "tender_year",
                   "buyer_city",
                   "tender_finalprice")

# Declare Source Procurement Risk Variables
source_cri_vars = c("tender_recordedbidscount",
                    "tender_publications_firstcallfortenderdate",
                    "tender_biddeadline",
                    "tender_publications_firstdcontractawarddate",
                    "tender_proceduretype",
                    "tender_isawarded")

# Inspecting/Calculating Integrity Risk Scores  --------------------------------

df <- df %>%
  mutate(
    #Single bidder
    singleb=ifelse(tender_recordedbidscount>1,0,1),
    singleb=as.factor(singleb),
    #Advert Period
    submp=as.duration(
      interval(tender_publications_firstcallfortenderdate,tender_biddeadline)) 
    %/% as.duration(days(1)),
    submp=ifelse(submp>365,NA,submp),
    submp=ifelse(submp<0,0,submp),
    submp10= ntile(submp, 10),
    #No Call for Tenders 
    ncft = case_when(
      is.na(tender_biddeadline) & 
        tender_isawarded==T~1,
      TRUE~0
    ),
    #Decision Period
    decp=as.duration(interval(tender_biddeadline,tender_publications_firstdcontractawarddate))
    %/% as.duration(days(1)),
    decp=ifelse(decp>365,NA,decp),
    decp=ifelse(decp<1,NA,decp),
    decp10=ntile(decp, 10),
    #Procedure Type
    proc=case_when(
      tender_proceduretype=="OPEN"~0,
      tender_proceduretype=="APPROACHING_BIDDERS"~1,
      tender_proceduretype=="DESIGN_CONTEST"~1,
      tender_proceduretype=="OTHER"~1,
      tender_proceduretype=="DPS_PURCHASE"~1,
      tender_proceduretype=="NEGOTIATED_WITH_PUBLICATION"~1,
      tender_proceduretype=="RESTRICTED"~2,
      TRUE~NA_real_)     
  )

# Risk Parameters (Testing)
df <- df %>%
  mutate(
    singleb=case_when(
      singleb==0~0,
      singleb==1~100,
      is.na(singleb)~99),
    singleb=as.factor(singleb),
    ncft=case_when(
      ncft==0~0,
      ncft==1~100,
      is.na(ncft)~99),
    ncft=as.factor(ncft),
    corr_proc=case_when( 
      proc==0~0,
      proc==1~50,
      proc==2~100,
      is.na(proc)~99),
    corr_proc=as.factor(corr_proc),
    corr_submp=case_when( 
      submp10 %in% c(1)~0,
      submp10 %in% c(7:10)~100,
      submp10 %in% c(2:6)~50,
      is.na(submp10)~99),
    corr_submp=as.factor(corr_submp),
    corr_decp=case_when(
      decp10 %in% c(5:8)~100,
      decp10 %in% c(1:4)~50,
      decp10 %in% c(9:10)~0,
      is.na(decp10)~99),
    corr_decp=as.factor(corr_decp),
  )

# Validation Regressions
model1<- glm(singleb~
               # Risk Indicators 
               corr_proc+
               corr_submp+
               corr_decp+
               # ncft+
               # Control Variables
               log(tender_finalprice)+
               as.factor(tender_year),
             family = binomial,
             data=df)

model1.1<- glm(singleb~
                 #Risk Indicators 
                 corr_proc+
                 #Control Variables
                 log(tender_finalprice)+
                 as.factor(tender_year),
               family = binomial,
               data=df)

model1.2<- glm(singleb~
                 #Risk Indicators 
                 corr_submp+
                 #Control Variables
                 log(tender_finalprice)+
                 as.factor(tender_year),
               family = binomial,
               data=df)

model1.3<- glm(singleb~
                 #Risk Indicators 
                 corr_decp+
                 #Control Variables
                 log(tender_finalprice)+
                 as.factor(tender_year),
               family = binomial,
               data=df)

model1.4<- glm(singleb~ #We drop this model since there are no levels!
                 #Risk Indicators 
                 # ncft+
                 #Control Variables
                 log(tender_finalprice)+
                 as.factor(tender_year),
               family = binomial,
               data=df)

stargazer(model1.1,model1.2, model1.3, model1, type = "text", 
          title="Single Bidding Validation",
          digits=1, 
          omit = c("tender_finalprice", "tender_year"),
          notes = "Included controls not shown are: Log Price, and Year",
          out="stepw_singleb.txt")

# Declare Predictors
main_cri_vars = c("singleb","corr_proc","corr_submp","corr_decp","ncft")

# Drop NAs and Calculate CRI
df = df %>% 
  mutate_at(main_cri_vars, as.character) %>% 
  mutate_at(main_cri_vars, 
            funs(case_when(
              . == "99" ~ NA_character_,
              TRUE ~ .))) %>% 
  mutate_at(main_cri_vars, as.numeric) 

df$cri = rowMeans(df[,main_cri_vars], na.rm = T)

df$cri_integrity = 100 - df$cri

# Creating a Water Data Frame  --------------------------------

# Clean Main Data
df = df %>% 
  mutate(
    #General cleaning
    tender_title=tolower(tender_title),
    tender_title=stringi::stri_trans_general(tender_title, "Latin-ASCII"),
    buyer_name=tolower(buyer_name),
    buyer_name=stringi::stri_trans_general(buyer_name, "Latin-ASCII"),
    bidder_name=tolower(bidder_name),
    bidder_name=stringi::stri_trans_general(bidder_name, "Latin-ASCII"),
    tender_cpvs_8=substr(tender_cpvs,1,8),
    tender_cpvs_6=substr(tender_cpvs,1,6),
    tender_cpvs_4=substr(tender_cpvs,1,4),
    tender_cpvs_2=substr(tender_cpvs,1,2),
    #CPV Codes Matching
    water_cpv=ifelse(tender_cpvs%in%unique(water_cpvs$cpv_full),1,0),
    water_cpv_8=ifelse(tender_cpvs_8%in%unique(water_cpvs$cpv_short_8),1,0),
    water_cpv_6=ifelse(tender_cpvs_6%in%unique(water_cpvs$cpv_short_6),1,0),
    water_cpv_4=ifelse(tender_cpvs_4%in%unique(water_cpvs$cpv_short_4),1,0),
    water_cpv_2=ifelse(tender_cpvs_2%in%unique(water_cpvs$cpv_short_2),1,0)
  )

# Identify Water Contracts
df = df %>% 
  mutate(
    #Buyer Name Matching
    water_buyername=case_when(
      #broad
      grepl("water",buyer_name)~1,
      grepl("sewe+",buyer_name)~1,
      grepl("pip+",buyer_name)~1,
      #narrow
      (grepl("housing",buyer_name)&grepl("development",buyer_name))~1,
      grepl("wasre|irrigation|sewer",buyer_name)~1,
      (grepl("ministry|sanitation|project|authority",buyer_name)
       &grepl("water",buyer_name))~1,
      (grepl("kenya|institute|towers|national|pipe",buyer_name)
       &grepl("water",buyer_name))~1,
      (grepl("athi",buyer_name)
       &grepl("water",buyer_name)
       &grepl("works",buyer_name))~1,
      (grepl("pipe+",buyer_name)&grepl("water|kenya|national",buyer_name))~1,
      TRUE~0),
    #Bidder Name Matching
    water_biddername=case_when(
      #broad
      grepl("water",bidder_name)~1,
      grepl("sewe+",bidder_name)~1,
      grepl("pip+",bidder_name)~1,
      #narrow
      (grepl("housing",bidder_name)&grepl("development",bidder_name))~1,
      grepl("wasre|irrigation|sewer",bidder_name)~1,
      (grepl("ministry|sanitation|project|authority",bidder_name)
       &grepl("water",bidder_name))~1,
      (grepl("kenya|institute|towers|national|pipe",bidder_name)
       &grepl("water",bidder_name))~1,
      (grepl("athi",bidder_name)
       &grepl("water",bidder_name)
       &grepl("works",bidder_name))~1,
      (grepl("pipe+",bidder_name)&grepl("water|kenya|national",bidder_name))~1,
      TRUE~0),
    #Tender Title Matching
    water_tendertitle=case_when(
      #broad
      grepl("water",tender_title)~1,
      grepl("sewe+",tender_title)~1,
      grepl("pipe",tender_title)~1,
      #narrow
      grepl("sanitation|sanitary|sewer",tender_title)~1,
      (grepl("water",tender_title)&
         grepl("network|construction|channel|system|testing",tender_title))~1,
      (grepl("water",tender_title)&
         grepl("district|treatment|channel|system|testing",tender_title))~1,
      (grepl("water",tender_title)&grepl("pipe+",tender_title))~1,
      (grepl("water",tender_title)&grepl("sewe+",tender_title))~1,
      (grepl("water",tender_title)&grepl("distill+",tender_title))~1,
      (grepl("water",tender_title)&grepl("gutt+",tender_title))~1,
      (grepl("exten+",tender_title)&grepl("pipe+",tender_title))~1,
      (grepl("water",tender_title)
       &grepl("rain",tender_title)
       &grepl("collect+",tender_title))~1,
      (grepl("water",tender_title)
       &grepl("supply",tender_title)
       &!grepl("drink+",tender_title))~1,
      TRUE~0)
  )

# Classify Contracts into Pillars
df = df %>% 
  mutate(
    #Investment Contracts
    inv_contract=ifelse(tender_cpvs_2%in%investment,1,0),
    #Operations Contracts
    op_contract=ifelse(tender_cpvs_2%in%operation,1,0),
    #CUI Contracts
    inter_contract=water_biddername,
    #Pipe Contracts
    pipe_contact=case_when(
      tender_cpvs_8%in%pipes~1,
      grepl("pipe+",tender_title)~1,
      TRUE~0)
  ) 


# Identify Top Water Contract Settlements
settlements = df %>%
  filter(water_cpv==1 | water_buyername==1 | water_tendertitle==1|water_biddername==1) %>% 
  group_by(buyer_city) %>% 
  summarise(n_water_contracts=n()) %>% 
  filter(n_water_contracts>=10) %>% 
  arrange(desc(n_water_contracts)) %>% 
  head(n_areas)

# Create Water Data Frame
df_water = df %>% 
  filter(buyer_city%in%c("Nairobi", "Mombasa")) %>%
  # filter(buyer_city%in%settlements$buyer_city) %>%
  filter(water_cpv==1 | water_buyername==1 | water_tendertitle==1 | water_biddername==1) 

# Summarizing the Water Data Frame --------------------------------

# Total Number of Contracts 
dfw_count_total = df_water %>%
  group_by(tender_year,buyer_city) %>%
  summarise(count_total=n()) %>%
  rename(settlement_name=buyer_city,
         year=tender_year)

# Number of Investment Contracts
dfw_count_inv = df_water %>%
  filter(inv_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(count_inv=n()) %>%
  rename(settlement_name=buyer_city,
         year=tender_year)

# Merging to New Summarized Data Frame
dfw_sums = left_join(dfw_count_total, dfw_count_inv) %>%
  mutate(country="Kenya")

# Number of Operations Contracts
dfw_sums = df_water %>%
  filter(op_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(count_op=n()) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Number of Pipeline Contracts
dfw_sums = df_water %>%
  filter(pipe_contact==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(count_pipe=n()) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Number of Interactions Contracts
dfw_sums = df_water %>%
  filter(inter_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(count_int=n()) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Total Average CRI Integrity Scores
dfw_sums = df_water %>%
  group_by(tender_year,buyer_city) %>%
  summarise(avg_int_all=mean(cri_integrity, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Operations Average CRI Integrity Scores
dfw_sums = df_water %>%
  filter(op_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(avg_int_op=mean(cri_integrity, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Investments Average CRI Integrity Scores
dfw_sums = df_water %>%
  filter(inv_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(avg_int_inv=mean(cri_integrity, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Interactions Average CRI Integrity Scores
dfw_sums = df_water %>%
  filter(inter_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(avg_int_inter=mean(cri_integrity, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Pipeline Average CRI Integrity Scores
dfw_sums = df_water %>%
  filter(pipe_contact==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(avg_int_pipe=mean(cri_integrity, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Total Contract Value
dfw_sums = df_water %>%
  group_by(tender_year,buyer_city) %>%
  summarise(contract_value_total=sum(tender_finalprice, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Investments Contract Value
dfw_sums = df_water %>%
  filter(inv_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(contract_value_inv=sum(tender_finalprice, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Pipeline Contract Value
dfw_sums = df_water %>%
  filter(pipe_contact==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(contract_value_pipe=sum(tender_finalprice, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Operations Contract Value
dfw_sums = df_water %>%
  filter(op_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(contract_value_op=sum(tender_finalprice, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Interactions Contract Value
dfw_sums = df_water %>%
  filter(inter_contract==1) %>%
  group_by(tender_year,buyer_city) %>%
  summarise(contract_value_int=sum(tender_finalprice, na.rm = T)) %>%
  rename(settlement_name=buyer_city,
         year=tender_year) %>% 
  right_join(dfw_sums)

# Pipe Length Data (DUMMY!)
dfw_sums = left_join(dfw_sums, pipelength)

# PPP Conversion Factor
dfw_sums = left_join(dfw_sums, bf_wb_ppp) %>% 
  mutate(total_pipe_valueinUSD = contract_value_pipe / bf_wb_ppp,
         contract_value_total_IUSD = contract_value_total / bf_wb_ppp,
         contract_value_inv_total_IUSD = contract_value_inv / bf_wb_ppp,
         contract_value_op_total_IUSD = contract_value_op / bf_wb_ppp,
         contract_value_int_total_IUSD = contract_value_int / bf_wb_ppp)

# Survey Bribery Rates 
dfw_sums = left_join(dfw_sums, survey_data)  %>%
  mutate(cui_bribery = (bribes/n)*100,
         cui_survey_int = (100-cui_bribery))


# Calculating Pipe Length Integrity --------------------------------

# Pipe length data preparation
dfw_sums = dfw_sums %>%
  mutate(lpipelength = log(pipelength),
         ltotal_pipe_valueinUSD = log(total_pipe_valueinUSD),
         pipelength_tr = pipelength - mean(pipelength, na.rm=TRUE),
         pipelength.S = scale(pipelength)
         ) %>% 
  group_by(settlement_name) %>% 
  mutate(pipelength_base = pipelength[1]) %>% 
  ungroup() %>% 
  arrange(year, settlement_name) %>%
  group_by(settlement_name) %>%
  mutate(L.pipelength = lag(pipelength)) %>% 
  ungroup() %>%
  mutate (D.pipelength = pipelength-L.pipelength, 
          L.C.pipelength = lag(pipelength_tr),
          D.C.pipelength = pipelength_tr-L.C.pipelength) %>% 
  group_by(settlement_name) %>% 
  mutate(L.total_pipe_valueinUSD = lag(ltotal_pipe_valueinUSD),
         pipelength_min = min(pipelength, na.rm = TRUE))

# Setting NA values
dfw_sums[dfw_sums=="NaN"] <- NA
dfw_sums[dfw_sums=="Inf"] <- NA
dfw_sums[dfw_sums=="-Inf"] <- NA

# Lagged Total Value of Pipes.
dfw_sums = dfw_sums %>%
  arrange(year,settlement_name) %>%
  group_by(settlement_name) %>%
  mutate(L.total_pipe_valueinUSD = lag(total_pipe_valueinUSD))

# Conditional Regression Calculation
if(is.na(unique(dfw_sums$D.pipelength))){
  dfw_sums$pipe_int = NA
} else {
  ##Best model (add residuals to data frame)
  model = lm(D.pipelength ~ log(total_pipe_valueinUSD + .0001) + pipelength_min, data = dfw_sums, subset = (dfw_sums$count_pipe >= 2), na.action = na.omit)
  tmp1 = residuals(model)
  dfw_sums[names(tmp1),"Residuals.1"]<-tmp1
  tmp1 = rstandard(model)
  dfw_sums[names(tmp1),"resid.standardized.1"]<-tmp1
  tmp1 = hatvalues(model)
  dfw_sums[names(tmp1),"yhat"]<-tmp1
  
  ##lower values mean more infrastructure missing
  pipelength_y = dfw_sums %>% filter(yhat>=0)
  with(pipelength_y, plot(D.pipelength, yhat, frame= FALSE))
  
  ## distribution
  pipelength_y <- dfw_sums %>%
    select(yhat,count_pipe)%>%
    filter (count_pipe>=2)
  hist(pipelength_y$yhat)
  
  ##standardize residuals from regression
  dfw_sums[['residuals_standardization_original']] <- (dfw_sums[['resid.standardized.1']] - min(dfw_sums[['resid.standardized.1']],na.rm=TRUE)) / diff(range(dfw_sums[['resid.standardized.1']],na.rm=TRUE))
  
  ##create pipe investment_integrity
  dfw_sums = dfw_sums %>%
    mutate (pipe_int = (residuals_standardization_original * 100))
}

# We drop our dummy pipeline integrity calculations since it is dummy data
dfw_sums$pipe_int = NA


# Calculating the Cross Sectional WIRI --------------------------------


# Calculate the Pillar Weights
weights = readRDS("Inputs/weights.rds")
cui_w = weights["cui_survey_int", "weight"] + weights["avg_cri_inter_int_100", "weight"]
op_w  = weights["avg_cri_op_int_100", "weight"]
inv_w = weights["pipe_int", "weight"] + weights["avg_cri_inv_int_100", "weight"]
pillar_weights = c(inv_w, op_w, cui_w)

# WIRI Investments
inv_int = dfw_sums %>% 
  group_by(settlement_name) %>% 
  summarise(avg_int_inv = mean(avg_int_inv, na.rm=TRUE), 
            pipe_int = mean(pipe_int, na.rm=TRUE)) 
inv_int$wiri_inv <-rowMeans(inv_int [,c("avg_int_inv", "pipe_int")], na.rm=TRUE)

# WIRI Operations
ops_int = dfw_sums %>% 
  group_by(settlement_name) %>% 
  summarise(avg_int_op=mean(avg_int_op, na.rm=TRUE)) 
ops_int$wiri_ops<-rowMeans(ops_int[,c("avg_int_op")], na.rm=TRUE)

# WIRI Integrity
cui_int = dfw_sums %>% 
  group_by(settlement_name) %>% 
  summarise(avg_int_inter = mean(avg_int_inter, na.rm=TRUE),
            cui_survey_int = mean(cui_survey_int, na.rm=TRUE))
cui_int$wiri_cui <-rowMeans(cui_int [,c("avg_int_inter", "cui_survey_int")], na.rm=TRUE)

# Aggregate Pillar Data
WIRI = left_join(cui_int, inv_int)
WIRI = left_join(WIRI,ops_int)
WIRI[is.na(WIRI)] = NA

# Drop rows where two pillars are missing 
WIRI = WIRI %>%
  filter((!is.na(wiri_inv) & !is.na(wiri_ops)) |
           (!is.na(wiri_inv) & !is.na(wiri_ops) & !is.na(wiri_cui)) |
           (!is.na(wiri_inv) & !is.na(wiri_cui)) |
           (!is.na(wiri_cui) & !is.na(wiri_ops))
  )

# Missing values are penalized as 0
WIRI = WIRI %>%
  mutate(wiri_inv=ifelse(is.na(wiri_inv),0,wiri_inv),
         wiri_ops=ifelse(is.na(wiri_ops),0,wiri_ops),
         wiri_cui=ifelse(is.na(wiri_cui),0,wiri_cui)
  )

# Weighted Means Calculations
WIRI = WIRI %>%
  rowwise() %>% 
  mutate(
    WIRI = weighted.mean(c(wiri_inv, wiri_ops, wiri_cui), w = pillar_weights, na.rm = T)
  )


# Calculating the Timeseries WIRI --------------------------------

# Pillar scores
WIRI_ts = dfw_sums %>%
  rowwise() %>% 
  mutate(
    wiri_inv_ts = mean(c(avg_int_inv, pipe_int), na.rm=TRUE),
    wiri_ops_ts = mean(avg_int_op, na.rm=TRUE),
    wiri_cui_ts = mean(c(avg_int_inter, cui_survey_int), na.rm=TRUE)
  )
WIRI_ts[is.na(WIRI_ts)] = NA 


# Drop rows where two pillars are missing
WIRI_ts = WIRI_ts %>%
  filter((!is.na(wiri_inv_ts) & !is.na(wiri_ops_ts)) |
           (!is.na(wiri_inv_ts) & !is.na(wiri_ops_ts) & !is.na(wiri_cui_ts)) |
           (!is.na(wiri_inv_ts) & !is.na(wiri_cui_ts)) |
           (!is.na(wiri_cui_ts) & !is.na(wiri_ops_ts))
  )

# Missing values are penalized as 0
WIRI_ts = WIRI_ts %>%
  mutate(wiri_inv_ts = ifelse(is.na(wiri_inv_ts),0,wiri_inv_ts),
         wiri_ops_ts = ifelse(is.na(wiri_ops_ts),0,wiri_ops_ts),
         wiri_cui_ts = ifelse(is.na(wiri_cui_ts),0,wiri_cui_ts)
  )

# Weighted Means Calculations
WIRI_ts = WIRI_ts %>%
  rowwise() %>% 
  mutate(
    WIRI_ts = weighted.mean(c(wiri_inv_ts, wiri_ops_ts, wiri_cui_ts), w = pillar_weights, na.rm = T)
  )

# Visualizaing the WIRI --------------------------------

# Cross-Section Table 1
WIRI %>% 
  select(settlement_name, WIRI, wiri_inv, wiri_ops, wiri_cui) %>% 
  write_csv("table_1.csv")

# Time-series Table 2
WIRI_ts %>% 
  select(settlement_name, year, WIRI_ts, wiri_inv_ts, wiri_ops_ts, wiri_cui_ts) %>% 
  write_csv("table_2.csv")


# Cross-Section Figure 1
WIRI %>%
  select(settlement_name, WIRI, wiri_inv, wiri_ops, wiri_cui) %>% 
  gather(Indicator, Value, wiri_inv:wiri_cui) %>% 
  ggplot(aes(x = reorder(settlement_name, desc(WIRI)), y = Value)) +
  geom_col(aes(fill = Indicator), position = position_dodge(width=.9), width = .8) +
  geom_col(data = WIRI, aes(x = settlement_name, y = WIRI), color = "black", fill = "#14b795", alpha = 0.2) +
  geom_hline(yintercept = mean(WIRI$WIRI, na.rm = T), linetype="dashed")+
  geom_label(data = WIRI, aes(x = settlement_name, y = WIRI, label = round(WIRI, digits = 1)), 
             color = "black",
             size = 4)+
  coord_cartesian(ylim = c(0,100)) +
  labs(x=NULL, 
       y="WIRI Score",
       fill="WIRI Pillar:",
       subtitle = sprintf("WIRI mean = %s", round(mean(WIRI$WIRI, na.rm = T), digits = 1)))
ggsave("fig1.png", width = 25, height = 15, units = "cm")


# Time-series Figure 2
WIRI_ts %>%
  mutate(year = as.integer(year)) %>% 
  ggplot(aes(x=year, y=WIRI_ts))+
  geom_line(color="#14b795")+
  geom_point(color="#14b795",size=2)+
  scale_x_continuous(breaks = c(2020, 2021)) + #this is custom here!
  facet_wrap(~settlement_name)+ #usually outputs more settlements
  geom_hline(yintercept = mean(WIRI$WIRI, na.rm = T), linetype = "dashed")+
  labs(x=NULL, y="WIRI",
       subtitle = sprintf("WIRI mean = %s", round(mean(WIRI$WIRI, na.rm = T), digits = 2)))
ggsave("fig2.png", width = 25, height = 15, units = "cm")
