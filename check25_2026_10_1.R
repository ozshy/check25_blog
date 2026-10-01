# Code for a blog titled: "Consumer use of personal checks"
# This code uses the 2021-2025 translation-level and individual public datasets
#Note: Figure 2 is processed in this code as a table. In the blog, Figure 2 stacks a chart above the table. 

# Packages used
library(ggplot2); theme_set(theme_bw())# for graphics
#library(mfx)# binomial logit marginal effects
#library(stargazer) # for displaying multinomial coefficients. Does not work with mfx
#library(texreg) # for displaying multinomial coefficients. Works with mfx (unlike stargazer). Also displays multiple regression.
#library(huxtable)#displays multiple regressions as table => advantage, since the table can be edited in R => Problem: built-in to_latex output does not run in LaTeX in my experience. 
#library(nnet)# multinomial regressions
#library(haven)# may be needed to handle haven labelled when data is converted from other software e.g. Stata
#library("xtable") #exporting to LaTeX
library(dplyr)# for sample_n
#library(scales)# to have % in ggplot axes and tables and for commas , in data frame numbers
#library(ineq)# GINI coefficient
library(Hmisc)# for cutting data into bins & weighted medians
#library(gtools)# for stars.pval function
#library("missForest")#to impute NAs, preparation for random forest (not used because cannot impute based on selected variables (no var selection.)
#library(mice)# for imputations. => cannot impute a large number of NA?
library(missRanger)# for imputations of bill based on "merch only

#++++++++++++++ 

#2025 begins####
#setwd("~/SDCPC/2025_SDCPC")#W
setwd("~/Research/SCPC_DCPC/2025_SDCPC")#H

dir()
# Read transaction dataset
trans2025_1.df = readRDS("dcpc-2025-tranlevel-public.rds")
dim(trans2025_1.df)
names(trans2025_1.df)
(sampled_num_trans_2025 = nrow(trans2025_1.df))

# Read individual dataset
indiv2025_1.df = readRDS("dcpc-2025-indlevel-public.rds")
dim(indiv2025_1.df)
names(indiv2025_1.df)
(sampled_num_resp_2025 = nrow(indiv2025_1.df))

# select needed variables from the indiv dataset
indiv2025_2.df = subset(indiv2025_1.df, select = c(id, ind_weight_all, age, chk_t_m, pa035))

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2025_1.df$date)
trans2025_2.df = subset(trans2025_1.df, date > "2025-09-30" & date < "2025-11-01")
table(trans2025_2.df$date)
#
check2025_1.df = left_join(trans2025_2.df, indiv2025_2.df, by = "id")
dim(check2025_1.df)# num of payments (transactions)
length(unique(check2025_1.df$id))# num respondents
table(check2025_1.df$date)

# select only the needed variables
check2025_2.df = subset(check2025_1.df, select = c(id, ind_weight_all, age, amnt, pi, merch, bill, in_person))
sum(is.na(check2025_2.df))# number of trans with missing obs
sum(is.na(check2025_2.df$amnt))
sum(is.na(check2025_2.df$id))
sum(is.na(check2025_2.df$age))
sum(is.na(check2025_2.df$pi))
sum(is.na(check2025_2.df$merch))
sum(is.na(check2025_2.df$merch & check2025_2.df$pi))# missing both, prob same resp

table(check2025_2.df$pi, useNA = "always")

# delete obs with NAs (missing pi and missing merch & weights)
dim(check2025_2.df)# num payments before
length(unique(check2025_2.df$id))
check2025_3.df = subset(check2025_2.df, !is.na(pi))
sum(is.na(check2025_3.df$ind_weight_all))
check2025_3.df = subset(check2025_3.df, !is.na(ind_weight_all))
dim(check2025_3.df)# num payments after
length(unique(check2025_3.df$id))
length(unique(check2025_2.df$id))-length(unique(check2025_3.df$id))# num resp removed due to missing pi and/or merch

# are there amnt < or = 0?
summary(check2025_3.df$amnt)
nrow(subset(check2025_3.df, amnt <= 0))

# impute missing bill by merchant category
table(check2025_3.df$bill, useNA = "always")
check2025_3.df = missRanger(data = check2025_3.df, formula = bill ~ merch)
table(check2025_3.df$bill, useNA = "always")

# rescale weights
names(check2025_3.df)
nrow(check2025_3.df)
sum(check2025_3.df$ind_weight_all, na.rm = T)
#newly adjusted weights: new column w
check2025_3.df$w = nrow(check2025_3.df) *check2025_3.df$ind_weight_all/sum(check2025_3.df$ind_weight_all)
sum(check2025_3.df$w)#verify sum = num trans

# basic stats for Table 1
(num_resp2025 = length(unique(check2025_3.df$id)))
(num_checks2025 = nrow(subset(check2025_3.df, pi==2)))
(num_checks_per_resp2025 = (31/3)*num_checks2025/num_resp2025)
(avg_val_checks2025 = check2025_3.df %>% filter(pi == 2) %>%
    summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2025 = check2025_3.df %>% filter(pi == 2) %>%
    summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2025 = check2025_3.df %>% filter(pi == 2) %>%
    summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2025 = check2025_3.df %>% filter(pi == 2) %>%
    summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2025 = check2025_3.df %>% filter(pi == 2) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2025 = check2025_3.df %>% filter(pi == 2) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2025 = (31/3)*wnum_checks2025/num_resp2025)

# Bills only
(num_resp2025 = length(unique(check2025_3.df$id)))
(num_checks2025_bill = nrow(subset(check2025_3.df, pi==2 & bill==1)))
(num_checks_per_resp2025_bill = (31/3)*num_checks2025_bill/num_resp2025)
(avg_val_checks2025_bill = check2025_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2025_bill = check2025_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2025_bill = check2025_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2025_bill = check2025_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2025_bill = check2025_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2025_bill = check2025_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2025_bill = (31/3)*wnum_checks2025_bill/num_resp2025)

# Non-Bills only
(num_resp2025 = length(unique(check2025_3.df$id)))
(num_checks2025_nonbill = nrow(subset(check2025_3.df, pi==2 & bill==0)))
(num_checks_per_resp2025_nonbill = (31/3)*num_checks2025_nonbill/num_resp2025)
(avg_val_checks2025_nonbill = check2025_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2025_nonbill = check2025_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2025_nonbill = check2025_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2025_nonbill = check2025_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2025_nonbill = check2025_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2025_nonbill = check2025_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2025_nonbill = (31/3)*wnum_checks2025_nonbill/num_resp2025)

# verify num bills + nonbills = total
num_checks2025
num_checks2025_bill+num_checks2025_nonbill
#verify sum of check bills + nonbills = all checks per resp
num_checks_per_resp2025 
num_checks_per_resp2025_bill+num_checks_per_resp2025_nonbill
wnum_checks_per_resp2025 
wnum_checks_per_resp2025_bill+wnum_checks_per_resp2025_nonbill

#2025 ends####

#+++++++++++++++++++

#2024 begins####

#setwd("~/SDCPC/2024_SDCPC")#W
setwd("~/Research/SCPC_DCPC/2024_SDCPC")#H
dir()
# Read transaction dataset
trans2024_1.df = readRDS("dcpc-2024-tranlevel-public.rds")
dim(trans2024_1.df)
names(trans2024_1.df)
(sampled_num_trans_2024 = nrow(trans2024_1.df))

# Read individual dataset
indiv2024_1.df = readRDS("dcpc-2024-indlevel-public.rds")
dim(indiv2024_1.df)
names(indiv2024_1.df)
(sampled_num_resp_2024 = nrow(indiv2024_1.df))

# select needed variables from the indiv dataset
indiv2024_2.df = subset(indiv2024_1.df, select = c(id, ind_weight_all, age))

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2024_1.df$date)
trans2024_2.df = subset(trans2024_1.df, date > "2024-09-30" & date < "2024-11-01")
table(trans2024_2.df$date)
#
check2024_1.df = left_join(trans2024_2.df, indiv2024_2.df, by = "id")
dim(check2024_1.df)# num of payments (transactions)
length(unique(check2024_1.df$id))# num respondents
table(check2024_1.df$date)

# select only the needed variables
check2024_2.df = subset(check2024_1.df, select = c(id, ind_weight_all, age, amnt, pi, merch, bill, in_person))
sum(is.na(check2024_2.df))# number of trans with missing obs
sum(is.na(check2024_2.df$amnt))
sum(is.na(check2024_2.df$id))
sum(is.na(check2024_2.df$age))
sum(is.na(check2024_2.df$pi))
sum(is.na(check2024_2.df$merch))
sum(is.na(check2024_2.df$merch & check2024_2.df$pi))# missing both, prob same resp
table(check2024_2.df$pi, useNA = "always")

# delete obs with NAs (missing pi and missing merch &weight)
dim(check2024_2.df)# num payments before
length(unique(check2024_2.df$id))
check2024_3.df = subset(check2024_2.df, !is.na(pi))
sum(is.na(check2024_3.df$ind_weight_all))
check2024_3.df = subset(check2024_3.df, !is.na(ind_weight_all))
dim(check2024_3.df)# num payments after
length(unique(check2024_3.df$id))
length(unique(check2024_2.df$id))-length(unique(check2024_3.df$id))# num resp removed due to missing pi and/or merch

# are there amnt < or = 0?
summary(check2024_3.df$amnt)
nrow(subset(check2024_3.df, amnt <= 0))

# rescale weights
names(check2024_3.df)
nrow(check2024_3.df)
sum(check2024_3.df$ind_weight_all, na.rm = T)
#newly adjusted weights: new column w
check2024_3.df$w = nrow(check2024_3.df) *check2024_3.df$ind_weight_all/sum(check2024_3.df$ind_weight_all)
sum(check2024_3.df$w)#verify sum = num trans

# are there amnt < or = 0? => remove those
summary(check2024_3.df$amnt)
nrow(subset(check2024_3.df, amnt <= 0))
dim(check2024_3.df)
check2024_3.df = subset(check2024_3.df, amnt > 0)
dim(check2024_3.df)

# impute missing bill by merchant category
table(check2024_3.df$bill, useNA = "always")
check2024_3.df = missRanger(data = check2024_3.df, formula = bill ~ merch)
table(check2024_3.df$bill, useNA = "always")

# basic stats for Table 1
(num_resp2024 = length(unique(check2024_3.df$id)))
(num_checks2024 = nrow(subset(check2024_3.df, pi==2)))
(num_checks_per_resp2024 = (31/3)*num_checks2024/num_resp2024)
(avg_val_checks2024 = check2024_3.df %>% filter(pi == 2) %>%
    summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2024 = check2024_3.df %>% filter(pi == 2) %>%
    summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2024 = check2024_3.df %>% filter(pi == 2) %>%
    summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2024 = check2024_3.df %>% filter(pi == 2) %>%
    summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2024 = check2024_3.df %>% filter(pi == 2) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2024 = check2024_3.df %>% filter(pi == 2) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2024 = (31/3)*wnum_checks2024/num_resp2024)

# Bills only
(num_resp2024 = length(unique(check2024_3.df$id)))
(num_checks2024_bill = nrow(subset(check2024_3.df, pi==2 & bill==1)))
(num_checks_per_resp2024_bill = (31/3)*num_checks2024_bill/num_resp2024)
(avg_val_checks2024_bill = check2024_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2024_bill = check2024_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2024_bill = check2024_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2024_bill = check2024_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2024_bill = check2024_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2024_bill = check2024_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2024_bill = (31/3)*wnum_checks2024_bill/num_resp2024)

# Non-Bills only
(num_resp2024 = length(unique(check2024_3.df$id)))
(num_checks2024_nonbill = nrow(subset(check2024_3.df, pi==2 & bill==0)))
(num_checks_per_resp2024_nonbill = (31/3)*num_checks2024_nonbill/num_resp2024)
(avg_val_checks2024_nonbill = check2024_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2024_nonbill = check2024_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2024_nonbill = check2024_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2024_nonbill = check2024_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2024_nonbill = check2024_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2024_nonbill = check2024_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2024_nonbill = (31/3)*wnum_checks2024_nonbill/num_resp2024)

# verify num bills + nonbills = total
num_checks2024
num_checks2024_bill+num_checks2024_nonbill
#verify sum of check bills + nonbills = all checks per resp
num_checks_per_resp2024 
num_checks_per_resp2024_bill+num_checks_per_resp2024_nonbill
wnum_checks_per_resp2024 
wnum_checks_per_resp2024_bill+wnum_checks_per_resp2024_nonbill

#2024 ends####

#+++++++++++++++++++

#2023 begins####
#setwd("~/SDCPC/2023_SDCPC")#W
setwd("~/Research/SCPC_DCPC/2023_SDCPC")#H
dir()
# Read transaction dataset
trans2023_1.df = readRDS("dcpc-2023-tranlevel-public.rds")
dim(trans2023_1.df)
names(trans2023_1.df)
(sampled_num_trans_2023 = nrow(trans2023_1.df))

# Read individual dataset
indiv2023_1.df = readRDS("dcpc-2023-indlevel-public.rds")
dim(indiv2023_1.df)
names(indiv2023_1.df)
(sampled_num_resp_2023 = nrow(indiv2023_1.df))

# select needed variables from the indiv dataset
indiv2023_2.df = subset(indiv2023_1.df, select = c(id, ind_weight_all, age))

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2023_1.df$date)
trans2023_2.df = subset(trans2023_1.df, date > "2023-09-30" & date < "2023-11-01")
table(trans2023_2.df$date)
#
check2023_1.df = left_join(trans2023_2.df, indiv2023_2.df, by = "id")
dim(check2024_1.df)# num of payments (transactions)
length(unique(check2024_1.df$id))# num respondents
table(check2024_1.df$date)

# select only the needed variables
check2023_2.df = subset(check2023_1.df, select = c(id, ind_weight_all, age, amnt, pi, merch, bill, in_person))
sum(is.na(check2023_2.df))# number of trans with missing obs
sum(is.na(check2023_2.df$amnt))
sum(is.na(check2023_2.df$id))
sum(is.na(check2023_2.df$age))
sum(is.na(check2023_2.df$pi))
sum(is.na(check2023_2.df$merch))
sum(is.na(check2023_2.df$merch & check2023_2.df$pi))# missing both, prob same resp

table(check2023_2.df$pi, useNA = "always")

# delete obs with NAs (missing pi and missing merch)
dim(check2023_2.df)# num payments before
length(unique(check2023_2.df$id))
check2023_3.df = subset(check2023_2.df, !is.na(pi))
sum(is.na(check2023_3.df$ind_weight_all))
check2023_3.df = subset(check2023_3.df, !is.na(ind_weight_all))
dim(check2023_3.df)# num payments after
length(unique(check2023_3.df$id))
length(unique(check2023_2.df$id))-length(unique(check2023_3.df$id))# num resp removed due to missing pi and/or merch

# are there amnt < or = 0?
summary(check2023_3.df$amnt)
nrow(subset(check2023_3.df, amnt <= 0))

# impute missing bill by merchant category, if needed
table(check2023_3.df$bill, useNA = "always")
#check2024_3.df = missRanger(data = check2024_3.df, formula = bill ~ merch)
#table(check2024_3.df$bill, useNA = "always")

# rescale weights
names(check2023_3.df)
nrow(check2023_3.df)
sum(check2023_3.df$ind_weight_all, na.rm = T)
#newly adjusted weights: new column w
check2023_3.df$w = nrow(check2023_3.df) *check2023_3.df$ind_weight_all/sum(check2023_3.df$ind_weight_all)
sum(check2023_3.df$w)#verify sum = num trans

# basic stats for Table 1
(num_resp2023 = length(unique(check2023_3.df$id)))
(num_checks2023 = nrow(subset(check2023_3.df, pi==2)))
(num_checks_per_resp2023 = (31/3)*num_checks2023/num_resp2023)
(avg_val_checks2023 = check2023_3.df %>% filter(pi == 2) %>%
    summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2023 = check2023_3.df %>% filter(pi == 2) %>%
    summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2023 = check2023_3.df %>% filter(pi == 2) %>%
    summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2023 = check2023_3.df %>% filter(pi == 2) %>%
    summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2023 = check2023_3.df %>% filter(pi == 2) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2023 = check2023_3.df %>% filter(pi == 2) %>% summarise(wnum = as.numeric(sum(w))) %>% pull(wnum))
(wnum_checks_per_resp2023 = (31/3)*wnum_checks2023/num_resp2023)

# Bills only
(num_resp2023 = length(unique(check2023_3.df$id)))
(num_checks2023_bill = nrow(subset(check2023_3.df, pi==2 & bill==1)))
(num_checks_per_resp2023_bill = (31/3)*num_checks2023_bill/num_resp2023)
(avg_val_checks2023_bill = check2023_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2023_bill = check2023_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2023_bill = check2023_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2023_bill = check2023_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2023_bill = check2023_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2023_bill = check2023_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wnum = as.numeric(sum(w))) %>% pull(wnum))
(wnum_checks_per_resp2023_bill = (31/3)*wnum_checks2023_bill/num_resp2023)

# Non-Bills only
(num_resp2023 = length(unique(check2023_3.df$id)))
(num_checks2023_nonbill = nrow(subset(check2023_3.df, pi==2 & bill==0)))
(num_checks_per_resp2023_nonbill = (31/3)*num_checks2023_nonbill/num_resp2023)
(avg_val_checks2023_nonbill = check2023_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2023_nonbill = check2023_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2023_nonbill = check2023_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2023_nonbill = check2023_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2023_nonbill = check2023_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2023_nonbill = check2023_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2023_nonbill = (31/3)*wnum_checks2023_nonbill/num_resp2023)

# verify num bills + nonbills = total
num_checks2023
num_checks2023_bill+num_checks2023_nonbill
#verify sum of check bills + nonbills = all checks per resp
num_checks_per_resp2023 
num_checks_per_resp2023_bill+num_checks_per_resp2023_nonbill
wnum_checks_per_resp2023 
wnum_checks_per_resp2023_bill+wnum_checks_per_resp2023_nonbill

#2023 ends####

#+++++++++++++++++++

#2022 begins####
#setwd("~/SDCPC/2022_DCPC")#W
setwd("~/Research/SCPC_DCPC/2022_SDCPC")#H
dir()
# Read transaction dataset
trans2022_1.df = readRDS("dcpc-2022-tranlevel-public.rds")
dim(trans2022_1.df)
names(trans2022_1.df)
(sampled_num_trans_2022 = nrow(trans2022_1.df))

# Read individual dataset
indiv2022_1.df = readRDS("dcpc-2022-indlevel-public.rds")
dim(indiv2022_1.df)
names(indiv2022_1.df)
(sampled_num_resp_2022 = nrow(indiv2022_1.df))

# select needed variables from the indiv dataset
indiv2022_2.df = subset(indiv2022_1.df, select = c(id, ind_weight_all, age))

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2022_1.df$date)
trans2022_2.df = subset(trans2022_1.df, date > "2022-09-30" & date < "2022-11-01")
table(trans2022_2.df$date)
#
check2022_1.df = left_join(trans2022_2.df, indiv2022_2.df, by = "id")
dim(check2024_1.df)# num of payments (transactions)
length(unique(check2024_1.df$id))# num respondents
table(check2024_1.df$date)

# select only the needed variables
check2022_2.df = subset(check2022_1.df, select = c(id, ind_weight_all, age, amnt, pi, merch, bill, in_person))
sum(is.na(check2022_2.df))# number of trans with missing obs
sum(is.na(check2022_2.df$amnt))
sum(is.na(check2022_2.df$id))
sum(is.na(check2022_2.df$age))
sum(is.na(check2022_2.df$pi))
sum(is.na(check2022_2.df$merch))
sum(is.na(check2022_2.df$merch & check2022_2.df$pi))# missing both, prob same resp
sum(is.na(check2022_2.df$bill))

table(check2022_2.df$bill, useNA = "always")

# delete obs with NAs (missing pi and missing merch & weight)
dim(check2022_2.df)# num payments before
length(unique(check2022_2.df$id))
check2022_3.df = subset(check2022_2.df, !is.na(pi))
sum(is.na(check2022_3.df$ind_weight_all))
check2022_3.df = subset(check2022_3.df, !is.na(ind_weight_all))
dim(check2022_3.df)# num payments after
length(unique(check2022_3.df$id))
length(unique(check2022_2.df$id))-length(unique(check2022_3.df$id))# num resp removed due to missing pi and/or merch

# are there amnt < or = 0? => remove
summary(check2022_3.df$amnt)
nrow(subset(check2022_3.df, amnt <= 0))
dim(check2022_3.df)
check2022_3.df = subset(check2022_3.df, amnt > 0)
dim(check2022_3.df)

# impute missing bill by merchant category
table(check2022_3.df$bill, useNA = "always")
check2022_3.df = missRanger(data = check2022_3.df, formula = bill ~ merch)
table(check2022_3.df$bill, useNA = "always")

#rescale weights
names(check2022_3.df)
nrow(check2022_3.df)
sum(check2022_3.df$ind_weight_all, na.rm = T)
#newly adjusted weights: new column w
check2022_3.df$w = nrow(check2022_3.df) *check2022_3.df$ind_weight_all/sum(check2022_3.df$ind_weight_all)
sum(check2022_3.df$w)#verify sum = num trans

# basic stats for Table 1
(num_resp2022 = length(unique(check2022_3.df$id)))
(num_checks2022 = nrow(subset(check2022_3.df, pi==2)))
(num_checks_per_resp2022 = (31/3)*num_checks2022/num_resp2022)
(avg_val_checks2022 = check2022_3.df %>% filter(pi == 2) %>%
    summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2022 = check2022_3.df %>% filter(pi == 2) %>%
    summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2022 = check2022_3.df %>% filter(pi == 2) %>%
    summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2022 = check2022_3.df %>% filter(pi == 2) %>%
    summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2022 = check2022_3.df %>% filter(pi == 2) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2022 = check2022_3.df %>% filter(pi == 2) %>% summarise(wnum = as.numeric(sum(w))) %>% pull(wnum))
(wnum_checks_per_resp2022 = (31/3)*wnum_checks2022/num_resp2022)

# Bills only
(num_resp2022 = length(unique(check2022_3.df$id)))
(num_checks2022_bill = nrow(subset(check2022_3.df, pi==2 & bill==1)))
(num_checks_per_resp2022_bill = (31/3)*num_checks2022_bill/num_resp2022)
(avg_val_checks2022_bill = check2022_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2022_bill = check2022_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2022_bill = check2022_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2022_bill = check2022_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2022_bill = check2022_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2022_bill = check2022_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wnum = as.numeric(sum(w))) %>% pull(wnum))
(wnum_checks_per_resp2022_bill = (31/3)*wnum_checks2022_bill/num_resp2022)

# Non-Bills only
(num_resp2022 = length(unique(check2022_3.df$id)))
(num_checks2022_nonbill = nrow(subset(check2022_3.df, pi==2 & bill==0)))
(num_checks_per_resp2022_nonbill = (31/3)*num_checks2022_nonbill/num_resp2022)
(avg_val_checks2022_nonbill = check2022_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2022_nonbill = check2022_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2022_nonbill = check2022_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2022_nonbill = check2022_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2022_nonbill = check2022_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2022_nonbill = check2022_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wnum = sum(w)) %>% pull(wnum))
(wnum_checks_per_resp2022_nonbill = (31/3)*wnum_checks2022_nonbill/num_resp2022)

# verify num bills + nonbills = total
num_checks2022
num_checks2022_bill+num_checks2022_nonbill
#verify sum of check bills + nonbills = all checks per resp
num_checks_per_resp2022 
num_checks_per_resp2022_bill+num_checks_per_resp2022_nonbill
wnum_checks_per_resp2022 
wnum_checks_per_resp2022_bill+wnum_checks_per_resp2022_nonbill

#2022 ends####

#+++++++++++++++++++
#
#2021 begins####
#setwd("~/SDCPC/2021_DCPC")#H
setwd("~/Research/SCPC_DCPC/2021_DCPC")#W
dir()
# Read transaction dataset
trans2021_1.df = readRDS("dcpc-2021-tranlevel-public.rds")
dim(trans2021_1.df)
names(trans2021_1.df)
(sampled_num_trans_2021 = nrow(trans2021_1.df))

# Read individual dataset
indiv2021_1.df = readRDS("dcpc-2021-indlevel-public.rds")
dim(indiv2021_1.df)
names(indiv2021_1.df)
(sampled_num_resp_2021 = nrow(indiv2021_1.df))

# select needed variables from the indiv dataset
indiv2021_2.df = subset(indiv2021_1.df, select = c(id, ind_weight_all, age))

# merge the indiv with the trans datasets by id
# delete transactions not made in October
table(trans2021_1.df$date)
trans2021_2.df = subset(trans2021_1.df, date > "2021-09-30" & date < "2021-11-01")
table(trans2021_2.df$date)
#
check2021_1.df = left_join(trans2021_2.df, indiv2021_2.df, by = "id")
dim(check2024_1.df)# num of payments (transactions)
length(unique(check2024_1.df$id))# num respondents
table(check2024_1.df$date)

# select only the needed variables
check2021_2.df = subset(check2021_1.df, select = c(id, ind_weight_all, age, amnt, pi, merch, bill, in_person))
sum(is.na(check2021_2.df))# number of trans with missing obs
sum(is.na(check2021_2.df$amnt))
sum(is.na(check2021_2.df$id))
sum(is.na(check2021_2.df$age))
sum(is.na(check2021_2.df$pi))
sum(is.na(check2021_2.df$merch))
sum(is.na(check2021_2.df$merch & check2021_2.df$pi))# missing both, prob same resp

table(check2021_2.df$pi, useNA = "always")

# delete obs with NAs (missing pi and missing merch & weight)
dim(check2021_2.df)# num payments before
length(unique(check2021_2.df$id))
check2021_3.df = subset(check2021_2.df, !is.na(pi))
sum(is.na(check2021_3.df$ind_weight_all))
check2021_3.df = subset(check2021_3.df, !is.na(ind_weight_all))
dim(check2021_3.df)# num payments after
length(unique(check2021_3.df$id))
length(unique(check2021_2.df$id))-length(unique(check2021_3.df$id))# num resp removed due to missing pi and/or merch

# are there amnt < or = 0?
summary(check2021_3.df$amnt)
nrow(subset(check2021_3.df, amnt <= 0))
dim(check2021_3.df)
check2021_3.df = subset(check2021_3.df, amnt > 0)
dim(check2021_3.df)

# impute missing bill by merchant category, if needed
table(check2021_3.df$bill, useNA = "always")
check2021_3.df = missRanger(data = check2021_3.df, formula = bill ~ merch)
table(check2021_3.df$bill, useNA = "always")

# rescale weights
names(check2021_3.df)
nrow(check2021_3.df)
sum(check2021_3.df$ind_weight_all, na.rm = T)
#newly adjusted weights: new column w
check2021_3.df$w = nrow(check2021_3.df) *check2021_3.df$ind_weight_all/sum(check2021_3.df$ind_weight_all)
sum(check2021_3.df$w)#verify sum = num trans

# basic stats for Table 1
(num_resp2021 = length(unique(check2021_3.df$id)))
(num_checks2021 = nrow(subset(check2021_3.df, pi==2)))
(num_checks_per_resp2021 = (31/3)*num_checks2021/num_resp2021)
(avg_val_checks2021 = check2021_3.df %>% filter(pi == 2) %>%
    summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2021 = check2021_3.df %>% filter(pi == 2) %>%
    summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2021 = check2021_3.df %>% filter(pi == 2) %>%
    summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2021 = check2021_3.df %>% filter(pi == 2) %>%
    summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2021 = check2021_3.df %>% filter(pi == 2) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2021 = check2021_3.df %>% filter(pi == 2) %>% summarise(wnum = as.numeric(sum(w)))%>% pull(wnum))
(wnum_checks_per_resp2021 = (31/3)*wnum_checks2021/num_resp2021)

# Bills only
(num_resp2021 = length(unique(check2021_3.df$id)))
(num_checks2021_bill = nrow(subset(check2021_3.df, pi==2 & bill==1)))
(num_checks_per_resp2021_bill = (31/3)*num_checks2021_bill/num_resp2021)
(avg_val_checks2021_bill = check2021_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(avg = mean(amnt, na.rm = TRUE)) %>% pull(avg))
(med_val_checks2021_bill = check2021_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(med = median(amnt, na.rm = TRUE)) %>% pull(med))
(min_val_checks2021_bill = check2021_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(min = min(amnt, na.rm = TRUE)) %>% pull(min))
(max_val_checks2021_bill = check2021_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(max = max(amnt, na.rm = TRUE)) %>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2021_bill = check2021_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2021_bill = check2021_3.df %>% filter(pi == 2 & bill ==1) %>% summarise(wnum = as.numeric(sum(w))) %>% pull(wnum))
(wnum_checks_per_resp2021_bill = (31/3)*wnum_checks2021_bill/num_resp2021)

# Non-Bills only
(num_resp2021 = length(unique(check2021_3.df$id)))
(num_checks2021_nonbill = nrow(subset(check2021_3.df, pi==2 & bill==0)))
(num_checks_per_resp2021_nonbill = (31/3)*num_checks2021_nonbill/num_resp2021)
(avg_val_checks2021_nonbill = check2021_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(avg = mean(amnt, na.rm = TRUE))%>% pull(avg))
(med_val_checks2021_nonbill = check2021_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(med = median(amnt, na.rm = TRUE))%>% pull(med))
(min_val_checks2021_nonbill = check2021_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(min = min(amnt, na.rm = TRUE))%>% pull(min))
(max_val_checks2021_nonbill = check2021_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(max = max(amnt, na.rm = TRUE))%>% pull(max))
# added weighted values for Table 1
(wmed_val_checks2021_nonbill = check2021_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wmed = as.numeric(wtd.quantile(amnt, weights = w, probs = 0.5))) %>% pull(wmed))
(wnum_checks2021_nonbill = check2021_3.df %>% filter(pi == 2 & bill ==0) %>% summarise(wnum = as.numeric(sum(w))) %>% pull(wnum))
(wnum_checks_per_resp2021_nonbill = (31/3)*wnum_checks2021_nonbill/num_resp2021)

# verify num bills + nonbills = total
num_checks2021
num_checks2021_bill+num_checks2021_nonbill
#verify sum of check bills + nonbills = all checks per resp
num_checks_per_resp2021 
num_checks_per_resp2021_bill+num_checks_per_resp2021_nonbill
wnum_checks_per_resp2021 
wnum_checks_per_resp2021_bill+wnum_checks_per_resp2021_nonbill

#2021 ends####

#+++++++++++++++++

#Begin: Table 1: Check use 2021:2025####

(check_use_var.vec = c("Checks per respondent", "Checks per respondent (W)", "Median check value", "Median check value (W)", "Maximum check value", "Minimum check value", "Checks per respondent", "checks per respondent (W)", "Median check value", "Median check value (W)", "Maximum check value", "Minimum check value", "Checks per respondent", "Checks per respondent (W)", "Median check value", "Median check value (W)", "Maximum check value", "Minimum check value", "Fraction of bills"))
length(check_use_var.vec)

payment_type.vec = c("All payments", "All payments", "All payments", "All payments", "All payments", "All payments", "Bill payments", "Bill payments", "Bill payments", "Bill payments", "Bill payments", "Bill payments", "Nonbill payments", "Nonbill payments", "Nonbill payments", "Nonbill payments", "Nonbill payments", "Nonbill payments", "Bill/All payments")
length(payment_type.vec)

(y2021.vec = c(num_checks_per_resp2021, wnum_checks_per_resp2021, med_val_checks2021, wmed_val_checks2021, max_val_checks2021, min_val_checks2021, num_checks_per_resp2021_bill, wnum_checks_per_resp2021_bill, med_val_checks2021_bill, wmed_val_checks2021_bill, max_val_checks2021_bill, min_val_checks2021_bill, num_checks_per_resp2021_nonbill, wnum_checks_per_resp2021_nonbill, med_val_checks2021_nonbill, wmed_val_checks2021_nonbill, max_val_checks2021_nonbill, min_val_checks2021_nonbill, 100*num_checks2021_bill/num_checks2021))

(y2022.vec = c(num_checks_per_resp2022, wnum_checks_per_resp2022, med_val_checks2022, wmed_val_checks2022, max_val_checks2022, min_val_checks2022, num_checks_per_resp2022_bill, wnum_checks_per_resp2022_bill, med_val_checks2022_bill, wmed_val_checks2022_bill, max_val_checks2022_bill, min_val_checks2022_bill, num_checks_per_resp2022_nonbill, wnum_checks_per_resp2022_nonbill, med_val_checks2022_nonbill, wmed_val_checks2022_nonbill, max_val_checks2022_nonbill, min_val_checks2022_nonbill, 100*num_checks2022_bill/num_checks2022))

(y2023.vec = c(num_checks_per_resp2023, wnum_checks_per_resp2023, med_val_checks2023, wmed_val_checks2023, max_val_checks2023, min_val_checks2023, num_checks_per_resp2023_bill, wnum_checks_per_resp2023_bill, med_val_checks2023_bill, wmed_val_checks2023_bill, max_val_checks2023_bill, min_val_checks2023_bill, num_checks_per_resp2023_nonbill, wnum_checks_per_resp2023_nonbill, med_val_checks2023_nonbill, wmed_val_checks2023_nonbill, max_val_checks2023_nonbill, min_val_checks2023_nonbill, 100*num_checks2023_bill/num_checks2023))

(y2024.vec = c(num_checks_per_resp2024, wnum_checks_per_resp2024, med_val_checks2024, wmed_val_checks2024, max_val_checks2024, min_val_checks2024, num_checks_per_resp2024_bill, wnum_checks_per_resp2024_bill, med_val_checks2024_bill, wmed_val_checks2024_bill, max_val_checks2024_bill, min_val_checks2024_bill, num_checks_per_resp2024_nonbill, wnum_checks_per_resp2024_nonbill, med_val_checks2024_nonbill, wmed_val_checks2024_nonbill, max_val_checks2024_nonbill, min_val_checks2024_nonbill, 100*num_checks2024_bill/num_checks2024))

(y2025.vec = c(num_checks_per_resp2025, wnum_checks_per_resp2025, med_val_checks2025, wmed_val_checks2025, max_val_checks2025, min_val_checks2025, num_checks_per_resp2025_bill, wnum_checks_per_resp2025_bill, med_val_checks2025_bill, wmed_val_checks2025_bill, max_val_checks2025_bill, min_val_checks2025_bill, num_checks_per_resp2025_nonbill, wnum_checks_per_resp2025_nonbill, med_val_checks2025_nonbill, wmed_val_checks2025_nonbill, max_val_checks2025_nonbill, min_val_checks2025_nonbill, 100*num_checks2025_bill/num_checks2025))

# make it a data frame
(check_use.df = data.frame("Payment type" = payment_type.vec, "Variable" = check_use_var.vec, "Y2021" = y2021.vec, "Y2022" = y2022.vec, "Y2023" = y2023.vec, "Y2024" = y2024.vec, "Y2025" = y2025.vec))

# Export to CSV
#setwd("C:/Oz_local_workspace_1_checks/check25_R")# Choose a WD where the files are written: 
setwd("~/Papers/checks25_blog")
write.csv(check_use.df, "table_1w.csv", row.names = FALSE)

#Compute CAGR for checks per resp (1st row)
(check_use_cagr = (num_checks_per_resp2025/num_checks_per_resp2021)^(1/(2025-2021)) -1)
#
(check_use_bill_cagr = (num_checks_per_resp2025_bill/num_checks_per_resp2021_bill)^(1/(2025-2021)) -1)
#
(check_use_cagr = (num_checks_per_resp2025_nonbill/num_checks_per_resp2021_nonbill)^(1/(2025-2021)) -1)

#Compute CAGR for weighted checks per resp (2nd row)
(wcheck_use_cagr = (wnum_checks_per_resp2025/wnum_checks_per_resp2021)^(1/(2025-2021)) -1)
#
(wcheck_use_bill_cagr = (wnum_checks_per_resp2025_bill/wnum_checks_per_resp2021_bill)^(1/(2025-2021)) -1)
#
(wcheck_use_cagr_nonbill = (wnum_checks_per_resp2025_nonbill/wnum_checks_per_resp2021_nonbill)^(1/(2025-2021)) -1)

#End: Table 1: Check use 2021:2025####

#+++++++++++++++++++

#Begin: Figure 1 (age)####
age1.df = check2025_3.df
dim(age1.df)
length(unique(age1.df$id))
names(age1.df)

#delete payments (resp) with missing age
summary(age1.df$age)
age2.df = subset(age1.df, !is.na(age1.df$age))
summary(age2.df$age)
dim(age2.df)
length(unique(age2.df$id))

# Construct and age groups
age3.df =age2.df %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(0, 24, 34, 44, 54, 64, 75, Inf),
      labels = c("18–24","25–34","35–44","45–54","55–64", "65–74","75+"),
      right = TRUE   # <18 becomes 18. 
    )
  )
table(age3.df$age_group, useNA = "always")
sum(table(age3.df$age_group))
nrow(age3.df)
class(age3.df$age_group)

# number resp in each age group
(num_resp_by_age.df = age3.df %>% group_by(age_group) %>% summarise(num_resp_by_age = n_distinct(id)))
# verify sum
length(unique(age3.df$id))
sum(num_resp_by_age.df$num_resp_by_age)

# number checks in each age group
(num_checks_by_age.df = age3.df %>% group_by(age_group) %>% summarise(num_checks_by_age = sum(pi==2)))
# verify sum
nrow(subset(age3.df, pi==2))
sum(num_checks_by_age.df$num_checks_by_age)

#Make the above data frame for plotting
(checks_by_age1.df = data.frame("Age group" = num_resp_by_age.df$age_group, "Number respondents" = num_resp_by_age.df$num_resp_by_age ,"Num  checks" = num_checks_by_age.df$num_checks_by_age, "Checks per respondent" = (31/3)*num_checks_by_age.df$num_checks_by_age/num_resp_by_age.df$num_resp_by_age))

# Export to CSV
#setwd("C:/Oz_local_workspace_1_checks")# Choose a WD where to save the file
#write.csv(checks_by_age1.df, "figure_1.csv", row.names = FALSE)

ggplot(checks_by_age1.df, aes(x=Age.group, y=Checks.per.respondent)) +geom_point(size=3, color="black") +geom_path(aes(group=1), linetype="solid", linewidth=1.2, color="black") +labs(x="Age group", y="Number of checks per respondent") +theme(axis.text.x = element_text(size = 14, color = "black"),  axis.text.y = element_text(size = 16, color = "black"), text = element_text(size = 20)) +geom_hline(yintercept = num_checks_per_resp2025, color = "red", linetype = "longdash", linewidth = 1.2) +annotate("text", x = 2, y = 0.2+num_checks_per_resp2025, label = paste("All age groups=", round(num_checks_per_resp2025,2)), size = 6, color="red") + scale_y_continuous(breaks = seq(0, 4.5, 0.5))

#End: Figure 1 (age)####

#+++++++++++++++++++

#Begin: Figure 2: Incidences of check payment####

# Num resp who did not write any check (zero checks)
names(check2025_3.df)
(num_resp = length(unique(check2025_3.df$id)))# total num resp (all pi)
#
# num resp who wrote at least one check
length(unique(subset(check2025_3.df, pi==2)$id))
#
# num resp who wrote zero (0) checks
(num_resp_0_checks = length(unique(check2025_3.df$id)) -length(unique(subset(check2025_3.df, pi==2)$id)))
#
# data frame with number of checks written by each resp (zero excluded)
checks_by_resp.df = check2025_3.df %>% filter(pi == 2) %>%  count(id, name = "num_checks")
nrow(checks_by_resp.df)
# verify the above number is also
length(unique(subset(check2025_3.df, pi==2)$id))# num resp who wrote checks
head(checks_by_resp.df)

# num resp who wrote 1,2,3,4,... checks over 3 days
(num_resp_1_checks =nrow(subset(checks_by_resp.df, num_checks==1)))
#
(num_resp_2_checks =nrow(subset(checks_by_resp.df, num_checks==2)))
#
(num_resp_3_checks =nrow(subset(checks_by_resp.df, num_checks==3)))
#
(num_resp_4_checks =nrow(subset(checks_by_resp.df, num_checks==4)))
#
(num_resp_5_checks =nrow(subset(checks_by_resp.df, num_checks==5)))
#
(num_resp_6_checks =nrow(subset(checks_by_resp.df, num_checks==6)))
#
(num_resp_7_checks =nrow(subset(checks_by_resp.df, num_checks==7)))
#
(num_resp_8_checks =nrow(subset(checks_by_resp.df, num_checks==8)))
#
(num_resp_9_checks =nrow(subset(checks_by_resp.df, num_checks==9)))
#
(num_resp_10_checks =nrow(subset(checks_by_resp.df, num_checks==10)))
# verify all these sum up to
nrow(subset(check2025_3.df, pi==2))# num of check payments in the sample
sum(checks_by_resp.df$num_checks)

# Turn sums into percentages
num_resp_0_checks
(perc_resp_0_checks = 100*num_resp_0_checks/num_resp)
#
(perc_resp_1_checks = 100*num_resp_1_checks/num_resp)
#
(perc_resp_2_checks = 100*num_resp_2_checks/num_resp)
#
(perc_resp_3_checks = 100*num_resp_3_checks/num_resp)
#
(perc_resp_4_checks = 100*num_resp_4_checks/num_resp)
#
(perc_resp_5_checks = 100*num_resp_5_checks/num_resp)
#
(perc_resp_6_checks = 100*num_resp_6_checks/num_resp)
#
(perc_resp_7_checks = 100*num_resp_7_checks/num_resp)
#
(perc_resp_8_checks = 100*num_resp_8_checks/num_resp)
#
(perc_resp_9_checks = 100*num_resp_9_checks/num_resp)
#
(perc_resp_10_checks = 100*num_resp_10_checks/num_resp)
#
# verify sum up to 100%
perc_resp_0_checks +perc_resp_1_checks +perc_resp_2_checks +perc_resp_3_checks +perc_resp_4_checks +perc_resp_5_checks +perc_resp_6_checks +perc_resp_7_checks +perc_resp_8_checks +perc_resp_9_checks +perc_resp_10_checks 


# Make it a data frame
checkdistvar.vec = c("Number respondents", "Percentage respondents (%)")
(checkdist0.vec =c(num_resp_0_checks, perc_resp_0_checks))
(checkdist1.vec =c(num_resp_1_checks, perc_resp_1_checks))
(checkdist2.vec =c(num_resp_2_checks, perc_resp_2_checks))
(checkdist3.vec =c(num_resp_3_checks, perc_resp_3_checks))
(checkdist4.vec =c(num_resp_4_checks, perc_resp_4_checks))
(checkdist5.vec =c(num_resp_5_checks, perc_resp_5_checks))
(checkdist6.vec =c(num_resp_6_checks, perc_resp_6_checks))
(checkdist7.vec =c(num_resp_7_checks, perc_resp_7_checks))
(checkdist8.vec =c(num_resp_8_checks, perc_resp_8_checks))
(checkdist9.vec =c(num_resp_9_checks, perc_resp_9_checks))
(checkdist10.vec =c(num_resp_10_checks, perc_resp_10_checks))
#
(checkdist1.df = data.frame("Variable" =checkdistvar.vec, "0" = checkdist0.vec, "1" = checkdist1.vec, "2" = checkdist2.vec, "3" = checkdist3.vec, "4" = checkdist4.vec, "5" = checkdist5.vec, "6" = checkdist6.vec, "7" = checkdist7.vec, "8" = checkdist8.vec, "9" = checkdist9.vec, "10" = checkdist10.vec))

# Table 2 Export to CSV
#setwd("C:/Oz_local_workspace_1_checks")# Choose a WD to save this file
#write.csv(checkdist1.df, "table_2.csv", row.names = FALSE)

# Info for Table 2 (bottom, 3-day part)
length(unique(check2025_3.df$id))# num resp
nrow(subset(check2025_3.df, pi==2))# num of check payments in the sample
sum(checks_by_resp.df$num_checks)# verify

## top part of Figure 2: Incidence in past 30 days
# use the indiv dataset
names(indiv2025_2.df)
table(indiv2025_2.df$chk_t_m, useNA = "always")
# remove NA
indiv2025_3.df = subset(indiv2025_2.df, !is.na(chk_t_m))
table(indiv2025_3.df$chk_t_m, useNA = "always")
round(100*prop.table(table(indiv2025_3.df$chk_t_m)),2)# perc used checks in the past 30 days
sum(table(indiv2025_3.df$chk_t_m))
# the above weighted
# adjust the weights
indiv2025_3.df$w = nrow(indiv2025_3.df) *indiv2025_3.df$ind_weight_all/sum(indiv2025_3.df$ind_weight_all)
sum(indiv2025_3.df$w)
#
sum(subset(indiv2025_3.df, chk_t_m==0)$w)# sum of "no"
sum(subset(indiv2025_3.df, chk_t_m==1)$w)# sum of "yes"
sum(subset(indiv2025_3.df, chk_t_m==0)$w)+sum(subset(indiv2025_3.df, chk_t_m==1)$w)
# in percents
100*sum(subset(indiv2025_3.df, chk_t_m==0)$w)/nrow(indiv2025_3.df)# no %
100*sum(subset(indiv2025_3.df, chk_t_m==1)$w)/nrow(indiv2025_3.df)# yes %
100*sum(subset(indiv2025_3.df, chk_t_m==0)$w)/nrow(indiv2025_3.df)+100*sum(subset(indiv2025_3.df, chk_t_m==1)$w)/nrow(indiv2025_3.df)

## top part of Figure 2: Incidence in past 12 months
# use the indiv dataset
names(indiv2025_3.df)
table(indiv2025_3.df$pa035, useNA = "always")
# remove NA
indiv2025_4.df = subset(indiv2025_3.df, !is.na(pa035))
table(indiv2025_4.df$pa035, useNA = "always")
round(100*prop.table(table(indiv2025_4.df$pa035)),2)# perc used checks in the past 30 days
sum(table(indiv2025_4.df$pa035))
# the above weighted
# adjust the weights
indiv2025_4.df$w = nrow(indiv2025_4.df) *indiv2025_4.df$ind_weight_all/sum(indiv2025_4.df$ind_weight_all)
sum(indiv2025_4.df$w)
#
sum(subset(indiv2025_4.df, pa035==0)$w)# sum of "no"
sum(subset(indiv2025_4.df, pa035==1)$w)# sum of "yes"
# in percents
100*sum(subset(indiv2025_4.df, pa035==0)$w)/nrow(indiv2025_4.df)# no %
100*sum(subset(indiv2025_4.df, pa035==1)$w)/nrow(indiv2025_4.df)# yes %
100*sum(subset(indiv2025_4.df, pa035==0)$w)/nrow(indiv2025_4.df) + 100*sum(subset(indiv2025_4.df, pa035==1)$w)/nrow(indiv2025_4.df)


#End: Figure 2: Incidences of check payment####

#+++++++++++++++++++

#Begin: Table 2: Checks by spending type####

## Vector of 21 merchant names 
(merch1_name = "Grocery stores, convenience stores without gas stations, pharmacies")
(merch2_name = "Gas stations")
(merch3_name = "Sit-down restaurants and bars")
(merch4_name = "Fast food restaurants, coffee shops, cafeterias, food trucks")
(merch5_name = "General merchandise stores, department stores, other stores, online shopping")
(merch6_name = "General services: hair dressers, auto repair, parking lots, laundry or dry cleaning, etc.")
(merch7_name = "Arts, entertainment, recreation")
(merch8_name = "Utilities not paid to the government: electricity, natural gas, water, sewer, trash, heating oil")
(merch9_name = "Taxis, airplanes, delivery")
(merch10_name = "Telephone, internet, cable or satellite TV, video or music streaming services, movie theaters")
(merch11_name = "Building contractors, plumbers, electricians, HVAC, etc.")
(merch12_name = "Professional services: legal, accounting, architectural services; veterinarians, photographers or photo
processers")
(merch13_name = "Hotels, motels, RV parks, campsites")
(merch14_name = "Rent for apartments, homes, or other buildings, real estate companies, property managers, etc.")
(merch15_name = "Mortgage companies, credit card companies, banks, insurance companies, stock brokers, IRA funds, mutual funds, credit unions, sending remittances")
(merch16_name = "Can be a gift or repayment to a family member, friend, or co-worker. Can be a payment to somebody who did a small job for you.")
(merch17_name = "Charitable or religious donations")
(merch18_name = "Hospital, doctor, dentist, nursing homes, etc.")
(merch19_name = "Government taxes or fees")
(merch20_name = "Schools, colleges, childcare centers")
(merch21_name = "Public transportation and tolls")
# Make names a vector
(merch_name.vec = c(merch1_name, merch2_name, merch3_name, merch4_name, merch5_name, merch6_name, merch7_name, merch8_name, merch9_name, merch10_name, merch11_name, merch12_name, merch13_name, merch14_name, merch15_name, merch16_name, merch17_name, merch18_name, merch19_name, merch20_name, merch21_name))
# Finanlizing merchant name table
merch_num.vec = 1:21
(merch_name.df = data.frame(merch_num.vec, merch_name.vec))
dim(merch_name.df)

### Abbreviating 21 merchant description (for tables)
(merch1_abv = "1. Grocery store")
(merch2_abv = "2. Gas station")
(merch3_abv = "3. Restaurant/bar")
(merch4_abv = "4. Fast food/coffee shop")
(merch5_abv = "5. General merchandise store")
(merch6_abv = "6. General service")
(merch7_abv = "7. Art/entertainment")
(merch8_abv = "8. Non-government utility")
(merch9_abv = "9. Taxi/airplane/delivery")
(merch10_abv = "10. Phone/internet/cable")
(merch11_abv = "11. Contractor/plumber/ electrician")
(merch12_abv = "12. Professional service")
(merch13_abv = "13. Hotel/motel/campsite")
(merch14_abv = "14. Rent")
(merch15_abv = "15. Mortgage/insurance/credit card")
(merch16_abv = "16. Person-to-person")
(merch17_abv = "17. Charitable/religious donation")
(merch18_abv = "18. Hospital/doctor/dentist")
(merch19_abv = "19. Government taxes")
(merch20_abv = "20. School/college/childcare centers")
(merch21_abv = "21. Public transport/tolls")
# Make names a vector
(merch_abv.vec = c(merch1_abv, merch2_abv, merch3_abv, merch4_abv, merch5_abv, merch6_abv, merch7_abv, merch8_abv, merch9_abv, merch10_abv, merch11_abv, merch12_abv, merch13_abv, merch14_abv, merch15_abv, merch16_abv, merch17_abv, merch18_abv, merch19_abv, merch20_abv, merch21_abv))

# adding merch abv (super shorter for figures) description
(merch1_abv_fig = "1. Grocery store")
(merch2_abv_fig = "2. Gas station")
(merch3_abv_fig = "3. Restaurant/bar")
(merch4_abv_fig = "4. Fast food/coffee shop")
(merch5_abv_fig = "5. General merchandise")
(merch6_abv_fig = "6. General service")
(merch7_abv_fig = "7. Art/entertainment")
(merch8_abv_fig = "8. Non-gov't utility")
(merch9_abv_fig = "9. Taxi/airplane/delivery")
(merch10_abv_fig = "10. Phone/internet/cable")
(merch11_abv_fig = "11. Contractor")
(merch12_abv_fig = "12. Professional service")
(merch13_abv_fig = "13. Hotel/motel/campsite")
(merch14_abv_fig = "14. Rent")
(merch15_abv_fig = "15. Mortg/insur/credit c")
(merch16_abv_fig = "16. Person-to-person")
(merch17_abv_fig = "17. Charitable/religious")
(merch18_abv_fig = "18. Hospital/doctor/dentist")
(merch19_abv_fig = "19. Government taxes")
(merch20_abv_fig = "20. Education/childcare")
(merch21_abv_fig = "21. Public transport/tolls")
# Make names a vector
(merch_abv_fig.vec = c(merch1_abv_fig, merch2_abv_fig, merch3_abv_fig, merch4_abv_fig, merch5_abv_fig, merch6_abv_fig, merch7_abv_fig, merch8_abv_fig, merch9_abv_fig, merch10_abv_fig, merch11_abv_fig, merch12_abv_fig, merch13_abv_fig, merch14_abv_fig, merch15_abv_fig, merch16_abv_fig, merch17_abv_fig, merch18_abv_fig, merch19_abv_fig, merch20_abv_fig, merch21_abv_fig))

#analysis of all payment methods (all pi)
names(check2025_3.df)
nrow(check2025_3.df)# num payments (all pi)
length(unique(check2025_3.df$id))# num resp (all pi)
table(check2025_3.df$pi, useNA = "always")
table(check2025_3.df$merch, useNA = "always")
# remove missing merchants
check2025_4.df = subset(check2025_3.df, !is.na(merch))
nrow(check2025_4.df)
(num_payments_by_merch.vec =as.vector(unname(table(check2025_4.df$merch))))
class(num_payments_by_merch.vec)
length(num_payments_by_merch.vec)
length(unique(check2025_4.df$id))# num resp all payments

#Analysis of 
# Restrict check payments only
check_only2025_4.df = subset(check2025_4.df, pi==2)
table(check_only2025_4.df$merch, useNA = "always")# no missing merch
nrow(check_only2025_4.df)# num of check payments

# 
#num checks by spending category
(temp_table = table(factor(check_only2025_4.df$merch, levels = 1:21)))
(num_checks_only_by_merch.vec = as.integer(unname(temp_table)))
class(num_checks_only_by_merch.vec)
length(num_checks_only_by_merch.vec)
# verify sum
sum(num_checks_only_by_merch.vec)# should equal to:
nrow(check_only2025_4.df)# num of check payments

#perc out of check payments only made in each merch category
(perc_checks_payments_by_merch.vec = 100*num_checks_only_by_merch.vec/nrow(check_only2025_4.df))
# verify sums up to 100% 
sum(perc_checks_payments_by_merch.vec)

#perc out of ALL payments made in each merch category
(perc_checks_of_all_payments.vec = 100*num_checks_only_by_merch.vec/num_payments_by_merch.vec)
# sums up > 100% because each merch is separate

# make it a data frame
(check_merch1.df = data.frame("Spending type" = merch_abv.vec, "Number checks in sample" = num_checks_only_by_merch.vec, "Percentage of all checks" = perc_checks_payments_by_merch.vec, "Percentage of all payments" = perc_checks_of_all_payments.vec))

# Add total row at the bottom
(merch_abv2.vec = c(merch_abv.vec, "Total"))
#
(num_checks_only_by_merch2.vec = c(num_checks_only_by_merch.vec, sum(num_checks_only_by_merch.vec)))
#
(perc_checks_payments_by_merch2.vec = c(perc_checks_payments_by_merch.vec, sum(perc_checks_payments_by_merch.vec)))
#
(perc_checks_of_all_payments2.vec = c(perc_checks_of_all_payments.vec, sum(perc_checks_of_all_payments.vec)))
#
# Revise the data frame
(check_merch2.df = data.frame("Spending type" = merch_abv2.vec, "Number checks in sample" = num_checks_only_by_merch2.vec, "Percentage of all checks" = perc_checks_payments_by_merch2.vec, "Percentage of all payments" = perc_checks_of_all_payments2.vec))

# Table 3 Export to CSV
#setwd("C:/Oz_local_workspace_1_checks")# back to the WD
#write.csv(check_merch2.df, "table_3.csv", row.names = FALSE)

#End: Table 2: Checks by spending type####
