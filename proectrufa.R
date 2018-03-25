library("tidyverse") 
library("nycflights13") 
library("tidyr") 
library("stringr") 
library("dplyr") 
library("tibble") 
library("readr") 
tbl = read.csv("eddypro.csv", skip = 1, na = c("","NA","-9999", "-9999.0"), comment = c("[")) 
tbl = tbl[-1,] 
tbl 
tbl=tbl[tbl$DOY > 62&tbl$DOY < 156,]
tbl 
glimpse(tbl) 
tbl = select(tbl, -(roll)) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>% 
  str_replace_all("[-]","_minus_") %>% 
  str_replace_all("[@]","_at_") %>% 
  str_replace_all("[$]","_dollar_") %>% 
  str_replace_all("[#]","_hash_") %>% 
  str_replace_all("[/]","_div_") %>% 
  str_replace_all("[%]","_perc_") %>% 
  str_replace_all("[&]","_amp_") %>% 
  str_replace_all("[\\^]","_power_") %>% 
  str_replace_all("[()]","_") 
glimpse(tbl) 
sapply(tbl,is.numeric) 
tbl_numeric = tbl[,sapply(tbl,is.numeric)] 
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ] 
cor_td = cor(tbl_numeric) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
vars 
formula1 = as.formula(paste("co2_flux~" , paste(vars,collapse = "+"), sep="")) 
formula1 
mod
anova(mod) 
summary(mod) 
formula2 = as.formula(paste("co2_flux~ " , paste(vars,collapse = "+"), sep=""))formula3 = as.formula(paste("co2_flux ~ Tau + rand_err_Tau + H + rand_err_h2o_flux + 
                                                                                                           +     H_strg + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_time_lag + sonic_temperature + air_temperature + air_density + air_molar_volume + es + RH + VPD + u_rot + wind_speed + u. +TKE + x_offset  + un_H + H_scf + LE_scf + un_co2_flux + un_h2o_flux + w_spikes"))
names(mod1) 
anova(mod1) 
summary(mod1) 
formula3 = as.formula(paste("co2_flux ~ Tau + rand_err_Tau + H + rand_err_h2o_flux + 
                            H_strg + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
                            h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                            air_molar_volume + es + RH + VPD + u_rot + wind_speed + u. +TKE + X.z.d..L + T. + x_offset + x_90. + un_Tau + un_H + 
                            H_scf + un_LE + LE_scf + un_co2_flux + co2_scf + un_h2o_flux + 
                            h2o_scf + u_spikes + w_spikes + u_var + v_var + w_var + h2o_var")) 
model1= lm(formula3, data = tbl)
anova(model1) 
summary(model1)
formula4 = as.formula(paste("co2_flux ~ Tau + rand_err_Tau + H + rand_err_h2o_flux + 
                            +     H_strg + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + 
                            +     h2o_time_lag + sonic_temperature + air_temperature + air_density + 
                            +     air_molar_volume + es + RH + VPD + u_rot + wind_speed + u. +TKE + un_H + 
                            +     H_scf + un_LE + LE_scf + un_co2_flux + co2_scf+ w_spikes"))
model2 = lm(formula = formula4, data = tbl)
anova(model2) 
summary(model2)
formula5 = as.formula(paste("co2_flux ~ Tau +  H + rand_err_h2o_flux+ air_density + + u_rot + u. +TKE + un_H + 
                            + H_scf + un_LE + LE_scf + un_co2_flux + w_spikes"))
model3 = lm(formula = formula5, data = tbl)
anova(model3) 
summary(model3)