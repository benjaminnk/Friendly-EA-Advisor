# PURPOSE: Munge and Analysis of tables for C19 DB
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-07-23
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(glitr)
    library(glamr)
    library(gisr)
    library(Wavelength)
    library(ICPIutilities)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(here)
    library(gt)
    library(googlesheets4)
    library(glue)
    library(googledrive)
    
    
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions  
  

# LOAD DATA ============================================================================  

    df_data<-read.csv("Data/Potential COVID Mock up.xlsx - dataset.csv")
    #country
    df_country<-read.csv("Data/Potential COVID Mock up.xlsx - country_metadata.csv")

# MUNGE ============================================================================
  #full dataset
    df<-left_join(df_country,df_data, by="country_id")
    
    #get just vaccine data 
    df<-df%>%
      dplyr::filter(series_id=="79713")
    
    
    #regional table
   df_region<-df_country%>%
    
     mutate_at(vars(`population_total`),~replace_na(.,0))%>%
    group_by(state_region) %>%                         # Specify group indicator
      summarise_at(vars(population_total),              # Specify column
                   list(population_region = sum))
   
    #income group table
    df_IC<-df_country%>%
      group_by(income_group) %>%     
      mutate_at(vars(`population_total`),~replace_na(.,0))%>%# Specify group indicator
      summarise_at(vars(population_total),              # Specify column
                   list(population_region = sum))%>%
      dplyr::rename(`state_region`=`income_group`)
    
    #vax regional
    df_vaxr<-df%>%group_by(state_region, date)%>%
      summarise_at(vars(value),              # Specify column
                   list(value= sum))
   
     #income group vaccine data
    df_vaxrIC<-df%>%group_by(income_group, date)%>%
      summarise_at(vars(value),              # Specify column
                   list(value= sum))%>%
      dplyr::rename(`state_region`=`income_group`)
  
 
    
    #join region and ic into 2 dfs
    df_icv<-left_join(df_IC, df_vaxrIC,by="state_region")
    df_srv<-left_join(df_region, df_vaxr,by="state_region")
    
    #bind region and IC together
    df_all<-bind_rows(df_srv,df_icv)%>%
      dplyr::mutate("Vaccine Percentage"=value /population_region)%>%
      dplyr::mutate(`Benchmark` = `Vaccine Percentage`)%>%
      mutate(`Benchmark` = ifelse(`Vaccine Percentage` >=.7, "Above 70%",
                             ifelse(`Vaccine Percentage`<.35, "Below 35%",
                                    "35-70%")))
    #regional counts
    df_all1<-df_all%>%
      count(state_region,date,`Goal`)
    
    df_all2<-bind_rows(df_all1,df_all)
      
    
    write.csv(df_all, "income_regional_vaccine_total.csv")
    write.csv(df_all2, "income_regional_vaccine_total_counts.csv")
    
    
    ## dataset for all
    
    
    df_data<-read.csv("Data/Potential COVID Mock up.xlsx - dataset.csv")
    #country
    df_country<-read.csv("Data/Potential COVID Mock up.xlsx - country_metadata.csv")
    
    # MUNGE for smoothing ============================================================================
    #full dataset
    df<-left_join(df_country,df_data, by="country_id")
    
   
    
    
    #regional table
    df_region_all<-df_country%>%
      mutate_at(vars(`population_total`),~replace_na(.,0))%>%
      group_by(state_region) %>%                         # Specify group indicator
      summarise_at(vars(population_total),              # Specify column
                   list(population_region = sum))
    
    #income group table
    df_IC_all<-df_country%>%
      group_by(income_group) %>%     
      mutate_at(vars(`population_total`),~replace_na(.,0))%>%# Specify group indicator
      summarise_at(vars(population_total),              # Specify column
                   list(population_region = sum))%>%
      dplyr::rename(`state_region`=`income_group`)
    
    #regional
    df_r_data<-df%>%group_by(state_region, date,series_id)%>%
      summarise_at(vars(value),              # Specify column
                   list(value= sum))
    
    #income group  data
    df_IC_data<-df%>%group_by(income_group, date,series_id)%>%
      summarise_at(vars(value),              # Specify column
                   list(value= sum))%>%
      dplyr::rename(`state_region`=`income_group`)
    
    
    
    #join region and ic into 2 dfs
    df_icv<-left_join(df_IC_all, df_IC_data,by="state_region")
    df_srv<-left_join(df_region_all, df_r_data,by="state_region")
    
    #bind region and IC together
    df_all<-bind_rows(df_srv,df_icv)
      #dplyr::mutate("Vaccine Percentage"=value /population_region)
    df_all<-df_all%>%
      pivot_wider(names_from = "series_id", 
                         values_from="value")
    
  df_all<-df_all%>%
    dplyr::arrange(desc(state_region)) %>% 
      dplyr::group_by(state_region) %>% 
      dplyr::mutate(cases_07da = zoo::rollmean(`79711` , k = 7, fill = NA),
                    cases_21da = zoo::rollmean(`79711` , k = 21, fill = NA)) %>% 
    dplyr::mutate(deaths_07da = zoo::rollmean(`79712` , k = 7, fill = NA),
                  deaths_21da = zoo::rollmean(`79712` , k = 21, fill = NA))%>%
      dplyr::ungroup()
  
  df_all<-df_all%>%
    dplyr::mutate("Vaccine Percentage"=`79713` /population_region)
  
  df<- df%>%
    dplyr::mutate(`Goal` = `Vaccine Percentage`)%>%
    mutate(`Goal` = ifelse(`Vaccine Percentage` >=.7, "Above 70%",
                           ifelse(`Vaccine Percentage`<.35, "Below 35%",
                                  "35-70%")))
 
                  
  df_all<-df_all%>%
    pivot_longer(`79711`:deaths_21da,names_to = "series")
  
  write.csv(df_all, "income_regional_vaccine_total_smoothed.csv")
  
  
  ## random notes on benchmark counts:
  #filter based on income group
  df_UMIC<-df%>%dplyr::filter(income_group=="Upper Middle Income Country (World Bank Classification)")
  df1<-df_UMIC%>%
    dplyr::filter(date =="2021-07-01")%>%
    count(state_region,`Goal`)
  
  df_vaxb<-df_all%>%
    dplyr::select(state_region,date,Goal)
  write.csv(df_vaxb,"Benchmark dummy.csv")
  
