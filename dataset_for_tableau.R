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
     df_region<-df_region%>%
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
      dplyr::mutate("Vaccine Percentage"=value /population_region)
    
    write.csv(df_all, "income_regional_vaccine_total.csv")
  
