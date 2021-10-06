# PURPOSE: Munge and Analysis of tables for C19 DB
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-07-28
# NOTES: 

# LOCALS & SETUP ============================================================================

# Libraries

library(tidyverse)




# Set paths  
proj_paths

si_paths 

# Functions  


# LOAD DATA ============================================================================  

df_data<-read.csv("Data/obj_1_2_dataset (2).csv")
#country
df_country<-read.csv("Data/Interagency Dashboard Data Guide - crosswalk_proposed_list.csv")

# MUNGE ============================================================================
#full dataset
df<-left_join(df_country,df_data, by="country_id")


#LMICS list (not complete)
LMICS<-c("Afghanistan","Benin","Burkina Faso","Burundi","Central African Republic","Chad","	
Congo (Kinshasa)",
         "Eritrea","Ethiopia","Gambia","The Guinea","Guinea-Bissau","Haiti",
         "Liberia","Madagascar","Malawi","Mali","Mozambique","Nepal","Niger","Rwanda",
         "Sierra Leone","Somalia","South Sudan","Syrian Arab Republic",
         "Tajikistan","Tanzania","Togo","Uganda","Yemen,Rep.","Angola", "Algeria", "Bangladesh"," Bhutan",
         "Bolivia", "Cabo Verde", "Cambodia", "Cameroon", "Comoros", "Cote d'Ivoire"," Djibouti", 
         "Egypt", El Salvador, Eswatini, Ghana, Honduras, India, Indonesia, Kenya, Kiribati,
 #        Kyrgyz Republic Lao PDR, Lesotho, Mauritania, Micronesia, Fed. Sts., Moldova, Mongolia, Morocco,
  #       Myanmar, Nicaragua, Nigeria, Pakistan, Papua New Guinea, Philippines, São Tomé and Principe,
   #      Senegal, Solomon Islands, Sri Lanka, Sudan, Timor-Leste,"Burma (Myanmar)" 
    #     Tunisia, Ukraine, Uzbekistan, Vanuatu, Vietnam, West Bank and Gaza, Zambia, Zimbabwe)
)
df_LMICS<-df%>%
  dplyr::filter(data_services_country %in% LMICS)
  
  
  #get just vaccine data 
  df<-df%>%
  dplyr::filter(series_id=="79713")


df<-df%>%
  dplyr::mutate("Vaccine Percentage"=value /population_total)

df<-df%>%
  dplyr::mutate(`Benchmark` = `Vaccine Percentage`)%>%
  mutate(`Benchmark` = ifelse(`Vaccine Percentage` >=.2, "Above 20%",
                              ifelse(`Vaccine Percentage`<=.05, "Below 5%",
                                     "5-15%")))

#create benchmark tables
df1<-df%>%
  dplyr::filter(date =="2021-07-26")%>%
  count(state_region,`Benchmark`)%>%
  group_by(state_region) %>% 
  mutate(total_n = sum(n)) %>%
  dplyr::mutate("Percentage"=n /total_n)

df2<-df%>%
  dplyr::filter(date =="2021-07-26")%>%
  count(income_group,`Benchmark`)%>%
  group_by(income_group) %>% 
  mutate(total_n = sum(n)) %>%
  dplyr::mutate("Percentage"=n /total_n)%>%
  dplyr::rename(`state_region`=`income_group`)
  
#test state and income group counts
df_region_state<-df%>%
  dplyr::filter(date =="2021-07-26")%>%
  count(state_region,income_group,`Benchmark`)%>%
  group_by(state_region,income_group) %>% 
  mutate(total_n = sum(n)) %>%
  dplyr::mutate("Percentage"=n /total_n)

#%>%
 # dplyr::rename(`state_region`=`income_group`)

df_Rs<-left_join(df_region_state,df1, by="state_region")
df_benchmark_counts<-bind_rows(df2, df1)
write.csv(df_benchmark_counts,"Vaccine chart_benchmarks_by_region_income_7.24.2021.csv")

#write new table df2
write.csv(df_region_state,"Vaccine chart_benchmarks_by_region_income_7.24.2021.csv")



#regional table pop
df_region<-df_country%>%
  
  mutate_at(vars(`population_total`),~replace_na(.,0))%>%
  group_by(state_region) %>%                         # Specify group indicator
  summarise_at(vars(population_total),              # Specify column
               list(population_region = sum))

#income group table pop
df_IC<-df_country%>%
  group_by(income_group) %>%     
  mutate_at(vars(`population_total`),~replace_na(.,0))%>%# Specify group indicator
  summarise_at(vars(population_total),              # Specify column
               list(population_region = sum))%>%
  dplyr::rename(`state_region`=`income_group`)

#vax regional vax total
df_vaxr<-df%>%group_by(state_region, date)%>%
  summarise_at(vars(value),              # Specify column
               list(value= sum))

df_test<-df%>%
  group_by(state_region,income_group, date) %>% summarize(total_value = sum(value))

#income group vaccine data
df_vaxrIC<-df%>%group_by(income_group, date)%>%
  summarise_at(vars(value),              # Specify column
               list(value= sum))%>%
  dplyr::rename(`state_region`=`income_group`)



#join region and ic into 2 dfs
df_icv<-left_join(df_IC, df_vaxrIC,by="state_region")
df_srv<-left_join(df_region, df_vaxr,by="state_region")

#bind region and IC together
df_region_ic_vaxperct<-bind_rows(df_srv,df_icv)%>%
  dplyr::mutate("Vaccine Percentage"=value /population_region)

df_region_ic_vaxperct<-df_region_ic_vaxperct%>% 
  dplyr::filter(date =="2021-07-26" |date =="2021-07-19" )

write.csv(df_region_ic_vaxperct,"Vaccine chart_by_region_income_7.24.2021.csv")
 

