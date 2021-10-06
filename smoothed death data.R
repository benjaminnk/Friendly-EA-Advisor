#get 7 day death average
library(tidyverse)

df_data<-read.csv("Data/obj_1_2_dataset (2).csv")
#country
df_country<-read.csv("Data/final_country_list (1).csv")

df<-left_join(df_country,df_data, by="country_id")

df<-df%>%
  dplyr::filter(series_id=="79711")

df_dr<-df%>%group_by(state_region, date)%>%
  summarise_at(vars(value),              # Specify column
               list(value= sum))

df_dIC<-df%>%group_by(income_group, date)%>%
  summarise_at(vars(value),              # Specify column
               list(value= sum))%>%
  dplyr::rename(`state_region`=`income_group`)

df_all<-bind_rows(df_dIC,df_dr)

df_all_july<-df_all%>%
  filter(date >= as.Date("2021-07-19") & date <= as.Date("2021-07-26"))

df_all_july2<-df_all_july%>%
  group_by(state_region)%>%
  summarise_at(vars(value),              # Specify column
               list(`7_day_total`= sum))

write.csv(df_all_july2,"7 day total.csv")

df_all2<-df_all%>%
  dplyr::arrange(desc(state_region)) %>% 
  dplyr::group_by(state_region) %>% 
  dplyr::mutate(deaths_07da = zoo::rollapply(`value`,width=3 , k = 7, fill =`value`))

  
  df_all<-df_all1%>%
  dplyr::mutate(`Goal` = `deaths_07da`)%>%
  mutate(`Goal` = ifelse(`deaths_07da` >=8000, "Bad",
                         ifelse(`deaths_07da`<7500, "Good",
                                "Okay")))

df_all1<-df_all1%>%
  group_by(state_region) %>% 
  #arrange(date) %>%
  mutate(pct_change = (`deaths_07da`/lag(`deaths_07da`) - 1) * 100)

write.csv(df_all1, "income_regional_vaccine_total_smoothed.csv")