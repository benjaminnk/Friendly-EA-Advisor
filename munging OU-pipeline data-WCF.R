# PURPOSE: Munge and Analysis of Budget team data for EOFY Review-WCF
# AUTHOR: Ben Kasdan | SIEI
# LICENSE: MIT
# DATE: 2021-11-08
# NOTES: 

# LOCALS & SETUP ============================================================================

# Libraries
library(glitr)
library(glamr)
library(tidyverse)
library(gophr)
library(scales)
library(extrafont)
library(tidytext)
library(here)
library(gt)
library(googledrive)
library(googlesheets4)
library(janitor)
library(readxl)
library(data.table)

filters<-c("Operating Unit",
           "FY 2004-2021 Q4 Total Pipeline (to include Obligated but not yet Outlaid and Unobligated but intended for Award)",
           "Unliquidated Obligations (ULO) / Obligations Pending Outlay (OPO): Expired Award on Non-Expired Fund Accounts (Untouchable Pipeline)",
           "ULO / OPO: Expired Awards on Expired Fund Accounts as of the Last calendar day of Previous FY (Untouchable Pipeline)",
           "Funds on CODB obligations that have not been fully outlaid and can't be used to support Current COP approved activities (Untouchable Pipeline)",
           "Other ULO / OPO",
           "Total Untouchable Pipeline",
           "Projected Remaining Pipeline at Current FY Close","Months of Allowable Pipeline Needed",
           "Total Allowable Buffer Pipeline","New Adjusted Pipeline",
           "Adjusted Excess Pipeline",
           "Notes",
           "Further work in OU intended?")


percent_clean <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}


# LOAD DATA ============================================================================  


budget<-list.files("~/Data/Budget Files/WCF",full.names = TRUE) #can try and automate via google folder
glimpse(budget)
data <- "C:/Users/Bkasdan/Documents/Data/Budget Files/WCF/Angola USAID_WCF EOFY (Bilateral).xlsx"

get_ou_pipeline<-function(df){
  df<-read_xlsx(df, sheet = "OU")
  df<-df%>%
    dplyr::filter(Attribute %in% filters)%>%
    rename_at(vars(contains('Data Entry')), ~ 'data_entry')%>%
    select(c("Attribute","data_entry"))%>%
    transpose()
  colnames(df) <- filters
  df<-df%>%
    clean_names()%>%
    filter(operating_unit!="Operating Unit")
  df$operating_unit <- gsub("Democaratic Republic of the Congo", "Democratic Republic of the Congo",df$operating_unit)
  df<-df%>%
    rename("ulos_on_expired_awards"=unliquidated_obligations_ulo_obligations_pending_outlay_opo_expired_award_on_non_expired_fund_accounts_untouchable_pipeline)%>%
    rename("pipeline_available_at_fy22_end"=projected_remaining_pipeline_at_current_fy_close)%>%
    rename("cop22_applied_pipeline"=adjusted_excess_pipeline)%>%
    mutate(ulos_on_expired_awards=as.numeric(ulos_on_expired_awards))%>%
    mutate(ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline=as.numeric(ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline))%>%
    mutate("pipeline_in_expired_awards"=ulos_on_expired_awards+ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline )%>%
    select(-c(ulo_opo_expired_awards_on_expired_fund_accounts_as_of_the_last_calendar_day_of_previous_fy_untouchable_pipeline))%>%
    rename("codb_untouchable_pipeline"=funds_on_codb_obligations_that_have_not_been_fully_outlaid_and_can_t_be_used_to_support_current_cop_approved_activities_untouchable_pipeline)%>%
    dplyr::relocate(pipeline_in_expired_awards, .before = notes)%>%
    mutate_at(c(2:12), as.numeric)%>%
    rename("fy22_startup_pipeline"=fy_2004_2021_q4_total_pipeline_to_include_obligated_but_not_yet_outlaid_and_unobligated_but_intended_for_award)
  return (df)
}


# MUNGE ============================================================================


# VIZ ============================================================================

df_wcf<- purrr::map_dfr(.x = budget,
                      .f = ~ get_ou_pipeline(.x))%>%
  mutate(funding_agency="USAID-WCF")

#get just wcf applied pipeline
df_wcf1<-df_wcf%>%
  select(operating_unit,cop22_applied_pipeline)%>%
  rename(cop22_applied_pipeline_wcf=cop22_applied_pipeline)
df11<-df1%>%
  select(operating_unit,cop22_applied_pipeline)

df_all<-left_join(df11, df_wcf1, by="operating_unit")%>%
  arrange(desc(cop22_applied_pipeline))



#df_all1<-df_all%>%
 # filter(cop22_applied_pipeline>0 & cop22_applied_pipeline_wcf>0)

df_top10<-df_all%>%
  arrange(desc(cop22_applied_pipeline))%>%
  slice_max(cop22_applied_pipeline,n=29)

df_bottom28<-df_all%>%
  arrange(desc(cop22_applied_pipeline))
df_bottom28<-df_bottom28%>%
  slice_min(cop22_applied_pipeline,n=29)%>%
  arrange(desc(cop22_applied_pipeline))

# charting ============================================================================
  esf<-function(df){
    df%>%
      gt()%>%

  fmt_currency( # add dolar signs
    columns = c(cop22_applied_pipeline_wcf,cop22_applied_pipeline ),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(120))%>%
  cols_label(
    operating_unit = "Operating Unit",
    #total_outlays_during_previous_fy = "Outlays",
    #previous_cop_total_planned_funding = " COP20 Budget",
    #budget_execution="Total Execution",
    #mechs_overspend="Mechanisms Overspending (#)",
    cop22_applied_pipeline_wcf= "COP22 WCF Applied Pipeline",
    cop22_applied_pipeline="COP22 Applied Pipeline"
  )%>%

    fmt_missing(columns = everything(),
                missing_text = "-")%>%
  gt::tab_options(
    source_notes.font.size = 8,
    table.font.size = 13, 
    data_row.padding = gt::px(5),
    source_notes.padding = gt::px(1),) %>%
  
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ))%>%
  
  cols_align(
    align = "center",
    columns = everything()
  )%>%
  cols_align(
    align = "left",
    columns = tidyselect::contains("operating")
  )%>%
      tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
               locations = cells_body(               
                  columns = (cop22_applied_pipeline),
                 rows =(cop22_applied_pipeline) <= 0 ))%>%
      
  tab_header(
    title = glue::glue(" COP20 USAID End of Fiscal Year Financial Performance Summary"),
    )%>%
  #gt::tab_source_note(
  # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | Please reach out to your budget backstop for questions"))
  )
  }
   
###Google sheet output====
target_location <- gs4_get(" https://docs.google.com/spreadsheets/d/1FSnWTGbxw9Xu2M9vBd8JAiK5RcpFJ5eHjDCinhhNnH4/edit#gid=0")
sheet_write(df_wcf, target_location, sheet = "WCF OU pipline")

df_top10%>%
  esf() %>%
  gtsave(.,"COP22_all_pipeline_10.png") 

df_bottom28%>%
esf() %>%
  gtsave(.,"COP22_all_pipeline_48.png") 
