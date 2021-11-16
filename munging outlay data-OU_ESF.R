# PURPOSE: Munge and Analysis of Budget team data for EOFY Review
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



percent_clean <- function(x, y) {
  ifelse(y > 0.000, (x / y), NA_real_)
}


# LOAD DATA ============================================================================  


budget<-list.files("~/Data/Budget Files/OU_ESF",full.names = TRUE) #can try and automate via google folder
glimpse(budget)



  

# MUNGE ============================================================================
get_be_esf<-function(df){
  df<-read_xlsx(df, 
                sheet = "EOFY ESF", skip = 2)%>%
    clean_names()%>%
    select(c(operating_unit, last_active_cop,esf,total_outlays_during_fy_21))
  df$operating_unit <- gsub("Democaratic Republic of the Congo", "Democratic Republic of the Congo",df$operating_unit)
  
    df<-df%>%
      mutate(outlay_check=esf-total_outlays_during_fy_21)%>%
      mutate(mechs_overspend = outlay_check)%>%
      mutate(mechs_overspend = ifelse(outlay_check >=0, "0",
                                      ifelse(outlay_check<0, "1",NA)))%>%
      mutate(mechs_overspend=as.numeric(mechs_overspend))%>%
      group_by(operating_unit,last_active_cop)%>%
      summarise_at(vars(esf,total_outlays_during_fy_21, mechs_overspend,), sum, na.rm = TRUE)
 df<-df%>%
   mutate("unoutlayed"=esf-total_outlays_during_fy_21)%>%
 ungroup()
    
     return(df)
}

# VIZ ============================================================================
#OU ESF
df1 <- purrr::map_dfr(.x = budget,
                      .f = ~ get_be_esf(.x))

df3<-df1%>%
  filter(!last_active_cop=="2021 COP")%>%
  arrange(desc(unoutlayed))%>%
  slice_max(unoutlayed, n = 10)
  
  
#WCF ESF
data <- "~/Data/Budget Files/USAID_WCF-ESF.xlsx" #rename tab EOFY ESF
df2<-get_be_esf(data)%>%
  mutate(funding_agency="USAID-WCF")

df_top_ten<-df3%>%
  slice_max(unoutlayed,n=10)


df_bottom_20<-df1%>%
  slice_min(mechs_overspend, n = 20)
# charting ============================================================================

df3<-df3%>%
  gt()%>%
  cols_hide(
    columns = c(
      last_active_cop,mechs_overspend
    )
  )%>%
  fmt_currency( # add dolar signs
    columns = c(total_outlays_during_fy_21, esf,unoutlayed ),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(120))%>%
  cols_label(
    operating_unit = "Operating Unit",
    total_outlays_during_fy_21 = "Outlays",
    esf = " ESF Funds",
    unoutlayed="Remaining Funds",
    mechs_overspend="Mechanisms Overspending (#)"
  )%>%
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
  tab_header(
    title = glue::glue(" COP20 USAID End of Fiscal Year Financial Performance Summary: ESF Funds"),
    subtitle = "OUs with highest remaining COP20 ESF Funds")%>%
  #gt::tab_source_note(
  # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | Please reach out to your budget backstop for questions"))
  )  %>%
  gtsave(.,"FY20_ESF.png") 


df3<-df_bottom_20%>%
  gt()%>%
  fmt_percent(
    columns = c(budget_execution,),
    decimals = 0)%>%
  fmt_currency( # add dolar signs
    columns = c(total_outlays_during_previous_fy, previous_cop_total_planned_funding ),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(120))%>%
  cols_label(
    operating_unit = "Operating Unit",
    total_outlays_during_previous_fy = "Outlays",
    previous_cop_total_planned_funding = " COP20 Budget",
    budget_execution="Total Execution",
    mechs_overspend="Mechanisms Overspending (#)"
  )%>%
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
  tab_header(
    title = glue::glue(" COP20 USAID End of Fiscal Year Financial Performance Summary"),
    subtitle = "OUs with fewest mechanisms that have over-outlayed their budget")%>%
  #gt::tab_source_note(
  # source_note = ("USAID mechanisms only. Partner designations provided by the OHA Local Partners Team. Visual excludes TBDs"))%>%
  gt::tab_source_note(
    source_note = gt::md(glue::glue("**Source**: EOFY Workbooks | Please reach out to your budget backstop for questions"))
  ) 

###Google sheet output====
target_location <- gs4_get(" https://docs.google.com/spreadsheets/d/1FSnWTGbxw9Xu2M9vBd8JAiK5RcpFJ5eHjDCinhhNnH4/edit#gid=0")
sheet_write(df1, target_location, sheet = "ESF OU outlays")
sheet_write(df2, target_location, sheet = "ESF WCF OU outlays")
