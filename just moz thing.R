library(glamr)
library(tidyverse)
library(gophr)
library(gt)
library(glue)
library(webshot)
library(dplyr)
library(devtools)
library(tidyr)
library(gisr)
library(scales)
library(sf)
library(glitr)
library(readxl)






#select which data source to use
df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()




#This function can be used to print out budget execution by partner type (local, international)for USAID at a global level. 
#You will need to ensure that you have load_secrets from the glamr package set up beforehand
#Be sure to load the following source files below before running
source("~/GitHub/stacks-of-hondos/ea_style.R")
source("~/GitHub/stacks-of-hondos/prep_fsd.R")
source("~/GitHub/stacks-of-hondos/utilities.R")



  df<-df_fsd%>%
    prep_fsd()%>%
    
    #filter for fiscal year
    dplyr::filter( fiscal_year=="2021" | fiscal_year=="2022")%>%
    #dplyr::filter(fundingagency == "USAID") %>% 
    #filter for OU
    dplyr::filter(operatingunit== "Vietnam")%>%
    dplyr::filter(fundingagency=="USAID")%>%
    #dplyr::filter(operatingunit %in% ou)%>%
    #dplyr::filter(fundingagency %in% funding_agency) %>% 
    #dplyr::filter(mech_id_mech_name %in% id) %>% 
    #dplyr::filter(program %in% prog) %>% 
    #select specific variables
    dplyr::select (c(fundingagency, mech_id_mech_name,fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))
    #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    
    
   df<-df%>% 
    group_by(mech_id_mech_name,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup(mech_id_mech_name,fiscal_year)%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)
   # dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    
    df<-df%>%
      dplyr::relocate(expenditure_amt_2021 , .before = cop_budget_total_2021 )%>%
      filter(cop_budget_total_2022>0)
    df<-df%>%
      dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021) %>%
      select(-c(expenditure_amt_2022,budget_execution_2022))
    df<-df%>%
    dplyr::relocate(cop_budget_total_2022,.after = budget_execution_2021)%>%
      arrange(desc(cop_budget_total_2022))
    
    #break into separate functions
   df1<-df%>%
     arrange(desc(cop_budget_total_2022))%>%
     slice_max(cop_budget_total_2022,n=15)
   df2<-df%>%
     arrange(desc(cop_budget_total_2022))%>%
     slice_min(cop_budget_total_2022,n=15)
   
     
     moz_style<-function(df){
       df%>%
         gt()%>%
     fmt_percent(
       columns = tidyselect::contains("_execution"),
       decimals = 0)%>%
     fmt_currency( # add dolar signs
       columns = tidyselect::contains("_budget_total"),
       decimals = 0,
       currency = "USD")%>%
     fmt_currency( # add dolar signs
       columns = tidyselect::contains("expenditure_amt_"),
       decimals = 0,
       currency = "USD")%>%
     tab_options(
       table.font.names = "Source Sans Pro"
     ) %>% 
     fmt_missing(columns = everything(),
                 missing_text = "-") %>%
     cols_width(
       everything() ~ px(120))%>%
     
     tab_style(
       style = cell_borders(
         sides = "all",
         weight = px(1),
       ),
       locations = cells_body(
         columns = everything(),
         rows = everything()
       ))%>%
     tab_style(
       style = cell_text(weight = 700),
       locations = cells_body(
         columns = tidyselect::contains("_execution")
       )
     )%>%
     cols_align(
       align = "center",
       columns = everything()
     )%>%
     cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      # expenditure_amt_2020 = "Expenditure",
       cop_budget_total_2022 = "Budget",
       #budget_execution_2020="Budget Execution",
       expenditure_amt_2021 = "Expenditure",
       cop_budget_total_2021 = "Budget",
       budget_execution_2021="Budget Execution",
       mech_id_mech_name="Mechanism"
       
     )%>%
         tab_spanner(
           label = "COP20 Performance",
           columns = tidyselect::contains("2021"))%>%
         tab_spanner(
           label = "COP21 Budget",
           columns = tidyselect::contains("2022"))%>%
         gt::tab_style(
           style = list(
             gt::cell_text(weight = "bold")), 
           locations = gt::cells_column_spanners(spanners = tidyselect::everything()))%>%
     cols_align(
       align = "left",
       columns = 1
     )%>%
     
         tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
                   locations = cells_body(               
                     columns = (budget_execution_2021),
                     rows = (budget_execution_2021) >= 0.9 & (budget_execution_2021) < 1.1)) %>%
         tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
                   locations = cells_body(               
                     columns = (budget_execution_2021),
                     rows =(budget_execution_2021) < 0.9 ))%>%
         tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
                   locations = cells_body(               
                     columns = (budget_execution_2021),
                     rows = (budget_execution_2021)>= 1.1 & (budget_execution_2021) < 1.2))%>%
         tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
                   locations = cells_body(               
                     columns = (budget_execution_2021),
                     rows = (budget_execution_2021) >= 1.2 ))%>%
         
     gt::tab_options(
       source_notes.font.size = 8,
       table.font.size = 13, 
       data_row.padding = gt::px(5),
       source_notes.padding = gt::px(1),) %>%
       tab_header(
         title = glue::glue("COP20 USAID Program Financial Summary: Vietnam"),
         subtitle = legend_chunk)%>%
     
     tab_footnote(
       footnote = "Excluding M&O",
       locations = cells_column_labels(
         columns =c(expenditure_amt_2021)))%>%
     gt::tab_source_note(
       source_note = gt::md(glue::glue("**Source**: {source} | Includes only mechanisms that submitted ER template during the clean period")))
  
}

table_out<-"GitHub/stacks-of-hondos/Images/moz"

df1%>%
  moz_style() %>%
  gtsave(.,path=table_out,filename = glue::glue("moz partner performance_top10.png"))
df2%>%
  moz_style() %>%
  gtsave(.,path=table_out,filename = glue::glue("moz partner performance_bottom10.png"))

df%>% moz_style() %>%
  gtsave(.,path=table_out,filename = glue::glue("vietnam partner performance .png"))

get_ou_mech_pa_only<-function(df,id="mech_id_mech_name"){
  df<-df_fsd%>%
    prep_fsd()%>%
    
    #filter for fiscal year
    dplyr::filter(fiscal_year=="2021" | fiscal_year=="2022")%>%
    dplyr::filter(fundingagency == "USAID") %>% 
    #filter for OU
    dplyr::filter(operatingunit== "Vietnam")%>%
   
    #dplyr::filter(fundingagency %in% funding_agency) %>% 
    dplyr::filter(mech_id_mech_name %in% id) %>% 
    #dplyr::filter(program %in% prog) %>% 
    #select specific variables
    dplyr::select (c(fundingagency, mech_id_mech_name,program, fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    #mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    
    
    
    group_by(mech_id_mech_name,program, fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup(program, fiscal_year)%>%
    
    
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
   
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    select(-c(budget_execution_2022,expenditure_amt_2022))%>%
    gt()%>%
    fmt_percent(
      columns = tidyselect::contains("_execution"),
      decimals = 0)%>%
    fmt_currency( # add dolar signs
      columns = tidyselect::contains("_budget_total"),
      decimals = 0,
      currency = "USD")%>%
    fmt_currency( # add dolar signs
      columns = tidyselect::contains("expenditure_amt_"),
      decimals = 0,
      currency = "USD")%>%
    tab_options(
      table.font.names = "Source Sans Pro"
    ) %>% 
    fmt_missing(columns = everything(),
                missing_text = "-") %>%
    cols_width(
      everything() ~ px(120))%>%
    
    tab_style(
      style = cell_borders(
        sides = "all",
        weight = px(1),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))%>%
    tab_style(
      style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution")
      )
    )%>%
    cols_align(
      align = "center",
      columns = everything()
    )%>%
    cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
      # expenditure_amt_2020 = "Expenditure",
      cop_budget_total_2022 = "Budget",
      #budget_execution_2020="Budget Execution",
      expenditure_amt_2021 = "Expenditure",
      cop_budget_total_2021 = "Budget",
      budget_execution_2021="Budget Execution",
      mech_id_mech_name="Mechanism"
      
    )%>%
    tab_spanner(
      label = "COP20 Performance",
      columns = tidyselect::contains("2021"))%>%
    tab_spanner(
      label = "COP21 Budget",
      columns = tidyselect::contains("2022"))%>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything()))%>%
    cols_align(
      align = "left",
      columns = 1
    )%>%
    
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 0.9 & (budget_execution_2021) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows =(budget_execution_2021) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021)>= 1.1 & (budget_execution_2021) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2021),
                rows = (budget_execution_2021) >= 1.2 ))%>%
    
    #break into separate functions
    
    cols_label(
      mech_id_mech_name = "Mechanism",
      program= "Program Area")%>%
    tab_header(
      title = glue::glue("COP20 and Looking Forward to COP21: {id}"),
      subtitle = legend_chunk) %>%
    
    tab_options(footnotes.font.size = "small")
return(df)
}
