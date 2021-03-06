library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
library(tidytext)
library(gt)
library(glue)
library(webshot)


df_fsd<-si_path()%>%
  return_latest("Fin")%>%
  gophr::read_msd()

  source<-source_info(si_path(),"Fin")

  #use this function to print out budget execution by agency at global level
  
get_global_agency_be<-function(df){
  df<-df_fsd%>%
  remove_mo()%>%
  dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
dplyr::select (c(fundingagency,fiscal_year,cop_budget_total,expenditure_amt))%>%
  mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
  dplyr::mutate(agency_category=fundingagency)%>%
  mutate(agency_category  = ifelse(agency_category == "USAID", "USAID",
                                   ifelse(agency_category  == "HHS/CDC", "CDC",
                                          ifelse(agency_category  =="Dedup", "Dedup","Other"))))%>%
  mutate( agency_category = fct_relevel(agency_category, "USAID","CDC","Other"))%>%
  group_by(agency_category,fiscal_year)%>%
  summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
  dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
  ungroup()%>%
  pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
  dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
  dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
   dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
gt()%>%
  fmt_percent(
    columns = c(`budget_execution_2020`, `budget_execution_2021`),
    decimals = 0)%>%
  fmt_currency( # add dolar signs
    columns = c(`cop_budget_total_2020`,`expenditure_amt_2020`,`cop_budget_total_2021`,`expenditure_amt_2021` ),
    decimals = 0,
    currency = "USD")%>%
  tab_options(
    table.font.names = "Source Sans Pro"
  ) %>% 
  cols_width(
    everything() ~ px(90))%>%
  cols_label( #update GT to check on tidy select), also look at clean_names, also potentially case_when
    agency_category = "Agency",
    expenditure_amt_2020 = "Expenditure",
    cop_budget_total_2020 = "Budget",
    budget_execution_2020="Budget Execution",
    agency_category = "Agency",
    expenditure_amt_2021 = "Expenditure",
    cop_budget_total_2021 = "Budget",
    budget_execution_2021="Budget Execution"
  )%>%

    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1.5),
      ),
      locations = cells_body(
        columns = everything(),
        rows = everything()
      ))%>%
    tab_style(
      style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution_")
      )
    )%>%
    tab_spanner(
      label = "COP20 Performance",
      columns = c(
        expenditure_amt_2021,cop_budget_total_2021, budget_execution_2021,))%>%
    tab_spanner(
      label = "COP19 Performance",
      columns = c(
        expenditure_amt_2020,cop_budget_total_2020,budget_execution_2020))%>%
    gt::tab_style(
      style = list(
        gt::cell_text(weight = "bold")), 
      locations = gt::cells_column_spanners(spanners = tidyselect::everything())
    )%>%
    cols_align(
      align = "center",
      columns = everything()
    )%>%
    cols_align(
      align = "left",
      columns = tidyselect::contains("agency")
    )%>%
    tab_style(style = cell_fill(color = "#5bb5d5",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020) >= 0.9 & (budget_execution_2020) < 1.1)) %>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows =(budget_execution_2020) < 0.9 ))%>%
    tab_style(style = cell_fill(color = "#ffcaa2",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020)>= 1.1 & (budget_execution_2020) < 1.2))%>%
    tab_style(style = cell_fill(color = "#ff989f",alpha = .75),      
              locations = cells_body(               
                columns = (budget_execution_2020),
                rows = (budget_execution_2020) >= 1.2 ))%>%
    
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
  
   tab_footnote(
     footnote = "Excluding M&O",
     locations = cells_column_labels(
       columns =c(expenditure_amt_2020, expenditure_amt_2021)))%>%
  tab_header(
    title = (" COP 2020 & 2021 Global Financial Performance Summary"))%>% 
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to oha.ea@usaid.gov for questions"))
    ) %>%
  tab_source_note(
    source_note = md("*Other* based on aggregated funding agencies"))
  

return(df)
}

#testing
table_out<-"GitHub/stacks-of-hondos/Images"
get_global_agency_be(df_fsd)%>%
  gtsave(., path=table_out, filename="global performance_all_agencies.png")
