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
read_msd()

source<-source_info(si_path(),"Fin")
  
ou_list<-si_path()%>%
  return_latest("COP17")%>%
  gophr::read_msd()%>%
  distinct(operatingunit)%>%
  pull()

country_list<-si_path()%>%
  return_latest("COP17")%>%
  gophr::read_msd()%>%
  distinct(countryname)%>%
  pull()

#using source info for getting other data
#potential for sep functions for table, gt, munging, etc.

#use this function to print out budget execution by agency at different OUs

get_ou_agency_be<-function(df, ou="operatingunit"){
  df<-df%>%
    remove_mo()%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    #dplyr::filter(operatingunit== "Mozambique")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    dplyr::select (c(fundingagency,fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    dplyr::mutate(agency_category=fundingagency)%>%
    mutate(agency_category  = ifelse(agency_category == "USAID", "USAID",
                                     ifelse(agency_category  == "HHS/CDC", "CDC",
                                            ifelse(agency_category  =="Dedup", "Dedup","Other"))))%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","HHS/CDC","Other"))%>%
    group_by(fundingagency,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=expenditure_amt/cop_budget_total)%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
    #break into separate functions
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
    cols_label(
      fundingagency = "Agency",
      expenditure_amt_2020 = "Expenditure",
      cop_budget_total_2020 = "Budget",
      budget_execution_2020="Budget Execution",
      #agency_category = "Agency",
      expenditure_amt_2021 = "Expenditure",
      cop_budget_total_2021 = "Budget",
      budget_execution_2021="Budget Execution"
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
    tab_style(
      style = cell_text(weight = 700),
      locations = cells_body(
        columns = tidyselect::contains("_execution_")
      ))%>%
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
    
    
    tab_footnote(
      footnote = "Excluding M&O",
      locations = cells_column_labels(
        columns =c(expenditure_amt_2020, expenditure_amt_2021)))%>%
    tab_header(
      title = glue::glue(" COP2019 & COP2020 {ou} Financial Performance Summary"),
      subtitle = legend_chunk)%>%
    gt::tab_source_note(
      source_note = gt::md(glue::glue("**Source**: {source} | Please reach out to gh.oha.ea@usaid.gov for questions"))
    ) 
  return(df)
}

table_out<-"GitHub/stacks-of-hondos/Images"
#to run for one OU testing below
get_ou_agency_be(df_fsd, "South Africa")%>%
gtsave("test.png")
#to run for all (giving me)
purrr::map(ou_list, ~get_ou_agency_be(df_fsd, ou = .x)%>%
gtsave(.,path=table_out,filename = glue::glue("{.x}_ou_budget_execution.png")))

glamr::export_drivefile()

### New version
get_ou_agency_be_n<-function(df, ou="operatingunit"){
  df<-df%>%
    prep_fsd()%>%
    dplyr::filter(fiscal_year=="2020" | fiscal_year=="2021")%>%
    #dplyr::filter(operatingunit== "Mozambique")%>%
    dplyr::filter(operatingunit %in% ou)%>%
    dplyr::select (c(fundingagency,fiscal_year,cop_budget_total,expenditure_amt))%>%
    mutate_at(vars(cop_budget_total,expenditure_amt),~replace_na(.,0))%>%
    mutate( fundingagency = fct_relevel(fundingagency, "USAID","CDC"))%>%
    group_by(fundingagency,fiscal_year)%>%
    summarise_at(vars(cop_budget_total,expenditure_amt), sum, na.rm = TRUE)%>%
    dplyr::mutate(budget_execution=percent_clean(expenditure_amt,cop_budget_total))%>%
    ungroup()%>%
    pivot_wider(names_from = fiscal_year,values_from = cop_budget_total:budget_execution, values_fill = 0)%>%
    dplyr::relocate(expenditure_amt_2020, .before = cop_budget_total_2020) %>%
    dplyr::relocate(expenditure_amt_2021, .before = cop_budget_total_2021) %>%
    dplyr::relocate(budget_execution_2021, .after = cop_budget_total_2021)%>%
    dplyr::relocate(budget_execution_2020, .after = cop_budget_total_2020) %>%
    
    #break into separate functions
   
    ea_style()%>%
    #gt()%>%
    cols_label(
     fundingagency = "Funding Agency")%>%
    tab_header(
      title = glue::glue(" COP2019 & COP2020 {ou} Financial Performance Summary"),
      subtitle = legend_chunk)
      
     
    
  return(df)
}

get_ou_agency_be_n(df_fsd,"South Africa")