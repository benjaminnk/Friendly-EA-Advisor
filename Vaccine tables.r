# PURPOSE: Munge and Analysis of COVID Data
# AUTHOR: Ben Kasdan | SI
# LICENSE: MIT
# DATE: 2021-07-01
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    oha
  
  # Set paths  
    proj_paths
   
    si_paths 
    
  # Functions 
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
    
    up_arrow <- "<span style=\"color:green\">&#9650;</span>"
    down_arrow <- "<span style=\"color:red\">&#9660;</span>"
    
  

# LOAD DATA ============================================================================  

  #Data
    df_data<-read.csv("Data/Potential COVID Mock up.xlsx - dataset.csv")
  #country
    df_country<-read.csv("Data/Potential COVID Mock up.xlsx - country_metadata.csv")
    
  

# MUNGE ============================================================================
  
  #Bind the two  
    df<-left_join(df_country,df_data, by="country_id")
    
    #get just vaccine data 77573
    df<-df%>%
      dplyr::filter(series_id=="77573")
    
    #loop over some dates I chose two
    library(lubridate)
    df<-df%>%
      dplyr::filter(date == "2021-05-31" | date =="2021-06-18")
    
    #mutate find vax per total pop
    df<-df%>%
      dplyr::mutate("Vaccine Percentage"=value /population_total)%>%
      mutate_at(vars(`Vaccine Percentage`),~replace_na(.,0))%>%
     mutate(`date`= as.character(`date`, format="%X"))
   # df<-df%>%
      #pivot_wider(names_from = date,
       #                  values_from = `Vaccine Percentage`)
   
      
  
# VIZ ============================================================================

library(gt)
    
    #view all data in gt for country-in this example I filtered out a region
    df%>%
      gt()
  #put dates on top  
 tbl1<- df %>% 
    dplyr::filter(state_region=="Europe and Eurasia")%>%
    dplyr::select(-c(country_id, iso_alpha3,usaid_region,data_services_region,classification_value_name
                          ,population_total,pop_2020_thousands,series_id,state_region,value  ))%>%
    pivot_wider(names_from = date, values_from=`Vaccine Percentage`)%>%
    gt()%>%
      #rowname_col = "Date",
  tab_spanner(
    label = "Total Population Vaccinated",
    columns = c("2021-05-31","2021-06-18"))%>%
  fmt_percent(
    columns = vars("2021-05-31","2021-06-18"),
    decimals = 2)%>%
   fmt_missing(columns = everything(),
               missing_text = "-") %>% 
   tab_options(
     table.font.names = "Source Sans Pro") %>%
   cols_width(
     
     everything() ~ px(140)
   ) %>% 
   tab_style(
     style = cell_borders(
       sides = "right",
       weight = px(1.5),
     ),
     locations = cells_body(
       columns = everything(),
       rows = everything()
     ))%>%
  cols_label(country_name="Name")%>%
  tab_header(title = "Status of Global Vaccination Campaign in Europe and Eurasia as of June 18, 2021",
             ) %>% 
  tab_source_note("Source: CJ's Dataset 2020-06-18")%>%
   tab_footnote(
     footnote = "Percentage is based off of total population, not the population 18+",
     locations = cells_column_labels(
       columns = "2021-06-18"))
  
  
  #ignore the below
  
    tab_style(style = cell_fill(color = pal[4]),      ## defining the what (the 4th value of the pal object)
              locations = cells_body(                 ## telling it where (ie, the body of a cell)
                columns = vars("Vaccine Percentage"),      ## which col this refers to (note `vars()`)
                rows = "Vaccine Percentage" >= .2)) %>%   ## the argument
    tab_style(style = cell_fill(color = pal[3]),
              locations = cells_body(
                columns = vars("Vaccine Percentage"),
                rows = "Vaccine Percentage" < .2 &  "Vaccine Percentage" >= .15)) %>% 
    tab_style(style = cell_fill(color = pal[2]),
              locations = cells_body(
                columns = vars("Vaccine Percentage"),
                rows = "Vaccine Percentage" < .15 &   "Vaccine Percentage" >= .05)) %>% 
    tab_style(style = cell_fill(color = pal[1]),
              locations = cells_body(
                columns = vars("Vaccine Percentage"),
                rows = "Vaccine Percentage" < .05))
 
 gtsave(tbl1, "vax_table.png")
 
 #This one is to focus on one region (old)
 df1<-df%>%
   select(state_region, population_total,value, date)%>%
   group_by(state_region,date)%>%
   summarize(Vaccine_total=sum(value),Population=sum(population_total))%>%
   dplyr::mutate("Vaccine Percentage"=Vaccine_total /Population)%>%
   as.data.frame(df1)
 tbl2<-df1%>%
   dplyr::select(-c("Vaccine_total", "Population"))%>%
   gt(groupname_col = "state_region")%>%
         tab_spanner(
           label = "Total Population Vaccinated",
           columns = c("Vaccine Percentage"))%>%
         fmt_percent(
           columns = vars("Vaccine Percentage"),
           decimals = 2
         )
 gtsave(tbl2, "vax_table_region.png")

 
 #NEw way to focus on regional data
#grab population data   
 df2<-df%>%
   group_by(state_region) %>%                         # Specify group indicator
   summarise_at(vars(population_total),              # Specify column
                list(population = sum))
 #grab vax data
 df_vaxr<-df%>%group_by(state_region, date)%>%
   summarise_at(vars(value),              # Specify column
                list(value= sum))
 
   

 df3<-left_join(df_vaxr,df2, by="state_region")%>%
   dplyr::mutate("Vaccine Percentage"=value /population)
 df3<-df3%>%
   dplyr::filter(state_region!="NA")
 df5<-df3%>%
   dplyr::select(-c("value", "population"))%>%
   pivot_wider(names_from = date, values_from="Vaccine Percentage")
 
 
tbl4<-df3%>%
  dplyr::select(-c("value", "population"))%>%
  pivot_wider(names_from = date, values_from="Vaccine Percentage")%>%
  gt()%>%
 
  tab_spanner(
    label = "Total Population Vaccinated",
    columns = c("2021-05-31","2021-06-18"))%>%
  fmt_percent(
    columns = vars("2021-05-31","2021-06-18"),
    decimals = 2)%>%
  fmt_missing(columns = everything(),
              missing_text = "-") %>% 
  tab_options(
    table.font.names = "Source Sans Pro") %>%
  cols_width(
    
    everything() ~ px(140)
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "all",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ))%>%
  #cols_label(country_name="state_region")%>%
  tab_header(title = "Status of Global Vaccination Campaign  as of June 18, 2021",
  ) %>% 
  tab_source_note("Source: CJ's Dataset 2020-06-18")%>%
  tab_footnote(
    footnote = "Percentage is based off of total population, not the population 18+",
    locations = cells_column_labels(
      columns = "2021-06-18"))
#  text_transform(
 #   locations = cells_body(
  #    columns = vars("2021-05-31","2021-06-18"),
   #   rows = "2021-05-31" > "2021-06-18"),
#    fn = function(x) paste(x, up_arrow)
 # ) %>%
  #text_transform(
   # locations = cells_body(
    #  columns = close,
     # rows = "2021-06-18" < "2021-05-31"),
  #  fn = function(x) paste(x, down_arrow)
#  )
 gtsave(tbl4, "vax_table_region_new.png")

    
# SPINDOWN ============================================================================

