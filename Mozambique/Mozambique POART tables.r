---
title: "Supporting nice tables for Q reviews or Outlays"
author: adapted from"jdavis"
date: "1/4/2021"
output: html_document
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


We'll be reproducing the table located here:
https://docs.google.com/presentation/d/1tR3a81fd5mtkAX-RkX3hZv7skahkHryIrbrI7mli1Pc/edit#slide=id.p6


T
## Libraries + globals
  
```{r}
library(tidyverse)
library(glamr)
#install.packages("gt")
library(gt)
library(googledrive)
library(googlesheets4)
library(ICPIutilities)
library(glitr)
library(RColorBrewer)
library(scales)
library(dplyr)
library(extrafont)
library(scales)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)
library(janitor)
library(lubridate)


pal <- RColorBrewer::brewer.pal(5, "Spectral")[2:4] ## we'll come back to this
save_path <- "2021-01-18"

```
  
##munging your data
  We're going to look at a simple table that's in long format and convert to wide using `pivot_wider
  
```{r}
## load Outlay File
 df<-read.csv("Data/Q2 Outlay-Moz.csv")
  df<-df%>%
      dplyr::rename("COP 21 Budget" = COP.Planning.Level,
                    "Mechanism Name"=Mechanism.Name,
                    "Prime Partner Name"=Prime.Partner.Name)
 df<-df%>%
    mutate_at(vars(Outlay: `COP 21 Budget`),~replace_na(.,0))
  
 df<-df%>%
  dplyr::mutate("Outlay Percentage"= Outlay/`COP 21 Budget`)
 df1<-df %>% 
  dplyr::select(`Prime Partner Name`, `Mechanism Name`,Outlay,`COP 21 Budget`,`Outlay Percentage`)


```

```{r}

df1 %>% 
  %>%
  arrange(desc(`Outlay Percentage`))%>% # sort by outlay %
  gt()%>%
  fmt_percent(
   columns = vars(`Outlay Percentage`),
  decimals = 0)%>%
   fmt_currency( # add dolar signs
    columns = vars(`COP 21 Budget`,`Outlay`),
    decimals = 0,
    currency = "USD")%>%
    grand_summary_rows(
    columns = vars(`Outlay`,`COP 21 Budget`),
    fns = "sum",
      formatter = fmt_currency,
    currency = "USD",
    decimals = 0)%>%
  grand_summary_rows(
    columns = vars(`Outlay Percentage`),
    fns = "mean",
      formatter = fmt_percent,
    decimals = 0)%>%
   tab_options(
    table.font.names = "Source Sans Pro"
    ) %>% 
   cols_width(
    vars(OU) ~ px(110),
    everything() ~ px(120))%>%
 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    ))%>%
  


  tab_style(style = cell_fill(color ="#ffcaa2"),      ## defining the what (the 4th value of the pal object)
              locations = cells_body(                 ## telling it where (ie, the body of a cell)
                columns = vars(`Outlay Percentage`),      ## which col this refers to (note `vars()`)
                rows = `Outlay Percentage` >= .6)) %>%   ## the argument   ## using Q2 colors here
    tab_style(style = cell_fill(color = "#5bb5d5"),
              locations = cells_body(
                columns = vars(`Outlay Percentage`),
                rows = `Outlay Percentage` < .6)) %>% 
    tab_style(style = cell_fill(color ="#ff939a"  ),
              locations = cells_body(
                columns = vars(`Outlay Percentage`),
                rows = `Outlay Percentage` < .4)) %>% 
 tab_header(title = "USAID Outlays Through FY21 Q2") %>% 
  tab_source_note("Source:USAID Phoenix Financial System May 2021")
  


```

 


`