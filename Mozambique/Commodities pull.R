




library(glamr)
library(tidyverse)
library(gophr)
library(extrafont)
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

df_fsd<-si_path()%>%
  return_latest("commodities_data")%>%
  read_csv()

df_fsd<-df_fsd%>%
  pivot_wider(names_from = `Major Category`, values_from="Budget")

df_fsd<-df_fsd%>%
  mutate(total_com=ARV+Laboratory+RTKs,
          com_perct=total_com/Total*100)

write.csv(df_fsd, "Commodities data for Moz 1.27.2021.csv")