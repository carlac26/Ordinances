
#Libraries------------------------------------------------------------------------------------------------------------

#load packages
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggtext)
library(showtext)
library(viridis)
font_add_google("Lato")
showtext_auto()


#Oridinance Dataset---------------------------------------------
#import csv , df <- names imported dataset 
##read_csv(imports csv input csv name here usually need full path name but file is already in my environment so i dont need it)
df <- read_csv("ordinances_r.csv")
#view() shows dataset
View(df)

#using package lubridate names Date as a date column, ymd is format date is in
#need to use this exact notation of dataframe$columnname so it changes the column in the data set
df$Date <- ymd(df$Date)

#manipulate dataset into time series formate - remove "dummy" variables
##name new set time_series
ordinancesdf <- df %>%
#recognize year column as date column in the year format 
  mutate(year = lubridate::year(Date)) %>%
#select only specific columns needed for data visualization, the ":" means select all columns between x:y 
  select(year, Setback:"visual impact" ) %>%
#filters so only includes years after 2007, not enough before then stretches out graph but can remove filter
  filter(year > 2007) %>%
  group_by(year) %>%
#sums across columns based on year grouping , so adds up total setbacks per year, does this same calculation across columns specified
  summarise(
    across(Setback:"visual impact",
           list( n = ~ sum(.x, na.rm = TRUE))))

#change dataset into 3 columns with only 2 variables and one grouping column to be able to put into ggplot to graph 
time_series <-  ordinancesdf %>%
#lengthens dataset by pivoting specified columns into one column, this will be our grouping colomn  
  pivot_longer(
 #!year calls every column except for the year column     
    cols = !year,
#name of new column holding the grouping variables
      names_to = "ordinance",
#drop NAs
      values_drop_na = TRUE,
#place values in pivoted columns into new colomn called count
      values_to = "count"
    )

#to export cleaned data
#write.csv(time_series,"~/R/Solar ordinances/timeseries.csv", row.names = FALSE)

#simple line plot using gglpot
lineplot <-  ggplot(time_series,
          aes(x=year, y=count, group=ordinance, color=ordinance)) +
          geom_line() +
          theme_ipsum() +
          ggtitle("Ordinances") +
#add more ticks scaled continously
          scale_x_continuous(
            breaks = scales::pretty_breaks(n = 6))
# Turn it interactive using ggplotly
ggplotly(lineplot, tooltip = c("x", "y","group")) 

 #make separate line charts on a grid
seperatep <- time_series %>%
  ggplot( aes(x=year, y=count, group=ordinance, fill=ordinance)) +
  geom_area() +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("ordinances") +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=14)
  ) +
  facet_wrap(~ordinance)

# Turn it interactive
b <- ggplotly(seperatep)
b

stacked <-  time_series %>%
  ggplot( aes(x=year, y=count, fill=ordinance, text=ordinance)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  theme(legend.position="none") +
  ggtitle("Ordinances") +
  theme_ipsum()

# Turn it interactive
p <- ggplotly(stacked, tooltip="text")
p

#Projects Dataset-------------------------------------------

#import dataset
projectsdf <- read_csv("solarprojectsdirty.csv")

projects <- projectsdf %>%
  #recognize year column as date column in the year format 
  mutate(Year = lubridate::year(Operating_Date)) %>%
  select( Year, Capacity = `Nameplate Capacity (MW)`) %>%
  group_by(Year) %>%
  summarise(total_capacity = sum(Capacity, na.rm = TRUE) ) %>%
  #change in capacity
  mutate(chg_cap = total_capacity - lag(total_capacity))

#combine datasets
combined <- df %>%
  mutate(Year = lubridate::year(Date)) %>%
  count(Year) %>%
  full_join(projects, by = 'Year')

  

projects_state <- projectsdf %>%
  #recognize year column as date column in the year format 
  mutate(Year = lubridate::year(Operating_Date)) %>%
  select( Year, State, Capacity = `Nameplate Capacity (MW)`) %>%
  group_by(Year, State) %>%
  summarise(total_capacity = sum(Capacity, na.rm = TRUE) ) 


#state by state view
projects_state_wide <- projects_state %>%
  pivot_wider(names_from= State, values_from = total_capacity)
  
  
state_df <- df %>%
  group_by(State) %>%
  summarise( n = n() )


write.csv(state_df,"~/R/Solar ordinances/state_df.csv", row.names = FALSE)


