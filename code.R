## Next Steps
## Author: Carla Canovi (The New School)
## Date: Sep 26, 2024

pacman::p_load(MASS, readxl, tseries, car, tile, simcf, # data refinement
               survival, spaMM, plm, coda, fields, # data analysis
               sf, sp, tmap, raster, spdep, rgdal, rgeos,
               ggplot2, ggpubr, shiny, shinyjs, tmaptools, graph4lg, # spatial 
               tidyverse)

#Ordinance Dataset---------------------------------------------
#import csv , df <- names imported dataset 
##read_csv(imports csv input csv name here usually need full path name but file is already in my environment so i dont need it)
#df <- read_csv("ordinances_r.csv")
#view() shows dataset
#View(df)
#add dates from mit dataset in
#mit <- read_csv("solarord_long_revised.csv") 

#mit <- mit %>%
 #rename(County = County, State = state_name, Name = citation) %>%
  #select(County,State, year, Name)
  


#using package lubridate names Date as a date column, ymd is format date is in
#need to use this exact notation of dataframe$columnname so it changes the column in the data set
#df$Date <- ymd(df$Date)

#manipulate dataset into time series formate - remove "dummy" variables
##name new set time_series
#df <- df %>%
#recognize year column as date column in the year format 
  #mutate(year = lubridate::year(Date)) 


#revised_df <- df %>%
 # left_join( mit, join_by(State, County, Name) )%>%
  #mutate(year = ifelse(
   # Source == "NREL" & !is.na(year.y), year.y, year.x),
    #year.x = NULL, year.y = NULL , Year = NULL, Date = NULL) %>%
  #distinct()
    

#write_csv(revised_df, "updatedyearordinance.csv")

revised_df <- read_csv("updatedyearordinance.csv")

######clean duplicates ######
namecol <- revised_df %>%
  select(Name)%>%
  distinct() 

a <- replace(revised_df, revised_df==0, NA)
  
  setback <- a %>% 
    select(Name, Setback) %>%
    drop_na(Setback) %>%
  left_join(namecol, by= "Name") 
  
  setback <- a %>% 
    select(Name, Setback) %>%
    drop_na(Setback) %>%
    left_join(namecol, by= "Name") 

########### Full County + Year Dataset####
  
  
  data <- read_excel("county_list.xlsx")
  
  year <- c(rep(2010,3142), rep(2011,3142), rep(2012,3142),
            rep(2013,3142), rep(2014,3142), rep(2015,3142),
            rep(2016,3142), rep(2017,3142), rep(2018,3142),
            rep(2019,3142), rep(2020,3142), rep(2021,3142),
            rep(2022,3142), rep(2023,3142), rep(2024, 3142))
  
  fulldata <- rbind(data,data,data,data,data,
                    data,data,data,data,data,
                    data,data,data,data,data)
  
  fulldata$year <- year
#state refined 
unique(revised_df$State) %>% sort()  

revised_df$State[revised_df$State=="MISSOURI"] <- "Missouri"

revised_df$State[revised_df$State=="ARIZONA"] <- "Arizona"

revised_df$State[revised_df$State=="ALABAMA"] <- "Alabama"

revised_df$State[revised_df$State=="Forida"] <- "Florida"
#County Refined
revised_df$County[revised_df$County=="n/a"] <- "N/A" 
revised_df$County[revised_df$County=="N/a"] <- "N/A" 

revised_df$County[revised_df$County=="Dekalb"] <- "DeKalb" 
revised_df$County[revised_df$County=="DeWitt"] <- "De Witt" 
revised_df$County[revised_df$County=="Koscuisko"] <- "Kosciusko"
revised_df$County[revised_df$County=="Chautaqua"] <- "Chautauqua"
revised_df$County[revised_df$County=="Lac Qui Parle"] <- "Lac qui Parle"
revised_df$County[revised_df$County=="FIllmore"] <- "Fillmore"
revised_df$County[revised_df$County=="Notrona"] <- "Natrona"
revised_df$County[revised_df$County=="Gadsen"] <- "Gadsden"
# Oglethorpe, GA (2023) -> right-censored
revised_df$County[revised_df$County=="DeWitt"] <- "De Witt" 
revised_df$County[revised_df$County=="Dearborne"] <- "Dearborn" 
revised_df$County[revised_df$County=="Koscuisko"] <- "Kosciusko"
revised_df$County[revised_df$County=="Saint Joseph"] <- "St. Joseph"
revised_df$County[revised_df$County=="Fredrick"] <- "Frederick"
revised_df$County[revised_df$County=="Queen Annes"] <- "Queen Anne's"
revised_df$County[revised_df$County=="Gladwind"] <- "Gladwin"
# Granville, NC (2009) -> excluded from the analysis (left the risk set)
# Linclon, NC (2009) -> excluded from the analysis (left the risk set)
revised_df$County[revised_df$County=="Dinwiddle"] <- "Dinwiddie"
revised_df$County[revised_df$County=="Dinwiddle"] <- "Dinwiddie"
revised_df$County[revised_df$County=="King Williams"] <- "King William"

ordinances_unique <- revised_df %>% select(State, County, year) %>% unique()

ordinancesdf <- revised_df %>%
#select only specific columns needed for data visualization, the ":" means select all columns between x:y 
  #select(year, Setback:"visual impact" ) %>%
#filters so only includes years after 2007, not enough before then stretches out graph but can remove filter
  filter(year > 2008) %>%
  group_by(year) %>%
#sums across columns based on year grouping , so adds up total setbacks per year, does this same calculation across columns specified
  summarise(
    across(Setback:"visual impact",
           list( n = ~ sum(.x, na.rm = TRUE))))

# Remove the '_n' from the end of column names
colnames(ordinancesdf) <- gsub("_n$", "", colnames(ordinancesdf))



#write_csv(ordinancesdf, "ordinancetypebyyr.csv")

################# Co-occurance Matrix ########################


# Drop the 'year' column as it's not part of the ordinance data
matrixdata <- revised_df %>%
  select(6:17)

df_yr <- ordinancesdf %>%
  select(!year)
# Convert the data frame to a matrix
ordinance_matrix <- as.matrix(matrixdata)

matrix_yr_binary <- as.matrix((df_yr>0)+0)
matrix_yr <- as.matrix(df_yr)
# Create the co-occurrence matrix by multiplying the transposed matrix with the original
co_occurrence_matrix <- t(ordinance_matrix) %*% ordinance_matrix

ordinance_matrix.cor = cor(ordinance_matrix)
ordinance_matrix.cor_spearman = cor(ordinance_matrix,method = "spearman")
ordinance_matrix.cor_spearman = cor(ordinance_matrix,method = "spearman")

#matrix showing correlations by year 
cor_matrix_yr_s = cor(matrix_yr,method = "spearman")

# Print the co-occurrence matrix
print(co_occurrence_matrix)
#save matrix

#write.csv(co_occurrence_matrix,file= "co-occurance-matrix.csv", row.names = TRUE)
#write.csv(cor_matrix_yr_s,file= "correlation-matrix-yr-spearman.csv", row.names = TRUE)
#######histogram######

# Sum the occurrences of each ordinance type across all years
ordinance_totals <- colSums(ordinancesdf[ , !(names(ordinancesdf) %in% c("year"))])

# Convert the results to a data frame for plotting
ordinance_df <- data.frame(
  Ordinance = names(ordinance_totals),
  Frequency = as.numeric(ordinance_totals)
)

# Create a histogram using ggplot2
p <- ggplot(ordinance_df, aes(x = reorder(Ordinance, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the coordinates to make it horizontal
  labs(title = "Frequency of Each Ordinance Type",
       x = "Ordinance Type",
       y = "Frequency") +
  theme_minimal()


ggplotly(p, tooltip = "y")


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
write.csv(time_series,"timeseries.csv", row.names = FALSE)

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


#write.csv(state_df,"~/R/Solar ordinances/state_df.csv", row.names = FALSE)


