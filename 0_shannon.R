library(tidyverse)
library(vegan)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/23/30/aa5720aec0e577431faeee352b91a937" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))

#Organize data for all depths
d1<- read_csv(infile1)|>
  filter(lakeid == "WI")

df<- d1%>%
  filter(spname != "UNKNOWN"  & spname != "UNKNOWN 1")%>%
  filter(spname != "FILAMENTOUS ALGAE")%>%
  filter(spname != "FILAMENTOUS ALGEA")%>%
  filter(spname != "LEMNA MINOR")%>%
  mutate(spname = ifelse(spname == "POTAMOGETON NATANS:", "POTAMOGETON NATANS", spname))%>%
  mutate(spname = ifelse(spname == "NYMPHAEA ODORATA SSP. TUBEROSA", "NYMPHAEA ODORATA",
                         ifelse(spname == "NUPHAR VARIEGATA", "NYMPHAEA ODORATA", spname)))%>%
  mutate(spname = ifelse(spname == "STRUCKENIA PECTINATA", "STUCKENIA PECTINATA", spname))%>%
  mutate(spname = ifelse(spname == "STUCKENIA PECTINATA", "Potamogeton pectinatus L.", spname))%>%
  mutate(spname = ifelse(spname == "BRASENIA SCHREBERI:", "BRASENIA SCHREBERI", spname))%>%
  dplyr::select(-flag)

# Group by year and species, then count occurrences
species_counts_by_year <- df %>%
  group_by(year4, spname) %>%
  summarize(Count = n(), .groups = 'drop')

# Calculate the total number of occurrences per year
total_occurrences_by_year <- species_counts_by_year %>%
  group_by(year4) %>%
  summarize(Total_Count = sum(Count), .groups = 'drop')

# Join the total occurrences back to the species counts
species_counts_with_totals <- species_counts_by_year %>%
  left_join(total_occurrences_by_year, by = "year4")

# Calculate relative frequencies
species_counts_with_frequencies <- species_counts_with_totals %>%
  mutate(Relative_Frequency = Count / Total_Count)

#decending order
species_counts_with_frequencies <- species_counts_with_frequencies %>%
  arrange(year4, desc(Relative_Frequency))%>%
  mutate(Relative_Frequency_perc =Relative_Frequency*100 )

table_frequ<- species_counts_with_frequencies %>% 
  dplyr::select(year4, spname, Relative_Frequency_perc)%>%
  pivot_wider(names_from = year4, values_from = Relative_Frequency_perc)

#Shannon

# Select only numeric columns (exclude 'label')
df2_numeric <- table_frequ[, -1]  # or df2 %>% select(-label)

# Replace NAs with 0 (optional, depending on how you want to treat missing data)
df2_numeric[is.na(df2_numeric)] <- 0

# Apply Shannon index per column (i.e., time period)
shannon_index <- apply(df2_numeric, 2, function(x) diversity(x, index = "shannon"))

# Create a tidy dataframe for plotting or export
shannon_index_df <- data.frame(
  year = names(shannon_index),
  shannon = as.numeric(shannon_index)
)

#Average based off year groupings
data_category<- shannon_index_df%>%
  mutate(Year = as.numeric(year))%>%
  mutate(year = ifelse(Year < 1998, "1995-1997",
                       ifelse(Year > 1997 & Year < 2001, "1998-2000",
                              ifelse(Year > 2000 & Year < 2004, "2001-2003",
                                     ifelse(Year > 2003 & Year < 2008, "2004-2007",
                                            ifelse(Year > 2007 & Year < 2011, "2008-2010",
                                                   ifelse(Year > 2010 & Year < 2014, "2011-2013",
                                                          ifelse(Year > 2013 & Year < 2017, "2014-2016", "2017-2018"))))))))

data_category_summary<- data_category%>%
  group_by(year)%>%
  summarize(shannon = mean(shannon))


