library(tidyverse)

# Download file from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/38/30/66796c3bc77617e7cc95c4b09d4995c5" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

chloro_lter<- read_csv(infile1)|> filter(lakeid == "WI") |>
  filter(depth_range_m %in% c("0", "0-2"))|>  
  filter(month(sampledate) %in% c(6,7,8)) 

#Use correct fluor, then uncorrect fluor, and spec if only available
#Take the average when there are multiple replicates per sampling dates
chloro_all <- chloro_lter %>%
  group_by(lakeid, year4, sampledate) %>%
  summarise(
    avg_correct = mean(correct_chl_fluor, na.rm = TRUE),
    avg_uncorrect = mean(uncorrect_chl_fluor[is.na(correct_chl_fluor)], na.rm = TRUE),
    avg_spec = mean(tri_chl_spec[is.na(correct_chl_fluor) & is.na(uncorrect_chl_fluor)], na.rm = TRUE),
    chl_use = coalesce(avg_correct, avg_uncorrect, avg_spec),
    method_used = case_when(
      !is.na(avg_correct) ~ "correct_fluor",
      !is.na(avg_uncorrect) ~ "uncorrect_fluor",
      !is.na(avg_spec) ~ "spec",
      TRUE ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  filter(method_used == 'correct_fluor') |> 
  select(lakeid, year4, sampledate, chl_use, method_used) |> 
  mutate(removal = ifelse(year(sampledate) < 2008, "<2008", ">=2008"))

