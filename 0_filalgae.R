
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/24/30/a03d18be68db4cf2280846afe2643d5e" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

macrophyte_ntl<- read_csv(infile1)|> filter(lakeid == "WI")|> filter(year4< 2020)

#Filamentous algae
fil_algae_timeseries<- macrophyte_ntl%>%
  mutate(fil_algae_wt = replace_na(fil_algae_wt, 0))%>%
  group_by(year4)%>%
  summarize(fil_algae_sum = sum(fil_algae_wt))%>%
  mutate(removal = ifelse(year4 < 2008, '< 2008', '≥ 2008'))

#write_csv(fil_algae_timeseries, "data/fil_algae_timeseries.csv")
