library(tidyverse)
library(NTLlakeloads)

# Load Light
# Package ID: knb-lter-ntl.259.21 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Light Extinction 1981 - current.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/259/21/561d4f72dd0615bb24357c4bc5459e57" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

light <- read_csv(infile1) |> 
  filter(lakeid == 'WI') |> 
  filter(!is.na(extcoef))

# Load Secchi
secchi = loadLTERsecchi() |> filter(lakeid == 'WI') |> 
  dplyr::select(sampledate, year4, secnview)

# Calculate photic zone depth
# I(z) = I0*e^(−kz)
# z = −(1/k) * log(I/I0)

# Add photic zone depth
light.join = light |> left_join(secchi) |> 
  mutate(pz = -(1/extcoef) * log(0.01)) |> 
  filter(month(sampledate) %in% c(6,7,8)) |> 
  filter(year4 >= 2021)

ggplot(light.join) + 
  geom_smooth(aes(x = pz, y = secnview), method = 'lm', color = 'black', linewidth = 0.3) +
  geom_point(aes(x = pz, y = secnview), size = 1.1) +
  xlab('Photic Zone Depth (m)') +
  ylab('Secchi Depth (m)') +
  theme_bw(base_size = 9)

ggsave('figures/FigureS3.png', width = 4, height = 2, dpi = 500)

# Linear model fit
lm(pz ~ secnview, data = light.join)


