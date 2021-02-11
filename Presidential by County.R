setwd("C:/Users/icamo/OneDrive/Documents/Statistics Books and Data/ggplot2 Elegent Graphics")
library(ggplot2)
library(dplyr)
library(tidyr)

# USE DIFFERENT DATASET
county.pres = na.omit(read.csv('countypres_2000-2016.csv'))
head(county.pres)

unique(county.pres$party)

# Use 2016 data
county.pres.2016 = filter(county.pres, year == 2016)
head(county.pres.2016)
unique(county.pres.2016$party)

# Add proportional votes of each county
county.pres.2016 = county.pres.2016 %>%
  select(year, state, state_po, county, party, candidatevotes, totalvotes) %>%
  mutate(propvotes = candidatevotes / totalvotes)
head(county.pres.2016)

county.pres.2016 = county.pres.2016 %>%
  select(year, state, county, party, propvotes)
head(county.pres.2016)

# SC county selection
sc.county.2016 = filter(county.pres.2016, state == 'South Carolina')
head(sc.county.2016)

# Spread out the party
# Make all counties lowercase to match county map and indicate winning
# party
sc.county.pres.sp = spread(sc.county.2016, key = party, value = propvotes) %>%
  mutate(county = tolower(county)) %>%
  mutate(winning.party = ifelse(democrat > republican, 'Democrat', 'Republican'))
head(sc.county.pres.sp)



# Join the county and result data frames
sc_merge = merge(x = sc_counties, y = sc.county.pres.sp[,c('county', 'winning.party', 'republican')],
                 by.x = 'id', by.y = 'county', all.x = TRUE)
head(sc_merge)

# Create SC county map
sc_counties = map_data('county', 'south carolina') %>%
  select(lon = long, lat, group, id = subregion)
head(sc_counties)

# Visualizing vector boundaries
ggplot(sc_counties, aes(lon, lat)) +
  geom_polygon(aes(group = group), fill = NA, color = 'grey50') +
  coord_quickmap()

ggplot(sc_merge, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = winning.party), color = 'grey50') +
  coord_quickmap()

# Colors
party = c('Republican' = 'red', 'Democrat' = 'blue')

ggplot(sc_merge, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = winning.party), color = 'grey50') +
  scale_fill_manual(values = party) +
  coord_quickmap()

# A gradient scale for percent voting one party
ggplot(sc_merge, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = republican), color = 'grey50') +
  scale_fill_gradient2(low = 'blue', midpoint = 0.5, high = 'red') +
  coord_quickmap()


# Getting the major cities
sc_cities = maps::us.cities %>%
  as_tibble() %>%
  filter(country.etc == 'SC') %>%
  select(-country.etc, lon = long) %>%
  arrange(desc(pop))

# Adding to plot
ggplot(sc_merge, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = winning.party), color = 'grey50') +
  scale_fill_manual(values = party) +
  geom_point(aes(size = pop), sc_cities, color = 'black') +
  coord_quickmap()

ggplot(sc_merge, aes(lon, lat)) +
  geom_polygon(aes(group = group, fill = republican), color = 'grey50') +
  scale_fill_gradient2(low = 'red', midpoint = 0.5, high = 'blue') +
  geom_point(aes(size = pop), sc_cities, color = 'black') +
  coord_quickmap()
