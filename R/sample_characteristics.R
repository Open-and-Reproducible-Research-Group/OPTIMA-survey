library(tidyverse)
library(sf)
library(fuzzyjoin)

df <- read_csv("data/processed/preprocessed_data.csv")


# Survey respondents
table(df$X63, df$X64)

df$X63 <- factor(df$X63, levels = c("National", "LPU", "SumDU", "DonNU", "LutskNTU"))

ggplot(df, aes(x = X63, fill = as_factor(X64))) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Survey Respondents by Survey and Year", x = "Survey", y = "Count") +
  theme_minimal() +
  theme(legend.title = element_blank())


# Academic role
table(df$X8, df$X64)


# Gender
table(df$X9, df$X64)


# Degree
table(df$X10, df$X64)


# Years working in research/higher education
table(df$X11, df$X64)


# Field
table(df$X12, df$X64)


# Publications in past 3 years
table(df$X13, df$X64)


# Peer reviews in past 3 years
table(df$X14, df$X64)


# Type of institution
table(df$X54, df$X64)


# Location of institution for mapping
locations <- df %>% 
  group_by(X64, X55) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = X64, values_from = count) %>% 
  rename(NAME_1 = X55) %>% 
  mutate(NAME_1 = gsub(" Oblast", "", NAME_1)) %>% 
  mutate(NAME_1 = gsub("Autonomous Republic of ", "", NAME_1))

# Add Sevastopol for completeness
locations[nrow(locations) + 1,] = list("Sevastopol", 0, 0, 0)

ukraine_map <- st_read("data/additional/map/gadm41_UKR_1.dbf") %>%
  filter(!grepl("\\?", NAME_1)) %>% 
  st_make_valid(.)


# Join sample and geographical data by fuzzy string matching
map_counts <- stringdist_join(ukraine_map, locations, 
                              by = "NAME_1",
                              mode = "left",
                              method = "jw",
                              max_dist = 99, 
                              distance_col = "dist") %>%
  group_by(NAME_1.x) %>%
  slice_min(order_by = dist, n = 1)


# Create again shapefile out of merged data
map_counts <- st_as_sf(map_counts)


# Plot number of respondents by region
ggplot() +
  geom_sf(data = map_counts, aes(fill = `2021`), color = "white") +
  scale_fill_gradient(low = "white", high = "navy") +
  geom_text(data = map_counts,
            aes(label = NAME_1.y,
                x = st_coordinates(st_centroid(geometry))[, "X"],
                y = st_coordinates(st_centroid(geometry))[, "Y"]),
            color = "black", size = 2, check_overlap = FALSE) +
  theme(axis.text = element_blank(),  # Hide axis labels
        axis.title = element_blank(),  # Hide axis titles
        axis.ticks = element_blank(), # Hide axis ticks
        legend.title = element_blank(), # Hide legend title
        panel.grid = element_blank())  # Hide grid lines


# Additional 2022 questions


