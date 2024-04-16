library(tidyverse)
library(sf)

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


# Location of institution
locations <- df %>% 
  group_by(X64, X55) %>% 
  summarize(count = n()) %>% 
  pivot_wider(names_from = X64, values_from = count)

ukraine_map <- st_read("data/additional/map/UKR_adm1.shp") %>% 
  st_make_valid(.)


ggplot() +
  geom_sf(data = ukraine_map, color = "white") +
  geom_text(data = ukraine_map,
            aes(label = NAME_1,
                x = st_coordinates(st_centroid(geometry))[, "X"],
                y = st_coordinates(st_centroid(geometry))[, "Y"]),
            color = "black", size = 3, check_overlap = TRUE)


# Additional 2022 questions


