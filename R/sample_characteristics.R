library(tidyverse)
library(psych)

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
table(df$X55, df$X64)


# Additional 2022 questions


