# Function to create a table with answer categories as basis for plot
table_answers <- function(df, question, group,
                          filter = NULL, filter_val = NULL) {
  
  if (!is.null(filter)) {
    df <- df %>% 
      filter(.data[[filter]] == filter_val)
  }
  
  answers <- df %>%
    group_by(.data[[group]]) %>%
    pivot_longer(question, names_to = "var", values_to = "val") %>%
    count(var, val) %>% 
    mutate(perc = round((n/sum(n) * 100), 2))
  
  answers
}

  
# Create a stand-alone overview table for grouped item responses
table_answers_overview <- function(df, columns, group,
                                   filter = NULL, filter_val = NULL) {
  
  answers <- data.frame(group = character(),
                        var = character(),
                        val = character(),
                        n = integer(),
                        perc = numeric())
  
  names(answers)[names(answers) == "group"] <- group
  
  for (column in columns) {
    answer <- table_answers(df, column, group, filter, filter_val)
    answers <- rbind(answers, answer)
  }
  
  answers <- answers %>% 
    mutate(val = fct_explicit_na(val),
           val = gsub("'", "", val),
           val = gsub(" ", "_", val)) %>% 
    pivot_wider(names_from = val, values_from = c(n, perc)) 
  
  answers
}


# Create an overview plot over the responses to various items (without grouping)
plot_agreement_overview <- function(
    df, var_overview, columns, sort = TRUE,
    filter = NULL, filter_val = NULL) {
  
  if (!is.null(filter)) {
    df <- df %>% 
      filter(.data[[filter]] == filter_val)
  }
  
  step1 <- df %>% 
    select(all_of(columns)) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "val") %>%
    count(var, val) %>% 
    mutate(total_perc = n/nrow(df))
  
  # Remove "don't know" answers and NAs
  nas <- step1 %>% 
    filter(val %in% c("NA", "don't know"))
  
  pdata <- step1 %>% 
    anti_join(nas) %>% 
    mutate(val = fct_relevel(val, "strongly agree", "rather agree",
                             "rather disagree", "strongly disagree")) %>% 
    group_by(var) %>%
    mutate(prop = n/sum(n),
           order = if (sort) {
             case_when(
               str_detect(val, "\\sagree$") ~ prop,
               TRUE ~ 0) } else {0},
           order = sum(order))
  
  labels <- var_overview %>% 
    filter(var_id %in% columns) %>% 
    mutate(label = str_wrap(var_full, width = 80)) %>%  
    select(var_id, label)
  
  pdata_item <- pdata %>% 
    left_join(labels, by = c("var" = "var_id"))
  
  p1 <- pdata_item %>% 
    # Replace 'var' with 'label' for full item text (needs linebreak solution)
    ggplot(aes(fct_reorder(var, order), prop, fill = val)) +
    geom_chicklet(width = .7) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#1065ab", "#8ec4ca", "#f6a582", "#b31529")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata_item, var, order)) %>% 
    left_join(labels, by = c("var" = "var_id")) %>% 
    distinct() %>% 
    group_by(label, order) %>% 
    mutate(val = factor(val, levels = c("NA", "don't know")))
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(label, order), x = total_perc, fill = val)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c("NA" = "grey70", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3),
                       labels = function(x) paste0(round(x * 100, 0), "%")) +
    scale_y_discrete(position = "right") +
    guides(fill = guide_legend(ncol = 1)) +
    theme(legend.position = "top",
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
  
  final_plot <- p1 + 
    theme(plot.margin = margin()) + p2 +
    plot_layout(widths = c(6, 1))
  
  final_plot
  
}


# Create an overview plot over the responses to various items (without grouping)
plot_frequency_overview <- function(
    df, var_overview, columns, sort = TRUE,
    filter = NULL, filter_val = NULL) {
  
  if (!is.null(filter)) {
    df <- df %>% 
      filter(.data[[filter]] == filter_val)
  }
  
  step1 <- df %>% 
    select(all_of(columns)) %>% 
    pivot_longer(everything(), names_to = "var", values_to = "val") %>%
    count(var, val) %>% 
    mutate(total_perc = n/nrow(df))
  
  # Remove "don't know" answers and NAs
  nas <- step1 %>% 
    filter(val %in% c("NA", "don't know"))
  
  pdata <- step1 %>% 
    anti_join(nas) %>% 
    mutate(val = fct_relevel(val, "very often", "frequently", "sometimes",
                             "rarely", "never")) %>% 
    group_by(var) %>%
    mutate(prop = n/sum(n),
           order = if (sort) {
             case_when(
               str_detect(val, "\\brarely\\b|\\bnever\\b") ~ prop,
               TRUE ~ 0) } else {0},
           order = sum(order))
  
  labels <- var_overview %>% 
    filter(var_id %in% columns) %>% 
    mutate(label = str_wrap(var_full, width = 80)) %>%  
    select(var_id, label)
  
  pdata_item <- pdata %>% 
    left_join(labels, by = c("var" = "var_id"))
  
  p1 <- pdata_item %>% 
    # Replace 'var' with 'label' for full item text (needs linebreak solution)
    ggplot(aes(fct_reorder(var, order), prop, fill = val)) + 
    geom_chicklet(width = .7) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#1065ab", "#8ec4ca", "#e0e0e0",
                                 "#f6a582", "#b31529")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata_item, var, order)) %>% 
    left_join(labels, by = c("var" = "var_id")) %>% 
    distinct() %>% 
    group_by(label, order)  %>% 
    mutate(val = factor(val, levels = c("NA", "don't know")))
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(label, order), x = total_perc, fill = val)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c("NA" = "grey70", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_y_discrete(position = "right") +
    scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6),
                       labels = function(x) paste0(round(x * 100, 0), "%")) +
    guides(fill = guide_legend(ncol = 1)) +
    theme(#panel.border = element_rect(fill = NA, colour = "grey80"),
          legend.position = "top",
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
  
  final_plot <- p1 + 
    theme(plot.margin = margin()) + p2 +
    plot_layout(widths = c(6, 1))
  
  final_plot
  
}


# Plot answers to agreement items separated by grouping variable
plot_agreement <- function(
    df, var_overview, question, group,
    filter = NULL, filter_val = NULL, sort = FALSE) {
  
  if (!is.null(filter)) {
    df <- df %>% 
      filter(.data[[filter]] == filter_val)
  }
  
  step1 <- table_answers(df, question, group, filter, filter_val)
  
  # Remove "don't know" answers and NAs
  nas <- step1 %>% 
    filter(val %in% c("NA", "don't know"))
  
  pdata <- step1 %>% 
    anti_join(nas) %>% 
    mutate(val = fct_relevel(val, "strongly agree", "rather agree",
                             "rather disagree", "strongly disagree")) %>% 
    group_by(.data[[group]], var) %>%
    mutate(prop = n/sum(n),
           order = if (sort) {
             case_when(
               str_detect(val, "\\sagree$") ~ prop,
               TRUE ~ 0) } else {0},
           order = sum(order))
  
  p1 <- pdata %>% 
    ggplot(aes(fct_reorder(as.factor(.data[[group]]), order), prop, fill = val)) +
    geom_chicklet(width = .7) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#1065ab", "#8ec4ca", "#f6a582", "#b31529")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata, var, order)) %>% 
    distinct() %>% 
    group_by(.data[[group]], order) %>% 
    mutate(val = factor(val, levels = c("NA", "don't know")))
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(as.factor(.data[[group]]), order),
               x = perc, fill = val)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c("NA" = "grey70", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_discrete(position = "right") +
    guides(fill = guide_legend(ncol = 1)) +
    theme(#panel.border = element_rect(fill = NA, colour = "grey80"),
          legend.position = "top",
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
  
  final_plot <- p1 + 
    theme(plot.margin = margin()) + p2 +
    plot_layout(widths = c(6, 1))
  
  
  final_plot
  
}


# Plot answers to frequency items separated by grouping variable
plot_frequency <- function(
    df, var_overview, question, group,
    filter = NULL, filter_val = NULL, sort = FALSE) {
  
  if (!is.null(filter)) {
    df <- df %>% 
      filter(.data[[filter]] == filter_val)
  }
  
  step1 <- table_answers(df, question, group, filter, filter_val)
  
  # Remove "don't know" answers and NAs
  nas <- step1 %>% 
    filter(val %in% c("NA", "don't know"))
  
  pdata <- step1 %>% 
    anti_join(nas) %>% 
    mutate(val = fct_relevel(val, "very often", "frequently", "sometimes",
                             "rarely", "never")) %>% 
    group_by(.data[[group]], var) %>%
    mutate(prop = n/sum(n),
           order = if (sort) {
             case_when(
               str_detect(val, "\\brarely\\b|\\bnever\\b") ~ prop,
               TRUE ~ 0) } else {0},
           order = sum(order))
  
  p1 <- pdata %>% 
    ggplot(aes(fct_reorder(as.factor(.data[[group]]), order), prop, fill = val)) +
    geom_chicklet(width = .7) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("#1065ab", "#8ec4ca", "#e0e0e0",
                                 "#f6a582", "#b31529")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata, var, order)) %>% 
    distinct() %>% 
    group_by(.data[[group]], order) %>% 
    mutate(val = factor(val, levels = c("NA", "don't know")))
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(as.factor(.data[[group]]), order),
               x = perc, fill = val)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c("NA" = "grey70", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(breaks = c(0, 30, 60),
                       labels = function(x) paste0(x, "%")) +
    scale_y_discrete(position = "right") +
    guides(fill = guide_legend(ncol = 1)) +
    theme(#panel.border = element_rect(fill = NA, colour = "grey80"),
          legend.position = "top",
          legend.title = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          axis.ticks = element_blank())
  
  final_plot <- p1 + 
    theme(plot.margin = margin()) + p2 +
    plot_layout(widths = c(6, 1))
  
  final_plot
  
}



plot_agreement_area <- function(df, question, legend = TRUE) {
  
  props <- table_answers(df, question, group = "X64") %>% 
    mutate(val = fct_relevel(val, "NA", "don't know",
                             "strongly disagree", "rather disagree",
                             "rather agree", "strongly agree")) %>% 
    group_by(var)
  
  plot <- ggplot(props, aes(x = X64, y = perc, fill = val)) + 
    geom_area(alpha = 0.8, size = 0.5, colour = "black") +
    labs(x = "Survey Year", y = "Proportions", fill = "Responses", title = question) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_fill_manual(values = c("NA" = "grey70", "don't know" = "grey30",
                                 "strongly disagree" = "#b31529",
                                 "rather disagree" = "#f6a582",
                                 "rather agree" = "#8ec4ca",
                                 "strongly agree" = "#1065ab")) +
    geom_segment(aes(x = 2021, y = 0, xend = 2023, yend = 0)) +
    geom_segment(aes(x = 2021, y = 0, xend = 2021, yend = 100)) +
    geom_segment(aes(x = 2023, y = 0, xend = 2023, yend = 100)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.ticks = element_blank())
  
  if (!legend) {
    plot <- plot +
      theme(legend.position = "none")
  }
  
  plot
}



plot_frequency_area <- function(df, question, legend = TRUE) {
  
  props <- table_answers(df, question, group = "X64") %>%
    mutate(val = fct_relevel(val, "don't know", "never",
                             "rarely", "sometimes",
                             "frequently", "very often")) %>% 
    group_by(var)
  
  plot <- ggplot(props, aes(x = X64, y = perc, fill = val)) + 
    geom_area(alpha = 0.8, size = 0.7, colour = "black") +
    labs(x = "Survey Year", y = "Proportions", fill = "Responses", title = question) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_fill_manual(values = c("don't know" = "grey30",
                                 "never" = "#b31529",
                                 "rarely" = "#f6a582",
                                 "sometimes" = "#cdcdc8",
                                 "frequently" = "#8ec4ca",
                                 "very often" = "#1065ab")) +
    geom_segment(aes(x = 2021, y = 0, xend = 2023, yend = 0)) +
    geom_segment(aes(x = 2021, y = 0, xend = 2021, yend = 100)) +
    geom_segment(aes(x = 2023, y = 0, xend = 2023, yend = 100)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.ticks = element_blank())
  
  if (!legend) {
    plot <- plot +
      theme(legend.position = "none")
  }
  
  plot
  
}



plot_agreement_line <- function(df, question, legend = TRUE, ylim = 75) {
  
  props <- table_answers(df, question, group = "X64") %>% 
    mutate(val = fct_relevel(val, "NA", "don't know",
                             "strongly disagree", "rather disagree",
                             "rather agree", "strongly agree")) %>% 
    group_by(var)
  
  plot <- ggplot(props, aes(x = X64, y = perc, color = val)) + 
    geom_line(size = 0.8) +
    labs(x = "Survey Year", y = "Proportions", color = "Responses", title = question) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_color_manual(values = c("NA" = "grey70", "don't know" = "grey30",
                                  "strongly disagree" = "#b31529",
                                  "rather disagree" = "#f6a582",
                                  "rather agree" = "#8ec4ca",
                                  "strongly agree" = "#1065ab")) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, ylim)) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.ticks = element_blank())
  
  if (!legend) {
    plot <- plot +
      theme(legend.position = "none")
  }
  
  plot
}



plot_frequency_line <- function(df, question, legend = TRUE, ylim = 65) {
  
  props <- table_answers(df, question, group = "X64") %>% 
    mutate(val = fct_relevel(val, "don't know", "never",
                             "rarely", "sometimes",
                             "frequently", "very often")) %>% 
    group_by(var)
  
  plot <- ggplot(props, aes(x = X64, y = perc, color = val)) + 
    geom_line(size = 0.8) +
    labs(x = "Survey Year", y = "Proportions", color = "Responses", title = question) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_color_manual(values = c("don't know" = "grey30",
                                 "never" = "#b31529",
                                 "rarely" = "#f6a582",
                                 "sometimes" = "#e0e0e0",
                                 "frequently" = "#8ec4ca",
                                 "very often" = "#1065ab")) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, ylim)) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.ticks = element_blank())
  
  if (!legend) {
    plot <- plot +
      theme(legend.position = "none")
  }
  
  plot
}