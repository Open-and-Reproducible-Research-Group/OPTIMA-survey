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


# Function to create a table with answer categories separated by year
table_answers_year <- function(df, question, group,
                               filter = NULL, filter_val = NULL) {
  
  if (!is.null(filter)) {
    df <- df %>% 
      filter(.data[[filter]] == filter_val)
  }
  
  answers <- df %>%
    group_by(.data[[group]], X64) %>%
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
    df, var_overview, columns, sort = TRUE, xlim = 0.3,
    filter = NULL, filter_val = NULL, label_width = 40) {
  
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
    mutate(label = str_wrap(var_full, label_width)) %>%  
    select(var_id, label)
  
  pdata_item <- pdata %>% 
    left_join(labels, by = c("var" = "var_id"))
  
  p1 <- pdata_item %>% 
    ggplot(aes(fct_reorder(label, order), prop, fill = val)) +
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
    left_join(distinct(pdata_item, var, order)) %>% 
    left_join(labels, by = c("var" = "var_id")) %>% 
    distinct() %>% 
    group_by(label, order) %>% 
    mutate(val = factor(val, levels = c("NA", "don't know")))
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(label, order), x = total_perc, fill = val)) +
    geom_col(width = .7) +
    scale_fill_manual(values = c("NA" = "grey50", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0, xlim),
                       labels = scales::label_percent(),
                       n.breaks = 2) +
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
    df, var_overview, columns, sort = TRUE, xlim = 0.6,
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
    mutate(label = var_short) %>%  
    select(var_id, label)
  
  pdata_item <- pdata %>% 
    left_join(labels, by = c("var" = "var_id"))
  
  p1 <- pdata_item %>% 
    ggplot(aes(fct_reorder(label, order), prop, fill = val)) + 
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
    scale_fill_manual(values = c("NA" = "grey50", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_y_discrete(position = "right") +
    scale_x_continuous(limits = c(0, xlim),
                       labels = scales::label_percent()) +
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


# Plot answers to agreement items separated by grouping variable
plot_agreement <- function(
    df, var_overview, question, group, xlim = 30,
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
    scale_fill_manual(values = c("NA" = "grey50", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0, xlim), labels = function(x) paste0(x, "%")) +
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


# Plot answers to frequency items separated by grouping variable
plot_frequency <- function(
    df, var_overview, question, group, xlim = 60,
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
    scale_fill_manual(values = c("NA" = "grey50", "don't know" = "grey30")) +
    labs(x = NULL, y = NULL) +
    scale_x_continuous(limits = c(0, xlim),
                       labels = function(x) paste0(x, "%")) +
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



plot_agreement_area <- function(df, question, legend = TRUE,
                                filter_dks = TRUE, filter_nas = TRUE) {
  
  
  if (filter_dks) {
    df <- df %>% 
      filter(.data[[question]] != "don't know")
  }
  
  if (filter_nas) {
    df <- df %>% 
      filter(.data[[question]] != "NA")
  }
  
  props <- table_answers(df, question, group = "X64") %>% 
    mutate(val = fct_relevel(val, "NA", "don't know",
                             "strongly disagree", "rather disagree",
                             "rather agree", "strongly agree")) %>% 
    group_by(var)
  
  title <- var_overview[var_overview$var_id == question, ]$var_short
  
  plot <- ggplot(props, aes(x = X64, y = perc, fill = val)) + 
    geom_area(alpha = 0.8, size = 0.5, colour = "black") +
    labs(x = "Survey Year", y = "Proportions", fill = "Responses", title = title) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_fill_manual(values = c("NA" = "grey50", "don't know" = "grey30",
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



plot_frequency_area <- function(df, question, legend = TRUE,
                                filter_dks = TRUE) {
  
  if (filter_dks) {
    df <- df %>% 
      filter(.data[[question]] != "don't know")
  }
  
  props <- table_answers(df, question, group = "X64") %>%
    mutate(val = fct_relevel(val, "don't know", "never",
                             "rarely", "sometimes",
                             "frequently", "very often")) %>% 
    group_by(var)
  
  title <- var_overview[var_overview$var_id == question, ]$var_short
  
  plot <- ggplot(props, aes(x = X64, y = perc, fill = val)) + 
    geom_area(alpha = 0.8, size = 0.7, colour = "black") +
    labs(x = "Survey Year", y = "Proportions", fill = "Responses", title = title) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_fill_manual(values = c("don't know" = "grey30",
                                 "never" = "#b31529",
                                 "rarely" = "#f6a582",
                                 "sometimes" = "#e0e0e0",
                                 "frequently" = "#8ec4ca",
                                 "very often" = "#1065ab")) +
    geom_segment(aes(x = 2021, y = 0, xend = 2023, yend = 0)) +
    geom_segment(aes(x = 2021, y = 0, xend = 2021, yend = 100)) +
    geom_segment(aes(x = 2023, y = 0, xend = 2023, yend = 100)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"),
          panel.background = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.ticks = element_blank())
  
  if (!legend) {
    plot <- plot +
      theme(legend.position = "none")
  }
  
  plot
  
}

plot_time <- function(df, var_overview, questions, legend = TRUE, ncol = 3,
                      type = c("agreement", "frequency"), var_wrap = 35) {
  
  type <- match.arg(type)
  
  color_scale <- switch(type,
    agreement = list(
      scale_color_manual(
        values = c("NA" = "grey50", 
                   "don't know" = "grey30",
                   "strongly disagree" = "#b31529",
                   "rather disagree" = "#f6a582",
                   "rather agree" = "#8ec4ca",
                   "strongly agree" = "#1065ab"))
      ),
    frequency = list(
      scale_color_manual(
        values = c("don't know" = "grey30",
                   "never" = "#b31529",
                   "rarely" = "#f6a582",
                   "sometimes" = "#cdcdc8",
                   "frequently" = "#8ec4ca",
                   "very often" = "#1065ab"))
    )
  )
  
  prop_df <- df %>% 
    select(all_of(questions), year = X64) %>% 
    pivot_longer(cols = starts_with("X"), names_to = "var") %>% 
    filter(!(value %in% c("don't know", "NA"))) %>% 
    count(year, var, value) %>% 
    group_by(year, var) %>% 
    mutate(perc = n/sum(n))
  
  # add full variable text
  titles <- var_overview %>% 
    mutate(var_label = str_wrap(var_full, width = var_wrap)) %>% 
    select(var_id, var_label)
  
  with_titles <- prop_df %>% 
    left_join(titles, by = join_by(var == var_id))
  
  # sort by agreement
  with_sorting <- with_titles %>% 
    mutate(cumulative_agreement = cumsum(perc), 
           agreement_sort = case_when(
             value == "rather agree" ~ cumulative_agreement,
             value == "frequently" ~ cumulative_agreement,
             TRUE ~ 0
           )) 
  
  
  p <- ggplot(with_sorting, aes(x = year, y = perc, color = value)) +
    geom_line(linewidth = 0.8) +
    geom_point() +
    facet_wrap(vars(var_label %>% 
                      fct_reorder(agreement_sort, .fun = max) %>% 
                      fct_rev()), 
               ncol = ncol) +
    labs(x = "Survey Year", y = "Proportions", colour = NULL) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_y_continuous(labels = scales::label_percent()) +
    color_scale +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"),
          panel.spacing.x = unit(1, "lines"),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "top")
  
  if (!legend) {
    p <- p +
      theme(legend.position = "none")
  }
  
  p
}


# Dichotomize agreement response into agree/disagree
dichotomize_agreement <- function(df, question) {
  
  var <- sym(question)
  
  dichotomized <- df %>% 
    # Dichotomize agreement
    mutate({{ var }} := case_when(
      str_detect({{ var }}, "\\sagree$") ~ "agree",
      str_detect({{ var }}, "\\sdisagree$") ~ "disagree",
      is.na({{ var }}) ~ "NA",
      TRUE ~ {{ var }}))
  
  dichotomized
}


# Plot dichotomized agreement over time
plot_agreement_groups <- function(df, question, group,
                                  filter_dks = TRUE, filter_nas = TRUE,
                                  legend_reverse = TRUE) {
  
  df <- df %>% 
    dichotomize_agreement(., question)
  
  if (filter_dks) {
    df <- df %>% 
      filter(.data[[question]] != "don't know")
  }
  
  if (filter_nas) {
    df <- df %>% 
      filter(.data[[question]] != "NA")
  }
  
  groups <- table_answers_year(df, question, group) %>% 
    filter(val == "agree")
  
  title <- var_overview[var_overview$var_id == question, ]$var_short
  
  ggplot(groups, aes(x = X64, y = perc, color = .data[[group]])) + 
    geom_line(size = 0.8, alpha = 0.8) +
    geom_point() +
    labs(x = "Survey Year", y = "% respondents agreeing",
         color = NULL, title = title) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(25, 100)) +
    scale_color_brewer(palette = "Dark2") +
    guides(color = guide_legend(reverse = legend_reverse)) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"),
          panel.background = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.ticks = element_blank())
  
}


# Run logistic regressions over multiple agreement items
agreement_log_regression <- function(df, questions) {
  
  results <- data.frame()
  
  for (question in questions) {
    # Binary agreement variable
    item_data <- df %>% 
      select(c(.data[[question]], X64)) %>% 
      filter(.data[[question]] != "don't know") %>% 
      filter(.data[[question]] != "NA") %>% 
      mutate(agreement = case_when(
        str_detect(.data[[question]], "\\sagree$") ~ 1,
        TRUE ~ 0)) %>% 
      rename(., year = X64)
    
    # Percentage of agreement by year
    perc <- item_data %>% 
      group_by(year) %>% 
      summarise(perc = sum(agreement)/n()) %>% 
      pivot_wider(names_from = year, values_from = perc, names_prefix = "perc_")
    
    # Logistic model
    log_model <- glm(agreement ~ factor(year),
                     item_data,
                     family = "binomial")
    
    log_tidy <- tidy(log_model) %>% 
      pivot_wider(names_from = term,
                  values_from = c(estimate, std.error, statistic, p.value))
    
    
    # McFadden's R squared
    r_squared <- with(summary(log_model), 1 - deviance/null.deviance)
    
    # Combining values into dataframe row
    row <- cbind(question, perc, log_tidy, r_squared)
    
    results <- rbind(results, row)
    
  }
  
  results <- results %>%
    rename_with(~ gsub("factor(year)", "", .x, fixed = TRUE))
  
  results
  
}




# Dichotomize frequency response into agree/disagree
dichotomize_frequency <- function(df, question) {
  
  var <- sym(question)
  
  dichotomized <- df %>% 
    # Dichotomize agreement
    mutate({{ var }} := case_when(
      {{ var }} == "very often" ~ "often",
      {{ var }} == "frequently" ~ "often",
      {{ var }} == "sometimes" ~ "not often",
      {{ var }} == "rarely" ~ "not often",
      {{ var }} == "never" ~ "not often",
      is.na({{ var }}) ~ "NA",
      TRUE ~ {{ var }}))
  
  dichotomized
}


# Plot dichotomized frequency over time
plot_frequency_groups <- function(df, question, group,
                                  filter_dks = TRUE,
                                  legend_reverse = TRUE) {
  
  df <- df %>% 
    dichotomize_frequency(., question)
  
  if (filter_dks) {
    df <- df %>% 
      filter(.data[[question]] != "don't know")
  }
  
  
  groups <- table_answers_year(df, question, group) %>% 
    filter(val == "often")
  
  title <- var_overview[var_overview$var_id == question, ]$var_short
  
  ggplot(groups, aes(x = X64, y = perc, color = .data[[group]])) + 
    geom_line(size = 0.8, alpha = 0.8) +
    geom_point() +
    labs(x = "Survey Year", y = "% respondents estimating as frequent",
         color = NULL, title = title) +
    scale_x_continuous(breaks = c(2021, 2022, 2023)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 60)) +
    scale_color_brewer(palette = "Dark2") +
    guides(color = guide_legend(reverse = legend_reverse)) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"),
          panel.background = element_blank(),
          plot.title = element_text(size = 10, hjust = 0.5),
          axis.ticks = element_blank())
  
}


# Run logistic regressions over multiple frequency items
frequency_log_regression <- function(df, questions) {
  
  results <- data.frame()
  
  for (question in questions) {
    # Binary frequency variable
    item_data <- df %>% 
      select(c(.data[[question]], X64)) %>% 
      filter(.data[[question]] != "don't know") %>% 
      mutate(frequency = dichotomize_frequency(., question),
             frequency = case_when(
               frequency == "often" ~ 1,
               TRUE ~ 0)) %>% 
      rename(., year = X64)
    
    # Percentage of frequency by year
    perc <- item_data %>% 
      group_by(year) %>% 
      summarise(perc = sum(frequency)/n()) %>% 
      pivot_wider(names_from = year, values_from = perc, names_prefix = "perc_")
    
    # Logistic model
    log_model <- glm(frequency ~ factor(year),
                     item_data,
                     family = "binomial")
    
    log_tidy <- tidy(log_model) %>% 
      pivot_wider(names_from = term,
                  values_from = c(estimate, std.error, statistic, p.value))
    
    
    # McFadden's R squared
    r_squared <- with(summary(log_model), 1 - deviance/null.deviance)
    
    # Combining values into dataframe row
    row <- cbind(question, perc, log_tidy, r_squared)
    
    results <- rbind(results, row)
  }
  
  results <- results %>%
    rename_with(~ gsub("factor(year)", "", .x, fixed = TRUE))
  
  results
  
}



# Run logistic regressions over multiple agreement items
# Staff as additional predictor
agreement_log_regression_staff <- function(df, questions) {
  
  results <- data.frame()
  
  df <- df %>% 
    mutate(staff = case_when(
      str_detect(X8, "student") ~ 0,
      TRUE ~ 1))
  
  for (question in questions) {
    # Binary agreement variable
    item_data <- df %>% 
      select(c(.data[[question]], X64, staff)) %>% 
      filter(.data[[question]] != "don't know") %>% 
      filter(.data[[question]] != "NA") %>% 
      mutate(agreement = case_when(
        str_detect(.data[[question]], "\\sagree$") ~ 1,
        TRUE ~ 0)) %>% 
      rename(., year = X64)
    
    # Percentage of agreement by year
    perc_year <- item_data %>% 
      group_by(year) %>% 
      summarise(perc_year = sum(agreement)/n()) %>% 
      pivot_wider(names_from = year, values_from = perc_year,
                  names_prefix = "perc_")
    
    perc_staff <- item_data %>% 
      group_by(staff) %>% 
      summarise(perc_staff = sum(agreement)/n()) %>% 
      pivot_wider(names_from = staff, values_from = perc_staff,
                  names_prefix = "perc_staff")
    
    # Logistic model
    log_model <- glm(agreement ~ factor(year) + staff,
                     item_data,
                     family = "binomial")
    
    log_tidy <- tidy(log_model) %>% 
      pivot_wider(names_from = term,
                  values_from = c(estimate, std.error, statistic, p.value))
    
    
    # McFadden's R squared
    r_squared <- with(summary(log_model), 1 - deviance/null.deviance)
    
    # Combining values into dataframe row
    row <- cbind(question, perc_year, perc_staff, log_tidy, r_squared)
    
    results <- rbind(results, row)
    
  }
  
  results <- results %>%
    rename_with(~ gsub("factor(year)", "", .x, fixed = TRUE))
  
  results
  
}



# Run logistic regressions over multiple frequency items
# Staff as additional predictor
frequency_log_regression_staff <- function(df, questions) {
  
  results <- data.frame()
  
  df <- df %>% 
    mutate(staff = case_when(
      str_detect(X8, "student") ~ 0,
      TRUE ~ 1))
  
  for (question in questions) {
    # Binary frequency variable
    item_data <- df %>% 
      select(c(.data[[question]], X64, staff)) %>% 
      filter(.data[[question]] != "don't know") %>% 
      mutate(frequency = dichotomize_frequency(., question),
             frequency = case_when(
               frequency == "often" ~ 1,
               TRUE ~ 0)) %>% 
      rename(., year = X64)
    
    # Percentage of frequency by year
    perc_year <- item_data %>% 
      group_by(year) %>% 
      summarise(perc_year = sum(frequency)/n()) %>% 
      pivot_wider(names_from = year, values_from = perc_year,
                  names_prefix = "perc_")
    
    perc_staff <- item_data %>% 
      group_by(staff) %>% 
      summarise(perc_staff = sum(frequency)/n()) %>% 
      pivot_wider(names_from = staff, values_from = perc_staff,
                  names_prefix = "perc_staff")
    
    # Logistic model
    log_model <- glm(frequency ~ factor(year) + staff,
                     item_data,
                     family = "binomial")
    
    log_tidy <- tidy(log_model) %>% 
      pivot_wider(names_from = term,
                  values_from = c(estimate, std.error, statistic, p.value))
    
    
    # McFadden's R squared
    r_squared <- with(summary(log_model), 1 - deviance/null.deviance)
    
    # Combining values into dataframe row
    row <- cbind(question, perc_year, perc_staff, log_tidy, r_squared)
    
    results <- rbind(results, row)
  }
  
  results <- results %>%
    rename_with(~ gsub("factor(year)", "", .x, fixed = TRUE))
  
  results
  
}



# Function to calculate percentage of agreement after dichotomization
percentage_agree <- function(column) {
  
  dks_excluded <- column[!is.na(column) & column != "don't know"]
  
  percent <- round(sum(dks_excluded == "agree") / length(dks_excluded) * 100, 2)
  
  percent
}

# Function to calculate percentage of frequency after dichotomization
percentage_often <- function(column) {
  
  dks_excluded <- column[column != "don't know"]
  
  percent <- round(sum(dks_excluded == "often") / length(dks_excluded) * 100, 2)
  
  percent
}


plot_groups_overview_agreement  <- function(df, var_overview, columns, group,
                                            legend_title, legend_position) {
  
  df <- df %>%
    group_by(.data[[group]]) %>%
    summarise(across(all_of(columns), percentage_agree), .groups = 'drop') %>%
    filter(!is.na(.data[[group]]) & .data[[group]] != "don't know") %>%
    pivot_longer(cols = -.data[[group]], names_to = "Question", values_to = "Percentage") %>%
    merge(var_overview, by.x = "Question", by.y = "var_id")
  
  # Create the plot
  ggplot(df, aes(x = var_short, y = Percentage, color = .data[[group]])) +
    geom_point(size = 3) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "Statement",
         y = "Percentage of Agreement",
         color = legend_title) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position = legend_position)
}


plot_groups_overview_frequency  <- function(df, var_overview, columns, group,
                                            legend_title, legend_position) {
  
  df <- df %>%
    group_by(.data[[group]]) %>%
    summarise(across(all_of(columns), percentage_often), .groups = 'drop') %>%
    filter(!is.na(.data[[group]]) & .data[[group]] != "don't know") %>%
    pivot_longer(cols = -.data[[group]], names_to = "Question", values_to = "Percentage") %>%
    merge(var_overview, by.x = "Question", by.y = "var_id")
  
  # Create the plot
  ggplot(df, aes(x = var_short, y = Percentage, color = .data[[group]])) +
    geom_point(size = 3) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "Statement",
         y = "Percentage of High Frequency",
         color = legend_title) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    theme(panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey90"),
          panel.background = element_blank(),
          axis.text.x = element_text(angle = 60, hjust = 1),
          legend.position = legend_position)
}