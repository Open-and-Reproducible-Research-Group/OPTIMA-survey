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
    filter(val == "don't know" | is.na(val))
  
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
    scale_fill_manual(values = c("steelblue", "olivedrab4", "goldenrod2", "indianred")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata_item, var, order)) %>% 
    left_join(labels, by = c("var" = "var_id")) %>% 
    distinct() %>% 
    group_by(label, order)
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(label, order), x = total_perc)) +
    geom_col(fill = "grey50", width = .7) +
    labs(x = NULL, y = "Proportion 'don't know' or NA") +
    scale_x_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
    scale_y_discrete(position = "right") +
    theme(panel.border = element_rect(fill = NA, colour = "grey80"),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank())
  
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
    filter(val == "don't know" | is.na(val))
  
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
    scale_fill_manual(values = c("steelblue", "olivedrab4", "lightgoldenrod",
                                 "tan2", "indianred")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata_item, var, order)) %>% 
    left_join(labels, by = c("var" = "var_id")) %>% 
    distinct() %>% 
    group_by(label, order)
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(label, order), x = total_perc)) +
    geom_col(fill = "grey50", width = .7) +
    labs(x = NULL, y = "Proportion 'don't know'") +
    scale_y_discrete(position = "right") +
    scale_x_continuous(labels = function(x) paste0(round(x * 100, 0), "%")) +
    theme(panel.border = element_rect(fill = NA, colour = "grey80"),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank())
  
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
    filter(val == "don't know" | is.na(val))
  
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
    scale_fill_manual(values = c("steelblue", "olivedrab4",
                                 "goldenrod2", "indianred")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata, var, order)) %>% 
    distinct() %>% 
    group_by(.data[[group]], order) %>% 
    summarise(perc = sum(perc))
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(as.factor(.data[[group]]), order),
               x = perc)) +
    geom_col(fill = "grey50", width = .7) +
    labs(x = NULL, y = "Proportion 'don't know' or NA") +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_discrete(position = "right") +
    theme(panel.border = element_rect(fill = NA, colour = "grey80"),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank())
  
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
    filter(val == "don't know" | is.na(val))
  
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
    scale_fill_manual(values = c("steelblue", "olivedrab4", "lightgoldenrod",
                                 "tan2", "indianred")) +
    theme(legend.position = "top",
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank()) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    labs(x = NULL, y = NULL, fill = NULL, title = str_to_title(filter_val))
  
  # Plot "don't know" and missing answers
  p_nas <- nas %>% 
    left_join(select(pdata, var, order)) %>% 
    distinct() %>% 
    group_by(.data[[group]], order) %>% 
    summarise(perc = sum(perc))
  
  p2 <- p_nas %>% 
    ggplot(aes(y = fct_reorder(as.factor(.data[[group]]), order),
               x = perc)) +
    geom_col(fill = "grey50", width = .7) +
    labs(x = NULL, y = "Proportion 'don't know'") +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_discrete(position = "right") +
    theme(panel.border = element_rect(fill = NA, colour = "grey80"),
          axis.text.y = element_blank(),
          axis.title.y = element_text( angle = 270),
          panel.background = element_rect(fill = "white"),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_blank())
  
  final_plot <- p1 + 
    theme(plot.margin = margin()) + p2 +
    plot_layout(widths = c(6, 1))
  
  final_plot
  
}