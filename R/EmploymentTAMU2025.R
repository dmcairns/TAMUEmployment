#### Graphing Functions ----

employmentRankGrid <- function(theData, startSemester, endSemester, colorScheme,
                               showLegend=FALSE){
  require(gt)
  req(startSemester)
  req(endSemester)

  # Function that creates the employment grid using the DT structure
  if(is.reactive(startSemester)){
    localStartSemester <- startSemester()
  } else {
    localStartSemester <- startSemester
  }
  if(is.reactive(endSemester)){
    localEndSemester <- endSemester()
  } else {
    localEndSemester <- endSemester
  }

  displayData <- theData %>%
    select(-any_of(c("row_names", "recnum"))) %>%
    filter(shortSemester >= startSemester) %>%
    filter(shortSemester <= endSemester) %>%
    arrange(numericSemester) %>%
    select("Faculty", "sem2", "rank") %>%
    pivot_wider(id_cols=c("Faculty"), names_from="sem2", values_from=rank) %>%
    arrange(Faculty, .locale="en")

  displayGrid <- displayData %>%
    gt() %>%
    data_color(columns = -one_of("Faculty"),
               palette = colorScheme$color,
               domain = colorScheme$value,
               apply_to = c("text"),
               autocolor_text = FALSE,
               na_color = "#FFFFFF00") %>%
    data_color(columns = -one_of("Faculty"),
               palette = colorScheme$color,
               domain = colorScheme$value,
               autocolor_text = FALSE,
               apply_to = c("fill"),
               na_color = "#FFFFFF00") %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom", "left", "right"),
        color = "black",
        weight = px(1.5),
        style = "solid"
      ),
      locations = cells_body()
    )


  return(displayGrid)

}

departmentStaffingChart <- function(theData, startSemester, endSemester, showFacultyNames){
  require(c3)
  req(startSemester)
  req(endSemester)

  theChart <- theData %>%
    filter(shortSemester <= endSemester) %>%
    filter(shortSemester >= startSemester) %>%
    group_by(shortSemester, sem2) %>%
    arrange(shortSemester) %>%
    summarize(n=n()) %>%
    c3(x='sem2') %>%
    c3_line('area-step') %>%
    legend(hide=TRUE)

  return(theChart)
}

employmentRankLegend <- function(theData){
  # 1. Prepare the legend data (removing 'Unknown' and duplicates)
  legend_df <- theData %>%
    filter(title != "Unknown") %>%
    distinct(title, .keep_all = TRUE) %>%
    select(title, color) %>%
    pivot_wider(names_from = title, values_from = color)

  # 2. Start the gt object
  legend_gt <- legend_df %>%
    gt() %>%
    tab_header(title = md("**Legend**"))

  # 3. Use a loop to apply colors to each column automatically
  for (col_name in names(legend_df)) {
    current_color <- legend_df[[col_name]]

    legend_gt <- legend_gt %>%
      tab_style(
        style = list(
          cell_fill(color = current_color),
          cell_text(color = ifelse(
            # Basic logic to switch text color for readability on dark backgrounds
            current_color %in% c("#500000", "#383c74", "#2c4709"),
            "white", "black"),
            size = "small", weight = "bold")
        ),
        locations = cells_body(columns = all_of(col_name))
      )
  }

  # 4. Final Polish
  legend_gt <- legend_gt %>%
    cols_align(align = "center") %>%
    tab_options(
      table.width = pct(100),
      column_labels.hidden = FALSE, # Hide labels so the cell itself is the legend
      table.border.top.style = "none"
    )
  legend_gt
}

#### Color Schemes ----
employmentGridColorScheme <- data.frame(value=c(1:14),
                                        title=c("Assistant Professor",
                                                "Associate Professor",
                                                "Professor",
                                                "Visiting Assistant Professor",
                                                "Unknown",
                                                "Research Assistant Professor",
                                                "Instructional Assistant Professor",
                                                "Instructional Associate Professor",
                                                "Instructional Professor",
                                                "Unknown",
                                                "Associate Professor of the Practice",
                                                "Unknown",
                                                "Visiting Lecturer",
                                                "GAL"
                                        ),
                                        color=c(
                                          "#cbb3b3",
                                          "#854d4d",
                                          "#500000",
                                          "#2c4709",
                                          "#FFFFFF",
                                          "#f5b0c7",
                                          "#a8abd3",
                                          "#6267af",
                                          "#383c74",
                                          "#FFFFFF",
                                          "#ebeb52",
                                          "#FFFFFF",
                                          "#a2f2ed",
                                          "#d2a2f2"
                                        )
)


