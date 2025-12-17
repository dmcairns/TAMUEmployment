#### Graphing Functions ----

employmentRankGrid <- function(theData, startSemester, endSemester, colorScheme){
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
    arrange(Faculty)

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

#### Color Schemes ----
employmentGridColorScheme <- data.frame(value=c(1:14),
                                        title=c("Assistant Professor",
                                                "Associate Professor",
                                                "Professor",
                                                "Visiting Assistant Professor",
                                                "Unknown",
                                                "Unknown",
                                                "Instructional Assistant Professor",
                                                "Instructional Associate Professor",
                                                "Instructional Professor",
                                                "Unknown",
                                                "Unknown",
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
                                          "#FFFFFF",
                                          "#a8abd3",
                                          "#6267af",
                                          "#383c74",
                                          "#FFFFFF",
                                          "#FFFFFF",
                                          "#FFFFFF",
                                          "#a2f2ed",
                                          "#d2a2f2"
                                        )
)


