
# CALC_MEAN_BILL function
calc_mean_bill <- function(island_name) {
  filtered_data <- subset(na.omit(data), species == "Adelie" & island == island_name)
  mean_bill_length <- mean(filtered_data$bill_length_mm)
  return(round(mean_bill_length, 2))
}


# CREATE_SCATTERPLOT function
create_scatterplot <- function(data, selected_species, selected_island) {
  # Filter the data for the specified species and island
  filtered_data <- data %>%
    na.omit() %>%
    filter(species == selected_species)#, island == selected_island)
  
  # Create the scatterplot
  plot <- ggplot(
    filtered_data,
    aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
  ) +
    geom_point() +
    labs(
      x = "Bill Length (mm)",
      y = "Bill Depth (mm)",
      title = selected_species #paste("Penguin Bill Dimensions -", selected_species, "-", selected_island)
    )
  
  return(plot)
}

# CREATE_DATAVIZ_THEME function using the GGPLOT2 THEME_CLASSIC() default theme (https://ggplot2.tidyverse.org/reference/ggtheme.html)
# and adapting it 
create_dataviz_theme <- function() {
  theme_classic() +
    theme(
      plot.title = element_text(color = "#009999", size = 18, face = "bold")#,
      #text       = element_text(family="Times New Roman") # This line causes a lot of warnings!
    )
}