# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   https://r-pkgs.org
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

#cleans names of water quality and weather dataset
wq_clean_names <- c(
  Temp = "Water Temperature (C)",
  SpCond = "Specific Conductivity (mS/cm)",
  Sal = "Salinity (ppt)",
  DO_Pct = "Dissolved Oxygen (% Saturation)",
  DO_mgl = "Dissolved Oxygen (mg/L)",
  Depth = "Sonde Depth (m)",
  pH = "pH",
  ChlFluor = "Chlorophyll Fluorescence (ug/L)", # include foot note
  Turb = "Turbidity (NTU)",
  ATemp = "Average Air Temperature (C)",
  RH = "Average Relative Humidity (% Saturation)",
  BP = "Average Barometric Pressure (mb)",
  WSpd = "Average Wind Speed (m/s)",
  MaxWSpd = "Max Wind Speed (m/s)",
  MaxWSpdT = "Time of Max Wind Speed Measured (hh:mm)",
  Wdir = "Average Wind Direction", # include foot note
  SDWDir = "Wind Direction (sd)",
  TotPAR = "Total Photosynthetically Active Radiation",
  TotPrcp = "Total Precipitation (mm)",
  year = "Year",
  time = "Time",
  month = "Month",
  season = "Season",
  date = "Date")

# function to streamline scatter plots
wq_scatterplot <- function(df, var1, var2, plot_title = NULL) {

  # creating
  var1_name <- as_name(ensym(var1))
  var2_name <- as_name(ensym(var2))

# accessing the variable name from the wq_clean_names object and assigning it to the x and y labels
  x_label <- wq_clean_names[[var1_name]]
  y_label <- wq_clean_names[[var2_name]]

  df |>
    ggplot() +
    geom_point(aes(x = {{var1}}, y = {{var2}}), alpha = 0.5, color = "steelblue") +
    geom_smooth(aes(x = {{var1}}, y = {{var2}}), method = "lm", color = "red") +
    labs(
      title = {{plot_title}},
      x = x_label,
      y = y_label) +
    theme_minimal()
}

# function to streamline box plots
wq_boxplot <- function(df, var1, var2) {

  # creating
  var1_name <- as_name(ensym(var1))
  var2_name <- as_name(ensym(var2))

  label  <- englue("{var1_name} {var2_name}")

  # accessing the variable name from the wq_clean_names object and assigning it to the x and y labels
  x_label <- wq_clean_names[[var1_name]]
  y_label <- wq_clean_names[[var2_name]]

  df |>
    ggplot() +
    geom_boxplot(aes(x = {{var1}}, y = {{var2}})) +
    labs(
      title = label,
      x = x_label,
      y = y_label) +
    theme_minimal()
}

# function to streamline line graphs. Ensure the way is a valid time variable
wq_line_graph <- function(df, var1, var2) {

  # creating
  var1_name <- as_name(ensym(var1))
  var2_name <- as_name(ensym(var2))

  label  <- englue("{var1_name} {var2_name}")

  # accessing the variable name from the wq_clean_names object and assigning it to the x and y labels
  x_label <- wq_clean_names[[var1_name]]
  y_label <- wq_clean_names[[var2_name]]

  df |>
    ggplot() +
    geom_line(aes(x = {{var1}}, y = {{var2}})) +
    labs(
      title = label,
      x = x_label,
      y = y_label) +
    theme_minimal()
}


