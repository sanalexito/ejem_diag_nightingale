library(dplyr)
library(ggplot2)
library(stringr)

hike_data <- openxlsx::read.xlsx("~/datos.xlsx")

plot_df <- hike_data %>%
  group_by(region) %>%
  summarise(
    sum_length = sum(length_num),
    mean_gain = mean(as.numeric(gain)),
    n = n()
  ) %>%
  mutate(mean_gain = round(mean_gain, digits = 0))

plt <- ggplot(plot_df) +
  # Make custom panel grid
  geom_hline(
    aes(yintercept = y), 
    data.frame(y = c(0:3) * 1000),
    color = "lightgrey"
  ) + 
  # Add bars to represent the cumulative track lengths
  # str_wrap(region, 5) wraps the text so each line has at most 5 characters
  # (but it doesn't break long words!)
  geom_col(
    aes(
      x = reorder(str_wrap(region, 5), sum_length),
      y = sum_length,
      fill = n
    ),
    position = "dodge2",
    show.legend = TRUE,
    alpha = .9
  ) +
  
  # Add dots to represent the mean gain
  geom_point(
    aes(
      x = reorder(str_wrap(region, 5),sum_length),
      y = mean_gain
   ),
    size = 3,
   color = "gray12"
   ) +
  
  # Lollipop shaft for mean gain per region
  geom_segment(
    aes(
      x = reorder(str_wrap(region, 5), sum_length),
      y = 0,
      xend = reorder(str_wrap(region, 5), sum_length),
      yend = 3000
    ),
    linetype = "dashed",
    color = "gray12"
  ) + 
  # Make it circular!
  coord_polar()

plt
