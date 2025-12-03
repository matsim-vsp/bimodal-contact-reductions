library(tidyverse)

my_theme <- function(base_size = 30,
                        base_family = "Helvetica",
                                 panel_width = 6, 
                                 panel_height = 4,
                                 base_line_size = 0.5) {
  
  # Calculate relative sizes based on panel dimensions
  relative_size <- sqrt(panel_width * panel_height) / sqrt(6 * 4)  # Using 6x4 as reference
  adjusted_line_size <- base_line_size * relative_size

   theme_minimal(
    base_size = base_size, 
    base_family = base_family,
    base_line_size = base_line_size
  ) %+replace%
  
  theme(
    # Text elements
    text = element_text(size = base_size),
    title = element_text(size = base_size * 1.2),
    axis.title = element_text(size = base_size * 1.1),
    axis.text = element_text(size = base_size),
    legend.text = element_text(size = base_size),
    #legend.title = element_text(size = base_size * 1.1),
    legend.position = "none", 
    legend.title = element_blank(),
    strip.text = element_text(size = base_size),
    plot.caption = element_text(size = base_size * 0.8),
    axis.ticks.x = element_line(size = 1), 
    axis.ticks.y = element_line(size = 1),
    axis.ticks.length = unit(15, "pt"),
    
    # Panel and line elements
    panel.grid.major = element_line(size = adjusted_line_size * 0.5),
    panel.grid.minor = element_line(size = adjusted_line_size * 0.25),
    #axis.line = element_line(size = adjusted_line_size),
    #axis.ticks = element_line(size = adjusted_line_size),

    # Panel dimensions
    plot.margin = margin(t=0.5, l=1, r=1, b=0.25, unit = "cm"),
    panel.spacing = unit(0.5, "cm")
  )
}
