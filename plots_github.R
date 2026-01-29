library(sf)
library(tidyverse)
library(patchwork)

load("complete_borders.RData") 
load("final_fh.RData")
# Plotting the results of the comparison between direct estimates and the FH estimates for the mean of the Wealth Index by district

# Necessary data: complete_borders dataframe containing:
# - direct estimates and FH estimates of mean and CV  
# - the districts' geometries

# Direct Estimation Means by district
map_direct_means <- ggplot(data = complete_borders, aes(fill = Direct)) +
  geom_sf() + 
  labs(title = "Direkte Schätzung", fill = "Wealth Index") +
  scale_fill_viridis_c(
    limits = c(-2.1, 1.5), 
    breaks = c(-2, -1, 0, 1, 2),
    na.value = "white") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.key.size = unit(1.5, "cm"))

map_direct_means    

# FH estimates for the mean 
map_fh_means <- ggplot(data = complete_borders, aes(fill = FH)) +
  geom_sf() +
  labs(title = "Fay-Herriot", fill = "Wealth Index") +
  scale_fill_continuous(
    type = "viridis", 
    limits = c(-2.1, 1.5), 
    breaks = c(-2, -1, 0, 1, 2), 
    na.value = "white") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.key.size = unit(1.5, "cm"),
    )

map_fh_means

combined_means <- map_direct_means +
  map_fh_means + 
  plot_layout(guides = "collect") &
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )
combined_means
ggsave("maps_combined.pdf", combined_means, 
       width = 40, height = 20, units = "cm", 
       dpi = 300, bg = "transparent")

# CVs
# Direct Estimation CVs
## White areas: no data
## Red areas: CV >= 100%
## Green areas: CV < 25%
complete_borders$direct_reliable_cv <- complete_borders$Direct_CV * 100 < 25
map_direct_cv <- ggplot(data = complete_borders, aes(fill = Direct_CV * 100)) +
  geom_sf(
    data = complete_borders[complete_borders$direct_reliable_cv, ],
    fill = "#51cf5f") +
  geom_sf(data = complete_borders[!complete_borders$direct_reliable_cv, ],
          aes(fill = Direct_CV * 100)
          ) +
  scale_fill_gradient(
    na.value = "red",
    low = "yellow", 
    high = "red",
    limits = c(25, 100),
    labs(title = "CV (%)"),
    breaks = c(25, 50, 75, 100),
    labels = c("25", "50", "75", "100")
    ) +
  labs(title = "Direkte Schätzung") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.key.size = unit(1.5, "cm")
  )

map_direct_cv

# FH CVs
complete_borders$fh_reliable_cv <- complete_borders$FH_CV * 100 < 25
map_fh_cv <- ggplot(data = complete_borders, aes(fill = FH_CV * 100)) +
  geom_sf(
    data = complete_borders[complete_borders$fh_reliable_cv, ],
    fill = "#51cf5f") +
  geom_sf(data = complete_borders[!complete_borders$fh_reliable_cv, ],
          aes(fill = FH_CV * 100)
  ) +
  scale_fill_gradient(
    na.value = "red",
    low = "yellow", 
    high = "red",
    limits = c(25, 100),
    labs(title = "CV (%)"),
    breaks = c(25, 50, 75, 100),
    labels = c("25", "50", "75", "100")
  ) +
  labs(title = "Fay-Herriot") +
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.key.size = unit(1.5, "cm")
  )

map_fh_cv

combined_cvs <- map_direct_cv +
  map_fh_cv + 
  plot_layout(guides = "collect") &
  theme(
    plot.background = element_rect(fill = "transparent", colour = NA),
    panel.background = element_rect(fill = "transparent", colour = NA),
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.box.background = element_rect(fill = "transparent", colour = NA)
  )
combined_cvs
ggsave("maps_cvs.pdf", combined_cvs, 
       width = 40, height = 20, units = "cm", 
       dpi = 300, bg = "transparent")



## QQ-Plots (plot(final_fh))
## Necessary data: final_fh which contains the model summary
p <- plot(fh_complete, 
          label = "orig", 
          gg_theme = theme(
            panel.background = element_rect(fill = "transparent", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA),
            legend.background = element_rect(fill = "transparent", colour = NA),
            legend.box.background = element_rect(fill = "transparent", colour = NA),
            plot.title = element_text(face = "bold"),
            title = element_text(color = "black")) 
)
