library(ggplot2)
library(dplyr)

col_cog      <- "#2E6DA4"   # blue — cognitive
col_soc      <- "#27AE60"   # green — social
col_interface <- "#C0392B"  # red — interface

df <- tibble(
  author    = c(
    "Berkowitz & LePage (1967)",
    "Higgins et al. (1982)",
    "Kelley (1973)",
    "Pierce et al. (2003)",
    "Bateson (2012)",
    "Buttrick (2020)",
    "Lerner & Keltner (2001)",
    "Shapira & Simon (2018)",
    "Stroebe et al. (2017)",
    "Drakulich & Baranauskas (2021)",
    "Goldberg et al. (1999)",
    "Jost et al. (2003)",
    "Lacombe et al. (2024)"
  ),
  mechanism = c(2, 2, 3, 2, 3, 1, 3, 1, 1, 3, 3, 3, 3),
  section   = c(
    "I — Cognitive", "I — Cognitive", "I — Cognitive", "I — Cognitive",
    "II — Social", "II — Social", "II — Social", "II — Social", "II — Social",
    "III — Interface", "III — Interface", "III — Interface", "III — Interface"
  )
) |>
  mutate(
    # for authors covering two points, duplicate the row
    author = factor(author, levels = rev(unique(author)))
  )

# add the two authors that cover points 1 & 2
df_extra <- tibble(
  author    = factor(c("Buttrick (2020)", "Shapira & Simon (2018)"),
                     levels = levels(df$author)),
  mechanism = c(2, 2),
  section   = c("II — Social", "II — Social")
)

df_full <- bind_rows(df, df_extra)

mech_labels <- c(
  "1" = "1 — \"World is\ndangerous\"",
  "2" = "2 — Psychological\nOwnership",
  "3" = "3 — Anger,\nnot Fear"
)

ggplot(df_full, aes(x = factor(mechanism), y = author, color = section)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_line(aes(group = author), color = "#CCCCCC",
            linewidth = 0.6, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "I — Cognitive"   = col_cog,
      "II — Social"     = col_soc,
      "III — Interface" = col_interface
    ),
    name = "Bibliography Section"
  ) +
  scale_x_discrete(labels = mech_labels) +
  labs(
    title    = "Authors by Mechanism Point and Bibliography Section",
    subtitle = "Dashed lines connect authors contributing to multiple points",
    x        = "Mechanism Point",
    y        = NULL,
    caption  = "Each dot = one contribution to the mechanism"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.background   = element_rect(fill = "white", color = NA),
    panel.background  = element_rect(fill = "white", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "#EEEEEE"),
    panel.grid.minor  = element_blank(),
    axis.text.y       = element_text(size = 10, color = "#2C3E50"),
    axis.text.x       = element_text(size = 10, color = "#2C3E50",
                                     lineheight = 1.2),
    legend.position   = "bottom",
    legend.title      = element_text(size = 10, face = "bold"),
    plot.title        = element_text(size = 13, face = "bold",
                                     color = "#1C2B4A"),
    plot.subtitle     = element_text(size = 10, color = "#7F8C8D"),
    plot.caption      = element_text(size = 9,  color = "#7F8C8D", hjust = 0)
  )

ggsave("mechanism_dotplot.png", width = 10, height = 6,
       dpi = 200, bg = "white")
message("Saved: mechanism_dotplot.png")