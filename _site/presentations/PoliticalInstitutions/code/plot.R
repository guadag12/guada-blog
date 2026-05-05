library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)

col_red   <- "#C0392B"
col_grey  <- "#BDBDBD"
col_dark  <- "#2C3E50"
col_text  <- "#7F8C8D"
col_bg    <- "white"

theme_clean <- theme_minimal(base_size = 13) +
  theme(
    plot.background   = element_rect(fill = col_bg, color = NA),
    panel.background  = element_rect(fill = col_bg, color = NA),
    panel.grid.minor  = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "#EEEEEE"),
    axis.title.x      = element_text(size = 11, color = col_dark, margin = margin(t = 8)),
    axis.title.y      = element_blank(),
    axis.text         = element_text(size = 11, color = col_dark),
    plot.title        = element_text(size = 13, face = "bold", color = col_dark, margin = margin(b = 4)),
    plot.subtitle     = element_text(size = 10, color = col_text, margin = margin(b = 10)),
    legend.position   = "none"
  )

# ── B. Lollipop: Gun Ownership ───────────────────────────────────────────────
b_df <- tibble(
  label     = c("Non-owners", "Gun owners"),
  n         = c(59013, 4870),
  pct       = c(92, 8),
  highlight = c(FALSE, TRUE),
  ann       = c("59,013  (92%)", "4,870  (8%)")
) |> mutate(label = factor(label, levels = c("Non-owners", "Gun owners")))

pB <- ggplot(b_df, aes(y = label, x = n, color = highlight)) +
  geom_segment(aes(x = 0, xend = n, y = label, yend = label),
               linewidth = 1.2, color = col_grey) +
  geom_point(size = 9) +
  geom_text(aes(label = ann), hjust = -0.12, size = 3.6,
            fontface = "bold", color = col_dark) +
  scale_color_manual(values = c("FALSE" = col_grey, "TRUE" = col_red)) +
  scale_x_continuous(labels = label_comma(),
                     limits = c(0, 85000), expand = c(0, 0)) +
  labs(title    = "B. Gun Ownership in the Sample",
       subtitle = "Gun owners are a small minority — but do they punch above their weight?",
       x = "N") +
  theme_clean

# ── C. Horizontal bars: Voted only, by ownership ─────────────────────────────
c_df <- tibble(
  group     = c("Non-owners", "Gun owners"),
  pct       = c(80, 90),
  label     = c("80%  (47,217 voted)", "90%  (4,395 voted)"),
  highlight = c(FALSE, TRUE)
) |> mutate(group = factor(group, levels = c("Non-owners", "Gun owners")))

pC <- ggplot(c_df, aes(y = group, x = pct, fill = highlight)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = label), hjust = -0.07,
            size = 3.7, fontface = "bold", color = col_dark) +
  # bracket showing the gap
  annotate("segment", x = 80, xend = 80, y = 1.28, yend = 1.72,
           linewidth = 0.9, color = col_red) +
  annotate("segment", x = 80, xend = 90, y = 1.5, yend = 1.5,
           linewidth = 0.9, color = col_red,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed")) +
  annotate("text", x = 85, y = 1.5,
           label = "+10 pp", vjust = -0.6,
           size = 3.8, color = col_red, fontface = "bold") +
  scale_fill_manual(values = c("FALSE" = col_grey, "TRUE" = col_red)) +
  scale_x_continuous(labels = label_percent(scale = 1),
                     limits = c(0, 130), breaks = c(0, 25, 50, 75, 100),
                     expand = c(0, 0)) +
  labs(title    = "C. Share Who Voted, by Gun Ownership",
       subtitle = "Gun owners turn out at a higher rate",
       x = "% who voted") +
  theme_clean

# ── Combine ───────────────────────────────────────────────────────────────────
fig <- pB + pC +
  plot_layout(ncol = 2) +
  plot_annotation(
    caption = "Source: National Electoral Court of Argentina and ANMAC",
    theme = theme(
      plot.caption = element_text(size = 9, color = col_text,
                                  hjust = 1, margin = margin(t = 8))
    )
  )

ggsave("img/figure_BC_v2.png", fig,
       width = 12, height = 4.0, dpi = 200, bg = "white")

message("Saved: figure_BC_clean.png")