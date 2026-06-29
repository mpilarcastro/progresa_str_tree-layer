# Figure poster Neobiota

# 1. Cargamos la librería necesaria
library(ggplot2)

#----------------------------------
# Gráfico 1 Tree density
#----------------------------------

p1<-ggplot(veg_summary, aes(x = veg_ty, y = mean_density, fill = veg_ty)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_density - se_density, ymax = mean_density + se_density),
    width = 0.2
  ) +
  
  # LETRAS AUTOMÁTICAS CON GEOM_TEXT
  geom_text(
    aes(
      # Le sumamos un margen al error estándar (ajusta el + 15 según tu escala)
      y = mean_density + se_density + 15, 
      label = c("a", "a", "b", "ab")
    ),
    size = 6,
    fontface = "bold",
    vjust = 0 # Alineación vertical para que empuje la letra hacia arriba desde ese punto
  ) +
  
  labs(
    title = "Tree density",
    x = NULL, 
    y = expression(paste("Tree density (ha"^-1, ")"))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "black", face = "bold")
  )

#----------------------------------
# Gráfico 2 Tree diversity
#----------------------------------

p2<-ggplot(veg_summary, aes(x = veg_ty, y = mean_shannon, fill = veg_ty)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_shannon - se_shannon, ymax = mean_shannon + se_shannon),
    width = 0.2
  ) +
  
  # LETRAS AUTOMÁTICAS CON GEOM_TEXT
  geom_text(
    aes(
      # Le sumamos un margen al error estándar (ajusta el + 15 según tu escala)
      y = mean_shannon + se_shannon + 0.03, 
      label = c("a", "a", "b", "a")
    ),
    size = 6,
    fontface = "bold",
    vjust = 0 # Alineación vertical para que empuje la letra hacia arriba desde ese punto
  ) +
  
  labs(
    title = "Tree diversity",
    x = NULL, 
    y = expression(paste("Tree diversity (Shannon)"))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "black", face = "bold")
  )

#----------------------------------
# Gráfico 3 Tree health
#----------------------------------

p3<-ggplot(veg_summary, aes(x = veg_ty, y = health_plot_media, fill = veg_ty)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = health_plot_media - health_plot_se, ymax = health_plot_media + health_plot_se),
    width = 0.2
  ) +
  
  # LETRAS AUTOMÁTICAS CON GEOM_TEXT
  geom_text(
    aes(
      # Le sumamos un margen al error estándar (ajusta el + 15 según tu escala)
      y = health_plot_media + health_plot_se + 0.1, 
      label = c("b", "ab", "a", "b")
    ),
    size = 6,
    fontface = "bold",
    vjust = 0 # Alineación vertical para que empuje la letra hacia arriba desde ese punto
  ) +
  
  labs(
    title = "Tree health index",
    x = NULL, 
    y = expression(paste("Tree health index"))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "black", face = "bold")
)
#----------------------------------
# Cargo datos de arbustos
#----------------------------------

veg_summary2 <- read.csv("str_shrub_results.csv", sep=";", header = T)

#----------------------------------
# Gráfico 4 Shrub cover
#----------------------------------

p4<-ggplot(veg_summary2, aes(x = forty, y = cover, fill = forty)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = cover - se_cover, ymax = cover + se_cover),
    width = 0.2
  ) +
  
  # LETRAS AUTOMÁTICAS CON GEOM_TEXT
  geom_text(
    aes(
      # Le sumamos un margen al error estándar (ajusta el + 15 según tu escala)
      y = cover + se_cover + 0.05, 
      label = c("a", "ab", "b", "a")
    ),
    size = 6,
    fontface = "bold",
    vjust = 0 # Alineación vertical para que empuje la letra hacia arriba desde ese punto
  ) +
  
  labs(
    title = "Shrub cover",
    x = NULL, 
    y = expression(paste("Shrub cover (%)"))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "black", face = "bold")
  )

#----------------------------------
# Gráfico 5 Shrub height
#----------------------------------

p5<-ggplot(veg_summary2, aes(x = forty, y = height, fill = forty)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = height - se_height, ymax = height + se_height),
    width = 0.2
  ) +
  
  # LETRAS AUTOMÁTICAS CON GEOM_TEXT
  geom_text(
    aes(
      # Le sumamos un margen al error estándar (ajusta el + 15 según tu escala)
      y = height + se_height + 0.05, 
      label = c("ab", "ab", "b", "a")
    ),
    size = 6,
    fontface = "bold",
    vjust = 0 # Alineación vertical para que empuje la letra hacia arriba desde ese punto
  ) +
  
  labs(
    title = "Shrub height",
    x = NULL, 
    y = expression(paste("Shrub height (m)"))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12, color = "black")
  )


#----------------------------------
# Gráfico 6 Shrub diversity
#----------------------------------

p6<-ggplot(veg_summary2, aes(x = forty, y = mean_shannon, fill = forty)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = mean_shannon - se_shannon, ymax = mean_shannon + se_shannon),
    width = 0.2
  ) +
  
  # LETRAS AUTOMÁTICAS CON GEOM_TEXT
  geom_text(
    aes(
      # Le sumamos un margen al error estándar (ajusta el + 15 según tu escala)
      y = mean_shannon + se_shannon + 0.05, 
      label = c("a", "a", "a", "a")
    ),
    size = 6,
    fontface = "bold",
    vjust = 0 # Alineación vertical para que empuje la letra hacia arriba desde ese punto
  ) +
  
  labs(
    title = "Shrub diversity",
    x = NULL, 
    y = expression(paste("Shrub diversty (Shannon)"))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 12, color = "black", face = "bold")
  )

#-------------------------------------
# Coloco los gra´ficos
#------------------------------------


library(patchwork)

# Listamos los gráficos fila por fila:
# Fila 1: p1 (izq), p4 (der)
# Fila 2: p2 (izq), p5 (der)
# Fila 3: p3 (izq), p6 (der)
panel_final <- p1 + p4 + p2 + p5 + p3 + p6 + 
  plot_layout(ncol = 2, nrow = 3, byrow = TRUE)

# Guardamos con tus medidas exactas
ggsave(
  filename = "panel_6_graficos_poster.tiff", 
  plot = panel_final,                     
  width = 28,                             
  height = 24,                            
  units = "cm",                           
  dpi = 300                               
)
