# This code was written by Pilar Castro-Diez for analysis of tree structure in Huelva
# Last update 25/05/2026

# The code performs the following:
#   1. Open file with data of tree structure
#   2. Creates df with variables at plot level (datos_plot)
#   3. Calculates sp richness and diversity with vegan (adds them to datos_plot)
#   4. Creates summary table with means and se and export it as "result_str_treelayer.csv"
#   5. Statistical comparison of variables among vegetation types


# Working directory
#setwd("G:/R_analyses/2026-PROGRESA-C/srt_tree-layer")

# R packages
library(dplyr)
library(nlme)
library(emmeans) #para pairwise Tukey comparison
library(multcomp) #para asignar letras
library(glmmTMB) #for gamma, ordinal, mix models
library(DHARMa) # for assessing gamma model conditions
library(car) # for Anova

#-------------------------
#    1. Read data
#-------------------------
tree = read.table("tree_layer_str.csv", header=TRUE, sep=";")
str(tree)
## veg.ty: vegetation type
  #E0: managed eucalypt plantations
  #E10: recent abandoned plantation (c.a. 10 years ago)
  #E27: old abandonded plantations (>27 years ago)
  #Q: Q. suber forest
## Plot: unique code asigned to each sample plot
## Sp: tree species
## ba_tree: tree basal area (m2), calculated from DBH
## Comments: verbatim health status
## Health: health index
  # 0: dead
  # 1: highly damaged
  # 2: damaged
  # 3: healthy
## d: tree DBH (cm)
## h: tree heigh (m)

#-------------------------------------------------------------------------
### run to repeat all analyses considering only tree species = E. globulus
#tree <- tree %>% filter(sp == "Eucalyptus globulus") 
#-------------------------------------------------------------------------

#--------------------------------------------------------------------------------------
#   2.  CREATES df datos_plot with variables calculated at plot level  
#--------------------------------------------------------------------------------------

# Create df by plot, with mean DBH (d), mean heigh (h), mean health, basal area (m2/ha) and tree density (per ha)

# área de la parcela de 10 m de radio en m2
plot_area <- pi * 10^2

datos_plot <- tree %>%
  group_by(veg_ty, plot) %>%
  summarise(
    veg_ty = first(veg_ty),
    d_plot        = mean(d, na.rm = TRUE),
    h_plot        = mean(h, na.rm = TRUE),
    health_plot   = mean(health, na.rm = TRUE),
    density_ha = n() / plot_area * 10000,
    ba_ha = sum(ba_tree, na.rm = TRUE)/ plot_area * 10000, #m2/ha
    prop_eg = mean(sp == "Eucalyptus globulus", na.rm = TRUE), #calculo la proporción de árboles que son E. globulus
     .groups = "drop"
  )


#-----------------------------------------------------------------------
#           3. Calculates sp richness and diversity with vegan
#-----------------------------------------------------------------------

library(tidyr)
library(tibble) # Necesario para manejar los nombres de las filas

# --- 3.1. CREAR LA MATRIZ DE COMUNIDAD ---

comunidad <- tree[, c(1:4)] %>%
  # Paso A: Sumar el área basal por parcela y especie
  group_by(plot, sp) %>%
  summarise(ba_total = sum(ba_tree, na.rm = TRUE), .groups = "drop") %>%
  
  # Paso B: Pivotar a formato ancho (especies en columnas)
  # Rellenamos con 0 las especies que no estén en un plot
  pivot_wider(names_from = sp, values_from = ba_total, values_fill = 0) %>%
  
  # Paso C: Pasar la columna 'plot' a los nombres de las filas (rownames)
  # Esto garantiza que la matriz sea 100% numérica
  column_to_rownames(var = "plot")

# --- 3.2. CREAR LA TABLA DE METADATOS (AMBIENTE) ---

# Usamos los rownames de 'comunidad' para asegurar que el orden sea IDÉNTICO
ambiente <- data.frame(plot = rownames(comunidad)) %>%
  left_join(
    tree %>% dplyr::select(plot, veg_ty) %>% distinct(), 
    by = "plot"
  ) %>%
  column_to_rownames(var = "plot")

# --- 3.3. CALCULO RIQUEZA Y DIVERSIDAD CON VEGAN ---

library(vegan)

# 3.3.1. Calcular la Riqueza de especies (Número de especies por plot)
riqueza_plots <- specnumber(comunidad)

# 3.3.2. Calcular el Índice de Diversidad de Shannon
shannon_plots <- diversity(comunidad, index = "shannon") #Por defecto usa la base de logaritmo natural (ln)

# 3.3.3. Añado los vectores calculados de riqueza y shannon al df plot, conincidiendo el código plot
# 3.3.3.1. Creamos una tabla temporal con los índices y sus respectivos IDs de plot
indices_df <- data.frame(
  plot = names(riqueza_plots),
  Riqueza = riqueza_plots,
  Shannon = shannon_plots
)

# 3.3.3.2. Fusionamos con dataframe 'datos_plots' existente.
datos_plot <- left_join(datos_plot, indices_df, by = "plot")

# 3.3.3.3. Borro dfs y vectores temporales
rm(indices_df, ambiente, comunidad, plot_area, riqueza_plots, shannon_plots)

#-----------------------------------------------------------------------------------------------------
#           4. Creates summary table with means and se and export it as "result_str_treelayer.csv"
#------------------------------------------------------------------------------------------------------

# calculate mean and se of plot density, ba, prop_eg
veg_summary <- datos_plot %>%
  group_by(veg_ty) %>%
  summarise(
    
    ba_ha_mean = mean(ba_ha, na.rm = TRUE), #m2/ha
    se_ba = sd(ba_ha, na.rm = TRUE) / sqrt(n()),
    
    mean_density = mean(density_ha, na.rm = TRUE), #no/ha
    se_density = sd(density_ha, na.rm = TRUE) / sqrt(n()),
    
    mean_prop_eg = mean(prop_eg, na.rm = TRUE),
    se_prop_eg = sd(prop_eg, na.rm = TRUE) / sqrt(n()),
    
    
    mean_richness = mean(Riqueza, na.rm = TRUE),
    se_richness = sd(Riqueza, na.rm = TRUE) / sqrt(n()),
    
    mean_shannon = mean(Shannon, na.rm = TRUE),
    se_shannon = sd(Shannon, na.rm = TRUE) / sqrt(n()),
    
    d_plot_media = mean(d_plot, na.rm = TRUE), 
    d_plot_se = sd(d_plot, na.rm = TRUE) / sqrt(n()),
    
    h_plot_media = mean(h_plot, na.rm = TRUE), 
    h_plot_se = sd(h_plot, na.rm = TRUE) / sqrt(n()),
    
    health_plot_media = mean(health_plot, na.rm = TRUE), 
    health_plot_se = sd(health_plot, na.rm = TRUE) / sqrt(n()),
    
    n_plots = n(),
    .groups = "drop"
  )


write.table(veg_summary, file="result_str_treelayer.csv", col.names=T, row.names=F, sep=";")

#-------------------------------------------------------------------------------------------
#       5. Statistical comparison of variables among vegetation types
#-------------------------------------------------------------------------------------------


#----------------
# 5.1. Basal area
#----------------

# 1. Ajustamos el modelo lineal tradicional (asume varianzas iguales)
mod_ba_lm<-gls(ba_ha ~ veg_ty, data=datos_plot) # library(nlme)

# 2. Ajustamos el modelo GLS (permite varianzas diferentes por veg_ty)
mod_ba_gls <- gls(ba_ha ~ veg_ty, data = datos_plot, 
               weights = varIdent(form = ~ 1 | veg_ty))

# 3. Comparamos ambos modelos numéricamente
anova(mod_ba_lm, mod_ba_gls) # gls menor AIC (pero mayor BIC), y ambos difieren P=0.0495. Me quedo con el segundo

anova(mod_ba_gls)
mi_emmeans<-emmeans(mod_ba_gls, pairwise ~ veg_ty, adjust = "Tukey") #posthoc
mi_emmeans
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor

#------------------
# 5.2. Tree density
#------------------

# Creo modelo lineal (lm) y modelo gls y los comparo
mod_dens_lm <- gls(density_ha ~ veg_ty, data=datos_plot)
mod_dens_gls <- gls(density_ha ~ veg_ty, data = datos_plot, 
                     weights = varIdent(form = ~ 1 | veg_ty))
anova(mod_dens_lm, mod_dens_gls) # difieren significativamente, AI del gls 13 unidades de AIC menor que lm. Elijo gls

anova(mod_dens_gls)

mi_emmeans<-emmeans(mod_dens_gls, pairwise ~ veg_ty, adjust = "Tukey")
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor
#----------------------------
# 5.3. Proportion E. globulus
#----------------------------

mod_propEG_lm <- gls(prop_eg ~ veg_ty, data= subset(datos_plot, veg_ty!="Q"))
mod_propEG_gls <- gls(prop_eg ~ veg_ty, data = subset(datos_plot, veg_ty!="Q"), 
                    weights = varIdent(form = ~ 1 | veg_ty))
anova(mod_propEG_lm, mod_propEG_gls) #Elijo gls

anova(mod_propEG_gls)

mi_emmenans<-emmeans(mod_propEG_gls, pairwise ~ veg_ty, adjust = "Tukey")
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor

# ------------
# 5.4. DBH 
# ------------
# Elijo modelo Gamma para el Diámetro porque la distribución es muy sesgada hacia un lado
m_dbh_disp <- glmmTMB(d ~ veg_ty + (1 | plot), 
                      dispformula = ~ veg_ty, 
                      data = tree, 
                      family = Gamma(link = "log"))

# Validar con DHARMa (Obligatorio)
residuos_dbh <- simulateResiduals(fittedModel = m_dbh_disp)
plot(residuos_dbh)

# Veo el resultado del modelo
Anova(m_dbh_disp) #ojo, Anova con A mayúscula, paquete car

# Calcular las medias estimadas y las comparaciones de parejas
mi_emmeans<-emmeans(m_dbh_disp, pairwise ~ veg_ty, type = "response") #type response sirve para que deshaga el log y devuelva las medias en la escala original
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor
# ---------------------------
# 5.5. Species richness
# ---------------------------

# 1. AJUSTE DEL GLM DE POISSON
m_riq_poisson <- glm(Riqueza ~ veg_ty, data = datos_plot, family = poisson(link = "log"))

# 2. Diagnóstico de residuos Simular los residuos basados en tu modelo de Poisson
residuos_simulados <- simulateResiduals(fittedModel = m_riq_poisson)
plot(residuos_simulados) # Hay sobredispersión. Usar modelo quasipoisson que corrige errores estandar

# 4. QUASIPOISSON (Si el índice de dispersión fue mayor a 1.2 - 1.5)
m_riq_quasi <- glm(Riqueza ~ veg_ty, data = datos_plot, family = quasipoisson(link = "log"))
summary(m_riq_quasi)

# Diagnóstico de reisiduos (no se aplica DHARMa): Extraemos los residuos de Pearson y los valores predichos 
residuos <- residuals(m_riq_quasi, type = "pearson")
ajustados <- fitted(m_riq_quasi)

# Dibujamos el gráfico
plot(ajustados, residuos, 
     xlab = "Valores Ajustados (Predichos)", 
     ylab = "Residuos de Pearson",
     main = "Diagnóstico Quasipoisson")
abline(h = 0, col = "red", lty = 2)  ## No hay estructuras. Bien.

# 5. Para saber si "Tipo de Bosque" en general es significativo
anova(m_riq_quasi, test = "F") # Usamos test = "F" para Quasipoisson (o "Chisq" si fue Poisson puro)

# 6. COMPARACIONES MÚLTIPLES POST-HOC 
mi_emmeans<-emmeans(m_riq_quasi, poly ~ veg_ty, type = "response")
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor

# -------------------------------------------------------------------
# 5.6. Species diversity (Shannon)
# -------------------------------------------------------------------

# 1. Ajustas el modelo lineal tradicional
m_shannon <- lm(Shannon ~ veg_ty, data = datos_plot)

# 2. Diagnóstico de residuos
residuos_shannon <- simulateResiduals(fittedModel = m_shannon)
plot(residuos_shannon)

# 3. Significación
anova(m_shannon)

# 4. Post-hoc
mi_emmeans<-emmeans(m_shannon, ~ veg_ty)
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor

# -------------------------------------------------------------------
# 5.7. tree height (gamma model)
# -------------------------------------------------------------------
# Modelo A: GLMM con distribución Gamma (link logarítmico por defecto)
# Asume que la dispersión general es la misma, pero ajusta la asimetría
# plot es un factor aleatorio 
m_gamma <- glmmTMB(h ~ veg_ty + (1 | plot), 
                   data = tree, 
                   family = Gamma(link = "log"))

# Modelo B: GLMM Gamma modelando la varianza (dispersión) por tipo de vegetación
# Permite que la plantación tenga poca varianza y el bosque natural mucha (dispersión distinta entre veg_ty).
m_gamma_disp <- glmmTMB(h ~ veg_ty + (1 | plot), 
                        dispformula = ~ veg_ty, 
                        data = tree, 
                        family = Gamma(link = "log"))


# 2. Selección del mejor modelo
# Quédate con el que tenga el AIC más bajo (una diferencia > 2 es significativa).
AIC(m_gamma, m_gamma_disp) #el último

# 3. Validación de Supuestos (Diagnóstico moderno)
residuos_simulados <- simulateResiduals(fittedModel = m_gamma_disp)
plot(residuos_simulados)
# Este plot genera dos gráficos maravillosos:
# 1. Un QQ-plot ajustado para GLMMs (con tests estadísticos de normalidad integrados).
# 2. Un gráfico de residuos vs predicciones que evalúa la homocedasticidad con regresiones cuantílicas.

# Veo el resultado del modelo
Anova(m_gamma_disp) #ojo, Anova con A mayúscula, paquete car

# Calcular las medias estimadas y las comparaciones de parejas
mi_emmeans<-emmeans(m_gamma_disp, pairwise ~ veg_ty, type = "response") #type response sirve para que deshaga el log y devuelva las medias en la escala original
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor

# -----------------------------------------
# 5.8. Health index (modelo ordinal mixto)
# -----------------------------------------

library(ordinal)

# ES CRUCIAL que R sepa que es un factor ordenado
tree$health_ord <- factor(tree$health, ordered = TRUE)

# Ajustamos el modelo ordinal mixto, con plot como factor aleatorio
m_health_ord <- clmm(health_ord ~ veg_ty + (1 | plot), data = tree)

summary(m_health_ord)
Anova(m_health_ord)

#Comparación múltiple
mi_emmeans<-emmeans(m_health_ord, pairwise ~ veg_ty)
mi_emmeans
cld(mi_emmeans, Letters = letters, decreasing = FALSE) # Asignar letras de menor a mayor

#----------------------------------------------------------------------
# Repeat using only E. globulus


