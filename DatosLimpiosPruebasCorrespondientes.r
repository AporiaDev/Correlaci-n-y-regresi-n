# ============================================================
#   LIMPIEZA DE DATOS + MODELO + SIGNIFICANCIAS + ANOVA
#   Guardando resultados en "resultadosDatosLimpios"
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Cargar dataset
# ------------------------------------------------------------
data <- readRDS("data/datos_analysis.rds")

# ------------------------------------------------------------
# 2. Eliminar outliers por IQR
# ------------------------------------------------------------
remove_outliers_iqr <- function(df) {
  df %>% mutate(across(where(is.numeric), function(x) {
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    IQR_value <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR_value
    upper <- Q3 + 1.5 * IQR_value
    x[x < lower | x > upper] <- NA
    return(x)
  })) %>% 
    na.omit()
}

data_clean <- remove_outliers_iqr(data)

# ------------------------------------------------------------
# 3. Transformación log1p(INGLABO)
# ------------------------------------------------------------
data_clean <- data_clean %>% mutate(log_inglabo = log1p(INGLABO))

# ------------------------------------------------------------
# 4. Dummies RAMA2D_R4 (si es necesario)
# ------------------------------------------------------------
data_clean$RAMA2D_R4 <- as.factor(data_clean$RAMA2D_R4)

# ------------------------------------------------------------
# 5. Modelo de regresión
# ------------------------------------------------------------
modelo <- lm(
  log_inglabo ~ P6800 + P6850 + RAMA2D_R4,
  data = data_clean
)

# ------------------------------------------------------------
# 6. Crear carpeta de resultados
# ------------------------------------------------------------
dir.create("resultadosDatosLimpios", showWarnings = FALSE)

# ------------------------------------------------------------
# 7. Guardar resultados
# ------------------------------------------------------------

# --- Coeficientes ---
write.csv(
  summary(modelo)$coefficients,
  "resultadosDatosLimpios/coeficientes.csv",
  row.names = TRUE
)

# --- ANOVA ---
write.csv(
  anova(modelo),
  "resultadosDatosLimpios/anova.csv"
)

# --- R2 y R2 ajustado ---
r2_df <- data.frame(
  R2 = summary(modelo)$r.squared,
  R2_Ajustado = summary(modelo)$adj.r.squared
)

write.csv(
  r2_df,
  "resultadosDatosLimpios/r2.csv",
  row.names = FALSE
)

# --- Significancia global (F-test) ---
f <- summary(modelo)$fstatistic
p_global <- pf(f[1], f[2], f[3], lower.tail = FALSE)

significancia_global <- data.frame(
  Estadistico_F = f[1],
  gl1 = f[2],
  gl2 = f[3],
  p_value_global = p_global
)

write.csv(
  significancia_global,
  "resultadosDatosLimpios/significancia_global.csv",
  row.names = FALSE
)

# --- Resumen completo del modelo en TXT ---
sink("resultadosDatosLimpios/ResumenModelo.txt")
cat("=====================================================\n")
cat("         RESUMEN COMPLETO DEL MODELO\n")
cat("=====================================================\n\n")
print(summary(modelo))
sink()

cat("\n✔️ Todos los resultados fueron guardados en la carpeta 'resultadosDatosLimpios'\n")
