# Suponiendo 'modelo' ya estimado
coef_tab <- summary(modelo)$coefficients
coef_df <- as.data.frame(coef_tab)
coef_df$term <- rownames(coef_df)
names(coef_df) <- c("Estimate","Std.Error","t.value","Pr..t..","term")

# efecto porcentual aproximado
coef_df$Pct_change <- (exp(coef_df$Estimate)-1)*100

# Reordenar columnas
coef_df <- coef_df[,c("term","Estimate","Std.Error","t.value","Pr..t..","Pct_change")]

write.csv(coef_df, "resultadosDatosLimpios/coeficientes_con_efecto_pct.csv", row.names = FALSE)
