
marzo <- read.csv2("data/Data_Marzo.csv", encoding = "latin1")
abril <- read.csv2("data/Data_Abril.csv", encoding = "latin1")
mayo  <- read.csv2("data/Data_Mayo.csv", encoding = "latin1")

id_variable <- c("P7280", "P1806", "P7260", "P7422S1")

marzo.sel  <- marzo[, id_variable, drop = FALSE]
abril.sel  <- abril[, id_variable, drop = FALSE]
mayo.sel   <- mayo[,  id_variable, drop = FALSE]

marzo.sel$Mes <- "Marzo"
abril.sel$Mes <- "Abril"
mayo.sel$Mes  <- "Mayo"

datos_todos <- rbind(marzo.sel, abril.sel, mayo.sel)

head(datos_todos)
nrow(datos_todos)
