rankhospital <- function(estado, resultado, num = "best"){
        setwd("/Users/pManuel1827/Documents/GitHub/ProgramacionActuarial3/Calidad de hospitales")
# Lectura de datos
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
# Revisi�n de la validez de estado y resultado
        estados <- levels(factor(outcome[,7]))
        resultados <- c("ataque", "falla", "neumon�a")
        if (estado %in% estados == F) {
                stop(print("Estado NO v�lido")) #se detiene
        }
        
        col_acceso <- if (resultado == "ataque") {
                11
        } else if (resultado == "falla") {
                17
        } else if (resultado == "neumon�a") {
                23
        } else {TRUE}
        
        if ((resultado == TRUE)) {
                stop("Resultado NO v�lido")
        }
# Regresa el nombre del hospital con el puesto dado de la tasa m�s
        # baja de mortalidad de 30 d�as
        new_data <- subset(outcome, State == estado)
        
        new_data[, col_acceso] <- as.numeric(new_data[, col_acceso])
        mal <- is.na(new_data[, col_acceso])
        Info <- new_data[!mal, ]
        
        Nombre_Col_Res <- names(Info)[col_acceso]
        Nombre_Hosp <- names(Info)[2]
        index <- with(Info, order(Info[Nombre_Col_Res], Info[Nombre_Hosp]))
        Datos_Ordenados <- Info[index, ]
        
        if (is.character(num) == TRUE) {
                if (num == "mejor") {
                        num = 1
                }
                else if (num == "peor") {
                        num = length(Datos_Ordenados[, col_acceso])
                }
        }
        Datos_Ordenados[num, 2]
}