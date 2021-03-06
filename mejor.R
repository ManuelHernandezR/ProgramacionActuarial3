setwd("/Users/pManuel1827/Documents/GitHub/ProgramacionActuarial3/Calidad de hospitales")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])
mejor <- function(estado, resultado = ataque/falla/neumonia){
        setwd("/Users/pManuel1827/Documents/GitHub/ProgramacionActuarial3/Calidad de hospitales")
# Lectura de datos
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
# Revisi�n de la validez de estado y resultado        
        estados <- levels(factor(outcome[, 7]))
        resultados <- c("Infarto", "Falla Card�aca", "Neumon�a")
        
        if (estado %in% estados == F) {
                stop("Estado NO v�lido") #se detiene
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
# Regresa el nombre del hospital con la tasa m�s baja de
# mortalidad de 30 d�as
        col_a <- outcome[outcome$State == estado,]
        col_b <- col_a[,col_acceso]
        Hosp_selec <- col_a[,2]
        col_c <- cbind(Hosp_selec,col_b)
        
        col_d <- suppressWarnings(as.numeric(col_c[,2]))
        orden1 <- suppressWarnings(order(col_d, na.last = NA))
        orden2 <- suppressWarnings((col_c[orden1,]))
        orden2[1]
        
}