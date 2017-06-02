rankingcompleto <- function(resultado, num = "mejor") {
        setwd("/Users/pManuel1827/Documents/GitHub/ProgramacionActuarial3/Calidad de hospitales")
# Lectura de datos
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
# Revisión de la validez de estado y resultado
        if (!((resultado == "ataque") | (resultado == "falla")
              | (resultado == "neumonía"))) {
                stop ("Resultado NO Válido")
                break
        }
        
        newout<- if (resultado == "ataque") {
                outcome[c(2,7,11)] 
        } else if (resultado == "falla") {
                outcome[c(2,7,17)] 
        } else {
                outcome[c(2,7,23)] 
        }
        
#eds = inicio, pdf = pos
# Para cada estado, encuentra el hospital con la posición dada.
        inicio <- sort(unique(outcome$State))
        pos <- vector("character")
        edos <- vector("character")
        status <- levels(inicio)
        
        for (estado in inicio){
                ext <- subset(newout, newout$State==estado  & !newout[[3]]=="Not Available")
                lista <- ext[order(as.numeric(ext[[3]]),ext[[1]]),]
                
                if (num == "peor"){
                        peor <- lista[which.max(lista[[3]]),]
                        hospital <- peor$Hospital.Name
                        pos <- c(pos,hospital)
                }else if(num == "mejor"){
                        mejor <- lista[which.min(lista[[3]]),]
                        hospital <- mejor$Hospital.Name
                        pos <- c(pos,hospital)
                }else {
                        hospital <- lista[num,1]
                        pos <- c(pos,hospital)  
                }
                edos <- c(edos, inicio)
        }
        
#Data frame que incluye: hospital y estado al que pertenece
        formulario <- data.frame(pos,edos)
        colnames(formulario) <- c("hospital", "state")
        rownames(formulario)<- status
        formulario
       
}