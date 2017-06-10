##Primero, se establece el directorio de trabajo...
        setwd("~/GitHub/ProgramacionActuarial3")

#,de ahí, se invocan los archivos de test y train
        xtest <- read.table("./test/X_test.txt")
        ytest <- read.table("./test/y_test.txt")
        stest <- read.table("./test/subject_test.txt")

        xtrain <- read.table("./train/X_train.txt")
        ytrain <- read.table("./train/y_train.txt")
        strain <- read.table("./train/subject_train.txt")

#Ahora, prosigue la fusión apropiada de las variables x, y, s...
        unionx <- rbind(xtrain, xtest)
        uniony <- rbind(ytrain, ytest)
        unions <- rbind(strain, stest)

#, y prosigue la depuración de las variables que no se usarán
        rm(xtest); rm(ytest); rm(stest); rm(xtrain); rm(ytrain); rm(strain)

##Segundo, se procede a extraer los datos de media y desviación estándar
##del archivo features.txt, quitando lo que no es útil.
        rasgos <- read.table("./features.txt")
        grep("mean\\(\\)|std\\(\\)", rasgos[, 2]) -> MediaDesv
        unionx[,MediaDesv] -> unionx

        names(unionx) <- gsub("\\(\\)", "", rasgos[MediaDesv, 2])
        names(unionx) <- gsub("mean", "Mean", names(unionx))
        names(unionx) <- gsub("std", "Std", names(unionx))
        names(unionx) <- gsub("-", "", names(unionx))

#El segundo paso termina con la depuración de lo inservible
        remove(rasgos); rm(MediaDesv)

##Tercero, se procede a invocar las actividades en la nueva base de datos,
##por medio del uso de activity_labels.txt...
        actividad <- read.table("./activity_labels.txt") 
        actividad[, 2] <- tolower(gsub("_", "", actividad[, 2]))
        substr(actividad[2, 2], 8, 8) <- toupper(substr(actividad[2, 2], 8, 8))
        substr(actividad[3, 2], 8, 8) <- toupper(substr(actividad[3, 2], 8, 8))
        uniony[, 1] <- actividad[uniony[, 1], 2]
        names(uniony) <- "actividad"

##Cuarto, se le proporciona a las etiquetas un nombre apropiado para la nueva
##base de datos (al combinar las etiquetas de cada dirección).
        names(unions) <- "sujeto"
        DatosLimpios <- cbind(unions, uniony, unionx)
        rm(unionx); rm(uniony)

##Quinto, de lo recopilado, se crea otra base de datos, usando el promedio de
##cada medición para reducir los datos y completar la base de datos

        longActiv <- dim(actividad)[1]
        longDatos <- dim(DatosLimpios)[2]
        longS <- length(table(unions))
        resultado <- as.data.frame(matrix(NA, nrow = longActiv * longS, ncol = longDatos))
        colnames(resultado) <- colnames(DatosLimpios)

        m <- 1
        for(k in 1:longS) {
                for(l in 1:longActiv) {
                        resultado[m, 2] <- actividad[l, 2]
                        resultado[m, 1] <- sort(unique(unions)[, 1])[k]
                        tomo1 <- k == DatosLimpios$sujeto
                        tomo2 <- actividad[l, 2] == DatosLimpios$actividad
                        resultado[m, 3:longDatos] <- colMeans(DatosLimpios[tomo1&tomo2, 3:longDatos])
                        m <- m + 1
                        }
        }

write.table(resultado, "BaseCompleta.txt", row.names = FALSE)