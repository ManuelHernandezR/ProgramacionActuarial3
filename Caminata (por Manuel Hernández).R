xmayor <- 0
xmenor <- 0
caminata <- vector("numeric")
contador <- 0
#caminato <- vector("numeric",length = 100)
for(m in 1:100){  
  ab <- 5
  while(ab>=3 && ab<=10) {
    print(ab) 
    caminata <- c(caminata,ab)
    contador = contador + 1
    moneda <- rbinom(1,1,0.5)
    if(moneda==1){ #Caminata aleatoria
      ab <- ab + 0.5
    } else {
      ab <- ab - 0.5
    }
  }
  if (caminata[[contador]]<=3) { #las variables menores que 3
    xmenor <- xmenor + 1
  } else {xmayor <- xmayor + 1}
}
plot(caminata, type = "l")
  print(xmenor)
  print(xmayor)# las variablea mayores que 3