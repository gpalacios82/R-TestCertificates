library(openssl)
library(readxl)
library(lubridate)
library(dplyr)
library(purrr)
library(mailR)
library(htmlTable)

#Inicializamos variables de alerta y fichero de certificados
dias_yellow <- 60
dias_red <- 30
urls <- read_excel("C:/dir/certificados.xlsx")


# Parametros del smtp
from <- "from@domain.com"
to <- c("to@domain.com","to2@domain.com")
smtp <- list(host.name = "smtp.domain.com", port = "25",
             user.name = "from@domain.com", passwd = "password", tls = T)

# Inicializamos y leemos datos
urls$caducidad <- Sys.Date()
urls$dias <- 0
urls$yellow_alert <- T
urls$red_alert <- T
urls$validez <- F

download_ssl_cert2 <- possibly(download_ssl_cert, otherwise = NA)

# Obtenemos tiempos de caducidad de certificados
i <- 1
while (i <= nrow(urls))
{
  print(paste('Verificando: ',urls$URL[i],sep=""))
  cert <- download_ssl_cert2(urls$URL[i])
  
  if(sum(!is.na(cert))>0){
    urls$caducidad[i] <- lubridate::dmy(paste(substr(cert[[1]]$validity[2],4,6),substr(cert[[1]]$validity[2],1,3),substr(cert[[1]]$validity[2],17,20)))
    urls$validez[i] <- cert_verify(cert, ca_bundle())
    urls$dias[i] <- as.numeric(urls$caducidad[i]-Sys.Date())
    
    urls$yellow_alert[i] <- urls$dias[i] <= dias_yellow
    urls$red_alert[i] <- urls$dias[i] <= dias_red
  }
  
  i <- i+1
}

urls <- urls %>%
  arrange(dias)

# Calculamos alertas
alert <- F
if(sum(urls$red_alert)>0 || sum(!urls$validez)>0){
  alert <- T
  subject <- '[RED] SSL > Hay certificados proximos a caducar'
}else{
  if(sum(urls$yellow_alert)>0){
    alert <- T
    subject <- '[YELLOW] SSL > Hay certificados pendientes de renovar'
  }
}


# Si hay alertas, enviamos email
if(alert)
{
  #Enviar mail
  urls_error <- urls %>%
    filter(yellow_alert == T)
  
  y <- htmlTable(urls_error, rnames = FALSE, css.cell = "padding-left: 1.2em; padding-right: 1.2em;")
  
  send.mail(from = from, to = to, subject = subject, body = paste0('<p>Estos son los certificados afectados:</p>',y),
            smtp = smtp, encoding = "iso-8859-1", authenticate = T, send = T, html = T)
}

