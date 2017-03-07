source("/home/javier/Documentos/javier/bd_minsa/MINSA TOTAL PREPOST/spatcontrol/spatcontrol.R",chdir=TRUE,echo = F)

# cargamos la tabla de post2009
setwd("/home/javier/Documentos/javier/bd_minsa/MINSA TOTAL PREPOST/resultados")
post2009<-read.csv("cons_roc_2009_2015_CLAUDIA.csv",header=TRUE)

# creamos una columna con nombre de tratadas para
# que coincidan con la de UNION_ROCIADOS.csv
post2009$Residual_T <- 0
post2009$Residual_T <- as.numeric(post2009$P != 0)
post2009$STATUS <- unlist("T")


# cargamos la tabla de viv no rociadas
post2009nr <- read.csv("viv_no_roc_CLAU.csv")
head (post2009nr)

# creamos nuevas columas y le asignamos primero 0
# luego ponemos 1 a cada una de las columnas correspondientes
# a cerradas, deshabitadas, renuentes, locales publicos y lotes vacios 
post2009nr$Residual_R <- unlist(0)
post2009nr$Residual_C <- unlist(0)
post2009nr$Residual_D <- unlist(0)
post2009nr$Residual_LP <- unlist(0)
post2009nr$Residual_LV <- unlist(0)
post2009nr$Residual_R <- as.numeric(post2009nr$STATUS== "R")
post2009nr$Residual_C <- as.numeric(post2009nr$STATUS == "C")
post2009nr$Residual_D <- as.numeric(post2009nr$STATUS == "DES")
post2009nr$Residual_LP <- as.numeric(post2009nr$STATUS == "LP")

post2009nr$Residual_LV <- as.numeric(post2009nr$STATUS == "LV")

  
# post2009nr<-changeNameCol(post2009nr,"unicode","UNICODE")

# aniade columnas a post2009nr
lqf<-setdiff(names(post2009),names(post2009nr))
for(nombre in lqf){
  post2009nr[[nombre]]<-0
}
# aniade columnas a post2009
lqf<-setdiff(names(post2009nr),names(post2009))
for(nombre in lqf){
  post2009[[nombre]]<-0
}

# Une las dos
post2009completo<-rbind.general(post2009nr,post2009)
# setwd("/home/javier/Documentos/javier/bd_minsa/MINSA TOTAL PREPOST/resultados")
write.csv(post2009completo,"CONS_ROCIADO_2009_2015_con_cerradas_CLAUDIA.csv",row.names=FALSE)


