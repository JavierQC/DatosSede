 # source("spatcontrol/spatcontrol.R",chdir=TRUE)
source("/home/javier/Documentos/javier/bd_minsa/MINSA TOTAL PREPOST/spatcontrol/spatcontrol.R",chdir=TRUE,echo = F)
setwd("/home/javier/Documentos/javier/bd_minsa/MINSA TOTAL PREPOST/resultados")
# importacion de datos
pre2009<-read.csv("UNION_ROCIADOS.csv",header=TRUE, sep=";")
head(pre2009) 

post2009<-read.csv("CONS_ROCIADO_2009_2015_con_cerradas.csv",header=TRUE)
head(post2009)

# Conversion de los "NULL" en 0
pre2009<-set_to(pre2009,init=c("NULL"),final=0)
post2009<-set_to(post2009)


### combinando columnas que se necesitan combinar
# checkando nombres
# intersect(names(pre2009),names(post2009)) # muestra lo commun
# setdiff(names(post2009),names(pre2009)) # muestra lo unicamente in el primero
# setdiff(names(pre2009),names(post2009)) # muestra lo unicamente in el primero

## como hay bastante columnas con el mismo nombre menos
## que en lugar de subguiones tiene puntos, cambiamos puntos a 

# Primero cambiamos algunos nombres que nos interesan
colnames(pre2009)[colnames(pre2009)=="I.cap.T"] <- "IN_TRI"
colnames(pre2009)[colnames(pre2009)=="P.cap.T"] <- "PE_TRI"
colnames(pre2009)[colnames(pre2009)=="Nb.cargas"] <- "CARGASNTSCD"
colnames(pre2009)[colnames(pre2009)=="sup.m2"] <- "SUPER"
colnames(pre2009)[colnames(pre2009)=="CICLO"] <- "CICLO_ROCIADO"
colnames(pre2009)[colnames(pre2009)=="OBSERVACIONES"] <- "NOTAS"
colnames(pre2009)[colnames(pre2009)=="I.num.amb"] <- "IN_AMB"
colnames(pre2009)[colnames(pre2009)=="P.num.amb"] <- "PE_AMB"
colnames(pre2009)[colnames(pre2009)=="I.Gri"] <- "IN_GRI"
colnames(pre2009)[colnames(pre2009)=="P.Gri"] <- "PE_GRI"
colnames(pre2009)[colnames(pre2009)=="I.S"] <- "IN_SIL"
colnames(pre2009)[colnames(pre2009)=="I.NO"] <- "IN_NOB"
colnames(pre2009)[colnames(pre2009)=="I.AD"] <- "IN_ADO"
colnames(pre2009)[colnames(pre2009)=="I.LSE"] <- "IN_LAD"
colnames(pre2009)[colnames(pre2009)=="P.S"] <- "PE_SIL"
colnames(pre2009)[colnames(pre2009)=="P.NO"] <- "PE_NOB"
colnames(pre2009)[colnames(pre2009)=="P.LSE"] <- "PE_LAD"
colnames(pre2009)[colnames(pre2009)=="P.AD"] <- "PE_ADO"
colnames(pre2009)[colnames(pre2009)=="NOMBRE.DEL.BRIGADISTA"] <- "ROCIADOR"
colnames(pre2009)[colnames(pre2009)=="NOMBRE.JEFE.DE.FAMILIA"] <- "JEFEVIV"
colnames(pre2009)[colnames(pre2009)=="No.RESID"] <- "NUMRES"
colnames(pre2009)[colnames(pre2009)=="P.otro"] <- "PE_MAT"
colnames(pre2009)[colnames(pre2009)=="I.otro"] <- "IN_MAT"

## subguillones en nombres de columnas

names(pre2009) <- gsub("\\.", "_", names(pre2009))
names(post2009) <- gsub("\\.", "_", names(post2009))

## Hacemos que las recuperadas de pre2009 Residual_Rec
#  tambien figuren en la columna de tratadas Residual_T
pre2009$Residual_T <- as.numeric(pre2009$Residual_Rec == 1 | pre2009$Residual_T == 1)
pre2009$Residual_LV <- unlist(0)
pre2009$Residual_LP <- unlist(0)

# Uniendo columnas de post2009 a una post2009$OV <- as.numeric(post2009$T_OV == 1 | post2009$P_OV == 1) post2009$OV <- as.numeric(post2009$T_OV == 1 | post2009$P_OV == 1) sola 
#  setdiff(names(pre2009),names(post2009))
post2009$CU <- unlist(0)
post2009$CO <- unlist(0)
post2009$OV <- unlist(0)
post2009$PE <- unlist(0)
post2009$AV <- unlist(0)
post2009$GA <- unlist(0)
post2009$CU <- ifelse((post2009$TEC_CUY== 1 | post2009$PAT_CUY == 1),1,0)
post2009$CO <- ifelse((post2009$TEC_CON == 1 | post2009$PAT_CON == 1), 1,0)
post2009$OV <- ifelse((post2009$TEC_OVE == 1 | post2009$PAT_OVE == 1), 1,0)
post2009$PE <- ifelse((post2009$TEC_PER == 1 | post2009$PAT_PER == 1), 1,0)
post2009$AV <- ifelse((post2009$TEC_AVE== 1 | post2009$PAT_AVE == 1), 1,0)
post2009$GA <- ifelse((post2009$TEC_GAT == 1 | post2009$PAT_GAT == 1), 1,0)

# para verificar que solamente se obtenga valores diferentes de 0
# en cada linea despues de realizar lo anterior colocamos
# post2009$CU[which(post2009$TEC_CUY != 0)]
# post2009$CU[which(post2009$P_CU != 0)]
# post2009$CO[which(post2009$T_CO != 0)]
# post2009$CO[which(post2009$P_CO != 0)]
# post2009$OV[which(post2009$T_OV != 0)]
# post2009$OV[which(post2009$P_OV != 0)]
# post2009$PE[which(post2009$T_PE != 0)]
# post2009$PE[which(post2009$P_PE != 0)]
# post2009$AV[which(post2009$T_AV != 0)]
# post2009$AV[which(post2009$P_AV != 0)]
# post2009$GA[which(post2009$T_GA != 0)]
# post2009$GA[which(post2009$P_GA != 0)]

#agregando dos columnas mas a pre2009 (excel) para piedra y bloqueta
#en el peridomicilio
pre2009$PE_BLOQ <- 0
pre2009$PE_BLOQ[grep("BLOQ",pre2009$PE_MAT)] <- 1  
pre2009$PE_PIE <- 0
pre2009$PE_PIE[grep("PIE",pre2009$PE_MAT)] <- 1  


# Uniformizxamos la FR_A a formato 2000+FR_A
pre2009$FR_A <- ifelse(pre2009$FR_A <= 9,2000+pre2009$FR_A, pre2009$FR_A)
pre2009$FR_A[pre2009$FR_A == 2000] <- 0 

# no se pueden juntar por ahora:
# I/P.cap/pos/rev.A/N porque no esta con formato 2 la base nueva
# otros.animales con 
# IN_MAT con I_otro porque hay que checkar porque IN_MAT no tiene nada

# haciendo una columna STATUS
pre2009$STATUS <- ifelse(pre2009$Residual_T==1,"T",0)
pre2009$STATUS <- ifelse(pre2009$Residual_R==1,"R",pre2009$STATUS)
pre2009$STATUS <- ifelse(pre2009$Residual_C==1,"C",pre2009$STATUS)
pre2009$STATUS <- ifelse(pre2009$Residual_D==1,"DES",pre2009$STATUS)


#Completando STATUS con LP  y LV que se encuentran en el campo OBSERVACIONES
pre2009$STATUS[(pre2009$STATUS==0) & grepl("P.",pre2009$NOTAS, fixed = TRUE)] <- "LP"
pre2009$STATUS[(pre2009$STATUS==0) & (grepl("V.",pre2009$NOTAS, fixed = TRUE) | grepl("LOTE VACIO",pre2009$NOTAS))] <- "LV"
pre2009$Residual_LP <- ifelse(pre2009$STATUS == "LP",1,0)
pre2009$Residual_LV <- ifelse(pre2009$STATUS == "LV",1,0)

#Si STATUS = NA y contiene la palabra "CONSTRUCCIÓN" o "NO SE ROCIO" ponemos C
#pre2009$STATUS[(pre2009$STATUS==0) & (grepl("CONSTRUCCION",pre2009$NOTAS) | grepl("NO SE ROCIO",pre2009$NOTAS))] <- "C"
#Si STATUS = NA y contiene la palabra "FUE ROCIADA"
#pre2009$STATUS[(pre2009$STATUS==0) & grepl("FUE ROCIADA",pre2009$NOTAS)] <- "T"

# juntando columnas para UNICODE P, D, L, V
pre2009$UNICODE<-paste(pre2009$P,pre2009$D,pre2009$L,pre2009$V,sep=".")

# juntar las dos tablas para las columnas communas
#tendremos que cambia ralgunos nombres en el post 2009 para poder hacer el bind general 
 
 colnames(post2009)[colnames(post2009)=="JEFE_FAMILIA"] <- "JEFEVIV"
 colnames(post2009)[colnames(post2009)=="BRIGADISTA"] <- "ROCIADOR"
 colnames(post2009)[colnames(post2009)=="I_NUM_AMB"] <- "IN_AMB"
 colnames(post2009)[colnames(post2009)=="I_S" ] <- "IN_SIL"
 colnames(post2009)[colnames(post2009)=="I_NO"] <-  "IN_NOB"
 colnames(post2009)[colnames(post2009)=="I_LAD" ] <- "IN_LAD"
 colnames(post2009)[colnames(post2009)== "I_AD"] <-"IN_ADO"
 colnames(post2009)[colnames(post2009)=="I_TRIAT"] <- "IN_TRI"
 colnames(post2009)[colnames(post2009)=="P_GRI"] <- "PE_GRI"
 colnames(post2009)[colnames(post2009)=="P_TRIAT"] <- "PE_TRI"
 colnames(post2009)[colnames(post2009)=="No_CARGAS"] <-"CARGASNTSCD"
 colnames(post2009)[colnames(post2009)=="SUP_M2"] <- "SUPER"
 colnames(post2009)[colnames(post2009)=="OBSERVACIONES" ] <- "NOTAS"
 colnames(post2009)[colnames(post2009)=="P_BLOQ" ] <- "PE_BLOQ"
 colnames(post2009)[colnames(post2009)=="P_PIE"] <-  "PE_PIE"
 colnames(post2009)[colnames(post2009)=="I_GRI"] <- "IN_GRI" 
 colnames(post2009)[colnames(post2009)=="P_S" ] <-  "PE_SIL"

 
 prepost<-rbind.general(pre2009,post2009)
 length(intersect(names(pre2009),names(post2009)))
# hacemos cambios de algunos valores
prepost$PE_SIL[prepost$PE_SIL == 24] <- 1
prepost$IN_GRI[prepost$IN_GRI == "X"] <- 1
prepost$PE_GRI[prepost$PE_GRI == "X"] <- 1
prepost$IN_GRI[prepost$IN_GRI == 3] <- 1

### Inspectors ID
# remove accents and put in upper case
prepost$ROCIADOR <- toupper(iconv(prepost$ROCIADOR, to="ASCII//TRANSLIT"))
# avoid trailing spaces
prepost$ROCIADOR <- gsub(" *$","",prepost$ROCIADOR)

# avoid first letter second familly name
prepost$ROCIADOR <- gsub(" [A-Z].$","",prepost$ROCIADOR)


# individual corrections
prepost$ROCIADOR[prepost$ROCIADOR == "CARLOS VELAVELA"] <- "CARLOS VELA VELA"
prepost$ROCIADOR[prepost$ROCIADOR == "CRISTIAN VELASQUEZ"] <- "CRISTHIAN VELASQUEZ"
prepost$ROCIADOR[prepost$ROCIADOR == "JAIME ROJAS"] <- "JAIME ROJAS VILCA"
prepost$ROCIADOR[prepost$ROCIADOR == "OSCAR BARRIOS"] <- "OSCAR BARRIOS ZEA"
prepost$ROCIADOR[prepost$ROCIADOR == "VIDAL MAYTA"] <- "VIDAL MAYTA HUANCA"
prepost$ROCIADOR[prepost$ROCIADOR == "E. MANZANEDA"] <- "EDGAR MANZANEDA"
prepost$ROCIADOR[prepost$ROCIADOR == "JAIME ROJAS"] <- "JAIME ROJAS VILCA"
prepost$ROCIADOR[prepost$ROCIADOR == "VANDER TICONA"] <- "VANDER TICONA CUBA"
prepost$ROCIADOR[prepost$ROCIADOR == "EDGAR TITO"] <- "EDGARD TITO"
prepost$ROCIADOR[prepost$ROCIADOR == "RAFAEL VALDIVIA"] <- "RAFAEL VALDIVIA RODRIGUEZ"


# le asignamos un id al brigadista
prepost$IdObserver <- unlist(0)
prepost$IdObserver <- as.numeric(factor(prepost$ROCIADOR))

# convertimos a Mayusculas JEFEVIV
prepost$JEFEVIV <- toupper(iconv(prepost$JEFEVIV, to="ASCII//TRANSLIT"))

# eliminando registros con 0 y sin vivienda
prepost <- prepost[prepost$UNICODE != "1.4.41.",] # 1 registro elimado
prepost <- prepost[prepost$UNICODE != "0.0.0.0",] # 2 registros eliminados
prepost <- prepost[prepost$FR_A != "   -",] # 
# prepost <- prepost[(prepost$UNICODE != "1.4.236.172" & prepost$PE_TRI != "0"),]
prepostna <- prepost[!(is.na(prepost$P)& !(is.na(prepost$D) & !(is.na(prepost$L)))), ] # 1 registro eliminado
# eliminando duplicados de la base general
prepost <- prepost[!duplicated(prepost),]

# -------------------------------------------------
# PRIMER CICLO DE ROCIADO CERRO COLORADO
# -------------------------------------------------
ciclo1_CC <- prepost[prepost$CICLO_ROCIADO==1 & prepost$D == 4,]
#UNICODE duplicados 1 ciclo
indice_dupli <- ciclo1_CC[which(duplicated(ciclo1_CC$UNICODE)),1]
dupli_c1CC<-ciclo1_CC[ciclo1_CC$UNICODE %in% indice_dupli,]
#Ordenar
dupli_c1CC <- dupli_c1CC[order(dupli_c1CC$UNICODE),]
duplicadosC1 <- ciclo1_CC[duplicated(ciclo1_CC),]


# -------------------------------------------------
# SEGUNDO CICLO DE ROCIADO CERRO COLORADO
# -------------------------------------------------
ciclo2_CC <- prepost[prepost$CICLO_ROCIADO==2 & prepost$D == 4,]
#UNICODE duplicados 2 ciclo
indice_dupli2 <- ciclo2_CC[which(duplicated(ciclo2_CC$UNICODE)),1]
dupli_c2CC<-ciclo2_CC[ciclo2_CC$UNICODE %in% indice_dupli2,]
#Ordenar
dupli_c2CC <- dupli_c2CC[order(dupli_c2CC$UNICODE),]
duplicadosC2 <- ciclo2_CC[duplicated(ciclo2_CC),]

# guardamos el archivo final como generalRociadoPA.csv
write.csv(prepost,"generalRociadoPA_Javier_28feb17.csv",row.names=FALSE)

