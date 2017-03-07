#################################
#
# Autor: Gian Franco modificado por Javier Quintanilla
#
# OBTENER DATOS DE DE LA BASE DE DATOS DE MINSA (FOXPRO)
######################

#-------------------------------------------------------------------
#============ Funcion CreateUnicode =========================================
#--------------------------------------------------------------------
  CreateUnicode<-function(data) {
    #Esta funcion retorna un data frame con los con el UNICODE
    #
    #ARGS
    # data = La base donde se desea obtener el UNICODE
    #
    #RETURNS
    # data = Con las columnas UNICODE, P, D, L, V al inicio de el dataframe
    #
    
    #Separando en "P", "D", "L" y "V"
      data$P<-substr(data$CODCNSLDD,1,1)
      data$D<-substr(data$CODCNSLDD,2,3)
      data$L<-substr(data$CODCNSLDD,4,6)
      data$V<-data$CODIGO
      
    #Eliminado espacios en blanco
      data$D <- gsub(" ","",data$D, fixed = T)
      data$L <- gsub(" ","",data$L, fixed = T)
      
    #Contruyendo UNICODE
      data$UNICODE<-paste(data$P, data$D, data$L, data$V, sep = ".")
    
    #Ordenando Las columnas
      n_col <- length(data)
      data <- data[, c(n_col, n_col-4,n_col-3,n_col-2,n_col-1, 1:(n_col-5) )]
    
    return(data)
  }
#=========================================================================

#install.packages("stringr") # SI SE REQUIERE INSTALAR PRIMERO
library(stringr)
 
#Ruta
  setwd('/home/javier/Documentos/javier/bd_minsa/MINSA_CONTROL')

#Obtenemos las bases que nos interesan
  t_dist<- read.csv("t_dist.csv") #contiene los códigos y nombre de distritos, formulacion de insecticida
  t_loc<- read.csv("t_loc.csv") #contiene nombre de localidades, codigo distrito, y nro de viv total de las loc
  t_roc<- read.csv("t_roc.csv") #contiene códigos y nombres de los rociadores
  t_mat<- read.csv("t_mat.csv") #contiene el código y el nombre de los materiales de las viviendas
  td_cnsldd<- read.csv("td_cnsldd.csv") #tabla de consolidación donde se encuentra la mayoría de la información
  th_cnsldd<- read.csv("th_cnsldd.csv") #contiene las fechas de rociado de las viviendas, ciclo de rociado 

#Eliminando inconsistencias en bases foxpro
  t_dist<- t_dist[!grepl("00015", t_dist$CODDIST, fixed = TRUE),]#Eliminamos duplicadado "Cerro Colorado"
  t_loc<- t_loc[!grepl("00194", t_loc$CODLOC, fixed = TRUE),]#Eliminamos "Huanca-Murco"
  t_loc<- t_loc[!grepl("00195", t_loc$CODLOC, fixed = TRUE),]#Eliminamos duplicadado "Canco"
  t_loc<- t_loc[!grepl("00196", t_loc$CODLOC, fixed = TRUE),]#Eliminamos "Huambo-Canca"
  t_roc<- t_roc[!(grepl('00081', t_roc$CODROC, fixed = TRUE) & grepl('Pepe Quintanilla', t_roc$ROCIADOR, fixed = TRUE)),]#Eliminamos codigo "00081" con "Pepe Quintanilla" ya que se repite con otro codigo igual
  t_roc<- t_roc[!grepl("Andy", t_roc$ROCIADOR, fixed = TRUE),]#Eliminamos "Andy Catacora" nunca rocea

# Reemplazando tildes, en nuestro caso al exportar la data de FOXPRO, corrigio las tildes asi que lo comentamos
#  t_dist$INSECTICID<-enc2utf8(t_dist$INSECTICID)
#  t_dist$INSECTICID<- gsub("<ed>","i",t_dist$INSECTICID)#reemplazando la vocal "i"
#  t_loc$LOCALIDAD<- enc2utf8(t_loc$LOCALIDAD)
#  t_loc$LOCALIDAD<- gsub("<e1>","a",t_loc$LOCALIDAD)#reemplazando "á" por "a"
#  t_loc$LOCALIDAD<- gsub("<e9>","e",t_loc$LOCALIDAD)#reemplazando "é" por "e"
#  t_loc$LOCALIDAD<- gsub("<ed>","i",t_loc$LOCALIDAD)#reemplazando "í" por "i"
#  t_loc$LOCALIDAD<- gsub("<f3>","o",t_loc$LOCALIDAD)#reemplazando "ó" por "o"
#  t_loc$LOCALIDAD<- gsub("<fa>","u",t_loc$LOCALIDAD)#reemplazando "ú" por "u"
#  t_loc$LOCALIDAD<- gsub("<f1>","ni",t_loc$LOCALIDAD)#reemplazando "ñ" por "ni"
#  t_roc$ROCIADOR<- enc2utf8(t_roc$ROCIADOR)
#  t_roc$ROCIADOR<- gsub("<e1>","a",t_roc$ROCIADOR)#reemplazando "á" por "a"
#  t_roc$ROCIADOR<- gsub("<e9>","e",t_roc$ROCIADOR)#reemplazando "é" por "e"
#  t_roc$ROCIADOR<- gsub("<ed>","i",t_roc$ROCIADOR)#reemplazando "í" por "i"
#  t_roc$ROCIADOR<- gsub("<f3>","o",t_roc$ROCIADOR)#reemplazando "ó" por "o"
#  t_roc$ROCIADOR<- gsub("<fa>","u",t_roc$ROCIADOR)#reemplazando "ú" por "u"
#  t_roc$ROCIADOR<- gsub("<f1>","ni",t_roc$ROCIADOR)#reemplazando "ñ" por "ni"
#  t_mat$MATERIAL<- enc2utf8(t_mat$MATERIAL)
#  t_mat$MATERIAL<- gsub("<f3>","o",t_mat$MATERIAL)#reemplazando "ó" por "o"
#  td_cnsldd$JEFEVIV<- enc2utf8(td_cnsldd$JEFEVIV)
  td_cnsldd$JEFEVIV<- gsub("Ă¡","a",td_cnsldd$JEFEVIV)#reemplazando "á" por "a"
  td_cnsldd$JEFEVIV<- gsub("Ă©","e",td_cnsldd$JEFEVIV)#reemplazando "é" por "e"  
  td_cnsldd$JEFEVIV<- gsub("Ă","i",td_cnsldd$JEFEVIV)#reemplazando "í" por "i"
  td_cnsldd$JEFEVIV<- gsub("Ă³>","o",td_cnsldd$JEFEVIV)#reemplazando "ó" por "o"
  td_cnsldd$JEFEVIV<- gsub("Ăº","u",td_cnsldd$JEFEVIV)#reemplazando "ú" por "u"
  td_cnsldd$JEFEVIV<- gsub("Ă±","ni",td_cnsldd$JEFEVIV)#reemplazando "ñ" por "ni"
  td_cnsldd$JEFEVIV<- gsub("á","a",td_cnsldd$JEFEVIV)#reemplazando "á" por "a"
  td_cnsldd$JEFEVIV<- gsub("é","e",td_cnsldd$JEFEVIV)#reemplazando "é" por "e"  
  td_cnsldd$JEFEVIV<- gsub("í","i",td_cnsldd$JEFEVIV)#reemplazando "í" por "i"
  td_cnsldd$JEFEVIV<- gsub("ó","o",td_cnsldd$JEFEVIV)#reemplazando "ó" por "o"
  td_cnsldd$JEFEVIV<- gsub("ú","u",td_cnsldd$JEFEVIV)#reemplazando "ú" por "u"
  td_cnsldd$JEFEVIV<- gsub("ñ","ni",td_cnsldd$JEFEVIV)#reemplazando "ñ" por "ni"
  
#  td_cnsldd$IN_MAT<- enc2/media/javier/343AE4C65DCD7C64/bd_minsautf8(td_cnsldd$IN_MAT)
#  td_cnsldd$IN_MAT<- gsub("<f3>","o",td_cnsldd$IN_MAT)#reemplazando "ó" por "o"
#  td_cnsldd$PE_MAT<- enc2utf8(td_cnsldd$PE_MAT)
#  td_cnsldd$PE_MAT<- gsub("<f3>","o",td_cnsldd$PE_MAT)#reemplazando "ó" por "o"

# Seleccionado solo columnas con datos
  distrito<- t_dist[,c("CODDIST","DISTRITO","CODIGO","CODPRV","CODNSTCD","INSECTICID")]
  localidad<- t_loc[,c("CODLOC","LOCALIDAD","CODIGO","NUMVIV","CODDIST","CODINT")]
  materiales<-t_mat[,c("CODMAT","MATERIAL")]
  rociador<-t_roc[,c("CODROC","ROCIADOR")]
  fecha_roc_minsa<-th_cnsldd[,c("NUMERO","CODCNSLDD","CODPRV","CODDIST","CODLOC","CODROC","CICLO","FECHA","CODNSTCD","SAVED","CODJEF","INICIO","FINAL")]
  cons_roc_minsa<-td_cnsldd[,c("CODCNSLDD","CODIGO","SEEKVIV","CODVIV","JEFEVIV","NUMRES","IN_AMB","IN_MAT","IN_GRI","IN_TRI","PE_AMB","PE_MAT","PE_GRI","PE_TRI","AN_TEC","AN_PAT","TRARES_T","TRARES_RR","CARGANSTCD","SUPER","NOTAS","NUMREC","IN_CODMAT","PE_CODMAT","CICLO")]

#---------------------------------------------------------------------#
#-------                fecha_roc_minsa                      ---------#
#---------------------------------------------------------------------#
  #Separando la fecha
    fecha_roc_minsa$FECHA<-as.character(fecha_roc_minsa$FECHA, format="%Y%m%d")
    fecha_roc_minsa$FR_A<-str_sub( fecha_roc_minsa$FECHA, -4,-1) 
    fecha_roc_minsa$FR_M<-str_sub( substr( fecha_roc_minsa$FECHA, 1,5 ),-2,-1)
    fecha_roc_minsa$FR_D<-str_sub( fecha_roc_minsa$FECHA, 1,2 ) 
    fecha_roc_minsa$FECHA<-paste(fecha_roc_minsa$FR_A,fecha_roc_minsa$FR_M,fecha_roc_minsa$FR_D,sep="-")
    
  #Merge entre "fecha_roc_minsa" y "rociador"
    fecha<- merge(fecha_roc_minsa,rociador, by = "CODROC", all.x = T)
    fecha<-fecha[,c(3,8,14:16,1,17,12,13)]

#---------------------------------------------------------------------#
#--------                  cons_roc_minsa                    ---------#
#---------------------------------------------------------------------#
  #Se selecciona solo los que tienen "0" los demas son sumatorias de este.
    cons_roc_minsa<-cons_roc_minsa[0==cons_roc_minsa$NUMREC,]
  
  #Separando en "P", "D", "L" y "V"
    cons_roc_minsa$P<-substr(cons_roc_minsa$CODCNSLDD,1,1)
    cons_roc_minsa$D<-substr(cons_roc_minsa$CODCNSLDD,2,3)
    cons_roc_minsa$L<-substr(cons_roc_minsa$CODCNSLDD,4,6)
    cons_roc_minsa$V<-cons_roc_minsa$CODIGO
  
  #Eliminado espacios en blanco
    cons_roc_minsa$D <- gsub(" ","",cons_roc_minsa$D, fixed = T)
    cons_roc_minsa$L <- gsub(" ","",cons_roc_minsa$L, fixed = T)
    cons_roc_minsa$V <- gsub(" ","",cons_roc_minsa$V, fixed = T)
    cons_roc_minsa$V <- gsub("-","",cons_roc_minsa$V, fixed = T) # eliminando guion de vivienda en adicionadas "-"
    
  #Contruyendo UNICODE
    cons_roc_minsa$UNICODE<-paste(cons_roc_minsa$P, cons_roc_minsa$D, cons_roc_minsa$L, cons_roc_minsa$V, sep = ".")
  
  #Completando ciclos de rociado I y II
    cons_roc_minsa$CICLO_ROCIADO<-substr(cons_roc_minsa$CODCNSLDD,8,8)#comprobado que salen bien
  
  #Creando columna "MAS20TRI_IN" y "MAS20TRI_PE"
    cons_roc_minsa$MAS20TRI_IN<- unlist(0)
    cons_roc_minsa$MAS20TRI_PE<- unlist(0)
    cons_roc_minsa[grepl("+",cons_roc_minsa$IN_TRI, fixed = TRUE),"MAS20TRI_IN"]<- 1
    cons_roc_minsa[grepl("+",cons_roc_minsa$PE_TRI, fixed = TRUE),"MAS20TRI_PE"]<- 1
    
  #Creando columnas de materiales en el INTRA y asignando su valor
    cons_roc_minsa$IN_SIL<- unlist(0)
    cons_roc_minsa$IN_NOB<- unlist(0)
    cons_roc_minsa$IN_LAD<- unlist(0)
    cons_roc_minsa$IN_ADO<- unlist(0)
    cons_roc_minsa$IN_BLOQ<- unlist(0)
    cons_roc_minsa[grepl("Sillar",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_SIL"]<- 1
    cons_roc_minsa[grepl("Noble",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_NOB"]<- 1
    cons_roc_minsa[grepl("Ladrillo",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_LAD"]<- 1
    cons_roc_minsa[grepl("Adobe",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_ADO"]<- 1
    cons_roc_minsa[grepl("Bloqueta",cons_roc_minsa$IN_MAT, fixed = TRUE),"IN_BLOQ"]<- 1
    
  #Creando columnas de materiales en el PERI y asignando su valor
    cons_roc_minsa$PE_SIL<- unlist(0)
    cons_roc_minsa$PE_NOB<- unlist(0)
    cons_roc_minsa$PE_LAD<- unlist(0)
    cons_roc_minsa$PE_ADO<- unlist(0)
    cons_roc_minsa$PE_BLOQ<- unlist(0)
    cons_roc_minsa$PE_PIE<- unlist(0)
    cons_roc_minsa[grepl("Sillar",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_SIL"]<- 1
    cons_roc_minsa[grepl("Noble",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_NOB"]<- 1
    cons_roc_minsa[grepl("Ladrillo",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_LAD"]<- 1
    cons_roc_minsa[grepl("Adobe",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_ADO"]<- 1
    cons_roc_minsa[grepl("Bloqueta",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_BLOQ"]<- 1
    cons_roc_minsa[grepl("Piedra",cons_roc_minsa$PE_MAT, fixed = TRUE),"PE_PIE"]<- 1
    
  #Creando columnas de animales en el TECHO y asignando su valor
    cons_roc_minsa$TEC_CUY<- unlist(0)
    cons_roc_minsa$TEC_CON<- unlist(0)
    cons_roc_minsa$TEC_OVE<- unlist(0)
    cons_roc_minsa$TEC_PER<- unlist(0)
    cons_roc_minsa$TEC_AVE<- unlist(0)
    cons_roc_minsa$TEC_GAT<- unlist(0)
    cons_roc_minsa[grepl("Cuyes",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_CUY"]<- 1
    cons_roc_minsa[grepl("Conejos",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_CON"]<- 1
    cons_roc_minsa[grepl("Ovejas",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_OVE"]<- 1
    cons_roc_minsa[grepl("Perros",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_PER"]<- 1
    cons_roc_minsa[grepl("Aves",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_AVE"]<- 1
    cons_roc_minsa[grepl("Gatos",cons_roc_minsa$AN_TEC, fixed = TRUE),"TEC_GAT"]<- 1
    
  #Creando columnas de animales en el PATIO y asignando su valor
    cons_roc_minsa$PAT_CUY<- unlist(0)
    cons_roc_minsa$PAT_CON<- unlist(0)
    cons_roc_minsa$PAT_OVE<- unlist(0)
    cons_roc_minsa$PAT_PER<- unlist(0)
    cons_roc_minsa$PAT_AVE<- unlist(0)
    cons_roc_minsa$PAT_GAT<- unlist(0)
    cons_roc_minsa[grepl("Cuyes",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_CUY"]<- 1
    cons_roc_minsa[grepl("Conejos",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_CON"]<- 1
    cons_roc_minsa[grepl("Ovejas",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_OVE"]<- 1
    cons_roc_minsa[grepl("Perros",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_PER"]<- 1
    cons_roc_minsa[grepl("Aves",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_AVE"]<- 1
    cons_roc_minsa[grepl("Gatos",cons_roc_minsa$AN_PAT, fixed = TRUE),"PAT_GAT"]<- 1

#CONSOLIDADO FINAL
  CONS_ROCIADO_2009_2015<- merge(cons_roc_minsa, fecha, all.x = TRUE)
  CONS_ROCIADO_2009_2015<- CONS_ROCIADO_2009_2015[,c(30,26:29,57:60,62,5:8,34:38,9,10,32,11,12,39:44,13,14,33,15,45:50,16,51:56,31,19:21)]# Tener cuidado cuando las tablas cambien de tamanio ya que esta en numeros

#Obteniendo solo las filas que son unicas, eliminando duplicados de filas
  CONS_ROCIADO_2009_2015<- unique(CONS_ROCIADO_2009_2015)
  
#--------------------------------------------------------------------------------------
# Solo que tengan las columnas UNICODE, CICLO y STATUS
#--------------------------------------------------------------------------------------
  setwd('/home/javier/Documentos/javier/bd_minsa/MINSA_CONTROL')
  
  #Cerradas 
    tdc_cnsldd<- read.csv("tdc_cnsldd.csv")
  #Renuente
    tdr_cnsldd<- read.csv("tdr_cnsldd.csv")
  #Deshabitada 
    tdd_cnsldd<- read.csv("tdd_cnsldd.csv")
  #Local publico
    tdp_cnsldd<- read.csv("tdp_cnsldd.csv")
  #Lote vacio
    tdv_cnsldd<- read.csv("tdv_cnsldd.csv")
  
  #Utilizando la funcion CreateUnicode, para obtener los UNICODEs
    tdc_cnsldd <- CreateUnicode(tdc_cnsldd)
    tdr_cnsldd <- CreateUnicode(tdr_cnsldd)
    tdd_cnsldd <- CreateUnicode(tdd_cnsldd)
    tdp_cnsldd <- CreateUnicode(tdp_cnsldd)
    tdv_cnsldd <- CreateUnicode(tdv_cnsldd)

  # Obteniendo el ciclo de rociado real sacado de la columna CODCNSLDD
    tdc_cnsldd$CICLO_ROCIADO<-substr(tdc_cnsldd$CODCNSLDD,8,8)
    tdr_cnsldd$CICLO_ROCIADO<-substr(tdr_cnsldd$CODCNSLDD,8,8)
    tdd_cnsldd$CICLO_ROCIADO<-substr(tdd_cnsldd$CODCNSLDD,8,8)
    tdp_cnsldd$CICLO_ROCIADO<-substr(tdp_cnsldd$CODCNSLDD,8,8)
    tdv_cnsldd$CICLO_ROCIADO<-substr(tdv_cnsldd$CODCNSLDD,8,8)
  
  #Creando una columna STATUS
    tdc_cnsldd$STATUS <- unlist("C")
    tdr_cnsldd$STATUS <- unlist("R")
    tdd_cnsldd$STATUS <- unlist("DES")
    tdp_cnsldd$STATUS <- unlist("LP")
    tdv_cnsldd$STATUS <- unlist("LV")
    
  #Obteniendo solo columna que necesito
    tdc_cnsldd <- tdc_cnsldd[, c(1:5,11,12)]
    tdr_cnsldd <- tdr_cnsldd[, c(1:5,11,12)]
    tdd_cnsldd <- tdd_cnsldd[, c(1:5,11,12)]
    tdp_cnsldd <- tdp_cnsldd[, c(1:5,11,12)]
    tdv_cnsldd <- tdv_cnsldd[, c(1:5,11,12)]
    
  #Juntando 
    C_R <- merge(tdc_cnsldd,tdr_cnsldd, all = TRUE)
    C_R_D <- merge(C_R,tdd_cnsldd, all = TRUE)
    C_R_D_LP <- merge(C_R_D,tdp_cnsldd, all = TRUE)
    C_R_D_LP_LV <-merge(C_R_D_LP,tdv_cnsldd, all = TRUE)
  
#UNIENDO CON LOS REGISTROS OBTENIDOS DE LA BASE DEL MINSA 
    cerrdas<-read.csv("~/Documentos/javier/bd_minsa/MINSA_CONTROL/CERRADAS_MINSA.csv")
    C_R_D_LP_LV_1<-rbind(C_R_D_LP_LV,cerrdas)
    
  #Obteniendo registros unicos
    C_R_D_LP_LV <- unique(C_R_D_LP_LV) # Todos los distritos de Arequipa
    C_R_D_LP_LV_1 <- unique(C_R_D_LP_LV_1)# Aumenatando  casas cerradas y renuentes de MINSA (FEB 2017)

  #Aplicando funcion filtro para obtener solo los datos del distrito ASA
   # statusMinsa<-filtro(C_R_D_LP_LV,dentro)
    statusMinsa <- C_R_D_LP_LV
    statusMinsa <- C_R_D_LP_LV_1
  #Juntando con las tratadas    
    tratadas <- CONS_ROCIADO_2009_2015
    tratadas$STATUS <- unlist("T")
    tratadas <- tratadas[,c(1:5,21,32,48,52)]
    tratadas <- unique(tratadas)
    
  #Merge
   # statusMinsaASA <- merge(statusMinsa, tratadas, all = TRUE)
    statusMinsa <- merge(statusMinsa, tratadas, all = TRUE)
#----------------------------------------------------------------------------------------
# Imprimiendo datos
#----------------------------------------------------------------------------------------
#  setwd("/home/gianfranco/Documentos/github/Participation/bd_minsa/resultados")
#  write.csv(CONS_ROCIADO_2009_2015,'CONS_ROCIADO_2009_2015.csv')
#  write.csv(statusMinsaASA,'statusMinsaASA.csv')
#  write.csv(tratadas_ASA,'tratadas_ASA.csv')

    #Guardando informacion en una carpeta aparte
    setwd("/home/javier/Documentos/javier/bd_minsa/MINSA TOTAL PREPOST/resultados")
    write.csv(CONS_ROCIADO_2009_2015,"cons_roc_2009_2015_CLAUDIA.csv", row.names = F)
    write.csv(C_R_D_LP_LV_1, "viv_no_roc_CLAU.csv", row.names = F) # archivo que servira para mas adelante
    write.csv(statusMinsa,"statusMinsa2009_2015_CLAU",row.names = F) # archivo con Cerradas, etc y Tratadas
    