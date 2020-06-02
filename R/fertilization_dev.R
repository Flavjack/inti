# #--------------developer bucles------------------
#
# #calculo de fertilizantes compuestos,
# #issue description.
# # un plan de fertilizacion es una operacion engorrosa, y útil para los calculos de nutrición mineral que requierem
# # las plantas, los cuales deben de hacerce conjuntamente con los analisis de suelo, foliar, cosecha extraida y agua
# # estos minerales deben ser repuestos para evitar reducir la fertilidad del suelo en detrimento de la agricultura.
# # la complicacion surge cuando se usan fuentes mienrales NO puras, es decir cuando se ingrese N adicionalmente
# # ya estamos ingresando P2O5 ó K2O.Este debe tener tal equilibrio que no debe superar la demanda del cultivo,
# # para evitar gastos inecesarios y contaminación.
#
# # --------------------------------ETAPA1: requerimientos de la plantas---------------------------------------------
#
# Nitrogeno <-0        #En su forma de N
# Phosphor <-250          #En su forma de P2O5
# Potassium <-0         #En su forma de K2O
# Calcium <-80            #  Mg<-80; Zn<-0;  B<-10;  Cu<-0
#
# need_crop<- c(  Nitrogeno,
#                 Phosphor,
#                 Potassium,
#                 Calcium )
#
# need_cropname <- c("N", "P2O5", "K2O", "Ca")
#
# for(i in 1:length(need_crop)){
#   if( need_crop[i]==0 ){
#
#     q  <- i
#
#
#   }else{
#
#   }
#
# }
#
#
#
# #-------------------------------ETAPA2: ingresar los fertilizantes %-----------------------------------------
# # Construir la matriz con los fertilizantes. c( N, P2O, k2O, Ca )
# #ingrese los fertilizantes que cubriran la necesidades del cultivo -> need_crop
#
# f1 <- c(0.1, 0, 0.4, 0.05)
# f2 <- c(0 ,0.1, 0.1, 0)
# f3 <- c(0.3, 0.1, 0.1,0)
# f4 <- c(0.5, 0, 0, 0.05) #posible mejora con 'Declarations'
#
# #contruir matriz
# mymatrix <- c(f1,
#               f2,
#               f3,
#               f4,
#               need_crop)
# #matriz de los fertilizantes
# need_fert <- matrix( data=mymatrix,  ncol= length(need_crop), byrow = T  )
# colnames(need_fert)<-paste(need_cropname,sep='')
#
#
# #------------------------------ETAPA3: Filtrar por elementos signficantes
#
# # fertilizantes que requerimos para el plan de fertilizacion
# nc.1 <- need_cropname[need_crop>0] ;nc.1
#
# nc.0 <- need_cropname[need_crop<=0] ;nc.0
#
# need_fert[,nc.0[1]] #llamar a los elementos nitrogendos
# #n_c.l<-which(!is.na(need_crop[need_crop>0]));n_c.l
# #ubicacion de los elementos need_crop
#
# #PC1 :  1er punto de control
# #if(pc1!=0){
#
#
#
#
#
#
#
# A2<-order(A1, na.last = TRUE, decreasing = FALSE,
#           method = c("shell", "radix"))
# # A2 ubicacion elementos  ORDENADO CRECIENTEMENTE
#
# #1) inicio    ORDENAMIENTO DE LOS ELEMENTOS#
# for (i in A2) {
#   A3<-A1[A2]
#   comment(A3)<-'A3 : Elementos ordenados segun A2'
#   A4<-A3[A3>0]
#   # A4 CON LAS EXIGENCIAS SIGNIFICATIVAS
#   comment(A4)<-'A4 : Exigencias significativas'
#   A5<-A3[A3<=0]
#   # A5 EXIGENCIAS NO SIGNIFICATIVAS
#   comment(A5)<-'A5 : Exigencias NO signifcativas'
#   nombre<-c("Nitrogeno","Fosforo","Potasio","Calcio","Magnesio","Zinc","Boro","Cobre")
#   comment(nombre)<-'nombre : Nombre de los elementos seg?n orden A1'
#   l1<-length(A1); comment(l1)<-'l1 : longitud de A1'
#   l4<-length(A4); comment(l4)<-'l4 : longitud de A4'
#   l5<-length(A5); comment(l5)<-'l5 : longitud de A5'
#   l1
#   n_<-nombre[A2];
#   comment(n_)<-'n_ : Nombre de los elementos  ordenados de manera creciente segun A2'
#
#   nombre_Ordenado<-0   #  declarar arreglo = 0, indica guardar n?meros
#   comment(nombre_Ordenado)<-'nombre_Ordenado : nombre en orden A2 de las exigencias SIGNIFICATIVAS'
# }                           # A3 elemento ORDENADO CRECIENTEMENTE
# #1final   ORDENAMIENTO DE LOS ELEMENTOS#
#
# #2) inicio   BUCLE QUE FINALIZA LOS CU?LCULOS O MUESTRA NOMBRE DE LOS ELEMENTOS SIGNIFICATIVOS DE MANERA CRECIENTE#
# if((l1-l5)==0){
#   print("C?lculo terminado")
# }else{
#   for (i in (l5+1):l1 ) {
#     nombre_Ordenado[i-l5]<-c(nombre[A2[i]])
#   }
# }
# comment(nombre_Ordenado)<-'Nombre de las exigenicas significativas en orden creciente'
# #2final   BUCLE QUE FINALIZA LOS CU?LCULOS O MUESTRA NOMBRE DE LOS ELEMENTOS SIGNIFICATIVOS DE MANERA CRECIENTE#
#
# paste("Por favor, 1ero cubrir las exigencias de ",nombre_Ordenado[1],"  ",A4[1],"kg, ingrese ley: ")
#
# #3) inicio BUCLE PARA HALLAR EL      e
# ubic<-NA # declarar NA porque solo se desea que ubique un numero natural
# for(f in 1:l1){
#   switch (nombre[f]==nombre_Ordenado[1],ubic[f]<-f)
#
#
#   nombre_ubic<-which(!is.na(ubic));comment(nombre_ubic)<-'nombre_ubic : muestra la posici?n 1er elemento a cubrir'
#   e<-nombre_ubic;
#
# }
# #3final del bucle para hallar     e
#
#
# #4)  inicio  C?LCULO DE q y q1
# for(s in 1:l1){
#   q1<-NA
#   q<-0
#   if(A1[s]<0 | ley1[s]<0){
#     if(A1[s]<0){
#       q1<-c(paste("Corregir la EXIGENCIA del ",nombre[s]," este No puede ser negativo"))
#       break
#     }else{
#       q1<-c(paste("Corregir la LEY del ", nombre[s]," este No puede ser negativo"))
#       break
#     }
#
#   }
#   if(A1[s]==0 & ley1[s]>0){
#     q1<-c(paste("Contaminacion por ",nombre[s]))
#   }
#   if(A1[e]>0 & ley1[e]==0 ){
#     q1<-c(paste("Cambie de fertilizante,  No cuenta con  ",nombre[e]," dentro de la ley"))
#   }else{
#     q<-A1[e]/ley1[e]; q1<-paste(q," Kg del fertilizante cubre las exigencias de ", nombre[e])
#     comment(q)<-'A1[e]/ley1[e]'
#   }
# }
# q1
# #4 final  C?LCULO DE q y q1
#
#
# #5) Bucle para saber si la nueva exigencia (A1) se hizo negativo
# Q1<-ley1[-e]*q;Q1
# comment(Q1)<- 'Q1 = ley1[-e] * q1'
# p1<-A1[-e]-Q1; comment(p1)<-' p1 : A1[-e] -  Q1'
#
# if(length(p1[p1<0])!=0){
#   re<-c("contaminacion por el fertilizante empleado")
# }else{
#   re<-c("hasta aqu?, los c?lculos son los correctos")
# }
# re
# #####################################################
# n_n<-NA
# nombrq<-nombre[-e];comment(nombrq)<-'nombreq : nombre[-e]'
# for(i in 1:length(p1)){
#   if(p1[i]==0){
#     n_n[i]<-paste(nombrq[i],"cubierto.")
#   }
#   if (p1[i]<0){
#     n_n[i]<-paste("Ocurre contaminaci?n por ", nombrq[i]," escoja otro fertilizante.")
#   }
#   if(p1[i]>0){
#     n_n[i]<-paste("NO sobrepasa las exigencias de ", nombrq[i],".")
#
#   }
# }
# n_n
# comment(n_n)<-'n_n : Despu?s de hacer los c?lculos, verifica la situacion de las exigencias'
