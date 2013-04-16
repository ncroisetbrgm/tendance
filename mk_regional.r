nom_sortie<-paste("mk_region_",nom_fichier_ssextension,sep="")

if (is.null(Data_test$CODE_BSS)) {Data_test$CODE_BSS<-Data_test$"Code national BSS"}
if (is.null(Data_test$DATE_DEBUT_PRELEVEMENT)) {Data_test$DATE_DEBUT_PRELEVEMENT<-Data_test$"Date prélèvement"}
if (is.null(Data_test$CODE_SIGNE)) {Data_test$CODE_SIGNE<-Data_test$"Code remarque analyse"}
if (is.null(Data_test$RESULTAT)) {Data_test$RESULTAT<-Data_test$"Résultat de l'analyse"}
if (is.null(Data_test$UNITE_SPATIALE)) {Data_test$UNITE_SPATIALE<-Data_test$"Unité spatiale"}

liste_MESO<-unique(Data_test$"UNITE_SPATIALE")
nb_MESO<-length(liste_MESO)


tauR<-vector("numeric",nb_MESO)
pvalR <-vector("numeric",nb_MESO)
slope_Sen<-vector("numeric",nb_MESO)


for (m in 1:nb_MESO) {

Data_test_MESO<-Data_test[which(Data_test$UNITE_SPATIALE==liste_MESO[m]),]


nb_anal<-length(Data_test_MESO$"CODE_BSS")

#Compte le nombre de points BSS
liste_pts<-unique(Data_test_MESO$"CODE_BSS")
nb_pts<-length(liste_pts)

Sprim<-0
Dprim<-0
varprim<-0
s<-0

for (i in 1:nb_pts){

    indic<-which(Data_test_MESO$"CODE_BSS"==liste_pts[i])

    donnees<-Data_test_MESO$"RESULTAT"[indic]
    date_an<-as.Date(Data_test_MESO$"DATE_DEBUT_PRELEVEMENT"[indic],format="%d/%m/%Y")
    date_an<-date_an[which(is.na(donnees)==FALSE)]
    donnees<-donnees[which(is.na(donnees)==FALSE)]

#    date_an<-as.Date(Data_test_MESO$"DATE_DEBUT_PRELEVEMENT"[indic],format="%d/%m/%Y %H:%M:%S")

    if (length(donnees)>=10&&length(unique(donnees))>1) {
       kend<-Kendall(donnees,as.numeric(date_an))
       S<-kend$S
       d<-kend$D
       v<-kend$varS
       Sprim<-Sprim+S
       Dprim<-Dprim+d
       varprim<-varprim+v
       donneesx<-outer(donnees,donnees,"-")
       datex<-outer(date_an,date_an,"-")
       z<-donneesx/matrix(as.numeric(datex),ncol=length(donnees))
       s<-c(s,z[lower.tri(z)])
       }
    }
tauR[m]<-as.numeric(Sprim/Dprim)
pvalR [m]<- 2 * (1 - pnorm(abs(as.numeric(Sprim/sqrt(varprim)))))
slope_Sen[m]<-median(s[-1],na.rm=TRUE)

}

table<-cbind(as.vector(liste_MESO),tauR,pvalR,slope_Sen*365)
colnames(table)<-c("Unité spatiale","Tau régional","pval régionale","Pente de Sen")
sortie<-write.table(table,file=paste(chemin,nom_sortie,".txt",sep=""),row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
