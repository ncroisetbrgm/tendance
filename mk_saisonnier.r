nom_sortie<-paste("mk_saison_",nom_fichier,sep="")

if (is.null(Data_test$CODE_BSS)) {Data_test$CODE_BSS<-Data_test$"Code national BSS"}
if (is.null(Data_test$DATE_DEBUT_PRELEVEMENT)) {Data_test$DATE_DEBUT_PRELEVEMENT<-Data_test$"Date prélèvement"}
if (is.null(Data_test$CODE_SIGNE)) {Data_test$CODE_SIGNE<-Data_test$"Code remarque analyse"}
if (is.null(Data_test$RESULTAT)) {Data_test$RESULTAT<-Data_test$"Résultat de l'analyse"}

#Compte le nombre de points BSS
liste_pts<-unique(Data_test$CODE_BSS)
nb_pts<-length(liste_pts)

tauS_trimes<-vector("numeric",nb_pts)
pvalS_trimes<-vector("numeric",nb_pts)
tauS_mens<-vector("numeric",nb_pts)
pvalS_mens<-vector("numeric",nb_pts)
nbr_anal<-vector("numeric",nb_pts)
slope_Sen_trimes<-vector("numeric",nb_pts)
slope_Sen_mens<-vector("numeric",nb_pts)


for (i in 1:nb_pts){

    indic<-which(Data_test$CODE_BSS==liste_pts[i])

    #Création des vecteurs donnees/date_an/codesigne et suppression des analyses non faites donnees=NA
    date_an<-as.Date(Data_test$DATE_DEBUT_PRELEVEMENT[indic],format="%d/%m/%Y")
    donnees<-Data_test$RESULTAT[indic]
    date_an<-date_an[which(is.na(donnees)==FALSE)]
    codesigne<-Data_test$CODE_SIGNE[indic]
    codesigne<-codesigne[which(is.na(donnees)==FALSE)]
    donnees<-donnees[which(is.na(donnees)==FALSE)]
    nbr_anal[i]<-length(donnees)
    if (nbr_anal[i]==0) {next}

    #Calcul de la corrélation entre les concentrations et la saison (trimestre)
    date_saison<-months(date_an)
    date_saison<-ifelse(date_saison=="janvier"|date_saison=="février"|date_saison=="mars","hiver",date_saison)
    date_saison<-ifelse(date_saison=="avril"|date_saison=="mai"|date_saison=="juin","printemps",date_saison)
    date_saison<-ifelse(date_saison=="juillet"|date_saison=="août"|date_saison=="septembre","été",date_saison)
    date_saison<-ifelse(date_saison=="octobre"|date_saison=="novembre"|date_saison=="décembre","automne",date_saison)

    Sprim<-0
    Dprim<-0
    varprim<-0
    s<-0
    for (k in 1:length(unique(date_saison))) {
        donnees_sais<-donnees[which(date_saison==unique(date_saison)[k])]
        date_an_sais<-date_an[which(date_saison==unique(date_saison)[k])]
        if (length(donnees_sais)>3&&length(unique(donnees_sais))>1&&length(unique(date_an_sais))>1) {
           kend<-Kendall(donnees_sais,as.numeric(date_an_sais))
           S<-kend$S
           d<-kend$D
           v<-kend$varS
           Sprim<-Sprim+S
           Dprim<-Dprim+d
           varprim<-varprim+v
           donneesx<-outer(donnees_sais,donnees_sais,"-")
           datex<-outer(date_an_sais,date_an_sais,"-")
           z<-donneesx/matrix(as.numeric(datex),ncol=length(donnees_sais))
           s<-c(s,z[lower.tri(z)])
           }
        }
    tauS_trimes[i]<-as.numeric(Sprim/Dprim)
    pvalS_trimes[i] <- 2 * (1 - pnorm(abs(as.numeric(Sprim/sqrt(varprim)))))
    slope_Sen_trimes[i]<-median(s[-1],na.rm=TRUE)


    Sprim<-0
    Dprim<-0
    varprim<-0
    date_saison<-months(date_an)

    for (k in 1:length(unique(date_saison))) {
        donnees_sais<-donnees[which(date_saison==unique(date_saison)[k])]
        date_an_sais<-date_an[which(date_saison==unique(date_saison)[k])]
        if (length(donnees_sais)>3&&length(unique(donnees_sais))>1&&length(unique(date_an_sais))>1) {
           kend<-Kendall(donnees_sais,as.numeric(date_an_sais))
           S<-kend$S
           d<-kend$D
           v<-kend$varS
           Sprim<-Sprim+S
           Dprim<-Dprim+d
           varprim<-varprim+v
           donneesx<-outer(donnees_sais,donnees_sais,"-")
           datex<-outer(date_an_sais,date_an_sais,"-")
           z<-donneesx/matrix(as.numeric(datex),ncol=length(donnees_sais))
           s<-c(s,z[lower.tri(z)])
           }
        }
    tauS_mens[i]<-as.numeric(Sprim/Dprim)
    pvalS_mens[i] <- 2 * (1 - pnorm(abs(as.numeric(Sprim/sqrt(varprim)))))
    slope_Sen_mens[i]<-median(s[-1],na.rm=TRUE)
    }


table<-cbind(as.vector(liste_pts),tauS_trimes,pvalS_trimes,slope_Sen_trimes*365,tauS_mens,pvalS_mens,slope_Sen_mens*365)
colnames(table)<-c("CODE_BSS","Tau trimestre","pval trimestre","Pente de Sen trimestre","taus mens","pval mens","Pente de Sen mens")
sortie<-write.table(table,file=paste(chemin,nom_sortie,sep=""),row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
