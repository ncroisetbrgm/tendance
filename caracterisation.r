nom_sortie<-paste("caract_",nom_fichier_ssextension,sep="")

print("Voulez-vous une représentation graphique des données ? Tapez oui ou non.")
graphiques<-scan( what="",nlines=1)

if (is.null(try(write.table(0,file=paste(chemin,nom_sortie,".txt",sep="")),silent=TRUE))==FALSE) {stop("Le tableau de résultat est ouvert. Fermez-le et relancer le script.")}

while (is.null(dev.list())==FALSE) {dev.off()}
if (graphiques=="oui") {
pdf(paste(chemin,"graphes_",nom_sortie,".pdf", sep=""))
layout (matrix(c(1,2,3,2),2,2),widths=c(2,1),heights=c(3,1))
}


#Compte le nombre de points BSS
if (is.null(Data_test$CODE_BSS)) {Data_test$CODE_BSS<-Data_test$"Code national BSS"}
if (is.null(Data_test$DATE_DEBUT_PRELEVEMENT)) {Data_test$DATE_DEBUT_PRELEVEMENT<-Data_test$"Date prélèvement"}
if (is.null(Data_test$CODE_SIGNE)) {Data_test$CODE_SIGNE<-Data_test$"Code remarque analyse"}
if (is.null(Data_test$RESULTAT)) {Data_test$RESULTAT<-Data_test$"Résultat de l'analyse"}

#liste_pts<-unique(Data_test$"Code national BSS")} else {
   liste_pts<-unique(Data_test$CODE_BSS)
#    }
nb_pts<-length(liste_pts)

#Initialisation
date_min<-vector("numeric",nb_pts)
class(date_min)<-"Date"
date_max<-vector("numeric",nb_pts)
class(date_max)<-"Date"
nbr_anal<-vector("numeric",nb_pts)
long<-vector("numeric",nb_pts)
moyenne<-vector("numeric",nb_pts)
mediane<-vector("numeric",nb_pts)
percentil1<-vector("numeric",nb_pts)
percentil2<-vector("numeric",nb_pts)
taux_quanti<-vector("numeric",nb_pts)
ecarttype<-vector("numeric",nb_pts)
pval_shapi<-vector("numeric",nb_pts)
normalite<-vector("character",nb_pts)
LQmin<-vector("numeric",nb_pts)
LQmax<-vector("numeric",nb_pts)
LQmin2<-vector("numeric",nb_pts)
LQmax2<-vector("numeric",nb_pts)
med_ecarts<-vector("numeric",nb_pts)
ec_typ_ecarts<-vector("numeric",nb_pts)
moy_ecarts2<-vector("numeric",nb_pts)
ec_typ_ecarts2<-vector("numeric",nb_pts)
moy_ecarts<-vector("numeric",nb_pts)
autocorr_r1<-vector("numeric",nb_pts)
signif_autocorr<-vector("numeric",nb_pts)
text_med<-vector("character",nb_pts)
text_perc1<-vector("character",nb_pts)
text_perc2<-vector("character",nb_pts)


if (is.null(Data_test$UNITE_GRAPH)&&is.null(Data_test$"Unité du graphique")) {
   if (is.null(Data_test$UNITE)) {Data_test$UNITE<-Data_test$"Unité"}

   tab_Unite<-read.table("Unite_SANDRE.txt",comment.char="",
   header=FALSE, sep=";", row.names=NULL,na.strings="", dec=".", strip.white=FALSE,quote="")

   vect_unite<-Data_test$UNITE
   if (length(unique(vect_unite))==1) {

      unite<-Data_test$UNITE[1]
      unite<-tab_Unite[which(tab_Unite[2]==as.character(unite)),3]
     } else {
     stop("Attention, les unités ne sont pas toutes identiques.")}
} else {
   if (is.null(Data_test$UNITE_GRAPH)) {Data_test$UNITE_GRAPH<-Data_test$"Unité du graphique"}
   vect_unite<-Data_test$UNITE_GRAPH
   if (length(unique(vect_unite))==1) {
      unite<-Data_test$UNITE_GRAPH[1]
     } else {
     stop("Attention, les unités ne sont pas toutes identiques.") }

}

test<-0
if (is.null(Data_test$CODE_PARAMETRE)) {test=1}

if (test==1) {
text_legende="Concentration"
} else {
param<-Data_test$CODE_PARAMETRE[1]
if (param==1301) text_legende<-"Température" else if (param==1302) text_legende<-"pH" else if (param==1303) text_legende<-"Conductivité à 25°" else if(param==1330) text_legende<-"Potentiel d'oxydo-réduction (Eh)" else if(param==1311) text_legende<-"Oxygène dissous" else if (param==1304) text_legende<-"Conductivité à 20°" else text_legende<-"Concentration"
}

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

    date_min[i]<-min(date_an)
    date_max[i]<-max(date_an)

    #Calcul de la longueur de la chronique
    long[i]<- date_max[i]-date_min[i]

    #Moyennes/déciles/ecart-type
    #Pour le calcul de la moyenne on remplace les <LQ par LQ/2
    donneesm<-donnees
    donneesm[which(codesigne!=1)]<-donneesm[which(codesigne!=1)]/2
    moyenne[i]<-mean(donneesm,na.rm=TRUE)
    mediane[i]<-median(donnees,na.rm=TRUE)
    ecarttype[i]<-sd(donnees,na.rm=TRUE)*(nbr_anal[i]-1)/nbr_anal[i]
    percentil1[i]<-quantile(donnees,probs=0.1,na.rm=TRUE)
    percentil2[i]<-quantile(donnees,probs=0.9,na.rm=TRUE)


    #Taux de quantification - LQs

    taux_quanti[i]<-length(which(codesigne==1))/nbr_anal[i]
    if (length(donnees[which(codesigne!=1)])==0) {
    LQmin[i]<-NA
    LQmax[i]<-NA
    } else {
    LQmin[i]<-min(donnees[which(codesigne!=1)])
    LQmax[i]<-max(donnees[which(codesigne!=1)])
    }
    if (length(donnees[which(codesigne==10)])==0) {
    LQmin2[i]<-NA
    LQmax2[i]<-NA
    } else {
    LQmin2[i]<-min(donnees[which(codesigne==10)])
    LQmax2[i]<-max(donnees[which(codesigne==10)])
    }
    #Remarque dans le cas où mediane/decile<LQ
    if(LQmax[i]>mediane[i]&&is.na(LQmax[i])==FALSE&&is.na(mediane[i])==FALSE)text_med[i]<-"Attention : la valeur de la médiane est inférieure à la limite de quantification maximale"
    if(LQmax[i]>percentil1[i]&&is.na(LQmax[i])==FALSE&&is.na(percentil1[i])==FALSE)text_perc1[i]<-"Attention : la valeur du premier décile est inférieure à la limite de quantification maximale"
    if(LQmax[i]>percentil2[i]&&is.na(LQmax[i])==FALSE&&is.na(percentil2[i])==FALSE)text_perc2[i]<-"Attention : la valeur du dernier décile est inférieure à la limite de quantification maximale"


    #Calcul de la moyenne et de l'écart-type des écarts entre deux dates d'analyses
    dates_triees<-sort(date_an)
    ecarts<-diff(dates_triees)
    moy_ecarts[i]<-mean(ecarts)
    med_ecarts[i]<-median(ecarts)
    ec_typ_ecarts[i]<-sd(ecarts)

    #identification d'outliers dans la fréquence de prélèvements
    if (length(which(ecarts>=med_ecarts[i]+2* ec_typ_ecarts[i]))!=0) {
       moy_ecarts2[i]<-mean(ecarts[-which(ecarts>=med_ecarts[i]+2* ec_typ_ecarts[i])])
       ec_typ_ecarts2[i]<-sd(ecarts[-which(ecarts>=med_ecarts[i]+2* ec_typ_ecarts[i])])
    }
    else {moy_ecarts2[i]<-moy_ecarts[i]
          ec_typ_ecarts2[i]<- ec_typ_ecarts[i] }


    #Calcul de la normalité. Test de Shapiro.
          if (nbr_anal[i]>=3) {
                              if (length(unique(donnees))!=1) {
                              shapishapo<-shapiro.test(donnees)
                              pval_shapi[i]<-shapishapo$p.value
                              if (pval_shapi[i]>=0.05|is.na(pval_shapi[i])) {normalite[i]<-"Un test de Shapiro-Wilke a été appliqué. Les données de cette chronique sont normalement distribuées"}
                              else {normalite[i]<-"Un test de Shapiro-Wilke a été appliqué. Les données de cette chronique ne sont pas normalement distribuées"}
                              }
                              else {
                              pval_shapi[i]<-NA
                              normalite[i]<-"Toutes les valeurs sont identiques. Le test de Shapiro-Wilke n'a pas été appliqué."}
                              }
         else {pval_shapi[i]<-NA
         normalite[i]<-"Il n'y a pas assez de données pour estimer la normalité de la distribution"}


    #Autocorrélation des données

    #Tri des données par ordre chronologique
    donnees<-donnees[order(date_an)]

    autocorr<-acf(donnees,type="correlation",plot=FALSE,na.action=na.pass)
    if (length(autocorr$acf)>1&&is.na(autocorr$acf[2])==FALSE) {
    
    #Autocorrélation au rang 1
    autocorr_r1[i]<-autocorr$acf[2]

    #Significativité de l'autocorrélation au seuil de 5%
    seuil<-qnorm((1 + 0.95)/2)/sqrt(autocorr$n.used)
    if(abs(autocorr_r1[i])>abs(seuil)) {signif_autocorr[i]<-"La probabilité que les données soient autocorrélées au rang 1 est supérieure à 95%"}
                                 else {signif_autocorr[i]<-"Il est peu probable que les données soient autocorrélées"}
    }
    else {autocorr_r1[i]<-NA
    signif_autocorr[i]<-"L'autocorrélation des données n'a pas pu être testée."}


    if (graphiques=="oui") {
    point<-rep(1,nbr_anal[i])
    point[which(codesigne[order(date_an)]==1)]<-3
    plot(sort(date_an),donnees,main=liste_pts[i],xaxp=c(min(date_an),max(date_an),2),xlab="Année",ylab=paste(text_legende," (",unite,")"),type="b",pch=point,lty=2)
    par(mar=c(5, 4, 1, 2))
    plot.new()
    legend("left", legend = c("quantification", "non quantification (<LQ, <LD, traces...)"), col = "black", pch = c(3,1),bty="n")
    par(mar=c(5, 4, 12, 2) + 0.1)

    boxplot(donnees,ylab=paste(text_legende," (",unite,")"),main="Boxplot")
    points(moyenne[i],col="red",pch=3)
    par(mar=c(5, 4, 4, 2) + 0.1)
    }

}

if (graphiques=="oui") {dev.off()}

#Ecriture du tableau de résultats

table<-cbind(as.vector(liste_pts),format(date_min,"%d/%m/%Y"),format(date_max,"%d/%m/%Y"),nbr_anal,long,moyenne,mediane,text_med,ecarttype,percentil1,text_perc1,percentil2,text_perc2,taux_quanti,LQmin,LQmax,LQmin2,LQmax2,moy_ecarts,ec_typ_ecarts,moy_ecarts2,ec_typ_ecarts2,pval_shapi,normalite,autocorr_r1,signif_autocorr)
colnames(table)<-c("BSS","Date min","Date max","Nbre analyses","Longueur de la chronique (jours)","Moyenne des résultats","Mediane des résultats","Remarque médiane","Ecart-type des résultats",
                               "Premier décile des résultats","Remarque premier décile","Dernier décile résultats","Remarque dernier décile","Taux de quantification","LQ min tt codes",
                               "LQ max tt codes","LQ min code 10","LQ max code 10","Moyenne du nombre de jours d'écarts entre deux analyses","Ecart-type du nombre jours d'écarts entre deux analyses",
                               "Moyenne du nombre de jours d'écarts entre deux analyses sans outliers","Ecart-type du nombre jours d'écarts entre deux analyses sans outliers","p-value - test de Shapiro","Normalité de la distribution des données",
                               "Valeur de l'autocorrélation au rang 1","Significativité de l'autocorrélation")
sortie<-write.table(table,file=paste(chemin,nom_sortie,".txt",sep=""),row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)

print("Le module caractérisation a bien été exécuté. Les fichiers résultats ont été créés dans le répertoire contenant votre fichier de données.")

