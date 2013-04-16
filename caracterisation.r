nom_sortie<-paste("caract_",nom_fichier_ssextension,sep="")

print("Voulez-vous une repr�sentation graphique des donn�es ? Tapez oui ou non.")
graphiques<-scan( what="",nlines=1)

if (is.null(try(write.table(0,file=paste(chemin,nom_sortie,".txt",sep="")),silent=TRUE))==FALSE) {stop("Le tableau de r�sultat est ouvert. Fermez-le et relancer le script.")}

while (is.null(dev.list())==FALSE) {dev.off()}
if (graphiques=="oui") {
pdf(paste(chemin,"graphes_",nom_sortie,".pdf", sep=""))
layout (matrix(c(1,2,3,2),2,2),widths=c(2,1),heights=c(3,1))
}


#Compte le nombre de points BSS
if (is.null(Data_test$CODE_BSS)) {Data_test$CODE_BSS<-Data_test$"Code national BSS"}
if (is.null(Data_test$DATE_DEBUT_PRELEVEMENT)) {Data_test$DATE_DEBUT_PRELEVEMENT<-Data_test$"Date pr�l�vement"}
if (is.null(Data_test$CODE_SIGNE)) {Data_test$CODE_SIGNE<-Data_test$"Code remarque analyse"}
if (is.null(Data_test$RESULTAT)) {Data_test$RESULTAT<-Data_test$"R�sultat de l'analyse"}

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


if (is.null(Data_test$UNITE_GRAPH)&&is.null(Data_test$"Unit� du graphique")) {
   if (is.null(Data_test$UNITE)) {Data_test$UNITE<-Data_test$"Unit�"}

   tab_Unite<-read.table("Unite_SANDRE.txt",comment.char="",
   header=FALSE, sep=";", row.names=NULL,na.strings="", dec=".", strip.white=FALSE,quote="")

   vect_unite<-Data_test$UNITE
   if (length(unique(vect_unite))==1) {

      unite<-Data_test$UNITE[1]
      unite<-tab_Unite[which(tab_Unite[2]==as.character(unite)),3]
     } else {
     stop("Attention, les unit�s ne sont pas toutes identiques.")}
} else {
   if (is.null(Data_test$UNITE_GRAPH)) {Data_test$UNITE_GRAPH<-Data_test$"Unit� du graphique"}
   vect_unite<-Data_test$UNITE_GRAPH
   if (length(unique(vect_unite))==1) {
      unite<-Data_test$UNITE_GRAPH[1]
     } else {
     stop("Attention, les unit�s ne sont pas toutes identiques.") }

}

test<-0
if (is.null(Data_test$CODE_PARAMETRE)) {test=1}

if (test==1) {
text_legende="Concentration"
} else {
param<-Data_test$CODE_PARAMETRE[1]
if (param==1301) text_legende<-"Temp�rature" else if (param==1302) text_legende<-"pH" else if (param==1303) text_legende<-"Conductivit� � 25�" else if(param==1330) text_legende<-"Potentiel d'oxydo-r�duction (Eh)" else if(param==1311) text_legende<-"Oxyg�ne dissous" else if (param==1304) text_legende<-"Conductivit� � 20�" else text_legende<-"Concentration"
}

for (i in 1:nb_pts){
    indic<-which(Data_test$CODE_BSS==liste_pts[i])

    #Cr�ation des vecteurs donnees/date_an/codesigne et suppression des analyses non faites donnees=NA
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

    #Moyennes/d�ciles/ecart-type
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
    #Remarque dans le cas o� mediane/decile<LQ
    if(LQmax[i]>mediane[i]&&is.na(LQmax[i])==FALSE&&is.na(mediane[i])==FALSE)text_med[i]<-"Attention : la valeur de la m�diane est inf�rieure � la limite de quantification maximale"
    if(LQmax[i]>percentil1[i]&&is.na(LQmax[i])==FALSE&&is.na(percentil1[i])==FALSE)text_perc1[i]<-"Attention : la valeur du premier d�cile est inf�rieure � la limite de quantification maximale"
    if(LQmax[i]>percentil2[i]&&is.na(LQmax[i])==FALSE&&is.na(percentil2[i])==FALSE)text_perc2[i]<-"Attention : la valeur du dernier d�cile est inf�rieure � la limite de quantification maximale"


    #Calcul de la moyenne et de l'�cart-type des �carts entre deux dates d'analyses
    dates_triees<-sort(date_an)
    ecarts<-diff(dates_triees)
    moy_ecarts[i]<-mean(ecarts)
    med_ecarts[i]<-median(ecarts)
    ec_typ_ecarts[i]<-sd(ecarts)

    #identification d'outliers dans la fr�quence de pr�l�vements
    if (length(which(ecarts>=med_ecarts[i]+2* ec_typ_ecarts[i]))!=0) {
       moy_ecarts2[i]<-mean(ecarts[-which(ecarts>=med_ecarts[i]+2* ec_typ_ecarts[i])])
       ec_typ_ecarts2[i]<-sd(ecarts[-which(ecarts>=med_ecarts[i]+2* ec_typ_ecarts[i])])
    }
    else {moy_ecarts2[i]<-moy_ecarts[i]
          ec_typ_ecarts2[i]<- ec_typ_ecarts[i] }


    #Calcul de la normalit�. Test de Shapiro.
          if (nbr_anal[i]>=3) {
                              if (length(unique(donnees))!=1) {
                              shapishapo<-shapiro.test(donnees)
                              pval_shapi[i]<-shapishapo$p.value
                              if (pval_shapi[i]>=0.05|is.na(pval_shapi[i])) {normalite[i]<-"Un test de Shapiro-Wilke a �t� appliqu�. Les donn�es de cette chronique sont normalement distribu�es"}
                              else {normalite[i]<-"Un test de Shapiro-Wilke a �t� appliqu�. Les donn�es de cette chronique ne sont pas normalement distribu�es"}
                              }
                              else {
                              pval_shapi[i]<-NA
                              normalite[i]<-"Toutes les valeurs sont identiques. Le test de Shapiro-Wilke n'a pas �t� appliqu�."}
                              }
         else {pval_shapi[i]<-NA
         normalite[i]<-"Il n'y a pas assez de donn�es pour estimer la normalit� de la distribution"}


    #Autocorr�lation des donn�es

    #Tri des donn�es par ordre chronologique
    donnees<-donnees[order(date_an)]

    autocorr<-acf(donnees,type="correlation",plot=FALSE,na.action=na.pass)
    if (length(autocorr$acf)>1&&is.na(autocorr$acf[2])==FALSE) {
    
    #Autocorr�lation au rang 1
    autocorr_r1[i]<-autocorr$acf[2]

    #Significativit� de l'autocorr�lation au seuil de 5%
    seuil<-qnorm((1 + 0.95)/2)/sqrt(autocorr$n.used)
    if(abs(autocorr_r1[i])>abs(seuil)) {signif_autocorr[i]<-"La probabilit� que les donn�es soient autocorr�l�es au rang 1 est sup�rieure � 95%"}
                                 else {signif_autocorr[i]<-"Il est peu probable que les donn�es soient autocorr�l�es"}
    }
    else {autocorr_r1[i]<-NA
    signif_autocorr[i]<-"L'autocorr�lation des donn�es n'a pas pu �tre test�e."}


    if (graphiques=="oui") {
    point<-rep(1,nbr_anal[i])
    point[which(codesigne[order(date_an)]==1)]<-3
    plot(sort(date_an),donnees,main=liste_pts[i],xaxp=c(min(date_an),max(date_an),2),xlab="Ann�e",ylab=paste(text_legende," (",unite,")"),type="b",pch=point,lty=2)
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

#Ecriture du tableau de r�sultats

table<-cbind(as.vector(liste_pts),format(date_min,"%d/%m/%Y"),format(date_max,"%d/%m/%Y"),nbr_anal,long,moyenne,mediane,text_med,ecarttype,percentil1,text_perc1,percentil2,text_perc2,taux_quanti,LQmin,LQmax,LQmin2,LQmax2,moy_ecarts,ec_typ_ecarts,moy_ecarts2,ec_typ_ecarts2,pval_shapi,normalite,autocorr_r1,signif_autocorr)
colnames(table)<-c("BSS","Date min","Date max","Nbre analyses","Longueur de la chronique (jours)","Moyenne des r�sultats","Mediane des r�sultats","Remarque m�diane","Ecart-type des r�sultats",
                               "Premier d�cile des r�sultats","Remarque premier d�cile","Dernier d�cile r�sultats","Remarque dernier d�cile","Taux de quantification","LQ min tt codes",
                               "LQ max tt codes","LQ min code 10","LQ max code 10","Moyenne du nombre de jours d'�carts entre deux analyses","Ecart-type du nombre jours d'�carts entre deux analyses",
                               "Moyenne du nombre de jours d'�carts entre deux analyses sans outliers","Ecart-type du nombre jours d'�carts entre deux analyses sans outliers","p-value - test de Shapiro","Normalit� de la distribution des donn�es",
                               "Valeur de l'autocorr�lation au rang 1","Significativit� de l'autocorr�lation")
sortie<-write.table(table,file=paste(chemin,nom_sortie,".txt",sep=""),row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)

print("Le module caract�risation a bien �t� ex�cut�. Les fichiers r�sultats ont �t� cr��s dans le r�pertoire contenant votre fichier de donn�es.")

