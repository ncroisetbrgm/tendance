#Initialisation
#rm(list=ls())


nom_sortie<-paste("tendances_",nom_fichier_ssextension,sep="")

#-------------------------------------------------------------------------------
# Breakpoint detection in a time series according to Pettitt 1979
# R script by Pascal Haenggi, v20090819
#-------------------------------------------------------------------------------
# x = time series with 1. column: year and 2. column: value
# alpha = significance level for test, e.g. 0.05
pettitt <- function(x, alpha) {
  # rank and rank_cumsum
  xtmp <- cbind(x, rank(x[,2]))
  xtmp <- cbind(xtmp, cumsum(xtmp[,3]))
  xtmp <- cbind(xtmp, 1:nrow(xtmp))
  # Xk (test statistic)
  xtmp <- cbind(xtmp, 2 * xtmp[,4] - xtmp[,5] * (nrow(xtmp) + 1))
  colnames(xtmp) <- c("YYYY", "VALUE", "rank", "rank_cumsum", "k", "Xk")
  # breakpoint
  XE  <- max(abs(xtmp[,'Xk']))
  XEa <- xtmp[which(abs(xtmp[,'Xk'])==XE), 'YYYY']
  # p-value
  n    <- nrow(xtmp)
  pval <- exp( (-6 * (XE^2)) / (n^2 + n^3) )
  # significance level (global for all series with same n)
  Xksign <- sqrt( -1/6 * ((log(alpha, base = exp(1))) * (n^2 + n^3)) )
  significant <- XE > Xksign

  res <- list(Xk=xtmp,XEa=XEa,XE=XE,Xksign=Xksign,sig=significant,pval=pval)
  return(res)
}

#-------------------------------------------------------------------------------
#Test de rupture de Buishand (1982)
#N. Croiset 30/11/2012
#-------------------------------------------------------------------------------
# x= matrice avec en première colonne la date, et en deuxième les valeurs
# alpha = significance level for test, 1,5,ou 10 pour 1%, 5% ou 10%
buishand<-function(x,alpha){

# critical levels (Buishand, 1982)
cniv<-c(0.1,0.05,0.01)
cval <- matrix(data = c(
10,1.05,1.14,1.29,
20,1.1,1.22,1.42,
30,1.12,1.24,1.46,
40,1.13,1.26,1.5,
50,1.14,1.27,1.52,
100,1.17,1.29,1.55,
1000,1.22,1.36,1.63), nrow=7, ncol=4, byrow=T,
dimnames = list(NULL,c('nb_points','alpha10','alpha5','alpha1')))

#Calcul des S*k
n<- length(x[,2])
if (n<10) {Qmax<-NA; date_rupt<-NA; significant<-NA;pval=NA}
else {
x<-x[order(x[,1]),]
sk<-cumsum(x[,2]-mean(x[,2]))
Q<-sk/(sd(x[,2])*(n-1)/n)^0.5
Qmax<-max(abs(Q))/n^0.5
date_rupt<-x[which.max(abs(Q)),1]

#Calcul de la significativité du test avec le alpha donné comme paramètre

a<- max(which(cval[,'nb_points']<=n))
Qcrit<-cval[a,paste('alpha',alpha,sep="")]
if(Qmax>Qcrit){significant<-1}
else {significant<-0}

#Calcul de la pvalue - pas de manière exacte, juste l'intervalle dans laquelle elle se trouve :
#  0.01   <=0.01
#  0.05   0.01-0.005
#  0.1    0.05-0.1
#  >0.1

b<-which(cval[a,2:4]<=Qmax)
if (length(b)==0) {pval<-">0.1"}
else {pval<-cniv[max(b)]}
}
res<-list(x=x,Qmax=Qmax,date_rupt=as.Date(date_rupt,origin="1970-01-01"),sig=significant,pval=pval)

return(res)
}



print("voulez-vous une représentation graphique des résultats ? Tapez oui ou non.")
graphiques<-scan( what="",nlines=1)

if (is.null(try(write.table(0,file=paste(chemin,nom_sortie,".txt",sep="")),silent=TRUE))==FALSE) {stop("Attention, le tableau de résultat est ouvert.")}


while (is.null(dev.list())==FALSE) {dev.off()}
if (graphiques=="oui") {
pdf(paste(chemin,"graphes_tendances_",nom_fichier_ssextension,".pdf", sep=""))
#layout(rbind(1,2), heights=c(7,1))
#layout(matrix(c(1,1,3,2,4,4),2,3,byrow=TRUE),heights=c(6,3),widths=c(4,3,3))
layout(matrix(c(1,1,2,4,3,4),3,2,byrow=TRUE),heights=c(6,2,1.5),widths=c(4,6))
}


#Compte le nombre de points BSS
if (is.null(Data_test$DATE_DEBUT_PRELEVEMENT)) {Data_test$DATE_DEBUT_PRELEVEMENT<-Data_test$"Date prélèvement"}
if (is.null(Data_test$CODE_SIGNE)) {Data_test$CODE_SIGNE<-Data_test$"Code remarque analyse"}
if (is.null(Data_test$RESULTAT)) {Data_test$RESULTAT<-Data_test$"Résultat de l'analyse"}
if ((is.null(Data_test$CODE_BSS)&&is.null(Data_test$"Code national BSS"))==FALSE) {
  if (is.null(Data_test$CODE_BSS)) {Data_test$CODE_BSS<-Data_test$"Code national BSS"}}   
if ((is.null(Data_test$LIBELLE_PARAMETRE)&&is.null(Data_test$"Paramètre"))==FALSE) {
  if (is.null(Data_test$LIBELLE_PARAMETRE)) {Data_test$LIBELLE_PARAMETRE<-Data_test$"Paramètre"}}

if(is.null(Data_test$LIBELLE_PARAMETRE)&&is.null(Data_test$CODE_BSS=="")) {
  Data_test$ID<-"Chronique"
} else if (is.null(Data_test$CODE_BSS)) {
  Data_test$ID<-Data_test$LIBELLE_PARAMETRE
} else if (is.null(Data_test$LIBELLE_PARAMETRE)) {
  Data_test$ID<-Data_test$CODE_BSS
}else {Data_test$ID<-paste(Data_test$CODE_BSS,"/",Data_test$LIBELLE_PARAMETRE)}


liste_pts<-unique(Data_test$ID)
nb_pts<-length(liste_pts)


#Initialisatio
date_min<-vector("numeric",nb_pts)
class(date_min)<-"Date"
date_max<-vector("numeric",nb_pts)
class(date_max)<-"Date"
nbr_anal<-vector("numeric",nb_pts)
long<-vector("numeric",nb_pts)
moy_ecarts<-vector("numeric",nb_pts)
moy_ecarts2<-vector("numeric",nb_pts)
med_ecarts<-vector("numeric",nb_pts)
ec_typ_ecarts<-vector("numeric",nb_pts)
ec_typ_ecarts2<-vector("numeric",nb_pts)
pval_shapi<-vector("numeric",nb_pts)
pval_rupt<-vector("character",nb_pts)
rupt<-vector("numeric",nb_pts)
slope_Sen<-vector("numeric",nb_pts)
slope_Sen_an<-vector("numeric",nb_pts)
interc<-vector("numeric",nb_pts)
pval_mk<-vector("numeric",nb_pts)
estimate_mk<-vector("numeric",nb_pts)
pente_lin<-vector("numeric",nb_pts)
pente_lin_an<-vector("numeric",nb_pts)
interc_lin<-vector("numeric",nb_pts)
rcarre<-vector("numeric",nb_pts)
pval_lin<-vector("numeric",nb_pts)
slope_Sen1<-vector("numeric",nb_pts)
interc1<-vector("numeric",nb_pts)
pval_mk1<-vector("numeric",nb_pts)
estimate_mk1<-vector("numeric",nb_pts)
slope_Sen2<-vector("numeric",nb_pts)
interc2<-vector("numeric",nb_pts)
pval_mk2<-vector("numeric",nb_pts)
estimate_mk2<-vector("numeric",nb_pts)
pente_lin1<-vector("numeric",nb_pts)
rcarre1<-vector("numeric",nb_pts)
pval_LR1<-vector("numeric",nb_pts)
pente_lin2<-vector("numeric",nb_pts)
rcarre2<-vector("numeric",nb_pts)
pval_LR2<-vector("numeric",nb_pts)
sig_ruptB<-vector("numeric",nb_pts)
ruptB<-vector("numeric",nb_pts)
normalite<-vector("character",nb_pts)
tend<-vector("character",nb_pts) 
tend_lin<-vector("character",nb_pts)
text_rupt<-vector("character",nb_pts)
Zmax<-vector("numeric",nb_pts)
rupt_MK<-vector("numeric",nb_pts)
class(rupt_MK)<-"Date"
pval_mk2b<-vector("numeric",nb_pts)
estimate_mk2b <-vector("numeric",nb_pts)
slope_Sen2b<-vector("numeric",nb_pts)
slope_Sen2b_an<-vector("numeric",nb_pts)
interc2b<-vector("numeric",nb_pts)
pval_mk1b<-vector("numeric",nb_pts)
estimate_mk1b <-vector("numeric",nb_pts)
slope_Sen1b<-vector("numeric",nb_pts)
slope_Sen1b_an<-vector("numeric",nb_pts)
interc1b<-vector("numeric",nb_pts)
moy1<-vector("numeric",nb_pts)
moy2<-vector("numeric",nb_pts)
text_mkmodif<-vector("character",nb_pts)
pval_mkmodif<-vector("numeric",nb_pts)
pval_ruptMK<-vector("numeric",nb_pts)
text_ruptMK<-vector("numeric",nb_pts)
pval_anov<-vector("numeric",nb_pts)
tau_mkmodif<-vector("numeric",nb_pts)
text_anov<-vector("character",nb_pts)
taux_quanti<-vector("numeric",nb_pts)
autocorr_r1<-vector("numeric",nb_pts)
signif_autocorr<-vector("character",nb_pts)
test_toto<-vector("numeric",nb_pts)

if (is.null(Data_test$UNITE_GRAPH)&&is.null(Data_test$"Unité du graphique")) {
   if (is.null(Data_test$UNITE)) {Data_test$UNITE<-Data_test$"Unité"}

   tab_Unite<-read.table("Unite_SANDRE.txt",comment.char="",
   header=FALSE, sep=";", row.names=NULL,na.strings="", dec=".", strip.white=FALSE,quote="")
  
   liste_unite<-tab_Unite[,3]
   names(liste_unite)<-tab_Unite[,2]
   Data_test$UNITE_GRAPH<-liste_unite[as.character(Data_test$UNITE)]
} else {
   if (is.null(Data_test$UNITE_GRAPH)) {Data_test$UNITE_GRAPH<-Data_test$"Unité du graphique"}
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

#identifie les indices des données du pt BSS étudié

    indic<-which(Data_test$ID==liste_pts[i])
    
    vect_unite<-Data_test$UNITE_GRAPH[indic]
    unite<-vect_unite[1]
    if (length(unique(vect_unite))!=1) {print("Attention ! Vos unités ne sont pas toutes identiques pour un même point et un même paramètre !")}
      
    
    
    #Création des vecteurs donnees/date_an/codesigne et suppression des analyses non faites donnees=NA
    date_an<-as.Date(Data_test$DATE_DEBUT_PRELEVEMENT[indic],format="%d/%m/%Y")
    donnees<-Data_test$RESULTAT[indic]
    date_an<-date_an[which(is.na(donnees)==FALSE)]
    codesigne<-Data_test$CODE_SIGNE[indic]
    codesigne<-codesigne[which(is.na(donnees)==FALSE)]
    donnees<-donnees[which(is.na(donnees)==FALSE)]
    nbr_anal[i]<-length(donnees)
    if (nbr_anal[i]==0) {next}

#Calcul de la longueur de la chronique

    date_min[i]<-min(date_an)
    date_max[i]<-max(date_an)

    long[i]<- date_max[i]-date_min[i]
    dates_triees<-sort(date_an)
    donnees_triees<-donnees[order(date_an)]
    
    #Taux de quantification - LQs

    taux_quanti[i]<-length(which(codesigne==1))/nbr_anal[i]


#Trace la chronique
  if (graphiques=="oui") {
  point<-rep(1,nbr_anal[i])
  point[which(codesigne[order(date_an)]==1)]<-3
  par(mar=c(6,5,4,2))
  plot(sort(date_an),donnees[order(date_an)],main=liste_pts[i],xlab="Année",ylab=paste(text_legende," (",unite,")"),type="b",pch=point,lty=2)
  text_leg<-c("Série temporelle","Valeur > LQ", "Valeur <LQ, <LD, traces...")
  col_leg<-c("black","black","black")
  pch_leg<-c(NA,3,1)
  lty_leg<-c(2,NA,NA)
  type_leg<-c("l","p","p")
#  if (taux_quanti[i]<=0.5) {text(x=grconvertX(0.5, from = "npc", to = "user"),y=grconvertY(0, from = "npc", to = "user"),"Attention, taux de quantification <50% !",col="red",font=2,adj=0,cex=0.9)}
  if (taux_quanti[i]<=0.5) {mtext("Attention, taux de quantification <50% !",side=1,adj=1,padj=1,col="red",font=2,cex=0.8,line=4)}
  }



    #pour les chroniques avec au moins 3 analyses, calcul de la normalité et test de rupture
    if (nbr_anal[i]>=3) {
    #Calcul de la normalité. Test de Shapiro.
              if (length(unique(donnees))!=1) {
                    shapishapo<-shapiro.test(donnees)
                    pval_shapi[i]<-shapishapo$p.value
                    if (pval_shapi[i]>=0.05|is.na(pval_shapi[i])) {
                    normalite[i]<-"Un test de Shapiro-Wilke a été appliqué. Les données de cette chronique sont normalement distribuées"
                    norm1_graph<-"Données normalement distribuées"
                    norm2_graph<-paste("(pval-Shapiro=",format(pval_shapi[i],digits=2,scientific=TRUE),")")
                    }else {
                    normalite[i]<-"Un test de Shapiro-Wilke a été appliqué. Les données de cette chronique ne sont pas normalement distribuées"
                    norm1_graph<-"Données non normalement distribuées"
                    norm2_graph <-paste("(pval-Shapiro=",format(pval_shapi[i],digits=2,scientific=TRUE),")")
                    }
                } else {
                     pval_shapi[i]<-NA
                     normalite[i]<-"Toutes les valeurs sont identiques. Le test de Shapiro-Wilke n'a pas été appliqué."
                     norm1_graph<-"Chronique stationnaire"
                     norm2_graph<-""
                     }

                #Calcul des dates de rupture avec la P-value pour le test de Pettitt et pour le test de Buishand 1 si c'est significatif 0 si ça ne l'est pas

                #test de Pettitt si données non normalement distribuée
                if (pval_shapi[i]<0.05&&is.na(pval_shapi[i])==FALSE){
                   jeu<-cbind(date_an,donnees)
                   res <- pettitt(jeu, 0.05)
                   pval_rupt[i]<-res$pval
                   rupt[i]<-ifelse(pval_rupt[i]>0.05,NA,as.Date(min(res$XEa),origin="1970-01-01"))
                   test_rupt<-"test non paramétrique de Pettitt"
                   pres_rupt<-ifelse(pval_rupt[i]<=0.05,"oui","non")
                   rupt_graph1<-ifelse(pval_rupt[i]<=0.05,format(as.Date(rupt[i],origin="1970-01-01"),"%d/%m/%Y"),"Pas de rupture significative détectée")
                   rupt_graph2<-ifelse(pval_rupt[i]<=0.05,format(pval_rupt[i],digits=2,scientific=TRUE),"")
                   testrupt_graph<-"(Pettitt)"
                   } else if (pval_shapi[i]>=0.05&&is.na(pval_shapi[i])==FALSE&&nbr_anal[i]>=10){
                #test de Buishand si données normalement distribuées
                   jeu<-cbind(date_an,donnees)
                   res <- buishand(jeu,5)
                   sig_ruptB[i]<-res$sig
                   pval_rupt[i]<-ifelse(sig_ruptB[i]==1,"<0.05",">0.05")
                   rupt[i]<-ifelse(sig_ruptB[i]==1,as.Date(min(res$date_rupt),origin="1970-01-01"),NA)
                   test_rupt<-"test paramétrique de Buishand"
                   pres_rupt<-ifelse(sig_ruptB[i]==1,"oui","non")
                   rupt_graph1<-ifelse(sig_ruptB[i]==1,format(as.Date(rupt[i],origin="1970-01-01"),"%d/%m/%Y"),"Pas de rupture significative détectée")
                   rupt_graph2<-ifelse(sig_ruptB[i]==1,"<0.05","")
                   testrupt_graph<-"(Buishand)"
                   } else if (pval_shapi[i]>=0.05&&is.na(pval_shapi[i])==FALSE&&nbr_anal[i]<10) {
                   sig_ruptB[i]<-NA
                   pval_rupt[i]<-NA
                   rupt[i]<-NA
                   pres_rupt<-"trop peu de données"
                   rupt_graph1<-"Non effectué (pas assez de données)"
                   rupt_graph2<-""
                   testrupt_graph<-""
                   } else if (is.na(pval_shapi[i])==TRUE) {
                   sig_ruptB[i]<-NA
                   pval_rupt[i]<-NA 
                   rupt[i]<-NA
                   pres_rupt<-"chronique stationnaire"
                   rupt_graph1<-"Non effectué"
                   rupt_graph2<-""
                   testrupt_graph<-""
                   }
    } else {
         pval_shapi[i]<-NA
         pval_rupt[i]<-NA
         rupt[i]<-NA
         sig_ruptB[i]<-NA
         normalite[i]<-"Il y a trop peu de données pour étudier la normalité de la distribution"
         pres_rupt<-"trop peu de données"
    }

 #Tri des données par ordre chronologique
    donneesrk<-donnees[order(date_an)]

    autocorr<-acf(donneesrk,type="correlation",plot=FALSE,na.action=na.pass)
    if (length(autocorr$acf)>1&&is.na(autocorr$acf[2])==FALSE) {

       #Autocorrélation au rang 1
       autocorr_r1[i]<-autocorr$acf[2]
    
       #Significativité de l'autocorrélation au seuil de 5%
       seuil<-qnorm((1 + 0.95)/2)/sqrt(autocorr$n.used)
       if(abs(autocorr_r1[i])>abs(seuil)) {
           signif_autocorr[i]<-"La probabilité que les données soient autocorrélées au rang 1 est supérieure à 95%"
           autocor_graph1<-"Données autocorrélées"
           autocor_graph2<-"(pval<0.05)"
           if (length(donneesrk)>=40) {
              test_mkmod<-mkTrend(donneesrk,ci=0.95)
              pval_mkmodif[i]<-test_mkmod$`Corrected p.value`
              test_toto[i]<-test_mkmod$`p.value`
              tau_mkmodif[i]<-test_mkmod$`tau`
              ifelse(pval_mkmodif<=0.05&&is.na(pval_mkmodif[i])==FALSE,text_mkmodif[i]<-"Un test de Mann-Kendall modifié a été appliqué. Une tendance significative est détectée.",text_mkmodif[i]<-"Un test de Mann-Kendall modifié a été appliqué. Aucune tendance significative n'est détectée.")
              tendMKmodif_graph2<-format(pval_mkmodif[i],digits=2,scientific=TRUE)
              }else {
              text_mkmodif[i]<-"Les données sont autocorrélées mais il n'y a pas suffisamment de données pour appliquer un test de Mann-Kendall modifié."
              tendMKmodif_graph1<-"Non effectué (pas assez de données)"
              tendMKmodif_graph2<-""
              pval_mkmodif[i]<-NA
              }
          } else{
          text_mkmodif[i]<-"Les données ne sont pas autocorrélées. Le test de Mann-Kendall modifié n'a donc pas été appliqué."
          signif_autocorr[i]<-"Il est peu probable que les données soient autocorrélées"
          pval_mkmodif[i]<-NA
          tendMKmodif_graph1<-"Non effectué (pas d'autocorrélation)"
          tendMKmodif_graph2<-""
          autocor_graph1<-"Données non autocorrélées"
          autocor_graph2<-""
             }
    }
    else {text_mkmodif[i]<-"L'autocorrélation des données n'a pas pu être testée."
         signif_autocorr[i]<-"L'autocorrélation des données n'a pas pu être testée."
         autocorr_r1[i]<-NA
         pval_mkmodif[i]<-NA
         tendMKmodif_graph1<-"Non effectué (pas d'autocorrélation)"
         tendMKmodif_graph2<-""
         autocor_graph1<-"Données non autocorrélées"
         autocor_graph2<-""
         }


#Calcul de tendances MK ou linéaire

#Pout toutes les chroniques : Mann-Kendall
    if (normalite[i]=="Toutes les valeurs sont identiques") {
       tend[i]<-"La chronique est stationnaire"
       tendMK_graph1<-"Chronique stationnaire"
       tendL_graph1<-"Chronique stationnaire"
       tendMK_graph2<-""
       tendL_graph2<-""
    } else {
             #Mann-Kendall
             if (nbr_anal[i]>=10) {
                mk_test <- cor.test(donnees,as.numeric(date_an),use="everything",method=c("kendall"))
                pval_mk[i]<-mk_test$p.value
                if (pval_mk[i]==0) {pval_graph<-"<2.2e-16"} else {pval_graph<-format(pval_mk[i],digits=2,scientific=TRUE)}
                estimate_mk[i]<-mk_test$estimate

               #Calcul pente de Sen
               if (pval_mk[i]<=0.05){
#                  tend[i]<-"Une tendance significative est observée"
                  donneesx<-outer(donnees,donnees,"-")
                  datex<-outer(date_an,date_an,"-")
                  z<-donneesx/matrix(as.numeric(datex),ncol=nbr_anal[i])
                  s<-z[lower.tri(z)]
                  slope_Sen[i]<-median(s,na.rm=TRUE)
                  slope_Sen_an[i]<-slope_Sen[i]*365
                  #Calcul intercept (Conover,1980)
                  interc[i]<-median(donnees,na.rm=TRUE)-slope_Sen[i]*as.numeric(median(date_an,na.rm=TRUE))
                  tend[i]<-paste("Un test de tendance de Mann-Kendall a été appliqué. Une tendance significative de pente",format(slope_Sen[i]*365,digits=3),unite,"/an est détectée.")
#                  test_graph<-"Mann-Kendall"
                  tendMK_graph1<-paste(format(slope_Sen[i]*365,digits=3,scientific=TRUE),unite,"/an")
                  tendMK_graph2<-pval_graph
                  if (graphiques=="oui") {
                    abline(a=interc[i],b=slope_Sen[i],col="red")
                    text_leg<-c(text_leg,"Tendance (Mann-Kendall)")
                    col_leg<-c(col_leg,"red")
                    pch_leg<-c(pch_leg,NA)
                    lty_leg<-c(lty_leg,1)
                    type_leg<-c(type_leg,"l")
                    }

                  }
                  else {
                  tend[i]<-"Un test de tendance de Mann-Kendall a été appliqué. Aucune tendance significative n'est détectée"
#                 test_graph<-"Mann-Kendall"
                  tendMK_graph1<-"Aucune tendance significative détectée"
                  tendMK_graph2<-pval_graph
                  interc[i]<-NA
                  slope_Sen_an[i]<-NA
                  }

             } else {
             tend[i]<-"Il n'y a pas assez de données pour effectuer un test de tendance de Mann-Kendall."
             tendMK_graph1<-"Non effectué (pas assez de données)"
             tendMK_graph2<-""
             interc[i]<-NA
             slope_Sen_an[i]<-NA
             pval_mk[i]<-NA 
             estimate_mk[i]<-NA
             }


#Calcul de régression linéaire si les données sont normalement dsitribuées
        if (pval_shapi[i]>=0.05&&is.na(pval_shapi[i])==FALSE){
             #régression linéaire
             if (nbr_anal[i]>=5) {
                lin_test <- lm(donnees~as.numeric(date_an))
                pente_lin[i]<-lin_test$coefficient[2]
                pente_lin_an[i]<-pente_lin[i]*365
                interc_lin[i]<-lin_test$coefficient[1]
                rcarre[i]<-(cor(donnees,as.numeric(date_an)))^2
                pval_lin[i]<-summary(lin_test)$coefficient[2,4]
                if (pval_lin[i]<=0.05) {
                  tend_lin[i]<-paste("Une régression linéaire a été appliquée. Une tendance significative de pente",format(pente_lin[i]*365,digits=3),"est détectée.")
                  tendL_graph1<-paste(format(pente_lin[i]*365,digits=3,scientific=TRUE),unite,"/an")
                  tendL_graph2<-format(pval_lin[i],digits=2,scientific=TRUE)
                  if (graphiques=="oui") {
                     abline(a=interc_lin[i],b=pente_lin[i],col="blue")
                     text_leg<-c(text_leg,"Tendance (régression linéaire)")
                     col_leg<-c(col_leg,"blue")
                     pch_leg<-c(pch_leg,NA)
                     lty_leg<-c(lty_leg,1)
                     type_leg<-c(type_leg,"l")
                     }
                   } else {
                   tend_lin[i]<-"Une régression linéaire a été appliquée. Aucune tendance significative n'est détectée."
                   pente_lin_an[i]<-NA
                   interc_lin[i]<-NA
                   tendL_graph1<-"Aucune tendance significative détectée"
                   tendL_graph2<-format(pval_lin[i],digits=2,scientific=TRUE)
                   }
                } else {tendL_graph1<-"Non effectué (pas assez de données)"
                       tend_lin[i]<-"Il n'y a pas assez de données pour effectuer une régressino linéaire"
                       tendL_graph2<-""
                       rcarre[i]<-NA
                       pval_lin[i]<-NA
                       pente_lin_an[i]<-NA
                       interc_lin[i]<-NA
                        }

        } else {tendL_graph1<-"Non effectué (données non normalement distribuées)"
                       tend_lin[i]<-"La régression linéaire n'a pas été effectuée car les données ne sont pas normalement distribuées"
                       tendL_graph2<-""
                       rcarre[i]<-NA
                       pval_lin[i]<-NA
                       pente_lin_an[i]<-NA
                       interc_lin[i]<-NA
                       }
        }
#si rupture significative
      if (pres_rupt=="oui"){
       moy1[i]<-mean(donnees[which(date_an<=rupt[i])])
       moy2[i]<-mean(donnees[which(date_an>rupt[i])])

       text_rupt[i]<-paste("Un changement de moyenne significatif a été détecté. La date de rupture, déterminée par le",test_rupt,"est :",format(as.Date(rupt[i],origin="1970-01-01"),"%d/%m/%Y"))
       if (graphiques=="oui") {
          text_leg<-c(text_leg,"Date de changement de moyenne","Moyenne avant/après rupture")
          col_leg<-c(col_leg,"green","green")
          pch_leg<-c(pch_leg,NA,NA)
          lty_leg<-c(lty_leg,2,1)
          type_leg<-c(type_leg,"h","h")

          abline(v=rupt[i],col="green",lty=2)
          x0<-date_min[i]
          x1<-rupt[i]
          y0<-moy1[i]
          y1<-y0
          segments(x0,y0,x1,y1,col="green")

          x0<-rupt[i]
          x1<-date_max[i]
          y0<-moy2[i]
          y1<-moy2[i]
          segments(x0,y0,x1,y1,col="green")
        }

      } else if (pres_rupt=="trop peu de données") {
      moy1[i]<-NA
      moy2[i]<-NA
      text_rupt[i]<-paste("Il y a trop peu de données pour appliquer un test de changement de moyenne.")
      } else if (pres_rupt=="chronique stationnaire") {
      moy1[i]<-NA
      moy2[i]<-NA
      text_rupt[i]<-"Chronique stationnaire"
      } else {
      moy1[i]<-NA
      moy2[i]<-NA
      text_rupt[i]<-paste("Un",test_rupt,"a été appliqué. Aucun changement de moyenne significatif n'a été détecté.") }

#rupture de pente MK
Zmax[i]<--1
if (nbr_anal[i]>=20) {
      for (l in 10:(nbr_anal[i]-10)) {
       if (length(unique(donnees_triees[1:l]))==1|length(unique(donnees_triees[(l+1):nbr_anal[i]]))==1) {next}
       mk_test_P1<-Kendall(donnees_triees[1:l],as.numeric(dates_triees[1:l]))
       mk_test_P2<-Kendall(donnees_triees[(l+1):nbr_anal[i]],as.numeric(dates_triees[(l+1):nbr_anal[i]]))
       Z<-abs((mk_test_P1$tau-mk_test_P2$tau)/(1/mk_test_P1$D^2*mk_test_P1$varS+1/mk_test_P2$D^2*mk_test_P2$varS)^0.5)
       if (Z>Zmax[i]) {
           Zmax[i]<-Z
           rupt_MK[i]<-dates_triees[l]
           indic_rupt<-l
           pval_ruptMK[i]<-2*pnorm(-abs(Zmax[i]))
           }
    }

     if (Zmax[i]!=-1&pval_ruptMK[i]<=0.05) {
        
        text_ruptMK[i]<-paste("Une inversion de tendance significative a été détectée. La date de rupture, détectée par le test de Darken est:",format(as.Date(rupt_MK[i],origin="1970-01-01"),"%d/%m/%Y"))

        if (graphiques=="oui") {
          text_leg<-c(text_leg,"Date d'inversion de tendance","Tendance avant/après rupture")
          col_leg<-c(col_leg,"orange","orange")
          pch_leg<-c(pch_leg,NA,NA)
          lty_leg<-c(lty_leg,2,1)
          type_leg<-c(type_leg,"h","h")
          abline(v=rupt_MK[i],col="orange",lty=2)
          ruptMK_graph1<-format(as.Date(rupt_MK[i],origin="1970-01-01"),"%d/%m/%Y")
          ruptMK_graph2<-format(pval_ruptMK[i],digits=2,scientific=TRUE)
          }

  #MK sur tronçon avant rupture
          donnees_part<-donnees_triees[1:indic_rupt]
          date_part<- dates_triees[1:indic_rupt]
          mk_test <- cor.test(donnees_part,as.numeric(date_part),use="everything",method=c("kendall"))
          pval_mk1b[i]<-mk_test$p.value
          estimate_mk1b[i]<-mk_test$estimate
          #Calcul pente de Sen
          if (pval_mk1b[i]<0.05&&is.na(pval_mk1b[i])==FALSE) {
             donneesx<-outer(donnees_part,donnees_part,"-")
             datex<-outer(date_part,date_part,"-")
             z<-donneesx/matrix(as.numeric(datex),ncol=length(donnees_part))
             s<-z[lower.tri(z)]
             slope_Sen1b[i]<-median(s,na.rm=TRUE)
             slope_Sen1b_an[i]<-slope_Sen1b[i]*365
             interc1b[i]<-median(donnees_part,na.rm=TRUE)-slope_Sen1b[i]*as.numeric(median(date_part,na.rm=TRUE))
             if (graphiques=="oui") {
                sen1b<-function(x) {return(slope_Sen1b[i]*x+interc1b[i])}
                x0<-date_min[i]
                x1<-rupt_MK[i]
                y0<-sen1b(as.numeric(x0))
                y1<-sen1b(as.numeric(x1))
                segments(x0,y0,x1,y1,col="orange")
                tendMKinv1_graph1<-paste(format(slope_Sen1b[i]*365,digits=3,scientific=TRUE),unite,"/an")
                tendMKinv1_graph2<-format(pval_mk1b[i],digits=2,scientific=TRUE)
                }
             } else{
                slope_Sen1b_an[i]<-NA
                interc1b[i]<-NA
                pval_mk1b[i]<-NA
                estimate_mk1b[i]<-NA
                tendMKinv1_graph1<-"Pas de tendance significative détectée"
                tendMKinv1_graph2<-format(pval_mk1b[i],digits=2,scientific=TRUE)
              }

  #MK sur tronçon après rupture
          donnees_part<-donnees_triees[indic_rupt:nbr_anal[i]]
          date_part<- dates_triees[indic_rupt:nbr_anal[i]]
          mk_test <- cor.test(donnees_part,as.numeric(date_part),use="everything",method=c("kendall"))
          pval_mk2b[i]<-mk_test$p.value
          estimate_mk2b[i]<-mk_test$estimate
          #Calcul pente de Sen
          if (pval_mk2b[i]<0.05&&is.na(pval_mk2b[i])==FALSE) {
             donneesx<-outer(donnees_part,donnees_part,"-")
             datex<-outer(date_part,date_part,"-")
             z<-donneesx/matrix(as.numeric(datex),ncol=length(donnees_part))
             s<-z[lower.tri(z)]
             slope_Sen2b[i]<-median(s,na.rm=TRUE)
             slope_Sen2b_an[i]<-slope_Sen2b[i]*365
             interc2b[i]<-median(donnees_part,na.rm=TRUE)-slope_Sen2b[i]*as.numeric(median(date_part,na.rm=TRUE))
             if (graphiques=="oui") {
                sen2b<-function(x) {return(slope_Sen2b[i]*x+interc2b[i])}
                x1<-date_max[i]
                x0<-rupt_MK[i]
                y0<-sen2b(as.numeric(x0))
                y1<-sen2b(as.numeric(x1))
                segments(x0,y0,x1,y1,col="orange")
                tendMKinv2_graph1<-paste(format(slope_Sen2b[i]*365,digits=3,scientific=TRUE),unite,"/an")
                tendMKinv2_graph2<-format(pval_mk2b[i],digits=2,scientific=TRUE)
                }
             } else{
                slope_Sen2b_an[i]<-NA
                interc2b[i]<-NA
                pval_mk2b[i]<-NA
                estimate_mk2b[i]<-NA
                tendMKinv2_graph1<-"Pas de tendance significative détectée"
                tendMKinv2_graph2<-format(pval_mk2b[i],digits=2,scientific=TRUE)
              }

       } else {text_ruptMK[i]<-"Un test de Darken a été appliqué. Aucun inversion de tendance significative n'a été détectée"
               ruptMK_graph1<-"Pas d'inversion significative détectée"
               ruptMK_graph2<-""
               rupt_MK[i]<-NA
               slope_Sen2b_an[i]<-NA
               interc2b[i]<-NA     
               pval_mk2b[i]<-NA    
               estimate_mk2b[i]<-NA
               slope_Sen1b_an[i]<-NA
               interc1b[i]<-NA     
               pval_mk1b[i]<-NA    
               estimate_mk1b[i]<-NA
              }
  } else {text_ruptMK[i]<-"Il n'y a pas assez de données pour effectuer un test d'inversion de tendance de Darken"
          pval_ruptMK[i]<-NA
          rupt_MK[i]<-NA
          ruptMK_graph1<-"Test non effectué (pas assez de données)"
          ruptMK_graph2<-""
          ruptMK_graph2<-""
          slope_Sen2b_an[i]<-NA
          interc2b[i]<-NA      
          pval_mk2b[i]<-NA     
          estimate_mk2b[i]<-NA 
          slope_Sen1b_an[i]<-NA
          interc1b[i]<-NA      
          pval_mk1b[i]<-NA     
          estimate_mk1b[i]<-NA
         }
  #referme : nbr_anal[i]>=20


#Calcul de la corrélation entre les concentrations et la saison (trimestre)
date_saison<-months(date_an)
date_saison<-ifelse(date_saison=="janvier"|date_saison=="février"|date_saison=="mars","hiver",date_saison)
date_saison<-ifelse(date_saison=="avril"|date_saison=="mai"|date_saison=="juin","printemps",date_saison)
date_saison<-ifelse(date_saison=="juillet"|date_saison=="août"|date_saison=="septembre","été",date_saison)
date_saison<-ifelse(date_saison=="octobre"|date_saison=="novembre"|date_saison=="décembre","automne",date_saison)
if (nbr_anal[i]>=10) {    
                     if (pval_shapi[i]<0.05&&is.na(pval_shapi[i])==FALSE&&length(unique(date_saison))!=1) {
                               supernova<-aov(donnees~factor(date_saison))
                               anov<-summary(supernova)
                               pval_anov[i]<-anov[[1]]$"Pr(>F)"[1]
                               test_anov<-"ANOVA"
                               } else if (pval_shapi[i]>=0.05&&is.na(pval_shapi[i])==FALSE) {
                               kruskallwall<-kruskal.test(donnees~factor(date_saison))
                               pval_anov[i]<-kruskallwall$p.value
                               test_anov<-"de Kruskal-Wallis"
                               } else {pval_anov[i]<-9999}

} else {pval_anov[i]<-NA}

if (pval_anov[i]<0.05 &&is.na(pval_anov[i])==FALSE) {
   text_anov[i]<-paste("Un test",test_anov,"a été appliqué. Au moins deux saisons sont significativement différentes.")
   } else if (pval_anov[i]>=0.05 &&is.na(pval_anov[i])==FALSE) {
   text_anov[i]<-paste("Un test",test_anov,"a été appliqué. Les saisons ne sont pas significativement différentes.")
   } else {text_anov[i]<-"Test non effectué"}

ifelse (is.na(pval_mkmodif[i])==FALSE&&is.na(pval_mk[i])==FALSE&&pval_mkmodif[i]>0.05&&pval_mk[i]<=0.05,tendMKmodif_graph1<-"Tendance non significative si prise en compte de l'autocorrélation",tendMKmodif_graph1<-"")

if (graphiques=="oui") {
  par(mar=c(0,1,1,0.5))
  par(adj=0)
  plot.new()
  title(main="Légende")
  legend(x=0,y=1,legend=text_leg,col=col_leg,pch=pch_leg,lty=lty_leg,bty="n",title.adj=0,cex=0.85)
  plot.new()
  par(mar=c(0.5, 1, 0, 0))
  plot.window(xlim=c(0,1),ylim=c(0,1))
  text(x=0,y=0.98,"Caractéristiques de la chronique",font=2,adj=0,cex=1.1)
  text(x=0,y=0.8,paste("Nombre de données :",nbr_anal[i]),cex=0.9,adj=0)
  text(x=0,y=0.7,paste("Longueur de la chronique :",long[i],"jours (",round(long[i]/365,1),"années)"),cex=0.9,adj=0)
  text(x=0,y=0.6,paste("Taux de quantification :",round(taux_quanti[i]*100,1),"%"),cex=0.9,adj=0)
  text(x=0,y=0.4,autocor_graph1,cex=0.9,adj=0)
  text(x=0.1,y=0.3,autocor_graph2,cex=0.9,adj=0)
  text(x=0,y=0.2,norm1_graph,cex=0.9,adj=0)
  text(x=0.1,y=0.1,norm2_graph,cex=0.9,adj=0)
  plot.new()
  par(mar=c(1, 0, 0, 0.5) , adj=0.5)
#  par(mar=c(0, 0, 0, 0))
  plot.window(xlim=c(0,1),ylim=c(0,1))
  mat_tend<-matrix(c("Mann-Kendall","Mann-Kendall modifié","Régression linéaire",tendMK_graph1,tendMKmodif_graph1,tendL_graph1,tendMK_graph2,tendMKmodif_graph2,tendL_graph2),nrow=3)
  colnames(mat_tend)<-c("Test","Pente","P-value")
  text(x=0,y=1,"Tendances identifiées sur la longueur totale de la chronique",adj=0,cex=0.85,font=2)
  addtable2plot(x=0,y=0.98,mat_tend,bty="o",hlines=TRUE,vlines=TRUE,display.colnames=TRUE,xjust=0,yjust=0,ypad=1.2,xpad=0.1,cex=0.8)
  mat_rupt<-matrix(c(paste("Changement de moyenne",testrupt_graph),"Inversion de tendance",rupt_graph1,ruptMK_graph1,rupt_graph2,ruptMK_graph2),nrow=2)
  colnames(mat_rupt)<-c("Test","Date","P-value")
  text(x=0,y=0.68,"Ruptures identifiées",adj=0,cex=0.85,font=2)
  addtable2plot(x=0,y=0.66,mat_rupt,bty="o",hlines=TRUE,vlines=TRUE,display.colnames=TRUE,xjust=0,yjust=0,ypad=1.2,xpad=0.2,cex=0.8)
  if (pres_rupt=="oui"){
  text(x=0.05,y=0.41,"Moyenne des données avant/après rupture",adj=0,cex=0.9,font=2)
  mat_moy<-matrix(c("Avant rupture","Après rupture",paste(round(moy1[i],2),unite),paste(round(moy2[i],2),unite)),nrow=2)
  colnames(mat_moy)<-c("","Moyenne")
  addtable2plot(x=0.05,y=0.4,mat_moy,bty="o",hlines=TRUE,vlines=TRUE,display.colnames=TRUE,xjust=0,yjust=0,ypad=1.2,xpad=0.2,cex=0.8)
  }
  if (Zmax[i]!=-1&pval_ruptMK[i]<=0.05&is.na(pval_ruptMK[i])==FALSE){
  text(x=0.05,y=0.15,"Tendance avant/après inversion",adj=0,cex=0.9,font=2)
  mat_tend<-matrix(c("Mann-Kendall avant inversion","Mann-Kendall après inversion",tendMKinv1_graph1,tendMKinv2_graph1,tendMKinv1_graph2,tendMKinv2_graph2),nrow=2)
  colnames(mat_tend)<-c("Test","Pente","P-value")
  addtable2plot(x=0.05,y=0.13,mat_tend,bty="o",hlines=TRUE,vlines=TRUE,display.colnames=TRUE,xjust=0,yjust=0,ypad=1.2,xpad=0.2,cex=0.8)
  }
  par(mar=c(5, 4, 4, 2) + 0.1)
  }
}  #referme boucle sur les points


if (graphiques=="oui") {dev.off()}



#Ecriture du tableau de résultats

table<-cbind(as.vector(liste_pts),format(date_min,"%d/%m/%Y"),format(date_max,"%d/%m/%Y"),nbr_anal,long,pval_shapi,normalite,
    pval_mk,estimate_mk,slope_Sen_an,interc,tend,
    pval_lin,rcarre,pente_lin_an,interc_lin,tend_lin,autocorr_r1,signif_autocorr,
    pval_mkmodif,text_mkmodif,pval_rupt,format(as.Date(rupt,origin="1970-01-01")),text_rupt,
    moy1,moy2,pval_ruptMK,format(as.Date(rupt_MK,origin="1970-01-01")),text_ruptMK,
    slope_Sen1b_an,interc1b,slope_Sen2b_an,interc2b,
    pval_anov,text_anov)
colnames(table)<-c("Identifiant","Date min","Date max","Nbre analyses","Longueur de la chronique (jours)","p-value - test de Shapiro","Normalité de la distribution des données",
    "p-value - test de Mann-Kendall","Tau - test de Mann-Kendall","Pente de Sen - test de Mann-Kendall (unité/an)","Ordonnée à l'origine - test de Mann-Kendall","Tendance - test de Mann-Kendall",
    "p-value - régression linéaire","r carré - régression linéaire","Pente - régression linéaire (unité/an)","Ordonnée à l'origine - régression linéaire","Tendance - régression linéaire","Valeur de l'autocorrélation au rang 1","Significativité de l'autocorrélation",
    "p-value - test de Mann-Kendall modifié","Tendance - test de Mann-Kendall modifié","p-value - test de changement de moyenne","Date rupture - test de changement de moyenne","Test de changement de moyenne",
    "Moyenne tronçon - pré-rupture","Moyenne - tronçon post-rupture","p-value - test d'inversion de pente de Darken","Date rupture - test d'inversion de pente de Darken","Test d'inversion de pente de Darken",
    "Pente de Sen - tronçon pré-rupture (unité/an)","Ordonnée à l'origine - tronçon pré-rupture","Pente de Sen - tronçon post-rupture (unité/an)","Ordonnée à l'origine - tronçon post-rupture",
    "pvalue du test de variabilité","Analyse de la variabilité entre saisons")
sortie<-write.table(table,file=paste(chemin,nom_sortie,".txt",sep=""),row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)

print("Le module tendances et ruptures a bien été exécuté. Les fichiers résultats ont été créés dans le répertoire contenant votre fichier de données.")

