#Initialisation
nom_sortie_ann<-paste("reech_ann_",nom_fichier,sep="")
nom_sortie_mens<-paste("reech_mens_",nom_fichier,sep="")

#nom_sortie<-"reechantillonnage/recapitulatif"
#nom_sortie_ann<-"reechantillonnage/Data_reech_ann"
#nom_sortie_mens<-"reechantillonnage/Donnees_reech_annuel"

if (is.null(Data_test$CODE_BSS)) {Data_test$CODE_BSS<-Data_test$"Code national BSS"}
if (is.null(Data_test$DATE_DEBUT_PRELEVEMENT)) {Data_test$DATE_DEBUT_PRELEVEMENT<-Data_test$"Date prélèvement"}
if (is.null(Data_test$CODE_SIGNE)) {Data_test$CODE_SIGNE<-Data_test$"Code remarque analyse"}
if (is.null(Data_test$RESULTAT)) {Data_test$RESULTAT<-Data_test$"Résultat de l'analyse"}


#Compte le nombre de points BSS
liste_pts<-unique(Data_test$CODE_BSS)
nb_pts<-length(liste_pts)

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


#Initialisation
date_repere<-vector("numeric",nb_pts)
class(date_repere)<-"Date"
nbr_anal<-vector("numeric",nb_pts)
indic_a_sortir_ann<-c()
pval_anov<-vector("numeric",nb_pts)
tauS_trimes<-vector("numeric",nb_pts)
pvalS_trimes<-vector("numeric",nb_pts)
tauS_mens<-vector("numeric",nb_pts)
pvalS_mens<-vector("numeric",nb_pts)


datean_poursortie<-vector("list",nb_pts)
resultat_poursortie<-vector("list",nb_pts)
coderem_poursortie<-vector("list",nb_pts)
codebss_poursortie<-vector("list",nb_pts)
unite_poursortie<-vector("list",nb_pts)


print("Quelle est la périodicité qui vous intéresse ?")
print("Pour une périodicité annuelle, tapez 1. Pour une périodicité mensuelle, tapez 2.")
period<-scan(what="",nlines=1)

print("L'outil identifie automatiquement la plus longue chronique ayant la périodicité indiquée ou préférez-vous donner une date?")
print("Pour une recherche automatique, tapez 1. Pour entrer une date, tapez 2.")
autom<-scan(what="",nlines=1)
if (autom==2) {
   print("Entrez la date sous la forme jj/mm/aaa")
   date_manuelle<-scan(what="",nlines=1)
   date_manuelle<-as.Date(date_manuelle,format="%d/%m/%Y")
   }
   

if (period==1) {

for (i in 1:nb_pts){

    indic<-which(Data_test$CODE_BSS==liste_pts[i])

    date_an<-as.Date(Data_test$DATE_DEBUT_PRELEVEMENT[indic],format="%d/%m/%Y")
    donnees<-Data_test$RESULTAT[indic]
    date_an<-date_an[which(is.na(donnees)==FALSE)]
    codesigne<-Data_test$CODE_SIGNE[indic]
    codesigne<-codesigne[which(is.na(donnees)==FALSE)]
    donnees<-donnees[which(is.na(donnees)==FALSE)]
    nbr_anal[i]<-length(donnees)

    if (autom==1){

    #Tests de cyclicité dans le prélèvement

        diff_date<-outer(date_an,date_an,"-") #la matrice résultats est une matrice de différence de temps, pas vraiment numérique...
        #il faut transformer la matrice diff_date en numérique
        diff_date<-matrix(as.numeric(diff_date),ncol=nbr_anal[i])
    
        #cyclicité annuelle (+/-30jours)
    
        modulos_ann<-diff_date%%365  #matrice du nb de jours d'éacrts entre chaque analyses 2 à 2 modulo 365
        long_chron_ann<-matrix(round(diff_date/365),ncol=nbr_anal[i]) #matrice du nbre d'année entre chaque analyses 2 à 2
    
        #création d'une matrice qui contient des 1 si périodicité mensuelle et des 0 sinon
        matrice_testann<-matrix(0,ncol=nbr_anal[i],nrow=nbr_anal[i])
        matrice_testann[which(modulos_ann>=335|modulos_ann<=30)]<-1

    
        #création d'une matrice qui contient le nbre d'année par rapport à l'analyse si périodicité annuelle (0 sinon)
        multip_ann<-matrice_testann*long_chron_ann
        diag(multip_ann)<-0.1
#        multip_ann[which(matrice_testann==1&&multip_ann==0)]<-0.1
        nbre_annee<-apply(multip_ann,2,function(x) length(unique(x)))-1 #apply applique un fonction à une partie de la matrice, le chiffre 2 indique que l'on s'intéresse aux colonnes.
        long_totale<-apply(multip_ann,2,function(x) round(max(x)-min(x)))
    
        #Pour toutes les chroniques avec des analyses sur au moins 10 années différentes, on sort un tableau récapitulatif
        indic_chronann<-which(nbre_annee>=10)
    
        if (length(indic_chronann!=0)){
           indic_ok<-which.max(nbre_annee)
           date_ok<-date_an[indic_ok]
           indic_aretenir<-which(matrice_testann[indic_ok,]!=0)
           multip_ann_aretenir<-multip_ann[indic_ok,indic_aretenir]
           multip_ann_aretenir[which(multip_ann_aretenir==0)]<-0.1
           nbre_annee_aretenir<-unique(multip_ann_aretenir)
           date_aretenir<-date_an[indic_aretenir]
           resultat_aretenir<-donnees[indic_aretenir]
           coderem_aretenir<-codesigne[indic_aretenir]

           for (k in 1:length(nbre_annee_aretenir)) {
               indic3<-which(multip_ann_aretenir==nbre_annee_aretenir[k])
               nbre_pouruneannee<-length(indic3)
               datean_poursortie[[i]]<-c(datean_poursortie[[i]],mean(date_aretenir[indic3]))
               resultat_poursortie[[i]]<-c(resultat_poursortie[[i]],mean(resultat_aretenir[indic3]))
               coderem_poursortie[[i]]<-c(coderem_poursortie[[i]],min(coderem_aretenir[indic3]))
               codebss_poursortie[[i]]<-c(codebss_poursortie[[i]],as.character(liste_pts[i]))
               unite_poursortie[[i]]<-c(unite_poursortie[[i]],as.character(unite))
               }
           }
    }


if (autom==2) {
    diff_date_a<-as.numeric(date_manuelle-date_an)
    modulos_ann_a<-diff_date_a%%365
    indic_aretenir_a<-(which(modulos_ann_a>=335|modulos_ann_a<=30))
    long_chron_a<-round(diff_date_a/365)
    multip_ann_aretenir_a<-long_chron_a[indic_aretenir_a]
    nbre_annee_aretenir_a<-unique(multip_ann_aretenir_a)
    date_aretenir_a<-date_an[indic_aretenir_a]
    resultat_aretenir_a<-donnees[indic_aretenir_a]
    coderem_aretenir_a<-codesigne[indic_aretenir_a]
    
    for (k in 1:length(nbre_annee_aretenir_a)) {
        indic3<-which(multip_ann_aretenir_a==nbre_annee_aretenir_a[k])
        nbre_pouruneannee<-length(indic3)
        datean_poursortie[[i]]<-c(datean_poursortie[[i]],mean(date_aretenir_a[indic3]))
        resultat_poursortie[[i]]<-c(resultat_poursortie[[i]],mean(resultat_aretenir_a[indic3]))
        coderem_poursortie[[i]]<-c(coderem_poursortie[[i]],min(coderem_aretenir_a[indic3]))
        codebss_poursortie[[i]]<-c(codebss_poursortie[[i]],as.character(liste_pts[i]))
        unite_poursortie[[i]]<-c(unite_poursortie[[i]],as.character(unite))
        }

    }

}
resultat_sortietot<-unlist(resultat_poursortie)
datean_sortietot<-unlist(datean_poursortie)
coderem_sortietot<-unlist(coderem_poursortie)
codebss_sortietot<-unlist(codebss_poursortie)
unite_sortietot<-unlist(unite_poursortie)

table<-cbind(codebss_sortietot,format(as.Date(datean_sortietot,origin="1970-01-01"),"%d/%m/%Y"),resultat_sortietot,coderem_sortietot,unite_sortietot)
colnames(table)<-c("Code national BSS","Date prélèvement","Résultat de l'analyse","Code remarque analyse","Unité du graphique")
sortie<-write.table(table,file=paste(chemin,nom_sortie_ann,sep=""),row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)

}

if (period==2) {

print("Implémentation à venir...")
}
#for (i in 1:1){

#    indic<-which(Data_test$CODE_BSS==liste_pts[i])

#    date_an<-as.Date(Data_test$DATE_DEBUT_PRELEVEMENT[indic],format="%d/%m/%Y")
#    donnees<-Data_test$RESULTAT[indic]
#    date_an<-date_an[which(is.na(donnees)==FALSE)]
#    codesigne<-Data_test$CODE_SIGNE[indic]
#    codesigne<-codesigne[which(is.na(donnees)==FALSE)]
#    donnees<-donnees[which(is.na(donnees)==FALSE)]
#    nbr_anal[i]<-length(donnees)
#
##Tests de cyclicité dans le prélèvement
#
#    diff_date<-outer(date_an,date_an,"-") #la matrice résultats est une matrice de différence de temps, pas vraiment numérique...
#    #il faut transformer la matrice diff_date en numérique
#    diff_date<-matrix(as.numeric(diff_date),ncol=nbr_anal[i])
#
#    #cyclicité mensuelle (+/-30jours)
#
#    modulos_mens<-diff_date%%30 #matrice du nb de jours d'éacrts entre chaque analyses 2 à 2 modulo 365
#    long_chron_mens<-matrix(round(diff_date/30),ncol=nbr_anal[i]) #matrice du nbre d'année entre chaque analyses 2 à 2
#
#    #création d'une matrice qui contient des 1 si périodicité mensuelle et des 0 sinon
#    matrice_testmens<-matrix(0,ncol=nbr_anal[i],nrow=nbr_anal[i])
#    matrice_testmens[which(modulos_mens>=25|modulos_mens<=5)]<-1
#
#
#    #création d'une matrice qui contient le nbre de mois par rapport à l'analyse si périodicité mensuelle (0 sinon)
#    multip_mens<-matrice_testmens*long_chron_mens
#
#    #on supprime les valeurs négatives de la matrice
#    multip_mens<-ifelse(multip_mens<0,0,multip_mens)
#
#    #d'abord on ordonne les prelevements ensuite, on calcule le pourcentage de prelevments disponibles dans la chroniques (le nombre d'analyses/le nombre de mois de la chronique)
#    ordre_prelev<-apply(multip_mens,2,function(x) as.numeric(factor(x))) #apply applique un fonction à une partie de la matrice, le chiffre 2 indique que l'on s'intéresse aux colonnes.
#    pourc_chron<-(ordre_prelev-1)/multip_mens
#
#    compt_mens<-max(ordre_prelev[which(pourc_chron>=0.8)])
#    per_max<-multip_mens[which(ordre_prelev==compt_mens)]
###    print(paste("Vous disposez d'une chronique mensuelle de ",max(compt_mens)," prélèvements réalisés sur une période de ",per_max," mois." ))
#
###    ecarts_tt<-datex[lower.tri(diff_date)]
###    nb_annuel<-length(which((abs(diff_date)<=400)&(abs(diff_date)>=330)))/2+1
#
##    ecarts=vector("numeric",nbr_anal[i]-1)
#
#
#
#}
}
