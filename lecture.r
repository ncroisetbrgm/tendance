#Initialisation
rm(list=ls())
rm(list=ls(all.names=TRUE))


print("Sélectionnez le fichier texte contenant vos données.")

fichier<-choose.files(default="",caption="Selectionnez le fichier texte contenant vos données")
nom_fichier<- sub("(.*\\\\)(.*?$)",'\\2',fichier)
chemin<- sub("(.*\\\\)(.*?$)",'\\1',fichier)


print("Quel séparateur est utilisé dans le fichier texte contenant les données ?")
print("Tabulation, tapez 1. Caractère |, tapez 2.")
separateur<-scan(what="",nlines=1)

if (separateur==1) {separateur<-"\t"} else {separateur<-"|"}

titre <-scan(fichier,what=character(),comment.char="",nlines=1,
    sep=separateur, na.strings="", dec=".", strip.white=FALSE,quote="")

Data_test <-
  read.table(fichier,comment.char="",
   header=FALSE,skip=1, sep=separateur,row.names=NULL,na.strings="", dec=".", strip.white=FALSE,quote="")




colnames(Data_test)<-titre

nom_fichier_ssextension<-substr(nom_fichier,1,nchar(nom_fichier)-4)
 
print("Vos données sont chargées. Vous pouvez à présent exécuter un module.")
