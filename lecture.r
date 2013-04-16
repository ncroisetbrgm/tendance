#Initialisation
rm(list=ls())
rm(list=ls(all.names=TRUE))


print("S�lectionnez le fichier texte contenant vos donn�es.")

fichier<-choose.files(default="",caption="Selectionnez le fichier texte contenant vos donn�es")
nom_fichier<- sub("(.*\\\\)(.*?$)",'\\2',fichier)
chemin<- sub("(.*\\\\)(.*?$)",'\\1',fichier)


print("Quel s�parateur est utilis� dans le fichier texte contenant les donn�es ?")
print("Tabulation, tapez 1. Caract�re |, tapez 2.")
separateur<-scan(what="",nlines=1)

if (separateur==1) {separateur<-"\t"} else {separateur<-"|"}

titre <-scan(fichier,what=character(),comment.char="",nlines=1,
    sep=separateur, na.strings="", dec=".", strip.white=FALSE,quote="")

Data_test <-
  read.table(fichier,comment.char="",
   header=FALSE,skip=1, sep=separateur,row.names=NULL,na.strings="", dec=".", strip.white=FALSE,quote="")




colnames(Data_test)<-titre

nom_fichier_ssextension<-substr(nom_fichier,1,nchar(nom_fichier)-4)
 
print("Vos donn�es sont charg�es. Vous pouvez � pr�sent ex�cuter un module.")
