library(glmnet)
library(ISLR)

mydata <- read.csv("C:/Users/nourg/OneDrive/Bureau/MEMOIRE/S15/ONP.csv")
dim(mydata)

noms <-names(mydata) # Nom des variables caractéristiques 
head(noms)

mydata[,61] <- as.numeric(mydata[,61])
mydata <- mydata[,-1] # Nous supprimons la colonne des liens "URL"
noms <-names(mydata) 

# Histogramme de distributions des variables (pour détecter les outliers, les valeurs qui pourraient poser problème)
par(mfrow=c(3,4))
for(i in 1:length(mydata))
  {hist(mydata[,i], ylab="Fréquences", xlab=names(mydata)[i], main="Histogramme de distribution de la variable")}

par(mfrow=c(1,2))
hist(mydata[,2], main="Histogramme de distribution de la variable 'Nb de mots dans le titre'",breaks = 50, col = "skyblue", border = "white")
hist(mydata[,3], main="Histogramme de distribution de la variable 'Nb de mots dans l'article'",breaks = 50, col = "skyblue", border = "white")

# Après avoir parcouru et analysé tous les histogrammes de distributions, nous avons pu identifier diverses problèmes:

par(mfrow=c(1,2))
hist(mydata[,4], main="Histogramme de distribution de la variable 'Taux de mots uniques'",breaks = 50, col = "skyblue", border = "white") 
# Présence d'outliers ("problème d'échelle"): nous supprimons ces observations

mydata <- mydata[mydata[,4]<1,] #ce sont des taux donc la valeur doit être < 1


#Pareil pour les variables 5 et 6 : "n_non_stop_words" et "n_non_stop_unique_tokens" 
mydata <- mydata[mydata[,5]<1,]
mydata <- mydata[mydata[,6]<1,]

noms <-names(mydata) #Mise à jour de la variable Noms 

hist(mydata[,4], main="Histogramme de distribution de la variable 'Taux de mots uniques'",breaks = 50, col = "skyblue", border = "white") 
# L'histogramme est plus cohérent 


# Les valeurs manquantes correspondent à des 0.
# Il faut donc savoir différencier la valeur manquante de la vraie donnée.
# A nouveau, en analysant les histogrammes, nous identifions les variables pour lequelles il existe des observations = 0 incohérentes 
# (incohérence par rapport à la stat concernée par exemple)


# Par exemple 

par(mfrow=c(1,3))
hist(mydata[,11]) # Longueur moyenne des mots (elle ne peut pas valoir 0)
hist(mydata[,45]) # Perception globale de la polarité du texte
hist(mydata[,44]) # Subjectivité globale du texte 
# etc. 

# Nous supprimons ces variables 

for(i in c(11,20,44,45,46,48,49,50,53))
  {mydata <- mydata[mydata[,i]!=0,]} 
noms <-names(mydata)

#_______________________________________________________


# Nous identifions les données asymétriques à droite : 
head(mydata[,3])
head(mydata$shares)
# Nous allons donc les transformer pour réduire l'asymétrie. 
# Pour les variables dont toutes les valeurs sont supérieures à 0, nous prenons le log.
# Pour les autres variables qui peuvent admettre des 0, nous prenons la racine carré.
# Nous les renommons en ajoutant le préfixe log_ pour garder en tête cette transformation 


for(i in c(3,7,8,9,10,22,26:30,39:43,47, 60)){
  
  if(!sum(mydata[,i]==0)){
    
    mydata[,i] <- log(mydata[,i]); names(mydata)[i] <- paste("log_",names(mydata)[i], sep="")}
  
  else{
    
    mydata[,i] <- sqrt(mydata[,i]); names(mydata)[i] <- paste("sqrt_",names(mydata)[i], sep="")}
}

par(mfrow=c(2,2))
hist(mydata[,19]) 
hist(mydata[,21])
hist(mydata[,23])
hist(mydata[,25])
# Ces variables contiennent des valeurs observées négatives --> Incohérent (erreur de saisie)
# Nous les supprimons. 

mydata <- mydata[, -c(19,21,23,25)]
noms <-names(mydata)

#________________________________ 

# Peut-on omettre les variables relatives au contenu ? 

#  ANALYSE 

ref_lifestyle <- c(which(mydata[,13]==1))
ref_enter <- c(which(mydata[,14]==1))
ref_bus <- c(which(mydata[,15]==1))
ref_socmed <- c(which(mydata[,16]==1))
ref_tech <- c(which(mydata[,17]==1))
ref_world <- c(which(mydata[,18]==1))
ref_autres <- -c(ref_lifestyle,ref_bus,ref_enter,ref_socmed,ref_tech,ref_world)

log_shares <- mydata$log_shares


sub<- list(log_shares[ref_lifestyle],
           log_shares[ref_enter],
           log_shares[ref_bus],
           log_shares[ref_socmed],
           log_shares[ref_tech],
           log_shares[ref_world],
           log_shares[ref_autres])
par(mfrow=c(1,1))
boxplot(sub,xlab="Domaines",main="Nombre de partages de l'article en fonction du contenu",ylab="Log_partages",names=c("Lifestyle",
                                                        "Divertissement",
                                                        "Bus",
                                                        "Réseaux Sociaux",
                                                        "Tech",
                                                        "Monde",
                                                        "Autres"),col=c("cadetblue2"))

# Les niveaux des boxplots diffèrent d'un sujet à l'autre, notamment Lifestyle et Réseaux Sociaux
# Nous les gardons. 


# Peut-on supprimer les variables relatives au jour de publication ? Celles-ci sont du type :
head(mydata$weekday_is_friday)

# ANALYSE : 

mond <- which(mydata$weekday_is_monday==1)
tues <- which(mydata$weekday_is_tuesday==1)
wed <- which(mydata$weekday_is_wednesday==1)
thurs <- which(mydata$weekday_is_thursday==1)
fri <- which(mydata$weekday_is_friday==1)
sat <- which(mydata$weekday_is_saturday==1)
sun <- which(mydata$weekday_is_sunday==1)
we <-  which(mydata$is_weekend==1)

log_shares <- mydata$log_shares


pub_day <- list(log_shares[mond],
           log_shares[tues],
           log_shares[wed],
           log_shares[thurs],
           log_shares[fri],
           log_shares[sat],
           log_shares[sun],
           log_shares[we])

boxplot(pub_day,main="Nombre de partages de l'article en fonction du jour de publication", xlab="Jour de la semaine",ylab="Log_partages",names=c("Lundi",
                                                                       "Mardi",
                                                                       "Mercredi",
                                                                       "Jeudi",
                                                                       "Vendredi",
                                                                       "Samedi",
                                                                       "Dimanche",
                                                                       "Week-end"),col=c("cadetblue2"))

# Pas de différences entre lundi, mardi, mercredi, jeudi vendredi 
# Boxplots élevés le week-end 
# Nous gardons seulement l'information semaine VS Week-End

names(mydata)[27:34]
mydata <- mydata[,-c(27:33)]

mydata <- mydata[,-c(28:32)]

dim(mydata)
noms <-names(mydata)

# Nous préparons la matrice X des observations :
X <- as.matrix(mydata)
ncol_shares <- ncol(X) # Num de colonne de la variable à expliquer 
X <- X[,-ncol_shares] # On ne garde que les covariables 
dim(X)


# IMPLEMENTATION DE LA PROCEDURE LASSO 

(lasso.mod <- glmnet(X,mydata$log_shares, alpha=1)) #Pour différentes valeurs de Lambda générées par défaut 

# La grille générée par défaut est : 
(lasso.mod$lambda)
beta <- as.matrix(lasso.mod$beta)


# Le nombre de variables sélectionnées en fonction de lambda : 
plot(lasso.mod$lambda,lasso.mod$df,type='s',col="red",main="Evolution du nombre de variables sélectionnées en fonction de lambda", xlab="Lambda",ylab="Nb de variables sélectionnées") #s for step 



# VALIDATION CROISEE 

(lasso.mod.cv <- cv.glmnet(X,mydata$log_shares,alpha=1,nfolds=5))
# Erreur de validation croisée 
plot(lasso.mod.cv,main="Lasso",ylab="Erreur quadratique moyenne",main=" ")


(lambda.opt.lasso <- lasso.mod.cv$lambda.min)
(coeffs.lasso.cv <- coef(lasso.mod.cv,s="lambda.min"))
