Ligne 1 à 6: On importe les bibliothèques nécessaire

Ligne 9: On charge le fichier .csv

Ligne 11 à 33: on nettoie les données et on observe ses propriétés 

Ligne 37 à 47: On cherche à tracer l'histogramme des effectifs des chantiers perturbants par arrondissement


Ligne 11 à 33:
#On voudrait trouver un moyen de mesurer la penibilité de la présence de travaux à Paris
#On fixe une echelle de confort allant de 0 à 20 (0= péinibilité max, 20=confort max)
#0/20= on est dans une zone très proche de travaux perturbants
#20/20= on est dans une zone éloignée aux max des travaux perturbants

#On commence par récupérer les coordonnées géographiques des travaux 
list1<-list()

for (coord in data["geo_point_2d"])
{
  list1<-str_split(coord, ",")
}

n=length(list1)
Lats=1:n*c(0)
Longs=1:n*c(0)

for (k in seq(n))
{
  print(k)
  Longs[k]=as.double(list1[[k]][[2]])
  Lats[k]=as.double(list1[[k]][[1]])
}
#Pour des raisons inconnues, on ne parvient à obtenir que les 113 premières coordonnées

#On va calculer la distance de chaque chantier par rapport aux autres
#D1= distance du chantier 1 par rapport aux autres
#D2= distance du chantier 2 par rapport aux autres
#D3= distance du chantier 3 par rapport aux autres
#... et ainsi de suite

D=1:n*c(0)

for (k in 1:n)
{
  d=0
  for (q in 1:n)
  {
    d=d+(Longs[i]-Longs[k])^2+(Lats[i]-Lats[k])^2
    D[k]=d
  } 
} 

D=D[1:113]
D=sort(D)
D_max=max(D) #distance max
D_min=min(D) #distance min
delta=(D_max-D_min)

delta

#A présent on implemente la fonction qui va arbitrairement mesurer le confort d'une zone d'habitation
#sur une échelle de 0 à 20

f_confort <- function(d){
  (abs(d-D_min)/delta)*20
 
}

#On stocke les images de D par f_confort dans une nouvelle liste Im_f

n=length(D)
Im_f=1:n*c(0)

for (k in 1:n)
{
  Im_f[k]=f_confort(D[k])
} 

#On crée la dataframe
df=data.frame(distance=D,confort=Im_f)

#On trace le graphe
ggplot(df) + geom_bin2d(aes(x = distance, y = confort),  bins = 50)

#On également tracer le boxplot
ggplot(df) + geom_boxplot(aes(x = distance, y = confort))+ geom_jitter(aes(x = distance, y = confort),  col = "red", alpha = 0.2)




#Maintenant, on va afficher les travaux perturbants sur une carte de Paris 
Longs=Longs[1:113]
Lats=Lats[1:113]

list2<-list()

for (obj in data["Objet"])
{
  list2<-obj
}

description=list2[1:113]

df2=data.frame(long=Longs,lat=Lats, des=description)
#Premierement on affiche la carte centrée sur Paris
Paris <- leaflet() %>% setView(lng = 2.34029006958, lat = 48.872505188, zoom = 12) %>% 
  addTiles()
Paris %>% addProviderTiles("Wikimedia")    

#A présent on place les travaux perturbants sur la carte
leaflet(df2) %>% addTiles() %>%
  addMarkers(~long, ~lat,popup = ~des)






