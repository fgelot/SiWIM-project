
# L'outil SiWIM

SiWIM signifie "Slovenian Weigh-in-Motion", soit pesage en marche slovéne. Il s'agit du seul système de pesage par pont (B-WIM pour Bridge Weigh-in-Motion) commercialisé à ce jour, suite à l'action COST323 et au projet WAVE du $4^{eme}$ PCRD.

Pour en savoir plus: https://www.cestel.eu/105/history

# Le pesage par pont instrumenté

## Le principe du B-WIM

Le principe du B-WIM a été proposé par Fred Moses dans les années 1970 (Fred Moses, Weigh-in-motion system using instrumented bridges, Transportation Engineering Journal, 105(3) :233–249, 1979). 

En comportement élastique linéaire, la déformation mesurée sur un pont au pesage d'un charge sera proportionnelle à cette charge. 

![Principe du pesage par ponts.](images/principe.png)


Par calcul inverse à partir des déformations, les poids et dimensions du trafic peuvent être évalués. Pour cela, il s'agit de minimiser: 

$$E=\displaystyle{\Sigma_{k=1}^T [M(t_k)-M^*(t_k)]^2}$$
où: 

+ $M(t_k)$est la réponse calculée,
+ $M^*(t_k)$ est la réponse mesurée.

## Les composants du B-WIM

Un système de pesage par pont instrumenté sera ainsi constitué des unités suivantes: 
+ Capteurs de déformations, 
+ sonde de température, 
+ centrale d'acquisition et de calcul, 
+ borne serveur qui communique les données calulées à un serveur accessible à distance, 
+ caméra, 
+ et éléments de connectique (câbles).

![Capteur de déformation.](images/capteur.png)
![Unité d'acquisition et de calcul.](images/centrale.png)
![Caméra.](images/camera.png)

# Cas du pont de Normandie

Suite à une demande du maitre d'ouvrage, un système de pesage par pont instrumenté est actuellement installé sur/dans le pont de Normandie. 

![Pont de Normandie: profil en long.](images/normandie1.png)
![Pont de Normandie: instrumentation.](images/normandie2.png)

Tous les poids et dimensions des véhicules du trafic sont estimés et enregistrés. 

# Problématiques

Après une récupération des données (web scraping) et une préparation de celles-ci, nous nous sommes intéressés aux problématiques suivantes: 
+ Est-il possible de retrouver de façon statistique les silhouettes de poids lourds usuelles? (effets différents sur les ouvrages, chargement différen, péages adaptés, ..)
Pour cela, une ACP et du clustering ont été réalisés.
+ Est-il possible de prédire le trafic futur, et si oui sous quelles conditions? (liens avec l'endommagement de l'infrastructure, l'évolution financière liée aux péages, ...)
Une analyse des séries temporrelles a été réalisée. 
+ Est-il possible de distinguer les poids lourds en anomalies, sachant que l'anomalie peut être physique (chargement) ou liée au système (calcul inverse)?
Pour cela, une détection des otuliers a été réalisée, avec mise en lien avec des données de type calage des modèles. 

# Auteurs

+ Jérémy Arthaud,
+ Frédéric Gelot,
+ Franziska Schmidt. 