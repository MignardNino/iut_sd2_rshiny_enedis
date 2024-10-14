# iut_sd2_rshiny_enedis
Suivi du projet sur les DPE des logements du Rhône

## Application Shiny - Installation et Documentation

### Description
Ce dépôt contient tout le nécessaire pour installer et exécuter une application **Shiny**. L'application permet de visualiser des données et est accompagnée de fichiers CSS pour personnaliser son apparence. Vous trouverez également des documents techniques et fonctionnels pour mieux comprendre son fonctionnement.

### Code d'accés de l'App
Username : `shiny`
Passeword : `azerty`

### Structure du dépôt

- `App_RShiny_installation/`: Dossier complet d'installation
  - `www/`: Dossier contenant les fichiers CSS pour styliser l'application.
    - `css_1.css`: Fichier CSS pour la personnalisation de l'interface.
    - `css_2.css`: Second fichier CSS pour des styles supplémentaires.

  - `AppShiny.R`: Script principal pour lancer l'application **Shiny**.
  - `AppShiny_Packages.R`: Script permettant d'installer les packages R requis pour l'application.

- `Documentation/`: Dossier contenant la documentation associée à l'application.
  - `Documentation Fonctionnelle de l'application.pdf`: Guide fonctionnel décrivant les fonctionnalités principales de l'application.
  - `Documentation technique de l'application.pdf`: Documentation technique détaillant la structure du code et les technologies utilisées.

- `R_Markown/`: Fichier permettant d'inqtaller le RMarkdown
  - `www/`: Dossier contenant le fichier CSS pour styliser le RMarkdown.
    - `R_Markdown.css`: Fichier CSS pour la personnalisation de le RMarkdown.
  - `rapport_dpe`: Script principal pour lancer le script **RMarkdown**.
    
- `README.md`: Ce fichier décrivant l'application et fournissant les instructions d'installation.  
  
- `URL de l'App`: Ce fichier permet d'acceder a l'App via un URL.


### Prérequis
- **R** (version ≥ 4.0)
- **RStudio** (recommandé)
- Les packages R requis sont spécifiés dans le fichier `AppShiny_Packages.R`.
