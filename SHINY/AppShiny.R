#install.packages(c("shiny", "httr", "jsonlite", "dplyr", "ggplot2", 
 #             "corrplot", "leaflet", "htmlwidgets", "DT", "shinymanager","sf"))
              library(shiny)
              library(httr)
              library(jsonlite)
              library(dplyr)
              library(ggplot2)
              library(corrplot)
              library(leaflet)
              library(htmlwidgets)
              library(DT)
              library(shinymanager)
              library(sf)
             
              # Fonction de conversion Lambert-93 vers WGS84
              convert_lambert93_to_wgs84 <- function(x, y) {
                library(sp)
                install.packages("rgdal")
                coords <- data.frame(x = as.numeric(x), y = as.numeric(y))
                coordinates(coords) <- c("x", "y")
                proj4string(coords) <- CRS("+init=epsg:2154")  # Lambert-93
                coords <- spTransform(coords, CRS("+proj=longlat +datum=WGS84"))  # WGS84
                
                return(data.frame(lon = coordinates(coords)[, 1], lat = coordinates(coords)[, 2]))
              }
              
              # Fonction pour récupérer les données de l'API pour le département 69 uniquement (logements existants)
              get_data_69_existants <- function() {
                base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"
                all_data <- data.frame()
                page <- 1000
                page_size <- 10
                has_more_data <- TRUE
                
                while (has_more_data) {
                  params <- list(
                    page = page,
                    size = page_size,
                    select = "N°DPE,Etiquette_DPE,Date_réception_DPE,Code_postal_(BAN),Surface_habitable_logement,Coût_chauffage,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN)",
                    q = "69*",  # Filtrer les codes postaux commençant par 69
                    q_fields = "Code_postal_(BAN)"  # Champ spécifique à filtrer
                  )
                  
                  url_encoded <- modify_url(base_url, query = params)
                  response <- GET(url_encoded)
                  
                  if (status_code(response) == 200) {
                    data_json <- content(response, "text")
                    df_page <- fromJSON(data_json)$result
                    
                    if (!is.null(df_page) && nrow(df_page) > 0) {
                      all_data <- rbind(all_data, df_page)
                    }
                    
                    if (nrow(df_page) < page_size) {
                      has_more_data <- FALSE
                    } else {
                      page <- page + 1
                    }
                  } else {
                    print(paste("Erreur à la page", page, ":", status_code(response)))
                    has_more_data <- FALSE  # Arrêter la boucle si une erreur survient
                  }
                }
                
                return(all_data)
              }
              
              # Fonction pour récupérer les données de l'API pour les logements neufs (département 69)
              get_data_69_neufs <- function() {
                base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-neufs/lines"
                all_data <- data.frame()
                page <- 1
                page_size <- 1000
                has_more_data <- TRUE
                
                while (has_more_data) {
                  params <- list(
                    page = page,
                    size = page_size,
                    select = "N°DPE,Etiquette_DPE,Date_réception_DPE,Code_postal_(BAN),Surface_habitable_logement,Coût_chauffage,Coordonnée_cartographique_X_(BAN),Coordonnée_cartographique_Y_(BAN)",
                    q = "69*",  # Filtrer les codes postaux commençant par 69
                    q_fields = "Code_postal_(BAN)"  # Champ spécifique à filtrer
                  )
                  
                  url_encoded <- modify_url(base_url, query = params)
                  response <- GET(url_encoded)
                  
                  if (status_code(response) == 200) {
                    data_json <- content(response, "text")
                    df_page <- fromJSON(data_json)$result
                    
                    if (!is.null(df_page) && nrow(df_page) > 0) {
                      all_data <- rbind(all_data, df_page)
                    }
                    
                    if (nrow(df_page) < page_size) {
                      has_more_data <- FALSE
                    } else {
                      page <- page + 1
                    }
                  } else {
                    print(paste("Erreur à la page", page, ":", status_code(response)))
                    has_more_data <- FALSE  # Arrêter la boucle si une erreur survient
                  }
                }
                
                return(all_data)
              }
              
              # Interface utilisateur
              credentials <- data.frame(
                user = c("shiny", "shinymanager"), # mandatory
                password = c("azerty", "12345"), # mandatory
                admin = c(FALSE, TRUE),
                comment = "Connection Valide",
                stringsAsFactors = FALSE)
              
              ui <- fluidPage(
                tags$h2("My secure application"),
                verbatimTextOutput("auth_output"),
                
                
                # Sélecteur de fichier CSS
                selectInput("css_file", "Choisir un thème :", 
                            choices = c("Clair" = "css_1.css", "Sombre" = "css_2.css"), 
                            selected = "css_1.css"),
                
                # Dynamically include the selected CSS file
                tags$head(
                  uiOutput("css_link")
                ),
                
                titlePanel("Application DPE - Analyse des logements existants et neufs (69)"),
                
                sidebarLayout(
                  sidebarPanel(
                    actionButton("get_data", "Récupérer les données pour le département 69"),
                    
                    # Filtre pour choisir entre anciens, neufs, ou les deux
                    selectInput("type_logement", "Type de logement", 
                                choices = c("Tous", "ancien", "neuf"), selected = "Tous"),
                    
                    # Ajout des filtres dynamiques
                    selectInput("etiquette_dpe", "Filtrer par Etiquette DPE", 
                                choices = c("Tous", "A", "B", "C", "D", "E", "F", "G"), selected = "Tous"),
                    sliderInput("surface_range", "Filtrer par surface habitable (m²)", 
                                min = 0, max = 200, value = c(0, 200)),
                    sliderInput("cout_chauffage_range", "Filtrer par coût de chauffage (€)", 
                                min = 0, max = 3000, value = c(0, 3000)),
                    
                    # Boutons de téléchargement
                    downloadButton("download_csv", "Télécharger en CSV"),
                    downloadButton("download_plot", "Télécharger graphique en PNG")
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Contexte", uiOutput("contexte")),
                      tabPanel("KPI - Synthèse des Données et Tableau interactif", uiOutput("kpi_ui"), DTOutput("data_table")),
                      tabPanel(
                        "Régression linéaire", 
                        selectInput("x_var", "Choisir la variable X", 
                                    choices = c("Surface habitable" = "Surface_habitable_logement", 
                                                "Coût du chauffage" = "Coût_chauffage", 
                                                "Étiquette DPE" = "Etiquette_DPE", 
                                                "Longitude" = "lon", 
                                                "Latitude" = "lat")),
                        selectInput("y_var", "Choisir la variable Y", 
                                    choices = c("Surface habitable" = "Surface_habitable_logement", 
                                                "Coût du chauffage" = "Coût_chauffage", 
                                                "Étiquette DPE" = "Etiquette_DPE", 
                                                "Longitude" = "lon", 
                                                "Latitude" = "lat")),
                        plotOutput("regression_plot")
                      ),
                      tabPanel("Répartition du coût par DPE", plotOutput("boxplot_dpe")),
                      tabPanel("Corrélogramme", plotOutput("corr_plot")),
                      tabPanel("Distribution des coûts de chauffages", plotOutput("hist_cout_chauffage")),
                      tabPanel("Carte des logements", leafletOutput("map"))
                    )
                  )
                )
              )
              ui <- secure_app(ui)
              # Serveur
              server <- function(input, output, session) {
                # check_credentials returns a function to authenticate users
                res_auth <- secure_server(
                  check_credentials = check_credentials(credentials)
                )
                
                output$auth_output <- renderPrint({
                  reactiveValuesToList(res_auth)
                })
                
                # your classic server logi
                
                # Créer un lien dynamique vers le fichier CSS sélectionné
                output$css_link <- renderUI({
                  tags$link(rel = "stylesheet", type = "text/css", href = input$css_file)
                })
                # Créer une variable réactive pour stocker les données fusionnées
                data <- reactiveVal(data.frame())
                
                # Récupérer les données quand l'utilisateur clique sur le bouton
                observeEvent(input$get_data, {
                  existants_data <- get_data_69_existants()  # Appel à la fonction pour les logements existants
                  neufs_data <- get_data_69_neufs()  # Appel à la fonction pour les logements neufs
                  
                  # Ajouter la colonne "Logement" pour différencier existants et neufs
                  existants_data$Logement <- "ancien"
                  neufs_data$Logement <- "neuf"
                  
                  # Fusionner les données sur les colonnes communes
                  colonnes_communes <- intersect(names(existants_data), names(neufs_data))
                  merged_data <- rbind(existants_data[, colonnes_communes], neufs_data[, colonnes_communes])
                  
                  print(colnames(merged_data))  # Afficher les noms de colonnes
                  
                  # Conversion des coordonnées Lambert-93 en WGS84
                  coords <- convert_lambert93_to_wgs84(merged_data$`Coordonnée_cartographique_X_(BAN)`, 
                                                       merged_data$`Coordonnée_cartographique_Y_(BAN)`)
                  
                  # Ajouter les colonnes de longitude et latitude au dataframe
                  merged_data$lon <- coords$lon
                  merged_data$lat <- coords$lat
                  
                  data(merged_data)  # Stocker les données fusionnées dans la variable réactive
                })
                
                # Filtrer les données en fonction des inputs de l'utilisateur
                filtered_data <- reactive({
                  df <- data()
                  
                  # Vérifier si le dataframe est vide
                  if (nrow(df) == 0) {
                    return(data.frame())
                  }
                  
                  # Appliquer le filtre pour le type de logement
                  if (input$type_logement != "Tous") {
                    df <- df %>% filter(Logement == input$type_logement)
                  }
                  
                  # Vérifier si la colonne 'Surface_habitable_logement' existe avant d'appliquer le filtre
                  if ("Surface_habitable_logement" %in% colnames(df)) {
                    df <- df %>% filter(Surface_habitable_logement >= input$surface_range[1] & Surface_habitable_logement <= input$surface_range[2])
                  } else {
                    print("Colonne 'Surface_habitable_logement' manquante dans les données.")
                  }
                  
                  # Vérifier si la colonne 'Coût_chauffage' existe avant d'appliquer le filtre
                  if ("Coût_chauffage" %in% colnames(df)) {
                    df <- df %>% filter(Coût_chauffage >= input$cout_chauffage_range[1] & Coût_chauffage <= input$cout_chauffage_range[2])
                  } else {
                    print("Colonne 'Coût_chauffage' manquante dans les données.")
                  }
                  
                  # Filtrer selon l'étiquette DPE si un filtre est appliqué
                  if (input$etiquette_dpe != "Tous") {
                    df <- df %>% filter(Etiquette_DPE == input$etiquette_dpe)
                  }
                  
                  return(df)
                })
                
                # Calculer les données de contexte et les KPI
                context_data <- reactive({
                  df <- filtered_data()
                  
                  total_logements <- nrow(df)
                  
                  # Calcul du pourcentage de logements anciens et neufs
                  if (total_logements > 0) {
                    pct_anciens <- round((nrow(df %>% filter(Logement == "ancien")) / total_logements) * 100, 1)
                    pct_neufs <- round((nrow(df %>% filter(Logement == "neuf")) / total_logements) * 100, 1)
                  } else {
                    pct_anciens <- 0
                    pct_neufs <- 0
                  }
                  
                  # Déterminer la catégorie DPE majoritaire
                  if (total_logements > 0) {
                    categorie_majoritaire <- df %>%
                      count(Etiquette_DPE) %>%
                      top_n(1, n) %>%
                      pull(Etiquette_DPE)
                  } else {
                    categorie_majoritaire <- "Aucune"
                  }
                  
                  list(
                    total_logements = total_logements,
                    pct_anciens = pct_anciens,
                    pct_neufs = pct_neufs,
                    categorie_majoritaire = categorie_majoritaire
                  )
                })
                
                # Afficher les KPI dans l'onglet Tableau interactif
                output$kpi_ui <- renderUI({
                  context <- context_data()
                  
                  HTML(paste(
                    "<h3>KPI - Synthèse des Données</h3>",
                    "<ul>",
                    "<li><strong>Total de logements :</strong> ", context$total_logements, "</li>",
                    "<li><strong>Pourcentage de logements anciens :</strong> ", context$pct_anciens, "%</li>",
                    "<li><strong>Pourcentage de logements neufs :</strong> ", context$pct_neufs, "%</li>",
                    "<li><strong>Catégorie DPE majoritaire :</strong> ", context$categorie_majoritaire, "</li>",
                    "</ul>"
                  ))
                })
                
                # Output pour l'onglet Contexte
                output$contexte <- renderUI({
                  context <- context_data()
                  
                  HTML(paste(
                    "<h3>Contexte des Données DPE du Département 69 (Rhône)</h3>",
                    # Ajout des images avec des dimensions adaptées
                    div(
                      tags$img(src = "https://upload.wikimedia.org/wikipedia/fr/thumb/7/77/Logo_enedis_header.png/1200px-Logo_enedis_header.png", 
                               height = "100px", width = "300px"),
                      tags$img(src = "data:image/jpeg;base64,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", 
                               height = "100px", width = "300px"),
                      style = "text-align: center; margin-bottom: 20px;"
                    ),
                    "<p>Les Diagnostics de Performance Énergétique (DPE) sont des outils essentiels pour évaluer l'efficacité énergétique des logements. Ils permettent de classer les bâtiments sur une échelle de A à G, où A représente une très bonne performance énergétique et G la plus faible. Ces informations sont primordiales pour guider les propriétaires et les locataires dans leurs décisions, notamment en ce qui concerne les ventes et locations de biens immobiliers.</p>",
                    "<p>Dans cette application, les données DPE collectées pour le département du Rhône (69) permettent une analyse approfondie des logements, qu'ils soient anciens ou neufs. Les indicateurs disponibles offrent une vue d'ensemble des caractéristiques énergétiques des bâtiments, incluant des informations telles que les coûts de chauffage, la surface habitable, ainsi que les émissions de gaz à effet de serre (GES).</p>",
                    "<p>La performance énergétique des bâtiments est devenue un enjeu majeur dans la lutte contre le changement climatique et la réduction des émissions de CO2. Cette analyse des données DPE du département 69 vise à identifier les zones à fort potentiel d'amélioration, à faciliter la prise de décision en matière de rénovation énergétique et à promouvoir des pratiques plus durables pour les propriétaires et les collectivités locales.</p>",
                    "<p>En explorant ces données, nous pouvons visualiser la répartition des étiquettes énergétiques à travers la région, identifier les tendances liées à la consommation énergétique, et mieux comprendre les disparités géographiques au sein du département du Rhône. Cette contextualisation est essentielle pour soutenir des politiques publiques en matière d'efficacité énergétique et encourager la transition vers des logements plus performants sur le plan énergétique.</p>"
                  ))
                })
                
                # Tableau interactif
                output$data_table <- renderDT({
                  df <- filtered_data()
                  datatable(df, options = list(pageLength = 10, scrollX = TRUE))
                })
                
                # Visualisation de la distribution des surfaces habitables
                output$surface_plot <- renderPlot({
                  df <- filtered_data()
                  if (nrow(df) > 0) {
                    ggplot(df, aes(x = Surface_habitable_logement, fill = Logement)) +
                      geom_histogram(binwidth = 10, alpha = 0.7, position = "dodge") +
                      labs(title = "Distribution des surfaces habitables", x = "Surface habitable (m²)", y = "Nombre de logements") +
                      theme_minimal()
                  }
                })
                
                # Régression linéaire dynamique avec X en orange et Y en bleu
                output$regression_plot <- renderPlot({
                  df <- filtered_data()
                  
                  # S'assurer qu'il y a des données et que les variables sélectionnées existent dans le dataframe
                  if (nrow(df) > 0 && input$x_var %in% colnames(df) && input$y_var %in% colnames(df)) {
                    
                    ggplot(df, aes_string(x = input$x_var, y = input$y_var)) +
                      geom_point(color = "orange", alpha = 0.7) +  # Points pour X en orange
                      geom_point(aes_string(y = input$y_var), color = "blue", alpha = 0.7) +  # Points pour Y en bleu
                      geom_smooth(method = "lm", se = FALSE, color = "red") +  # Régression en rouge
                      labs(title = paste("Régression linéaire entre", input$x_var, "et", input$y_var),
                           x = input$x_var, y = input$y_var) +
                      theme_minimal()
                  }
                })
                
                
                # Boxplot du coût de chauffage selon l'étiquette DPE
                output$boxplot_dpe <- renderPlot({
                  df <- filtered_data()
                  if (nrow(df) > 0) {
                    ggplot(df, aes(x = Etiquette_DPE, y = Coût_chauffage, fill = Logement)) +
                      geom_boxplot(alpha = 0.7) +
                      labs(title = "Coût du chauffage selon l'étiquette DPE", x = "Étiquette DPE", y = "Coût du chauffage (€)") +
                      theme_minimal()
                  }
                })
                
                # Corrélogramme
                output$corr_plot <- renderPlot({
                  df <- filtered_data()
                  if (nrow(df) > 0) {
                    df <- df %>% mutate(Surface_habitable_logement = as.numeric(Surface_habitable_logement),
                                        Coût_chauffage = as.numeric(Coût_chauffage))
                    variables_corr <- df[, c("Coût_chauffage", "Surface_habitable_logement")]
                    matrice_correlation <- cor(variables_corr, use = "complete.obs")
                    corrplot(matrice_correlation, method = "circle")
                  }
                })
                
                # Histogramme des coûts de chauffage
                output$hist_cout_chauffage <- renderPlot({
                  df <- filtered_data()
                  if (nrow(df) > 0) {
                    ggplot(df, aes(x = Coût_chauffage, fill = Logement)) +
                      geom_histogram(binwidth = 50, alpha = 0.7, position = "dodge") +
                      labs(title = "Distribution des coûts de chauffage", x = "Coût du chauffage (€)", y = "Nombre de logements") +
                      theme_minimal()
                  }
                })
                
                # Carte interactive des logements
                output$map <- renderLeaflet({
                  df <- filtered_data()
                  if (nrow(df) > 0) {
                    # Conversion des coordonnées si nécessaire
                    df$lon <- as.numeric(df$lon)
                    df$lat <- as.numeric(df$lat)
                    df_valid <- df[!is.na(df$lon) & !is.na(df$lat), ]
                    
                    # Initialisation de la carte leaflet
                    leaflet(df_valid) %>%
                      addTiles() %>%
                      setView(lng = 2.2137, lat = 46.2276, zoom = 6) %>%  # Centre sur la France
                      setMaxBounds(lng1 = -5, lat1 = 41, lng2 = 9, lat2 = 51) %>%  # Limite à la France métropolitaine
                      addMarkers(
                        lng = ~lon,
                        lat = ~lat,
                        popup = ~paste("Étiquette DPE : ", Etiquette_DPE, "<br>",
                                       "Coût chauffage (€): ", Coût_chauffage, "<br>",
                                       "Surface habitable(en m2): ", Surface_habitable_logement, "<br>",
                                       "Logement: ", Logement),
                        clusterOptions = markerClusterOptions() # Ajout de l'option de clustering
                      ) %>%
                      addCircleMarkers(lng = ~lon, lat = ~lat, radius = 5, color = "blue", fillOpacity = 0.5)
                  }
                })
                
                # Télécharger les données en CSV
                output$download_csv <- downloadHandler(
                  filename = function() {
                    paste("DPE_data_", Sys.Date(), ".csv", sep = "")
                  },
                  content = function(file) {
                    write.csv(filtered_data(), file, row.names = FALSE)
                  }
                )
                
                # Télécharger le graphique en PNG
                output$download_plot <- downloadHandler(
                  filename = function() {
                    paste("plot_surface_vs_cout_", Sys.Date(), ".png", sep = "")
                  },
                  content = function(file) {
                    png(file)
                    plot <- ggplot(filtered_data(), aes(x = Surface_habitable_logement, y = Coût_chauffage)) +
                      geom_point(alpha = 0.7) +
                      geom_smooth(method = "lm", se = FALSE, color = "red") +
                      labs(title = "Surface habitable vs Coût du chauffage", x = "Surface habitable (m²)", y = "Coût du chauffage (€)") +
                      theme_minimal()
                    print(plot)
                    dev.off()
                  }
                )
              }
              
              # Lancer l'application
              shinyApp(ui, server)