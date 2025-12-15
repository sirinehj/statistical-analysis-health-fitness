# app.R
library(shiny)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(plotly)
library(corrplot)
library(bslib)
library(fresh)

# Charger les packages nécessaires
library(naniar)
library(readxl)
library(janitor)
library(lubridate)
library(VIM)
library(GGally)
library(tibble)
library(kableExtra)
library(psych)
library(nortest)
library(lmtest)
library(car)
library(broom)
library(caret)
library(randomForest)
library(pROC)

options(shiny.host = "0.0.0.0")
options(shiny.port = 6556)

# Créer un thème personnalisé
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#2c3e50",
    blue = "#3498db",
    green = "#27ae60",
    red = "#e74c3c",
    yellow = "#f39c12",
    purple = "#8e44ad"
  ),
  adminlte_sidebar(
    dark_bg = "#34495e",
    dark_hover_bg = "#2c3e50",
    dark_color = "#ecf0f1"
  ),
  adminlte_global(
    content_bg = "#ecf0f1"
  )
)

# Fonction pour préparer les données (identique au rapport)
preparer_donnees <- function(chemin_fichier) {
  tryCatch({
    # Importer les données
    if (grepl("\\.xlsx$|\\.xls$", chemin_fichier, ignore.case = TRUE)) {
      df_initial <- read_excel(chemin_fichier, na = "")
    } else {
      df_initial <- read.csv(chemin_fichier, na.strings = "")
    }
    
    # Nettoyer les noms de colonnes
    df_initial <- df_initial %>% clean_names()
    
    # Conversion des types
    df_initial <- df_initial %>%
      mutate(
        participant_id = as.integer(participant_id),
        sex = as.factor(sex),
        age = as.integer(age),
        measurement_date = as.Date(measurement_date),
        bmi = as.numeric(bmi),
        percent_body_fat = as.numeric(percent_body_fat),
        hand_grip_strength_kg = as.numeric(hand_grip_strength_kg),
        sit_and_reach_cm = as.numeric(sit_and_reach_cm),
        sit_ups_count = as.integer(sit_ups_count),
        vo2_estimate_ml_per_kg_min = as.numeric(vo2_estimate_ml_per_kg_min),
        measurement_year = year(measurement_date),
        measurement_month = month(measurement_date)
      )
    
    # Encodage du sexe
    df_initial <- df_initial %>%
      mutate(
        sex = dplyr::recode(as.character(sex),
                            "M" = "Male",
                            "F" = "Female"),
        sex = factor(sex)
      )
    
    # Traitement des outliers (comme dans le rapport)
    replace_outliers <- function(x, method = "median") {
      if (!is.numeric(x)) return(x)
      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower <- Q1 - 1.5 * IQR
      upper <- Q3 + 1.5 * IQR
      val <- ifelse(method == "median",
                    median(x, na.rm = TRUE),
                    mean(x, na.rm = TRUE))
      x[x < lower | x > upper] <- val
      return(x)
    }
    
    # Variables à traiter par médiane et moyenne (comme rapport)
    vars_median <- c("percent_body_fat", "hand_grip_strength_kg",
                     "vo2_estimate_ml_per_kg_min", "sit_and_reach_cm")
    vars_mean <- c("bmi", "sit_ups_count")
    
    # Appliquer le traitement des outliers
    df_cleaned <- df_initial %>%
      mutate(across(all_of(vars_median), ~ replace_outliers(.x, "median"))) %>%
      mutate(across(all_of(vars_mean), ~ replace_outliers(.x, "mean")))
    
    # Nettoyage final et création de variables
    df_cleaned <- df_cleaned %>%
      filter(bmi > 10, bmi < 60, age > 18, age < 65) %>%
      mutate(
        categorie_bmi = case_when(
          bmi < 18.5 ~ "Maigreur",
          bmi < 25 ~ "Normal",
          bmi < 30 ~ "Surpoids",
          TRUE ~ "Obésité"
        ),
        categorie_bmi = factor(categorie_bmi, 
                               levels = c("Maigreur", "Normal", "Surpoids", "Obésité")),
        age_group = case_when(
          age < 30 ~ "18-29",
          age < 40 ~ "30-39",
          age < 50 ~ "40-49",
          TRUE ~ "50-64"
        ),
        age_group = factor(age_group),
        fitness_level = case_when(
          vo2_estimate_ml_per_kg_min < 30 ~ "Faible",
          vo2_estimate_ml_per_kg_min < 40 ~ "Moyen",
          TRUE ~ "Élevé"
        ),
        fitness_level = factor(fitness_level, 
                               levels = c("Faible", "Moyen", "Élevé")),
        # Score de condition physique
        fitness_score = (hand_grip_strength_kg/50 * 0.3) + 
          (vo2_estimate_ml_per_kg_min/50 * 0.4) + 
          (sit_ups_count/50 * 0.2) + 
          (sit_and_reach_cm/40 * 0.1),
        fitness_score = round(fitness_score * 100, 1),
        # Âge fitness
        fitness_age = case_when(
          fitness_score >= 80 ~ age - 5,
          fitness_score >= 60 ~ age - 2,
          fitness_score >= 40 ~ age,
          TRUE ~ age + 5
        ),
        age_difference = fitness_age - age,
        # Catégories PBF et Âge pour les tests Chi-deux
        categorie_pbf = case_when(
          percent_body_fat < 20 ~ "Faible",
          percent_body_fat < 30 ~ "Moyen",
          percent_body_fat < 40 ~ "Élevé",
          TRUE ~ "Très élevé"
        ),
        categorie_pbf = factor(categorie_pbf),
        categorie_age_test = case_when(
          age < 30 ~ "Jeunes",
          age < 50 ~ "Adultes",
          TRUE ~ "Seniors"
        ),
        categorie_age_test = factor(categorie_age_test)
      )
    
    return(df_cleaned)
    
  }, error = function(e) {
    stop(paste("Erreur lors de la préparation des données:", e$message))
  })
}

# Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(
    title = div(
      tags$img(src = "https://cdn-icons-png.flaticon.com/512/3467/3467026.png", 
               height = "30px"),
      "Analyse de la Condition Physique - Rapport Complet"
    ),
    titleWidth = 350,
    tags$li(class = "dropdown",
            tags$a(icon("github"), "Code Source", 
                   href = "https://github.com", target = "_blank"))
  ),
  
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      id = "tabs",
      menuItem("📥 Importation Données", tabName = "import", icon = icon("upload"), selected = TRUE),
      menuItem("1. Préparation des données", tabName = "preparation", icon = icon("eraser")),
      menuItem("2. Analyse descriptive", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("3. Tests de normalité", tabName = "normalite", icon = icon("check-circle")),
      menuItem("4. Tests d'hypothèses", tabName = "tests", icon = icon("calculator"),
               menuSubItem("4.1 Tests paramétriques", tabName = "parametriques"),
               menuSubItem("4.2 Tests non-paramétriques", tabName = "nonparametriques")),
      menuItem("5. Modélisation par régression", tabName = "regression", icon = icon("line-chart"),
               menuSubItem("5.1 Régression Force", tabName = "reg_force"),
               menuSubItem("5.2 Régression VO2", tabName = "reg_vo2"),
               menuSubItem("5.3 Régression Sit-ups", tabName = "reg_situps"),
               menuSubItem("5.4 Régression Flexibilité", tabName = "reg_flex")),
      menuItem("6. Modèles de Prédiction", tabName = "prediction", icon = icon("brain"),
               menuSubItem("Calculateur Âge Fitness", tabName = "calculateur"),
               menuSubItem("Modèle Random Forest", tabName = "randomforest")),
      menuItem("7. Visualisations Avancées", tabName = "advanced", icon = icon("eye"))
    ),
    
    # Filtres globaux
    conditionalPanel(
      'input.tabs != "import"',
      hr(),
      h4("🎛️ Filtres de données", style = "color: #2c3e50;"),
      pickerInput(
        "sex_filter",
        "Filtrer par sexe:",
        choices = c("Homme" = "Male", "Femme" = "Female"),
        selected = c("Male", "Female"),
        multiple = TRUE,
        options = list(`actions-box` = TRUE, `selected-text-format` = "count > 1")
      ),
      sliderInput(
        "age_filter",
        "Filtrer par âge:",
        min = 19,
        max = 64,
        value = c(19, 64),
        ticks = TRUE
      ),
      pickerInput(
        "bmi_category_filter",
        "Filtrer par catégorie BMI:",
        choices = c("Toutes", "Maigreur", "Normal", "Surpoids", "Obésité"),
        selected = "Toutes",
        options = list(`actions-box` = TRUE)
      )
    )
  ),
  
  dashboardBody(
    use_theme(mytheme),
    
    tags$head(
      tags$style(HTML("
        /* Styles globaux améliorés */
        body {
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
        }
        
        .content-wrapper {
          background: transparent;
        }
        
        /* Amélioration des boîtes */
        .box {
          border-radius: 15px;
          box-shadow: 0 6px 15px rgba(0,0,0,0.08);
          border: none;
          margin-bottom: 20px;
          transition: all 0.3s ease;
          overflow: hidden;
        }
        
        .box:hover {
          transform: translateY(-5px);
          box-shadow: 0 12px 20px rgba(0,0,0,0.15);
        }
        
        .box-header {
          background: linear-gradient(90deg, #3498db, #2980b9);
          color: white;
          padding: 15px 20px;
          border-bottom: none;
        }
        
        .box-title {
          font-weight: 600;
          font-size: 16px;
        }
        
        /* Cartes de valeur améliorées */
        .small-box {
          border-radius: 12px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
          transition: all 0.3s ease;
          overflow: hidden;
          border: none;
        }
        
        .small-box:hover {
          transform: scale(1.05);
          box-shadow: 0 8px 20px rgba(0,0,0,0.15);
        }
        
        /* Section de titre de phase */
        .phase-header {
          background: linear-gradient(90deg, #2c3e50, #34495e);
          color: white;
          padding: 20px;
          border-radius: 10px;
          margin-bottom: 25px;
          box-shadow: 0 4px 15px rgba(44, 62, 80, 0.3);
        }
        
        .phase-title {
          font-size: 24px;
          font-weight: 700;
          margin: 0;
        }
        
        .phase-subtitle {
          font-size: 14px;
          opacity: 0.9;
          margin-top: 5px;
        }
        
        /* Style pour les cartes d'info */
        .info-card {
          background: white;
          border-radius: 10px;
          padding: 15px;
          margin: 10px 0;
          box-shadow: 0 3px 10px rgba(0,0,0,0.1);
          border-left: 4px solid #3498db;
        }
        
        .info-card .icon {
          font-size: 24px;
          margin-right: 10px;
          float: left;
        }
        
        .info-card .value {
          font-size: 24px;
          font-weight: bold;
          color: #2c3e50;
        }
        
        .info-card .label {
          font-size: 12px;
          color: #7f8c8d;
          text-transform: uppercase;
          margin-top: 5px;
        }
      "))
    ),
    
    tabItems(
      # Tab 1: Importation
      tabItem(
        tabName = "import",
        fluidRow(
          box(
            title = "📥 Chargement des données",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                     fileInput("file", "Choisir le fichier Excel ou CSV",
                               accept = c(".xlsx", ".xls", ".csv"),
                               buttonLabel = icon("folder-open") %>% tagList(" Parcourir..."),
                               placeholder = "Aucun fichier sélectionné")
              ),
              column(6,
                     actionBttn("load_sample", "Charger les données d'exemple (NFA 2015-2019)", 
                                icon = icon("database"), 
                                style = "gradient",
                                color = "success",
                                size = "md",
                                block = TRUE),
                     br(),
                     actionBttn("reset_data", "Réinitialiser", 
                                icon = icon("redo"), 
                                style = "bordered",
                                color = "warning",
                                size = "md",
                                block = TRUE)
              )
            ),
            hr(),
            conditionalPanel(
              condition = "output.data_loaded",
              div(class = "alert alert-success",
                  icon("check-circle"),
                  " Données chargées avec succès !",
                  style = "padding: 10px; border-radius: 5px; margin-bottom: 15px;")
            ),
            uiOutput("data_preview_info")
          )
        ),
        fluidRow(
          box(
            title = "👁️ Aperçu des données brutes",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("data_table_raw") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        )
      ),
      
      # Tab 2: Phase 1 - Préparation des données
      tabItem(
        tabName = "preparation",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 1 : PRÉPARATION DES DONNÉES", class = "phase-title"),
              p("Importation, nettoyage et préparation des données pour l'analyse", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Structure des données après nettoyage",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("data_structure")
          ),
          box(
            title = "🔍 Données manquantes",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("missing_data_plot") %>% 
              withSpinner(type = 4, color = "#f39c12", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📈 Comparaison avant/après traitement des outliers",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("outliers_comparison") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📋 Variables créées",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("created_vars_table") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          )
        )
      ),
      
      # Tab 3: Phase 2 - Analyse descriptive
      tabItem(
        tabName = "descriptive",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 2 : ANALYSE DESCRIPTIVE", class = "phase-title"),
              p("Caractérisation des distributions et relations préliminaires", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Tableau descriptif des variables numériques",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("descriptive_table") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📈 Histogrammes des variables",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            selectizeInput("hist_var_desc", "Variable:", 
                           choices = c("age", "bmi", "percent_body_fat",
                                       "hand_grip_strength_kg", "sit_and_reach_cm",
                                       "sit_ups_count", "vo2_estimate_ml_per_kg_min"),
                           selected = "age"),
            plotlyOutput("histogram_desc") %>% withSpinner(type = 4, color = "#3498db")
          ),
          box(
            title = "👥 Répartition par sexe",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("sex_distribution_desc") %>% withSpinner(type = 4, color = "#f39c12")
          )
        ),
        fluidRow(
          box(
            title = "📊 Comparaison descriptive par sexe",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("sex_comparison_table") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "🔗 Matrice de corrélation (Pearson)",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("correlation_matrix_desc") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "💪 Distribution de la force de préhension par sexe",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("hgs_density") %>% withSpinner(type = 4, color = "#3498db")
          ),
          box(
            title = "📈 Relation Âge - Force de préhension",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("age_strength_relation") %>% withSpinner(type = 4, color = "#3498db")
          )
        )
      ),
      
      # Tab 4: Phase 3 - Tests de normalité
      tabItem(
        tabName = "normalite",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 3 : TESTS DE NORMALITÉ", class = "phase-title"),
              p("Évaluation de la distribution normale des variables", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Test de Shapiro-Wilk",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            DTOutput("shapiro_table") %>% 
              withSpinner(type = 4, color = "#f39c12", size = 1)
          ),
          box(
            title = "📊 Test Anderson-Darling",
            status = "danger",
            solidHeader = TRUE,
            width = 6,
            DTOutput("anderson_table") %>% 
              withSpinner(type = 4, color = "#e74c3c", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📈 QQ-Plots interactifs",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectizeInput("qq_vars_norm", "Variables à visualiser:",
                           choices = c("age", "bmi", "percent_body_fat",
                                       "hand_grip_strength_kg", "sit_and_reach_cm",
                                       "sit_ups_count", "vo2_estimate_ml_per_kg_min"),
                           multiple = TRUE,
                           selected = c("age", "bmi", "vo2_estimate_ml_per_kg_min"),
                           options = list(maxItems = 4)),
            plotlyOutput("qq_plots_norm") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        )
      ),
      
      # Tab 5: Phase 4.1 - Tests paramétriques
      tabItem(
        tabName = "parametriques",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 4.1 : TESTS PARAMÉTRIQUES", class = "phase-title"),
              p("Comparaisons avec hypothèse de normalité", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "🎯 Test F de Fisher - Homogénéité des variances",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            DTOutput("fisher_test_table") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📊 Tests T comparatifs (Homme vs Femme)",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("BMI (Test de Welch)",
                       DTOutput("ttest_bmi_table") %>% withSpinner(type = 4, color = "#27ae60")),
              tabPanel("Souplesse (Test de Student)",
                       DTOutput("ttest_sit_table") %>% withSpinner(type = 4, color = "#27ae60")),
              tabPanel("VO2 estimé (Test de Student)",
                       DTOutput("ttest_vo2_table") %>% withSpinner(type = 4, color = "#27ae60"))
            )
          )
        ),
        fluidRow(
          box(
            title = "📈 Tests T à 1 échantillon",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("ttest_one_sample_table") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📊 ANOVA",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            tabsetPanel(
              tabPanel("ANOVA selon groupes d'âge",
                       DTOutput("anova_age_table") %>% withSpinner(type = 4, color = "#f39c12")),
              tabPanel("ANOVA selon catégories BMI",
                       DTOutput("anova_bmi_table") %>% withSpinner(type = 4, color = "#f39c12")),
              tabPanel("ANOVA selon catégories VO2",
                       DTOutput("anova_vo2_table") %>% withSpinner(type = 4, color = "#f39c12"))
            )
          )
        ),
        fluidRow(
          box(
            title = "📊 Test de proportion (Sexe)",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("prop_test_output")
          ),
          box(
            title = "📈 Test de corrélation Pearson",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("pearson_table") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          )
        )
      ),
      
      # Tab 6: Phase 4.2 - Tests non-paramétriques
      tabItem(
        tabName = "nonparametriques",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 4.2 : TESTS NON-PARAMÉTRIQUES", class = "phase-title"),
              p("Comparaisons sans hypothèse de normalité", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Test de Wilcoxon-Mann-Whitney",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            DTOutput("wilcoxon_table") %>% 
              withSpinner(type = 4, color = "#f39c12", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📊 Test de Kruskal-Wallis",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            DTOutput("kruskal_table") %>% 
              withSpinner(type = 4, color = "#e74c3c", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📊 Test du Chi-deux d'indépendance",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("chisq_table") %>% 
              withSpinner(type = 4, color = "#3498db", size = 1)
          )
        ),
        fluidRow(
          box(
            title = "📈 Test de corrélation de Spearman",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            DTOutput("spearman_table") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          )
        )
      ),
      
      # Tab 7: Phase 5.1 - Régression Force
      tabItem(
        tabName = "reg_force",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 5.1 : RÉGRESSION POUR LA FORCE", class = "phase-title"),
              p("Modélisation de la force de préhension", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Résultats du modèle",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("reg_force_summary")
          )
        ),
        fluidRow(
          box(
            title = "📈 Diagnostic des résidus",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("reg_force_diagnostics", height = "600px") %>% 
              withSpinner(type = 4, color = "#f39c12")
          )
        ),
        fluidRow(
          box(
            title = "📊 Coefficients significatifs",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("reg_force_coefs") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          ),
          box(
            title = "📈 Performance du modèle",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("reg_force_performance")
          )
        )
      ),
      
      # Tab 8: Phase 5.2 - Régression VO2
      tabItem(
        tabName = "reg_vo2",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 5.2 : RÉGRESSION POUR VO2", class = "phase-title"),
              p("Modélisation de la capacité cardiorespiratoire", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Résultats du modèle",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("reg_vo2_summary")
          )
        ),
        fluidRow(
          box(
            title = "📈 Diagnostic des résidus",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("reg_vo2_diagnostics", height = "600px") %>% 
              withSpinner(type = 4, color = "#f39c12")
          )
        ),
        fluidRow(
          box(
            title = "📊 Coefficients significatifs",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("reg_vo2_coefs") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          ),
          box(
            title = "📈 Performance du modèle",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("reg_vo2_performance")
          )
        )
      ),
      
      # Tab 9: Phase 5.3 - Régression Sit-ups
      tabItem(
        tabName = "reg_situps",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 5.3 : RÉGRESSION POUR SIT-UPS", class = "phase-title"),
              p("Modélisation de l'endurance musculaire", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Résultats du modèle",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("reg_situps_summary")
          )
        ),
        fluidRow(
          box(
            title = "📈 Diagnostic des résidus",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("reg_situps_diagnostics", height = "600px") %>% 
              withSpinner(type = 4, color = "#f39c12")
          )
        ),
        fluidRow(
          box(
            title = "📊 Coefficients significatifs",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("reg_situps_coefs") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          ),
          box(
            title = "📈 Performance du modèle",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("reg_situps_performance")
          )
        )
      ),
      
      # Tab 10: Phase 5.4 - Régression Flexibilité
      tabItem(
        tabName = "reg_flex",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 5.4 : RÉGRESSION POUR FLEXIBILITÉ", class = "phase-title"),
              p("Modélisation de la souplesse", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "📊 Résultats du modèle",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            verbatimTextOutput("reg_flex_summary")
          )
        ),
        fluidRow(
          box(
            title = "📈 Diagnostic des résidus",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            plotOutput("reg_flex_diagnostics", height = "600px") %>% 
              withSpinner(type = 4, color = "#f39c12")
          )
        ),
        fluidRow(
          box(
            title = "📊 Coefficients significatifs",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            DTOutput("reg_flex_coefs") %>% 
              withSpinner(type = 4, color = "#27ae60", size = 1)
          ),
          box(
            title = "📈 Performance du modèle",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("reg_flex_performance")
          )
        )
      ),
      
      # Tab 11: Phase 6.1 - Calculateur
      tabItem(
        tabName = "calculateur",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 6.1 : CALCULATEUR D'ÂGE FITNESS", class = "phase-title"),
              p("Estimation de l'âge fitness à partir des mesures", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "🎯 Entrez vos mesures",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            numericInput("pred_age_calc", "Âge (années):", value = 30, min = 18, max = 65),
            numericInput("pred_vo2_calc", "VO2 Max (ml/kg/min):", value = 35, min = 20, max = 60),
            numericInput("pred_grip_calc", "Force de préhension (kg):", value = 35, min = 10, max = 80),
            numericInput("pred_situps_calc", "Nombre de sit-ups:", value = 30, min = 5, max = 60),
            numericInput("pred_flex_calc", "Souplesse (cm):", value = 20, min = -10, max = 40),
            actionBttn("calculate_fitness", "Calculer l'Âge Fitness", 
                       icon = icon("calculator"), 
                       style = "gradient",
                       color = "success",
                       block = TRUE)
          ),
          box(
            title = "📊 Résultats",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            uiOutput("fitness_age_calculator_results")
          )
        )
      ),
      
      # Tab 12: Phase 6.2 - Random Forest
      tabItem(
        tabName = "randomforest",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 6.2 : MODÈLE RANDOM FOREST", class = "phase-title"),
              p("Prédiction du niveau de fitness", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "⚙️ Configuration du modèle",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(6,
                     selectizeInput("rf_vars_pred", "Variables prédictives:",
                                    choices = c("age", "sex", "bmi", "percent_body_fat",
                                                "hand_grip_strength_kg", "sit_and_reach_cm",
                                                "sit_ups_count", "vo2_estimate_ml_per_kg_min"),
                                    multiple = TRUE,
                                    selected = c("age", "sex", "bmi", "vo2_estimate_ml_per_kg_min"))
              ),
              column(6,
                     numericInput("rf_trees_pred", "Nombre d'arbres:", value = 100, min = 50, max = 500),
                     actionBttn("train_rf_model", "Entraîner le modèle", 
                                icon = icon("cogs"), 
                                style = "gradient",
                                color = "warning",
                                block = TRUE)
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "📊 Performance du modèle",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            uiOutput("rf_model_results")
          ),
          box(
            title = "📈 Importance des variables",
            status = "warning",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("rf_variable_importance") %>% 
              withSpinner(type = 4, color = "#f39c12", size = 1)
          )
        )
      ),
      
      # Tab 13: Phase 7 - Visualisations avancées
      tabItem(
        tabName = "advanced",
        fluidRow(
          div(class = "phase-header",
              h2("PHASE 7 : VISUALISATIONS AVANCÉES", class = "phase-title"),
              p("Explorations interactives et multivariées", class = "phase-subtitle")
          )
        ),
        fluidRow(
          box(
            title = "🎨 Visualisation 3D",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(3,
                     selectizeInput("scatter3d_x_adv", "Axe X:", 
                                    choices = c("age", "bmi", "percent_body_fat",
                                                "hand_grip_strength_kg", "sit_and_reach_cm",
                                                "sit_ups_count", "vo2_estimate_ml_per_kg_min",
                                                "fitness_score", "fitness_age"),
                                    selected = "age")
              ),
              column(3,
                     selectizeInput("scatter3d_y_adv", "Axe Y:", 
                                    choices = c("age", "bmi", "percent_body_fat",
                                                "hand_grip_strength_kg", "sit_and_reach_cm",
                                                "sit_ups_count", "vo2_estimate_ml_per_kg_min",
                                                "fitness_score", "fitness_age"),
                                    selected = "bmi")
              ),
              column(3,
                     selectizeInput("scatter3d_z_adv", "Axe Z:", 
                                    choices = c("age", "bmi", "percent_body_fat",
                                                "hand_grip_strength_kg", "sit_and_reach_cm",
                                                "sit_ups_count", "vo2_estimate_ml_per_kg_min",
                                                "fitness_score", "fitness_age"),
                                    selected = "vo2_estimate_ml_per_kg_min")
              ),
              column(3,
                     selectizeInput("scatter3d_color_adv", "Couleur par:", 
                                    choices = c("sex", "age_group", "categorie_bmi", "fitness_level"),
                                    selected = "sex")
              )
            ),
            plotlyOutput("scatter3d_adv") %>% withSpinner(type = 4, color = "#3498db")
          )
        ),
        fluidRow(
          box(
            title = "📊 Matrice de scatter plots",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            plotOutput("scatter_matrix_adv", height = "600px") %>% 
              withSpinner(type = 4, color = "#3498db")
          )
        ),
        fluidRow(
          box(
            title = "📈 Comparaisons par facettes",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            selectizeInput("facet_var_adv", "Variable de facetting:", 
                           choices = c("sex", "age_group", "categorie_bmi", "fitness_level"),
                           selected = "sex"),
            plotlyOutput("faceted_plots_adv") %>% withSpinner(type = 4, color = "#27ae60")
          )
        )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  # Données réactives
  data <- reactiveVal(NULL)
  rf_model <- reactiveVal(NULL)
  regression_models <- reactiveValues(
    force = NULL,
    vo2 = NULL,
    situps = NULL,
    flex = NULL
  )
  
  # Observer pour initialiser avec des données d'exemple
  observe({
    if (is.null(data())) {
      showNotification("Chargement des données d'exemple NFA 2015-2019...", type = "default")
      
      # Créer des données d'exemple similaires au rapport
      set.seed(123)
      n <- 2000
      
      sample_data <- tibble(
        participant_id = 1:n,
        sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6775, 0.3225)),
        age = sample(19:64, n, replace = TRUE),
        measurement_date = sample(seq(as.Date('2015-01-01'), as.Date('2019-12-31'), by="day"), n, replace = TRUE),
        bmi = pmax(15, pmin(37, rnorm(n, 24, 3))),
        percent_body_fat = pmax(26.5, pmin(60, rnorm(n, 48, 7))),
        hand_grip_strength_kg = pmax(7.4, pmin(68, rnorm(n, 36, 11))),
        sit_and_reach_cm = pmax(-15, pmin(34, rnorm(n, 8, 6))),
        sit_ups_count = sample(5:33, n, replace = TRUE),
        vo2_estimate_ml_per_kg_min = pmax(10, pmin(60, rnorm(n, 36, 7)))
      )    
      # Sauvegarder temporairement
      temp_file <- tempfile(fileext = ".csv")
      write.csv(sample_data, temp_file, row.names = FALSE)
      
      tryCatch({
        df <- preparer_donnees(temp_file)
        data(df)
        showNotification("Données d'exemple NFA chargées avec succès!", 
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Erreur:", e$message), type = "error")
      })
    }
  })
  
  # Output pour indiquer si les données sont chargées
  output$data_loaded <- reactive({
    return(!is.null(data()))
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
  
  # Charger les données d'exemple
  observeEvent(input$load_sample, {
    showNotification("Chargement des données d'exemple NFA 2015-2019...", type = "default")
    
    set.seed(123)
    n <- 2000
    
    sample_data <- tibble(
      participant_id = 1:n,
      sex = sample(c("M", "F"), n, replace = TRUE, prob = c(0.6775, 0.3225)),
      age = sample(19:64, n, replace = TRUE),
      measurement_date = sample(seq(as.Date('2015-01-01'), as.Date('2019-12-31'), by="day"), n),
      bmi = pmax(15, pmin(37, rnorm(n, 24, 3))),
      percent_body_fat = pmax(26.5, pmin(60, rnorm(n, 48, 7))),
      hand_grip_strength_kg = pmax(7.4, pmin(68, rnorm(n, 36, 11))),
      sit_and_reach_cm = pmax(-15, pmin(34, rnorm(n, 8, 6))),
      sit_ups_count = sample(5:33, n, replace = TRUE),
      vo2_estimate_ml_per_kg_min = pmax(10, pmin(60, rnorm(n, 36, 7)))
    )
    
    temp_file <- tempfile(fileext = ".csv")
    write.csv(sample_data, temp_file, row.names = FALSE)
    
    tryCatch({
      df <- preparer_donnees(temp_file)
      data(df)
      showNotification("Données NFA chargées avec succès!", 
                       type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  # Réinitialiser les données
  observeEvent(input$reset_data, {
    data(NULL)
    showNotification("Données réinitialisées", type = "message")
  })
  
  # Charger le fichier utilisateur
  observeEvent(input$file, {
    req(input$file)
    
    showNotification("Chargement des données...", type = "default")
    
    tryCatch({
      df <- preparer_donnees(input$file$datapath)
      data(df)
      showNotification("Données chargées avec succès!", 
                       type = "message", duration = 5)
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error", duration = 10)
    })
  })
  
  # Données filtrées
  filtered_data <- reactive({
    req(data())
    
    df <- data()
    
    # Appliquer les filtres
    if (!is.null(input$sex_filter)) {
      df <- df %>% filter(sex %in% input$sex_filter)
    }
    
    if (!is.null(input$age_filter)) {
      df <- df %>% filter(age >= input$age_filter[1] & age <= input$age_filter[2])
    }
    
    if (!is.null(input$bmi_category_filter) && input$bmi_category_filter != "Toutes") {
      df <- df %>% filter(categorie_bmi == input$bmi_category_filter)
    }
    
    return(df)
  })
  
  # PHASE 1: Préparation des données
  output$data_structure <- renderPrint({
    req(filtered_data())
    cat("=== STRUCTURE DES DONNÉES ===\n\n")
    str(filtered_data())
    cat("\n=== INFORMATIONS GÉNÉRALES ===\n")
    cat("Dimensions:", dim(filtered_data())[1], "observations ×", dim(filtered_data())[2], "variables\n")
    cat("Période: ", min(filtered_data()$measurement_date, na.rm = TRUE), "à", 
        max(filtered_data()$measurement_date, na.rm = TRUE), "\n")
    cat("Sexe - Hommes:", sum(filtered_data()$sex == "Male"), 
        "Femmes:", sum(filtered_data()$sex == "Female"), "\n")
  })
  
  output$missing_data_plot <- renderPlotly({
    req(filtered_data())
    
    df <- filtered_data()
    
    missing_counts <- sapply(df, function(x) sum(is.na(x)))
    missing_df <- data.frame(
      Variable = names(missing_counts),
      Missing = missing_counts,
      Percent = round(missing_counts/nrow(df) * 100, 1)
    ) %>%
      filter(Missing > 0) %>%
      arrange(desc(Missing))
    
    if(nrow(missing_df) > 0) {
      p <- plot_ly(missing_df, x = ~reorder(Variable, -Percent), y = ~Percent,
                   type = 'bar',
                   marker = list(color = ~Percent, 
                                 colorscale = 'RdYlBu',
                                 reversescale = TRUE),
                   text = ~paste(Missing, "valeurs manquantes"),
                   hovertemplate = "%{y:.1f}%<br>%{text}<extra></extra>") %>%
        layout(title = "Données manquantes par variable",
               xaxis = list(title = "", tickangle = 45),
               yaxis = list(title = "Pourcentage (%)", range = c(0, 100)),
               margin = list(b = 100))
    } else {
      p <- plot_ly() %>%
        add_annotations(text = "Aucune donnée manquante !",
                        x = 0.5, y = 0.5,
                        xref = "paper", yref = "paper",
                        showarrow = FALSE,
                        font = list(size = 20, color = "green"))
    }
    
    p
  })
  
  output$outliers_comparison <- renderPlotly({
    req(filtered_data())
    
    df <- filtered_data()
    
    vars_to_plot <- c("bmi", "vo2_estimate_ml_per_kg_min", "sit_and_reach_cm")
    
    plot_data <- df %>%
      select(all_of(vars_to_plot)) %>%
      pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
      mutate(variable = factor(variable, levels = vars_to_plot))
    
    p <- ggplot(plot_data, aes(x = variable, y = value, fill = variable)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(title = "Distribution des variables après traitement",
           x = "Variable", y = "Valeur") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$created_vars_table <- renderDT({
    req(filtered_data())
    
    df <- filtered_data()
    
    created_vars <- df %>%
      summarise(
        `Catégorie BMI` = paste(levels(categorie_bmi), collapse = ", "),
        `Groupe d'âge` = paste(levels(age_group), collapse = ", "),
        `Niveau de fitness` = paste(levels(fitness_level), collapse = ", "),
        `Score fitness moyen` = round(mean(fitness_score, na.rm = TRUE), 1),
        `Âge fitness moyen` = round(mean(fitness_age, na.rm = TRUE), 1)
      ) %>%
      t() %>%
      as.data.frame() %>%
      rename(Valeur = 1)
    
    datatable(created_vars,
              options = list(dom = 't', pageLength = 10),
              rownames = TRUE,
              caption = "Variables créées lors de la préparation")
  })
  
  # PHASE 2: Analyse descriptive
  output$descriptive_table <- renderDT({
    req(filtered_data())
    
    df <- filtered_data() %>%
      select(age, bmi, percent_body_fat,
             hand_grip_strength_kg, sit_and_reach_cm,
             sit_ups_count, vo2_estimate_ml_per_kg_min)
    
    desc_stats <- psych::describe(df) %>%
      as.data.frame() %>%
      select(n, mean, sd, median, min, max, skew, kurtosis) %>%
      round(3) %>%
      rename(
        `Effectif` = n,
        `Moyenne` = mean,
        `Écart-type` = sd,
        `Médiane` = median,
        `Minimum` = min,
        `Maximum` = max,
        `Asymétrie` = skew,
        `Aplatissement` = kurtosis
      )
    
    datatable(desc_stats,
              options = list(pageLength = 10, dom = 'Bfrtip'),
              caption = "Statistiques descriptives des variables numériques")
  })
  
  output$histogram_desc <- renderPlotly({
    req(filtered_data(), input$hist_var_desc)
    
    df <- filtered_data()
    
    p <- ggplot(df, aes_string(x = input$hist_var_desc)) +
      geom_histogram(bins = 30, fill = "#4DBBD5", color = "white", alpha = 0.8) +
      theme_minimal() +
      labs(title = paste("Distribution de", input$hist_var_desc),
           x = input$hist_var_desc,
           y = "Fréquence")
    
    ggplotly(p)
  })
  
  output$sex_distribution_desc <- renderPlotly({
    req(filtered_data())
    
    df <- filtered_data() %>%
      count(sex) %>%
      mutate(percent = n/sum(n) * 100)
    
    plot_ly(df, labels = ~sex, values = ~n, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = c('#3498db', '#e74c3c'),
                          line = list(color = '#FFFFFF', width = 1))) %>%
      layout(title = "Répartition par sexe",
             showlegend = TRUE)
  })
  
  output$sex_comparison_table <- renderDT({
    req(filtered_data())
    
    table_sex_stats <- filtered_data() %>%
      group_by(sex) %>%
      summarise(
        `Force moyenne (kg)` = round(mean(hand_grip_strength_kg), 2),
        `Écart-type force` = round(sd(hand_grip_strength_kg), 2),
        `VO2 moyen` = round(mean(vo2_estimate_ml_per_kg_min), 2),
        `Écart-type VO2` = round(sd(vo2_estimate_ml_per_kg_min), 2),
        `BMI moyen` = round(mean(bmi), 2),
        `Écart-type BMI` = round(sd(bmi), 2),
        .groups = "drop"
      )
    
    datatable(table_sex_stats,
              options = list(dom = 't'),
              caption = "Comparaison descriptive des performances par sexe")
  })
  
  output$correlation_matrix_desc <- renderPlotly({
    req(filtered_data())
    
    df <- filtered_data() %>%
      select(age, bmi, percent_body_fat,
             hand_grip_strength_kg, vo2_estimate_ml_per_kg_min,
             sit_ups_count, sit_and_reach_cm)
    
    cor_mat <- cor(df, use = "pairwise.complete.obs", method = "pearson")
    
    plot_ly(
      x = colnames(cor_mat),
      y = rownames(cor_mat),
      z = cor_mat,
      type = "heatmap",
      colorscale = "RdBu",
      zmin = -1, zmax = 1,
      hovertemplate = "Corrélation: %{z:.3f}<extra></extra>"
    ) %>%
      layout(
        title = "Matrice de corrélation de Pearson",
        xaxis = list(tickangle = 45),
        yaxis = list(autorange = "reversed"),
        margin = list(l = 150, r = 50, b = 150, t = 50)
      )
  })
  
  output$hgs_density <- renderPlotly({
    req(filtered_data())
    
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = hand_grip_strength_kg, fill = sex)) +
      geom_density(alpha = 0.4) +
      scale_fill_manual(values = c("Male" = "#4DBBD5", "Female" = "#E64B35")) +
      theme_minimal() +
      labs(title = "Distribution de la force de préhension par sexe",
           x = "Force de préhension (kg)",
           y = "Densité",
           fill = "Sexe")
    
    ggplotly(p)
  })
  
  output$age_strength_relation <- renderPlotly({
    req(filtered_data())
    
    df <- filtered_data()
    
    p <- ggplot(df, aes(age, hand_grip_strength_kg, color = sex)) +
      geom_point(alpha = 0.3) +
      geom_smooth(method = "lm", se = TRUE) +
      scale_color_manual(values = c("Male" = "#4DBBD5", "Female" = "#E64B35")) +
      theme_minimal() +
      labs(title = "Relation âge – force de préhension",
           x = "Âge",
           y = "Force de préhension (kg)",
           color = "Sexe")
    
    ggplotly(p)
  })
  
  # PHASE 3: Tests de normalité
  output$shapiro_table <- renderDT({
    req(filtered_data())
    
    numeric_vars <- filtered_data() %>%
      select(age, bmi, percent_body_fat, hand_grip_strength_kg,
             sit_and_reach_cm, sit_ups_count, vo2_estimate_ml_per_kg_min)
    
    shapiro_results <- lapply(numeric_vars, shapiro.test)
    
    shapiro_table <- tibble(
      Variable = names(shapiro_results),
      `Statistique W` = sapply(shapiro_results, function(x) round(x$statistic, 4)),
      `p-value` = sapply(shapiro_results, function(x) round(x$p.value, 4)),
      `Conclusion` = ifelse(sapply(shapiro_results, function(x) x$p.value) > 0.05,
                            "Normale", "Non normale")
    )
    
    datatable(shapiro_table,
              options = list(pageLength = 10, dom = 'Bfrtip'),
              caption = "Test de Shapiro-Wilk") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Normale", "Non normale"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$anderson_table <- renderDT({
    req(filtered_data())
    
    vars_to_test <- filtered_data() %>%
      select(age, bmi, percent_body_fat, hand_grip_strength_kg,
             sit_and_reach_cm, sit_ups_count, vo2_estimate_ml_per_kg_min)
    
    ad_results <- lapply(vars_to_test, ad.test)
    
    ad_table <- tibble(
      Variable = names(ad_results),
      `Statistique AD` = sapply(ad_results, function(x) round(x$statistic, 4)),
      `p-value` = sapply(ad_results, function(x) round(x$p.value, 4)),
      `Conclusion` = ifelse(sapply(ad_results, function(x) x$p.value) > 0.05,
                            "Normale", "Non normale")
    )
    
    datatable(ad_table,
              options = list(pageLength = 10, dom = 'Bfrtip'),
              caption = "Test Anderson-Darling") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Normale", "Non normale"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$qq_plots_norm <- renderPlotly({
    req(filtered_data(), input$qq_vars_norm)
    
    df <- filtered_data()
    
    plots <- list()
    
    for(i in seq_along(input$qq_vars_norm)) {
      var <- input$qq_vars_norm[i]
      
      theoretical_qq <- qqnorm(df[[var]], plot.it = FALSE)
      
      qq_data <- data.frame(
        Theoretical = theoretical_qq$x,
        Sample = theoretical_qq$y
      )
      
      p <- ggplot(qq_data, aes(x = Theoretical, y = Sample)) +
        geom_point(color = "#3498db", alpha = 0.6) +
        geom_abline(slope = 1, intercept = 0, color = "#e74c3c", linetype = "dashed") +
        theme_minimal() +
        labs(title = paste("QQ-Plot:", var),
             x = "Quantiles théoriques",
             y = "Quantiles échantillons")
      
      plots[[i]] <- ggplotly(p)
    }
    
    if(length(plots) == 1) {
      plots[[1]]
    } else if(length(plots) == 2) {
      subplot(plots[[1]], plots[[2]], nrows = 1)
    } else if(length(plots) == 3) {
      subplot(plots[[1]], plots[[2]], plots[[3]], nrows = 1)
    } else {
      subplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], nrows = 2)
    }
  })
  
  # PHASE 4.1: Tests paramétriques
  output$fisher_test_table <- renderDT({
    req(filtered_data())
    
    test_bmi <- var.test(bmi ~ sex, data = filtered_data())
    test_sit <- var.test(sit_and_reach_cm ~ sex, data = filtered_data())
    test_VO2 <- var.test(vo2_estimate_ml_per_kg_min ~ sex, data = filtered_data())
    
    fisher_table <- tibble(
      Variable = c("BMI", "Souplesse (Sit & Reach)", "VO2 estimé"),
      `Statistique F` = c(
        round(as.numeric(test_bmi$statistic), 4),
        round(as.numeric(test_sit$statistic), 4),
        round(as.numeric(test_VO2$statistic), 4)
      ),
      `p-value` = c(
        round(test_bmi$p.value, 4),
        round(test_sit$p.value, 4),
        round(test_VO2$p.value, 4)
      ),
      `Conclusion variances` = c(
        ifelse(test_bmi$p.value > 0.05, "Égales", "Inégales"),
        ifelse(test_sit$p.value > 0.05, "Égales", "Inégales"),
        ifelse(test_VO2$p.value > 0.05, "Égales", "Inégales")
      )
    )
    
    datatable(fisher_table,
              options = list(dom = 't'),
              caption = "Test F de Fisher – Homogénéité des variances") %>%
      formatStyle('Conclusion variances',
                  backgroundColor = styleEqual(c("Égales", "Inégales"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$ttest_bmi_table <- renderDT({
    req(filtered_data())
    
    t_test_bmi <- t.test(bmi ~ sex, data = filtered_data(), var.equal = FALSE)
    
    result_df <- tidy(t_test_bmi) %>%
      select(statistic, p.value, parameter) %>%
      rename(
        `Statistique t` = statistic,
        `p-value` = p.value,
        `Degrés de liberté` = parameter
      ) %>%
      mutate(across(where(is.numeric), round, 4))
    
    datatable(result_df,
              options = list(dom = 't'),
              caption = "Test t de Welch pour le BMI")
  })
  
  output$ttest_sit_table <- renderDT({
    req(filtered_data())
    
    t_test_sit <- t.test(sit_and_reach_cm ~ sex, data = filtered_data(), var.equal = TRUE)
    
    result_df <- tidy(t_test_sit) %>%
      select(statistic, p.value, parameter) %>%
      rename(
        `Statistique t` = statistic,
        `p-value` = p.value,
        `Degrés de liberté` = parameter
      ) %>%
      mutate(across(where(is.numeric), round, 4))
    
    datatable(result_df,
              options = list(dom = 't'),
              caption = "Test t de Student pour la souplesse")
  })
  
  output$ttest_vo2_table <- renderDT({
    req(filtered_data())
    
    t_test_VO2 <- t.test(vo2_estimate_ml_per_kg_min ~ sex, data = filtered_data(), var.equal = TRUE)
    
    result_df <- tidy(t_test_VO2) %>%
      select(statistic, p.value, parameter) %>%
      rename(
        `Statistique t` = statistic,
        `p-value` = p.value,
        `Degrés de liberté` = parameter
      ) %>%
      mutate(across(where(is.numeric), round, 4))
    
    datatable(result_df,
              options = list(dom = 't'),
              caption = "Test t de Student pour le VO2 estimé")
  })
  
  output$ttest_one_sample_table <- renderDT({
    req(filtered_data())
    
    ref_values <- list(bmi = 22.8, sit = 17.56, vo2 = 37.3)
    
    test_bmi <- t.test(filtered_data()$bmi, mu = ref_values$bmi)
    test_sit <- t.test(filtered_data()$sit_and_reach_cm, mu = ref_values$sit)
    test_vo2 <- t.test(filtered_data()$vo2_estimate_ml_per_kg_min, mu = ref_values$vo2)
    
    ttest1_table <- tibble(
      Variable = c("BMI", "Souplesse (Sit & Reach)", "VO2 estimé"),
      `Valeur théorique` = c(ref_values$bmi, ref_values$sit, ref_values$vo2),
      `Moyenne observée` = c(
        round(mean(filtered_data()$bmi, na.rm = TRUE), 2),
        round(mean(filtered_data()$sit_and_reach_cm, na.rm = TRUE), 2),
        round(mean(filtered_data()$vo2_estimate_ml_per_kg_min, na.rm = TRUE), 2)
      ),
      `Statistique t` = c(
        round(test_bmi$statistic, 4),
        round(test_sit$statistic, 4),
        round(test_vo2$statistic, 4)
      ),
      `p-value` = c(
        round(test_bmi$p.value, 4),
        round(test_sit$p.value, 4),
        round(test_vo2$p.value, 4)
      )
    )
    
    datatable(ttest1_table,
              options = list(dom = 't'),
              caption = "Test t à 1 échantillon – Comparaison avec valeurs de référence")
  })
  
  output$anova_age_table <- renderDT({
    req(filtered_data())
    
    anova_vo2_age <- aov(vo2_estimate_ml_per_kg_min ~ age_group, data = filtered_data())
    anova_bmi_age <- aov(bmi ~ age_group, data = filtered_data())
    anova_sit_age <- aov(sit_and_reach_cm ~ age_group, data = filtered_data())
    
    anova_age_table <- tibble(
      Variable = c("VO2", "BMI", "Souplesse"),
      `Statistique F` = c(
        round(summary(anova_vo2_age)[[1]]$`F value`[1], 4),
        round(summary(anova_bmi_age)[[1]]$`F value`[1], 4),
        round(summary(anova_sit_age)[[1]]$`F value`[1], 4)
      ),
      `p-value` = c(
        round(summary(anova_vo2_age)[[1]]$`Pr(>F)`[1], 4),
        round(summary(anova_bmi_age)[[1]]$`Pr(>F)`[1], 4),
        round(summary(anova_sit_age)[[1]]$`Pr(>F)`[1], 4)
      ),
      Conclusion = ifelse(
        c(summary(anova_vo2_age)[[1]]$`Pr(>F)`[1],
          summary(anova_bmi_age)[[1]]$`Pr(>F)`[1],
          summary(anova_sit_age)[[1]]$`Pr(>F)`[1]) < 0.05,
        "Différence significative",
        "Aucune différence significative"
      )
    )
    
    datatable(anova_age_table,
              options = list(dom = 't'),
              caption = "ANOVA selon les groupes d'âge") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Différence significative", "Aucune différence significative"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$anova_bmi_table <- renderDT({
    req(filtered_data())
    
    anova_vo2_bmi <- aov(vo2_estimate_ml_per_kg_min ~ categorie_bmi, data = filtered_data())
    anova_sit_bmi <- aov(sit_and_reach_cm ~ categorie_bmi, data = filtered_data())
    
    anova_bmi_table <- tibble(
      Variable = c("VO2 estimé", "Souplesse"),
      `Statistique F` = c(
        round(summary(anova_vo2_bmi)[[1]]$`F value`[1], 4),
        round(summary(anova_sit_bmi)[[1]]$`F value`[1], 4)
      ),
      `p-value` = c(
        round(summary(anova_vo2_bmi)[[1]]$`Pr(>F)`[1], 4),
        round(summary(anova_sit_bmi)[[1]]$`Pr(>F)`[1], 4)
      ),
      Conclusion = ifelse(
        c(summary(anova_vo2_bmi)[[1]]$`Pr(>F)`[1],
          summary(anova_sit_bmi)[[1]]$`Pr(>F)`[1]) < 0.05,
        "Différence significative",
        "Aucune différence significative"
      )
    )
    
    datatable(anova_bmi_table,
              options = list(dom = 't'),
              caption = "ANOVA selon les catégories de BMI") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Différence significative", "Aucune différence significative"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$anova_vo2_table <- renderDT({
    req(filtered_data())
    
    df_vo2 <- filtered_data() %>%
      mutate(categorie_vo2 = case_when(
        vo2_estimate_ml_per_kg_min < 30 ~ "Faible",
        vo2_estimate_ml_per_kg_min < 40 ~ "Moyen",
        TRUE ~ "Élevé"
      ))
    
    anova_bmi_vo2 <- aov(bmi ~ categorie_vo2, data = df_vo2)
    anova_sit_vo2 <- aov(sit_and_reach_cm ~ categorie_vo2, data = df_vo2)
    
    anova_vo2_table <- tibble(
      Variable = c("BMI", "Souplesse"),
      `Statistique F` = c(
        round(summary(anova_bmi_vo2)[[1]]$`F value`[1], 4),
        round(summary(anova_sit_vo2)[[1]]$`F value`[1], 4)
      ),
      `p-value` = c(
        round(summary(anova_bmi_vo2)[[1]]$`Pr(>F)`[1], 4),
        round(summary(anova_sit_vo2)[[1]]$`Pr(>F)`[1], 4)
      ),
      Conclusion = c(
        ifelse(summary(anova_bmi_vo2)[[1]]$`Pr(>F)`[1] < 0.05,
               "Différence significative", "Aucune différence significative"),
        ifelse(summary(anova_sit_vo2)[[1]]$`Pr(>F)`[1] < 0.05,
               "Différence significative", "Aucune différence significative")
      )
    )
    
    datatable(anova_vo2_table,
              options = list(dom = 't'),
              caption = "ANOVA selon les catégories de VO2") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Différence significative", "Aucune différence significative"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$prop_test_output <- renderPrint({
    req(filtered_data())
    
    sex_counts <- table(filtered_data()$sex)
    prop_test <- prop.test(x = sex_counts["Male"], n = sum(sex_counts), p = 0.5, correct = FALSE)
    
    cat("=== TEST DE PROPORTION ===\n\n")
    cat("Répartition observée:\n")
    cat("  Hommes:", sex_counts["Male"], "(", round(sex_counts["Male"]/sum(sex_counts)*100, 1), "%)\n")
    cat("  Femmes:", sex_counts["Female"], "(", round(sex_counts["Female"]/sum(sex_counts)*100, 1), "%)\n\n")
    cat("Test d'égalité à 50%:\n")
    cat("  Chi-deux =", round(prop_test$statistic, 4), "\n")
    cat("  p-value =", format.pval(prop_test$p.value), "\n")
    cat("  Conclusion:", ifelse(prop_test$p.value < 0.05, 
                                "Proportion significativement différente de 50%",
                                "Proportion non différente de 50%"))
  })
  
  output$pearson_table <- renderDT({
    req(filtered_data())
    
    df_norm <- filtered_data() %>%
      select(bmi, sit_and_reach_cm, vo2_estimate_ml_per_kg_min) %>%
      drop_na()
    
    cor_bmi_sit <- cor.test(df_norm$bmi, df_norm$sit_and_reach_cm, method = "pearson")
    cor_bmi_vo2 <- cor.test(df_norm$bmi, df_norm$vo2_estimate_ml_per_kg_min, method = "pearson")
    cor_sit_vo2 <- cor.test(df_norm$sit_and_reach_cm, df_norm$vo2_estimate_ml_per_kg_min, method = "pearson")
    
    pearson_table <- tibble(
      `Variables` = c("BMI et Souplesse", "BMI et VO2 estimé", "Souplesse et VO2 estimé"),
      `Coefficient r` = c(
        round(cor_bmi_sit$estimate, 4),
        round(cor_bmi_vo2$estimate, 4),
        round(cor_sit_vo2$estimate, 4)
      ),
      `p-value` = c(
        round(cor_bmi_sit$p.value, 4),
        round(cor_bmi_vo2$p.value, 4),
        round(cor_sit_vo2$p.value, 4)
      ),
      `Conclusion` = ifelse(
        c(cor_bmi_sit$p.value, cor_bmi_vo2$p.value, cor_sit_vo2$p.value) < 0.05,
        "Corrélation significative",
        "Aucune corrélation significative"
      )
    )
    
    datatable(pearson_table,
              options = list(dom = 't'),
              caption = "Test de corrélation de Pearson") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Corrélation significative", "Aucune corrélation significative"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  # PHASE 4.2: Tests non-paramétriques
  output$wilcoxon_table <- renderDT({
    req(filtered_data())
    
    wilcox_age <- wilcox.test(age ~ sex, data = filtered_data())
    wilcox_fat <- wilcox.test(percent_body_fat ~ sex, data = filtered_data())
    wilcox_hgs <- wilcox.test(hand_grip_strength_kg ~ sex, data = filtered_data())
    wilcox_situps <- wilcox.test(sit_ups_count ~ sex, data = filtered_data())
    
    wilcox_results <- tibble(
      Variable = c("Âge", "Masse grasse (%)", "Force de préhension (kg)", "Sit-ups"),
      `Statistique W` = c(
        round(wilcox_age$statistic, 4),
        round(wilcox_fat$statistic, 4),
        round(wilcox_hgs$statistic, 4),
        round(wilcox_situps$statistic, 4)
      ),
      `p-value` = c(
        round(wilcox_age$p.value, 4),
        round(wilcox_fat$p.value, 4),
        round(wilcox_hgs$p.value, 4),
        round(wilcox_situps$p.value, 4)
      ),
      Conclusion = ifelse(
        c(wilcox_age$p.value, wilcox_fat$p.value, wilcox_hgs$p.value, wilcox_situps$p.value) < 0.05,
        "Différence significative",
        "Aucune différence significative"
      )
    )
    
    datatable(wilcox_results,
              options = list(dom = 't', pageLength = 10),
              caption = "Test de Wilcoxon-Mann-Whitney (Homme vs Femme)") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Différence significative", "Aucune différence significative"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$kruskal_table <- renderDT({
    req(filtered_data())
    
    kw_age <- kruskal.test(vo2_estimate_ml_per_kg_min ~ age_group, data = filtered_data())
    kw_bmi <- kruskal.test(vo2_estimate_ml_per_kg_min ~ categorie_bmi, data = filtered_data())
    
    kw_results <- tibble(
      Test = c("VO2 ~ groupes d'âge", "VO2 ~ catégories BMI"),
      `Statistique H` = c(round(kw_age$statistic, 4), round(kw_bmi$statistic, 4)),
      `p-value` = c(round(kw_age$p.value, 4), round(kw_bmi$p.value, 4)),
      Conclusion = ifelse(
        c(kw_age$p.value, kw_bmi$p.value) < 0.05,
        "Différence significative",
        "Aucune différence significative"
      )
    )
    
    datatable(kw_results,
              options = list(dom = 't'),
              caption = "Test de Kruskal-Wallis") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Différence significative", "Aucune différence significative"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$chisq_table <- renderDT({
    req(filtered_data())
    
    chi_bmi <- chisq.test(table(filtered_data()$sex, filtered_data()$categorie_bmi))
    chi_pbf <- chisq.test(table(filtered_data()$sex, filtered_data()$categorie_pbf))
    chi_age <- chisq.test(table(filtered_data()$sex, filtered_data()$categorie_age_test))
    
    chi_global <- tibble(
      Test = c("Sexe × Catégorie BMI", "Sexe × Catégorie Masse Grasse", "Sexe × Catégorie d'Âge"),
      `Chi-deux` = c(
        round(chi_bmi$statistic, 2),
        round(chi_pbf$statistic, 2),
        round(chi_age$statistic, 2)
      ),
      `ddl` = c(chi_bmi$parameter, chi_pbf$parameter, chi_age$parameter),
      `p-value` = c(chi_bmi$p.value, chi_pbf$p.value, chi_age$p.value),
      Conclusion = ifelse(
        c(chi_bmi$p.value, chi_pbf$p.value, chi_age$p.value) < 0.05,
        "Dépendance significative",
        "Indépendance"
      )
    )
    
    datatable(chi_global,
              options = list(dom = 't', pageLength = 10),
              caption = "Test du Chi-deux d'indépendance") %>%
      formatStyle('Conclusion',
                  backgroundColor = styleEqual(c("Dépendance significative", "Indépendance"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$spearman_table <- renderDT({
    req(filtered_data())
    
    vars_non_param <- filtered_data() %>%
      select(age, percent_body_fat, hand_grip_strength_kg, sit_ups_count) %>%
      drop_na()
    
    spearman_results <- list()
    
    vars <- names(vars_non_param)
    for(i in 1:(length(vars)-1)) {
      for(j in (i+1):length(vars)) {
        var1 <- vars[i]
        var2 <- vars[j]
        spearman_test <- cor.test(vars_non_param[[var1]], vars_non_param[[var2]], 
                                  method = "spearman", exact = FALSE)
        
        if(spearman_test$p.value < 0.05) {
          spearman_results[[paste(var1, var2, sep = " ~ ")]] <- list(
            Correlation = spearman_test$estimate,
            p_value = spearman_test$p.value
          )
        }
      }
    }
    
    if(length(spearman_results) > 0) {
      spearman_df <- tibble(
        Variables = names(spearman_results),
        `Coefficient ρ` = sapply(spearman_results, function(x) round(x$Correlation, 3)),
        `p-value` = sapply(spearman_results, function(x) round(x$p_value, 4))
      )
    } else {
      spearman_df <- tibble(
        Variables = "Aucune corrélation significative",
        `Coefficient ρ` = NA,
        `p-value` = NA
      )
    }
    
    datatable(spearman_df,
              options = list(dom = 't'),
              caption = "Test de corrélation de Spearman (corrélations significatives)")
  })
  
  # PHASE 5: Régressions linéaires
  observe({
    req(filtered_data())
    
    df_reg <- filtered_data() %>%
      select(hand_grip_strength_kg, vo2_estimate_ml_per_kg_min,
             sit_ups_count, sit_and_reach_cm,
             sex, age, bmi, percent_body_fat) %>%
      drop_na()
    
    # Modèles de régression
    regression_models$force <- lm(hand_grip_strength_kg ~ sex + age + bmi + percent_body_fat, data = df_reg)
    regression_models$vo2 <- lm(vo2_estimate_ml_per_kg_min ~ sex + age + bmi + percent_body_fat, data = df_reg)
    regression_models$situps <- lm(sit_ups_count ~ sex + age + bmi + percent_body_fat, data = df_reg)
    regression_models$flex <- lm(sit_and_reach_cm ~ sex + age + bmi + percent_body_fat, data = df_reg)
  })
  
  # 5.1 Régression Force
  output$reg_force_summary <- renderPrint({
    req(regression_models$force)
    
    model <- regression_models$force
    cat("=== RÉGRESSION POUR LA FORCE (HAND GRIP STRENGTH) ===\n\n")
    print(summary(model))
    
    cat("\n--- Tests de diagnostic ---\n")
    cat("Shapiro-Wilk (résidus): W =", 
        round(shapiro.test(residuals(model))$statistic, 4),
        ", p =", round(shapiro.test(residuals(model))$p.value, 4), "\n")
    cat("Breusch-Pagan (hétéroscédasticité): BP =",
        round(bptest(model)$statistic, 4),
        ", p =", round(bptest(model)$p.value, 4), "\n")
    cat("\n--- Facteurs d'inflation de la variance (VIF) ---\n")
    print(vif(model))
  })
  
  output$reg_force_diagnostics <- renderPlot({
    req(regression_models$force)
    
    model <- regression_models$force
    par(mfrow = c(2, 2))
    plot(model, which = 1:4)
  })
  
  output$reg_force_coefs <- renderDT({
    req(regression_models$force)
    
    model <- regression_models$force
    coef_df <- as.data.frame(summary(model)$coefficients)
    colnames(coef_df) <- c("Estimation", "Erreur standard", "t value", "Pr(>|t|)")
    
    coef_df <- coef_df %>%
      mutate(
        `Pr(>|t|)` = round(`Pr(>|t|)`, 4),
        Significatif = ifelse(`Pr(>|t|)` < 0.05, "Oui", "Non"),
        across(where(is.numeric), ~round(., 3))
      )
    
    datatable(coef_df,
              options = list(dom = 't'),
              caption = "Coefficients de régression - Force") %>%
      formatStyle('Significatif',
                  backgroundColor = styleEqual(c("Oui", "Non"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$reg_force_performance <- renderUI({
    req(regression_models$force)
    
    model <- regression_models$force
    r2 <- summary(model)$r.squared
    adj_r2 <- summary(model)$adj.r.squared
    rmse <- sqrt(mean(residuals(model)^2))
    
    tagList(
      h4("Performance du modèle:"),
      div(class = "info-card",
          icon("chart-line", class = "icon", style = "color: #3498db;"),
          div(class = "value", round(r2, 3)),
          div(class = "label", "R²")
      ),
      div(class = "info-card",
          icon("chart-bar", class = "icon", style = "color: #27ae60;"),
          div(class = "value", round(adj_r2, 3)),
          div(class = "label", "R² ajusté")
      ),
      div(class = "info-card",
          icon("ruler", class = "icon", style = "color: #e74c3c;"),
          div(class = "value", round(rmse, 2)),
          div(class = "label", "RMSE (kg)")
      ),
      hr(),
      h5("Interprétation:"),
      p(ifelse(r2 > 0.5, 
               "✅ Le modèle explique bien la variance de la force de préhension",
               "⚠️ Le modèle explique modérément la variance de la force de préhension"))
    )
  })
  
  # 5.2 Régression VO2
  output$reg_vo2_summary <- renderPrint({
    req(regression_models$vo2)
    
    model <- regression_models$vo2
    cat("=== RÉGRESSION POUR VO2 ESTIMÉ ===\n\n")
    print(summary(model))
    
    cat("\n--- Tests de diagnostic ---\n")
    cat("Shapiro-Wilk (résidus): W =", 
        round(shapiro.test(residuals(model))$statistic, 4),
        ", p =", round(shapiro.test(residuals(model))$p.value, 4), "\n")
  })
  
  output$reg_vo2_diagnostics <- renderPlot({
    req(regression_models$vo2)
    
    model <- regression_models$vo2
    par(mfrow = c(2, 2))
    plot(model, which = 1:4)
  })
  
  output$reg_vo2_coefs <- renderDT({
    req(regression_models$vo2)
    
    model <- regression_models$vo2
    coef_df <- as.data.frame(summary(model)$coefficients)
    colnames(coef_df) <- c("Estimation", "Erreur standard", "t value", "Pr(>|t|)")
    
    coef_df <- coef_df %>%
      mutate(
        `Pr(>|t|)` = round(`Pr(>|t|)`, 4),
        Significatif = ifelse(`Pr(>|t|)` < 0.05, "Oui", "Non"),
        across(where(is.numeric), ~round(., 3))
      )
    
    datatable(coef_df,
              options = list(dom = 't'),
              caption = "Coefficients de régression - VO2") %>%
      formatStyle('Significatif',
                  backgroundColor = styleEqual(c("Oui", "Non"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$reg_vo2_performance <- renderUI({
    req(regression_models$vo2)
    
    model <- regression_models$vo2
    r2 <- summary(model)$r.squared
    adj_r2 <- summary(model)$adj.r.squared
    rmse <- sqrt(mean(residuals(model)^2))
    
    tagList(
      h4("Performance du modèle:"),
      div(class = "info-card",
          icon("chart-line", class = "icon", style = "color: #3498db;"),
          div(class = "value", round(r2, 3)),
          div(class = "label", "R²")
      ),
      div(class = "info-card",
          icon("chart-bar", class = "icon", style = "color: #27ae60;"),
          div(class = "value", round(adj_r2, 3)),
          div(class = "label", "R² ajusté")
      ),
      div(class = "info-card",
          icon("ruler", class = "icon", style = "color: #e74c3c;"),
          div(class = "value", round(rmse, 2)),
          div(class = "label", "RMSE (ml/kg/min)")
      ),
      hr(),
      h5("Interprétation:"),
      p(ifelse(r2 > 0.3, 
               "✅ Le modèle explique bien la variance du VO2 estimé",
               "⚠️ Le modèle explique modérément la variance du VO2 estimé"))
    )
  })
  
  # 5.3 Régression Sit-ups
  output$reg_situps_summary <- renderPrint({
    req(regression_models$situps)
    
    model <- regression_models$situps
    cat("=== RÉGRESSION POUR SIT-UPS ===\n\n")
    print(summary(model))
    
    cat("\n--- Tests de diagnostic ---\n")
    cat("Shapiro-Wilk (résidus): W =", 
        round(shapiro.test(residuals(model))$statistic, 4),
        ", p =", round(shapiro.test(residuals(model))$p.value, 4), "\n")
  })
  
  output$reg_situps_diagnostics <- renderPlot({
    req(regression_models$situps)
    
    model <- regression_models$situps
    par(mfrow = c(2, 2))
    plot(model, which = 1:4)
  })
  
  output$reg_situps_coefs <- renderDT({
    req(regression_models$situps)
    
    model <- regression_models$situps
    coef_df <- as.data.frame(summary(model)$coefficients)
    colnames(coef_df) <- c("Estimation", "Erreur standard", "t value", "Pr(>|t|)")
    
    coef_df <- coef_df %>%
      mutate(
        `Pr(>|t|)` = round(`Pr(>|t|)`, 4),
        Significatif = ifelse(`Pr(>|t|)` < 0.05, "Oui", "Non"),
        across(where(is.numeric), ~round(., 3))
      )
    
    datatable(coef_df,
              options = list(dom = 't'),
              caption = "Coefficients de régression - Sit-ups") %>%
      formatStyle('Significatif',
                  backgroundColor = styleEqual(c("Oui", "Non"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$reg_situps_performance <- renderUI({
    req(regression_models$situps)
    
    model <- regression_models$situps
    r2 <- summary(model)$r.squared
    adj_r2 <- summary(model)$adj.r.squared
    rmse <- sqrt(mean(residuals(model)^2))
    
    tagList(
      h4("Performance du modèle:"),
      div(class = "info-card",
          icon("chart-line", class = "icon", style = "color: #3498db;"),
          div(class = "value", round(r2, 3)),
          div(class = "label", "R²")
      ),
      div(class = "info-card",
          icon("chart-bar", class = "icon", style = "color: #27ae60;"),
          div(class = "value", round(adj_r2, 3)),
          div(class = "label", "R² ajusté")
      ),
      div(class = "info-card",
          icon("ruler", class = "icon", style = "color: #e74c3c;"),
          div(class = "value", round(rmse, 2)),
          div(class = "label", "RMSE (nombre)")
      ),
      hr(),
      h5("Interprétation:"),
      p(ifelse(r2 > 0.3, 
               "✅ Le modèle explique bien la variance du nombre de sit-ups",
               "⚠️ Le modèle explique modérément la variance du nombre de sit-ups"))
    )
  })
  
  # 5.4 Régression Flexibilité
  output$reg_flex_summary <- renderPrint({
    req(regression_models$flex)
    
    model <- regression_models$flex
    cat("=== RÉGRESSION POUR FLEXIBILITÉ ===\n\n")
    print(summary(model))
    
    cat("\n--- Tests de diagnostic ---\n")
    cat("Shapiro-Wilk (résidus): W =", 
        round(shapiro.test(residuals(model))$statistic, 4),
        ", p =", round(shapiro.test(residuals(model))$p.value, 4), "\n")
  })
  
  output$reg_flex_diagnostics <- renderPlot({
    req(regression_models$flex)
    
    model <- regression_models$flex
    par(mfrow = c(2, 2))
    plot(model, which = 1:4)
  })
  
  output$reg_flex_coefs <- renderDT({
    req(regression_models$flex)
    
    model <- regression_models$flex
    coef_df <- as.data.frame(summary(model)$coefficients)
    colnames(coef_df) <- c("Estimation", "Erreur standard", "t value", "Pr(>|t|)")
    
    coef_df <- coef_df %>%
      mutate(
        `Pr(>|t|)` = round(`Pr(>|t|)`, 4),
        Significatif = ifelse(`Pr(>|t|)` < 0.05, "Oui", "Non"),
        across(where(is.numeric), ~round(., 3))
      )
    
    datatable(coef_df,
              options = list(dom = 't'),
              caption = "Coefficients de régression - Flexibilité") %>%
      formatStyle('Significatif',
                  backgroundColor = styleEqual(c("Oui", "Non"), 
                                               c('#d4edda', '#f8d7da')))
  })
  
  output$reg_flex_performance <- renderUI({
    req(regression_models$flex)
    
    model <- regression_models$flex
    r2 <- summary(model)$r.squared
    adj_r2 <- summary(model)$adj.r.squared
    rmse <- sqrt(mean(residuals(model)^2))
    
    tagList(
      h4("Performance du modèle:"),
      div(class = "info-card",
          icon("chart-line", class = "icon", style = "color: #3498db;"),
          div(class = "value", round(r2, 3)),
          div(class = "label", "R²")
      ),
      div(class = "info-card",
          icon("chart-bar", class = "icon", style = "color: #27ae60;"),
          div(class = "value", round(adj_r2, 3)),
          div(class = "label", "R² ajusté")
      ),
      div(class = "info-card",
          icon("ruler", class = "icon", style = "color: #e74c3c;"),
          div(class = "value", round(rmse, 2)),
          div(class = "label", "RMSE (cm)")
      ),
      hr(),
      h5("Interprétation:"),
      p(ifelse(r2 > 0.2, 
               "✅ Le modèle explique raisonnablement la variance de la souplesse",
               "⚠️ Le modèle explique faiblement la variance de la souplesse"))
    )
  })
  
  # PHASE 6: Calculateur d'âge fitness
  observeEvent(input$calculate_fitness, {
    output$fitness_age_calculator_results <- renderUI({
      # Calcul du score fitness (identique à la fonction de préparation)
      fitness_score <- (input$pred_grip_calc/50 * 0.3) + 
        (input$pred_vo2_calc/50 * 0.4) + 
        (input$pred_situps_calc/50 * 0.2) + 
        (input$pred_flex_calc/40 * 0.1)
      fitness_score <- fitness_score * 100
      
      # Calcul de l'âge fitness
      fitness_age <- case_when(
        fitness_score >= 80 ~ input$pred_age_calc - 5,
        fitness_score >= 60 ~ input$pred_age_calc - 2,
        fitness_score >= 40 ~ input$pred_age_calc,
        TRUE ~ input$pred_age_calc + 5
      )
      
      age_diff <- fitness_age - input$pred_age_calc
      
      # Catégorie
      category <- if(fitness_score >= 80) {
        list(text = "Excellente", color = "#27ae60", icon = "trophy")
      } else if(fitness_score >= 60) {
        list(text = "Bonne", color = "#3498db", icon = "thumbs-up")
      } else if(fitness_score >= 40) {
        list(text = "Moyenne", color = "#f39c12", icon = "check-circle")
      } else {
        list(text = "À améliorer", color = "#e74c3c", icon = "exclamation-triangle")
      }
      
      tagList(
        div(class = "info-card",
            icon(category$icon, class = "icon", style = paste0("color: ", category$color, ";")),
            div(class = "value", style = paste0("color: ", category$color, ";"), 
                round(fitness_score, 1)),
            div(class = "label", "Score Fitness")
        ),
        hr(),
        h4("📊 Résultats:"),
        p(strong("Âge réel:"), input$pred_age_calc, "ans"),
        p(strong("Âge fitness:"), round(fitness_age, 1), "ans"),
        p(strong("Différence:"), 
          span(ifelse(age_diff < 0, 
                      paste0("🔽 ", abs(round(age_diff, 1)), " ans de moins"),
                      paste0("🔼 ", round(age_diff, 1), " ans de plus")),
               style = paste0("color: ", 
                              ifelse(age_diff < 0, "#27ae60", 
                                     ifelse(age_diff > 0, "#e74c3c", "#7f8c8d")), ";"))),
        p(strong("Catégorie:"), 
          span(category$text, 
               style = paste0("color: ", category$color, "; font-weight: bold;"))),
        hr(),
        p(em("Note: L'âge fitness estime votre condition physique par rapport à votre âge chronologique."))
      )
    })
  })
  
  # PHASE 6.2: Random Forest
  observeEvent(input$train_rf_model, {
    req(filtered_data(), input$rf_vars_pred)
    
    showNotification("Entraînement du modèle Random Forest...", type = "default")
    
    tryCatch({
      df <- filtered_data() %>%
        select(fitness_level, all_of(input$rf_vars_pred)) %>%
        drop_na()
      
      set.seed(123)
      rf_fit <- randomForest(fitness_level ~ ., 
                             data = df, 
                             ntree = input$rf_trees_pred,
                             importance = TRUE)
      
      rf_model(rf_fit)
      
      showNotification("Modèle Random Forest entraîné avec succès!", type = "default")
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  })
  
  output$rf_model_results <- renderUI({
    req(rf_model())
    
    model <- rf_model()
    accuracy <- 1 - mean(model$err.rate[, "OOB"])
    
    tagList(
      h4("📊 Performance du modèle:"),
      div(class = "info-card",
          icon("bullseye", class = "icon", style = "color: #3498db;"),
          div(class = "value", round(accuracy, 3)),
          div(class = "label", "Précision OOB")
      ),
      p(strong("Matrice de confusion OOB:")),
      renderTable({
        as.data.frame(model$confusion) %>%
          rownames_to_column("Classe réelle")
      })
    )
  })
  
  output$rf_variable_importance <- renderPlotly({
    req(rf_model())
    
    model <- rf_model()
    imp <- importance(model)
    
    imp_df <- as.data.frame(imp) %>%
      rownames_to_column("Variable") %>%
      arrange(desc(MeanDecreaseGini))
    
    p <- ggplot(imp_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
      geom_bar(stat = "identity", fill = "#3498db", alpha = 0.8) +
      coord_flip() +
      theme_minimal() +
      labs(title = "Importance des Variables (Random Forest)",
           x = "Variable", y = "Importance (Mean Decrease Gini)")
    
    ggplotly(p)
  })
  
  # PHASE 7: Visualisations avancées
  output$scatter3d_adv <- renderPlotly({
    req(filtered_data(), input$scatter3d_x_adv, input$scatter3d_y_adv, input$scatter3d_z_adv)
    
    df <- filtered_data()
    
    plot_ly(df, 
            x = ~get(input$scatter3d_x_adv),
            y = ~get(input$scatter3d_y_adv),
            z = ~get(input$scatter3d_z_adv),
            color = ~get(input$scatter3d_color_adv),
            type = "scatter3d",
            mode = "markers",
            marker = list(size = 5),
            text = ~paste(input$scatter3d_x_adv, ":", round(get(input$scatter3d_x_adv), 2), "<br>",
                          input$scatter3d_y_adv, ":", round(get(input$scatter3d_y_adv), 2), "<br>",
                          input$scatter3d_z_adv, ":", round(get(input$scatter3d_z_adv), 2)),
            hoverinfo = "text") %>%
      layout(title = paste("Visualisation 3D:", input$scatter3d_x_adv, "vs", 
                           input$scatter3d_y_adv, "vs", input$scatter3d_z_adv),
             scene = list(
               xaxis = list(title = input$scatter3d_x_adv),
               yaxis = list(title = input$scatter3d_y_adv),
               zaxis = list(title = input$scatter3d_z_adv)
             ))
  })
  
  output$scatter_matrix_adv <- renderPlot({
    req(filtered_data())
    
    df <- filtered_data() %>%
      select(age, bmi, hand_grip_strength_kg, vo2_estimate_ml_per_kg_min, 
             fitness_score) %>%
      select(where(is.numeric))
    
    ggpairs(df,
            lower = list(continuous = wrap("points", alpha = 0.5, color = "#3498db")),
            diag = list(continuous = wrap("barDiag", fill = "#3498db", color = "white")),
            upper = list(continuous = wrap("cor", size = 4))) +
      theme_minimal()
  })
  
  output$faceted_plots_adv <- renderPlotly({
    req(filtered_data(), input$facet_var_adv)
    
    df <- filtered_data()
    
    p <- ggplot(df, aes(x = fitness_score, y = vo2_estimate_ml_per_kg_min, color = sex)) +
      geom_point(alpha = 0.6) +
      facet_wrap(as.formula(paste("~", input$facet_var_adv))) +
      scale_color_manual(values = c("Male" = "#3498db", "Female" = "#e74c3c")) +
      theme_minimal() +
      labs(title = paste("Relation Score Fitness - VO2 par", input$facet_var_adv),
           x = "Score Fitness", y = "VO2 Max")
    
    ggplotly(p)
  })
  
  # Aperçu des données brutes
  output$data_table_raw <- renderDT({
    req(filtered_data())
    
    df <- filtered_data()
    
    datatable(
      head(df, 50),
      extensions = c('Buttons', 'Scroller', 'ColReorder', 'Responsive'),
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print', 'colvis'),
        scrollX = TRUE,
        scrollY = "400px",
        scroller = TRUE,
        colReorder = TRUE,
        responsive = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      filter = 'top',
      rownames = FALSE,
      class = 'display compact cell-border stripe hover'
    ) %>%
      formatStyle(columns = names(df), fontSize = '12px')
  })
  
  output$data_preview_info <- renderUI({
    req(filtered_data())
    
    df <- filtered_data()
    
    tagList(
      div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #3498db;",
          h4(icon("info-circle"), " Informations sur les données"),
          hr(style = "margin: 10px 0;"),
          fluidRow(
            column(6,
                   p(icon("table"), strong(" Dimensions:"), 
                     dim(df)[1], "observations ×", dim(df)[2], "variables"),
                   p(icon("calendar"), strong(" Période:"), 
                     paste(min(df$measurement_date, na.rm = TRUE), "à", 
                           max(df$measurement_date, na.rm = TRUE))),
                   p(icon("users"), strong(" Sexe:"),
                     paste(round(sum(df$sex == "Male")/nrow(df)*100, 1), "% Hommes"),
                     " | ",
                     paste(round(sum(df$sex == "Female")/nrow(df)*100, 1), "% Femmes"))
            ),
            column(6,
                   p(icon("calculator"), strong(" Âge moyen:"), 
                     round(mean(df$age, na.rm = TRUE), 1), "ans"),
                   p(icon("weight"), strong(" BMI moyen:"), 
                     round(mean(df$bmi, na.rm = TRUE), 1)),
                   p(icon("chart-line"), strong(" Score fitness moyen:"), 
                     round(mean(df$fitness_score, na.rm = TRUE), 1))
            )
          )
      )
    )
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)