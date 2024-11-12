### Une appli Shiny pour explorer la (co-)variation d'indicateurs thermiques ###
### Liés à l'écologie du saumon Atlantique (_Salmo salar_) adulte en montaison ###

## Martin Luquet, 2024_08_06
## Contact : martin.luquet.pro@gmail.com
## Désolé s'il y a un petit mélange de français et d'anglais j'ai changé
## plusieurs fois en cours de route !

# Packages ----

# Installation et chargement des packages avec pacman
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
    # Packages Shiny
  shiny, bslib, shinyTree,
    # Manipulation de données
  tidyverse, lubridate, dplyover,
    # Visualisation
  ggcorrplot, GGally, cowplot, ggfortify, factoextra
)

# Remarque : Installer dplyover depuis GitHub si nécessaire
if (!require("dplyover")) remotes::install_github("TimTeaFan/dplyover")



# User interface ----
ui <- page_sidebar(
  
  ## Titre général ----
  title = "Exploration d'indicateurs thermiques liés à l'écologie du 
             saumon Atlantique en eau douce",
  
  ## Choix des hyperparamètres ----
  accordion_panel("Hyperparamètres",
                    #Topt
                  checkboxGroupInput("topt", "Topt", 
                                     choices = c(12:20 ,"Adapt"), selected = 16, 
                                     inline = TRUE),
                    # Tcrit
                  checkboxGroupInput("tcrit", "Tcrit", 
                                     choices = 16:23, selected = 20, 
                                     inline = TRUE),
                    # CTmax (température létale)
                  checkboxGroupInput("ctmax", "CTmax", 
                                     choices = 24:33, selected = 27, 
                                     inline = TRUE),
                    # n_hw (nb de jours nécessaires pour une canicule)
                  checkboxGroupInput("n_hw", "N", 
                                     choices = 1:3, selected = 3,
                                     inline = TRUE),
                    # n_y (nb d'années en arrière pour la variable sumDevPrev)
                  checkboxGroupInput("n_y", "nY", 
                                     choices = 1:5, selected = 3,
                                     inline = TRUE)
  ),
  
  ## Barre latérale ----
  sidebar = sidebar(
    
    ### Choix du fichier ----
    fileInput("fileTemp", "Choisir un fichier de température",
              accept = c(".csv")),
    
    ### Définition de la saison de migration ----
    
      # On laisse choisir : manuel ou à partir d'un fichier
    selectizeInput(
      inputId = "period_mode",
      label = "Définition de la saison :",
      choices = c("Manuel", "Fichier de captures", "----"),
      selected = "----"
    ),
    
      # On affiche un curseur ou un onglet de sélection en fonction
    uiOutput("period_choice"),
    
    hr(),
    fluidPage(
    h6("Indicateurs à afficher"),

    ### Choix des variables ----
      # On crée un arbre dynamique :
      # Les cases possibles à cocher dépendront du choix des hyperparamètres
    shinyTree("variable_choice", checkbox = TRUE)),  
    
    # Ca c'est juste pour enlever les icônes par défaut qui ne nous servent pas
    tags$head(
      tags$style(HTML(".jstree-icon.jstree-themeicon { display: none !important; }"))
    )
    
  ),
  
  ## Onglets ----
  navset_card_underline(
    
    ### Temporal trends tab ----
    nav_panel("Tendances temporelles", 
              ## Temporal trends: sidebar
              page_sidebar(sidebar = sidebar(
                checkboxGroupInput(
                  "trends_options", "Options",
                  choices = list(
                    "Smoother" = "smooth",
                    "Tendance linéaire" = "slope",
                    "Statistiques (linéaire)" = "stats",
                    "Grouper les variables (fenêtres)" = "group"
                  )
                )
              ),
              ## Temporal trends: plot
              plotOutput("trends"), 
              )),
    
    ### Boxplots tab ----
    nav_panel("Boxplots", 
              ## Boxplots: sidebar
              page_sidebar(sidebar = sidebar(
                checkboxGroupInput(
                  "boxplot_options", "Options",
                  choices = list(
                    "Echelles indépendantes" = "independent",
                    "Standardiser par la moyenne" = "std_mean"
                  )
                )
              ),
              ## Boxplots: plot
              plotOutput("boxplots")
              )),
    
    ### Coef of variation tab ----
    nav_panel("Coefficients de variation", 
              ## Coef of variation: plot
              plotOutput("CV")),
    
    ### Co-variation tab ----
    nav_panel("Co-variation", 
              ## Boxplots: sidebar
              page_sidebar(sidebar = sidebar(
                checkboxGroupInput(
                  "covar_options", "Options",
                  choices = list(
                    "Corrélogramme" = "corrplot",
                    "Relations paire à paire" = "paired",
                    "Tableau" = "table"
                  ),
                  selected = c("corrplot")
                )
              ),
              ## Co-variation: plots
              plotOutput("Covar"), 
              hr(), 
              ## Co-variation: table
              tableOutput("Covar_table")
              ) 
    ),
    
    ### Ordination ----
    nav_panel("ACP", 
              ## PCA: sidebar
              page_sidebar(sidebar = sidebar(
                checkboxGroupInput(
                  "pca_options", "Options",
                  choices = list(
                    "Représentation 2D" = "2D",
                    "Contribution des axes" = "axes",
                    "Cercle des corrélations" = "circle",
                    "Tableau" = "table",
                    "Enlever les variables avec des NA" = "rm"
                  ),
                  selected = c("2D")
                )
              ), 
              ## PCA: plots
              plotOutput("PCA"), 
              hr(), 
              ## PCA: table
              tableOutput("PCA_table")
              )
    ),
    
    ### Caractéristiques de la saison ----
    nav_panel("Caractéristiques saison",
              
              tableOutput("s_fixed"),
              tableOutput("s_sliding"),
              
              )
  )
  
  
) # Fin de la définition de l'UI

# Server ----
server <- function(input, output, session) {
  
  ## Import des données ----
  
  ### JDD de températures ----
  temp_df <- reactive({
    req(input$fileTemp)
      read_csv2(input$fileTemp$datapath)
  })
  
  
  ### Fonctions ----
  source("R/Functions/indices_list.R", local = TRUE)
  source("R/Functions/indices_calc.R", local = TRUE)

  ## Définition de la saison de migration ----
  
  output$period_choice <- renderUI({
    
    ##' Mode Manuel ----
      # En mode "manuel", l'utilisateur peut choisir les dates 
      # de début et de fin de la saison
      # (numéros de jour calés sur une année non-bissextile)
    if (input$period_mode == "Manuel") {
      tagList(
        sliderInput("manual_start", "Date de début de saison :", 
                    min = 1, max = 365, value = 60, ticks = F),
        sliderInput("manual_end", "Date de fin de saison :", 
                    min = 1, max = 365, value = 60, ticks = F)
      ) } 
    
    ##' Mode Fichier ----
      # En mode "fichier" les dates de début et de fin de la saison 
      # seront calculées à partir d'un fichier de captures de saumons
      # On peut avoir une période globale "fixed"
      # Ou une par année "sliding"
    else if(input$period_mode == "Fichier de captures") {
      
      tagList(
        fileInput("fileCapt", "Choisir un fichier de captures", 
                  accept = c(".csv")),
        checkboxGroupInput("file_period", "La période est", 
                           choices = list("Fixée" = "fixed", 
                                          "Glissante" = "sliding"), 
                           selected = c("fixed"))
      )
    }
  })
  
  ## Sélection des variables selon les hyperparamètres ----
  
    # On va garder les noms des variables qui nous intéressent
    # En fonction du choix des hyperparamètres (Tcrit etc.)
  get_colnames <- reactive({
    
    req(input$fileTemp, input$period_mode != "----")
    
    # On regroupe les hyperparamètres
    hyperparameters <- 
      list(tcrit = input$tcrit, 
           topt = input$topt, 
           ctmax = input$ctmax,
           n_hw = input$n_hw, 
           n_y = input$n_y,
           file_period = 
             if (input$period_mode == "Manuel") "fixed" else 
               input$file_period
           )
    
    # Cette fonction va permettre de créer un objet pour chaque hyperparamètre,
    # contenant les valeurs sélectionnées
    # Il contient 0 si aucune valeur n'est sélectionnée
    initialize_variables <- function(var_name, input_list) {
      varname <- input_list[[var_name]]
      value <- if (is.null(varname)) 0 else varname
      assign(var_name, value, envir = .GlobalEnv)
    }
    
    lapply(names(hyperparameters), initialize_variables, input_list = hyperparameters)
    
    # Ensuite on ne veut garder que les variables correspondant aux 
    # valeurs d'hyperparamètres choisies
    # On enlève donc les variables concernées hors des valeurs sélectionnées
    criteria <- list(
      list(var = "tcrit", prefix = "c", exclude = c("Crit", "Hw")),
      list(var = "ctmax", prefix = "t", exclude = c("CTmax")),
      list(var = "n_hw", prefix = "n", exclude = "Hw"),
      list(var = "topt", prefix = "o", exclude = "Topt"),
      list(var = "n_y", prefix = "y", exclude = "PrevYear"),
      list(var = "file_period", prefix = "_", exclude = "")
    )
    
    # On filtre ces variables dans un objet contenant tous les noms possibles de variables
    
      # Fonction pour générer les noms de variables
        # Méthode ultra bourrine on peut sûrement faire bcp mieux
        # En fait on utilise indices_calc() sur un JDD simulé
    generate_varnames <- function(topt = 12:20, tcrit = 16:23, ctmax = 24:33,
                                  n_hw = 1:3, n_y = 1:5) {
      
      tibble(
        Tmin = 0,
        Tmax = 0,
        Tmoy = 0,
        DAYN = 1:365,
        YEAR = 1,
        fixed_start = 10,
        fixed_end = 11
      ) %>%
        indices_calc("fixed", topt, tcrit, ctmax, n_hw, n_y) %>%
        colnames() %>%
        .[-1] %>%
        c(., sub("fixed", "sliding", .))
      
    }
    
      # Fonction pour générer les noms de variables
    filter_vars <- function(varnames, criterion) {
      selected <- get(criterion$var)
      varnames %>%
        subset(grepl(paste(paste0(criterion$prefix, selected), collapse = "|"), .) | 
                 !grepl(paste(criterion$exclude, collapse = "|"), .))
    }
    
      # On génère les noms de variables à afficher ici
    colnames <- purrr::reduce(criteria, filter_vars, .init = generate_varnames())
    
    return(colnames)
    
  })
  
    ### Modification de l'UI -----
  
      # Dans l'UI on n'affiche que les variables correspondant 
      # aux hyperparamètres choisis
  
      #### Création de l'arbre des choix disponibles ----
  output$variable_choice <- renderTree({
    create_tree <- function(patterns) {
      matching_cols <- grep(paste(patterns, collapse = "|"), 
                            get_colnames(), value = TRUE) %>%
        setNames(as.list(rep("", length(.))), .)
    }
    
    list(
      "<b>Indices généraux</b>" = create_tree(c("Seas", "avWarm")),
      "<b>Dépassement de Tcrit</b>" = create_tree(c("supCrit", "CumCrit", "CTmax")),
      "<b>Canicules</b>" = create_tree(c("Hw")),
      "<b>Fluctuation de la température</b>" = create_tree(c("sd", "avDailyRange")),
      "<b>Acclimatation</b>" = create_tree(c("SlopeG", "nbNewMaxTCrit", "Prev", 
                                             "avDailyRange")),
      "<b>Effets sublétaux</b>" = create_tree(c("BelCrit"))
    )
  })
  
  
    #### Variables à afficher ----
  columns <- reactive({
    req(input$variable_choice)
    
    selected_vars <- shinyTree::get_selected(input$variable_choice, format = "slices")
    req(length(selected_vars) > 0)
    
    unlist(lapply(selected_vars, function(n) lapply(n, names))) %>% as.vector()
  })
  

  
  ## Préparation au calcul ----
  
    ### Stockage des périodes ----
  periods <- reactiveValues(fixed_periods = NULL, 
                            sliding_periods = NULL)
  
    ### Stockage des JDD contenant les indicateurs ----
  df <- reactiveValues(wide = NULL, long = NULL)
  
    ### Fonction de calcul des indicateurs ----
  transf_temp_df <- reactive({
    req(periods, !is.null(periods$fixed_periods))

      # Préparation du JDD
    dfT <- temp_df() %>%
      cross_join(periods$fixed_periods)
    
        # Pour gérer les années bissextiles en mode manuel
          # En fait on décale juste d'un les numéros de jour
          # S'ils sont après le 28 février
    if(input$period_mode == "Manuel") {
      dfT <- 
        dfT %>%
        group_by(YEAR) %>%
        mutate(bis = ifelse(lubridate::leap_year(YEAR), "yes", "no")) %>%
        mutate(fixed_start = ifelse(bis == "yes" & fixed_start >= 60,
                        fixed_start + 1, fixed_start),
               fixed_end = ifelse(bis == "yes" & fixed_end >= 60,
                        fixed_end + 1, fixed_end))
    }
    
      # On redéfinit les hyperparamètres
        # La strat c'est qu'on va tout calculer
        # Ensuite on fera des subsets du "gros" JDD en fonction des cases cochées
        # Mais comme ça pas besoin de tout re-calculer à chaque fois
    topt <- 12:20; tcrit <- 16:23; ctmax <- 24:33; n_hw <- 1:3; n_y <- 1:5

    # Calcule les indicateurs pour les périodes fixes
    df_transf <- indices_calc(dfT, "fixed", topt, tcrit, ctmax, n_hw, n_y)
    
    # Si elles existent, on fait de même pour les périodes glissantes
    if(input$period_mode == "Fichier de captures") {
      dfT <- dfT %>%
        left_join(periods$sliding_periods)
      
      df_transf <- left_join(df_transf, 
                             indices_calc(dfT, "sliding", topt, tcrit, ctmax, 
                                          n_hw, n_y))
    }
    
    return(df_transf)
    
  })

  ## Calcul des indicateurs ----
  
    # On recalculera tout à chaque changement de mode de définition de la saison
    # Ou d
  observe({
    
    req(input$period_mode)
    
    df$wide <- NULL
    df$long <- NULL
    columns <- NULL
    
    ### Calcul de la saison de migration ----
    
    ###' Manuel ----
    if (input$period_mode == "Manuel") {
      
      periods$fixed_periods <- 
        data.frame(fixed_start = input$manual_start, 
                   fixed_end = input$manual_end)
      periods$sliding_periods <- 0
      
      ###' Fichier ----
    } else if (input$period_mode == "Fichier de captures") {

      req(input$fileCapt)
      salmons_df <- read_csv2(input$fileCapt$datapath)
      
        # Périodes fixées
      fixed_start <- quantile(lubridate::yday(salmons_df$DATE), 0.05)
      fixed_end <- quantile(lubridate::yday(salmons_df$DATE), 0.95)
      
      periods$fixed_periods <-             
        data.frame(fixed_start = fixed_start,
                   fixed_end = fixed_end)
      
        # Périodes glissantes
      periods$sliding_periods <- 
        salmons_df %>%
        group_by(YEAR) %>%
        summarise(
          sliding_start = round(quantile(lubridate::yday(DATE), 0.05)),
          sliding_end = round(quantile(lubridate::yday(DATE), 0.95))
        )
      
    }
    
    ### Calcul des variables ----
    req(columns())  # Assure que columns est recalculé
    
    df_init <- transf_temp_df()
    
    columns <- grep(paste(columns(), collapse="|"), colnames(df_init), value = TRUE)
    req(length(columns) > 0)
    
      # Version avec une variable par co
    df$wide <- df_init %>% dplyr::select(YEAR, columns)
    
      # Version avec une seule colonne et une autre 
    df$long <-  df$wide %>%
      pivot_longer(cols = -YEAR, 
                   names_to = "Metrics", 
                   values_to = "Value") %>%
        # On garde en mémoire si la variable est "fixed" ou "sliding"
      mutate(window = str_extract(Metrics, "(?<=_)[^_]*$"))  %>%
        # Reste du nom de la variable
      mutate(varNoWind = str_remove(Metrics, "_[^_]*$")) %>%
        # Type de variable (= nom sans mode de saison et sans hyperparamètre)
      mutate(var = str_extract(Metrics, "^[^_]*"))
      
  })
  
  
  ## Contenu -----
  
  # ------------------------------------------ #
  ###### 1. Inter-annual variation ---------
  # ------------------------------------------ #
  
  
  output$trends <- renderPlot({
    
    req(df$long)
    
    # Si on veut grouper les variables similaires (fixed et sliding)
      # alors windGroup = TRUE
    if(input$period_mode == "Fichier de captures" &
      "group" %in% input$trends_options & 
       "sliding" %in% input$file_period) {
      windGroup <- TRUE} else{
        windGroup <- FALSE
      }
    
    p_trends <-  
      ggplot(df$long, aes(x = YEAR, y = Value)) +
      geom_point(
          # On groupe par type de fenêtre si coché
        if(windGroup == TRUE) aes(col = window)
        ) +
      geom_line(
        # On groupe par type de fenêtre si coché
        if(windGroup == TRUE) aes(col = window)
           ) +
      list(
        if ("smooth" %in% input$trends_options) 
          geom_smooth(alpha = 0.3, fill = "lightblue"),
        if ("slope" %in% input$trends_options) 
          if(windGroup == TRUE) 
            geom_smooth(aes(col = window), method = "lm")
            else geom_smooth(method = "lm", col = "red"),
        # On groupe par type de fenêtre si coché
        if(windGroup == TRUE) 
          facet_wrap(~varNoWind, scales = "free") else
            facet_wrap(~ Metrics, scales = "free")
      ) +
      labs(x = NULL, y = NULL)
    
    # To calculate summary statistics (slope, R², p-value)
    # Ici il peut y avoir des cas où on a des erreurs on a - d'1 valeur
    # pour une variable à cause des na (e.g. nHw_23_3_fixed)
    # voir plus tard
    if("stats" %in% input$trends_options & "slope" %in% input$trends_options
       & !("group" %in% input$trends_options)) {
      
      mods <- 
        df$long %>%
        group_by(Metrics) %>%
        filter(!all(is.na(Value))) %>%
        summarise(mod = list(lm(Value ~ YEAR))
        ) %>%
        rowwise() %>%
        mutate(beta = signif(coef(mod)[2], 2),
               r2 = round(summary(mod)$r.squared, 2),
               p = round(summary(mod)$coefficients["YEAR", 4], 2),
               signif = ifelse(p < 0.05, "yes", "no"),
               slope = ifelse(signif == "no", "null",
                              ifelse(beta < 0, "neg", "pos")),
               # Adding levels for colour matching
               slope = factor(slope, levels = c("neg", "null", "pos"))
        )
      
      p_trends <-
        p_trends +
        geom_label(data = mods, 
                   aes(label =
                         paste0(
                           "italic(beta)~`=`~", beta,
                           "*`,`~italic(R)^2~`=`~", r2,
                           "*`,`~italic(P)~`=`~", p
                         ), 
                       x = -Inf, 
                       y = Inf,
                       col = slope),
                   hjust   = -0.1,
                   vjust   = 1,
                   parse = TRUE) +
        scale_colour_manual(
          values = setNames(c("#F8766D", "black", "#619CFF"), 
                            c("neg", "null", "pos")),
          guide = "none")
      
      
    }
    
    p_trends
    
  }
  )
  
  
  # ------------------------------------------ #
  ###### 2. Variation per metrics ----------
  # ------------------------------------------ #
  
  
  output$boxplots <- renderPlot({
    
    req(df$long)
    
    df_bp <- df$long
    
    if("std_mean" %in% input$boxplot_options) {
      df_bp <- df_bp %>%
        group_by(Metrics) %>%
        mutate(Value = Value / mean(Value, na.rm = T))
    }
    
    p_boxplots <- 
      ggplot(df_bp, aes(x = Metrics, y = Value, col = Metrics)) +
      geom_boxplot() + 
      geom_point(position = position_jitterdodge()) +
      labs(x = NULL, y = NULL) + 
      theme(axis.text.x = element_text(angle = 90)) +
      list(
        if("independent" %in% input$boxplot_options)
        facet_wrap(~ Metrics, scales = "free", nrow = 1)
      )
      
    
    p_boxplots
    
    
  }
  )
  
  
  # ------------------------------------------ #
  ###### 3. Coefficients of variation  ----
  # ------------------------------------------ #
  
  
  output$CV <- renderPlot({
    
    req(df$long)
    
    p_CV <-
      df$long %>%
      group_by(Metrics) %>%
      summarise(CV = sd(Value, na.rm = T) / mean(Value, na.rm = T)) %>%
      arrange(CV) %>%
      ggplot(aes(y = reorder(Metrics, CV), x = CV)) +
      geom_point() +
      geom_segment(aes(x = 0, xend = CV)) +
      ylab(NULL) +
      xlab("Coefficient of variation")
    
    p_CV
    
  })
  
  
  # ------------------------------------------ #
  ###### 4. Co-variation ------------------
  # ------------------------------------------ #
  
  
  output$Covar <- renderPlot({
    
    req(df$wide)
    
    # Plot of corr. coefficients
    p_covar1 <- 
      df$wide %>%
      dplyr::select(-YEAR) %>%
        # On enlève les cas où on a moins de 4 valeurs non NA
      dplyr::select_if(~ sum(!is.na(.)) > 3) %>% 
      rstatix::cor_mat() %>%
      ggcorrplot::ggcorrplot(method = "circle")
    
    # Paired correlation plot
    p_covar2 <-
      df$wide %>%
      dplyr::select(-YEAR) %>%
      dplyr::select_if(~ sum(!is.na(.)) > 3) %>% 
      GGally::ggpairs()
    
    p_covar <- list(
      if("corrplot" %in% input$covar_options) p_covar1,
      if("paired" %in% input$covar_options) GGally::ggmatrix_gtable(p_covar2)
    ) %>% 
      Filter(Negate(is.null), .)
    
    if(length(p_covar) > 0) cowplot::plot_grid(plotlist = p_covar)
    
  })
  
  output$Covar_table <- renderTable({
    
    req(df$wide)
    
    covarTab <- 
      df$wide %>%
      dplyr::select(-YEAR) %>%
      dplyr::select_if(~ sum(!is.na(.)) > 3) %>% 
      rstatix::cor_mat()
    
    if("table" %in% input$covar_options) covarTab
    
  }, rownames = TRUE)
  
  
  
  # ------------------------------------------ #
  ###### 5. Ordination ------------------
  # ------------------------------------------ #
  
  output$PCA <- renderPlot({
    
    req(df$wide)
    
    # Making the PCA
    df_pca <- if("rm" %in% input$pca_options) 
      dplyr::select(df$wide, where(~ !any(is.na(.)))) 
    else 
      na.omit(df$wide)
    
    metrics_pca <- 
      df_pca %>%
      dplyr::select(-YEAR) %>%
      prcomp(., scale. = TRUE)

    # 2-D representation
    p_pca1 <- 
      metrics_pca %>%
      ggplot2::autoplot(data = df_pca, 
                        colour = "YEAR",
                        loadings = TRUE,
                        loadings.color = "black",
                        loadings.label = TRUE) +
      scale_colour_gradientn(colours = c("blue", "yellow", "red"))
    
    # Axes contribution
    p_pca2 <- factoextra::fviz_eig(metrics_pca)
    
    # Correlation circle
    p_pca3 <- 
      factoextra::fviz_pca_var(metrics_pca,
                               col.var = "contrib",
                               repel = TRUE) +
      scale_colour_gradientn(colours = c("purple", "green3", "orange3")) 
    
    p_pca <- list(
      if("2D" %in% input$pca_options) p_pca1,
      if("axes" %in% input$pca_options) p_pca2,
      if("circle" %in% input$pca_options) p_pca3
    ) %>% 
      Filter(Negate(is.null), .)
    
    if(length(p_pca) > 0) cowplot::plot_grid(plotlist = p_pca)
    
    
  })
  
  output$PCA_table <- renderTable({
    
    req(df$wide)
    
    # Là on la recalcule on pourrait optimiser ça en calculant la table à part
    df_pca <- if("rm" %in% input$pca_options) 
      dplyr::select(df$wide, where(~ !any(is.na(.)))) 
    else 
      na.omit(df$wide)
    
    metrics_pca <- 
      df_pca %>%
      dplyr::select(-YEAR) %>%
      prcomp(., scale. = TRUE)

    # PCA table
    if("table" %in% input$pca_options) metrics_pca$rotation
    
  }, rownames = TRUE)
  
  
  # ------------------------------------------------------ #
  ###### 6. Caractéristiques de la saison ------------------
  # ------------------------------------------------------ #
  
  output$s_fixed <- renderTable({
    
    req(input$period_mode != "----")
    
    file_period <- 
      if (input$period_mode == "Manuel") "fixed" else 
        input$file_period
    
    req(periods)
    
    if("fixed" %in% file_period)
      periods$fixed_periods %>%
      mutate(across(everything(), as.integer))
    

  }, 
  caption = "Période de migration (fixée)",
  caption.placement = getOption("xtable.caption.placement", "top")
  )
  
  output$s_sliding <- renderTable({
    
    req(input$file_period)
    req(periods)
    
    if("sliding" %in% input$file_period)
      periods$sliding_periods %>%
      mutate(across(everything(), as.integer))
    
  }, 
  caption = "Périodes de migration (glissantes)",
  caption.placement = getOption("xtable.caption.placement", "top")
  )

}

shinyApp(ui = ui, server = server)

# Ajouter des textes pour quand ça marche pas.
# Peut-être mettre le choix des hyperparamètres à gauche plutôt ?
# Avoir la valeur de Tadapt dans Season features serait pas mal

# reset des graphiques quand changement de mode :
# https://stackoverflow.com/questions/56555281/r-shiny-resetting-shinytree-node-selections
# quand on groupe -> p-values ?

# maxslopeG avec tomax ?

# remplacer les curseurs pour la date



# J'ai viré ça dans le calcul de df$long
# Normalement c'est OK mais à garder au cas où ^^
# /!\ Important
# On change les valeurs infinies en NA
# Ca arrive quand il y a des max sur des vecteurs nuls par exemple
# (peut-être corrigé, à vérifier)
# mutate(Value = ifelse(is.infinite(Value), NA, Value))  %>%

