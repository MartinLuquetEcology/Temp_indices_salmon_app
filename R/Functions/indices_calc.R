###### Fonction pour calculer tous les indicateurs sur un jeu de données ######

### Cette fonction indices_calc() calcule l'ensemble des indicateurs thermiques
### Présents dans le script indices_list.R
### Pour tous les valeurs d'hyperparamètres choisis
### Et pour un mode de saison donné ("Fixed" ou "Sliding")
### Sentez-vous libre de modifier la fonction
### Et notamment d'ajouter/modifier des indicateurs ! (Voir le README)

## Martin Luquet, 2024_08_06
## Contact : martin.luquet.pro@gmail.com

## Désolé s'il y a un petit mélange de français et d'anglais j'ai changé
## plusieurs fois en cours de route !

indices_calc <- function(df, per, topt, tcrit, ctmax, n_hw, n_y) {
  
  
  df %>%
    filter(DAYN >= get(paste0(per, "_start")) & DAYN <= get(paste0(per, "_end"))) %>%
    mutate(avTmoy = mean(Tmoy)) %>%
    group_by(YEAR, !!sym(paste0(per, "_start")), !!sym(paste0(per, "_end"))) %>%
    summarise(   
      
      # Calcul des indicateurs à partir d'ici
      
      # Indicateurs ne dépendant d'aucun hyperparamètre ----
      
        ## Indicateurs généraux ----
      
          # Température moyenne
          "avSeas_{per}" := avSeas(Tmoy),
          # Température maximale pendant la saison
          "maxSeas_{per}" := maxSeas(Tmax),
          # Température moyenne pendant la période chaude (Juillet-Aout)
          "avWarmJA_{per}" := avWarmJA(Tmoy, get(paste0(per, "_start"))),
          # Température moyenne pendant la période chaude (top 10% températures)
          "avWarm90perc_{per}" := avWarm90perc(DAYN, Tmoy),
      
        ## Indicateurs liés à la fluctuation de la température ----
      
          # Variance sur toute la saison
          "sdSeas_{per}" := sdSeas(Tmoy),
          # Amplitude journalière sur toute la saison
          "avDailyRange_{per}" := avDailyRange(Tmax, Tmin),
          # Variance autour de la tendance saisonnière
          "sdTrend_{per}" := sdTrend(DAYN,Tmoy),
          # Variance autour de la tendance saisonnière pendant la phase ascendante
          "sdTrendAsc_{per}" := sdTrendAsc(DAYN, Tmoy),
          # Variance pendant la période chaude (10% top temperature)
          "sdWarm_90perc_{per}" := sdWarm90perc(DAYN, Tmoy),
          # Variance pendant la période chaude (Juillet-Aout)
          "sdWarm_JA_{per}" := sdWarmJA(Tmoy, get(paste0(per, "_start"))),
      
        ## Indicateurs liés à l'acclimatation ----
      
          # Pente maximale de l'augmentation des températures
          "maxSlopeG_{per}" := maxSlopeG(DAYN, Tmax),
          # Pente moyenne de l'augmentation des températures
          "meanSlopeG_{per}" := meanSlopeG(DAYN, Tmax),
      
      # Indicateurs dépendant de Tcrit uniquement ----
      
        over(tcrit, 
             .fns = list( 
               
               ## Indicateurs liés au dépassement de la température critique ----
               
               # Nombre de jours où Tmax dépasse Tcrit
               nDTmaxsupCrit = ~ nDTsup(Tmax, .x),
               # Nombre de jours où Tmin dépasse Tcrit
               nDTminsupCrit = ~ nDTsup(Tmin, .x),
               # Cumul des températures moy. au-delà de tcrit
               TCumCrit = ~ TCum(Tmoy, .x),
               
               ## Effets sublétaux et coûts d'opportunité ----
               
               # Température moyenne sous Tcrit
               meanTBelCrit = ~ meanTBelCrit(Tmoy, .x),
               
               ## Déviation avec les températures précédentes ----
               # Nombre de fois où les poissons sont soumis à une température jamais expérimentée
               nbNewMaxTCrit = ~ nbNewMaxTcrit(Tmax, .x),
               # Ecart moyen avec le précédent maximum (positifs uniquement)
               avDevCritPrev = ~ avDevCritPrev(Tmax, .x),
               # Cumul des écarts avec le précédent maximum (positifs uniquement)
               sumDevCritPrev = ~ sumDevCritPrev(Tmax, .x)
               
               
           ), 
           .names = paste0("{fn}_c{x}_", per)
           ),
      
      # Indicateurs dépendant de CTmax uniquement ----
      
        over(ctmax, 
             .fns = list(
               ## Indicateurs liés au dépassement de la température létale ----
               
               # Nombre de jours où Tmax dépasse CTmax
               nDTmaxsupCTmax = ~ nDTsup(Tmax, .x)
               
           ), 
           .names = paste0("{fn}_t{x}_", per)
           ),

      # Indicateurs dépendant de Tcrit et N ----
      
        ## Indicateurs liés aux canicules ----
      over2x(tcrit,
             n_hw,
             .fns = list(
               # Nombre de périodes de canicules
               nHw = ~ nHw(Tmin, .x, .y),
               # Durée moyenne d'une période de canicule
               meanHw = ~ meanHw(Tmin, .x, .y),
               # Durée maximale d'une période de canicule
               maxDurHw = ~ maxDurHw(Tmin, .x, .y),
               # Durée totale des périodes de canicule sur la saison
               cumDurHw = ~ sum(lHw(Tmin, .x, .y)),
               # Durée totale de la saison caniculaire
               totDurHw = ~ totDurHw(Tmin, .x, .y),
               # Accumulation des températures pendant les périodes de canicule
               TcumHw = ~ TcumHw(Tmin, Tmoy, .x, .y),
               # Accumulation pondérée des températures pendant les périodes de canicule
               weighTcumHw = ~ weighTcumHw(Tmin, Tmoy, .x, .y),
               # Temps de récupération moyen pendant les périodes de canicule,
               meanRecovTimeHw = ~ meanRecovTimeHw(Tmin, .x, .y),
               # Régularité des périodes de canicule,
               sdRecovTimeHw = ~ sdRecovTimeHw(Tmin, .x, .y),
               # Proportion de la saison caniculaire occupée par des évènements stressants
               propHw = ~ propHw(Tmin, .x, .y),
               # Variation des températures pendant la période caniculaire
               sdWarm_Hw = ~ sdWarmHw(Tmin, Tmoy, .x, .y),
               # Moyenne des températures pendant la période caniculaire
               avWarmHw = ~ avWarmHw(Tmin, Tmoy, .x, .y)
             ),
             .names = paste0("{fn}_c{x}_n{y}_", per)
      ),
      
      # Indicateurs dépendant de Tcrit et Topt ----
      
      over2x(tcrit,
             topt,
             .fns = list(
               
               ## Effets sublétaux et coûts d'opportunité ----
               
               # Déviation par rapport à Topt
               avDevToptBelCrit = ~ avDevToptBelCrit(Tmoy, .x, .y)
               
             ),
             .names = paste0("{fn}_c{x}_o{y}_", per)
      ), 
      
        ## Cas particulier : version adaptative ----
          # L'embêtant ici c'est que le nom qu'on donne à l'indicateur
          # est différent de la valeur de Topt
          # (le nom contient "Adapt" alors que la valeur est numérique)
          # On ne peut donc pas employer la même procédure
          # Sûrement améliorable dans le futur !
            over(tcrit,
                 .fns = list(
                   # Déviation par rapport à Topt
                   avDevToptBelCrit = ~ avDevToptBelCrit(Tmoy, .x, avTmoy[1])
                   ),
                 .names = paste0("{fn}_c{x}_oAdapt_", per)
                 )
      
    ) %>%
    ungroup() %>% 
    dplyr::select(-paste0(per, c("_start", "_end"))) %>%
    
      # Indicateurs dépendant de NY ----
  
  mutate(
    over(n_y,
         .fns = list(
           # Ecart entre le max de l'année y et le max de l'année y-n
           devMaxPrevYear = ~ devMaxPrevYear(get(paste0("maxSeas_", per)), .x)
         ),
         .names = paste0("{fn}_y{x}_", per))
  ) 
  
}
