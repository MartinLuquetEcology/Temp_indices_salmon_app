########### Fonctions de calcul des indicateurs thermiques ##############

### Ces fonctions permettent le calcul d'indicateurs thermiques
### Sentez-vous libre de les modifier
### Ou d'en ajouter ! (Voir le README)

# Pour qu'un indicateur soit rangé dans telle catégorie il doit contenir
# les chaînes de caractère ci-dessous :
  # "Indices généraux" : "Seas" ou "avWarm"
  # "Dépassement de Tcrit" : "supCrit" ou "CumCrit"
  # "Canicules" : "Hw"
  # "Fluctuation de la température" : "sd" ou "avDailyRange"
  # "Acclimatation" : "SlopeG", "nbNewMaxTCrit" ou "Prev" ou "avDailyRange"
  # "Effets sublétaux" : "BelCrit"
# Bien sûr un indicateur peut être dans plusieurs catégories
# Pour modifier ces règles vous pouvez aussi modifier le script principal
# tempIndices_shinyApp.R l. 340 à 348

## Martin Luquet, 2024_08_06
## Contact : martin.luquet.pro@gmail.com

## Désolé s'il y a un petit mélange de français et d'anglais j'ai changé
## plusieurs fois en cours de route !

# Arguments communs aux fonctions ----

  # -- temp.var: une mesure de température journalière 
    # (vecteur, format numérique)
    # e.g. Tmoy (moyenne journalière), Tmax (max journalier), Tmin
  # -- startSeas: numéro de jour de début de saison 
    # (vecteur, format integer)
  # -- day.nb: vecteur contenant le pas de temps (e.g. numéro de jour de l'année)
    # format integer
  # -- temp.var.hw: mesure de température journalière à considérer pour 
  # définir une canicule lorsque dépassée au-delà d'un seuil pendant un certain temps
    # (e.g. Tmin)
    # (vecteur, format numérique)
  # -- temp.var.cum: mesure de température journalière pour lesquelles
  # de température doivent être sommées ou moyennées
    # (e.g. Tmoy)
    # (vecteur, format numérique)
  # -- maxSeas : nom de la variable contenant le max de l'année (chaîne de caractères)

  # -- tcrit: valeur de Température Critique Tcrit, format numérique
  # -- topt: valeur de Température Optimale Topt, format numérique
  # -- ndays: nombre de jours pour définir une canicule (format integer)
  # -- nY : nombre d'années de décalage entre les deux années à considérer

# Fonctions internes ----

  #### Liées au cumul des températures au-delà de Tcrit

  # Vector of records of a desired temperature metrics over a given threshold
tSeason <- function(temp.var, tcrit) {
  temp.var[temp.var >= tcrit] }

  #### Liées aux calcul des périodes de canicules, etc.

  # rle.T()
  # Lengths and values of each period over or under Tcrit
rle.T <- function(temp.var.hw, tcrit) 
  rle(ifelse(temp.var.hw >= tcrit, 1, 0))

  # lHw()
  # Length of each heatwave period (T > Tcrit during n days)
lHw <- function(temp.var.hw, tcrit, ndays) {
  with(
    rle.T(temp.var.hw, tcrit), lengths[values == 1]) %>% 
    .[.>= ndays]
}

  # pos.Hw()
  # Position of heatwave periods in the sequence of events
pos.Hw <- function(temp.var.hw, tcrit, ndays) {
  with(rle.T(temp.var.hw, tcrit), which(values == 1 & lengths >= ndays))
}

  #### Pour fitter des fonctions (gaussiennes, ...) sur les séries temporelles

    # -- x: the time vector (e.g. day numbers)
    # -- y: the temperature value vector (e.g. Tmoy)
    # -- tomax: if TRUE, values will be computed only during the ascending phase
      # of temperature

  # fitG()
  # Fit a Gaussian distribution on a temperature time-series
  # Using least squares (with optim)
fitG <- 
  function(x, y){
    
    # On recalibre x pour qu'il commence à 1, plus simple
    x <- seq(x)
    
    f = function(p) {
      if (p[2] <= 0) return(Inf)  # L'écart-type doit être positif
      d <- p[3] * dnorm(x, mean = p[1], sd = p[2])
      sum((d-y)^2)
    }
    
    # Initialisation parameters
    mu <- x[y == max(y)]
    sig <- sd(x)
    scale <- sum(y)
    
    optim(c(mu, sig, scale), f)
  }

  # fitted.G()
  # Get predicted temperature values from fitG
fitted.G <- function(x, y, tomax = FALSE) {
  fit <- fitG(x, y)
  fitVal <- fit$par[3] * dnorm(seq(x), fit$par[1], fit$par[2])
  
    # To get predicted temperatures during the ascending phase only
  if(tomax == TRUE) {
    mu <- round(fit$par[1])
    fitVal <- if(mu <= length(x) & mu >= 1) fitVal[1:mu] else NA
  }
  
  return(fitVal)
}

  #### Fonctions liées à l'acclimatation/aux températures déjà expérimentées les poissons

  # devTmax()
  # Vecteur des déviations positives avec le précédent maximum expérimenté, 
  # à chaque nouvel évènement de stress
    # ici un évènement de stress = un jour où la T° (e.g. Tmax) > Tcrit
devTmax <- function(temp.var, tcrit) {
  
  ovTcrit <- which(temp.var >= tcrit)
  
  devs <- rep(NA, length(ovTcrit))
  i <- 1
  
  # Si on est déjà au-dessus du seuil au début de la saison,
  # on enlève le premier jour
  if(1 %in% ovTcrit) {
    ovTcrit <- ovTcrit[-1]
    devs <- devs[-1]
  }
  
  # Vecteur des déviations ici :
  for (t in ovTcrit) {
    
    prevMax <- max(temp.var[1:t-1])
    devs[i] <- temp.var[t] - prevMax
    i <- i + 1
    
  }
  
  return(devs)
  
}

# 1. Indicateurs généraux ----

  ## Common arguments
    # -- temp.var: is the daily temperature metrics (e.g. Tmoy)

  # avSeas(): average Temperature during the Season of migration
avSeas <- function(temp.var) {mean(temp.var)}

  # maxSeas(): maximum Temperature during the Season of migration
maxSeas <- function(temp.var) {max(temp.var)}

  # sdSeas(): standard deviation of temp. during the Season of migration
sdSeas <- function(temp.var) {sd(temp.var)}

  # avWarmJA(): Température pendant la période chaude (Juillet-Août)
    # -- startSeas: numéro de jour de début de saison
avWarmJA <- function(temp.var, startSeas) {
  # On cale par rapport à la date de début choisie
  # Prend en compte les années bissextiles où on a transformé startSeas
  mean(temp.var[(183 - startSeas[1]) : (244 - startSeas[1])])
}

  # avWarm90perc(): Température pendant la période chaude (Top 10%)
    # Requiert fitted.G pour ajuster une gaussienne (voir Fonctions internes)
      # -- day.bn: day number
avWarm90perc <- function(day.nb, temp.var) {
  fit <- fitted.G(day.nb, temp.var)
  q90 <- temp.var[fit > quantile(fit, 0.9)]
  mean(q90)
}

  # avWarmHw(): Average temperature during the seasonal period prone to heatwaves
    # Voir "Arguments communs" ou "3. Canicules" pour les détails des arguments
avWarmHw <- function(temp.var.hw, temp.var.cum, tcrit, ndays) {
  rleT <- rle.T(temp.var.hw, tcrit)
  posHw <- pos.Hw(temp.var.hw, tcrit, ndays)
  
  if (length(posHw) > 0) {
    days <- cumsum(with(rleT, lengths))[range(posHw)]
    avW <- mean(temp.var.cum[days[1] : days[2]]) 
    
  } else {
    avW <- NA
  }
  
  return(avW)
  
}

# 2. Dépassement de Tcrit ----

## Common arguments
  #-- temp.var: the temperature metrics (e.g. Tmin, Tmax)
  #-- tcrit: the threshold value (Tcrit)

  # ndTsup()
  # Number of days when a desired temperature metrics is over a given threshold
    # Marche aussi pour le dépassement de CTmax
nDTsup <- function(temp.var, tcrit) {
  length(tSeason(temp.var, tcrit)) }

  # TCum()
  # Accumulation of a given temperature metrics over a given threshold
TCum <- function(temp.var, tcrit) {
  sum(tSeason(temp.var, tcrit) - tcrit) %>%
  round(1) }


# 3. Canicules ----

## Common arguments
  #-- temp.var.hw: the temperature metrics defining a heatwave when the threshold
    # is exceeded (e.g. Tmin)
  #-- temp.var.cum: when appropriate, the temperature metrics over which
    # temperature values should be summed/averaged (e.g. Tmoy)
  #-- tcrit: the threshold value (Tcrit)
  #-- ndays: the number of days needed to define a heatwave

  # nHw()
  # Nombre de canicules
nHw <- function(temp.var.hw, tcrit, ndays) {
  length(lHw(temp.var.hw, tcrit, ndays)) }

  # meanHw()
  # Durée moyenne d'une période de canicule
meanHw <- function(temp.var.hw, tcrit, ndays) {
  mean(lHw(temp.var.hw, tcrit, ndays)) }

  # maxHw()
  # Maximal duration of a heatwave period
maxDurHw <- function(temp.var.hw, tcrit, ndays) {
  
  lengths <- lHw(temp.var.hw, tcrit, ndays)
  ifelse(length(lengths > 0), max(lengths), NA)
  
}

  # cumDurHw()
  # Durée cumulée des périodes de canicule sur la saison
cumDurHw <- function(temp.var.hw, tcrit, ndays) {
  sum(lHw(temp.var.hw, tcrit, ndays)) }

  # totDurHw()
  # Durée totale de la période propice aux canicules
totDurHw <- function(temp.var.hw, tcrit, ndays) {
  
  sum(
    with(rle.T(temp.var.hw, tcrit), {
      valid_indices <- which(values == 1 & lengths >= ndays)
      if (length(valid_indices) == 0) {
        NA  # Retourne NA si aucune séquence valide n'est trouvée
      } else {
        valid_indices %>%
          .[c(1, length(.))] %>%
          {lengths[.[1]:.[2]]}
      }
    })
  )
  
}

  # TcumHw()
  # Temperature cumulée pendant les canicules
TcumHw <- 
  function(temp.var.hw, temp.var.cum, tcrit, ndays){
    rleT <- rle.T(temp.var.hw, tcrit)
    posHw <- pos.Hw(temp.var.hw, tcrit, ndays)
    
    if (length(posHw) > 0) {
      end <- cumsum(with(rleT, lengths))[posHw]
      start <- end - with(rleT, lengths)[posHw] + 1
      days <- unlist(sapply(1:length(start),
                            function(X) seq(start[X], end[X])))
      ddSum <- round(sum(temp.var.cum[days] - tcrit), 1)
      
    } else {
      ddSum <- NA
    }
    
    return(ddSum)
  }

  # weighTcumHw()
  # Cumul des températures pendant les canicules, pondéré par leur durée
weighTcumHw <- 
  function(temp.var.hw, temp.var.cum, tcrit, ndays){
    rleT <- rle.T(temp.var.hw, tcrit)
    posHw <- pos.Hw(temp.var.hw, tcrit, ndays)
    
    if (length(posHw) > 0) {
      end <- cumsum(with(rleT, lengths))[posHw]
      dur <- with(rleT, lengths)[posHw]
      start <- end - dur + 1
      days <- unlist(sapply(1:length(start),
                            function(X) seq(start[X], end[X])))
      weight <- unlist(sapply(dur, function(X) seq(1, X)))
      dd <- temp.var.cum[days] - tcrit
      pondDd <- dd * weight
      
      ddSum <- round(sum(pondDd), 1)
      
    } else {
      ddSum <- NA
    }
    
    return(ddSum)
  }

  # meanRecovTimeHw()
  # Temps de récupération moyen entre chaque canicule
meanRecovTimeHw <- function(temp.var.hw, tcrit, ndays) {
  rleT <- rle.T(temp.var.hw, tcrit)
  posHw <- pos.Hw(temp.var.hw, tcrit, ndays)
  
  if (length(posHw) > 0) {
    per <- seq(posHw[1], tail(posHw, n = 1)) 
    recup <- per[!per %in% posHw]
    dur <- with(rleT, lengths)[recup]
    mDur <- mean(dur)
    
  } else {
    mDur <- NA
  }
  
  return(mDur)
}

  # sdRecovTimeHw()
  # Régularité des évènements stressants
sdRecovTimeHw <- function(temp.var.hw, tcrit, ndays) {
  rleT <- rle.T(temp.var.hw, tcrit)
  posHw <- pos.Hw(temp.var.hw, tcrit, ndays)
  
  if (length(posHw) > 0) {
    # Les périodes stressantes (canicules)
    per <- seq(posHw[1], tail(posHw, n = 1)) 
    # Les périodes de 'récupération'
    recup <- per[!per %in% posHw]
    # La durée de chaque période de récup
    dur <- with(rleT, lengths)[recup]
    # L'écart-type de ces durées
    sdDur <- sd(dur)
    
  } else {
    sdDur <- NA
  }
  
  return(sdDur)
}

  # propHw()
  # Proportion of heatwaves during the seasonal period prone to heatwaves
propHw <- function(temp.var.hw, tcrit, ndays) {
  rleT <- rle.T(temp.var.hw, tcrit)
  posHw <- pos.Hw(temp.var.hw, tcrit, ndays)
  
  if (length(posHw) > 0) {
    per <- seq(posHw[1], tail(posHw, n = 1)) 
    stressHw <- sum(with(rleT, lengths)[posHw])
    totHw <- sum(with(rleT, lengths)[per])
    prop <- stressHw/totHw
    
  } else {
    prop <- NA
  }
  
  return(prop)
}

# 4. Fluctuation des températures ----

## Common arguments
  # -- day.nb: the time vector (e.g. day numbers)
  # -- temp.var: the temperature value vector (e.g. Tmoy)
  # -- tomax: if TRUE, values will be computed only during the ascending phase
    # of temperature

  # avDailyRange()
  # Amplitude journalière des températures
    # Tmax = max quotidien mesuré, Tmin = min quotidien mesuré
avDailyRange <- function(Tmax, Tmin) { mean(Tmax - Tmin) }

  # sdTrend()
  # Ecart-type autour de la tendance saisonnière
    # Requiert fitted.G pour ajuster une gaussienne (voir Fonctions internes)
sdTrend <- function(day.nb, temp.var, tomax = FALSE) {
  
  fit <- fitted.G(day.nb, temp.var, tomax)
  
  # To get temp.var values only during the ascending phase
  if(tomax == TRUE) temp.var <- temp.var[1:length(fit)]
  
  sd(abs(temp.var - fit))
}

  # sdTrendAsc()
  # Ecart-type autour de la tendance saisonnière
  # Pendant la période où les températures montent
    # Requiert fitted.G pour ajuster une gaussienne (voir Fonctions internes)
sdTrendAsc <- function(day.nb, temp.var) {sdTrend(day.nb, temp.var, tomax = TRUE)}

# sdTrend90perc()
# Get variance during the warm period (10% of highest temperatures)
  # Requiert fitted.G pour ajuster une gaussienne (voir Fonctions internes)
sdWarm90perc <- function(day.nb, temp.var) {
  fit <- fitted.G(day.nb, temp.var)
  q90 <- temp.var[fit > quantile(fit, 0.9)]
  sd(q90)
}

  # sdWarmHw()
  # Ecart-type des températures pendant la période de canicules
    # Voir la partie 3. Canicules pour les arguments
sdWarmHw <- function(temp.var.hw, temp.var.cum, tcrit, ndays) {
  rleT <- rle.T(temp.var.hw, tcrit)
  posHw <- pos.Hw(temp.var.hw, tcrit, ndays)
  
  if (length(posHw) > 0) {
    days <- cumsum(with(rleT, lengths))[range(posHw)]
    sdW <- sd(temp.var.cum[days[1] : days[2]]) 
    
  } else {
    sdW <- NA
  }
  
  return(sdW)
  
}

  # sdWarmJA()
  # Get variance during the warm period (July and August)
    # Will return NA if fixed_end is before the end of July
sdWarmJA <- function(temp.var, startSeas) {
    # On cale par rapport à la date de début choisie
    # Prend en compte les années bissextiles où on a transformé startSeas
  sd(temp.var[(183 - startSeas[1]) : (244 - startSeas[1])])
}



# 5. Acclimatation ----

  # maxSlopeG()
  # Pente maximale d'augmentation des températures
maxSlopeG <- function(day.nb, temp.var) {
  #-- day.nb: the time vector (e.g. day numbers)
  #-- temp.var: the temperature value vector (e.g. Tmoy)
  
  fit <- fitG(day.nb, temp.var)
  
  # Le point d'inflexion correspond à la pente pour day.nb = mu-sd
  # On peut la calculer avec la formule ci-dessous
  
  # On calcule l'amplitude à partir de l'échelle et du SD
  amp <- (fit$par[3] / (fit$par[2] * sqrt(2*pi)))
  sd <- fit$par[2]
  
  # Ici on peut obtenir la pente au point d'inflexion (mu-sd)
  slope <- (amp / sd) * exp(-1/2)
  
  return(slope)
}

  # meanSlopeG()
  # Pente moyenne d'augmentation des températures
meanSlopeG <- function(day.nb, temp.var) {
  #-- day.nb: the time vector (e.g. day numbers)
  #-- temp.var: the temperature value vector (e.g. Tmoy)
  
  fit <- fitG(day.nb, temp.var)
  
  # Ci-dessous on va calculer la pente moyenne
    # (pendant la période où les températures montent)
  
  # On calcule l'amplitude à partir de l'échelle et du SD
  amp <- (fit$par[3] / (fit$par[2] * sqrt(2*pi)))
  sd <- fit$par[2]
  
  # Ici on peut obtenir la pente moyenne sur la première moitié de la gaussienne
    # En fait on fait la moyenne de la dérivée sur l'intervalle -Inf, mu
    # (simplification de intégrale de f' sur intégrale de f)
  avSlope <- 2 / (sd * sqrt(2 * pi))
  
  return(avSlope)
}

  # NbNewMaxTcrit()
  # Nombre de fois où le poisson est exposé à un nouveau max. saisonnier
    # (au-dessus de Tcrit)
nbNewMaxTcrit <- function(temp.var, tcrit) {
  
  maxT <- -Inf
  count <- 0
  
  # Le vecteur des T° supérieures à tcrit
  supTcrit <- temp.var[temp.var >= tcrit]
  
  for (t in supTcrit) {
    # Chaque fois qu'on a un nouveau max le compteur augmente de 1
    if (t > maxT) {
      maxT <- t
      count <- count + 1
    }
  }
  
  return(count)
  
}

  # avDevCritPrev()
  # Ecart positif moyen entre chaque nouvel évènement stressant et le précédent 
  # max. expérimenté dans la saison
    # Requiert devTmax (voir Fonctions internes)
avDevCritPrev <- function(temp.var, tcrit) {
  
  devs <- devTmax(temp.var, tcrit)
  mean(devs[devs > 0])
  
}

  # sumDevCritPrev()
  # Somme des écarts postifis chaque nouvel évènement stressant et le précédent 
  # max. expérimenté dans la saison
    # Requiert devTmax (voir Fonctions internes)
sumDevCritPrev <- function(temp.var, tcrit) {
  
  devs <- devTmax(temp.var, tcrit)
  sum(devs[devs > 0])
  
}

  # devMaxPrevYear()
  # Ecart entre le max de l'année N et le max de l'année N-nY
    # -- maxSeas : nom de la variable contenant le max de l'année (chaîne de caractères)
    # -- nY : nombre d'années de décalage entre les deux années à considérer
devMaxPrevYear <- function(maxSeas, nY) {
  maxSeas - lag(maxSeas, nY)
  
}


# 6. Effets sublétaux ----

## Common arguments
  #-- temp.var: the temperature variable (e.g. Tmoy)
  #-- tcrit: Tcrit value
  #-- topt: Topt value

  # meanTBelCrit()
  # Mean temperature below Tcrit
meanTBelCrit <- function(temp.var, tcrit) {
  mean(temp.var[temp.var < tcrit])
}
  
  # avDevToptBelCrit()
  # Average absolute deviation to Topt
avDevToptBelCrit <- function (temp.var, tcrit, topt) {
  mean(abs(temp.var[temp.var < tcrit] - topt))
}
  



