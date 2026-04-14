#' Filtrer les anomalies de trajets velo
#'
#' Supprime les lignes considerees comme anormales :
#' anomalies signalees, totaux trop grands et valeurs negatives.
#'
#' @param trajet Un data.frame de trajets velo.
#'
#' @return Un data.frame filtre.
#' @export
filtre_anomalie <- function(trajet) {
  dplyr::filter(
    trajet,
    is.na(`Probabilité de présence d'anomalies`),
    Total < 10000,
    Total > 0
  )
}




#' Compter le nombre total de trajets
#'
#' Calcule la somme de la colonne `Total`.
#'
#' @param trajet Un data.frame de trajets velo.
#'
#' @return Un nombre correspondant au total des trajets.
#' @export
compter_nombre_trajets <- function(trajet) {
  trajet |>
    dplyr::pull(Total) |>
    sum(na.rm = TRUE)
}




#' Compter le nombre de boucles
#'
#' Compte le nombre de boucles distinctes dans un jeu de donnees.
#'
#' @param trajet Un data.frame de trajets velo.
#'
#' @return Un entier correspondant au nombre de boucles distinctes.
#' @export
compter_nombre_boucle <- function(trajet) {
  trajet |>
    dplyr::pull(`Numéro de boucle`) |>
    dplyr::n_distinct()
}






#' Trouver le trajet avec le maximum de passages
#'
#' Filtre les anomalies puis identifie la paire boucle-jour
#' avec le plus grand nombre de passages.
#'
#' @param trajet Un data.frame de trajets velo.
#'
#' @return Un data.frame d'une ligne contenant la boucle,
#' le jour, le total, la moyenne du jour et la moyenne de la boucle.
#' @export
trouver_trajet_max <- function(trajet) {
  trajet_filtre <- filtre_anomalie(trajet)

  trajet_max <- trajet_filtre |>
    dplyr::slice_max(Total, n = 1, with_ties = FALSE) |>
    dplyr::select(`Boucle de comptage`, Jour, Total)

  trajet_max$moyenne_jour_identique <- trajet_filtre |>
    dplyr::filter(Jour == trajet_max$Jour) |>
    dplyr::pull(Total) |>
    mean(na.rm = TRUE)

  trajet_max$moyenne_boucle_identique <- trajet_filtre |>
    dplyr::filter(`Boucle de comptage` == trajet_max$`Boucle de comptage`) |>
    dplyr::pull(Total) |>
    mean(na.rm = TRUE)

  trajet_max
}




#' Calculer la distribution des trajets sur la semaine
#'
#' Calcule le nombre total de trajets pour chaque jour de la semaine.
#'
#' @param trajet Un data.frame de trajets velo.
#'
#' @return Un data.frame contenant les jours de la semaine
#' et le nombre total de trajets.
#' @export
calcul_distribution_semaine <- function(trajet) {
  trajet |>
    dplyr::count(`Jour de la semaine`, wt = Total, sort = TRUE, name = "trajets")
}





#' Tracer la distribution hebdomadaire des trajets
#'
#' Filtre les anomalies, calcule la distribution des trajets
#' par jour de la semaine et produit un diagramme en colonnes.
#'
#' @param trajet Un data.frame de trajets velo.
#'
#' @return Un graphique ggplot.
#' @export
plot_distribution_semaine <- function(trajet) {
  trajet_weekday <- trajet |>
    filtre_anomalie() |>
    calcul_distribution_semaine() |>
    dplyr::mutate(
      jour = forcats::fct_recode(
        factor(`Jour de la semaine`),
        "lundi" = "1",
        "mardi" = "2",
        "mercredi" = "3",
        "jeudi" = "4",
        "vendredi" = "5",
        "samedi" = "6",
        "dimanche" = "7"
      )
    )

  ggplot2::ggplot(trajet_weekday) +
    ggplot2::aes(x = jour, y = trajets) +
    ggplot2::geom_col()
}




#' Filtrer un jeu de donnees par numero de boucle
#'
#' @param trajet Un data.frame de trajets velo.
#' @param boucle Un vecteur de numeros de boucle a conserver.
#'
#' @return Un data.frame filtre.
#' @export
filtrer_trajet <- function(trajet, boucle) {
  if (is.null(boucle)) {
    return(trajet)
  }

  stopifnot(is.character(boucle))

  i <- grep("^Numéro de boucle$", names(trajet))
  if (length(i) != 1) {
    stop("Colonne 'Numéro de boucle' introuvable ou ambigue.")
  }

  trajet[
    as.character(trajet[[i]]) %in% boucle,
    ,
    drop = FALSE
  ]
}

