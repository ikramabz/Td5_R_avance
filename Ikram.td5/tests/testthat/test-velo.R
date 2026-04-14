library(testthat)

# =========================
# Donnees de test
# =========================

trajet_test <- tibble::tibble(
  `Numéro de boucle` = c("880", "881", "880", "882", "883"),
  Jour = c("2025-10-20", "2025-10-20", "2025-10-21", "2025-10-22", "2025-10-22"),
  Total = c(100, 50, 40, -5, 12000),
  `Probabilité de présence d'anomalies` = c(NA, NA, NA, NA, "Forte"),
  `Jour de la semaine` = c("1", "1", "2", "3", "3"),
  `Boucle de comptage` = c("Boucle A", "Boucle B", "Boucle A", "Boucle C", "Boucle D")
)

trajet_propre <- tibble::tibble(
  `Numéro de boucle` = c("880", "881", "880", "882"),
  Jour = c("2025-10-20", "2025-10-20", "2025-10-21", "2025-10-22"),
  Total = c(100, 50, 40, 80),
  `Probabilité de présence d'anomalies` = c(NA, NA, NA, NA),
  `Jour de la semaine` = c("1", "1", "2", "3"),
  `Boucle de comptage` = c("Boucle A", "Boucle B", "Boucle A", "Boucle C")
)

# =========================
# Tests de filtre_anomalie()
# =========================

test_that("filtre_anomalie retire les lignes anormales", {
  res <- filtre_anomalie(trajet_test)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  expect_equal(res$Total, c(100, 50, 40))
})

# =========================
# Tests de compter_nombre_trajets()
# =========================

test_that("compter_nombre_trajets somme correctement la colonne Total", {
  res <- compter_nombre_trajets(trajet_propre)

  expect_equal(res, 270)
})

# =========================
# Tests de compter_nombre_boucle()
# =========================

test_that("compter_nombre_boucle compte les boucles distinctes", {
  res <- compter_nombre_boucle(trajet_propre)

  expect_equal(res, 3)
})

# =========================
# Tests de trouver_trajet_max()
# =========================

test_that("trouver_trajet_max renvoie la ligne du maximum apres filtrage", {
  res <- trouver_trajet_max(trajet_propre)

  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$`Boucle de comptage`, "Boucle A")
  expect_equal(res$Jour, "2025-10-20")
  expect_equal(res$Total, 100)
})

test_that("trouver_trajet_max calcule les moyennes attendues", {
  res <- trouver_trajet_max(trajet_propre)

  expect_equal(res$moyenne_jour_identique, 75)
  expect_equal(res$moyenne_boucle_identique, 70)
})

# =========================
# Tests de calcul_distribution_semaine()
# =========================

test_that("calcul_distribution_semaine calcule les totaux par jour", {
  res <- calcul_distribution_semaine(trajet_propre)

  res <- res[order(res$`Jour de la semaine`), ]

  expect_s3_class(res, "data.frame")
  expect_equal(res$`Jour de la semaine`, c("1", "2", "3"))
  expect_equal(res$trajets, c(150, 40, 80))
})

# =========================
# Tests de plot_distribution_semaine()
# =========================

test_that("plot_distribution_semaine renvoie un objet ggplot", {
  res <- plot_distribution_semaine(trajet_propre)

  expect_s3_class(res, "ggplot")
})

# =========================
# Tests de filtrer_trajet()
# =========================

test_that("filtrer_trajet renvoie uniquement les boucles demandees", {
  res <- filtrer_trajet(trajet_propre, boucle = c("880", "881"))

  expect_s3_class(res, "data.frame")
  expect_setequal(unique(res$`Numéro de boucle`), c("880", "881"))
})

test_that("filtrer_trajet renvoie un tableau vide pour une boucle inconnue", {
  res <- filtrer_trajet(trajet_propre, boucle = "lala")

  expect_equal(nrow(res), 0)
})

test_that("filtrer_trajet renvoie une erreur si boucle n'est pas de type character", {
  expect_error(filtrer_trajet(trajet_propre, boucle = 10))
})



test_that("filtrer_trajet renvoie le jeu complet si boucle est NULL", {
  res <- filtrer_trajet(trajet_propre, boucle = NULL)

  expect_equal(res, trajet_propre)
})
