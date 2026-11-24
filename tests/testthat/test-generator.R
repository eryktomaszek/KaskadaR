# Procedura generowania danych o znanej strukturze: stopnie trafnosci sa
# prawda konstrukcyjna, a systemy roznia sie wylacznie poziomem szumu.

test_that("generator jest powtarzalny i nie zmienia stanu generatora", {
  set.seed(1)
  przed <- .Random.seed
  a <- generuj_rankingi(liczba_zapytan = 5, liczba_dokumentow = 8)
  expect_identical(.Random.seed, przed)
  b <- generuj_rankingi(liczba_zapytan = 5, liczba_dokumentow = 8)
  expect_identical(a, b)
})

test_that("struktura danych odpowiada parametrom", {
  d <- generuj_rankingi(liczba_zapytan = 4, liczba_dokumentow = 6,
                        sigma = c(1, 2))
  expect_equal(nrow(d$oceny), 24)
  expect_equal(nrow(d$rankingi), 48)
  expect_equal(d$maksimum, 4L)
  expect_setequal(unique(d$rankingi$system), c("sigma_1", "sigma_2"))
  pozycje <- tapply(d$rankingi$pozycja,
                    paste(d$rankingi$system, d$rankingi$zapytanie),
                    function(p) identical(sort(p), 1:6))
  expect_true(all(pozycje))
})

test_that("czestosci stopni odpowiadaja zadanemu rozkladowi", {
  d <- generuj_rankingi(liczba_zapytan = 200, liczba_dokumentow = 50)
  czestosci <- as.numeric(table(factor(d$oceny$stopien, levels = 0:4))) /
    nrow(d$oceny)
  # 10000 ocen: blad standardowy udzialu nie przekracza 0,005
  expect_equal(czestosci, c(0.5, 0.2, 0.15, 0.1, 0.05), tolerance = 0.03)
})

test_that("mniejszy szum daje wyzsze srednie ERR", {
  d <- generuj_rankingi()
  p <- porownaj_err(d$rankingi, d$oceny, maksimum = d$maksimum, k = 10)
  srednie <- stats::setNames(p$podsumowanie$srednia_err,
                             p$podsumowanie$system)
  expect_gt(srednie[["sigma_0.5"]], srednie[["sigma_1"]])
  expect_gt(srednie[["sigma_1"]], srednie[["sigma_3"]])
})

test_that("ranking idealny ogranicza ERR kazdego systemu", {
  d <- generuj_rankingi(liczba_zapytan = 10, liczba_dokumentow = 20)
  idealne <- tapply(d$oceny$stopien, d$oceny$zapytanie, function(g) {
    oblicz_err(sort(g, decreasing = TRUE), d$maksimum, k = 10)$err
  })
  p <- porownaj_err(d$rankingi, d$oceny, maksimum = d$maksimum, k = 10)
  expect_true(all(p$wyniki$err <= idealne[p$wyniki$zapytanie] + 1e-12))
})
