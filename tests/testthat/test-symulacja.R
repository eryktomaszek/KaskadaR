# Kontrola modelu symulacja: srednia odwrotnosc pozycji zatrzymania
# symulowanych uzytkownikow estymuje ERR@k. Granica 4 bledow standardowych
# ogranicza odsetek falszywych alarmow ponizej 1e-4 dla jednego rankingu.

test_that("symulacja odtwarza ERR w granicach bledu Monte Carlo", {
  rankingi <- list(c(2, 1), c(0, 1, 0, 2, 2), c(4, 0, 3, 1, 0, 2), c(1, 1, 1))
  maksima <- c(2, 2, 4, 1)
  for (i in seq_along(rankingi)) {
    sym <- symuluj_uzytkownikow(rankingi[[i]], maksima[i], n = 20000,
                                ziarno = 100 + i)
    expect_lt(abs(sym$err_empiryczny - sym$teoria$err),
              4 * sym$blad_standardowy)
  }
})

test_that("empiryczny rozklad zatrzymania odtwarza q", {
  sym <- symuluj_uzytkownikow(c(2, 1, 0, 2), maksimum = 2, n = 50000)
  odchylenie <- sqrt(sym$rozklad$teoretyczne *
                       (1 - sym$rozklad$teoretyczne) / 50000)
  roznica <- abs(sym$rozklad$empiryczne - sym$rozklad$teoretyczne)
  expect_true(all(roznica <= 4 * odchylenie + 1e-12))
  expect_equal(sum(sym$rozklad$empiryczne), 1)
})

test_that("symulacja jest powtarzalna i nie zmienia stanu generatora", {
  set.seed(99)
  przed <- .Random.seed
  a <- symuluj_uzytkownikow(c(2, 1, 0), maksimum = 2, n = 500, ziarno = 7)
  expect_identical(.Random.seed, przed)
  b <- symuluj_uzytkownikow(c(2, 1, 0), maksimum = 2, n = 500, ziarno = 7)
  expect_identical(a$pozycje, b$pozycje)
})

test_that("uzytkownik bez zatrzymania wnosi zero", {
  sym <- symuluj_uzytkownikow(c(0, 0), maksimum = 1, n = 100)
  expect_true(all(is.na(sym$pozycje)))
  expect_equal(sym$err_empiryczny, 0)
})
