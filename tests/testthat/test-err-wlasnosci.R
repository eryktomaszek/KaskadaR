# Warstwa druga: wlasnosci, ktore musza zachodzic niezaleznie od danych.

# Niezalezna, najprostsza wersja obliczenia: rekurencja po pozycjach
# z jawna petla, bez wektoryzacji i bez postaci stabilnej.
err_rekurencja <- function(g, gmax) {
  dalej <- 1
  err <- 0
  for (i in seq_along(g)) {
    r <- (2^g[i] - 1) / 2^gmax
    err <- err + dalej * r / i
    dalej <- dalej * (1 - r)
  }
  c(err = err, dalej = dalej)
}

test_that("wynik zgadza sie z niezalezna rekurencja", {
  withr::local_seed(1)
  for (powtorzenie in 1:50) {
    gmax <- sample(1:6, 1)
    g <- sample(0:gmax, sample(1:30, 1), replace = TRUE)
    w <- oblicz_err(g, gmax)
    ref <- err_rekurencja(g, gmax)
    expect_equal(w$err, unname(ref["err"]), tolerance = 1e-12)
    expect_equal(w$dalej, unname(ref["dalej"]), tolerance = 1e-12)
  }
})

test_that("masy zatrzymania i niezatrzymania sumuja sie do jednosci", {
  withr::local_seed(2)
  for (powtorzenie in 1:50) {
    gmax <- sample(1:8, 1)
    g <- sample(0:gmax, sample(1:50, 1), replace = TRUE)
    w <- oblicz_err(g, gmax)
    expect_equal(sum(w$zatrzymanie) + w$dalej, 1, tolerance = 1e-12)
    expect_true(all(w$zatrzymanie >= 0))
    expect_true(w$err >= 0 && w$err <= 1)
  }
})

test_that("dopisanie pozycji na koncu nie obniza ERR", {
  withr::local_seed(3)
  for (powtorzenie in 1:50) {
    g <- sample(0:4, 10, replace = TRUE)
    dodatek <- sample(0:4, 3, replace = TRUE)
    expect_gte(oblicz_err(c(g, dodatek), 4)$err, oblicz_err(g, 4)$err)
  }
})

test_that("podniesienie jednego stopnia nie obniza ERR", {
  withr::local_seed(4)
  for (powtorzenie in 1:100) {
    g <- sample(0:3, 8, replace = TRUE)
    i <- sample(which(g < 4), 1)
    wyzszy <- g
    wyzszy[i] <- wyzszy[i] + 1
    expect_gte(oblicz_err(wyzszy, 4)$err, oblicz_err(g, 4)$err - 1e-15)
  }
})

test_that("ranking idealny ma najwyzsze ERR wsrod permutacji", {
  withr::local_seed(5)
  g <- sample(0:4, 7, replace = TRUE)
  idealny <- oblicz_err(sort(g, decreasing = TRUE), 4)$err
  for (powtorzenie in 1:200) {
    expect_lte(oblicz_err(sample(g), 4)$err, idealny + 1e-15)
  }
})

test_that("obciecie i dopelnienie do glebokosci k", {
  g <- c(3, 0, 2, 1, 4)
  expect_equal(oblicz_err(g, 4, k = 3)$err, oblicz_err(g[1:3], 4)$err)
  dopelniony <- oblicz_err(g, 4, k = 8)
  expect_equal(dopelniony$err, oblicz_err(g, 4)$err)
  expect_equal(dopelniony$diagnostyka$liczba_pozycji, 5)
  expect_equal(dopelniony$diagnostyka$dopelnione_pozycje, 3)
})

test_that("duze maksimum skali nie powoduje przepelnienia", {
  # Postac bezposrednia (2^g - 1) / 2^g_max daje Inf / Inf = NaN dla
  # g_max >= 1024; postac stabilna zwraca poprawne wartosci.
  w <- oblicz_err(c(2000, 0, 1999), maksimum = 2000)
  expect_false(anyNA(c(w$err, w$satysfakcja, w$zatrzymanie, w$dalej)))
  expect_equal(w$satysfakcja[1], 1)
  expect_equal(w$satysfakcja[3], 0.5)
  expect_equal(sum(w$zatrzymanie) + w$dalej, 1)
})
