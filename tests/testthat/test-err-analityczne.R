# Warstwa pierwsza: przypadki o wyniku policzonym recznie ze wzorow
# R_i = (2^g_i - 1) / 2^g_max, q_i = R_i * prod_{j<i} (1 - R_j),
# ERR@k = sum_i q_i / i, dalej = prod_{j<=k} (1 - R_j),
# E[I | I <= k] = sum_i i * q_i / (1 - dalej).

test_that("skala binarna, ranking (1, 0)", {
  # R = ((2^1 - 1) / 2^1, 0) = (0,5; 0)
  # q = (0,5; 0,5 * 0) = (0,5; 0)
  # ERR = 0,5 / 1 + 0 / 2 = 0,5; dalej = 0,5 * 1 = 0,5
  # srednia warunkowa = (1 * 0,5) / 0,5 = 1
  w <- oblicz_err(c(1, 0), maksimum = 1)
  expect_equal(w$satysfakcja, c(0.5, 0))
  expect_equal(w$zatrzymanie, c(0.5, 0))
  expect_equal(w$err, 0.5)
  expect_equal(w$dalej, 0.5)
  expect_equal(w$pozycja_warunkowa, 1)
})

test_that("skala binarna, ranking (0, 1)", {
  # R = (0; 0,5), q = (0; 1 * 0,5) = (0; 0,5)
  # ERR = 0 + 0,5 / 2 = 0,25; dalej = 1 * 0,5 = 0,5
  # srednia warunkowa = (2 * 0,5) / 0,5 = 2
  w <- oblicz_err(c(0, 1), maksimum = 1)
  expect_equal(w$zatrzymanie, c(0, 0.5))
  expect_equal(w$err, 0.25)
  expect_equal(w$dalej, 0.5)
  expect_equal(w$pozycja_warunkowa, 2)
})

test_that("skala 0..2, ranking (2, 1)", {
  # R = ((4 - 1) / 4, (2 - 1) / 4) = (0,75; 0,25)
  # q = (0,75; 0,25 * 0,25) = (0,75; 0,0625)
  # ERR = 0,75 + 0,0625 / 2 = 0,78125
  # dalej = 0,25 * 0,75 = 0,1875
  # srednia warunkowa = (0,75 + 2 * 0,0625) / (1 - 0,1875)
  #                   = 0,875 / 0,8125 = 14 / 13
  w <- oblicz_err(c(2, 1), maksimum = 2)
  expect_equal(w$satysfakcja, c(0.75, 0.25))
  expect_equal(w$zatrzymanie, c(0.75, 0.0625))
  expect_equal(w$err, 0.78125)
  expect_equal(w$dalej, 0.1875)
  expect_equal(w$pozycja_warunkowa, 14 / 13)
  expect_identical(w$status, "zdefiniowana")
})

test_that("same zera daja ERR rowne zero i niezdefiniowana srednia", {
  # R = 0 na kazdej pozycji, wiec q = 0, ERR = 0, dalej = 1
  w <- oblicz_err(c(0, 0, 0), maksimum = 4)
  expect_equal(w$err, 0)
  expect_equal(w$dalej, 1)
  expect_true(is.na(w$pozycja_warunkowa))
  expect_identical(w$status, "brak_zatrzymania")
})

test_that("pusta lista daje ERR rowne zero i mase niezatrzymania 1", {
  w <- oblicz_err(numeric(0), maksimum = 1)
  expect_equal(w$err, 0)
  expect_equal(w$dalej, 1)
  expect_true(is.na(w$pozycja_warunkowa))

  w10 <- oblicz_err(numeric(0), maksimum = 1, k = 10)
  expect_equal(w10$err, 0)
  expect_equal(w10$diagnostyka$dopelnione_pozycje, 10)
})

test_that("najwyzszy stopien nie zatrzymuje kazdego uzytkownika", {
  # R_max = 1 - 2^(-g_max): dla g_max = 3 jest to 7/8
  w <- oblicz_err(3, maksimum = 3)
  expect_equal(w$satysfakcja, 7 / 8)
  expect_equal(w$dalej, 1 / 8)
})

test_that("ERR przy ocenach binarnych rozni sie od RR", {
  # Model zaklada R = 0,5 dla dokumentu trafnego przy g_max = 1, wiec
  # pierwszy dokument trafny daje ERR = 0,5, a odwrotnosc pozycji (RR) = 1.
  # Zgodnosc z RR zachodzi tylko dla R w {0, 1}, czego skala nie dopuszcza.
  expect_equal(oblicz_err(c(1, 0, 0), maksimum = 1)$err, 0.5)
})

test_that("profil zatrzymania sumuje wklady do ERR", {
  p <- profil_zatrzymania(c(2, 1), maksimum = 2)
  expect_equal(p$dotarcie, c(1, 0.25))
  expect_equal(p$wklad, c(0.75, 0.03125))
  expect_equal(sum(p$wklad), 0.78125)
  expect_equal(p$zatrzymanie_skumulowane[2], 0.8125)
})
