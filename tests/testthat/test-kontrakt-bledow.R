# Warstwa trzecia: dane, przy ktorych metoda nie ma sensu.
#
# Testy opisuja kontrakt bledow z briefu tematu. Oczekiwanym wynikiem jest
# zatrzymanie wykonania z komunikatem, a nie liczba. Walidacja wejscia nie
# jest jeszcze zaimplementowana, wiec testy sa pomijane; po implementacji
# kontroli w R/skala.R (funkcja przygotuj_stopnie) i w R/err.R
# (porownaj_err) nalezy usunac wywolania skip().

pomin <- function() {
  skip("Walidacja wejscia do implementacji (kontrakt bledow, SPEC.md).")
}

test_that("stopien spoza skali zatrzymuje wykonanie", {
  pomin()
  expect_error(oblicz_err(c(1, 5), maksimum = 4),
               "Stopnie musza byc calkowite i nalezec do ustalonej skali")
  expect_error(oblicz_err(c(-1, 2), maksimum = 4),
               "Stopnie musza byc calkowite i nalezec do ustalonej skali")
  expect_error(oblicz_err(c(1.5, 2), maksimum = 4),
               "Stopnie musza byc calkowite i nalezec do ustalonej skali")
})

test_that("brak w stopniach zatrzymuje wykonanie zamiast zamiany na zero", {
  pomin()
  expect_error(oblicz_err(c(NA, 1), maksimum = 1),
               "Stopnie trafnosci nie moga zawierac brakow")
})

test_that("niepoprawne maksimum skali zatrzymuje wykonanie", {
  pomin()
  expect_error(oblicz_err(c(0, 1), maksimum = 0),
               "Maksimum skali musi byc dodatnia liczba calkowita")
  expect_error(oblicz_err(c(0, 1), maksimum = 1.5),
               "Maksimum skali musi byc dodatnia liczba calkowita")
  expect_error(oblicz_err(c(0, 1), maksimum = c(1, 2)),
               "Maksimum skali musi byc dodatnia liczba calkowita")
})

test_that("niepoprawna glebokosc oceny zatrzymuje wykonanie", {
  pomin()
  expect_error(oblicz_err(c(0, 1), maksimum = 1, k = -1), "k")
  expect_error(oblicz_err(c(0, 1), maksimum = 1, k = 2.5), "k")
})

test_that("tabele porownania bez wymaganych kolumn zatrzymuja wykonanie", {
  pomin()
  expect_error(porownaj_err(rankingi_przyklad[, -4], oceny_przyklad, 4),
               "pozycja")
  expect_error(porownaj_err(rankingi_przyklad, oceny_przyklad[, -3], 4),
               "stopien")
})
