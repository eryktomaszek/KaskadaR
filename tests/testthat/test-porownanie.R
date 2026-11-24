test_that("porownanie zgadza sie z obliczeniem dla pojedynczych rankingow", {
  p <- porownaj_err(rankingi_przyklad, oceny_przyklad, maksimum = 4, k = 10)
  wiersz <- p$wyniki[1, ]
  ranking <- rankingi_przyklad[rankingi_przyklad$system == wiersz$system &
                                 rankingi_przyklad$zapytanie == wiersz$zapytanie, ]
  ranking <- ranking[order(ranking$pozycja), ]
  oceny <- oceny_przyklad[oceny_przyklad$zapytanie == wiersz$zapytanie, ]
  g <- oceny$stopien[match(ranking$dokument, oceny$dokument)]
  expect_equal(wiersz$err, oblicz_err(g, 4, k = 10)$err)
  expect_equal(nrow(p$wyniki), 60)
  expect_equal(p$podsumowanie$liczba_zapytan, rep(20L, 3))
})

test_that("wynik nie zalezy od kolejnosci wierszy wejscia", {
  withr::local_seed(11)
  przetasowane <- rankingi_przyklad[sample(nrow(rankingi_przyklad)), ]
  a <- porownaj_err(rankingi_przyklad, oceny_przyklad, maksimum = 4)
  b <- porownaj_err(przetasowane, oceny_przyklad, maksimum = 4)
  expect_equal(a$wyniki, b$wyniki)
})

test_that("dokumenty bez oceny sa liczone i raportowane", {
  rankingi <- data.frame(system = "s", zapytanie = "z1",
                         dokument = c("a", "b", "c"), pozycja = 1:3)
  oceny <- data.frame(zapytanie = "z1", dokument = c("a", "c"),
                      stopien = c(0, 1))
  p <- porownaj_err(rankingi, oceny, maksimum = 1, k = 3)
  # stopnie (0, 0, 1): ERR = 0,5 / 3
  expect_equal(p$wyniki$err, 0.5 / 3)
  expect_equal(p$wyniki$nieocenione, 1L)
})
