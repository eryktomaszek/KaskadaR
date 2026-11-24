#' Generowanie rankingow o znanych stopniach trafnosci
#'
#' Procedura tworzy zapytania, dokumenty ze stopniami trafnosci oraz rankingi
#' kilku systemow o roznej jakosci. Dla kazdego zapytania i dokumentu stopien
#' \eqn{g} jest losowany ze skali \eqn{0, \ldots, g_{\max}} z
#' prawdopodobienstwami \code{rozklad}. System o parametrze \eqn{\sigma}
#' porzadkuje dokumenty malejaco wedlug \eqn{g + \varepsilon},
#' \eqn{\varepsilon \sim N(0, \sigma^2)}, niezaleznie dla kazdego dokumentu.
#' Mniejsze \eqn{\sigma} oznacza ranking blizszy idealnemu.
#'
#' Pelne stopnie stanowia prawde konstrukcyjna: dla kazdego rankingu znany
#' jest dokladny profil zatrzymania, a ranking idealny (malejace stopnie)
#' wyznacza najwyzsza osiagalna wartosc ERR dla zapytania.
#'
#' Kolejnosc losowania jest ustalona: najpierw wszystkie stopnie (zapytania
#' po kolei, dokumenty w obrebie zapytania), nastepnie szum dla kolejnych
#' wartosci \code{sigma}.
#'
#' @param liczba_zapytan Liczba zapytan.
#' @param liczba_dokumentow Liczba dokumentow ocenianych dla kazdego
#'   zapytania.
#' @param rozklad Prawdopodobienstwa stopni od 0 do \eqn{g_{\max}}; dlugosc
#'   wektora wyznacza maksimum skali.
#' @param sigma Odchylenia standardowe szumu, po jednym systemie na wartosc.
#' @param ziarno Ziarno generatora liczb losowych. Stan generatora w sesji
#'   uzytkownika zostaje przywrocony po zakonczeniu losowania.
#'
#' @return Lista ze skladnikami:
#'   \describe{
#'     \item{oceny}{ramka danych z kolumnami \code{zapytanie},
#'       \code{dokument}, \code{stopien}}
#'     \item{rankingi}{ramka danych z kolumnami \code{system},
#'       \code{zapytanie}, \code{dokument}, \code{pozycja}}
#'     \item{maksimum}{maksimum skali ocen}
#'     \item{parametry}{lista argumentow wywolania}
#'   }
#'
#' @examples
#' dane <- generuj_rankingi(liczba_zapytan = 5, liczba_dokumentow = 10)
#' head(dane$rankingi)
#' table(dane$oceny$stopien)
#'
#' @export
generuj_rankingi <- function(liczba_zapytan = 100,
                             liczba_dokumentow = 50,
                             rozklad = c(0.5, 0.2, 0.15, 0.1, 0.05),
                             sigma = c(0.5, 1, 3),
                             ziarno = 202713) {
  # TODO: kontrola argumentow (dodatnie liczby calkowite, `rozklad` jako
  #   rozklad prawdopodobienstwa, `sigma` dodatnie i skonczone).
  maksimum <- length(rozklad) - 1L
  zapytania <- sprintf("z%03d", seq_len(liczba_zapytan))
  dokumenty <- sprintf("d%03d", seq_len(liczba_dokumentow))
  liczba_ocen <- liczba_zapytan * liczba_dokumentow

  withr::with_seed(ziarno, {
    stopnie <- sample(0:maksimum, liczba_ocen, replace = TRUE, prob = rozklad)
    szumy <- lapply(sigma, function(s) stats::rnorm(liczba_ocen, sd = s))
  })

  oceny <- data.frame(
    zapytanie = rep(zapytania, each = liczba_dokumentow),
    dokument = rep(dokumenty, times = liczba_zapytan),
    stopien = as.integer(stopnie),
    stringsAsFactors = FALSE
  )

  rankingi <- lapply(seq_along(sigma), function(s) {
    wynik <- stopnie + szumy[[s]]
    # kolejnosc: zapytanie rosnaco, w obrebie zapytania wynik malejaco;
    # remisy rozstrzyga identyfikator dokumentu
    porzadek <- order(oceny$zapytanie, -wynik, oceny$dokument)
    data.frame(
      system = sprintf("sigma_%s", format(sigma[s])),
      zapytanie = oceny$zapytanie[porzadek],
      dokument = oceny$dokument[porzadek],
      pozycja = rep(seq_len(liczba_dokumentow), times = liczba_zapytan),
      stringsAsFactors = FALSE
    )
  })
  rankingi <- do.call(rbind, rankingi)
  rownames(rankingi) <- NULL

  list(
    oceny = oceny,
    rankingi = rankingi,
    maksimum = maksimum,
    parametry = list(liczba_zapytan = liczba_zapytan,
                     liczba_dokumentow = liczba_dokumentow,
                     rozklad = rozklad, sigma = sigma, ziarno = ziarno)
  )
}
