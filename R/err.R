#' Expected Reciprocal Rank i rozklad zatrzymania
#'
#' Funkcja wyznacza miare ERR dla jednego rankingu wraz z rozkladem pozycji
#' pierwszego zatrzymania uzytkownika w modelu kaskadowym.
#'
#' Dla stopni trafnosci \eqn{g_1, \ldots, g_k} na skali
#' \eqn{0, \ldots, g_{\max}} prawdopodobienstwo satysfakcji na pozycji
#' \eqn{i} wynosi \eqn{R_i = (2^{g_i} - 1) / 2^{g_{\max}}}. Bezwarunkowe
#' prawdopodobienstwo pierwszego zatrzymania na pozycji \eqn{i} wynosi
#' \eqn{q_i = R_i \prod_{j < i} (1 - R_j)}, a miara
#' \eqn{ERR@k = \sum_{i=1}^{k} q_i / i}. Uzytkownik, ktory nie zatrzymal sie
#' w pierwszych \eqn{k} pozycjach, wnosi do miary zero; jego masa
#' \eqn{\prod_{j=1}^{k} (1 - R_j)} jest zwracana jako skladnik \code{dalej}.
#'
#' Maksimum skali jest parametrem definicji ocen, wspolnym dla porownywanych
#' systemow, a nie najwyzszym stopniem wystepujacym w danym rankingu. Dla ocen
#' binarnych \eqn{g_{\max} = 1}, wiec dokument trafny zatrzymuje uzytkownika
#' z prawdopodobienstwem 0,5.
#'
#' @param stopnie Wektor stopni trafnosci w kolejnosci rankingu, liczby
#'   calkowite z zakresu od 0 do \code{maksimum}.
#' @param maksimum Najwyzszy stopien skali ocen.
#' @param k Glebokosc oceny. Lista krotsza od \code{k} jest dopelniana
#'   pozycjami o zerowej trafnosci, a dluzsza obcinana.
#'
#' @return Obiekt klasy \code{wynik_err}, lista ze skladnikami:
#'   \describe{
#'     \item{err}{wartosc ERR@k}
#'     \item{stopnie}{stopnie uzyte w obliczeniu, po dopelnieniu lub obcieciu}
#'     \item{satysfakcja}{wektor prawdopodobienstw satysfakcji \eqn{R_i}}
#'     \item{zatrzymanie}{wektor prawdopodobienstw pierwszego zatrzymania
#'       \eqn{q_i}}
#'     \item{dalej}{prawdopodobienstwo braku zatrzymania w pierwszych
#'       \eqn{k} pozycjach}
#'     \item{pozycja_warunkowa}{srednia pozycja zatrzymania warunkowo wzgledem
#'       zatrzymania w pierwszych \eqn{k} pozycjach; \code{NA}, gdy
#'       zatrzymanie jest niemozliwe}
#'     \item{parametry}{lista z wartosciami \code{maksimum} i \code{k}}
#'     \item{diagnostyka}{liczba faktycznych pozycji, liczba pozycji
#'       dopelnionych oraz suma kontrolna \eqn{\sum_i q_i + q_{dalej}}}
#'     \item{status}{\code{"zdefiniowana"} albo \code{"brak_zatrzymania"},
#'       gdy srednia warunkowa nie istnieje}
#'   }
#'
#' @references Chapelle, O., Metzler, D., Zhang, Y., Grinspan, P. (2009).
#'   Expected reciprocal rank for graded relevance. Proceedings of the 18th
#'   ACM Conference on Information and Knowledge Management, 621--630.
#'   \doi{10.1145/1645953.1646033}
#'
#' @seealso \code{\link{profil_zatrzymania}}, \code{\link{porownaj_err}}
#'
#' @examples
#' # Skala 0..2, ranking (2, 1): ERR = 0,75 + 0,0625 / 2 = 0,78125
#' wynik <- oblicz_err(c(2, 1), maksimum = 2)
#' wynik
#' summary(wynik)
#'
#' # Ta sama lista oceniana do glebokosci 5
#' oblicz_err(c(2, 1), maksimum = 2, k = 5)$diagnostyka
#'
#' @export
oblicz_err <- function(stopnie, maksimum, k = length(stopnie)) {
  przygotowane <- przygotuj_stopnie(stopnie, maksimum, k)
  g <- przygotowane$stopnie
  k <- length(g)

  r <- satysfakcja(g, maksimum)
  # dojscie[i] = prod_{j < i} (1 - R_j); ostatni element to masa `dalej`
  dojscie <- c(1, cumprod(dopelnienie_satysfakcji(g, maksimum)))
  q <- r * dojscie[seq_len(k)]
  dalej <- dojscie[k + 1L]
  pozycje <- seq_len(k)

  err <- sum(q / pozycje)
  masa_zatrzymania <- sum(q)
  if (masa_zatrzymania > 0) {
    pozycja_warunkowa <- sum(pozycje * q) / masa_zatrzymania
    status <- "zdefiniowana"
  } else {
    pozycja_warunkowa <- NA_real_
    status <- "brak_zatrzymania"
  }

  structure(
    list(
      err = err,
      stopnie = g,
      satysfakcja = r,
      zatrzymanie = q,
      dalej = dalej,
      pozycja_warunkowa = pozycja_warunkowa,
      parametry = list(maksimum = maksimum, k = k),
      diagnostyka = list(
        liczba_pozycji = przygotowane$liczba_pozycji,
        dopelnione_pozycje = przygotowane$dopelnione,
        suma_kontrolna = masa_zatrzymania + dalej
      ),
      status = status
    ),
    class = "wynik_err"
  )
}


#' Profil zatrzymania uzytkownika
#'
#' Tabela pozycja po pozycji: stopien trafnosci, prawdopodobienstwo
#' satysfakcji, prawdopodobienstwo dotarcia do pozycji, prawdopodobienstwo
#' pierwszego zatrzymania i wklad pozycji do miary ERR. Tabela pokazuje
#' mechanizm miary: wysoki stopien na pozycji poczatkowej zmniejsza
#' prawdopodobienstwo dotarcia do wszystkich dalszych pozycji.
#'
#' @inheritParams oblicz_err
#'
#' @return Ramka danych z kolumnami \code{pozycja}, \code{stopien},
#'   \code{satysfakcja}, \code{dotarcie}, \code{zatrzymanie},
#'   \code{zatrzymanie_skumulowane} i \code{wklad}. Suma kolumny \code{wklad}
#'   jest rowna ERR@k.
#'
#' @examples
#' profil_zatrzymania(c(2, 1, 0, 2), maksimum = 2)
#'
#' @export
profil_zatrzymania <- function(stopnie, maksimum, k = length(stopnie)) {
  wynik <- oblicz_err(stopnie, maksimum, k)
  pozycje <- seq_along(wynik$stopnie)
  data.frame(
    pozycja = pozycje,
    stopien = wynik$stopnie,
    satysfakcja = wynik$satysfakcja,
    dotarcie = c(1, cumprod(
      dopelnienie_satysfakcji(wynik$stopnie, wynik$parametry$maksimum)
    ))[pozycje],
    zatrzymanie = wynik$zatrzymanie,
    zatrzymanie_skumulowane = cumsum(wynik$zatrzymanie),
    wklad = wynik$zatrzymanie / pozycje
  )
}


#' Porownanie systemow miara ERR
#'
#' Funkcja wyznacza ERR@k dla kazdej pary system i zapytanie, a nastepnie
#' srednia miare kazdego systemu. Stopnie trafnosci pochodza z tabeli ocen
#' wspolnej dla wszystkich systemow, z jednym maksimum skali.
#'
#' Dokument obecny w rankingu, a nieobecny w tabeli ocen, otrzymuje stopien
#' zero; liczba takich dokumentow jest raportowana w kolumnie
#' \code{nieocenione}.
#'
#' @param rankingi Ramka danych z kolumnami \code{system}, \code{zapytanie},
#'   \code{dokument} i \code{pozycja}.
#' @param oceny Ramka danych z kolumnami \code{zapytanie}, \code{dokument}
#'   i \code{stopien}.
#' @inheritParams oblicz_err
#'
#' @return Obiekt klasy \code{porownanie_err}, lista ze skladnikami:
#'   \describe{
#'     \item{wyniki}{ramka danych z kolumnami \code{system}, \code{zapytanie},
#'       \code{err}, \code{dalej}, \code{liczba_pozycji}, \code{nieocenione}}
#'     \item{podsumowanie}{ramka danych z kolumnami \code{system},
#'       \code{liczba_zapytan}, \code{srednia_err}, \code{odchylenie_err}}
#'     \item{parametry}{lista z wartosciami \code{maksimum} i \code{k}}
#'   }
#'
#' @examples
#' porownanie <- porownaj_err(rankingi_przyklad, oceny_przyklad,
#'                            maksimum = 4, k = 10)
#' porownanie
#' summary(porownanie)
#'
#' @export
porownaj_err <- function(rankingi, oceny, maksimum, k = 10) {
  # TODO: kontrola struktury tabel (wymagane kolumny, typy, jednoznacznosc
  #   par zapytanie-dokument w `oceny`, jednoznacznosc pozycji w rankingu
  #   systemu dla zapytania) oraz jawna regula dla dokumentow bez oceny
  #   (argument rozstrzygajacy: stopien zero, pominiecie albo zatrzymanie).
  #   Zapytania obecne w ocenach, a nieobecne w rankingu systemu, wymagaja
  #   osobnego raportu, aby srednie systemow dotyczyly tego samego zbioru.
  klucz_ocen <- paste(oceny$zapytanie, oceny$dokument, sep = "\r")
  klucz_rankingu <- paste(rankingi$zapytanie, rankingi$dokument, sep = "\r")
  stopien <- oceny$stopien[match(klucz_rankingu, klucz_ocen)]
  nieoceniony <- is.na(stopien)
  stopien[nieoceniony] <- 0

  grupy <- split(
    data.frame(pozycja = rankingi$pozycja, stopien = stopien,
               nieoceniony = nieoceniony),
    list(rankingi$system, rankingi$zapytanie),
    drop = TRUE, sep = "\r"
  )
  nazwy <- do.call(rbind, strsplit(names(grupy), "\r", fixed = TRUE))

  miary <- lapply(grupy, function(g) {
    g <- g[order(g$pozycja), , drop = FALSE]
    w <- oblicz_err(g$stopien, maksimum, k)
    c(err = w$err, dalej = w$dalej,
      liczba_pozycji = w$diagnostyka$liczba_pozycji,
      nieocenione = sum(g$nieoceniony[seq_len(min(nrow(g), k))]))
  })
  miary <- do.call(rbind, miary)

  wyniki <- data.frame(
    system = nazwy[, 1],
    zapytanie = nazwy[, 2],
    err = miary[, "err"],
    dalej = miary[, "dalej"],
    liczba_pozycji = as.integer(miary[, "liczba_pozycji"]),
    nieocenione = as.integer(miary[, "nieocenione"]),
    stringsAsFactors = FALSE
  )
  wyniki <- wyniki[order(wyniki$system, wyniki$zapytanie), , drop = FALSE]
  rownames(wyniki) <- NULL

  podsumowanie <- data.frame(
    system = sort(unique(wyniki$system)),
    stringsAsFactors = FALSE
  )
  podsumowanie$liczba_zapytan <- as.integer(
    table(wyniki$system)[podsumowanie$system]
  )
  podsumowanie$srednia_err <- as.numeric(
    tapply(wyniki$err, wyniki$system, mean)[podsumowanie$system]
  )
  podsumowanie$odchylenie_err <- as.numeric(
    tapply(wyniki$err, wyniki$system, stats::sd)[podsumowanie$system]
  )

  structure(
    list(
      wyniki = wyniki,
      podsumowanie = podsumowanie,
      parametry = list(maksimum = maksimum, k = k)
    ),
    class = "porownanie_err"
  )
}
