#' Metody dla wyniku ERR
#'
#' Wypisanie, podsumowanie i wykres obiektu klasy \code{wynik_err}.
#' Podsumowanie zestawia miare z profilem zatrzymania i diagnostyka; wykres
#' pokazuje prawdopodobienstwo pierwszego zatrzymania na kolejnych pozycjach
#' oraz mase uzytkownikow, ktorzy nie zatrzymali sie w pierwszych \eqn{k}
#' pozycjach.
#'
#' @param x,object Obiekt klasy \code{wynik_err} albo
#'   \code{summary.wynik_err}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej; w \code{plot()} do
#'   \code{\link[graphics]{barplot}}.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{summary()}
#'   zwraca obiekt klasy \code{summary.wynik_err} z miara, profilem
#'   zatrzymania i diagnostyka. \code{plot()} niewidocznie zwraca wektor
#'   srodkow slupkow.
#'
#' @examples
#' wynik <- oblicz_err(c(2, 1, 0, 2), maksimum = 2)
#' print(wynik)
#' summary(wynik)
#' plot(wynik)
#'
#' @name metody_wynik_err
NULL

#' @rdname metody_wynik_err
#' @export
print.wynik_err <- function(x, cyfry = 4, ...) {
  k <- x$parametry$k
  etykiety <- c(sprintf("ERR@%d", k),
                sprintf("brak zatrzymania w top-%d", k),
                "srednia pozycja zatrzymania (warunkowo)",
                "skala ocen")
  wartosci <- c(format(x$err, digits = cyfry),
                format(x$dalej, digits = cyfry),
                format(x$pozycja_warunkowa, digits = cyfry),
                sprintf("0..%s", format(x$parametry$maksimum)))
  cat("Expected Reciprocal Rank w modelu kaskadowym\n")
  cat(paste0("  ", format(etykiety), "  ", wartosci), sep = "\n")
  invisible(x)
}

#' @rdname metody_wynik_err
#' @export
summary.wynik_err <- function(object, ...) {
  structure(
    list(
      err = object$err,
      dalej = object$dalej,
      pozycja_warunkowa = object$pozycja_warunkowa,
      status = object$status,
      profil = data.frame(
        pozycja = seq_along(object$stopnie),
        stopien = object$stopnie,
        satysfakcja = object$satysfakcja,
        zatrzymanie = object$zatrzymanie,
        wklad = object$zatrzymanie / seq_along(object$stopnie)
      ),
      parametry = object$parametry,
      diagnostyka = object$diagnostyka
    ),
    class = "summary.wynik_err"
  )
}

#' @rdname metody_wynik_err
#' @export
print.summary.wynik_err <- function(x, cyfry = 4, ...) {
  cat(sprintf("ERR@%d = %s (status: %s)\n\n", x$parametry$k,
              format(x$err, digits = cyfry), x$status))
  profil <- x$profil
  liczbowe <- vapply(profil, is.double, logical(1))
  profil[liczbowe] <- lapply(profil[liczbowe], signif, digits = cyfry)
  print(profil, row.names = FALSE)
  cat(sprintf("\nBrak zatrzymania w top-%d: %s\n", x$parametry$k,
              format(x$dalej, digits = cyfry)))
  cat(sprintf("Srednia pozycja zatrzymania (warunkowo): %s\n",
              format(x$pozycja_warunkowa, digits = cyfry)))
  cat(sprintf("Pozycje faktyczne: %d, dopelnione: %d, suma kontrolna: %s\n",
              x$diagnostyka$liczba_pozycji, x$diagnostyka$dopelnione_pozycje,
              format(x$diagnostyka$suma_kontrolna, digits = 12)))
  invisible(x)
}

#' @rdname metody_wynik_err
#' @export
plot.wynik_err <- function(x, ...) {
  wartosci <- c(x$zatrzymanie, x$dalej)
  etykiety <- c(seq_along(x$zatrzymanie), "dalej")
  kolory <- c(rep("#2E5E9E", length(x$zatrzymanie)), "grey70")
  srodki <- graphics::barplot(
    wartosci, names.arg = etykiety, col = kolory, border = NA,
    ylim = c(0, 1), las = 1,
    xlab = "Pozycja pierwszego zatrzymania",
    ylab = "Prawdopodobienstwo",
    main = sprintf("Profil zatrzymania, ERR@%d = %.4f",
                   x$parametry$k, x$err),
    ...
  )
  invisible(srodki)
}


#' Metody dla porownania systemow
#'
#' Wypisanie, podsumowanie i wykres obiektu klasy \code{porownanie_err}.
#' Wykres pokazuje rozklad ERR po zapytaniach dla kazdego systemu.
#'
#' @param x,object Obiekt klasy \code{porownanie_err}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej; w \code{plot()} do
#'   \code{\link[graphics]{boxplot}}.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{summary()}
#'   zwraca ramke danych z podsumowaniem systemow. \code{plot()}
#'   niewidocznie zwraca statystyki wykresu pudelkowego.
#'
#' @examples
#' porownanie <- porownaj_err(rankingi_przyklad, oceny_przyklad, maksimum = 4)
#' print(porownanie)
#' summary(porownanie)
#' plot(porownanie)
#'
#' @name metody_porownanie_err
NULL

#' @rdname metody_porownanie_err
#' @export
print.porownanie_err <- function(x, cyfry = 4, ...) {
  cat(sprintf("Porownanie systemow miara ERR@%d (skala 0..%s)\n\n",
              x$parametry$k, format(x$parametry$maksimum)))
  podsumowanie <- x$podsumowanie
  podsumowanie$srednia_err <- signif(podsumowanie$srednia_err, cyfry)
  podsumowanie$odchylenie_err <- signif(podsumowanie$odchylenie_err, cyfry)
  print(podsumowanie, row.names = FALSE)
  nieocenione <- sum(x$wyniki$nieocenione)
  if (nieocenione > 0) {
    cat(sprintf("\nDokumenty bez oceny w top-%d (liczone jako stopien 0): %d\n",
                x$parametry$k, nieocenione))
  }
  invisible(x)
}

#' @rdname metody_porownanie_err
#' @export
summary.porownanie_err <- function(object, ...) {
  object$podsumowanie
}

#' @rdname metody_porownanie_err
#' @export
plot.porownanie_err <- function(x, ...) {
  statystyki <- graphics::boxplot(
    err ~ system, data = x$wyniki, col = "#C9D6EA", border = "#2E5E9E",
    las = 1, ylim = c(0, 1), xlab = "System",
    ylab = sprintf("ERR@%d", x$parametry$k),
    main = "Rozklad ERR po zapytaniach", ...
  )
  invisible(statystyki)
}


#' Metody dla symulacji uzytkownikow
#'
#' Wypisanie i wykres obiektu klasy \code{symulacja_err}. Wykres zestawia
#' empiryczny rozklad pozycji zatrzymania z prawdopodobienstwami wynikajacymi
#' z modelu.
#'
#' @param x Obiekt klasy \code{symulacja_err}.
#' @param cyfry Liczba cyfr znaczacych w wydruku.
#' @param ... Argumenty przekazywane dalej; w \code{plot()} do
#'   \code{\link[graphics]{barplot}}.
#'
#' @return \code{print()} niewidocznie zwraca \code{x}. \code{plot()}
#'   niewidocznie zwraca macierz srodkow slupkow.
#'
#' @examples
#' sym <- symuluj_uzytkownikow(c(2, 1, 0, 2), maksimum = 2, n = 2000)
#' print(sym)
#' plot(sym)
#'
#' @name metody_symulacja_err
NULL

#' @rdname metody_symulacja_err
#' @export
print.symulacja_err <- function(x, cyfry = 4, ...) {
  roznica <- x$err_empiryczny - x$teoria$err
  cat(sprintf("Symulacja modelu kaskadowego: %d uzytkownikow, ziarno %s\n",
              x$parametry$n, format(x$parametry$ziarno)))
  cat(sprintf("  ERR ze wzoru:     %s\n", format(x$teoria$err, digits = cyfry)))
  cat(sprintf("  ERR z symulacji:  %s (blad standardowy %s)\n",
              format(x$err_empiryczny, digits = cyfry),
              format(x$blad_standardowy, digits = 2)))
  cat(sprintf("  roznica w bledach standardowych: %.2f\n",
              roznica / x$blad_standardowy))
  invisible(x)
}

#' @rdname metody_symulacja_err
#' @export
plot.symulacja_err <- function(x, ...) {
  wartosci <- rbind(x$rozklad$teoretyczne, x$rozklad$empiryczne)
  srodki <- graphics::barplot(
    wartosci, beside = TRUE, names.arg = x$rozklad$pozycja,
    col = c("#2E5E9E", "#E0A030"), border = NA, las = 1,
    ylim = c(0, 1), xlab = "Pozycja pierwszego zatrzymania",
    ylab = "Prawdopodobienstwo",
    main = "Rozklad zatrzymania: model i symulacja", ...
  )
  graphics::legend("topright", fill = c("#2E5E9E", "#E0A030"), border = NA,
                   legend = c("model", "symulacja"), bty = "n")
  invisible(srodki)
}
