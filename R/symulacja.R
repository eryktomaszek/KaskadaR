#' Symulacja uzytkownikow modelu kaskadowego
#'
#' Kazdy symulowany uzytkownik przeglada ranking od pierwszej pozycji.
#' Na pozycji \eqn{i} zostaje usatysfakcjonowany z prawdopodobienstwem
#' \eqn{R_i} i wtedy konczy poszukiwanie; w przeciwnym razie przechodzi do
#' nastepnej pozycji. Uzytkownik, ktory nie zatrzymal sie w pierwszych
#' \eqn{k} pozycjach, wnosi do sredniej odwrotnosci pozycji zero.
#'
#' Srednia odwrotnosc pozycji zatrzymania w symulacji jest estymatorem ERR@k
#' metoda Monte Carlo. Roznica miedzy ta srednia a wartoscia wyznaczona
#' wzorem powinna miescic sie w kilku bledach standardowych; empiryczny
#' rozklad pozycji zatrzymania powinien odtwarzac wektor \eqn{q_i}.
#'
#' @inheritParams oblicz_err
#' @param n Liczba symulowanych uzytkownikow.
#' @param ziarno Ziarno generatora liczb losowych. Stan generatora w sesji
#'   uzytkownika zostaje przywrocony po zakonczeniu symulacji.
#'
#' @return Obiekt klasy \code{symulacja_err}, lista ze skladnikami:
#'   \describe{
#'     \item{pozycje}{wektor pozycji zatrzymania kolejnych uzytkownikow,
#'       \code{NA} dla braku zatrzymania}
#'     \item{err_empiryczny}{srednia odwrotnosc pozycji zatrzymania}
#'     \item{blad_standardowy}{blad standardowy sredniej}
#'     \item{rozklad}{ramka danych z czestoscia empiryczna i
#'       prawdopodobienstwem teoretycznym dla kazdej pozycji i dla braku
#'       zatrzymania}
#'     \item{teoria}{obiekt \code{wynik_err} dla tego samego rankingu}
#'     \item{parametry}{lista z wartosciami \code{maksimum}, \code{k},
#'       \code{n} i \code{ziarno}}
#'   }
#'
#' @examples
#' sym <- symuluj_uzytkownikow(c(2, 1, 0, 2), maksimum = 2, n = 2000)
#' sym
#'
#' @export
symuluj_uzytkownikow <- function(stopnie, maksimum, n = 10000,
                                 ziarno = 202713, k = length(stopnie)) {
  # TODO: kontrola argumentow `n` (dodatnia liczba calkowita) i `ziarno`
  #   (pojedyncza liczba calkowita albo NULL dla biezacego stanu generatora).
  teoria <- oblicz_err(stopnie, maksimum, k)
  r <- teoria$satysfakcja
  k <- length(r)

  pozycje <- withr::with_seed(ziarno, {
    losowe <- matrix(stats::runif(n * k), nrow = n, ncol = k)
    sukces <- losowe < rep(r, each = n)
    zatrzymal <- rowSums(sukces) > 0
    pierwsza <- max.col(sukces, ties.method = "first")
    ifelse(zatrzymal, pierwsza, NA_integer_)
  })

  odwrotnosc <- ifelse(is.na(pozycje), 0, 1 / pozycje)
  czestosc <- tabulate(pozycje[!is.na(pozycje)], nbins = k) / n

  rozklad <- data.frame(
    pozycja = c(as.character(seq_len(k)), "dalej"),
    empiryczne = c(czestosc, mean(is.na(pozycje))),
    teoretyczne = c(teoria$zatrzymanie, teoria$dalej),
    stringsAsFactors = FALSE
  )

  structure(
    list(
      pozycje = as.integer(pozycje),
      err_empiryczny = mean(odwrotnosc),
      blad_standardowy = stats::sd(odwrotnosc) / sqrt(n),
      rozklad = rozklad,
      teoria = teoria,
      parametry = list(maksimum = maksimum, k = k, n = n, ziarno = ziarno)
    ),
    class = "symulacja_err"
  )
}
