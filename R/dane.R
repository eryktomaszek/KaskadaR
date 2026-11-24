#' Przykladowe oceny trafnosci
#'
#' Stopnie trafnosci dokumentow dla 20 zapytan, po 20 dokumentow na
#' zapytanie, na skali 0..4. Zbior wygenerowany funkcja
#' \code{\link{generuj_rankingi}} w skrypcie \code{data-raw/generuj_dane.R}.
#'
#' @format Ramka danych o 400 wierszach i 3 zmiennych:
#' \describe{
#'   \item{zapytanie}{identyfikator zapytania}
#'   \item{dokument}{identyfikator dokumentu}
#'   \item{stopien}{stopien trafnosci, liczba calkowita od 0 do 4}
#' }
#'
#' @source Dane symulowane, \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' table(oceny_przyklad$stopien)
"oceny_przyklad"


#' Przykladowe rankingi trzech systemow
#'
#' Rankingi dokumentow z \code{\link{oceny_przyklad}} wytworzone przez trzy
#' systemy o roznym poziomie szumu (\eqn{\sigma} rowne 0,5, 1 i 3). Zbior
#' wygenerowany funkcja \code{\link{generuj_rankingi}} w skrypcie
#' \code{data-raw/generuj_dane.R}.
#'
#' @format Ramka danych o 1200 wierszach i 4 zmiennych:
#' \describe{
#'   \item{system}{oznaczenie systemu: \code{sigma_0.5}, \code{sigma_1},
#'     \code{sigma_3}}
#'   \item{zapytanie}{identyfikator zapytania}
#'   \item{dokument}{identyfikator dokumentu}
#'   \item{pozycja}{pozycja dokumentu w rankingu systemu dla zapytania}
#' }
#'
#' @source Dane symulowane, \code{data-raw/generuj_dane.R}.
#'
#' @examples
#' head(rankingi_przyklad)
"rankingi_przyklad"
