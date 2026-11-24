#' KaskadaR: ocena rankingu w modelu kaskadowym
#'
#' Pakiet wyznacza miare Expected Reciprocal Rank (ERR) zaproponowana przez
#' Chapelle'a, Metzlera, Zhanga i Grinspana (2009). Miara opiera sie na modelu
#' kaskadowym: uzytkownik przeglada liste wynikow od gory i konczy poszukiwanie
#' na pierwszym dokumencie, ktory go satysfakcjonuje. Wartosc ERR jest
#' oczekiwana odwrotnoscia pozycji tego zatrzymania.
#'
#' Pakiet zwraca, obok samej miary, pelny profil zatrzymania: prawdopodobienstwo
#' satysfakcji na kazdej pozycji, bezwarunkowe prawdopodobienstwo pierwszego
#' zatrzymania oraz mase uzytkownikow, ktorzy nie zatrzymali sie w widocznej
#' czesci listy.
#'
#' Sciezka analizy:
#' \itemize{
#'   \item \code{\link{oblicz_err}} i \code{\link{profil_zatrzymania}} dla
#'     jednego rankingu,
#'   \item \code{\link{porownaj_err}} dla wielu systemow i zapytan,
#'   \item \code{\link{symuluj_uzytkownikow}} do kontroli modelu symulacja,
#'   \item \code{\link{generuj_rankingi}} do tworzenia danych o znanej
#'     strukturze.
#' }
#'
#' @references Chapelle, O., Metzler, D., Zhang, Y., Grinspan, P. (2009).
#'   Expected reciprocal rank for graded relevance. Proceedings of the 18th
#'   ACM Conference on Information and Knowledge Management, 621--630.
#'   \doi{10.1145/1645953.1646033}
#'
#' @keywords internal
"_PACKAGE"
