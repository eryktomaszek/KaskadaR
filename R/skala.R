# Skala ocen trafnosci i prawdopodobienstwo satysfakcji.
#
# Stopien g_i nalezy do skali 0..g_max ustalonej wspolnie dla wszystkich
# porownywanych systemow. Prawdopodobienstwo satysfakcji na pozycji i wynosi
#     R_i = (2^g_i - 1) / 2^g_max.
# Obliczenie prowadzi sie w postaci 2^(g_i - g_max) - 2^(-g_max), ktora dla
# duzego maksimum skali nie przekracza zakresu liczb zmiennoprzecinkowych.
# Dopelnienie 1 - R_i liczy sie osobno, bez odejmowania od jedynki wartosci
# bliskiej jedynce.

satysfakcja <- function(stopnie, maksimum) {
  2^(stopnie - maksimum) - 2^(-maksimum)
}

dopelnienie_satysfakcji <- function(stopnie, maksimum) {
  (1 - 2^(stopnie - maksimum)) + 2^(-maksimum)
}

# Przygotowanie wektora stopni do glebokosci k: obciecie dluzszej listy
# i dopelnienie krotszej zerowa trafnoscia. Liczba faktycznych pozycji trafia
# do diagnostyki wyniku.
przygotuj_stopnie <- function(stopnie, maksimum, k) {
  # TODO: kontrola wejscia zgodna z kontraktem bledow (SPEC.md):
  #   - `stopnie` jako wektor liczb calkowitych z zakresu 0..maksimum,
  #     komunikat "Stopnie musza byc calkowite i nalezec do ustalonej skali",
  #   - brak wartosci NA w `stopnie`, komunikat
  #     "Stopnie trafnosci nie moga zawierac brakow"; brak nie moze zostac
  #     niejawnie zastapiony zerem,
  #   - `maksimum` jako pojedyncza dodatnia liczba calkowita, komunikat
  #     "Maksimum skali musi byc dodatnia liczba calkowita",
  #   - `k` jako pojedyncza nieujemna liczba calkowita.
  #   Komunikaty bez znakow diakrytycznych, stop(..., call. = FALSE).
  #   Testy: tests/testthat/test-kontrakt-bledow.R (obecnie pomijane).
  if (!is.numeric(stopnie)) {
    stop("Argument `stopnie` musi byc wektorem liczbowym.", call. = FALSE)
  }
  k <- as.integer(k)
  liczba_pozycji <- min(length(stopnie), k)
  wynik <- numeric(k)
  wynik[seq_len(liczba_pozycji)] <- stopnie[seq_len(liczba_pozycji)]
  list(
    stopnie = wynik,
    liczba_pozycji = liczba_pozycji,
    dopelnione = k - liczba_pozycji
  )
}
