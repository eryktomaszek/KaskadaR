# Zbiory przykladowe pakietu.
#
# Skrypt nie wchodzi w sklad zbudowanego pakietu; zapisuje zbiory do katalogu
# data/. Uruchomienie z katalogu glownego pakietu po instalacji lub
# devtools::load_all():
#     Rscript -e "devtools::load_all(); source('data-raw/generuj_dane.R')"
#
# Zbiory powstaja ta sama procedura co dane eksperymentu (generuj_rankingi),
# w mniejszej skali: 20 zapytan, 20 dokumentow, trzy systemy o sigma 0,5, 1
# i 3. Pelne stopnie trafnosci sa prawda konstrukcyjna, wiec dla kazdego
# rankingu znany jest dokladny profil zatrzymania.

dane <- generuj_rankingi(liczba_zapytan = 20, liczba_dokumentow = 20,
                         ziarno = 202713)

oceny_przyklad <- dane$oceny
rankingi_przyklad <- dane$rankingi

stopifnot(
  nrow(oceny_przyklad) == 400,
  nrow(rankingi_przyklad) == 1200,
  all(oceny_przyklad$stopien %in% 0:4),
  !anyDuplicated(rankingi_przyklad[c("system", "zapytanie", "pozycja")])
)

dir.create("data", showWarnings = FALSE)
save(oceny_przyklad, file = "data/oceny_przyklad.rda",
     compress = "xz", version = 3)
save(rankingi_przyklad, file = "data/rankingi_przyklad.rda",
     compress = "xz", version = 3)

# TODO: osobny skrypt pobrania i przeksztalcenia zbioru Reuters-21578
#   (podzial ModApte, TOPICS="YES"), z zapisem identyfikatorow dokumentow,
#   wyboru kategorii i licznosci; pobranie poza funkcjami pakietu i testami.
