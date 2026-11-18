# KaskadaR

[![R-CMD-check](https://github.com/eryktomaszek/KaskadaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/eryktomaszek/KaskadaR/actions/workflows/R-CMD-check.yaml)

Pakiet języka R do oceny rankingu wyników wyszukiwania miarą Expected Reciprocal
Rank (ERR) w modelu kaskadowym (Chapelle i in. 2009). Model zakłada, że użytkownik
przegląda wyniki po kolei i kończy na pierwszym, który go zadowoli. Pakiet ma
pokazywać nie tylko wartość miary, ale też cały profil zatrzymania.

Projekt dyplomowy na kierunku elektroniczne przetwarzanie informacji, Instytut
Studiów Informacyjnych UJ. Promotor: dr Marek Deja.

## Stan

**W budowie.** Repozytorium zawiera na razie szkielet pakietu z seminarium
(`SzablonR`) pod docelową nazwą. Kod szkieletu będzie zastępowany moduł po module
implementacją metody.

## Instalacja

```r
# install.packages("remotes")
remotes::install_github("eryktomaszek/KaskadaR")
```

## Licencja

GPL-3.
