# Wstęp do Ćwiczenia 2

# Ćwiczenie 2 - materiały wstępne
# W SEKCJI 2 znajduje się wykonanećwiczenie 2

##### SEKCJA 1 #####

# Przyjrzymy się danym mydata z pakietu openair(Carslaw and Ropkins 2012).
# Na podstawie tego zbioru danych spróbujemy zbudować model klasyfikacji.
# Będzie on przewidywał, czy stężenia ozonu było wysokie, czy było niskie.
# Zanim zdefiniujemy co oznacza “wysokie” i “niskie” przyjrzymy się zestawowi
# naszych danych.

# Potrzebujemy pakietu openair jest w nim dostępny zestaw danych mydata
# oraz kilka przydatnych funkcji analizy danych o jakości powietrza.

# Pakiety
library(tidymodels)
library(skimr)
library(GGally)
library(openair)
library(ggpubr)
library(lubridate)


# Ustawienie preferencji dla tidymodels
tidymodels_prefer()

# Wczytanie i wybór danych oraz podsumowanie
air <- mydata |> selectByDate(year = 2002)
air |> skim()

# Usunięcie brakujących danych
air <- air |> na.omit()

# Analiza korelacji między nox i no2
set.seed(222)
air[sample(1:nrow(air), size = 300, replace = FALSE), ] |>
  select(nox, no2) |>
  ggpairs()

# Wykres regresji liniowej dla nox i no2
set.seed(222)
air[sample(1:nrow(air), size = 300, replace = FALSE), ] |>
  select(nox, no2) |>
  ggplot(aes(nox, no2)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  stat_cor(label.x = 10, label.y = 80) +
  stat_regline_equation(label.x = 10, label.y = 82) +
  theme_bw()

# Wykres czasowy stężeń ozonu
air |>
  ggplot(aes(date, o3)) +
  geom_line() +
  theme_bw()

# Znajdz minimalną i maksymalną wartość ozonu
air |>
  pull(o3) |>
  range()

# Przekształcenie zmiennej ilościowej o3 na jakościową (Niskie/Wysokie)
air <- air |>
  mutate(ozone = cut(
    o3,
    breaks = c(-0.1, 10, 53),
    labels = c("Niskie", "Wysokie")
  ))

# Sprawdzenie liczby obserwacji dla każdej z kategorii
air |> count(ozone)


##### SEKCJA 2 #####

# Ćwiczenie 2:

# Teraz zbuduj i przetestuj model regresji logistycznej. Następnie oceń
# jakość. Zastanów się, które zmienne uwzględnić w modelu, a które nie.
# Podczas dzielenia zestawu danych zastosuj równomierny podział danych
# (argument strata = ozone).

# Czy zmienne date, wd, pm10, pm25, so2, co wnoszą coś do modelu ?
# Zastanów się jakie role przypisać no2 i nox, ponieważ te dwa predyktory są z sobą mocno skorelowane.
# Czy stosować przekształcenia boxCox lub YeoJohnson - dla jakich zmiennych?
# Czy normalizacja zmiennych numerycznych jest potrzebna ?
# Czy wyizolować z date podgrupy, które będą ważnymi predatorami.
# Zastosój: set.seed(222) do podziału danych na zbiory uczące i testowe.
