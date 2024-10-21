# Wstęp do Ćwiczenia 2

# Ćwiczenie 2 - materiały wstępne
# W SEKCJI 2 znajduje się wykonane ćwiczenie 2

##### SEKCJA 1 #####

# Przyjrzymy się danym mydata z pakietu openair (Carslaw and Ropkins 2012).
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
library(rsample)

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

# Podzał danych na zbiór treningowy i testowy
set.seed(222)
data_split <- initial_split(air, prop = 0.75, strata = ozone)
train_data <- training(data_split)
test_data <- testing(data_split)

# Tworzenie zbiorów do kroswalidacji (5-krotna kroswalidacja)
set.seed(222)
cv_folds <- vfold_cv(train_data, v = 5, strata = ozone)

# Definicja modelu regresji logistycznej
logistic_model <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

# Tworzenie recepty do przekształceń danych
recipe_ozone <- recipe(ozone ~ nox + no2 + ws + wd + pm10 + pm25 + so2 + co + date,
  data = train_data
) |>
  step_date(date, features = c("month", "dow")) |>
  step_mutate(hour = lubridate::hour(date)) |>
  step_rm(date) |>
  step_normalize(all_numeric_predictors()) |>
  step_corr(all_numeric_predictors(), threshold = 0.9) |> # Usunięcie skorelowanych zmiennych
  step_YeoJohnson(all_numeric_predictors()) # Przekształcenie Yeo-Johnson dla zmiennych numerycznych

# Workflow łączący receptę z modelem
workflow_ozone <- workflow() |>
  add_recipe(recipe_ozone) |>
  add_model(logistic_model)

# Trening modelu z kroswalidacją
set.seed(222)
resampling_results <- workflow_ozone |>
  fit_resamples(
    resamples = cv_folds,
    metrics = metric_set(accuracy, roc_auc),
    control = control_resamples(save_pred = TRUE)
  )

# Podsumowanie wyników kroswalidacji
collect_metrics(resampling_results)

# Trening modelu na pełnych danych treningowych
final_model <- workflow_ozone |>
  last_fit(split = data_split)

# Ocena modelu na danych testowych
final_metrics <- final_model |>
  collect_metrics()

# Wyświetlenie wyników
final_metrics


# Czy zmienne date, wd, pm10, pm25, so2, co wnoszą coś do modelu?
# Zastanów się, jakie role przypisać no2 i nox, ponieważ te dwa predyktory są z sobą mocno skorelowane.
# Czy stosować przekształcenia boxCox lub YeoJohnson - dla jakich zmiennych?
# Czy normalizacja zmiennych numerycznych jest potrzebna?
# Czy wyizolować z date podgrupy, które będą ważnymi predatorami.
# Zastosuj: set.seed(222) do podziału danych na zbiory uczące i testowe.
