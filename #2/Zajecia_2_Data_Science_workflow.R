#getwd()
#install.packages('dplyr')
#library(dplyr)

# 2.1 Import danych ----
kraje_1 = read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")
# Funkcja install.packages() s³u¿y do jednorazowej instalacji wybranego pakietu, np.:
#install.packages("readxl") 	# uruchom linijkê, a potem j¹ zakomentuj
# Funkcja library()wczytuje wybrany pakiet, aby móc z niego korzystaæ, np.:
#library(readxl)

# 2.2 Przygotowanie danych ----

# 2.2.1 Podgl¹d danych ----

# Pierwsze/ostatnie wiersze
head(kraje_1)	# pierwsze 6 wierszy (obserwacji)
head(kraje_2)      

head(kraje_1, 10)	# pierwsze 10 wierszy (obserwacji)
head(kraje_2, 10)

tail(kraje_1, 5)	# ostatnie 5 wierszy (obserwacji)
tail(kraje_2, 5)


# Podstawowe statystyki wszystkich kolumn (zmiennych)
summary(kraje_1)	# min, max, œrednia, mediana, kwantyle
summary(kraje_2)

# Statystyki pojedynczej kolumny (zmiennej)
mean(kraje_1$Przyrost_populacji)		# œrednia
median(kraje_1$Przyrost_populacji)	# mediana
min(kraje_1$Przyrost_populacji)		# minimum
max(kraje_1$Przyrost_populacji)		# maksimum


# 2.2.2 Porz¹dkowanie nazw kolumn (zmiennych) ----

# Usuwanie zbêdnej kolumny
kraje_1$X = NULL
kraje_2$X = NULL

# Zmiana nazw kolumn z angielskich na polskie
colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")


# 2.2.3 Porz¹dkowanie typów danych ----

# W ramce danych kraje_2 sprawdŸ typ zmiennej Region 
is.numeric(kraje_2$Region) 	# czy zmienna jest liczbowa? Odp. Nie.
is.character(kraje_2$Region) 	# czy zmienna jest tekstowa? Odp. Tak.

# Region to zmienna kategorialna, wiêc nadajemy jej typ factor:
kraje_2$Region = as.factor(kraje_2$Region)

# Sprawdzenie kategorii:
summary(kraje_2)
levels(kraje_2$Region)

# Teraz widaæ, ¿e jest 7 kategorii regionów, na których operuje zmienna Region.


# 2.2.4 Porz¹dkowanie braków danych ----

# Szybka kontrola braków danych we wszystkich kolumnach:
colSums(is.na(kraje_1))	# nie ma braków danych
colSums(is.na(kraje_2))	# s¹ 4 braki danych w kolumnie (zmiennej) Internet_proc.

# Liczba braków w konkretnej kolumnie:
sum(is.na(kraje_2$Internet_proc.)) 	# 4 braki


# Zobaczmy te 4 wiersze, w których brakuje wartoœci:
kraje_2[is.na(kraje_2$Internet_proc.), ]


# Braki danych s¹ czêœci¹ rzeczywistoœci ekonomisty, dlatego trzeba umieæ je obs³u¿yæ i # podj¹æ decyzjê analityczn¹:
# OPCJA 1 - Pozostawiæ (teraz tak post¹pimy)
# OPCJA 2 - Usun¹æ obserwacje z brakami (czy usuniêcie tych obserwacji zmieni analizê?)
# OPCJA 3 - Uzupe³niæ braki (np. imputacja median¹)


# 2.2.5 Czyszczenie danych ----
# W ramce danych kraje_2, w kolumnie Region s¹ kategorie, w których nazwie jest znak &:
levels(kraje_2$Region)
# [1] "East Asia & Pacific"       "Europe & Central Asia"    
# [3] "Latin America & Caribbean" "Middle East & North Africa"
# [5] "North America"             "South Asia"               
# [7] "Sub-Saharan Africa"

# Znak & bywa problematyczny przy dalszym przetwarzaniu, dlatego zast¹p go s³ownym spójnikiem "and".
# Funkcja gsub() dzia³a jak "ZnajdŸ i zamieñ" (Ctrl+H) w Excelu. 
# Zamienia wszystkie wyst¹pienia tekstu na inny tekst
# Przyk³adowo: gsub("stary_tekst", "nowy_tekst", ramka$kolumna)

# W naszym przypadku wykonamy nastêpuj¹cy kod:
kraje_2$Region <- gsub("&", "and", kraje_2$Region)

# Sprawdzenie (po zamianie ponownie ustawiamy typ factor):
kraje_2$Region = as.factor(kraje_2$Region)
levels(kraje_2$Region)

# 2.3. £¹czenie (scalanie) ramek danych w jedn¹ ----
# Funkcja merge() ³¹czy dwie ramki danych/tabele po wspólnej kolumnie (kluczu) - dzia³a analogicznie jak 
# WYSZUKAJ.PIONOWO w Excelu

# Przyk³adowo: merge(ramka1, ramka2, by.x="kolumna1", by.y="kolumna2")

#   £¹czenie (scalanie) ramek danych kraje_1 i kraje_2
kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")


# Usuwanie zbêdnej kolumny po po³¹czeniu
kraje$Nazwa = NULL


# Zobacz ramkê danych po scaleniu
summary(kraje)
str(kraje)

# 2.4. Podstawowa analiza danych----
#Po zaimportowaniu danych i ich wstêpnym przygotowaniu przechodzimy do niezbêdnego etapu ka¿dej analizy ekonomicznej. Podstawowa analiza danych jest punktem wyjœcia do dalszych, bardziej zaawansowanych metod, takich jak modelowanie czy prognozowanie.

#Na tym etapie nie budujemy jeszcze modeli, tylko skupiamy siê na poznaniu zbioru danych i jego struktury, wykonuj¹c:
  #•	Filtrowanie i sortowanie danych (wybieranie interesuj¹cych obserwacji, np. krajów o wysokim PKB lub transakcji powy¿ej okreœlonej kwoty)
#•	Podsumowania statystyczne (uzyskanie ogólnego obrazu danych za pomoc¹ miar takich jak œrednia, mediana, minimum, maksimum czy odchylenie standardowe)
#•	Agregacjê i grupowanie (analiza danych wed³ug kategorii, np. obliczenie œrednich wartoœci dla poszczególnych regionów lub sektorów gospodarki)
#•	Wykrywanie brakuj¹cych lub nieprawid³owych wartoœci, które mog¹ wp³ywaæ na wyniki dalszej analizy.

#dplyr to pakiet R, którego zestaw funkcji zaprojektowano tak, aby umo¿liwia³ manipulowanie ramkami danych w intuicyjny i przyjazny dla u¿ytkownika sposób. Jest to jeden z podstawowych pakietów popularnego zestawu pakietów tidyverse w jêzyku programowania R.

# 2.4.1 Najczêœciej u¿ywane funkcje pakietu dplyr: ----
# mutate() - tworzenie nowych zmiennych na bazie istniej¹cych
# filter() – wybieranie wierszy spe³niaj¹cych okreœlone warunki
# select() – wybieranie kolumn
# arrange() - sortowanie
# group_by() - grupowanie
# summarise() – obliczanie wartoœci zagregowanych (np. œrednich, sum)


# mutate() – tworzenie nowych zmiennych na bazie istniej¹cych

library(dplyr)


# Tworzenie nowej zmiennej Populacja_w_mln w dplyr:
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)

# Równowa¿ny kod w base R:
kraje$Populacja_mln = kraje$Populacja / 1e6


# 1e6 to zapis miliona w R (1 razy 10 do potêgi 6)
# 1e9  = 1 000 000 000 (miliard)
# 1e12 = 1 000 000 000 000 (bilion)


# Tworzenie nowej zmiennej PKB_per_capita w dplyr:
kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)

# Równowa¿ny kod w base R:
kraje$PKB_per_capita = kraje$PKB / kraje$Populacja


# filter() – wybieranie wierszy 
# select() – wybieranie kolumn 

# Wyœwietl kraje, w których % poziom urbanizacji jest wiêkszy ni¿ 50
kraje %>%
  filter(Urbanizacja_proc. > 50)

# Równowa¿ny kod w base R:
kraje[kraje$Urbanizacja_proc. > 50, ]


# Wyœwietl tylko dane pokazuj¹ce zmienne Panstwo, Region, PKB, Populacja_mln
kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln)

# Równowa¿ny kod w base R:
kraje[, c("Panstwo", "Region", "PKB", "Populacja_mln")]

# arrange() – sortowanie 

# Posortuj kraje wed³ug przyrostu populacji rosn¹co
kraje %>%
  arrange(Przyrost_populacji)


# Posortuj kraje wed³ug przyrostu populacji malej¹co
kraje %>%
  arrange(desc(Przyrost_populacji))

# Równowa¿ny kod w base R:
kraje[order(kraje$Przyrost_populacji), ]  # rosn¹co
kraje[order(kraje$Przyrost_populacji, decreasing = TRUE), ]  # malej¹co


# Wybierz kraje z PKB wiêkszym ni¿ 1 bilion, posortuj je rosn¹co wzglêdem PKB 
# i wyœwietl nazwê pañstwa, PKB i PKB per capita. Ile jest takich krajów?
kraje %>%
  filter(PKB > 1e12) %>%
  arrange(PKB) %>%
  select(Panstwo, PKB, PKB_per_capita)


# Równowa¿ny kod w base R:

# Krok 1: Filtrowanie
kraje_filtr = kraje[kraje$PKB > 1e12, ]

# Krok 2: Sortowanie
kraje_sort = kraje_filtr[order(kraje_filtr$PKB), ]

# Krok 3: Wybór kolumn
kraje_sort[, c("Panstwo", "PKB", "PKB_per_capita")]

# Wniosek: dplyr jest bardziej czytelny przy wielu operacjach.



# Wybierz kraje z regionu Afryki Subsaharyjskiej, 
# wybierz zmienne Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja,
# a nastêpnie posortuj malej¹co po PKB per capita
kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita))


# Równowa¿ny kod w base R:
# Krok 1: Filtrowanie i wybór kolumn
kraje_reg = kraje[kraje$Region == "Sub-Saharan Africa", c("Panstwo", "PKB_per_capita", "Populacja_mln", "Urbanizacja_proc.")]

# Krok 2: Sortowanie
kraje_reg[order(kraje_reg$PKB_per_capita, decreasing = TRUE), ]


# group_by() – grupowanie 
# summarise() - obliczanie wartoœci zagregowanych (np. œrednich, sum) 

# Wyœwietl tylko te kraje, które s¹ bogatsze ni¿ œrednia regionu
bogate = kraje %>%
  group_by(Region) %>%
  filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))



# Równowa¿ny kod w base R:

bogate = kraje[kraje$PKB_per_capita > ave(kraje$PKB_per_capita, kraje$Region, 
                                          FUN = mean, na.rm = TRUE), ]

# ave() liczy œredni¹ wewn¹trz grup i zwraca wektor tej samej d³ugoœci co dane.



# ZnajdŸ najwiêksz¹ wartoœæ PKB per capita w ca³ym zbiorze krajów
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))


# Równowa¿ny kod w base R:

max(kraje$PKB_per_capita, na.rm = TRUE)



# ZnajdŸ najwiêksz¹ i najmniejsz¹ wartoœæ Populacji w mln w ca³ym zbiorze krajów
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))


# Równowa¿ny kod w base R:
min(kraje$Populacja_mln, na.rm = TRUE)
max(kraje$Populacja_mln, na.rm = TRUE)


# Oblicz œredni¹ populacjê w ca³ym zbiorze krajów (jedna liczba dla ca³ej ramki)
kraje %>%
  summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))

# Równowa¿ny kod w base R:
mean(kraje$Populacja_mln, na.rm = TRUE)

# Ile krajów jest w ca³ym zbiorze danych?
kraje %>%
  summarise(liczba_krajow = n())


# Równowa¿ny kod w base R:

nrow(kraje)



# Policz, ile krajów jest w ka¿dym regionie
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())


# Równowa¿ny kod w base R:

table(kraje$Region)



# Dla ka¿dego regionu œwiata: oblicz liczbê krajów (n), œredni % dostêp do internetu i œredni % poziom urbanizacji, a nastêpnie posortuj regiony malej¹co wg œredniego % dostêpu do internetu
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))


# Równowa¿ny kod w base R:
{
  wynik = aggregate(cbind(Internet_proc., Urbanizacja_proc.) ~ Region,
                    kraje, mean, na.rm = TRUE)
  wynik$liczba_krajow = as.vector(table(kraje$Region)[wynik$Region])
  colnames(wynik) = c("Region", "sredni_internet", "srednia_urbanizacja", "liczba_krajow")
  wynik[order(-wynik$sredni_internet), ]
  }

# 2.5. Wizualizacja [* zaawansowane *]----
# Wizualizacja danych tak¿e pozwala zidentyfikowaæ wzorce i zale¿noœci w zbiorze danych.

# install.packages("ggplot2")
library(ggplot2)



# 2.5.1 Prosty wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita)) +
  geom_point() +
  labs(
    title = "Urbanizacja a PKB per capita",
    x = "Urbanizacja (%)",
    y = "PKB per capita")



# 2.5.2 Zaawansowany wykres punktowy: urbanizacja a PKB per capita ----
ggplot(kraje, aes(x = Urbanizacja_proc., y = PKB_per_capita, color = Region)) +
  geom_point(size = 3, alpha = 0.7) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Urbanizacja a PKB per capita",
    subtitle = "Czy bardziej zurbanizowane kraje s¹ bogatsze?",
    x = "Urbanizacja (% ludnoœci miejskiej)",
    y = "PKB per capita (USD, skala log)",
    color = "Region œwiata"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom")



# 2.5.3 Zaawansowany wykres punktowy: rozmiar gospodarki a populacja ----

ggplot(kraje, aes(x = Populacja_mln, y = PKB, size = PKB_per_capita, color = Region)) +
  geom_point(alpha = 0.7) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Skala gospodarki i demografia",
    x = "Populacja (mln, log10)",
    y = "PKB (USD, log10)",
    size = "PKB per capita"
  ) +
  theme_minimal()



# 2.5.4 Prosty wykres s³upkowy: liczba krajów w regionach ----
ggplot(kraje, aes(x = Region)) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(
    title = "Liczba krajów w regionach œwiata",
    x = "Region",
    y = "Liczba krajów"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5))


# 2.5.5 Zaawansowany wykres s³upkowy poziomy: TOP 15 najbogatszych krajów ----
kraje %>%
  arrange(desc(PKB_per_capita)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Panstwo, PKB_per_capita), y = PKB_per_capita, fill = Region)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "TOP 15 najbogatszych krajów œwiata (2016)",
    subtitle = "PKB per capita w USD",
    x = NULL,
    y = "PKB per capita (USD)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10))


# 2.5.6 Wykres pude³kowy (boxplot): dostêp do internetu wed³ug regionów ----
ggplot(kraje, aes(x = reorder(Region, Internet_proc., FUN = median), 
                  y = Internet_proc., fill = Region)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) +
  coord_flip() +
  labs(
    title = "Dostêp do internetu wed³ug regionów œwiata",
    subtitle = "(punkty to poszczególne kraje)",
    x = NULL,
    y = "Dostêp do internetu (% populacji)",
    fill = "Region"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "none")
# 2.5.7 Wykres pude³kowy (boxplot): przyrost populacji wed³ug regionów ----
# (mediana, rozrzut i obserwacje odstaj¹ce)
ggplot(kraje, aes(x = Region, y = Przyrost_populacji)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.5) +
  coord_flip() +
  labs(
    title = "Tempo przyrostu populacji w regionach œwiata",
    subtitle = "(punkty to poszczególne kraje, linia przerywana = 0%)",
    x = "Region",
    y = "Przyrost populacji (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14))

# 2.6 Eksport----
# Zapisanie ramki danych do pliku CSV
write.csv(kraje, "kraje_analiza.csv") 



# Zapisanie ramki danych do pliku Excel wymaga pakietu writexl:
#install.packages("writexl")
library(writexl)

write_xlsx(kraje, "kraje_wynik.xlsx")


