# Funkcja obliczająca wartość przyszłą inwestycji (procent składany)
wartosc_przyszla = function(kapital, stopa, lata) {
  # Wzór: FV = PV * (1 + r)^n
  fv = kapital * (1 + stopa)^lata
  return(fv)
}

# Testowanie funkcji: 5000 zł na 5% (0.05) na 1 rok
wynik = wartosc_przyszla(5000, 0.05, 1)
print(paste("Wartość przyszła inwestycji:", wynik, "zł"))