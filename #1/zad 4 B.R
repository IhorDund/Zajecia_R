# Funkcja kalkulatora z obsługą błędów
kalkulator = function(a, b, operacja) {
  if (operacja == "+") {
    return(a + b)
  } else if (operacja == "-") {
    return(a - b)
  } else if (operacja == "*") {
    return(a * b)
  } else if (operacja == "/") {
    # Obsługa dzielenia przez zero
    if (b == 0) {
      return("Błąd: dzielenie przez zero!")
    } else {
      return(a / b)
    }
  } else {
    return("Nieznana operacja")
  }
}

# Testy:
print(kalkulator(20, 2, "+"))   # Wynik: 22
print(kalkulator(20, 2, "/"))   # Wynik: 10
print(kalkulator(15, 0, "/"))   # Wynik: Błąd...
print(kalkulator(10, 5, "abc")) # Wynik: Nieznana operacja