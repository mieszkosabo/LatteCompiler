# Kompilator Latter (wersja bazowa)

## Mieszko Sabo, 406322

Kompilacja kompilatora poprzez wywołanie `make`.
W katalogu tests znajduje się `tester.py`, na students można uruchomić go poprzez `python3 tester.py`.

## Zaimplementowane podpunkty

- back-end LLVM
- użycie rejestrów i phi zamiast alloc
- optymalizacja LCSE
- optymalizacja GCSE (nowy)
- tablice (nowy)
- klasy z dziedziczeniem i metodami wirtualnymi

## połączenie się z komputerem w labolatorium

1. najpierw ssh na [login]@students.mimuw.edu.pl
2. odpalić skrypt `lk_booted_pcs` i wybrać któryś z dostępnych komputerów np. red00
3. ssh na [login]@red00.mimuw.edu.pl
