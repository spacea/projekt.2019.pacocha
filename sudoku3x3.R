#sudokuu <- function(x,y){#w parametrach funkcji można dać rozmiar pożądanego sudoku oraz poziom trudności, 


new_sudoku <- c(sample(1:9)) #tworzymy nowy  wektor losowego pelnego sudoku

sudoku <- matrix(new_sudoku,3,3, byrow = TRUE) #robimy macierz

sudoku [sample(1:9)[1:3]] <- NA #zamieniamy 3 losowe liczby na NA

print(sudoku)

pozycja_x <- as.integer(readline(prompt="Proszę wpisać numer wiersza do zmiany : "))
pozycja_y <- as.integer(readline(prompt="Proszę wpisać numer kolumny do zmiany : "))
pozycja_zmiana <- as.integer(readline(prompt="Proszę wpisać porządaną liczbę : "))  

sudoku[pozycja_x,pozycja_y] <- pozycja_zmiana  #wprowadzamy zmiany do głównego sudoku

