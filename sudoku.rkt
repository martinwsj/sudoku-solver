;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;
;; ******************************************
;;   Martin Jeung (20554113)
;;   CS135 Fall 2014
;;   Assignment 10, Problem 3 (sudoku)
;; ******************************************
;;

;; puzzle boards
(define veryeasy
  '((? 4 5 8 9 3 7 1 6)
    (8 1 3 5 7 6 9 2 4)
    (7 6 9 2 1 4 5 3 8)
    (5 3 6 9 8 7 1 4 2)
    (4 9 2 1 6 5 8 7 3)
    (1 7 8 4 3 2 6 5 9)
    (6 8 4 7 2 1 3 9 5)
    (3 2 1 6 5 9 4 8 7)
    (9 5 7 3 4 8 2 6 1)))

(define easy
  '((? 4 5 8 ? 3 7 1 ?)
    (8 1 ? ? ? ? ? 2 4)
    (7 ? 9 ? ? ? 5 ? 8)
    (? ? ? 9 ? 7 ? ? ?)
    (? ? ? ? 6 ? ? ? ?)
    (? ? ? 4 ? 2 ? ? ?)
    (6 ? 4 ? ? ? 3 ? 5)
    (3 2 ? ? ? ? ? 8 7)
    (? 5 7 3 ? 8 2 6 ?)))

(define wikipedia '((5 3 ? ? 7 ? ? ? ?)
                    (6 ? ? 1 9 5 ? ? ?)
                    (? 9 8 ? ? ? ? 6 ?)
                    (8 ? ? ? 6 ? ? ? 3)
                    (4 ? ? 8 ? 3 ? ? 1)
                    (7 ? ? ? 2 ? ? ? 6)
                    (? 6 ? ? ? ? 2 8 ?)
                    (? ? ? 4 1 9 ? ? 5)
                    (? ? ? ? 8 ? ? 7 9)))

(define blank '((? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)))

(define martin '((? 8 ? ? 1 ? ? 2 ?)
                 (6 ? ? 3 ? 5 ? ? 1)
                 (? ? 7 ? ? ? 4 ? ?)
                 (? 2 ? 1 ? 9 ? 5 ?)
                 (7 ? ? ? ? ? ? ? 6)
                 (? 9 ? 6 ? 3 ? 4 ?)
                 (? ? 5 ? ? ? 3 ? ?)
                 (9 ? ? 2 ? 1 ? ? 8)
                 (? 3 ? ? 6 ? ? 7 ?)))

(define wrong
  '((? 4 5 8 9 2 7 1 6)
    (8 1 3 5 7 6 9 2 4)
    (7 6 9 2 1 4 5 3 8)
    (5 3 6 9 8 7 1 4 2)
    (4 9 2 1 6 5 8 7 3)
    (1 7 8 4 3 2 6 5 9)
    (6 8 4 7 2 1 3 9 5)
    (3 2 1 6 5 9 4 8 7)
    (9 5 7 3 4 8 2 6 1)))


;; Examples:
(check-expect (sudoku veryeasy)  '((2 4 5 8 9 3 7 1 6)
                                   (8 1 3 5 7 6 9 2 4)
                                   (7 6 9 2 1 4 5 3 8)
                                   (5 3 6 9 8 7 1 4 2)
                                   (4 9 2 1 6 5 8 7 3)
                                   (1 7 8 4 3 2 6 5 9)
                                   (6 8 4 7 2 1 3 9 5)
                                   (3 2 1 6 5 9 4 8 7)
                                   (9 5 7 3 4 8 2 6 1)))

(check-expect (sudoku easy) '((2 4 5 8 9 3 7 1 6) 
                              (8 1 3 5 7 6 9 2 4) 
                              (7 6 9 2 1 4 5 3 8) 
                              (5 3 6 9 8 7 1 4 2) 
                              (4 9 2 1 6 5 8 7 3) 
                              (1 7 8 4 3 2 6 5 9)
                              (6 8 4 7 2 1 3 9 5) 
                              (3 2 1 6 5 9 4 8 7) 
                              (9 5 7 3 4 8 2 6 1)))

(define (sudoku board)
  (local [
          ;; (missing row n) determines the missing numbers in the row
          ;; missing: (listof SudokuDigit) Nat -> (list of Nat)
          (define (missing row n)
            (cond [(= n 10) empty] 
                  [(member? n row) (missing row (+ n 1))]
                  [else (cons n (missing row (+ n 1)))]))
          
          ;; (createcolumn col x n) creates the column of a soduko board
          ;; createcolumn: (listof (listof SudokuDigit)) ((listof Nat) -> Nat)
          ;;                Nat -> (listof Nat)
          (define (createcolumn col x n)
            (cond [(= n 10) empty]
                  [else (cons (x (first col)) 
                              (createcolumn (rest col) x (+ n 1)))]))
          
          ;; (createsquare x fst scn thd) crates the square of a sudoku board
          ;; createsquare: (listof (listof SudokuDigit)) ((listof Nat) -> Nat) 
          ;;               ((listof Nat) -> Nat) ((listof Nat) -> Nat) Nat
          ;;                -> (list of Nat)
          (define (createsquare x fst scn thd n)
            (cond [(= n 4) empty]
                  [else (cons (fst (first x)) 
                              (cons (scn (first x)) 
                                    (cons (thd (first x)) 
                                          (createsquare (rest x) fst 
                                                        scn thd (+ n 1)))))]))
          
          ;; (occurences lon num) determines the amount of times num occurs 
          ;;   in lon
          ;; occurences: (listof Nat) Nat -> (listof Nat)
          (define (occurrences lon num)
            (foldr (lambda (x y) (cond [(= num x) (+ 1 y)] 
                                       [else y]))
                   0 (filter number? lon)))
          
          ;; (check lon n) makes sure that there is only 1 n in lon
          ;; check: (listof Nat) Nat -> Bool
          (define (check lon n)
            (cond [(= n 10) true]
                  [(>= 1 (occurrences lon n)) (check lon (+ n 1))]
                  [else false]))
          
          ;; (insert n x lon) put in the first n into the missing spots in lon
          ;;   and x is an accumulator
          ;; insert: (listof Nat) (listof Nat) (listof SudokuDigit) 
          ;;          -> (listof Nat)
          (define (insert n x lon)
            (cond [(empty? n) empty]
                  [(number? (first lon)) 
                   (insert n (append x (cons (first lon) empty)) (rest lon))]
                  [else (append  (insert (rest n) x lon) 
                                 (cons (remove empty 
                                               (append x (cons (first n) 
                                                               (rest lon))))
                                       empty))]))
          
          ;; (ninth n) takes the ninth element in n
          ;; ninth: (listof SudokuDigit) -> Nat
          (define (ninth n)
            (eighth (rest n)))

          ;; (after3 n) takes the 4th and beyond elements of n
          ;; after3: (listof SudokuDigit) -> (listof SudokuDigit)
          (define (after3 n)
            (rest (rest (rest n))))
          
          ;; (checkboard puzzle n) determines if the puzzle is a proper 
          ;;    solution with n as a counter
          ;; checkboard: Puzzle Nat -> Bool
          (define (checkboard puzzle n)
            (cond [(and (= n 1) (check (createcolumn puzzle first 1) 1) 
                        (check (createsquare puzzle first second third 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 2) (check (createcolumn puzzle second 1) 1) 
                        (check (createsquare puzzle fourth fifth sixth 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 3) (check (createcolumn puzzle third 1) 1) 
                        (check (createsquare puzzle seventh eighth ninth 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 4) (check (createcolumn puzzle fourth 1) 1) 
                        (check (createsquare (after3 puzzle) first 
                                             second third 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 5) (check (createcolumn puzzle fifth 1) 1) 
                        (check (createsquare (after3 puzzle) fourth 
                                             fifth sixth 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 6) (check (createcolumn puzzle sixth 1) 1) 
                        (check (createsquare (after3 puzzle) seventh 
                                             eighth ninth 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 7) (check (createcolumn puzzle seventh 1) 1) 
                        (check (createsquare (after3 (after3 puzzle)) first
                                             second third 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 8) (check (createcolumn puzzle eighth 1) 1)
                        (check (createsquare (after3 (after3 puzzle)) fourth
                                             fifth sixth 1) 1)) 
                   (checkboard puzzle (+ n 1))]
                  [(and (= n 9) (check (createcolumn puzzle ninth 1) 1)
                        (check (createsquare (after3 (after3 puzzle)) seventh
                                             eighth ninth 1) 1)) true]
                  [else false]))
          
          ;; (createrow puzzle n) creates a possible answer for the puzzle 
          ;;   and n is a counter
          ;; createrow: (listof SudokuDigit) Nat -> (listof SudokuDigit)
          (define (createrow puzzle n)
            (insert (missing (n puzzle) 1) empty (n puzzle)))
          
          ;; (createsudoku puzzle row x n) creates a possible solution
          ;;   for the puzzle in each row with x and n as counters
          ;; createsudoku: Puzzle (listof SudokuDigit) Nat Nat -> Puzzle
          (define (createsudoku puzzle row x n)
            (cond [(empty? row) empty]
                  [(= n 1) 
                   (append (cons (remove empty 
                                         (append x (cons (first row) 
                                                         (rest puzzle)))) empty)
                           (createsudoku puzzle (rest row) x n))]
                  [else (createsudoku (rest puzzle) row 
                                      (append x (cons (first puzzle) empty)) 
                                      (- n 1))]))
          
          ;; (checkall lob) determines if the row is correct
          ;; checkall: (listof SudokuDigit) -> (list of SudokuDigit)
          (define (checkall lob)
            (cond [(empty? lob) empty]
                  [(checkboard (first lob) 1) 
                   (cons (first lob) (checkall (rest lob)))]
                  [else (checkall (rest lob))]))
          
          ;; (solve board n x) solves the row and n and x is a coutner
          ;; solve: (listof SudokuDigit) Nat Nat -> (listof SudokuDigit)
          (define (solve board n x)
            (checkall (createsudoku board (createrow board n) empty x)))
          
          ;; (solver board) solves the puzzle
          ;; solver: Puzzle -> Puzzle 
          (define (solver board)
            (cond [(not (empty? (missing (first board) 1))) 
                   (solveall (solve board first 1))]
                  [(not (empty? (missing (second board) 1))) 
                   (solveall (solve board second 2))]
                  [(not (empty? (missing (third board) 1))) 
                   (solveall (solve board third 3))]
                  [(not (empty? (missing (fourth board) 1)))
                   (solveall (solve board fourth 4))]
                  [(not (empty? (missing (fifth board) 1))) 
                   (solveall (solve board fifth 5))]
                  [(not (empty? (missing (sixth board) 1)))
                   (solveall (solve board sixth 6))]
                  [(not (empty? (missing (seventh board) 1)))
                   (solveall (solve board seventh 7))]
                  [(not (empty? (missing (eighth board) 1)))
                   (solveall (solve board eighth 8))]
                  [(not (empty? (missing (ninth board) 1))) 
                   (solveall (solve board ninth 9))]
                  [else board]))
          
          ;; (solveall board) solves the puzzle
          ;; solver: Puzzle -> Board 
          (define (solveall board)
            (cond [(empty? board) empty]
                  [else (append (solver (first board))
                                (solveall (rest board)))]))]
    
    (cond [(equal? board blank) (solver veryeasy)]
          [(empty? (solver board)) false]
          [else (solver board)])))

;; Tests: 
(check-expect (sudoku martin) '((3 8 9 7 1 4 6 2 5)
                                (6 4 2 3 9 5 7 8 1) 
                                (5 1 7 8 2 6 4 3 9)
                                (4 2 6 1 7 9 8 5 3) 
                                (7 5 3 4 8 2 1 9 6) 
                                (1 9 8 6 5 3 2 4 7) 
                                (8 6 5 9 4 7 3 1 2)
                                (9 7 4 2 3 1 5 6 8)
                                (2 3 1 5 6 8 9 7 4)))

(check-expect (sudoku wikipedia) '((5 3 4 6 7 8 9 1 2) 
                                   (6 7 2 1 9 5 3 4 8) 
                                   (1 9 8 3 4 2 5 6 7) 
                                   (8 5 9 7 6 1 4 2 3)
                                   (4 2 6 8 5 3 7 9 1)
                                   (7 1 3 9 2 4 8 5 6) 
                                   (9 6 1 5 3 7 2 8 4) 
                                   (2 8 7 4 1 9 6 3 5) 
                                   (3 4 5 2 8 6 1 7 9)))

(check-expect (sudoku blank) '((2 4 5 8 9 3 7 1 6) 
                              (8 1 3 5 7 6 9 2 4) 
                              (7 6 9 2 1 4 5 3 8) 
                              (5 3 6 9 8 7 1 4 2) 
                              (4 9 2 1 6 5 8 7 3) 
                              (1 7 8 4 3 2 6 5 9)
                              (6 8 4 7 2 1 3 9 5) 
                              (3 2 1 6 5 9 4 8 7) 
                              (9 5 7 3 4 8 2 6 1)))

(check-expect (sudoku wrong) false)





