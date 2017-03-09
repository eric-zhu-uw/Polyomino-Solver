;;
;;**********************************************************************
;;  Eric Zhu (20661534)
;;  CS 135 Fall 2016
;;  Assignment 10, Problem 5 a,b,c,d,e,f,g
;;**********************************************************************

(require "a10.rkt")

;; Uncomment the following line if you want to use
;; the examples in kanoodle.rkt
;; (require "kanoodle.rkt")

;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty

(define-struct pos (x y))
;; A Pos is a (make-pos Int Int)

(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

;; A temporary neighbours function that always fails.  
;; Provide only the purpose, contract and function definition.
;;(define (neighbours s)
;;  empty)

;; (solve-puzzle grid polys viz-style)
;; Solve a polyomino puzzle, given the initially empty (or partially filled 
;; in) grid, a set of pieces that must be placed, and a Symbol indicating
;; what visualization style to use.  Legal viz styles are 'interactive
;; (draw every step of the search), 'at-end (just draw the solution, if one
;; is found), or 'offline (don't draw anything).  Produce either the solved
;; Grid (converted to a list of Strings, just for convenience) or false if
;; no solution exists.
;;
;; You don't need to modify this function at all.  It is provided for you
;; so that you can test your puzzle solving algorithm interactively.  If
;; you decide you want to write check-expect tests using solve-puzzle
;; (which you don't have to do, but can if you want), be sure to consume
;; 'offline for viz-style.

;; solve-puzzle: Grid (listof Grid) Sym -> (anyof (listof Str) false)
;; requires: viz-style is one of {'interactive, 'at-end or 'offline}

;; Some Examples are included below after the solve-puzzle function definition.

;; DO NOT MODIFY THIS CODE
(define (solve-puzzle grid polys viz-style)
  (local
    [(define result
       (search 
        (lambda (S) (empty? (state-pieces S)))
        neighbours
        (cond
          [(symbol=? viz-style 'interactive)
           (lambda (S) (draw-grid (state-puzzle S)))]
          [else false])
        (make-state grid polys)))
     
     (define maybe-last-draw
       (cond
         [(and (state? result)
               (symbol=? viz-style 'at-end))
          (draw-grid (state-puzzle result))]
         [else false]))]
    (cond
      [(boolean? result) result]
      [else (map list->string (state-puzzle result))])))

;; Examples:
;; (The examples are not provided in check-expect form.  They're meant to
;; demonstrate typical uses of the function, but we don't want to them to
;; open interactive visualizations every time you start the program.)

;; Solve offline (i.e. work like a normal Scheme function).
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'offline)

;; Display the result graphically, if a solution is found.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'at-end)

;; Display every step of the search as it progresses.
;(solve-puzzle
;  (strlist->grid '("...." "...." "...." "...." "...." "...."))
;  (cons '((#\L #\L) (#\. #\L)) (cons '((#\O)) tetrominoes-uc))
;  'interactive)


;; Problem 1.a

;; (build-2dlist col row f) takes the row and col and creates a two-dimensional
;;   equivalent of the built-in Racket function build-list that applies
;;   function f to all elements of the two-dimensional Grid
;; build-2dlist: Nat Nat (Any Any -> X) -> (list (list X))
;; Examples:
(check-expect (build-2dlist 5 6 +)
              '((0 1 2 3 4)(1 2 3 4 5)(2 3 4 5 6)
                           (3 4 5 6 7)(4 5 6 7 8)(5 6 7 8 9)))
(check-expect (build-2dlist 3 2 (lambda (x y) (list x y)))
              (list (list (list 0 0) (list 1 0) (list 2 0))
                    (list (list 0 1) (list 1 1) (list 2 1))))
(check-expect (build-2dlist 5 2 (lambda (x y) (* x y)))
              (list (list 0 0 0 0 0) (list 0 1 2 3 4)))

(define (build-2dlist col row f)
  (build-list row (lambda (y) (build-list col (lambda (x) (f x y))))))

;; Tests:
(check-expect (build-2dlist 0 0 +) empty)
(check-expect (build-2dlist 0 5 +) '(()()()()()))
(check-expect (build-2dlist 5 0 +) empty)
(check-expect (build-2dlist 1 1 +) (list (list 0)))
(check-expect (build-2dlist 2 2 -) (list (list 0 1) (list -1 0)))
(check-expect (build-2dlist 3 3 *) (list (list 0 0 0) (list 0 1 2)
                                         (list 0 2 4)))
(check-expect (build-2dlist 4 4 =) (list (list true false false false)
                                         (list false true false false)
                                         (list false false true false)
                                         (list false false false true)))
(check-expect (build-2dlist 2 4 (lambda (x y) (list x y)))
              '(((0 0) (1 0)) ((0 1) (1 1)) ((0 2) (1 2)) ((0 3) (1 3))))

;;**********************************************************************
;; Problem 1.b

;; (all-positions width height) takes the width and height and produces
;;   a (listof Pos) containing all possible positions in a grid with
;;   that particular width and height
;; all-positions: Nat Nat -> (listof Pos)
;; requires: width and height must be be > 0
;; Examples:
(check-expect (lists-equiv? (all-positions 2 2)
                            (list (make-pos 0 0) (make-pos 1 0)
                                  (make-pos 0 1) (make-pos 1 1))) true)
(check-expect (lists-equiv? (all-positions 1 1) (list (make-pos 0 0))) true)
(check-expect
 (lists-equiv? (all-positions 5 6)
               (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0) (make-pos 3 0)
                     (make-pos 4 0) (make-pos 0 1) (make-pos 1 1) (make-pos 2 1)
                     (make-pos 3 1) (make-pos 4 1) (make-pos 0 2) (make-pos 1 2)
                     (make-pos 2 2) (make-pos 3 2) (make-pos 4 2) (make-pos 0 3)
                     (make-pos 1 3) (make-pos 2 3) (make-pos 3 3) (make-pos 4 3)
                     (make-pos 0 4) (make-pos 1 4) (make-pos 2 4) (make-pos 3 4)
                     (make-pos 4 4) (make-pos 0 5) (make-pos 1 5) (make-pos 2 5)
                     (make-pos 3 5) (make-pos 4 5))) true)

(define (all-positions width height)
  (foldr (lambda (x y) (append x y)) empty
         (build-2dlist width height (lambda (x y) (make-pos x y)))))

;; Tests:
;; (1 1) (1 5) (5 1) (3 3) (one false case)
(check-expect (lists-equiv? (all-positions 2 2)
                            (list (make-pos 0 1) (make-pos 1 1)
                                  (make-pos 0 0) (make-pos 1 0))) true)
(check-expect (lists-equiv? (all-positions 1 4)
                            (list (make-pos 0 0) (make-pos 0 1)
                                  (make-pos 0 2) (make-pos 0 3))) true)
(check-expect (lists-equiv? (all-positions 1 1) (list (make-pos 0 0))) true)
(check-expect (lists-equiv? (all-positions 1 5)
                            (list (make-pos 0 0)(make-pos 0 1) (make-pos 0 2)
                                  (make-pos 0 3) (make-pos 0 4) )) true)
(check-expect (lists-equiv? (all-positions 5 1)
                            (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
                                  (make-pos 3 0) (make-pos 4 0))) true)
(check-expect (lists-equiv? (all-positions 3 3)
                            (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
                                  (make-pos 0 1) (make-pos 1 1) (make-pos 2 1)
                                  (make-pos 0 2) (make-pos 1 2) (make-pos 2 2)))
              true)
(check-expect (lists-equiv? (all-positions 3 3)
                            (list (make-pos 0 0) (make-pos 1 0) (make-pos 2 0)
                                  (make-pos 0 1) (make-pos 1 1) (make-pos 2 1)
                                  (make-pos 0 2) (make-pos 1 2))) false)


;;**********************************************************************
;; Problem 2

;; (all-orientations polyomino) takes a Grid representing a single polyomino
;;   and produces a listof Grids containing all distinct rotations and
;;   reflections of that polyomino; The rotations are found in a helper function
;;   and the main function only adds reflections and removes duplicates
;; all-orientations: Grid -> (listof Grid)
;; Examples:
(check-expect
 (lists-equiv? (all-orientations '((#\a #\a)(#\a #\a)(#\a #\.)))
               (list (list (list #\. #\a #\a) (list #\a #\a #\a))
                     (list (list #\a #\a #\a) (list #\. #\a #\a))
                     (list (list #\a #\a #\.) (list #\a #\a #\a))
                     (list (list #\a #\a #\a) (list #\a #\a #\.))
                     (list (list #\. #\a) (list #\a #\a) (list #\a #\a))
                     (list (list #\a #\a) (list #\a #\a) (list #\. #\a))
                     (list (list #\a #\.) (list #\a #\a) (list #\a #\a))
                     (list (list #\a #\a) (list #\a #\a) (list #\a #\.)))) true)  
(check-expect (lists-equiv? (all-orientations '((#\a)))
                            (list (list (list #\a)))) true)
(check-expect (lists-equiv? (all-orientations '((#\a #\a) (#\a #\.)))
                            (list (list (list #\a #\a) (list #\a #\.))
                                  (list (list #\a #\a) (list #\. #\a))
                                  (list (list #\. #\a) (list #\a #\a))
                                  (list (list #\a #\.) (list #\a #\a)))) true)

(define (all-orientations polyomino)
  (foldr (lambda (x y) (cons x (filter (lambda (z) (not (equal? x z))) y)))
         empty (append (map reverse (all-rotations polyomino))
                       (all-rotations polyomino))))

;; Tests
(check-expect (lists-equiv? (all-orientations '((#\a #\a)(#\a #\a)))
                            '(((#\a #\a)(#\a #\a)))) true)
(check-expect (lists-equiv? (all-orientations '((#\a) (#\a) (#\a)))
                            '(((#\a) (#\a) (#\a))
                              ((#\a #\a #\a)))) true)
(check-expect (lists-equiv? (all-orientations '((#\a #\. #\a)
                                                (#\a #\. #\a)
                                                (#\a #\a #\a)))
                            '(((#\a #\. #\a) (#\a #\. #\a) (#\a #\a #\a))
                              ((#\a #\a #\a) (#\a #\. #\.) (#\a #\a #\a))
                              ((#\a #\a #\a) (#\a #\. #\a) (#\a #\. #\a))
                              ((#\a #\a #\a) (#\. #\. #\a) (#\a #\a #\a))))
              true)
(check-expect (lists-equiv? (all-orientations '((#\a #\. #\.)
                                                (#\a #\a #\a)
                                                (#\. #\a #\.)))
                            '(((#\a #\. #\.) (#\a #\a #\a) (#\. #\a #\.))
                              ((#\. #\. #\a) (#\a #\a #\a) (#\. #\a #\.))
                              ((#\. #\a #\a) (#\a #\a #\.) (#\. #\a #\.))
                              ((#\a #\a #\.) (#\. #\a #\a) (#\. #\a #\.))
                              ((#\. #\a #\.) (#\a #\a #\a) (#\. #\. #\a))
                              ((#\. #\a #\.) (#\a #\a #\a) (#\a #\. #\.))
                              ((#\. #\a #\.) (#\. #\a #\a) (#\a #\a #\.))
                              ((#\. #\a #\.) (#\a #\a #\.) (#\. #\a #\a))))
              true)


;; (all-rotations polyomino) takes a Grid representing a single polyomino
;;   and outputs all 4 rotations of the polyomino in a list and does not care
;;   about repeated instances
;; all-rotations: Grid -> (listof Grid)
;; Examples:
(check-expect (lists-equiv? (all-rotations '((#\a #\. #\.)
                                             (#\a #\a #\a)
                                             (#\. #\a #\.)))
                            '(((#\a #\. #\.) (#\a #\a #\a) (#\. #\a #\.))
                              ((#\. #\a #\a) (#\a #\a #\.) (#\. #\a #\.))
                              ((#\. #\a #\.) (#\a #\a #\a) (#\. #\. #\a))
                              ((#\. #\a #\.) (#\. #\a #\a) (#\a #\a #\.))))
              true)
(check-expect (lists-equiv? (all-rotations '((#\a)))
                            '(((#\a)) ((#\a)) ((#\a)) ((#\a)))) true)
(check-expect (lists-equiv? (all-rotations '((#\a) (#\a)))
                            '(((#\a) (#\a)) ((#\a) (#\a))
                                            ((#\a #\a)) ((#\a #\a)))) true)

(define (all-rotations polyomino)
  (list polyomino
        (rotate-90-degrees polyomino)
        (rotate-90-degrees (rotate-90-degrees polyomino))
        (rotate-90-degrees (rotate-90-degrees (rotate-90-degrees polyomino)))))

;; Tests
;; Does not need additional testing since the Examples already demonstrate the
;;   the 3 unique examples


;; (rotate-90-degrees polyomino) takes a Grid represting a single polyomino
;;   and outputs the same polyomino rotate 90 degrees clockwise
;; rotate-90-degrees: Grid -> Grid
;; Examples:
(check-expect (rotate-90-degrees '((#\a))) '((#\a)))
(check-expect (rotate-90-degrees '((#\a #\a #\a) (#\. #\a #\.) (#\. #\a #\.)))
              '((#\. #\. #\a) (#\a #\a #\a) (#\. #\. #\a)))
(check-expect (rotate-90-degrees '((#\a #\a) (#\a #\.)))
              '((#\a #\a) (#\. #\a)))

(define (rotate-90-degrees polyomino)
  (local [;; (first-col-reverse polyomino) takes the first column of the
          ;;   polyomino and reverses it into the new first row of the
          ;;   rotated polyomino
          ;; first-col-reverse: Grid -> (listof Char)
          (define (first-col-reverse polyomino)
            (foldl (lambda (x y) (cons (first x) y)) empty polyomino))
          
          ;; (rest-col polyomino) removes the first column of the polyomino
          ;;   and returns the remaining columns of the polyomino in a Grid
          ;; rest-col: Grid -> Grid
          (define (rest-col polyomino)
            (foldr (lambda (x y) (cons (rest x) y)) empty polyomino))]
    
    (cond [(= 1 (length (first polyomino)))
           (cons (first-col-reverse polyomino) empty)]
          [else (cons (first-col-reverse polyomino)
                      (rotate-90-degrees (rest-col polyomino)))])))

;; Tests
;; Does not need additional testing since the Examples already demonstrate
;;   enough cases

;;**********************************************************************
;; Problem 3

;; (first-empty-pos grid) takes a grid and determines the first Pos
;;   of the grid which has the character #\. using an accumulator
;; first-empty-pos: Grid -> Pos
;; Examples:
(check-expect (first-empty-pos '((#\a #\a #\a) (#\. #\a #\.) (#\. #\a #\.)))
              (make-pos 0 1))
(check-expect (first-empty-pos '((#\.))) (make-pos 0 0))
(check-expect (first-empty-pos '((#\a #\a #\a) (#\a #\a #\a) (#\a #\a #\a)))
              false)

(define (first-empty-pos grid)
  (local [;; (find-row grid row-count) takes a grid and has an accumulator
          ;;   row-count and once it finds the correct row, it will call the
          ;;   find-col function and output the Pos of the first #\. in the grid
          ;; find-row: Grid Nat -> Pos
          (define (find-row grid row-count)
            (cond [(empty? grid) false]
                  [(ormap (lambda (x) (char=? #\. x)) (first grid))
                   (make-pos (find-col (first grid) 0) row-count)]
                  [else (find-row (rest grid) (add1 row-count))]))
          
          ;; (find-col row col-count) takes a row and has an accumulator
          ;;   col-count and once it finds the first #\. in the row,
          ;;   it will output the column position #\. is in the row
          ;; find-col: (listof Char) Nat -> Nat
          (define (find-col row col-count)
            (cond [(char=? #\. (first row)) col-count]
                  [else (find-col (rest row) (add1 col-count))]))]
    
    (find-row grid 0)))

;; Tests:
(check-expect (first-empty-pos '((#\.))) (make-pos 0 0))
(check-expect (first-empty-pos '((#\a))) false)
(check-expect (first-empty-pos '((#\. #\a #\a) (#\. #\a #\.) (#\. #\a #\.)))
              (make-pos 0 0))
(check-expect (first-empty-pos '((#\a #\a #\.) (#\. #\a #\.) (#\. #\a #\.)))
              (make-pos 2 0))
(check-expect (first-empty-pos '((#\a #\a #\a) (#\. #\a #\.) (#\. #\a #\.)))
              (make-pos 0 1))
(check-expect (first-empty-pos '((#\a #\a #\a) (#\a #\a #\.) (#\. #\a #\.)))
              (make-pos 2 1))
(check-expect (first-empty-pos '((#\a #\a #\a) (#\a #\a #\a) (#\. #\a #\.)))
              (make-pos 0 2))
(check-expect (first-empty-pos '((#\a #\a #\a) (#\a #\a #\a) (#\a #\a #\.)))
              (make-pos 2 2))
(check-expect (first-empty-pos '((#\a #\a #\a) (#\a #\a #\a) (#\a #\a #\a)))
              false)

;;**********************************************************************
;; Problem 4

;; (associated-list pos grid) takes a grid and the pos it should be in regards
;;   to the origin and creates an associated list where the pos are the key's
;;   and the characters are the values
;; associated-list: Pos Grid -> (listof (Pos Grid))
;; Examples:
(check-expect (associated-list (make-pos 0 0)
                               '((#\. #\a #\a)))
              (list (list (make-pos 0 0) #\.) (list (make-pos 1 0) #\a)
                    (list (make-pos 2 0) #\a)))
(check-expect (associated-list (make-pos 1 1) '((#\.) (#\.) (#\a)))
              (list (list (make-pos 1 1) #\.) (list (make-pos 1 2) #\.)
                    (list (make-pos 1 3) #\a)))
(check-expect (associated-list (make-pos 12 1) '((#\. #\.) (#\a #\a)))
              (list (list (make-pos 12 1) #\.) (list (make-pos 13 1) #\.)
                    (list (make-pos 12 2) #\a) (list (make-pos 13 2) #\a)))

(define (associated-list pos grid)
  (local [;; x-start is the x position of the pos
          (define x-start (pos-x pos))
          
          ;; y-start in the y position of the pos
          (define y-start (pos-y pos))
          
          ;; flattened-pos is the list of all positions of the grid in a list
          (define flattened-pos
            (foldr (lambda (x y) (cons (make-pos (+ (pos-x x) x-start)
                                                 (+ (pos-y x) y-start)) y))
                   empty (all-positions (length (first grid)) (length grid))))  
          
          ;; glattened-grind is the list of all characters of the grid in a list
          (define flattened-grid
            (foldr (lambda (x y)
                     (append (foldr (lambda (u v) (cons u v)) empty x) y))
                   empty grid))
          
          ;; (pos-and-val lopos logrid) takes a lopos and logrid and turns the
          ;;   two individual lists into a associated list
          ;; pos-and-val: (listof Pos) (listof Char) -> (listof (list Pos Char))
          (define (pos-and-val lopos logrid)
            (cond [(and (empty? logrid) (empty? lopos)) empty]
                  [else (cons (list (first lopos) (first logrid))
                              (pos-and-val (rest lopos)(rest logrid)))]))]
    
    (pos-and-val flattened-pos flattened-grid)))

;; Tests:
(check-expect (associated-list (make-pos 3 3) '((#\.)))
              (list (list (make-pos 3 3) #\.)))
(check-expect (associated-list (make-pos 1 5) '((#\a #\a)
                                                (#\b #\b)))
              (list (list (make-pos 1 5) #\a) (list (make-pos 2 5) #\a)
                    (list (make-pos 1 6) #\b) (list (make-pos 2 6) #\b)))


;; (determine-pos pos base-al top-al) takes a base-al and a top-al and searches
;;   both associated lists to see if they contain pos and they superimpose the
;;   top-al onto the base-al according to the rules of a legal superimpose
;; determine-pos: Pos (listof (Pos Char)) (listof (Pos Char)) -> Char
;; requires: base-al must contain the pos regardless
;; Examples:
(check-expect (determine-pos
               (make-pos 1 5) (list (list (make-pos 1 5) #\a)
                                    (list (make-pos 2 5) #\a))
               (list (list (make-pos 2 5) #\.))) #\a)
(check-expect (determine-pos
               (make-pos 2 5) (list (list (make-pos 1 5) #\a)
                                    (list (make-pos 2 5) #\a))
               (list (list (make-pos 2 5) #\.))) #\a)
(check-expect (determine-pos
               (make-pos 2 5) (list (list (make-pos 1 5) #\a)
                                    (list (make-pos 2 5) #\a))
               (list (list (make-pos 2 5) #\b))) #\b)

(define (determine-pos pos base-al top-al)
  (local [;; base-pos-val searches the associated list base-al and determines
          ;;   if it contains the particular position in regards to the origin
          (define base-pos-val (filter (lambda (x) (equal? pos (first x)))
                                       base-al))
          
          ;; top-pos-val searches the associated list top-al and determines
          ;;   if it contains the particular position in regards to the origin
          (define top-pos-val (filter (lambda (x) (equal? pos (first x)))
                                      top-al))]
    
    (cond [(empty? top-pos-val) (second (first base-pos-val))]
          [(or (char=? #\. (second (first base-pos-val)))
               (not (char=? #\. (second (first top-pos-val)))))
           (second (first top-pos-val))]
          [else (second (first base-pos-val))])))

;; Tests:
(check-expect (determine-pos
               (make-pos 2 5) (list (list (make-pos 1 5) #\a)
                                    (list (make-pos 2 5) #\.))
               (list (list (make-pos 2 5) #\b))) #\b)
(check-expect (determine-pos
               (make-pos 2 5) (list (list (make-pos 1 5) #\a)
                                    (list (make-pos 2 5) #\.))
               (list (list (make-pos 2 5) #\.))) #\.)
(check-expect (determine-pos
               (make-pos 2 5) (list (list (make-pos 1 5) #\a)
                                    (list (make-pos 2 5) #\a))
               (list (list (make-pos 2 5) #\.))) #\a)
(check-expect (determine-pos
               (make-pos 1 5) (list (list (make-pos 1 5) #\a)
                                    (list (make-pos 2 5) #\.))
               (list (list (make-pos 2 5) #\b))) #\a)


;; (superimpose base top pos) the top is laid over base such that the consumed
;;   pos indicates the location of the upper-left corner of top
;;   note: Any #\. characters in top do not overwrite the contents of base
;; superimpose: Grid Grid Pos -> Grid
;; Example:
(check-expect (superimpose '((#\b #\. #\. #\. #\.)(#\b #\b #\b #\. #\.)
                                                  (#\. #\b #\. #\. #\.))
                           '((#\a #\a)(#\a #\a)(#\a #\.)) (make-pos 0 0))
              '((#\a #\a #\. #\. #\.) (#\a #\a #\b #\. #\.)
                                      (#\a #\b #\. #\. #\.)))
(check-expect (superimpose '((#\a #\a #\.)) '((#\b)) (make-pos 2 0))
              '((#\a #\a #\b)))
(check-expect (superimpose '((#\a #\a #\a) (#\. #\a #\.))
                           '((#\b #\b #\b) (#\b #\. #\b))
                           (make-pos 0 0))
              '((#\b #\b #\b) (#\b #\a #\b)))

(define (superimpose base top pos)
  (local [;; base-al is the an association list of all the character positions
          ;;   and grid characters
          (define base-al (associated-list (make-pos 0 0) base))
          
          ;; top-al is the an association list of all the character positions
          ;;   and grid characters
          (define top-al (associated-list pos top))]
    
    (build-2dlist (length (first base)) (length base)
                  (lambda (x y) (determine-pos (make-pos x y)
                                               base-al top-al)))))

;; Tests:
(check-expect (superimpose '((#\a #\a #\.)) '((#\. #\. #\.)) (make-pos 0 0))
              '((#\a #\a #\.)))
(check-expect (superimpose '((#\. #\. #\.)) '((#\b #\b #\b)) (make-pos 0 0))
              '((#\b #\b #\b)))
(check-expect (superimpose '((#\a #\a #\.)) '((#\. #\. #\.)) (make-pos 12 3))
              '((#\a #\a #\.)))
(check-expect (superimpose '((#\a #\a #\.)) '((#\. #\b #\.)) (make-pos 3 0))
              '((#\a #\a #\.)))
(check-expect (superimpose '((#\a #\a #\.)) '((#\. #\b #\.)) (make-pos 2 0))
              '((#\a #\a #\.)))
(check-expect (superimpose '((#\a #\a #\.)) '((#\. #\b #\.)) (make-pos 1 0))
              '((#\a #\a #\b)))
(check-expect (superimpose '((#\a #\a #\.)) '((#\. #\b #\.)) (make-pos 0 0))
              '((#\a #\b #\.)))
(check-expect (superimpose '((#\b #\. #\. #\. #\.)(#\b #\b #\b #\. #\.)
                                                  (#\. #\b #\. #\. #\.))
                           '((#\. #\a #\.)(#\a #\a #\a) (#\. #\a #\.))
                           (make-pos 1 2))
              '((#\b #\. #\. #\. #\.)(#\b #\b #\b #\. #\.)
                                     (#\. #\b #\a #\. #\.)))
(check-expect (superimpose '((#\b #\. #\. #\. #\.)(#\b #\b #\b #\. #\.)
                                                  (#\. #\b #\. #\. #\.))
                           '((#\. #\a #\.)(#\a #\a #\a) (#\. #\a #\.))
                           (make-pos 0 0))
              '((#\b #\a #\. #\. #\.)(#\a #\a #\a #\. #\.)
                                     (#\. #\a #\. #\. #\.)))
(check-expect
 (superimpose '((#\b #\. #\. #\. #\.)(#\b #\b #\b #\. #\.)(#\. #\b #\. #\. #\.))
              '((#\a #\a)(#\a #\a)(#\a #\.)) (make-pos 0 0))
 '((#\a #\a #\. #\. #\.)(#\a #\a #\b #\. #\.)(#\a #\b #\. #\. #\.)))

;;**********************************************************************
;; Problem 5

;; (neighbours state) takes a state and produces a list of states where the
;;   polyomino has been placed in the puzzle and removed from the list of
;;   pieces yet to be placed
;;neighbours: State -> (listof State)
;; Examples:
(check-expect
 (lists-equiv? (neighbours (make-state '((#\. #\.)(#\. #\.)) '(((#\X #\X)))))
               (list (make-state '((#\X #\X) (#\. #\.)) empty)
                     (make-state '((#\X #\.) (#\X #\.)) empty))) true)
;; Does not need explicit testing

(define (neighbours a-state)
  (local
    [;; puzzle is a constant to represent the puzzle of a-state
     (define puzzle (state-puzzle a-state))
     
     ;; puzzle-pos is a constant to represent the first empty pos
     ;;   in the puzzle
     (define puzzle-pos (first-empty-pos puzzle))
     
     ;; all-pieces is a constant to represents all pieces in the list
     ;;   in a-state
     (define all-pieces (state-pieces a-state))
     
     ;; (subtract-pos pos x) takes a pos and the x position of another
     ;;   pos and subtracts x from (pos-x pos) to get the new pos
     ;; subtract-pos: Pos Nat -> Pos
     (define (subtract-pos pos x)
       (make-pos (- (pos-x pos) x)
                 (pos-y pos)))
     
     ;; (piece-pos-x row col-position) takes a row and an accumulator
     ;;   called col-position and returns the x-position of the first
     ;;   non #\. character
     ;;   piece-pos-x: (listof Char) Nat -> Nat
     ;;   requires: row must contain at least 1 non #\. character
     (define (piece-pos-x row col-position)
       (cond [(not (char=? #\. (first row))) col-position]
             [else (piece-pos-x (rest row) (add1 col-position))]))
     
     ;; (get-state orientations other-pieces) gets all the orientations
     ;;   of a particular piece and the other-pieces left over and
     ;;   determines which of the orientations can be legally superimposed
     ;;   onto the puzzle and outputs a list of the valid states
     ;; get-state: (listof Grid) (listof Grid) -> (listof State)
     (define (get-state orientations other-pieces)
       (cond
         [(empty? orientations) empty]
         [(compare
           (superimpose puzzle (first orientations)
                        (subtract-pos puzzle-pos
                                      (piece-pos-x (first (first orientations))
                                                   0)))
           puzzle (first orientations))
          (cons
           (make-state
            (superimpose puzzle (first orientations)
                         (subtract-pos puzzle-pos
                                       (piece-pos-x (first (first orientations))
                                                    0)))
            other-pieces)
           (get-state (rest orientations) other-pieces))]
         [else (get-state (rest orientations) other-pieces)]))
     
     ;; (remaining-piece lop p) takes a lop and a p and returns
     ;;    lop without p
     ;; remaining-piece: (listof Grid) Grid -> (listof Grid)
     (define (remaining-piece lop p)
       (cond [(empty? lop) empty]
             [(equal? (first lop) p) (remaining-piece (rest lop) p)]
             [else (cons (first lop) (remaining-piece (rest lop) p))]))
     
     ;; (all-possibility pieces) takes a list of pieces and returns
     ;;   all orientations of all pieces can be legally superimposed
     ;;   onto the puzzle grid
     (define (all-possibility pieces)
       (cond [(empty? pieces) empty]
             [else (append (get-state (all-orientations (first pieces))
                                      (remaining-piece all-pieces
                                                       (first pieces)))
                           (all-possibility (rest pieces)))]))]
    
    (all-possibility all-pieces)))

;; Tests: Explicit testing is not required


;; (compare imposed-grid unimposed-grid piece) compares the unimposed-grid with
;;   the same grid with the piece in the imposed-grid and determines if the
;;   super-impose is legal
;;   In particular, it checks to see if the piece is cut off after it is imposed
;;     or if any of top piece was imposed on already existing pieces in the
;;     unimposed grid
;; compare: Grid Grid Grid -> Bool
;; Examples: Explicit examples are not required
(define (compare imposed-grid unimposed-grid piece)
  (local
    [;; (check-g1/g2 lst1 lst2) is a function to see if lst2 was legally
     ;;   superimpose into lst1 by comparing all the characters in each row
     ;; check-g1/g2: (listof Char) (listof Char) -> Bool
     (define (check-g1/g2 lst1 lst2)
       (cond [(and (empty? lst1) (empty? lst2)) true]
             [(or (equal? #\. (first lst2)) (equal? (first lst1) (first lst2)))
              (check-g1/g2 (rest lst1) (rest lst2))]
             [else false]))
     
     ;; constant lo-pieces creates a list of all the pieces character that
     ;;  are not #\.
     (define lo-piece
       (foldr (lambda (x y) (append (filter (lambda (z) (not (char=? z #\.))) x)
                                    y)) empty piece))
     
     ;; constant piece-char determines the character of the pice and if its an
     ;;   empty piece returns false
     (define piece-char (cond [(empty? lo-piece) false]
                              [else (first lo-piece)]))
     
     ;; piece-size is the size of the piece
     (define piece-size (length lo-piece))
     
     ;; pieces-contained determines the number of characters of piece are
     ;;   contained within the imposed-grid
     (define pieces-contained
       (length
        (foldr (lambda (x y)
                 (append (filter (lambda (z) (equal? piece-char z)) x) y))
               empty imposed-grid)))]
    
    (and (= piece-size pieces-contained)
         (foldr (lambda (x y z) (and (check-g1/g2 x y) z))
                true imposed-grid unimposed-grid))))

;; Tests: Explicit testing is not required
