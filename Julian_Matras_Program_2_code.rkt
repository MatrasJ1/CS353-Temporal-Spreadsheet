#lang racket
; parse-grid : list-of-entries -> grid (association list)
; input shape: (((x y) value) ...)
; output shape: (((x . y) value) ...)
;Logic for parse functions written by Claude Sonnet 4.6 and revised by me
(define (parse-grid entries)
  (map parse-entry entries))

; parse-entry : entry -> (coord  value)
(define (parse-entry entry)
  (list (parse-coord (first entry)) (parse-value (second entry))))

; parse-value : value -> number or formula
(define (parse-value value)
  (cond
    [(number? value) value]
    [else
     (parse-formula value)]))

; parse-formula : raw-formula -> normalized formula
; input:  (= (x y) op (x y))
; output: (= (x . y) op (x . y))
(define (parse-formula formula)
  (match formula
    [`(= ,lhs ,op ,rhs)
     `(= ,(parse-term lhs) ,op ,(parse-term rhs))]
    [_ 'Malformed-formula]))

; parse-term : term -> number or coord
(define (parse-term term)
  (cond
    [(number? term) term]
    [else
     (parse-coord term)]))

; parse-coord : coord list -> coord (x . y)
(define (parse-coord coord)
  (cons (first coord) (second coord)))

; manhattan-distance : coord coord -> number
(define (manhattan-distance c1 c2)
  (+ (abs (- (car c1) (car c2)))
     (abs (- (cdr c1) (cdr c2)))))

; coord-parity : coord -> boolean
(define (coord-even? coord)
  (even? (+ (car coord) (cdr coord))))

;;; --- Grid Lookup ---

; lookup : coord grid visited -> value | sentinel value
(define (lookup coord grid visited)
  (let ([entry (assoc coord grid)]) ;searches association list for given key and returns coord value pair
    (cond
      [(member coord visited) '|#CYCLE|] ;cycle detected
      [entry (second entry)]
      [else
       0])))


;;; --- Cycle Detection via Visited Set ---

; eval-cell : coord grid visited -> number
; visited is a list of coords we're currently resolving (the call stack)
(define (eval-cell coord grid visited)
  (let ([value (lookup coord grid visited)]) ;find value from coordinate
    (cond
      [(eq? value '|#CYCLE|) 'cycle-error]
      [(number? value) value] ;if not a formula, return the number
      [else ;formula case
       (let ([result (eval-formula value grid (cons coord visited) coord)]) ;evaluate formula
         (cond
           [(eq? result 'cycle-error) 'cycle-error] ;propagate cycle error state
           [(eq? result 'parity-error) 'parity-error] ;propagate parity error state
           [else result]))])))


;;; --- Formula Evaluator ---

; eval-formula : formula grid visited -> number
; formula shape: (= <ref-or-num> <op> <ref-or-num>)
(define (eval-formula formula grid visited current-coord)
  (match formula
    [`(= ,lhs ,op ,rhs)
     (let ([l (resolve lhs grid visited current-coord)] ;get number either straight or from coord
           [r (resolve rhs grid visited current-coord)])
       (cond
         [(or (eq? l 'cycle-error) (eq? r 'cycle-error)) 'cycle-error] ;propagate up if either side is a cycle
         [(or (eq? l 'parity-error) (eq? r 'parity-error)) 'parity-error] ;propagate up if either side has parity error
         [else (apply-op op l r)]))] ;perform calculation
    [_ 'Malformed-formula]))


;;; --- Resolve: number or cell reference ---

; resolve : term grid visited current-coord -> number
; A term is either a raw number or a coordinate (x . y)
(define (resolve term grid visited current-coord)
  (cond
    [(number? term) term]
    [(pair? term)  ; it's a coord
     (let ([term-value (eval-cell term grid visited)]) ;evaluate the term if it's a coord
       (cond
         [(eq? term-value 'cycle-error) 'cycle-error]
         [(eq? term-value 'parity-error) 'parity-error]
         [(and (coord-even? current-coord) (not (coord-even? term))) 'parity-error]
         [else (- term-value (manhattan-distance current-coord term))]))]
    [else 'Unknown-term]))


;;; --- Operator Dispatch ---

; apply-op : symbol number number -> number
(define (apply-op op l r)
  (match op
    ['+ (+ l r)]
    ['- (- l r)]
    ['* (* l r)]
    ['/ (if (zero? r)
            (error "Division by zero")
            (/ l r))]
    [_  'Unknown-operator]))

; eval-grid : grid -> grid
; returns a new grid with all formulas replaced by their numeric results
(define (eval-grid grid)
  (map (lambda (entry)
         (list (car entry)
               (eval-cell (car entry) grid '())))
       grid))