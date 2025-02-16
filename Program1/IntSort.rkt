#lang racket

(require rackunit)

; Function to count occurrences of each integer in a list and return value-count pairs
(define (count-occurrences lst)
  (define hash-table (make-hash))
  (for-each (lambda (x) (hash-update! hash-table x add1 0)) lst)
  (hash-map hash-table cons))

; Function to sort the value-count pairs by value
(define (sort-value-count-pairs pairs)
  (sort pairs < #:key car))

; Function to expand value-count pairs into a sorted list of integers
(define (expand-value-count-pairs pairs)
  (apply append (map (lambda (pair) (make-list (cdr pair) (car pair))) pairs)))

; Function to verify if a list is sorted
(define (is-sorted? lst)
  (or (null? lst)
      (null? (cdr lst))
      (and (<= (car lst) (cadr lst)) (is-sorted? (cdr lst)))))

; Function to read a list of integers from a file
(define (read-integers filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((nums '()))
        (define val (read))
        (if (eof-object? val)
            (reverse nums)
            (loop (cons val nums)))))))

; Function to sort the integers from a file using counting sort
(define (counting-sort filename)
  (let* ((nums (read-integers filename))
         (value-counts (count-occurrences nums))
         (sorted-value-counts (sort-value-count-pairs value-counts))
         (sorted-integers (expand-value-count-pairs sorted-value-counts))
         (list-size (length nums)))  ; Get the size of the list

    ; Only print details for small datasets
    (when (< list-size 1000)
      (displayln "Value-Frequency Pairs: ")
      (displayln value-counts)
      
      (displayln "Sorted Value-Count Pairs: ")
      (displayln sorted-value-counts)
      
      (displayln "Sorted List: ")
      (for-each (lambda (x) (display x) (display " ")) sorted-integers)
      (newline))  ; Ensures proper formatting

    ; Always print whether the list is sorted
    (displayln (string-append "List is sorted: " (if (is-sorted? sorted-integers) "Yes" "No")))

    (void)))  ; Prevents returning the sorted list in the REPL

; Unit tests for the functions

; Test 1: count-occurrences function
(define test-list '(1 2 2 3 3 3 4))
(define expected-counts '((1 . 1) (2 . 2) (3 . 3) (4 . 1)))  ; Using a list of pairs instead of a hash table
(check-equal? (count-occurrences test-list) expected-counts)

; Test 2: sort-value-count-pairs function
(define unsorted-pairs '((3 . 2) (1 . 4) (4 . 1) (2 . 3)))
(define sorted-pairs '((1 . 4) (2 . 3) (3 . 2) (4 . 1)))
(check-equal? (sort-value-count-pairs unsorted-pairs) sorted-pairs)

; Test 3: expand-value-count-pairs function
(define value-count-pairs '((1 . 2) (2 . 3) (3 . 1)))
(define expanded-list '(1 1 2 2 2 3))
(check-equal? (expand-value-count-pairs value-count-pairs) expanded-list)

; Test 4: is-sorted? function
(define sorted-list '(1 2 3 4 5))
(define unsorted-list '(5 4 3 2 1))
(check-true (is-sorted? sorted-list))
(check-false (is-sorted? unsorted-list))

; Test 5: read-integers function
(define test-file "test.txt")
(with-output-to-file test-file
  (lambda ()
    (display "1 2 3 4 5")))
(check-equal? (read-integers test-file) '(1 2 3 4 5))

; Example usage:
(counting-sort "./data/data-1.txt")
(counting-sort "./data/data-2.txt")
(counting-sort "./data/data-3.txt")
(counting-sort "./data/data-4.txt")
(counting-sort "./data/data-5.txt")
(counting-sort "./data/data-6.txt")
(counting-sort "./data/data-7.txt")