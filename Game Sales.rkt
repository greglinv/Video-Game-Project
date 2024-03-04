#lang racket

(require csv-reading)

(define (read-csv-to-list-of-lists file-path)
  (with-input-from-file file-path
    (lambda ()
      (csv->list (current-input-port)))))

(define (find-column-index headers column-name)
  (for/or ([header (in-list headers)] [index (in-naturals)])
    (if (string=? header column-name)
        index
        #f)))

(define (filter-csv-data csv-data column-name criterion)
  (let* ((headers (first csv-data))
         (column-index (find-column-index headers column-name)))
    (filter (lambda (row)
              (match criterion
                [(? number? n) (> (string->number (list-ref row column-index)) n)]
                [(? string? s) (string=? (list-ref row column-index) s)]))
            (rest csv-data)))) ; Skip headers for filtering

;; Example usage:
(define csv-data (read-csv-to-list-of-lists "Video Games Sales.csv"))

;; Example of filtering: find games published by Nintendo
(define nintendo-games (filter-csv-data csv-data "Publisher" "Nintendo"))

;; Example of filtering: find games with global sales more than 20 million
(define high-selling-games (filter-csv-data csv-data "Global" 20))

;Accounts for Commas now, Need to come up with filtering and menus.