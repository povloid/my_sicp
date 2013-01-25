; Для того чтобы выполнить задание забежим вперед в 3 главу 
; и возьмем от туда построение таблици

;Итак общие функции работы с двумерными таблицами

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)


; Теперь специфика под задачу
(define table-1 (list '*table*))

(define (get op type)
  (lookup op type table-1))

(define (put op type element)
  (insert! op type element table-1))


; Обобщенные функции
(define (get-file-dep-name file)
  (car file))

;а)
(define (get-record file fname)
  ((get 'get-record (get-file-dep-name file))
   (cdr file) fname))
;б)
(define (get-salary file fname)
  ((get 'get-salary (get-file-dep-name file))
   (get-record file fname)))


;; Определим файлы
(define file1 (cons '30 (list 
                         (list (cons 'fname 'Калашников)
                               (cons 'iname 'Максим)
                               (cons 'lname 'Висарионович)
                               (cons 'salary 60000))
                         
                         (list (cons 'fname 'Чапаев)
                               (cons 'iname 'Василий)
                               (cons 'lname 'Иванович)
                               (cons 'salary 500000))
                         
                         (list (cons 'fname 'Ленин)
                               (cons 'iname 'Владимир)
                               (cons 'lname 'Ильич)
                               (cons 'salary 600000)))))


(define (install-30)
  (define (get-record file fname)
    ;(display (cdr (car (car file))))(display "|")
    (cond ((null? file) 'not-found)
          ((eq? (cdr (car (car file))) fname) (car file))
          (else
           (get-record (cdr file) fname))))
  
  (define (get-salary record)
    (if (eq? record 'not-found) 'not-found
        (cdr (cadddr record))))
  
  (put 'get-record '30 get-record)
  (put 'get-salary '30 get-salary)
  )


(install-30)

(newline)
(get-record file1 'Чапаев)
(get-salary file1 'Чапаев)

(define file2 (cons '70 (list 
                         (list 'fname 'Хрущев
                               'iname 'Никита
                               'lname 'Сергеевич
                               'salary 50000)
                         
                         (list 'fname 'Брежнев
                               'iname 'Леонид
                               'lname 'Ильич
                               'salary 60000)
                         
                         (list 'fname 'Горбачев
                               'iname 'Михаил
                               'lname 'Сергеевич
                               'salary 200000))))


(define (install-70)
  (define (get-attr-value record atr)
    (cond ((null? record) 'not-found)
          ((eq? (car record) atr) (car (cdr record)))
          (else 
           (get-attr-value (cdr record) atr))))
  
  (define (get-record file fname)
    ;;(display (get-attr-value (car file) 'fname))(display "|")
    (cond ((null? file) 'not-found)
          ((eq? (get-attr-value (car file) 'fname) fname) (car file))
          (else
           (get-record (cdr file) fname))))
  
  (define (get-salary record)
    (if (eq? record 'not-found) 'not-found
        (get-attr-value record 'salary)))
  
  (put 'get-record '70 get-record)
  (put 'get-salary '70 get-salary)
  )


(install-70)

(newline)
(get-record file2 'Брежнев)
(get-salary file2 'Брежнев)

(define file3 (cons '2000 (list 
                           (list (list 'fname 'Ельцин
                                       'iname 'Борис
                                       'lname 'Николаевич)
                                 'salary 5000000)
                           
                           (list (list 'fname 'Медведев
                                       'iname 'Дмитрий
                                       'lname 'Анатольевич)
                                 'salary 60000000)
                           
                           (list (list 'fname 'Путин
                                       'iname 'Владимир
                                       'lname 'Владимирович)
                                 'salary 20000000000))))


(define (install-2000)
  (define (get-attr-value record atr)
    (cond ((null? record) 'not-found)
          ((eq? (car record) atr) (car (cdr record)))
          (else 
           (get-attr-value (cdr record) atr))))
  
  (define (get-record file fname)
    ;;(display (car (car file)))(display ">")
    ;;(display (get-attr-value (car file) 'fname))(display "|")
    (cond ((null? file) 'not-found)
          ((eq? (get-attr-value  (car (car file)) 'fname) fname) (car file))
          (else
           (get-record (cdr file) fname))))
  
  (define (get-salary record)
    (if (eq? record 'not-found) 'not-found
        (get-attr-value record 'salary)))
  
  (put 'get-record '2000 get-record)
  (put 'get-salary '2000 get-salary)
  )


(install-2000)

(newline)

(get-record file3 'Медведев)
(get-salary file3 'Медведев)



(define (find-employee-record files fname)
  (if (null? files) 'not-found
      (let ((record (get-record (car files) fname)))
        (if (pair? record) record
            (find-employee-record (cdr files) fname)))))

;в)
(define files (list file1 file2 file3))

(newline)
(find-employee-record files 'Путин)

(find-employee-record files 'Калашников)
(find-employee-record files 'Хрущев)
(newline)
(find-employee-record files 'Хрущев2)


;г) Так как система адитивна то надо только внести коды подразделений и функции для адаптации к тем
;или иным структурам подразделений














