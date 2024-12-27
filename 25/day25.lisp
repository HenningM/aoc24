(set! partition (fn* (lst condf)
  (if (nil? lst)
      (list nil nil)
      (if (condf (car lst))
          (let* (
                 rest (partition (cdr lst) condf)
                 )
            (list (cons (car lst) (car rest)) (car (cdr rest))))
          (let* (
                 rest (partition (cdr lst) condf)
                 )
            (list (car rest) (cons (car lst) (car (cdr rest)))))))))

(set! split-on-empty-line-recursive (fn* (lst current result)
  (cond
    (nil? lst)
     (if current
         (cons (reverse current) result)
         result)
    (string= (car lst) "")
     (split-on-empty-line-recursive (cdr lst) nil (cons (reverse current) result))
    (list true)
     (split-on-empty-line-recursive (cdr lst) (cons (car lst) current) result))))

(set! split-on-empty-line (fn* (lst)
  (reverse (split-on-empty-line-recursive lst nil nil))))

(set! is-key (fn* (elem)
                  (let* (
                         frst (car elem)
                         )
                    (not (string<= frst "#####")))))

(set! key:sum-col (fn* (elem n)
                  (let* (
                         col (map elem (lambda (c) (nth (explode c) n)))
                         cnt (length (filter col (lambda (c) (char<= c #\#))))
                         )
                    cnt)))

(set! key:to-nums (fn* (elem)
                  (let* (
                         num-str (map (seq 4) (lambda (n) (key:sum-col elem n)))
                         )
                    num-str)))

(set! key:fits-lock (fn* (key lock)
                  (let* (
                         col-sum (map (seq 4) (lambda (n) (+ (nth key n) (nth lock n))))
                         col-over-five (filter col-sum (lambda (s) (< 5 s)))
                         )
                    (= 0 (length col-over-five)))))

(set! find-pairs (fn* (keys locks)
                  (let* (
                         key-pairs (map keys (lambda (k) (filter locks (lambda (l) (key:fits-lock k l)))))
                         )
                    (/ (length (flatten key-pairs)) 5))))

(set! solve-part-one (fn* (elems)
                     (let* (
                            parted-elems (partition elems is-key)
                            keys (map (car parted-elems) (lambda (e) (take 5 (drop 1 e))))
                            locks (map (car (cdr parted-elems)) (lambda (e) (take 5 (drop 1 e ))))
                            locks-nums (map locks key:to-nums)
                            keys-nums (map keys key:to-nums)
                            pairs (find-pairs keys-nums locks-nums)
                            )
                       pairs)))

(set! read:input (fn* (file)
                     (let* (
                            text (file:lines file)
                            elems (split-on-empty-line text)
                            )
                       elems)))

(let* (
      input (read:input "/dev/stdin")
      part-one (solve-part-one input)
      )
(print part-one))
