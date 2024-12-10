(set! parse:line (fn* (line)
                     (let* (
                            elems (split line " ")
                            nums (map elems number)
                            )
                       nums)))
(set! read:input (fn* (file)
                     (let* (
                            text (file:lines file)
                            elems (map text parse:line)
                            )
                       elems)))


(apply (read:input "/dev/stdin") print)
