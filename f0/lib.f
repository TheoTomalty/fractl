\ -*- mode: text -*-

: full-check-lambda-list ( #args #reqend #optend #lambda-list -- #args )
    vr- 2 pick list-length
    ( #args #reqend #optend length )
    swap unbox-fixnum rot unbox-fixnum swap between
        unless s" Invalid number of arguments." error then
;

: full-check-lambda-list-rest ( #args #reqend #lambda-list -- #args )
    vr- unbox-fixnum over list-length <=
        unless s" Not enough arguments." error then
;

: full-print ( #form -- 'nil )
    cr dup cl. vr- nil vr+
;

: full-values ( #args -- #arg[0] )
    dup nil <> if
        0 over cdr
        ( #args n @args )
        begin dup nil <> while
            dup car vr+ 2 pick cells value-store + !
            cdr swap 1+ swap
        repeat drop
        ( #args n )
        1+ num-values !
        full-car
    else
        0 num-values !
    then
;

: full-list ( #args -- #args ) ;

'' cons vr+ ' full-cons 2 wrap-function '' cons symbol-function !
'' car  vr+ ' full-car  1 wrap-function '' car  symbol-function !
'' cdr  vr+ ' full-cdr  1 wrap-function '' cdr  symbol-function !
'' check-lambda-list      vr+ ' full-check-lambda-list      4 wrap-function '' check-lambda-list      symbol-function !
'' check-lambda-list-rest vr+ ' full-check-lambda-list-rest 3 wrap-function '' check-lambda-list-rest symbol-function !
'' funcall vr+ ' full-funcall 1 wrap-function-rest '' funcall symbol-function !
'' print   vr+ ' full-print   1 wrap-function      '' print   symbol-function !
'' values  vr+ ' full-values  0 wrap-function-rest '' values  symbol-function !
'' list    vr+ ' full-list    0 wrap-function-rest '' list    symbol-function !

1 box-fixnum '' one symbol-value !
2 box-fixnum '' two symbol-value !
nil vr+ nil symbol-value !
t   vr+ t   symbol-value !


: main
    s" cl/base.lisp" load
    
    \\ s" (macrolet ((foo () 'one) (bar (x) (list 'quote x))) (print (foo)) (print (bar (baz bing (bong) boing))))" make-string-stream read drop
    \\ ( val )
    \\ eval cr
    \\ num-values @ 0= if
    \\     s" ;; No values" tell
    \\ else
    \\     cl. cr
    \\     num-values @ 1- 0 do
    \\         i cells value-store + @ cl. cr
    \\     loop
    \\ then
    \\ s" (x y &optional w)" read-closure read drop
    \\ ( val )
    \\ s" ARGS" make-symbol simple-expand-lambda-list cr cl. cr
    .s cr
;

hex
main
