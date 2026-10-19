\ -*- mode: text -*-

0 constant info-symbol-macro-tag
1 constant info-num

0 value info-tables

: init-info ( -- )
    info-num 0 do make-eq-hash-table loop info-num array to info-tables
; init-info

: info ( symbol type -- info found-p )
    cells info-tables + @ gethash
;

: setf-info ( value symbol type -- found-p )
    cells info-tables + @ sethash
;

( symbol -- info found-p )
: info-symbol-macro info-symbol-macro-tag info ;

: comp-offset ;
: comp-template 8+ ;

: make-comp ( template -- comp )
    2 cells malloc swap over comp-template !
    ( comp )
    0 over comp-offset !
;

variable compiler
0 compiler !

: c,, ( val -- )
    \dup <cfa ?dup if >name tell cr else dup . cr then
    compiler @ comp-template @ function-routine
    ( val addr )
    dup @ rot ,, swap !
;

: ct@ ( -- func ) compiler @ comp-template @ ;
: ct! ( func -- ) compiler @ comp-template ! ;

: co+!  ( n --   ) compiler @ comp-offset +! ;
: co-!  ( n --   ) compiler @ comp-offset -! ;
: co1+! (   --   ) 1 co+! ;
: co1-! (   --   ) 1 co-! ;
: co!   ( n --   ) compiler @ comp-offset ! ;
: co@   (   -- n ) compiler @ comp-offset @ ;

: ci@ (     -- i ) ct@ function-routine @ routine-length @ ;
: ci! ( x i --   ) ct@ function-routine @ swap routine-elem ! ;

: cswap ( comp -- comp ) compiler @ swap compiler ! ;

'' lexenv make-built-in-class constant lexenv-class

: lexenv-parent    ( env -- &parent    ) 0 object-slot ;
: lexenv-variables ( env -- &variables ) 1 object-slot ;
: lexenv-functions ( env -- &functions ) 2 object-slot ;
: lexenv-blocks    ( env -- &blocks    ) 3 object-slot ;
: lexenv-tags      ( env -- &tags      ) 4 object-slot ;
: lexenv-templates ( env -- &templates ) 5 object-slot ;
6 constant lexenv-size

: lexenv-inherit-slot ( parent obj addr-routine -- )
    rot dup nil <> if over execute @ then vr+ -rot
    ( val obj addr-routine )
    execute !
;

: lexenv-variable ( symbol lexenv -- entry )
    dup nil <> if lexenv-variables @ assocr else 2drop nil then
;

: lexenv-function ( symbol lexenv -- entry )
    dup nil <> if lexenv-functions @ assocr else 2drop nil then
;

: lexenv-block ( symbol lexenv -- entry )
    dup nil <> if lexenv-blocks @ assocr else 2drop nil then
;

: lexenv-tag ( symbol lexenv -- entry )
    dup nil <> if lexenv-tags @ assocr else 2drop nil then
;

: lexenv-template ( lexenv -- func ) lexenv-templates @ car ;

: make-lexenv ( #parent -- #lexenv )
    lexenv-size lexenv-class vr+ make-object 2dup lexenv-parent !
    ( parent lexenv )
    2dup ' lexenv-variables lexenv-inherit-slot
    2dup ' lexenv-functions lexenv-inherit-slot
    2dup ' lexenv-blocks    lexenv-inherit-slot
    2dup ' lexenv-tags      lexenv-inherit-slot
    2dup ' lexenv-templates lexenv-inherit-slot
    swap drop
;

'' macro-function make-built-in-class constant macro-function-class
: macro-function-function ( macro-function -- &function ) 0 object-slot ;

: make-macro-function ( function -- macro-function )
    1 macro-function-class vr+ make-object swap over macro-function-function !
;

: macro-function-p ( v -- b )
    dup object-p if object-class@ macro-function-class = else drop false then
;

'' variable-cell make-built-in-class constant variable-cell-class
: variable-cell-ref 0 object-slot ;

: make-variable-cell ( v -- cell )
    1 variable-cell-class vr+ make-object swap over variable-cell-ref !
;

make-eq-hash-table constant special-form-table

: set-sf-tag ( n sym -- n )
    vr+ over box-fixnum swap special-form-table sethash drop
;

00 '' let                  set-sf-tag constant sf-let-tag
01 '' let*                 set-sf-tag constant sf-let*-tag
02 '' symbol-macrolet      set-sf-tag constant sf-symbol-macrolet-tag
03 '' setq                 set-sf-tag constant sf-setq-tag
04 '' block                set-sf-tag constant sf-block-tag
05 '' return-from          set-sf-tag constant sf-return-from-tag
06 '' tagbody              set-sf-tag constant sf-tagbody-tag
07 '' go                   set-sf-tag constant sf-go-tag
08 '' catch                set-sf-tag constant sf-catch-tag
09 '' throw                set-sf-tag constant sf-throw-tag
10 '' load-time-value      set-sf-tag constant sf-load-time-value-tag
11 '' eval-when            set-sf-tag constant sf-eval-when-tag
12 '' locally              set-sf-tag constant sf-locally-tag
13 '' flet                 set-sf-tag constant sf-flet-tag
14 '' labels               set-sf-tag constant sf-labels-tag
15 '' macrolet             set-sf-tag constant sf-macrolet-tag
16 '' function             set-sf-tag constant sf-function-tag
17 '' multiple-value-call  set-sf-tag constant sf-multiple-value-call-tag
18 '' multiple-value-prog1 set-sf-tag constant sf-multiple-value-prog1-tag
19 '' the                  set-sf-tag constant sf-the-tag
20 '' if                   set-sf-tag constant sf-if-tag
21 '' progn                set-sf-tag constant sf-progn-tag
22 '' progv                set-sf-tag constant sf-progv-tag
23 '' unwind-protect       set-sf-tag constant sf-unwind-protect-tag
24 '' quote                set-sf-tag constant sf-quote-tag
25                                    constant sf-variable-tag
26                                    constant sf-funcall-tag
27                                    constant sf-literal-tag
28                                    constant sf-lambda-tag
29                                    constant sf-num
29                                    constant sf-invalid-tag

: get-sf-tag ( sym -- n )
    special-form-table gethash nil =
    ( n found-p )
    if drop sf-invalid-tag else unbox-fixnum then
;

' unimplemented sf-num copies sf-num array constant normalize-routines
: normalize-routine ( n -- routine ) cells normalize-routines + @ ;
: set-normalize-routine ( n -- )
    cells normalize-routines + latest @ >cfa swap !
;

' unimplemented sf-num copies sf-num array constant compile-routines
: compile-routine ( n -- routine ) cells compile-routines + @ ;
: set-compile-routine ( n -- )
    cells compile-routines + latest @ >cfa swap !
;

'' comp-node make-built-in-class constant node-class

: node-tag    ( node -- &tag    ) 0 object-slot ;
: node-lexenv ( node -- &lexenv ) 1 object-slot ;
: node-form   ( node -- &form   ) 2 object-slot ;
3 constant node-size

: make-node ( @lexenv @form #x_1 ... #x_n n tag -- #node )
    box-fixnum over node-size + node-class vr+ make-object
    ( #lexenv #form #x_1 ... #x_n n tag #node )
    swap over node-tag ! swap
    ( #lexenv #form #x_1 ... #x_n #node n )
    begin dup 0> while
        1-
        rot over node-size + 3 pick swap object-slot !
    repeat drop
    ( #lexenv #form #node )
    \ TODO: remove these increments without it crashing
    swap vr+ over node-form   !
    swap vr+ over node-lexenv !
;

: macroexpand-1 ( #form @lexenv -- #form b )
    over symbol-p if
        2dup lexenv-variable
        ( form lexenv variable-entry )
        dup nil <> if
            dup first :: symbol-macro = if
                second vr+ -rot drop vr- true
            else
                2drop false
            then
        else drop
            ( form lexenv )
            over info-symbol-macro nil <> if
                ( form lexenv replacement )
                vr+ -rot drop vr- true
            else
                2drop false
            then
        then
    else over cons-p if
        over car over lexenv-function
        ( #form @lexenv @function-entry )
        dup nil <> if
            second vr+ swap vr+ swap 2 nvfuncall true
        else
            2drop false
        then
    else
        drop false
    then then
;

: macroexpand ( #form @lexenv -- #form b )
    swap over
    ( lexenv form lexenv )
    macroexpand-1 if
        ( lexenv form )
        begin over macroexpand-1 while repeat
        swap drop true
    else
        swap drop false
    then
;

: normalize ( #lexenv #form -- #node )
    over macroexpand drop
    dup symbol-p if
        sf-variable-tag
    else dup cons-p if
        \ TODO: check that form is a proper list
        dup car special-form-table gethash nil <> if
            ( lexenv form tag )
            unbox-fixnum
        else
            drop sf-funcall-tag
        then
    else
        sf-literal-tag
    then then
    normalize-routine execute
;

: compile ( @node -- )
    dup node-tag @ unbox-fixnum compile-routine execute
;

128 constant value-store-size
value-store-size cells allot constant value-store
variable num-values

1 num-values !
: clear-value-store ( -- )
    num-values @ 1- 0 do
        i cells value-store + @ vr-
    loop
    1 num-values !
;

: values->r ( -- )
    r> num-values @ 1- 0
    ( raddr n-1 i )
    begin 2dup > while
        dup cells value-store + @ vr+ >r
        1+
    repeat 2drop
    ( raddr )
    num-values @ >r
    >r 
;

: values-r> ( -- )
    r> r> dup 1-
    ( raddr n n-1 )
    begin dup 0> while
        1-
        r> over cells value-store + v!
    repeat drop
    ( raddr n )
    num-values ! >r
;

: values-save ( #ret -- #values )
    num-values @ make-t-vector dup vector-data rot
    ( #values addr #ret )
    num-values @ 0> if
        over ! 8+ num-values @ 1-
    else
        vr- 0
    then 0 do
        ( #values addr )
        i cells value-store + @ over ! 8+
    loop drop
    1 num-values !
;

: values-restore ( #values -- #ret )
    dup vector-length@ 0<> if
        dup vector-data dup @ vr+ -rot 8+
        ( #ret #values addr )
        over vector-length@ 1- 0 do
            dup @ vr+ i cells value-store + ! 8+
        loop
        drop dup vector-length@ num-values ! vr-
    else
        nil vr+ swap vr-
        0 num-values !
    then
;

: make-variable-node ( lexenv form -- node )
    0 sf-variable-tag make-node
;

: use-symbol ( @symbol @lexenv -- )
    dup lexenv-templates @ swap 2 pick swap lexenv-variable
    ( @symbol @templates @entry )
    dup nil <> if dup first :: lexical-variable = if third swap
        ( @symbol @home-temlate @templates )
        begin 2dup car <> while
            dup car 3 pick vr+ add-symbol-template
            cdr
        repeat
    then then
    3 ndrop
 ;

: normalize-variable ( #lexenv #form -- #node )
    [ sf-variable-tag set-normalize-routine ]
    2dup swap use-symbol
    make-variable-node
;

: unbound-variable-guard ( v -- v )
    dup unbound-marker = if s" Variable is unbound." error then
;

: compile-variable ( @node -- )
    [ sf-variable-tag set-compile-routine ]
    dup node-form @ swap node-lexenv @ 2dup lexenv-variable
    ( symbol lexenv entry )
    dup nil <> if
        dup first case
            :: lexical-variable of
                dup fourth
                ( symbol lexenv entry offsets )
                ct@ swap assocr unbox-fixnum
                ( symbol lexenv entry variable-offset )
                co@ swap - ' lit c,, c,, ' pick c,,
                ( symbol lexenv entry )
                dup second nil <> if
                    \ Indirect.
                    ' variable-cell-ref c,, ' @ c,,
                then
                ' vr+ c,,
            endof
            :: dynamic-variable of
                drop nil
            endof
            unreachable
        endcase
    then
    nil = if
        ( symbol lexenv )
        \ Dynamic variable
        drop ' lit c,, vr+ c,, ' symbol-value c,, ' @ c,, ' unbound-variable-guard c,, ' vr+ c,,
    else 2drop then
    co1+!
;

: push-end ( addr #v -- addr )
    over @ cons over ! @ &cdr
;

: let-node-bindings 0 node-size + object-slot ;
: let-node-body     1 node-size + object-slot ;
: make-let-node ( lexenv form bindings body -- node )
    2 sf-let-tag make-node
;

: invalid-let s" Invalid LET form." error ;
: normalize-let ( #lexenv #form -- #node )
    [ sf-let-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-let then
    dup second dup list-p unless invalid-let then
    ( #lexenv #form @bindings )
    nil vr+ dsp@
    ( #lexenv #form @bindings #node-bindings addr )
    begin 2 pick dup nil <> while car
        ( #lexenv #form @bindings #node-bindings addr @binding )
        dup symbol-p if
            nil vr+ 2list
        else vr+ then
        dup list-p          unless invalid-let then
        dup list-length 2 = unless invalid-let then
        5 pick vr+ over second vr+ normalize
        ( #lexenv #form @bindings #node-bindings addr #binding #subnode )
        swap dup first vr+ swap vr- dup symbol-p unless invalid-let then swap
        ( #lexenv #form @bindings #node-bindings addr #symbol #subnode )
        :: lexical-variable vr+ nil vr+ 8 pick lexenv-template vr+ nil vr+ 6 nlist push-end
        ( #lexenv #form @bindings #node-bindings addr )
        2 pick cdr 2 poke
    repeat 2drop swap drop
    ( #lexenv #form #node-bindings )
    2 pick vr+ make-lexenv over
    ( #lexenv #form #node-bindings #new-lexenv @node-bindings )
    begin dup nil <> while
        dup car 2 nth-cdr vr+ over car first vr+ swap cons
        ( #lexenv #form #node-bindings #new-lexenv @node-bindings #entry )
        2 pick lexenv-variables push cdr
    repeat drop
    ( #lexenv #form #node-bindings #new-lexenv )
    '' progn vr+ 3 pick 2 nth-cdr vr+ cons normalize make-let-node
;

: invalid-let* s" Invalid LET* form." error ;
: normalize-let* ( #lexenv #form -- #node )
    [ sf-let*-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-let* then
    dup second nil <> if
        '' let vr+ over second car vr+ 1list
        ( #lexenv #form 'let #binding )
        2 pick cdr cdr vr+ 3 pick second cdr vr+ swap cons
            '' let* vr+ swap cons 3 nlist swap vr- normalize
    else
        dup cdr cdr vr+ swap vr- '' progn vr+ swap cons normalize
    then
;

: unwind-protect-offset ( offset -- #handler #token )
    8 * rsp@ @ + box-fixnum unwind-protect-token
;

: unwind-protect-routine ( routine -- #handler #token )
    box-fixnum unwind-protect-token
;

: drop-protected ( #handler #token -- ) 2drop ;

: call-protected ( #handler #token #ret -- #ret )
    -rot unwind-protect-token <> if s" No UNWIND-PROTECT token." error then
    ( #ret #handler )
    unbox-fixnum execute
;

: nvdrop ( x_n ... x_1 n -- )
    begin dup 0> while
        over unwind-protect-token = if
            call-protected 2 -
        else swap vr- 1-
        then
    repeat drop
;

: nvreturn ( x_n ... x_1 ret n -- )
    swap >r values->r nvdrop values-r> r>
;

: compile-let ( @node -- )
    [ sf-let-tag set-compile-routine ]
    co@ over let-node-bindings @
    ( @node offset @bindings )
    begin dup nil <> while
        dup car dup third case
            :: lexical-variable of
                ( @node offset @bindings @entry )
                dup second compile
                dup 2 nth-cdr second nil <> if ' make-variable-cell c,, then
                2 nth-cdr &fourth ct@ vr+ co@ box-fixnum cons swap push
            endof
            :: dynamic-variable of unimplemented endof
            unreachable
        endcase
        ' clear-value-store c,,
        cdr
    repeat drop
    ( @node offset )
    swap let-node-body @ compile
    ( offset )
    dup co@ swap - 1- ' lit c,, c,, ' nvreturn c,,
    1+ co!
;

: progn-node-nodes 0 node-size + object-slot ;
: progn-node-last  1 node-size + object-slot ;
: make-progn-node ( #lexenv #form #nodes #last -- #node )
    2 sf-progn-tag make-node
;

: normalize-progn ( #lexenv #form -- #node )
    [ sf-progn-tag set-normalize-routine ]
    dup cdr
    ( #lexenv #form @body )
    dup nil = if
        drop nil vr+ 2 pick vr+ nil vr+ normalize
    else
        nil vr+ dsp@
        ( #lexenv #form @body #nodes addr )
        begin 2 pick cdr nil <> while
            4 pick vr+ 3 pick car vr+ normalize push-end
            2 pick cdr 2 poke
        repeat
        drop swap car vr+ 3 pick swap normalize
    then make-progn-node
;

: compile-progn ( @node -- )
    [ sf-progn-tag set-compile-routine ]
    dup progn-node-nodes @
    ( @node @nodes )
    begin dup nil <> while
        dup car compile ' vr- c,, co1-! ' clear-value-store c,,
        cdr
    repeat
    drop progn-node-last @ compile
;

: setq-node-assignments 0 node-size + object-slot ;
: make-setq-node ( #lexenv #form #assignments -- #node )
    1 sf-setq-tag make-node
;

: invalid-setq s" Invalid SETQ form." error ;
: normalize-setq ( #lexenv #form -- #node )
    [ sf-setq-tag set-normalize-routine ]
    dup cdr nil vr+ dsp@
    ( #lexenv #form @pairs #assignments addr )
    begin 2 pick nil <> while
        2 pick car dup symbol-p unless invalid-setq then vr+
            3 pick cdr dup nil = if invalid-setq then car vr+
        ( #lexenv #form @pairs #assignments addr #symbol #subform )
        over 7 pick use-symbol
        over 7 pick lexenv-variable dup cons-p if dup car :: lexical-variable = if
            ( #lexenv #form @pairs #assignments addr #symbol #subform @entry )
            7 pick lexenv-template over third = unless t vr+ swap &second v! else drop then
        else drop then else drop then
        6 pick vr+ swap normalize 2list push-end 2 pick cdr cdr 2 poke
    repeat
    rot 2drop make-setq-node
;

: compile-nil ( -- )
    ' lit c,, nil vr+ c,, ' vr+ c,, co1+!
;

: vdup  dup  vr+ ;
: vpick pick vr+ ;
: vnip  swap vr- ;
: vtuck tuck vr+ ;
: vpoke ( #x_n ... #x_1 #x_0 #v n -- #v #x_(n-1) ... #x_0 )
    2 + cells dsp@ + v!
;

: compile-setq ( @node -- )
    [ sf-setq-tag set-compile-routine ]
    dup node-lexenv @ swap setq-node-assignments @
    ( @lexenv @assignments )
    dup nil = if compile-nil 2drop exit then
    begin dup nil <> while
        dup car second compile
        dup cdr nil = if ' vdup c,, co1+! then
        dup car first 2 pick lexenv-variable
        ( @lexenv @assignments @entry )
        dup nil <> if dup first case
            :: lexical-variable of
                ct@ over fourth assocr unbox-fixnum co@ swap - 1- ' lit c,, c,,
                dup second nil = if
                    ' vpoke c,,
                else
                    ' 1+ c,, ' pick c,, ' variable-cell-ref c,, ' v! c,,
                then
            endof
            :: dynamic-variable of drop nil endof
            unreachable
        endcase then
        nil = if
            ' lit c,, dup car first vr+ c,, ' symbol-value c,, ' v! c,,
        then
        co1-!
        ' clear-value-store c,,
        cdr
    repeat 2drop
;

: make-pop-form ( #form -- #form )
    s" X" make-symbol
    ( #form #sym )
    dup vr+ nil vr+ cons
    ( #form #sym #ret )
    '' cdr vr+ 3 pick vr+ 2list '' setq vr+ 4 pick vr+ rot 3 nlist swap cons
    over vr+ '' car vr+ 4 pick vr+ 2list 2list 1list swap cons
    '' let vr+ swap cons
    -rot vr- vr-
;

: simple-expand-lambda-list ( @lambda-list @form -- #bindings )
    swap >r s" LIST" make-symbol 0 0 0 nil vr+ dsp@ r> dup >r
    ( @form #list-sym reqend optend state #bindings addr @lambda-list )
    begin dup nil <> while
        dup car case
            '' &optional of 1 3 poke endof
            '' &rest     of 2 3 poke endof
            '' &body     of 2 3 poke endof
            ( #form #list-sym reqend optend state #bindings addr @lambda-list @var )
            4 pick case
                0 of
                    6 pick 1+ 6 poke
                    5 pick 1+ 5 poke
                    dup >r vr+ 7 pick vr+ make-pop-form 2list rot swap push-end swap r>
                endof
                1 of
                    5 pick 1+ 5 poke
                    dup >r vr+ 7 pick vr+ make-pop-form 2list rot swap push-end swap r>
                endof
                2 of
                    nil 5 poke
                    dup >r vr+ 7 pick vr+ 2list rot swap push-end swap r>
                endof
                unreachable
            endcase
        endcase
        cdr
    repeat
    2drop swap drop
    ( @form #list-sym reqend optend #bindings )
    over nil <> if
        ( @form #list-sym reqend optend #bindings )
        -rot swap box-fixnum swap box-fixnum '' check-lambda-list vr+ -rot
            5 pick vr+ -rot '' quote vr+ r> vr+ 2list 5 nlist
    else
        -rot drop box-fixnum '' check-lambda-list-rest vr+ swap
            4 pick vr+ swap '' quote vr+ r> vr+ 2list 4 nlist
    then
    ( @form #list-sym #bindings #check-form )
    rot swap 2list swap cons swap drop
;

: simple-expand-macro-lambda-list ( @lambda-list @form @env -- #bindings )
    drop vr+ '' cdr vr+ swap 2list tuck simple-expand-lambda-list swap vr-
;

: expand-lambda-list       ( @lambda-list @form      -- #bindings ) simple-expand-lambda-list       ;
: expand-macro-lambda-list ( @lambda-list @form @env -- #bindings ) simple-expand-macro-lambda-list ;

: lambda-node-args-sym 0 node-size + object-slot ;
: lambda-node-body     1 node-size + object-slot ;
: lambda-node-template 2 node-size + object-slot ;
: make-lambda-node ( #lexenv #form #args-sym #body #template -- #node )
    3 sf-lambda-tag make-node
;

: function-node-arg 0 node-size + object-slot ;
: make-function-node 1 sf-function-tag make-node ;

: invalid-function s" Invalid FUNCTION form." error ;
: normalize-function ( #lexenv #form -- #node )
    [ sf-function-tag set-normalize-routine ]
    dup list-length 2 = unless invalid-function then
    dup second
    ( #lexenv #form @arg )
    dup symbol-p if
        vr+ make-function-node
    else
        dup cons-p unless invalid-function then
        dup car case
            '' lambda of
                dup list-length 2 >= unless invalid-function then
                dup second list-p    unless invalid-function then
                '' lambda vr+ over second vr+ 2list make-routine 0 make-t-vector make-function
                    s" ARGS" make-symbol
                ( #lexenv #form @arg #template #args-sym )
                4 pick vr+ make-lexenv
                ( #lexenv #form @arg #template #args-sym #body-lexenv )
                2 pick vr+ over lexenv-templates push
                over vr+ :: lexical-variable vr+ nil vr+ 5 pick vr+ nil vr+ 5 nlist
                    over lexenv-variables push
                3 pick second 2 pick expand-lambda-list 4 pick cdr cdr vr+ cons
                    '' let* vr+ swap cons normalize rot
                ( #lexenv #form @arg #args-sym #body #template )
                >r >r >r drop r> r> r> make-lambda-node
            endof
            '' setf of unimplemented endof
            invalid-function
        endcase
    then
;

: unbound-function-guard ( v -- v )
    dup unbound-marker = if s" Function is unbound." error then
;

: compile-function ( @node -- )
    [ sf-function-tag set-compile-routine ]
    function-node-arg @
    ( @form )
    dup symbol-p if
        \ TODO: check lexenv
        ' lit c,, vr+ c,, ' symbol-function c,, ' @ c,, ' unbound-function-guard c,, ' vr+ c,, co1+!
    else
        \ (setf <name>)
        unimplemented
    then
;

: compile-lambda ( @node -- )
    [ sf-lambda-tag set-compile-routine ]
    dup lambda-node-template @ function-closed @
    ( @node @closed )
    dup vector-length@ 0 do
        dup i vector@ 2 pick node-lexenv @ lexenv-variable
        ( @node @closed @entry )
        dup nil <> unless unreachable then
        dup car :: lexical-variable = unless unreachable then
        &fourth
        ( @node @closed &offsets )
        2 pick lambda-node-template @ vr+ i 1+ box-fixnum cons over push
        @ 2 pick node-lexenv @ lexenv-template swap assocr unbox-fixnum
        ( @node @closed offset )
        co@ swap - ' lit c,, c,, ' vpick c,, co1+!
    loop drop
    ( @node )
    dup lambda-node-args-sym @ over lambda-node-body @ node-lexenv @ lexenv-variable &fourth
    ( @node &offsets )
    over lambda-node-template @ vr+ dup function-closed @ vector-length@ 1+ box-fixnum cons swap push
    ( @node )
    dup lambda-node-template @ vr+ make-comp cswap
    ( @node old-comp )
    docol c,,
    over lambda-node-template @ function-closed @ vector-length@ 1+ co+!
    over lambda-node-body @ compile ' lit c,, co@ 1- c,, ' nvreturn c,, ' exit c,,
    cswap dup comp-template @ swap free
    ( @node #template )
    ' lit c,, c,, ' lit c,, dup lambda-node-template @ function-closed @ vector-length@ dup c,,
    ( @node )
    ' instantiate-template c,, co-! co1+!
    drop
;

: funcall-node-function 0 node-size + object-slot ;
: funcall-node-args     1 node-size + object-slot ;
: make-funcall-node ( #lexenv #form #function #args -- #node )
    2 sf-funcall-tag make-node
;

: invalid-funcall s" Invalid form." error ;
: normalize-funcall ( #lexenv #form -- #node )
    [ sf-funcall-tag set-normalize-routine ]
    dup car dup symbol-p unless invalid-funcall then vr+
    ( #lexenv #form #symbol )
    dup 3 pick lexenv-function dup nil <> if
        ( #lexenv #form #symbol @entry )
        dup first :: lexical-function = unless unreachable then
        swap vr- second vr+ 2 pick vr+ swap normalize
    else drop then
    ( #lexenv #form #function )
    nil vr+ dsp@ 3 pick cdr
    ( #lexenv #form #function #arg-nodes addr @arg-forms )
    begin dup nil <> while
        dup cons-p unless invalid-funcall then
        swap over car vr+ 6 pick vr+ swap normalize push-end swap
        cdr
    repeat 2drop make-funcall-node
;

: compile-funcall ( @node -- )
    [ sf-funcall-tag set-compile-routine ]
    dup funcall-node-args @ dup list-length swap
    ( @node length @args )
    begin dup nil <> while
        dup car compile ' clear-value-store c,,
        cdr
    repeat drop
    ( @node length )
    over funcall-node-function @ symbol-p if
        ' lit c,, over funcall-node-function @ vr+ c,,
        ' symbol-function c,, ' @ c,, ' unbound-function-guard c,, ' vr+ c,, co1+!
    else
        over funcall-node-function @ compile
    then
    ' lit c,, dup c,, ' nvfuncall c,, co-! drop
;

: normalize-literal ( #lexenv #form -- #node )
    [ sf-literal-tag set-normalize-routine ]
    0 sf-literal-tag make-node
;

: compile-literal ( @node -- )
    [ sf-literal-tag set-compile-routine ]
    ' lit c,, node-form @ vr+ c,, ' vr+ c,, co1+!
;

: normalize-quote ( #lexenv #form -- #node )
    [ sf-quote-tag set-normalize-routine ]
    dup list-length 2 = unless s" Invalid QUOTE form." error then
    dup second vr+ swap vr-
    0 sf-literal-tag make-node
;

: block-node-symbol 0 node-size + object-slot ;
: block-node-entry  1 node-size + object-slot ;
: block-node-body   2 node-size + object-slot ;
: make-block-node ( #lexenv #form #symbol #entry #body -- #node )
    3 sf-block-tag make-node
;

: invalid-block s" Invalid BLOCK form." error ;
: normalize-block ( #lexenv #form -- #node )
    [ sf-block-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-block then
    dup second symbol-p  unless invalid-block then
    s" LABEL" make-symbol :: lexical-variable vr+ nil vr+ 4 pick lexenv-template vr+ nil vr+ 5 nlist
    ( #lexenv #form #let-entry )
    over second vr+ swap nil vr+ swap 4 pick lexenv-template vr+ swap nil vr+ nil vr+ 6 nlist
    ( #lexenv #form #entry )
    2 pick vr+ make-lexenv
    ( #lexenv #form #entry #body-lexenv )
    over fourth vr+ over lexenv-variables push
    over vr+ over lexenv-blocks push
    swap dup cdr vr+ swap vr- swap
    2 pick 2 nth-cdr vr+ '' progn vr+ swap cons normalize
    ( #lexenv #form #entry #body )
    2 pick second vr+ -rot make-block-node
;

'' block-label make-built-in-class constant block-label-class
: block-label-stack-addr  0 object-slot ;
: block-label-return-addr 1 object-slot ;
: block-label-branch-addr 2 object-slot ;
: make-block-label ( n -- #label )
    3 block-label-class vr+ make-object
    ( n #label )
    swap 8 * rsp@ @ +
        box-fixnum over block-label-branch-addr !
    ( #label )
    rsp@ 8+ box-fixnum over block-label-return-addr !
    dsp@    box-fixnum over block-label-stack-addr  !
;

: jump-block-label ( #ret #label -- )
    dup dup block-label-stack-addr @ unbox-fixnum @ = unless
        s" Block label no longer exists." error
    then
    dup >r block-label-stack-addr @ unbox-fixnum dsp@ - 3 lsr 2 - nvreturn r>
    dup block-label-return-addr @ unbox-fixnum begin dup rsp@ > while r> vr- repeat drop
    dup block-label-branch-addr @ unbox-fixnum >r
    vr-
;

: compile-block ( @node -- )
    [ sf-block-tag set-compile-routine ]
    dup block-node-entry @ first nil <> if
        ' lit c,, ci@ 0 c,, ' make-block-label c,, co1+!
        ( @node place )
        over block-node-entry @ third &fifth ct@ vr+ co@ box-fixnum cons swap push
        over block-node-body @ compile
        dup ci@ swap - 2 - swap ci!
        ' lit c,, 1 c,, ' nvreturn c,, co1-!
    else
        co@ box-fixnum over block-node-entry @ &fourth v!
        dup block-node-body @ compile
        dup block-node-entry @ fifth
        ( @node @places )
        begin dup nil <> while
            dup car unbox-fixnum dup ci@ swap - 8 * swap ci!
            cdr
        repeat drop
    then drop
;

: return-from-node-symbol 0 node-size + object-slot ;
: return-from-node-label  1 node-size + object-slot ;
: return-from-node-node   2 node-size + object-slot ;
: make-return-from-node ( #lexenv #form #symbol #label #node -- #node )
    3 sf-return-from-tag make-node
;

: invalid-return-from s" Invalid RETURN-FROM form." error ;
: normalize-return-from ( #lexenv #form -- #node )
    [ sf-return-from-tag set-normalize-routine ]
    dup list-length 2 3 between unless invalid-return-from then
    dup second dup symbol-p  unless invalid-return-from then vr+
    ( #lexenv #form #symbol )
    dup 3 pick lexenv-block dup nil <> unless s" Label not found in scope." error then
    ( #lexenv #form #symbol @entry )
    dup second 4 pick lexenv-template = unless t vr+ over &first v! then
    third first vr+ 3 pick vr+ swap normalize
    ( #lexenv #form #symbol #label )
    3 pick vr+ 3 pick 2 nth-cdr dup nil <> if car then vr+ normalize make-return-from-node
;

: compile-return-from ( @node -- )
    [ sf-return-from-tag set-compile-routine ]
    dup return-from-node-symbol @ over node-lexenv @ lexenv-block
    ( @node @entry )
    dup first nil <> if
        over return-from-node-node @ compile
        over return-from-node-label @ compile
        ' jump-block-label c,,
    else
        co@ over fourth unbox-fixnum -
        ( @node @entry distance )
        2 pick return-from-node-node @ compile
        ' lit c,, c,, ' nvreturn c,, ' branch c,,
        ( @node @entry )
        ci@ 0 c,, box-fixnum over &fifth push
    then 2drop
    co1+!
;

: tagbody-node-entry  0 node-size + object-slot ;
: tagbody-node-nodes  1 node-size + object-slot ;
: make-tagbody-node 2 sf-tagbody-tag make-node ;

: invalid-tagbody s" Invalid TAGBODY form." error ;
: normalize-tagbody ( #lexenv #form -- #node )
    [ sf-tagbody-tag set-normalize-routine ]
    dup list-length 1 >= unless invalid-tagbody then
    s" LABEL" make-symbol :: lexical-variable vr+ nil vr+ 4 pick lexenv-template vr+ nil vr+ 5 nlist
    ( #lexenv #form #let-entry )
    nil vr+ swap 3 pick lexenv-template vr+ swap nil vr+ nil vr+ 5 nlist
    ( #lexenv #form #entry )
    2 pick vr+ make-lexenv
    ( #lexenv #form #entry #body-lexenv )
    over third vr+ over lexenv-variables push
    0 3 pick cdr
    ( #lexenv #form #entry #body-lexenv m @body )
    begin dup nil <> while
        dup car dup symbol-p if
            ( #lexenv #form #entry #body-lexenv m @body @symbol )
            vr+ 2 pick box-fixnum 5 pick vr+ cons cons 3 pick lexenv-tags push swap 1+ swap
        else drop then cdr
    repeat 2drop
    ( #lexenv #form #entry #body-lexenv )
    nil vr+ swap dsp@ 8+ 4 pick cdr swap
    ( #lexenv #form #entry #nodes #body-lexenv @body addr )
    begin over nil <> while
        over car vr+ dup symbol-p if push-end else vr+ 3 pick vr+ swap normalize push-end then
        swap cdr swap
    repeat 2drop vr-
    ( #lexenv #form #entry #nodes )
    make-tagbody-node
;

'' tagbody-label make-built-in-class constant tagbody-label-class
: tagbody-label-stack-addr  0 object-slot ;
: tagbody-label-return-addr 1 object-slot ;
: tagbody-label-branch-addr 2 + object-slot ;
: make-tagbody-label ( n_m ... n_1 m -- #label )
    dup 2 + tagbody-label-class vr+ make-object
    ( n_0 ... n_(m-1) m #label )
    begin over 0 <> while
        swap 1- swap
        rot 8 * rsp@ @ + box-fixnum over 3 pick tagbody-label-branch-addr !
    repeat swap drop
    ( #label )
    rsp@ 8+ box-fixnum over tagbody-label-return-addr !
    dsp@    box-fixnum over tagbody-label-stack-addr  !
;

: jump-tagbody-label ( #label m -- )
    swap
    ( m #label )
    dup dup tagbody-label-stack-addr @ unbox-fixnum @ = unless
        s" Tagbody label no longer exists." error
    then
    dup >r swap >r tagbody-label-stack-addr @ unbox-fixnum dsp@ - 3 lsr 1- nvdrop r> r>
    dup tagbody-label-return-addr @ unbox-fixnum begin dup rsp@ > while r> vr- repeat drop
    dup rot tagbody-label-branch-addr @ unbox-fixnum >r vr-
;

: num-symbols ( @list -- n )
    0 swap begin dup nil <> while
        ( n @list )
        dup car symbol-p if swap 1+ swap then
        cdr
    repeat drop
;

: compile-tagbody ( @node -- )
    [ sf-tagbody-tag set-compile-routine ]
    dup tagbody-node-entry @ first nil <> if
        dup tagbody-node-nodes @ num-symbols ci@
        ( @node n place )
        over 0 do ' lit c,, 0 c,, loop ' lit c,, over c,, ' make-tagbody-label c,, co1+!
        2 pick tagbody-node-entry @ third &fifth ct@ vr+ co@ box-fixnum cons swap push
        2 pick tagbody-node-nodes @ 0 swap
        ( @node n place m @nodes )
        begin dup nil <> while
            dup car dup symbol-p if
                ( @node n place m @nodes @elem )
                ( the idx to store is place+2*m+1, the offset from 'make-tagbody-label is ci-(place+2*n+3) )
                3 pick 5 pick 2 * + 3 + ci@ swap - 4 pick 4 pick 2 * + 1+ ci!
                drop swap 1+ swap
            else
                compile ' vr- c,, co1-! ' clear-value-store c,,
            then cdr
        repeat 5 ndrop
        ' vr- c,, ' nil c,, ' vr+ c,,
    else
        co@ box-fixnum over tagbody-node-entry @ &fourth v!
        dup tagbody-node-nodes @ dup num-symbols make-unsigned-byte-32-vector swap 0 swap
        ( @node #tag-idxs m @nodes )
        begin dup nil <> while
            dup car dup symbol-p if
                ( @node #tag-idxs m @nodes @elem )
                drop over ci@ box-fixnum 4 pick -rot vector! swap 1+ swap
            else
                compile ' vr- c,, co1-! ' clear-value-store c,,
            then cdr
        repeat 2drop
        ( @node #tag-idxs )
        over tagbody-node-entry @ fifth
        ( @node #tag-idxs @places )
        begin dup nil <> while
            dup car dup car unbox-fixnum swap cdr unbox-fixnum
            ( @node #tag-idxs @places m idx )
            3 pick rot vector@ unbox-fixnum over - 8 * swap ci! cdr
        repeat drop vr- drop
        ' nil c,, ' vr+ c,,
    then
;

: go-node-symbol 0 node-size + object-slot ;
: go-node-label  1 node-size + object-slot ;
: make-go-node 2 sf-go-tag make-node ;

: invalid-go s" Invalid GO form." error ;
: normalize-go ( #lexenv #form -- #node )
    [ sf-go-tag set-normalize-routine ]
    dup list-length 2 =     unless invalid-go then
    dup second dup symbol-p unless invalid-go then vr+
    ( #lexenv #form #symbol )
    dup 3 pick lexenv-tag dup nil <> unless s" Label not found in scope." error then cdr
    ( #lexenv #form #symbol @entry )
    dup second 4 pick lexenv-template = unless t vr+ over &first v! then
    third first vr+ 3 pick vr+ swap normalize
    ( #lexenv #form #symbol #label )
    make-go-node
;

: compile-go ( @node -- )
    [ sf-go-tag set-compile-routine ]
    dup go-node-symbol @ over node-lexenv @ lexenv-tag
    ( @node @entry )
    dup second nil <> if
        over go-node-label @ compile
        ' lit c,, dup first unbox-fixnum c,, ' jump-tagbody-label c,,
    else
        co@ over fifth unbox-fixnum -
        ( @node @entry distance )
        ' lit c,, c,, ' nvdrop c,, ' branch c,,
        ( @node @entry )
        dup first ci@ box-fixnum cons over &sixth push 0 c,,
    then 2drop
    co1+!
;

: unwind-protect-node-prot    0 node-size + object-slot ;
: unwind-protect-node-handler 1 node-size + object-slot ;
: make-unwind-protect-node ( #lexenv #form #prot #handler -- #node )
    2 sf-unwind-protect-tag make-node
;

: normalize-unwind-protect ( #lexenv #form -- #node )
    [ sf-unwind-protect-tag set-normalize-routine ]
    dup list-length 2 >= unless s" Invalid UNWIND-PROTECT form." error then
    over vr+ over second vr+ normalize
    ( #lexenv #form #prot )
    2 pick vr+ 2 pick 2 nth-cdr vr+ '' progn vr+ swap cons normalize
    ( #lexenv #form #prot #handler )
    make-unwind-protect-node
;

: compile-unwind-protect ( @node -- )
    [ sf-unwind-protect-tag set-compile-routine ]
    ' lit c,, ci@ 0 c,, ' unwind-protect-offset c,, 2 co+!
    ( @node idx1 )
    over unwind-protect-node-prot @ compile ' values->r c,, ' call-protected c,,
    ' branch c,, ci@ 0 c,, ci@
    ( @node idx1 idx2 docol-idx )
    rot dup -rot - 2 - swap ci!
    ( @node idx2 )
    docol c,, 2 co-!
    over unwind-protect-node-handler @ compile ' vr- c,, co1-! ' exit c,,
    ci@ over - 8 * swap ci!
    ' values-r> c,,
    ( @node )
    drop
;

: if-node-condition 0 node-size + object-slot ;
: if-node-then      1 node-size + object-slot ;
: if-node-else      2 node-size + object-slot ;
: make-if-node 3 sf-if-tag make-node ;

: normalize-if ( #lexenv #form -- #node )
    [ sf-if-tag set-normalize-routine ]
    dup list-length 3 4 between unless s" Invalid IF form." error then
    over vr+ over second vr+ normalize
    ( #lexenv #form #condition )
    2 pick vr+ 2 pick third vr+ normalize
    ( #lexenv #form #condition #then )
    3 pick vr+ 3 pick 3 nth-cdr dup nil <> if car then vr+ normalize
    ( #lexenv #form #condition #then #else )
    make-if-node
;

: compile-if ( @node -- )
    [ sf-if-tag set-compile-routine ]
    dup if-node-condition @ compile ' clear-value-store c,,
    ' dup c,, ' nil c,, ' <> c,, ' swap c,, ' vr- c,, ' 0branch c,, ci@ 0 c,, co1-!
    ( @node idx1 )
    over if-node-then @ compile ' branch c,, ci@ 0 c,,
    ( @node idx1 idx2 )
    swap ci@ over - 8 * swap ci!
    ( @node idx2 )
    over if-node-else @ compile
    ci@ over - 8 * swap ci!
    drop
;

: mv-call-node-function 0 node-size + object-slot ;
: mv-call-node-args     1 node-size + object-slot ;
: make-mv-call-node 2 sf-multiple-value-call-tag make-node ;

: normalize-mv-call ( #lexenv #form -- #node )
    [ sf-multiple-value-call-tag set-normalize-routine ]
    dup list-length 2 >= unless s" Invalid MULTIPLE-VALUE-CALL form." error then
    over vr+ over second vr+ normalize
    ( #lexenv #form #function-node )
    nil vr+ dsp@ 3 pick 2 nth-cdr
    ( #lexenv #form #function-node #nodes addr @args )
    begin dup nil <> while
        swap over car vr+ 6 pick vr+ swap normalize push-end swap
        cdr
    repeat 2drop make-mv-call-node
;

: push-multiple-values-end ( addr #ret -- addr )
    num-values @ 0<> if
        push-end
        num-values @ 1- 0 do
            i cells value-store + @ vr+ push-end
        loop
    else
        vr-
    then
;

: compile-mv-call ( @node -- )
    [ sf-multiple-value-call-tag set-compile-routine ]
    dup mv-call-node-function @ compile
    ' nil c,, ' vr+ c,, ' dsp@ c,, 2 co+!
    mv-call-node-args @
    ( @args )
    begin dup nil <> while
        dup car compile
        ' push-multiple-values-end c,, ' clear-value-store c,, co1-!
        cdr
    repeat
    ' drop c,, co1-!
    ' full-funcall c,, co1-!
    drop
;

: mv-prog1-node-values 0 node-size + object-slot ;
: mv-prog1-node-nodes  1 node-size + object-slot ;
: make-mv-prog1-node 2 sf-multiple-value-prog1-tag make-node ;

: normalize-mv-prog1 ( #lexenv #form -- #node )
    [ sf-multiple-value-prog1-tag set-normalize-routine ]
    dup list-length 2 >= unless s" Invalid MULTIPLE-VALUE-PROG1 form." error then
    over vr+ over second vr+ normalize
    ( #lexenv #form #values )
    nil vr+ dsp@ 3 pick 2 nth-cdr
    ( #lexenv #form #values #nodes addr @forms )
    begin dup nil <> while
        swap 5 pick vr+ 2 pick car vr+ normalize push-end swap
        cdr
    repeat 2drop make-mv-prog1-node
;

: compile-mv-prog1 ( @node -- )
    [ sf-multiple-value-prog1-tag set-compile-routine ]
    dup mv-prog1-node-values @ compile ' values-save c,,
    mv-prog1-node-nodes @
    ( @nodes )
    begin dup nil <> while
        dup car compile
        ' vr- c,, ' clear-value-store c,, co1-!
        cdr
    repeat drop
    ( )
    ' values-restore c,,
;

: invalid-symbol-macrolet s" Invalid SYMBOL-MACROLET form." error ;
: normalize-symbol-macrolet ( #lexenv #form -- #node )
    [ sf-symbol-macrolet-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-symbol-macrolet then
    swap make-lexenv swap dup second
    ( #lexenv #form @bindings )
    begin dup nil <> while
        dup car dup list-length 2 = unless invalid-symbol-macrolet then
        dup first symbol-p          unless invalid-symbol-macrolet then
        ( #lexenv #form @bindings @binding )
        dup first vr+ swap second vr+ :: symbol-macro vr+ swap 3 nlist
        ( #lexenv #form @bindings #entry )
        3 pick lexenv-variables push cdr
    repeat drop
    ( #lexenv #form )
    dup 2 nth-cdr vr+ swap vr- '' progn vr+ swap cons normalize
;

: progv-node-variables 0 node-size + object-slot ;
: progv-node-values    1 node-size + object-slot ;
: progv-node-body      2 node-size + object-slot ;
: make-progv-node 3 sf-progv-tag make-node ;

: normalize-progv ( #lexenv #form -- #node )
    [ sf-progv-tag set-normalize-routine ]
    dup list-length 3 >= unless s" Invalid PROGV form." error then
    over vr+ over second vr+ normalize
    ( #lexenv #form #variables )
    2 pick vr+ 2 pick third vr+ normalize
    ( #lexenv #form #variables #values )
    3 pick vr+ 3 pick 2 nth-cdr vr+ '' progn vr+ swap cons normalize
    ( #lexenv #form #variables #values #body )
    make-progv-node
;

: maybe-car ( #list ) dup cons-p if car else unbound-marker then ;
: maybe-cdr ( #list ) dup cons-p if cdr else dup nil = unless s" Not a list." error then then ;

: progv-setup ( #variables #values -- #saved )
    2dup list-p swap list-p and unless s" PROGV expected a list for variables and values." error then
    over list-length make-t-vector dup vector-data 2 pick 4 pick
    ( #variables #values #saved addr @values @variables )
    begin dup nil <> while
        2dup car vr+ dup symbol-p unless s" PROGV variable not a symbol." error then swap maybe-car vr+
        ( #variables #values #saved addr @values @variables #variable #value )
        over symbol-value @ swap 2 pick symbol-value !
        cons >r rot r> over ! 8+ -rot
        ( #variables #values #saved addr @values @variables )
        cdr swap maybe-cdr swap
    repeat 3 ndrop
    ( #variables #values #saved )
    -rot vr- vr-
;

: progv-restore ( #saved #ret -- #saved #ret )
    over dup vector-data swap vector-length@ 0 do
        ( #saved #ret addr )
        dup i cells + @ dup cdr vr+ swap car symbol-value v!
    loop drop
;

: compile-progv ( @node -- )
    [ sf-progv-tag set-compile-routine ]
    dup progv-node-variables @ compile ' clear-value-store c,,
    dup progv-node-values    @ compile ' clear-value-store c,,
    ' progv-setup c,, co1-!
    ' lit c,, ' progv-restore c,, ' unwind-protect-routine c,, 2 co+!
    dup progv-node-body @ compile
    ' call-protected c,, 2 co-! ' vnip c,, co1-!
    drop
;

: catch-node-tag  0 node-size + object-slot ;
: catch-node-body 1 node-size + object-slot ;
: make-catch-node 2 sf-catch-tag make-node ;

: normalize-catch ( #lexenv #form -- #node )
    [ sf-catch-tag set-normalize-routine ]
    dup list-length 2 >= unless s" Invalid CATCH form." error then
    over vr+ over second vr+ normalize
    ( #lexenv #form #tag )
    2 pick vr+ 2 pick 2 nth-cdr vr+ '' progn vr+ swap cons normalize
    ( #lexenv #form #tag #body )
    make-catch-node
;

: catch-offset ( offset -- branch-addr return-addr #token )
    rsp@ @ + rsp@ 8+ catch-token
;

: jump-catch ( branch-addr return-addr #token #ret -- #ret )
    swap catch-token = unless s" CATCH token is missing." error then
    ( branch-addr return-addr #ret )
    swap begin dup rsp@ > while r> vr- repeat drop
    ( branch-addr #ret )
    swap >r
;

: compile-catch ( @node -- )
    [ sf-catch-tag set-compile-routine ]
    dup catch-node-tag @ compile
    ' clear-value-store c,,
    ' lit c,, ci@ 0 c,, ' catch-offset c,, 3 co+!
    ( @node idx )
    swap catch-node-body @ compile
    ( idx )
    ' >r c,, ' drop c,, ' drop c,, ' drop c,, ' r> c,, 3 co-!
    dup ci@ swap - 2 - 8 * swap ci!
    ( )
    ' vnip c,, co1-!
;

: throw-node-tag    0 node-size + object-slot ;
: throw-node-result 1 node-size + object-slot ;
: make-throw-node 2 sf-throw-tag make-node ;

: normalize-throw ( #lexenv #form -- #node )
    [ sf-throw-tag set-normalize-routine ]
    dup list-length 3 = unless s" Invalid THROW form." error then
    over vr+ over second vr+ normalize
    ( #lexenv #form #tag )
    2 pick vr+ 2 pick third vr+ normalize
    ( #lexenv #form #tag #result )
    make-throw-node
;

: throw ( #tag #result -- <noreturn> )
    >r values->r ( #tag ) 
    begin dsp@ 8+ s0 @ < while
        over catch-token = if
            dup 5 pick = if
                vr- values-r> r> jump-catch
            then
        then
        over unwind-protect-token = if
            call-protected
        else vnip then
    repeat
    s" No CATCH tag found." error
;

: compile-throw ( @node -- )
    [ sf-throw-tag set-compile-routine ]
    dup throw-node-tag @ compile
    ' clear-value-store c,,
    throw-node-result @ compile
    ' throw c,, co1-!
;

: invalid-flet s" Invalid FLET form." error ;
: normalize-flet ( #lexenv #form -- #node )
    [ sf-flet-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-flet then
    over vr+ make-lexenv nil vr+ dsp@ 3 pick second
    ( #lexenv #form #body-lexenv #entries addr @bindings )
    begin dup nil <> while
        dup car list-length 2 >= unless invalid-flet then
        dup car first symbol-p unless invalid-flet then
        s" LABEL" make-symbol '' lambda vr+ 2 pick car cdr vr+ cons '' function vr+ swap 2list
        ( #lexenv #form #body-lexenv #entries addr @bindings #symbol #lambda-form )
        7 pick vr+ swap normalize :: lexical-variable vr+ nil vr+ 9 pick lexenv-template vr+ nil vr+ 6 nlist
        ( #lexenv #form #body-lexenv #entries addr @bindings #entry )
        over car first vr+ :: lexical-function vr+ 2 pick first vr+ 3 nlist 5 pick lexenv-functions push
        dup first vr+ over 2 nth-cdr  vr+ cons 5 pick lexenv-variables push
        rot swap push-end swap cdr
    repeat 2drop swap
    ( #lexenv #form #entries #body-lexenv )
    2 pick 2 nth-cdr vr+ '' progn vr+ swap cons normalize make-let-node
;

: invalid-labels s" Invalid LABELS form." error ;
: normalize-labels ( #lexenv #form -- #node )
    [ sf-labels-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-labels then
    over vr+ make-lexenv nil vr+ dsp@ 3 pick second
    ( #lexenv #form #body-lexenv #entries addr @bindings )
    begin dup nil <> while
        dup car list-length 2 >= unless invalid-labels then
        dup car first symbol-p unless invalid-labels then
        s" LABEL" make-symbol 6 pick vr+ nil vr+ normalize
        ( #lexenv #form #body-lexenv #entries addr @bindings #symbol #node )
        :: lexical-variable vr+ nil vr+ 9 pick lexenv-template vr+ nil vr+ 6 nlist
        ( #lexenv #form #body-lexenv #entries addr @bindings #entry )
        over car first vr+ :: lexical-function vr+ 2 pick first vr+ 3 nlist 5 pick lexenv-functions push
        dup first vr+ over 2 nth-cdr  vr+ cons 5 pick lexenv-variables push
        rot swap push-end swap cdr
    repeat 2drop swap
    ( #lexenv #form #entries #body-lexenv )
    nil vr+ dsp@ 4 pick second
    ( #lexenv #form #entries #body-lexenv #setq-pairs addr @bindings )
    begin dup nil <> while
        dup car cdr '' lambda vr+ swap cons '' function vr+ swap 2list 4 pick vr+ swap normalize
        ( #lexenv #form #entries #body-lexenv #setq-pairs addr @bindings #node )
        over car first 5 pick lexenv-function second vr+ swap 2list rot swap push-end swap cdr
    repeat 2drop
    ( #lexenv #form #entries #body-lexenv #setq-pairs )
    over vr+ swap nil vr+ swap make-setq-node 1list
    ( #lexenv #form #entries #body-lexenv #setq-nodes )
    3 pick 2 nth-cdr vr+ '' progn vr+ swap cons 2 pick vr+ swap normalize
    ( #lexenv #form #entries #body-lexenv #setq-nodes #body-node )
    nil vr+ -rot make-progn-node
    ( #lexenv #form #entries #body )
    make-let-node
;

: normalize-load-time-value ( #lexenv #form -- #node )
    [ sf-load-time-value-tag set-normalize-routine ]
    unimplemented
;

: invalid-eval-when s" Invaid EVAL-WHEN form." error ;
: normalize-eval-when ( #lexenv #form -- #node )
    [ sf-eval-when-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-eval-when then
    dup second list-p    unless invalid-eval-when then
    dup second
    ( #lexenv #form @situations )
    begin dup nil <> while
        dup car case
            :: compile-toplevel of endof
            :: load-toplevel    of endof
            :: execute          of endof
            invalid-eval-when
        endcase
        cdr
    repeat drop
    ( #lexenv #form )
    dup 2 nth-cdr vr+ swap vr- '' progn vr+ swap cons normalize
;

: strip-lexenv ( #lexenv -- #lexenv )
    make-lexenv
    nil vr+ over lexenv-templates v!
    nil vr+ over lexenv-blocks    v!
    nil vr+ over lexenv-tags      v!
    nil vr+ dsp@ 2 pick lexenv-variables @
    ( #lexenv #variables addr @variables )
    begin dup nil <> while
        dup car second :: symbol-macro = if
            swap over car push-end swap
        then cdr
    repeat 2drop over lexenv-variables v!
    ( #lexenv )
    nil vr+ dsp@ 2 pick lexenv-functions @
    ( #lexenv #functions addr @functions )
    begin dup nil <> while
        dup car second :: macro = if
            swap over car push-end swap
        then cdr
    repeat 2drop over lexenv-functions v!
;

: expand-macro-to-lambda ( @form -- #lambda-form )
    s" FORM" make-symbol s" ENV" make-symbol 2 pick 2 nth-cdr vr+
    ( @form #form-sym #env-sym #body )
    3 pick second 3 pick 3 pick expand-macro-lambda-list swap cons '' let* vr+ swap cons
    '' block vr+ 4 pick first vr+ rot 3 nlist
    -rot 2list swap 2list
    ( @form #partial-lambda-form )
    '' lambda vr+ swap cons '' function vr+ swap 2list
    \over first vr+ swap cons '' named-lambda vr+ swap cons
    swap drop
;

: eval-in-lexenv ( #v #env -- #v )
    swap ( #env #v )
    \ TODO: unwind-protect call to cswap
    nil vr+ make-routine 0 make-t-vector make-function dup vr+ make-comp cswap -rot
    ( #env #old-comp #v #template )
    docol c,, ' vr- c,, ( Delete the function arguments )
    3 pick make-lexenv swap over lexenv-templates push >r rot drop r>
    ( #old-comp #v #lexenv )
    swap normalize dup compile vr- ' exit c,, cswap
    ( comp )
    dup comp-template @ swap free dup function-closed @ vector-length@ 0<>
        if unreachable then
    ( #func )
    nil vr+ full-funcall
;

: invalid-macrolet s" Invalid MACROLET form." error ;
: normalize-macrolet ( #lexenv #form -- #node )
    [ sf-macrolet-tag set-normalize-routine ]
    dup list-length 2 >= unless invalid-macrolet then
    dup second list-p    unless invalid-macrolet then
    over vr+ make-lexenv over second
    ( #lexenv #form #body-lexenv @bindings )
    begin dup nil <> while
        dup car dup list-p   unless invalid-macrolet then
        ( #lexenv #form #body-lexenv @bindings @binding )
        dup list-length 2 >= unless invalid-macrolet then
        dup first symbol-p   unless invalid-macrolet then
        expand-macro-to-lambda 4 pick vr+ strip-lexenv eval-in-lexenv
        ( #lexenv #form #body-lexenv @bindings #function )
        :: macro vr+ swap 2list over car first vr+ swap cons 2 pick lexenv-functions push cdr
    repeat drop
    ( #lexenv #form #body-lexenv )
    swap dup 2 nth-cdr vr+ swap vr- '' progn vr+ swap cons rot vr-
    ( #body-lexenv #form )
    normalize
;

: eval ( #v -- #v )
    nil vr+ eval-in-lexenv
;

: load ( string length -- )
    make-file-stream
    ( #stream )
    begin dup read 0= while
        ( #stream @form )
        dup cl. cr
        eval vr- clear-value-store
    repeat vr-
    free-stream
;
