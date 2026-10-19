\ -*- mode: text -*-
: test ( -- )
    s" Unknown sector-32 tag" tell cr
;

0 value cl.-exec

: cl. cl.-exec execute ;

0x7ff0000000000000 constant double-float-exponent-mask
0x7ff0000000000000 constant Infinity
0xfff0000000000000 constant -Infinity
0x7ff8000000000000 constant NaN
0x000e000000000000 constant boxed-tag-mask
                49 constant boxed-tag-offset
0x0000ffff00000000 constant boxed-sector-32-tag-mask
                32 constant boxed-sector-32-tag-offset
0x00000000ffffffff constant single-float-mask
0x7ffa000000000000 constant single-float-header
0x00000000ffffffff constant character-mask
0x7ffa000100000000 constant character-header
0x0001ffffffffffff constant fixnum-mask
0x7ffc000000000000 constant fixnum-header
0x0000000000000007 constant ptr-tag-mask
0x0001fffffffffff8 constant ptr-mask
0x7ffe000000000000 constant ptr-header
0xffff000000000000 constant ptr-header-mask

0 constant infinity-tag
4 constant nan-tag
5 constant sector-32-tag
6 constant fixnum-tag
7 constant ptr-tag

0 constant single-float-tag
1 constant character-tag

0 constant null-tag
1 constant cons-tag
2 constant symbol-tag
3 constant vector-tag
4 constant function-tag
5 constant package-tag
6 constant hash-table-tag
7 constant object-tag

ptr-header null-tag or constant null-template
ptr-header cons-tag or constant cons-template
ptr-header symbol-tag or constant symbol-template
ptr-header vector-tag or constant vector-template
ptr-header function-tag or constant function-template
ptr-header package-tag or constant package-template
ptr-header hash-table-tag or constant hash-table-template
ptr-header object-tag or constant object-template

00 constant double-float-type
01 constant single-float-type
02 constant character-type
03 constant fixnum-type
04 constant null-type
05 constant cons-type
06 constant symbol-type
07 constant vector-type
08 constant function-type
09 constant package-type
10 constant hash-table-type
11 constant object-type

: type-of ( value -- type )
    dup double-float-exponent-mask dup -rot and = not if
        double-float-type
    else
        dup boxed-tag-mask and boxed-tag-offset lsr case
            infinity-tag of double-float-type endof
            nan-tag of double-float-type endof
            sector-32-tag of
                dup boxed-sector-32-tag-mask and boxed-sector-32-tag-offset lsr case
                    single-float-tag of single-float-type endof
                    character-tag of character-type endof
                    s" Unknown sector-32 tag" error
                endcase
            endof
            fixnum-tag of fixnum-type endof
            ptr-tag of
                dup ptr-tag-mask and case
                    null-tag of null-type endof
                    cons-tag of cons-type endof
                    symbol-tag of symbol-type endof
                    vector-tag of vector-type endof
                    function-tag of function-type endof
                    package-tag of package-type endof
                    hash-table-tag of hash-table-type endof
                    object-tag of object-type endof
                    s" Unknown ptr tag" error
                endcase
            endof
            s" Unknown boxed tag" error
        endcase
    then
    swap drop
;

: box-fixnum ( n -- n )
    dup cls 15 < if s" Number is too large to fit in a fixnum." error then
    fixnum-mask and fixnum-header or
;

: unbox-fixnum ( n -- n )
    fixnum-mask and 15 lsl 15 asr
;

: box-character ( c -- c )
    dup character-mask invert and 0<> if s" Character is too large to fit in a box." error then
    character-header or
;

: unbox-character ( c -- c)
    dup character-mask invert and character-header <> if s" Value is not a character." error then
    character-mask and
;

: box-single-float ( f -- f )
    single-float-mask and single-float-header or
;

: unbox-single-float ( f -- f )
    dup single-float-mask invert and character-header <> if s" Value is not a single-float." error then
    single-float-mask and
;

: box-signed-byte-8 sxtb box-fixnum ;
: box-signed-byte-16 sxth box-fixnum ;
: box-signed-byte-32 sxtw box-fixnum ;

( ** POINTERS ** )

: box-ptr ( addr ptr-tag -- v )
    over ptr-mask invert and if s" Pointer is too large or unaligned." error then
    ptr-header or or
;

: unbox-tagged-ptr ( v ptr-tag -- addr )
    over ptr-header-mask and ptr-header <> if s" Value is not a pointer" error then
    over ptr-tag-mask and <> if s" Attempting to unbox wrong type of pointer." error then
    ( v )
    ptr-mask and
;

: unbox-ptr ( v -- addr )
    dup ptr-header-mask and ptr-header <> if s" Value is not a pointer" error then
    ptr-mask and
;

: ptr-p ( v -- b )
    ptr-header-mask and ptr-header =
;

: tagged-ptr-p ( v ptr-tag -- b )
    over ptr-tag-mask and = if ptr-p
    else drop false then
;

: box-null null-tag box-ptr ;
: box-cons cons-tag box-ptr ;
: box-symbol symbol-tag box-ptr ;
: box-vector vector-tag box-ptr ;
: box-function function-tag box-ptr ;
: box-package package-tag box-ptr ;
: box-hash-table hash-table-tag box-ptr ;
: box-object object-tag box-ptr ;

: unbox-null null-tag unbox-tagged-ptr ;
: unbox-cons cons-tag unbox-tagged-ptr ;
: unbox-symbol ( sym -- addr )
    dup ptr-header-mask and ptr-header <> if s" Value is not a pointer" error then
    dup ptr-tag-mask and symbol-tag <> if
    dup ptr-tag-mask and null-tag <> if
        s" Value is not a symbol." error
    then then
    ( v )
    ptr-mask and
;
: unbox-vector vector-tag unbox-tagged-ptr ;
: unbox-function function-tag unbox-tagged-ptr ;
: unbox-package package-tag unbox-tagged-ptr ;
: unbox-hash-table hash-table-tag unbox-tagged-ptr ;
: unbox-object object-tag unbox-tagged-ptr ;

: null-p null-tag tagged-ptr-p ;
: cons-p cons-tag tagged-ptr-p ;
: symbol-p dup symbol-tag tagged-ptr-p if drop true else null-tag tagged-ptr-p then ;
: vector-p vector-tag tagged-ptr-p ;
: function-p function-tag tagged-ptr-p ;
: package-p package-tag tagged-ptr-p ;
: hash-table-p hash-table-tag tagged-ptr-p ;
: object-p object-tag tagged-ptr-p ;

' unimplemented 8 copies 8 array constant free-routines

: set-free-routine ( tag -- )
    cells free-routines + latest @ >cfa swap !
;

: vr+ ( v -- v )
    dup ptr-p if
        dup unbox-ptr incref
    else then
;

: vr- ( v -- )
    dup ptr-p if
        dup unbox-ptr dup refcount 0= if
            ( v addr )
            drop dup ptr-tag-mask and cells free-routines + @ execute
        else
            decref drop
        then
    else drop then
;

: v! ( v addr -- )
    dup @ vr- !
;

( ** VECTORS ** )

00 constant t-vector-tag ( this needs to be zero because it is special )
03 constant bit-vector-tag
04 constant base-string-tag
05 constant character-string-tag
06 constant single-float-vector-tag
08 constant unsigned-byte-8-vector-tag
09 constant unsigned-byte-16-vector-tag
10 constant unsigned-byte-32-vector-tag
12 constant signed-byte-8-vector-tag
13 constant signed-byte-16-vector-tag
14 constant signed-byte-32-vector-tag

0x000000000000ffff constant vector-tag-mask
0xffffffffffff0000 constant vector-length-mask
                16 constant vector-length-offset

: vector-data ( @v -- addr ) unbox-vector 8+ ;
: vector-length@ ( @v -- length ) unbox-vector @ vector-length-offset lsr ;
: vector-tag@ ( @v -- tag ) unbox-vector @ vector-tag-mask and ;

: vector-length1+! ( @v -- )
    unbox-vector dup @ dup vector-length-offset lsr 1+
    ( addr header new-length )
    vector-length-offset lsl swap vector-tag-mask and or
    ( addr new-header )
    swap !
 ;

64 0 0 1 8 32 32 0 8 16 32 0 8 16 32 0
16 array constant vector-element-sizes

: vector-tag-to-element-size ( vector-tag -- element-size )
    cells vector-element-sizes + @
;

: vector-element-size@ ( @v -- element-size )
    vector-tag@ vector-tag-to-element-size
;

: vector-capacity-of ( n size -- capacity )
    swap over *
    ( size bits )
    128 + log2^ expt2 128 - swap /
;

: vector-capacity@ ( @v -- capacity )
    dup vector-length@ swap vector-tag@ vector-tag-to-element-size vector-capacity-of
;

: make-vector ( size vector-tag -- #v )
    dup vector-tag-to-element-size 2 pick * 8align 3 lsr 8+ malloc
    ( size vector-tag addr )
    -rot swap vector-length-offset lsl or over !
    ( addr )
    box-vector
    dup vector-data over vector-capacity@ 2 pick vector-element-size@ * 6 lsr 0 do
        ( v addr )
        i cells over + 0 swap !
    loop drop
;

: vector-free ( @v -- )
    [ vector-tag set-free-routine ]
    dup vector-tag@ t-vector-tag = if
        dup vector-data
        ( v addr )
        over vector-length@ 0 do
            i cells over + @ vr-
        loop drop
    then
    unbox-vector
    free
;

: make-t-vector t-vector-tag make-vector ;
: make-bit-vector bit-vector-tag make-vector ;
: make-base-string base-string-tag make-vector ;
: make-character-string character-string-tag make-vector ;
: make-single-float-vector single-float-vector-tag make-vector ;
: make-unsigned-byte-8-vector unsigned-byte-8-vector-tag make-vector ;
: make-unsigned-byte-16-vector unsigned-byte-16-vector-tag make-vector ;
: make-unsigned-byte-32-vector unsigned-byte-32-vector-tag make-vector ;
: make-signed-byte-8-vector signed-byte-8-vector-tag make-vector ;
: make-signed-byte-16-vector signed-byte-16-vector-tag make-vector ;
: make-signed-byte-32-vector signed-byte-32-vector-tag make-vector ;

' nop 0 0 ' box-fixnum ' box-character ' box-character ' unimplemented 0
' box-fixnum ' box-fixnum ' box-fixnum 0
' box-signed-byte-8 ' box-signed-byte-16 ' box-signed-byte-32 0
16 array constant vector-box-routines

: vector-tag-to-box-routine ( vector-tag -- box-routine )
    cells vector-box-routines + @
;

( n -- n )
: check-bit dup clz 63 < if s" Bit check failed." error then ;
: check-unsigned-byte-8 dup clz 56 < if s" Byte(8) check failed." error then ;
: check-unsigned-byte-16 dup clz 48 < if s" Byte(16) check failed." error then ;
: check-unsigned-byte-32 dup clz 32 < if s" Byte(32) check failed." error then ;
: check-signed-byte-8 dup cls 56 < if s" SByte(8) check failed." error then ;
: check-signed-byte-16 dup cls 48 < if s" SByte(16) check failed." error then ;
: check-signed-byte-32 dup cls 32 < if s" SByte(32) check failed." error then ;

' nop 0 0 ' unbox-fixnum ' unbox-character ' unbox-character ' unimplemented 0
' unbox-fixnum ' box-fixnum ' unbox-fixnum 0
' unbox-fixnum ' box-fixnum ' unbox-fixnum 0
16 array constant vector-unbox-routines

: vector-tag-to-unbox-routine ( vector-tag -- unbox-routine )
    cells vector-unbox-routines + @
;

' nop 0 0 ' check-bit ' check-unsigned-byte-8 ' nop ' unimplemented 0
' check-unsigned-byte-8 ' check-unsigned-byte-16 ' check-unsigned-byte-32 0
' check-signed-byte-8 ' check-signed-byte-16 ' check-signed-byte-32 0
16 array constant vector-check-routines

: vector-tag-to-check-routine ( vector-tag -- check-routine )
    cells vector-check-routines + @
;

: vector@ ( @v idx -- @v[idx] )
    over vector-length@ over <= if s" Index out of bounds." error then
    over vector-tag@ dup vector-tag-to-element-size dup 1 <> if
        ( v idx tag size )
        3 lsr rot swap dup -rot * 3 pick vector-data + swap
        ( v tag addr size )
        case
            1 of c@ endof 2 of s@ endof 4 of w@ endof 8 of @ endof
            unreachable
        endcase
    else drop swap
        ( v tag idx )
        dup 3 lsr 3 pick vector-data + c@
        ( v tag idx byte )
        swap 7 and 1 swap lsl and 0<>
    then
    ( v tag partial-value )
    swap vector-tag-to-box-routine execute swap drop 
;

: bit-set ( x n bit -- x )
    over lsl 1 2 pick lsl rot drop
    ( x bit<<n 1<<n )
    invert rot and or
;

: vector! ( @v idx #val -- )
    >r
    ( v idx )
    over vector-length@ over <= if s" Index out of bounds." error then
    over vector-tag@ dup vector-tag-to-element-size dup 1 <> if
        ( v idx tag size )
        3 lsr rot swap dup -rot * 3 pick vector-data + swap r>
        ( v tag addr size val )
        3 pick vector-tag-to-unbox-routine execute
        3 pick vector-tag-to-check-routine execute
        -rot
        ( v tag val addr size )
        case
            1 of c! endof 2 of s! endof 4 of w! endof 8 of v! endof
            unreachable
        endcase
    else drop swap
        ( v tag idx )
        2 pick vector-data over 3 lsr + dup c@
        ( v tag idx addr byte )
        2 pick 7 and r> unbox-fixnum check-bit bit-set
        swap c! drop
    then
    ( v tag )
    2drop
;

: vector-push ( #v #val -- #v )
    over vector-capacity@ 2 pick vector-length@ dup rot < if
        ( v val length )
        2 pick dup vector-length1+! -rot swap vector!
    else
        dup 1+ 3 pick vector-tag@ make-vector
        ( v val length new-v )
        over 0 do
            3 pick i vector@ vr+ over i rot vector!
        loop
        ( v new-v new-v length val )
        dup -rot 2swap rot vector! swap vr-
    then
;

: nvector ( #x_1 ... #x_n n -- #v )
    dup make-t-vector dup vector-data swap >r swap
    ( #x_1 ... #x_n addr n )
    begin dup 0> while
        1- 2dup cells + >r rot r> !
    repeat 2drop r>
;

: t-vector-p ( @v -- )
    dup vector-p if
        vector-tag@ t-vector-tag =
    else drop false then
;

: vector-member ( @v @val -- bool )
    swap dup vector-length@ 1-
    ( val v idx )
    begin dup 0>= while
        2dup vector@ 3 pick = if 3 ndrop true exit then
        1-
    repeat
    3 ndrop false
;

: vector-pushnew ( #v #val -- #v )
    2dup vector-member if
        vr-
    else
        vector-push
    then
;

: cl-string ( string length -- #s )
    0 make-character-string -rot
    ( s string length )
    begin dup while
        2dup utf8-decode box-character
        ( s string length len ch )
        4 pick swap vector-push
        ( s string length len new-s )
        -rot dup -rot -
        ( s string new-s len length )
        >r >r swap r> + rot drop r>
    repeat
    2drop
;

: char-upcase ( char -- char )
    dup 'a' >= if dup 'z' <= if
        'A' 'a' - +
    then then
;

: string-upcase ( @str -- @str )
    dup vector-length@ 0 do
        dup i vector@ unbox-character char-upcase box-character
        ( @str char )
        over i rot vector!
    loop
;

( Overwritten later. )
0 value nil
1 value t

: cons ( #car #cdr -- #cons )
    16 malloc
    ( car cdr addr )
    swap over 8+ !
    ( car addr )
    swap over !
    ( addr )
    box-cons
;
: full-cons cons ;

: push ( #val addr -- )
    tuck @ cons swap !
;

: car ( @v -- @v ) unbox-cons    @ ; : full-car dup car vr+ swap vr- ;
: cdr ( @v -- @v ) unbox-cons 8+ @ ; : full-cdr dup cdr vr+ swap vr- ;

: &car ( @v -- addr ) unbox-cons ;
: &cdr ( @v -- addr ) unbox-cons 8+ ;

:  first  car ;
: &first  &car ;
:  second cdr  car ;
: &second cdr &car ;
:  third  cdr cdr  car ;
: &third  cdr cdr &car ;
:  fourth cdr cdr cdr  car ;
: &fourth cdr cdr cdr &car ;
:  fifth  cdr cdr cdr cdr  car ;
: &fifth  cdr cdr cdr cdr &car ;
:  sixth  cdr cdr cdr cdr cdr  car ;
: &sixth  cdr cdr cdr cdr cdr &car ;

: nth-cdr ( @list n -- @list )
    dup 0= if drop else swap cdr swap 1- recurse then
;

: assoc ( @key @v -- @v )
    begin dup cons-p while
        2dup car car = if
            swap drop car exit
        then
        cdr
    repeat
    2drop nil
;

: assocr ( @key @v -- @v )
    assoc dup nil <> if cdr then
;

: nlist ( #x_n ... #x_1 n -- #list )
    nil vr+
    ( x_n ... x_1 n list )
    begin over 0> while
        -rot 1- -rot swap cons
    repeat
    swap drop
;

: 1list ( #x -- #list )
    nil vr+ cons
;

: 2list ( #x #y -- #list )
    nil vr+ cons cons
;

: list-length ( @list -- n )
    dup nil = if drop 0 else cdr recurse 1+ then
;

: list-p ( @list -- b )
    dup null-p if drop true else cons-p then
;

: cons-free ( @cons -- )
    [ cons-tag set-free-routine ]
    dup car vr-
    dup cdr vr-
    unbox-cons free
;

: eq ( @x @y -- b ) = ;
: eql ( @x @y -- b ) = ;
: equal ( @x @y -- b )
    over cons-p if
        dup cons-p if
            over car over car recurse if
                over cdr over cdr recurse
                -rot 2drop exit
            then
        then
    else eql exit then
    2drop false
;
: equalp ( @x @y -- b )
    over cons-p if
        dup cons-p if
        over car over car recurse if
        over cdr over cdr recurse
        -rot 2drop exit
        then then
    else over vector-p if
        dup vector-p if
        over vector-tag@ over vector-tag@ = if
        over vector-length@ over vector-length@ over = if 0
        ( @x @y length i )
        begin 2dup > while
            3 pick over vector@ 3 pick 2 pick vector@ recurse not if
                4 ndrop false exit
            then
            1+
        repeat
        4 ndrop true exit
        then drop then then
    else over hash-table-p if
        unimplemented
    else over object-p if
        unimplemented
    else eql exit
    then then then then
    2drop false
;

' eq ' eql ' equal ' equalp 4 array constant eq-routines
: eq-routine ( tag -- routine ) cells eq-routines + @ ;

0xcbf29ce484222325 constant fnv-offset-basis
0x100000001b3 constant fnv-prime

: fnv-hash-bytes ( hash addr n -- hash )
    0 do
        ( hash addr )
        dup i + c@ rot fnv-prime * xor swap
    loop
    drop
;

: value-hash ( @v -- hash )
    dsp@ fnv-offset-basis swap 8 fnv-hash-bytes swap drop
;

0 constant eq-hash-tag
1 constant eql-hash-tag
2 constant equal-hash-tag
3 constant equalp-hash-tag

' value-hash 12 copies 12 array constant eq-hash-routines
' value-hash 12 copies 12 array constant eql-hash-routines
' value-hash 12 copies 12 array constant equal-hash-routines
' value-hash 12 copies 12 array constant equalp-hash-routines

eq-hash-routines eql-hash-routines equal-hash-routines equalp-hash-routines 4 array constant hash-routines

: hash-routines-addr ( eq-tag -- routines )
    cells hash-routines + @
;

: set-hash-routine ( type eq-tag -- )
    hash-routines-addr swap cells + latest @ >cfa swap !
;

: hash ( @v eq-tag -- hash )
    hash-routines-addr
    ( v routines )
    swap dup type-of cells rot + @ execute
;

: eq-hash eq-hash-tag hash ;
: eql-hash eql-hash-tag hash ;
: equal-hash equal-hash-tag hash ;
: equalp-hash equalp-hash-tag hash ;

: cons-equal-hash  ( @cons -- hash )
    [ cons-type equal-hash-tag set-hash-routine ]
    fnv-offset-basis >r
    cons-tag dsp@ r> swap 1 fnv-hash-bytes >r drop
    dup car equal-hash dsp@ r> swap 8 fnv-hash-bytes >r drop
    dup cdr equal-hash dsp@ r> swap 8 fnv-hash-bytes >r drop
    drop r>
;

: cons-equalp-hash  ( @cons -- hash )
    [ cons-type equalp-hash-tag set-hash-routine ]
    fnv-offset-basis >r
    cons-tag dsp@ r> swap 1 fnv-hash-bytes >r drop
    dup car equalp-hash dsp@ r> swap 8 fnv-hash-bytes >r drop
    dup cdr equalp-hash dsp@ r> swap 8 fnv-hash-bytes >r drop
    drop r>
;

: vector-equalp-hash ( @v -- hash )
    [ vector-type equalp-hash-tag set-hash-routine ]
    fnv-offset-basis >r
    vector-tag dsp@ r> swap 1 fnv-hash-bytes >r drop
    dup vector-tag@ dsp@ r> swap 1 fnv-hash-bytes >r
    ( v tag )
    ?dup if
        vector-tag-to-element-size over vector-length@ * 8align 3 lsr
        swap vector-data swap r> -rot fnv-hash-bytes
    else
        vector-length@ swap vector-data swap 0 do
            dup i cells + @ equalp-hash dsp@ r> swap 8 fnv-hash-bytes >r drop
        loop
        drop r>
    then
;

: hash-table-equalp-hash ( @v -- hash )
    [ hash-table-type equalp-hash-tag set-hash-routine ]
    unimplemented
;

: object-equalp-hash ( @v -- hash )
    [ object-type equalp-hash-tag set-hash-routine ]
    ( Need equalp hash for structs, specifically. )
    unimplemented
;
2 itof constant default-rehash-size
1 itof 2 itof f/ constant default-rehash-threshold

: make-hash-table ( tag -- #ht )
    3 cells malloc dup box-hash-table swap rot
    ( ht addr tag )
    48 lsl over ! 8+
    ( ht addr )
    default-rehash-size over w! 4 +
    default-rehash-threshold over w! 4 +
    5 64 vector-capacity-of make-t-vector over ! 8+
    drop
;

: make-eq-hash-table eq-hash-tag make-hash-table ;
: make-eql-hash-table eql-hash-tag make-hash-table ;
: make-equal-hash-table equal-hash-tag make-hash-table ;
: make-equalp-hash-table equalp-hash-tag make-hash-table ;

: hash-table-tag@ ( @ht -- eq-tag )
    unbox-hash-table 6 + s@
;

: hash-table-count@ ( @ht -- n )
    unbox-hash-table @ -1 16 lsr and
;

: hash-table-count1+! ( @ht -- )
    unbox-hash-table dup @
    ( addr cell )
    dup -1 16 lsr and 1+ swap -1 48 lsl and or
    swap !
;

: hash-table-count1-! ( @ht -- )
    unbox-hash-table dup @
    ( addr cell )
    dup -1 16 lsr and 1+ swap -1 48 lsl and or
    swap !
;

: hash-table-rehash-size@ ( @ht -- f )
    unbox-hash-table 8+ w@
;

: hash-table-rehash-threshold@ ( @ht -- f )
    unbox-hash-table 12 + w@
;

: hash-table-data ( @ht -- addr ) unbox-hash-table 16 + ;

: hash-table-size@ ( @ht -- n )
    hash-table-data @ vector-length@
;

: hash-table-make-pair ( #key #value next -- #addr )
    3 cells malloc 16 +
    ( key value next addr )
    dup 8- -rot !
    ( key value addr )
    dup 8- -rot !
    ( key addr )
    dup -rot !
;

: hash-table-pair-key ( @addr -- &key ) ;
: hash-table-pair-value ( @addr -- &value ) 8+ ;
: hash-table-pair-next ( @addr -- &next ) 16 + ;

: hash-table-bucket-addr ( @key tag @vec -- addr )
    -rot hash over vector-length@ mod
    ( vec idx )
    cells swap vector-data +
;

: hash-table-get-addr ( @key tag @vec -- addr )
    3 ndup hash-table-bucket-addr
    ( @key tag @vec addr )
    begin dup @ ?dup while
        ( key tag vec addr pair )
        dup hash-table-pair-key @ 5 pick 5 pick eq-routine execute if
            drop >r 3 ndrop r>
            exit
        then
        hash-table-pair-next swap drop
    repeat
    4 ndrop 0
;

: hash-table-insert ( #key #value tag @vec -- b )
    3 pick -rot 3 ndup hash-table-get-addr
    ( #key #value @key tag @vec addr )
    ?dup if
        >r 3 ndrop r> @
        ( #key #value @pair )
        swap over hash-table-pair-value v!
        ( #key @pair )
        hash-table-pair-key v!
        ( )
        true
    else
        ( #key #value @key tag @vec )
        hash-table-bucket-addr -rot 2 pick @ hash-table-make-pair swap !
        false
    then
;

: hash-table-remove ( @key tag @vec -- b )
    hash-table-get-addr ?dup if
        ( addr )
        dup @ tuck hash-table-pair-next @ swap ! free true
    then
    false
;

: hash-table-maybe-rehash ( @ht -- )
    dup hash-table-count@ itof over hash-table-size@ itof f/
            over hash-table-rehash-threshold@ f> if
        dup hash-table-size@ itof over hash-table-rehash-size@ f* ftoi
        ( ht size )
        64 vector-capacity-of make-t-vector over hash-table-data @
        ( ht new-data old-data )
        over 3 pick hash-table-data ! rot hash-table-tag@ -rot
        ( tag new-data old-data )
        dup vector-length@ 0 do
            dup i vector@ begin ?dup while
                ( tag new-data old-data pair )
                dup hash-table-pair-key @ over hash-table-pair-value @
                ( tag new-data old-data pair key value )
                5 pick 5 pick hash-table-insert drop
                ( tag new-data old-data pair )
                dup hash-table-pair-next @ swap free
            repeat
        loop
        unbox-vector free 2drop
    else drop
    then
;

: box-boolean ( b -- @b )
    if t else nil then
;

: unbox-boolean ( @b -- b )
    nil = if false else true then
;

: hash-table-to-tag-vec ( @ht -- tag @vec )
    dup hash-table-tag@ swap hash-table-data @
;

: gethash ( @key @ht -- @value @found )
    hash-table-to-tag-vec hash-table-get-addr
    ?dup if
        @ hash-table-pair-value @ t
    else
        nil nil
    then
;

: sethash ( #value #key @ht -- @found )
    -rot swap 2 pick
    ( @ht #key #value @ht )
    hash-table-to-tag-vec hash-table-insert
    ( @ht b )
    tuck unless
        ( b @ht )
        dup hash-table-count1+! hash-table-maybe-rehash
    else drop
    then
    ( b )
    box-boolean
;

: remhash ( @key @ht -- @found )
    dup -rot hash-table-to-tag-vec hash-table-remove
    ( @ht b )
    swap over if hash-table-count1-! then
    box-boolean
;

: hash-table-free ( @ht )
    [ hash-table-tag set-free-routine ]
    dup hash-table-data @
    ( ht data )
    dup vector-length@ 0 do
        dup i vector@ begin ?dup while
            ( ht data pair )
            dup hash-table-pair-key @ vr-
            dup hash-table-pair-value @ vr-
            dup hash-table-pair-next @ swap free
        repeat
    loop
    unbox-vector free
    ( ht )
    unbox-hash-table free
;

( TODO: make new sector for special values like this, instead of using NaN-sector )
( This value should never make it into the lisp runtime )
NaN 1 or constant unbound-marker
NaN 2 or constant unwind-protect-token
NaN 3 or constant catch-token

: symbol-name     ( @sym -- &name     ) unbox-symbol ;
: symbol-package  ( @sym -- &package  ) unbox-symbol 8+ ;
: symbol-value    ( @sym -- &value    ) unbox-symbol 16 + ;
: symbol-function ( @sym -- &function ) unbox-symbol 24 + ;
: symbol-plist    ( @sym -- &plist    ) unbox-symbol 32 + ;

: cl-make-symbol ( #name -- #sym )
    5 cells malloc swap over ! dup 8+
    ( #sym addr )
    nil vr+ over ! 8+
    unbound-marker over ! 8+
    unbound-marker over ! 8+
    nil vr+ over ! 8+
    drop box-symbol
;

: make-symbol ( string length -- #sym )
    cl-string cl-make-symbol
;

: symbol-free ( @sym -- )
    [ symbol-tag set-free-routine null-tag set-free-routine ]
    dup symbol-name     @ vr-
    dup symbol-package  @ vr-
    dup symbol-value    @ vr-
    dup symbol-function @ vr-
    dup symbol-plist    @ vr-
    unbox-symbol free
;

s" NIL" make-symbol ptr-tag-mask invert and null-tag or to nil
nil vr+ nil symbol-package !
nil vr+ nil symbol-plist !
s" T" make-symbol to t

make-equalp-hash-table constant packages
make-equalp-hash-table constant nicknamed-packages

: package-name              ( @package -- addr ) unbox-package ;
: package-nicknames         ( @package -- addr ) unbox-package 8+ ;
: package-use-list          ( @package -- addr ) unbox-package 16 + ;
: package-used-by-list      ( @package -- addr ) unbox-package 24 + ;
: package-symbols           ( @package -- addr ) unbox-package 32 + ;
: package-shadowing-symbols ( @package -- addr ) unbox-package 40 + ;

: cl-find-package ( @name -- @package )
    dup packages gethash drop dup nil = if drop nicknamed-packages gethash drop else nip then
;
: cl-make-package ( #name -- @package )
    6 cells malloc swap over ! dup 8+
    ( addr addr )
    nil vr+ over ! 8+
    nil vr+ over ! 8+
    nil vr+ over ! 8+
    make-equalp-hash-table over ! 8+
    nil vr+ over ! 8+
    drop box-package
    vr+ dup dup package-name @ vr+ packages sethash drop
;

: make-package ( string length -- @package ) cl-string cl-make-package ;

: use-package ( @use-package @package -- )
    2dup vr+ package-use-list push
    swap vr+ package-used-by-list push
;

: nickname-package ( #nickname @package -- )
    over vr+ over vr+ swap nicknamed-packages sethash drop
    package-nicknames push
;

s" COMMON-LISP" make-package vr+ constant cl-package
s" CL" cl-string cl-package nickname-package

s" COMMON-LISP-USER" make-package vr+ constant cl-user-package
s" CL-USER" cl-string cl-user-package nickname-package
cl-package cl-user-package use-package

s" FR-INT" make-package vr+ constant fr-int-package
cl-package fr-int-package use-package

s" KEYWORD" make-package vr+ constant keyword-package

: find-symbol-in-package ( @str @package -- @sym @external-p @found-p )
    package-symbols @ gethash nil <> if
        ( cons )
        dup car swap cdr t
    else
        nil nil
    then
;

: intern-symbol-in-package ( #sym @package -- )
    tuck vr+ over symbol-package v! dup symbol-name @ swap
    ( @package @str #sym )
    2 pick keyword-package eq
    ( @package @str #sym keyword-p )
    dup if
        over vr+ dup symbol-value v!
    then
    box-boolean vr+ over swap cons >r -rot r> swap vr+ rot
    ( @sym #cons #str @package )
    package-symbols @ sethash 2drop
;

: intern-in-package ( #str @package -- @sym )
    swap cl-make-symbol dup rot intern-symbol-in-package
;

nil vr+ cl-package intern-symbol-in-package
t   vr+ cl-package intern-symbol-in-package


: :: immediate ( -- @sym )
    word cl-string string-upcase
    ( #str )
    dup keyword-package find-symbol-in-package nil <> if
        ( #str @sym @external-p )
        drop swap vr-
    else
        2drop keyword-package intern-in-package
    then
    ( @sym )
    state @ if
        ' lit , vr+ ,
    then
;

: find-symbol-inherited ( @str @package -- @sym @status )
    package-use-list @
    ( @str @list )
    begin dup nil <> while
        2dup car find-symbol-in-package nil <> if
            drop -rot 2drop :: INHERITED exit
        else 2drop then
        cdr
    repeat
    2drop nil nil
;

: cl-find-symbol ( @str @package -- @sym @status )
    2dup find-symbol-in-package nil <> if
        ( @str @package @sym @external-p )
        nil = if :: INTERNAL else :: EXTERNAL then
        >r >r 2drop r> r>
    else 2drop find-symbol-inherited then
;

: find-symbol ( string length @package -- @sym @status )
    -rot cl-string dup rot cl-find-symbol
    ( #str @sym @status )
    rot vr-
;

: cl-intern ( #str @package -- @sym @status )
    2dup cl-find-symbol dup nil = if
        ( #str @package @sym @status )
        2drop intern-in-package nil
    else
        >r >r drop vr- r> r>
    then
;

: intern ( string length @package -- @sym @status )
    -rot cl-string swap cl-intern
;

: export ( @sym @package -- )
    over symbol-name @ over cl-find-symbol nil <> if 2 pick = else drop false then
        unless s" Symbol not found in package." error then
    over symbol-name @ over package-symbols @ gethash nil <> if
        ( @sym @package @pair )
        t vr+ swap &cdr v!
    else drop
        ( @sym @package )
        package-symbols @ over symbol-name @ vr+ swap
        ( @sym #name @symbols )
        rot vr+ t vr+ cons -rot sethash drop
    then
    
;

s" *PACKAGE*" cl-package intern drop vr+ constant *package*
fr-int-package vr+ *package* symbol-value v!

: '' immediate ( -- @sym )
    word cl-string string-upcase *package* symbol-value @ cl-intern drop
    ( @sym )
    state @ if
        ' lit , vr+ ,
    then
;

: slurp-cl-symbols ( -- )
    begin word over @ s" --------" drop @ <> while
        cl-package intern drop cl-package export
    repeat
    2drop
; slurp-cl-symbols

&ALLOW-OTHER-KEYS                      &AUX                                    &BODY                                   &ENVIRONMENT
&KEY                                   &OPTIONAL                               &REST                                   &WHOLE
*                                      **                                      ***
*BREAK-ON-SIGNALS*                     *COMPILE-FILE-PATHNAME*                 *COMPILE-FILE-TRUENAME*                 *COMPILE-PRINT*
*COMPILE-VERBOSE*                      *DEBUGGER-HOOK*                         *DEBUG-IO*                              *DEFAULT-PATHNAME-DEFAULTS*
*ERROR-OUTPUT*                         *FEATURES*                              *GENSYM-COUNTER*                        *LOAD-PATHNAME*
*LOAD-PRINT*                           *LOAD-TRUENAME*                         *LOAD-VERBOSE*                          *MACROEXPAND-HOOK*
*MODULES*                              *PACKAGE*                               *PRINT-ARRAY*                           *PRINT-BASE*
*PRINT-CASE*                           *PRINT-CIRCLE*                          *PRINT-ESCAPE*                          *PRINT-GENSYM*
*PRINT-LENGTH*                         *PRINT-LEVEL*                           *PRINT-LINES*                           *PRINT-MISER-WIDTH*
*PRINT-PPRINT-DISPATCH*                *PRINT-PRETTY*                          *PRINT-RADIX*                           *PRINT-READABLY*
*PRINT-RIGHT-MARGIN*                   *QUERY-IO*                              *RANDOM-STATE*                          *READ-BASE*
*READ-DEFAULT-FLOAT-FORMAT*            *READ-EVAL*                             *READ-SUPPRESS*                         *READTABLE*
*STANDARD-INPUT*                       *STANDARD-OUTPUT*                       *TERMINAL-IO*                           *TRACE-OUTPUT*
+                                      ++                                      +++                                     -
/                                      //                                      ///                                     /=
<                                      <=                                      =                                       >
>=                                     1+                                      1-                                      ABORT
ABS                                    ACONS                                   ACOS                                    ACOSH
ADD-METHOD                             ADJOIN                                  ADJUSTABLE-ARRAY-P                      ADJUST-ARRAY
ALLOCATE-INSTANCE                      ALPHA-CHAR-P                            ALPHANUMERICP                           AND
APPEND                                 APPLY                                   APROPOS                                 APROPOS-LIST
AREF                                   ARITHMETIC-ERROR                        ARITHMETIC-ERROR-OPERANDS               ARITHMETIC-ERROR-OPERATION
ARRAY                                  ARRAY-DIMENSION                         ARRAY-DIMENSION-LIMIT                   ARRAY-DIMENSIONS
ARRAY-DISPLACEMENT                     ARRAY-ELEMENT-TYPE                      ARRAY-HAS-FILL-POINTER-P                ARRAY-IN-BOUNDS-P
ARRAYP                                 ARRAY-RANK                              ARRAY-RANK-LIMIT                        ARRAY-ROW-MAJOR-INDEX
ARRAY-TOTAL-SIZE                       ARRAY-TOTAL-SIZE-LIMIT                  ASH                                     ASIN
ASINH                                  ASSERT                                  ASSOC                                   ASSOC-IF
ASSOC-IF-NOT                           ATAN                                    ATANH                                   ATOM
BASE-CHAR                              BASE-STRING                             BIGNUM                                  BIT
BIT-AND                                BIT-ANDC1                               BIT-ANDC2                               BIT-EQV
BIT-IOR                                BIT-NAND                                BIT-NOR                                 BIT-NOT
BIT-ORC1                               BIT-ORC2                                BIT-VECTOR                              BIT-VECTOR-P
BIT-XOR                                BLOCK                                   BOOLE                                   BOOLE-1
BOOLE-2                                BOOLEAN                                 BOOLE-AND                               BOOLE-ANDC1
BOOLE-ANDC2                            BOOLE-C1                                BOOLE-C2                                BOOLE-CLR
BOOLE-EQV                              BOOLE-IOR                               BOOLE-NAND                              BOOLE-NOR
BOOLE-ORC1                             BOOLE-ORC2                              BOOLE-SET                               BOOLE-XOR
BOTH-CASE-P                            BOUNDP                                  BREAK                                   BROADCAST-STREAM
BROADCAST-STREAM-STREAMS               BUILT-IN-CLASS                          BUTLAST                                 BYTE
BYTE-POSITION                          BYTE-SIZE                               CAAAAR                                  CAAADR
CAAAR                                  CAADAR                                  CAADDR                                  CAADR
CAAR                                   CADAAR                                  CADADR                                  CADAR
CADDAR                                 CADDDR                                  CADDR                                   CADR
CALL-ARGUMENTS-LIMIT                   CALL-METHOD                             CALL-NEXT-METHOD                        CAR
CASE                                   CATCH                                   CCASE                                   CDAAAR
CDAADR                                 CDAAR                                   CDADAR                                  CDADDR
CDADR                                  CDAR                                    CDDAAR                                  CDDADR
CDDAR                                  CDDDAR                                  CDDDDR                                  CDDDR
CDDR                                   CDR                                     CEILING                                 CELL-ERROR
CELL-ERROR-NAME                        CERROR                                  CHANGE-CLASS                            CHAR
CHAR/=                                 CHAR<                                   CHAR<=                                  CHAR=
CHAR>                                  CHAR>=                                  CHARACTER                               CHARACTERP
CHAR-CODE                              CHAR-CODE-LIMIT                         CHAR-DOWNCASE                           CHAR-EQUAL
CHAR-GREATERP                          CHAR-INT                                CHAR-LESSP                              CHAR-NAME
CHAR-NOT-EQUAL                         CHAR-NOT-GREATERP                       CHAR-NOT-LESSP                          CHAR-UPCASE
CHECK-TYPE                             CIS                                     CLASS                                   CLASS-NAME
CLASS-OF                               CLEAR-INPUT                             CLEAR-OUTPUT                            CLOSE
CLRHASH                                CODE-CHAR                               COERCE                                  COMPILATION-SPEED
COMPILE                                COMPILED-FUNCTION                       COMPILED-FUNCTION-P                     COMPILE-FILE
COMPILE-FILE-PATHNAME                  COMPILER-MACRO                          COMPILER-MACRO-FUNCTION                 COMPLEMENT
COMPLEX                                COMPLEXP                                COMPUTE-APPLICABLE-METHODS              COMPUTE-RESTARTS
CONCATENATE                            CONCATENATED-STREAM                     CONCATENATED-STREAM-STREAMS             COND
CONDITION                              CONJUGATE                               CONS                                    CONSP
CONSTANTLY                             CONSTANTP                               CONTINUE                                CONTROL-ERROR
COPY-ALIST                             COPY-LIST                               COPY-PPRINT-DISPATCH                    COPY-READTABLE
COPY-SEQ                               COPY-STRUCTURE                          COPY-SYMBOL                             COPY-TREE
COS                                    COSH                                    COUNT                                   COUNT-IF
COUNT-IF-NOT                           CTYPECASE                               DEBUG                                   DECF
DECLAIM                                DECLARATION                             DECLARE                                 DECODE-FLOAT
DECODE-UNIVERSAL-TIME                  DEFCLASS                                DEFCONSTANT                             DEFGENERIC
DEFINE-COMPILER-MACRO                  DEFINE-CONDITION                        DEFINE-METHOD-COMBINATION               DEFINE-MODIFY-MACRO
DEFINE-SETF-EXPANDER                   DEFINE-SYMBOL-MACRO                     DEFMACRO                                DEFMETHOD
DEFPACKAGE                             DEFPARAMETER                            DEFSETF                                 DEFSTRUCT
DEFTYPE                                DEFUN                                   DEFVAR                                  DELETE
DELETE-DUPLICATES                      DELETE-FILE                             DELETE-IF                               DELETE-IF-NOT
DELETE-PACKAGE                         DENOMINATOR                             DEPOSIT-FIELD                           DESCRIBE
DESCRIBE-OBJECT                        DESTRUCTURING-BIND                      DIGIT-CHAR                              DIGIT-CHAR-P
DIRECTORY                              DIRECTORY-NAMESTRING                    DISASSEMBLE                             DIVISION-BY-ZERO
DO                                     DO*                                     DO-ALL-SYMBOLS                          DOCUMENTATION
DO-EXTERNAL-SYMBOLS                    DOLIST                                  DO-SYMBOLS                              DOTIMES
DOUBLE-FLOAT                           DOUBLE-FLOAT-EPSILON                    DOUBLE-FLOAT-NEGATIVE-EPSILON           DPB
DRIBBLE                                DYNAMIC-EXTENT                          ECASE                                   ECHO-STREAM
ECHO-STREAM-INPUT-STREAM               ECHO-STREAM-OUTPUT-STREAM               ED                                      EIGHTH
ELT                                    ENCODE-UNIVERSAL-TIME                   END-OF-FILE                             ENDP
ENOUGH-NAMESTRING                      ENSURE-DIRECTORIES-EXIST                ENSURE-GENERIC-FUNCTION                 EQ
EQL                                    EQUAL                                   EQUALP                                  ERROR
ETYPECASE                              EVAL                                    EVAL-WHEN                               EVENP
EVERY                                  EXP                                     EXPORT                                  EXPT
EXTENDED-CHAR                          FBOUNDP                                 FCEILING                                FDEFINITION
FFLOOR                                 FIFTH                                   FILE-AUTHOR                             FILE-ERROR
FILE-ERROR-PATHNAME                    FILE-LENGTH                             FILE-NAMESTRING                         FILE-POSITION
FILE-STREAM                            FILE-STRING-LENGTH                      FILE-WRITE-DATE                         FILL
FILL-POINTER                           FIND                                    FIND-ALL-SYMBOLS                        FIND-CLASS
FIND-IF                                FIND-IF-NOT                             FIND-METHOD                             FIND-PACKAGE
FIND-RESTART                           FIND-SYMBOL                             FINISH-OUTPUT                           FIRST
FIXNUM                                 FLET                                    FLOAT                                   FLOAT-DIGITS
FLOATING-POINT-INEXACT                 FLOATING-POINT-INVALID-OPERATION        FLOATING-POINT-OVERFLOW                 FLOATING-POINT-UNDERFLOW
FLOATP                                 FLOAT-PRECISION                         FLOAT-RADIX                             FLOAT-SIGN
FLOOR                                  FMAKUNBOUND                             FORCE-OUTPUT                            FORMAT
FORMATTER                              FOURTH                                  FRESH-LINE                              FROUND
FTRUNCATE                              FTYPE                                   FUNCALL                                 FUNCTION
FUNCTION-KEYWORDS                      FUNCTION-LAMBDA-EXPRESSION              FUNCTIONP                               GCD
GENERIC-FUNCTION                       GENSYM                                  GENTEMP                                 GET
GET-DECODED-TIME                       GET-DISPATCH-MACRO-CHARACTER            GETF                                    GETHASH
GET-INTERNAL-REAL-TIME                 GET-INTERNAL-RUN-TIME                   GET-MACRO-CHARACTER                     GET-OUTPUT-STREAM-STRING
GET-PROPERTIES                         GET-SETF-EXPANSION                      GET-UNIVERSAL-TIME                      GO
GRAPHIC-CHAR-P                         HANDLER-BIND                            HANDLER-CASE                            HASH-TABLE
HASH-TABLE-COUNT                       HASH-TABLE-P                            HASH-TABLE-REHASH-SIZE                  HASH-TABLE-REHASH-THRESHOLD
HASH-TABLE-SIZE                        HASH-TABLE-TEST                         HOST-NAMESTRING                         IDENTITY
IF                                     IGNORABLE                               IGNORE                                  IGNORE-ERRORS
IMAGPART                               IMPORT                                  INCF                                    INITIALIZE-INSTANCE
INLINE                                 IN-PACKAGE                              INPUT-STREAM-P                          INSPECT
INTEGER                                INTEGER-DECODE-FLOAT                    INTEGER-LENGTH                          INTEGERP
INTERACTIVE-STREAM-P                   INTERN                                  INTERNAL-TIME-UNITS-PER-SECOND          INTERSECTION
INVALID-METHOD-ERROR                   INVOKE-DEBUGGER                         INVOKE-RESTART                          INVOKE-RESTART-INTERACTIVELY
ISQRT                                  KEYWORD                                 KEYWORDP                                LABELS
LAMBDA                                 LAMBDA-LIST-KEYWORDS                    LAMBDA-PARAMETERS-LIMIT                 LAST
LCM                                    LDB                                     LDB-TEST                                LDIFF
LEAST-NEGATIVE-DOUBLE-FLOAT            LEAST-NEGATIVE-LONG-FLOAT               LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT  LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT  LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT  LEAST-NEGATIVE-SHORT-FLOAT              LEAST-NEGATIVE-SINGLE-FLOAT
LEAST-POSITIVE-DOUBLE-FLOAT            LEAST-POSITIVE-LONG-FLOAT               LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT  LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT  LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT  LEAST-POSITIVE-SHORT-FLOAT              LEAST-POSITIVE-SINGLE-FLOAT
LENGTH                                 LET                                     LET*                                    LISP-IMPLEMENTATION-TYPE
LISP-IMPLEMENTATION-VERSION            LIST                                    LIST*                                   LIST-ALL-PACKAGES
LISTEN                                 LIST-LENGTH                             LISTP                                   LOAD
LOAD-LOGICAL-PATHNAME-TRANSLATIONS     LOAD-TIME-VALUE                         LOCALLY                                 LOG
LOGAND                                 LOGANDC1                                LOGANDC2                                LOGBITP
LOGCOUNT                               LOGEQV                                  LOGICAL-PATHNAME                        LOGICAL-PATHNAME-TRANSLATIONS
LOGIOR                                 LOGNAND                                 LOGNOR                                  LOGNOT
LOGORC1                                LOGORC2                                 LOGTEST                                 LOGXOR
LONG-FLOAT                             LONG-FLOAT-EPSILON                      LONG-FLOAT-NEGATIVE-EPSILON             LONG-SITE-NAME
LOOP                                   LOOP-FINISH                             LOWER-CASE-P                            MACHINE-INSTANCE
MACHINE-TYPE                           MACHINE-VERSION                         MACROEXPAND                             MACROEXPAND-1
MACRO-FUNCTION                         MACROLET                                MAKE-ARRAY                              MAKE-BROADCAST-STREAM
MAKE-CONCATENATED-STREAM               MAKE-CONDITION                          MAKE-DISPATCH-MACRO-CHARACTER           MAKE-ECHO-STREAM
MAKE-HASH-TABLE                        MAKE-INSTANCE                           MAKE-INSTANCES-OBSOLETE                 MAKE-LIST
MAKE-LOAD-FORM                         MAKE-LOAD-FORM-SAVING-SLOTS             MAKE-METHOD                             MAKE-PACKAGE
MAKE-PATHNAME                          MAKE-RANDOM-STATE                       MAKE-SEQUENCE                           MAKE-STRING
MAKE-STRING-INPUT-STREAM               MAKE-STRING-OUTPUT-STREAM               MAKE-SYMBOL                             MAKE-SYNONYM-STREAM
MAKE-TWO-WAY-STREAM                    MAKUNBOUND                              MAP                                     MAPC
MAPCAN                                 MAPCAR                                  MAPCON                                  MAPHASH
MAP-INTO                               MAPL                                    MAPLIST                                 MASK-FIELD
MAX                                    MEMBER                                  MEMBER-IF                               MEMBER-IF-NOT
MERGE                                  MERGE-PATHNAMES                         METHOD                                  METHOD-COMBINATION
METHOD-COMBINATION-ERROR               METHOD-QUALIFIERS                       MIN                                     MINUSP
MISMATCH                               MOD                                     MOST-NEGATIVE-DOUBLE-FLOAT              MOST-NEGATIVE-FIXNUM
MOST-NEGATIVE-LONG-FLOAT               MOST-NEGATIVE-SHORT-FLOAT               MOST-NEGATIVE-SINGLE-FLOAT              MOST-POSITIVE-DOUBLE-FLOAT
MOST-POSITIVE-FIXNUM                   MOST-POSITIVE-LONG-FLOAT                MOST-POSITIVE-SHORT-FLOAT               MOST-POSITIVE-SINGLE-FLOAT
MUFFLE-WARNING                         MULTIPLE-VALUE-BIND                     MULTIPLE-VALUE-CALL                     MULTIPLE-VALUE-LIST
MULTIPLE-VALUE-PROG1                   MULTIPLE-VALUE-SETQ                     MULTIPLE-VALUES-LIMIT                   NAME-CHAR
NAMESTRING                             NBUTLAST                                NCONC                                   NEXT-METHOD-P
NIL                                    NINTERSECTION                           NINTH                                   NO-APPLICABLE-METHOD
NO-NEXT-METHOD                         NOT                                     NOTANY                                  NOTEVERY
NOTINLINE                              NRECONC                                 NREVERSE                                NSET-DIFFERENCE
NSET-EXCLUSIVE-OR                      NSTRING-CAPITALIZE                      NSTRING-DOWNCASE                        NSTRING-UPCASE
NSUBLIS                                NSUBST                                  NSUBST-IF                               NSUBST-IF-NOT
NSUBSTITUTE                            NSUBSTITUTE-IF                          NSUBSTITUTE-IF-NOT                      NTH
NTHCDR                                 NTH-VALUE                               NULL                                    NUMBER
NUMBERP                                NUMERATOR                               NUNION                                  ODDP
OPEN                                   OPEN-STREAM-P                           OPTIMIZE                                OR
OTHERWISE                              OUTPUT-STREAM-P                         PACKAGE                                 PACKAGE-ERROR
PACKAGE-ERROR-PACKAGE                  PACKAGE-NAME                            PACKAGE-NICKNAMES                       PACKAGEP
PACKAGE-SHADOWING-SYMBOLS              PACKAGE-USED-BY-LIST                    PACKAGE-USE-LIST                        PAIRLIS
PARSE-ERROR                            PARSE-INTEGER                           PARSE-NAMESTRING                        PATHNAME
PATHNAME-DEVICE                        PATHNAME-DIRECTORY                      PATHNAME-HOST                           PATHNAME-MATCH-P
PATHNAME-NAME                          PATHNAMEP                               PATHNAME-TYPE                           PATHNAME-VERSION
PEEK-CHAR                              PHASE                                   PI                                      PLUSP
POP                                    POSITION                                POSITION-IF                             POSITION-IF-NOT
PPRINT                                 PPRINT-DISPATCH                         PPRINT-EXIT-IF-LIST-EXHAUSTED           PPRINT-FILL
PPRINT-INDENT                          PPRINT-LINEAR                           PPRINT-LOGICAL-BLOCK                    PPRINT-NEWLINE
PPRINT-POP                             PPRINT-TAB                              PPRINT-TABULAR                          PRIN1
PRIN1-TO-STRING                        PRINC                                   PRINC-TO-STRING                         PRINT
PRINT-NOT-READABLE                     PRINT-NOT-READABLE-OBJECT               PRINT-OBJECT                            PRINT-UNREADABLE-OBJECT
PROBE-FILE                             PROCLAIM                                PROG                                    PROG*
PROG1                                  PROG2                                   PROGN                                   PROGRAM-ERROR
PROGV                                  PROVIDE                                 PSETF                                   PSETQ
PUSH                                   PUSHNEW                                 QUOTE                                   RANDOM
RANDOM-STATE                           RANDOM-STATE-P                          RASSOC                                  RASSOC-IF
RASSOC-IF-NOT                          RATIO                                   RATIONAL                                RATIONALIZE
RATIONALP                              READ                                    READ-BYTE                               READ-CHAR
READ-CHAR-NO-HANG                      READ-DELIMITED-LIST                     READER-ERROR                            READ-FROM-STRING
READ-LINE                              READ-PRESERVING-WHITESPACE              READ-SEQUENCE                           READTABLE
READTABLE-CASE                         READTABLEP                              REAL                                    REALP
REALPART                               REDUCE                                  REINITIALIZE-INSTANCE                   REM
REMF                                   REMHASH                                 REMOVE                                  REMOVE-DUPLICATES
REMOVE-IF                              REMOVE-IF-NOT                           REMOVE-METHOD                           REMPROP
RENAME-FILE                            RENAME-PACKAGE                          REPLACE                                 REQUIRE
REST                                   RESTART                                 RESTART-BIND                            RESTART-CASE
RESTART-NAME                           RETURN                                  RETURN-FROM                             REVAPPEND
REVERSE                                ROOM                                    ROTATEF                                 ROUND
ROW-MAJOR-AREF                         RPLACA                                  RPLACD                                  SAFETY
SATISFIES                              SBIT                                    SCALE-FLOAT                             SCHAR
SEARCH                                 SECOND                                  SEQUENCE                                SERIOUS-CONDITION
SET                                    SET-DIFFERENCE                          SET-DISPATCH-MACRO-CHARACTER            SET-EXCLUSIVE-OR
SETF                                   SET-MACRO-CHARACTER                     SET-PPRINT-DISPATCH                     SETQ
SET-SYNTAX-FROM-CHAR                   SEVENTH                                 SHADOW                                  SHADOWING-IMPORT
SHARED-INITIALIZE                      SHIFTF                                  SHORT-FLOAT                             SHORT-FLOAT-EPSILON
SHORT-FLOAT-NEGATIVE-EPSILON           SHORT-SITE-NAME                         SIGNAL                                  SIGNED-BYTE
SIGNUM                                 SIMPLE-ARRAY                            SIMPLE-BASE-STRING                      SIMPLE-BIT-VECTOR
SIMPLE-BIT-VECTOR-P                    SIMPLE-CONDITION                        SIMPLE-CONDITION-FORMAT-ARGUMENTS       SIMPLE-CONDITION-FORMAT-CONTROL
SIMPLE-ERROR                           SIMPLE-STRING                           SIMPLE-STRING-P                         SIMPLE-TYPE-ERROR
SIMPLE-VECTOR                          SIMPLE-VECTOR-P                         SIMPLE-WARNING                          SIN
SINGLE-FLOAT                           SINGLE-FLOAT-EPSILON                    SINGLE-FLOAT-NEGATIVE-EPSILON           SINH
SIXTH                                  SLEEP                                   SLOT-BOUNDP                             SLOT-EXISTS-P
SLOT-MAKUNBOUND                        SLOT-MISSING                            SLOT-UNBOUND                            SLOT-VALUE
SOFTWARE-TYPE                          SOFTWARE-VERSION                        SOME                                    SORT
SPACE                                  SPECIAL                                 SPECIAL-OPERATOR-P                      SPEED
SQRT                                   STABLE-SORT                             STANDARD                                STANDARD-CHAR
STANDARD-CHAR-P                        STANDARD-CLASS                          STANDARD-GENERIC-FUNCTION               STANDARD-METHOD
STANDARD-OBJECT                        STEP                                    STORAGE-CONDITION                       STORE-VALUE
STREAM                                 STREAM-ELEMENT-TYPE                     STREAM-ERROR                            STREAM-ERROR-STREAM
STREAM-EXTERNAL-FORMAT                 STREAMP                                 STRING                                  STRING/=
STRING<                                STRING<=                                STRING=                                 STRING>
STRING>=                               STRING-CAPITALIZE                       STRING-DOWNCASE                         STRING-EQUAL
STRING-GREATERP                        STRING-LEFT-TRIM                        STRING-LESSP                            STRING-NOT-EQUAL
STRING-NOT-GREATERP                    STRING-NOT-LESSP                        STRINGP                                 STRING-RIGHT-TRIM
STRING-STREAM                          STRING-TRIM                             STRING-UPCASE                           STRUCTURE
STRUCTURE-CLASS                        STRUCTURE-OBJECT                        STYLE-WARNING                           SUBLIS
SUBSEQ                                 SUBSETP                                 SUBST                                   SUBST-IF
SUBST-IF-NOT                           SUBSTITUTE                              SUBSTITUTE-IF                           SUBSTITUTE-IF-NOT
SUBTYPEP                               SVREF                                   SXHASH                                  SYMBOL
SYMBOL-FUNCTION                        SYMBOL-MACROLET                         SYMBOL-NAME                             SYMBOLP
SYMBOL-PACKAGE                         SYMBOL-PLIST                            SYMBOL-VALUE                            SYNONYM-STREAM
SYNONYM-STREAM-SYMBOL                  T                                       TAGBODY                                 TAILP
TAN                                    TANH                                    TENTH                                   TERPRI
THE                                    THIRD                                   THROW                                   TIME
TRACE                                  TRANSLATE-LOGICAL-PATHNAME              TRANSLATE-PATHNAME                      TREE-EQUAL
TRUENAME                               TRUNCATE                                TWO-WAY-STREAM                          TWO-WAY-STREAM-INPUT-STREAM
TWO-WAY-STREAM-OUTPUT-STREAM           TYPE                                    TYPECASE                                TYPE-ERROR
TYPE-ERROR-DATUM                       TYPE-ERROR-EXPECTED-TYPE                TYPE-OF                                 TYPEP
UNBOUND-SLOT                           UNBOUND-SLOT-INSTANCE                   UNBOUND-VARIABLE                        UNDEFINED-FUNCTION
UNEXPORT                               UNINTERN                                UNION                                   UNLESS
UNREAD-CHAR                            UNSIGNED-BYTE                           UNTRACE                                 UNUSE-PACKAGE
UNWIND-PROTECT                         UPDATE-INSTANCE-FOR-DIFFERENT-CLASS     UPDATE-INSTANCE-FOR-REDEFINED-CLASS     UPGRADED-ARRAY-ELEMENT-TYPE
UPGRADED-COMPLEX-PART-TYPE             UPPER-CASE-P                            USE-PACKAGE                             USER-HOMEDIR-PATHNAME
USE-VALUE                              VALUES                                  VALUES-LIST                             VARIABLE
VECTOR                                 VECTORP                                 VECTOR-POP                              VECTOR-PUSH
VECTOR-PUSH-EXTEND                     WARN                                    WARNING                                 WHEN
WILD-PATHNAME-P                        WITH-ACCESSORS                          WITH-COMPILATION-UNIT                   WITH-CONDITION-RESTARTS
WITH-HASH-TABLE-ITERATOR               WITH-INPUT-FROM-STRING                  WITH-OPEN-FILE                          WITH-OPEN-STREAM
WITH-OUTPUT-TO-STRING                  WITH-PACKAGE-ITERATOR                   WITH-SIMPLE-RESTART                     WITH-SLOTS
WITH-STANDARD-IO-SYNTAX                WRITE                                   WRITE-BYTE                              WRITE-CHAR
WRITE-LINE                             WRITE-SEQUENCE                          WRITE-STRING                            WRITE-TO-STRING
YES-OR-NO-P                            Y-OR-N-P                                ZEROP                                   --------


( routine: |mem-header|length|...codewords| )
: routine-length   ( routine -- &length ) ;
: routine-data     ( routine -- &data[] ) 8+ ;
: routine-capacity ( routine -- n ) 8- block-size@ 16 - 3 lsr ;
: routine-elem    ( routine i -- &codeword ) cells swap routine-data + ;
1 constant routine-header-size

: make-routine ( -- routine )
    1 cells malloc 0 over !
;

: routine-decref ( routine -- )
    dup decref
    dup refcount 0< if
        dup routine-data over routine-length @ 0 do
            ( routine data )
            dup i cells + @ vr-
        loop drop
        free
    else drop then
;

1 value exit-count
: exit-after ( n -- ) exit-count <= if 1 os-exit then 1 +to exit-count ;

: ,, ( routine val -- routine )
    over dup routine-length @ swap routine-capacity < if
        over dup routine-length @ cells swap routine-data + !
        ( routine )
        dup routine-length 1 swap +!
    else
        over routine-length @ dup 1+ routine-header-size + cells malloc
        ( routine val length new-routine )
        swap over routine-length ! rot
        ( val new-routine routine )
        dup routine-length @ 0 do
            2dup i routine-elem @ swap i routine-elem !
        loop
        routine-decref swap recurse
    then
;

: function-name    ( func -- &name    ) unbox-function ;
: function-routine ( func -- &routine ) unbox-function 8+ ;
: function-closed  ( func -- &closed  ) unbox-function 16 + ;

: make-function ( #name #routine #closed -- #func )
    3 cells malloc box-function
    swap over function-closed !
        swap over function-routine !
        swap over function-name    !
;

: instantiate-template ( #x_n ... #x_0 @template n -- #func )
    swap >r nvector r>
    ( #closed @template )
    dup function-name @ vr+ over function-routine @ dup incref rot drop rot make-function
;

: free-function ( func -- )
    [ function-tag set-free-routine ]
    dup function-name @ vr-
    dup function-routine @ routine-decref
    dup function-closed @ vr-
    drop
;

: check-nargs ( @args n -- @args )
    over list-length = unless s" Incorrect number of arguments." error then
;

: check-nargs-rest ( @args n -- @args )
    over list-length <= unless s" Not enough arguments." error then
;

: wrap-function-raw ( #name routine n rest-p -- #func )
    \ The routine passed to this function should never be malloc'd because it won't be freed
    >r make-routine
    ( name routine n wrapper )
    docol ,, ' lit ,, over ,, r> dup >r if ' check-nargs-rest else ' check-nargs then ,,
    swap 0 do
        ( name routine wrapper )
        ' dup ,, ' car ,, ' vr+ ,, ' swap ,,
        ' dup ,, ' cdr ,, ' vr+ ,, ' swap ,,
        ' vr- ,,
    loop
    r> unless ' vr- ,, then
    swap ,,
    ' exit ,,
    ( name wrapper )
    0 make-t-vector make-function
;

: wrap-function      false wrap-function-raw ;
: wrap-function-rest true  wrap-function-raw ;

: funcall ( #args @func -- #v )
    dup -rot >r >r function-closed @
    ( closed )
    dup vector-data swap vector-length@ 0 do
        ( ...closed addr )
        dup i cells + @ vr+ swap
    loop
    drop r> r> function-routine @ routine-data execute
;
: full-funcall ( #func #args -- #v ) over funcall swap vr- ;

: nvfuncall ( #x_n ... #x_1 #func n -- #v )
    swap >r nlist r> swap full-funcall
;

: add-symbol-template ( @template #symbol -- )
    swap function-closed dup @ rot vector-pushnew swap !
;

0x0000ffffffffffff constant object-class-mask
                48 constant object-length-offset

: object-length@ ( @obj -- length ) unbox-object @ object-length-offset lsr ;
: object-class@ ( @obj -- @class ) unbox-object @ object-class-mask and box-object ;
: object-length! ( length @obj -- )
    unbox-object swap object-length-offset lsl
    ( obj length<<48 )
    over @ object-class-mask and or swap !
;
\ WARNING: does not decref previous class
: object-class! ( #class @obj -- )
    unbox-object swap unbox-object
    ( obj class )
    over @ object-class-mask invert and or swap !
;
: object-slots ( @obj   -- addr ) unbox-object 8+ ;
: object-slot  ( @obj i -- addr ) cells swap object-slots + ;

: make-object ( n #class -- #obj )
    over 1+ cells malloc box-object
    ( n #class #obj )
    dup -rot object-class! dup -rot object-length!
;

: object-free ( @obj -- )
    [ object-tag set-free-routine ]
    dup object-class@ vr-
    dup object-slots
    ( @obj addr )
    over object-length@ 0 do
        i cells over + @ vr-
    loop drop
    unbox-object
    free
;

6 constant class-size
8 constant slot-class-size

: class-name ( @class -- &name ) 0 object-slot ;

0 box-object constant null-class
slot-class-size null-class make-object constant standard-class
standard-class vr+ dup object-class!
'' standard-class vr+ standard-class class-name !

: make-built-in-class ( @name -- #class )
    vr+ class-size standard-class vr+ make-object
    ( @name #class )
    dup class-name rot swap !
;

'' sharpsign-equal-wrapper make-built-in-class constant sharpsign-equal-wrapper-class

: make-sharpsign-equal-wrapper ( -- #obj )
    1 sharpsign-equal-wrapper-class vr+ make-object
    dup 0 object-slot unbound-marker swap !
;

: sharpsign-equal-wrapper-value ( @obj -- &value ) 0 object-slot ;
: sharpsign-equal-wrapper-p ( @obj -- bool )
    dup object-p if
        object-class@ sharpsign-equal-wrapper-class =
    else drop false then
;

: emit-string ( @str -- )
    dup vector-length@ 0 do
        dup i vector@ emit
    loop
    drop
;

: cl-print ( @x -- )
    dup type-of case
        double-float-type of
            s" #{" tell . s" }" tell
        endof
        single-float-type of unimplemented endof
        character-type of
            s" #\\" tell unbox-character emit
        endof
        fixnum-type of
            base @ swap 10 base ! unbox-fixnum . base !
        endof
        null-type of
            symbol-name @ emit-string
        endof
        cons-type of
            s" (" tell
            dup car recurse cdr
            begin dup cons-p while
                dup car recurse cdr
            repeat
            dup nil = if
                drop
            else
                s" . " tell recurse
            then
            s" ) " tell
        endof
        symbol-type of
            dup symbol-package @ keyword-package = if ':' emit then
            symbol-name @ emit-string
        endof
        vector-type of
            dup vector-tag@ character-string-tag = if
                '"' emit emit-string '"' emit
            else
                s" #(" tell
                dup vector-length@ 0 do
                    dup i vector@ recurse
                loop
                drop s" )" tell
            then
        endof
        function-type of s" #<FUNCTION " tell function-name @ recurse s" >" tell endof
        package-type of s" #<PACKAGE " tell unbox-ptr . s" > " tell endof
        hash-table-type of unimplemented endof
        object-type of
            s" #<" tell dup object-class@ class-name @ recurse s"  {" tell unbox-ptr . s" }> " tell
        endof
    endcase
;

' cl-print to cl.-exec

