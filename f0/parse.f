\ -*- mode: text -*-

0 constant string-stream
1 constant file-stream

: stream-type ;
: stream-next-char 8+ ;

: string-stream-addr   16 + ;
: string-stream-length 24 + ;
32 constant string-stream-size

: file-stream-fd       16 + ;
: file-stream-position 24 + ;
: file-stream-length   32 + ;
: file-stream-buffer   40 + ;
40  constant file-stream-size
128 constant file-stream-buffer-size

: make-file-stream ( string length -- #stream )
    drop O_RDONLY 0 open file-stream-size file-stream-buffer-size + malloc
    ( fd #stream )
    swap        over file-stream-fd       !
    0           over file-stream-position !
    0           over file-stream-length   !
    -1          over stream-next-char     !
    file-stream over stream-type          !
;

: make-string-stream ( string length -- #stream )
    string-stream-size malloc
        swap over string-stream-length !
        swap over string-stream-addr   !
    ( #stream )
    -1            over stream-next-char !
    string-stream over stream-type      !
;

: free-stream ( #stream -- )
    dup stream-type @ file-stream = if
        dup file-stream-fd @ close
    then
    free
;

: read-char ( @stream -- char eof )
    dup stream-next-char @ dup -1 <> if
        ( @stream char )
        swap stream-next-char -1 swap ! false exit
    else drop then
    ( @stream )
    dup stream-type @ case
        string-stream of
            dup string-stream-length @ 0= if
                drop 0 true
            else
                ( stream addr length )
                dup string-stream-addr @ over string-stream-length @ 2dup utf8-decode >r
                ( @stream addr length len )
                dup -rot - -rot + swap
                ( @stream addr length )
                rot dup -rot string-stream-length ! string-stream-addr !
                (  )
                r> false
            then
        endof
        file-stream of
            dup file-stream-length @ 0= if
                dup file-stream-fd @ over file-stream-buffer file-stream-buffer-size read
                ( @stream n )
                dup 0= if 2drop 0 true exit then
                over file-stream-length   !
                0 over    file-stream-position !
                ( @stream )
            then
            \ TODO: handle case where utf8 multi-byte char is on a read-boundary
            dup file-stream-buffer over file-stream-position @ + over file-stream-length @ utf8-decode >r
            ( @stream len )
            2dup swap file-stream-length   -!
                 swap file-stream-position +!
            ( )
            r> false
        endof
        unreachable
    endcase
;

: unread-char ( char @stream -- )
    dup stream-next-char @ -1 <> if s" Can not unread multiple characters at once." error then
    stream-next-char !
;

: skip-char ( @stream -- ) read-char 2drop ;

0 constant constituent-type
1 constant whitespace-type
2 constant terminating-macro-type
3 constant non-terminating-macro-type
4 constant single-escape-type
5 constant multiple-escape-type

0 constant mc-"
1 constant mc-#
2 constant mc-'
3 constant mc-(
4 constant mc-)
5 constant mc-,
6 constant mc-;
7 constant mc-`
8 constant mc-num

128 constant num-base-chars
3 cells constant char-info-size
num-base-chars char-info-size * allot constant char-info-start
char-info-start value current-char-info

' unimplemented mc-num copies mc-num array constant macro-char-routines
: macro-char-routine ( type -- routine ) cells macro-char-routines + @ ;
: set-macro-char-routine ( type -- )
    cells macro-char-routines + latest @ >cfa swap !
;

: char-info-type ( info -- &type ) ;
: char-info-name ( info -- &name ) 8+ ;
: char-info-data ( info -- &data ) 16 + ;
: char-info-char ( info -- char ) char-info-start - char-info-size / ;

: char-info ( n -- &info ) char-info-size * char-info-start + ;
: char-type ( n -- type ) char-info @ ;
: char-data ( n -- data ) char-info 16 + @ ;

make-equalp-hash-table constant char-names

: store-char-info ( string length data type -- )
    current-char-info ! current-char-info 16 + !
    ( string length )
    2dup cl-string string-upcase current-char-info swap char-names sethash drop
    cl-string current-char-info char-info-name !
    ( )
    char-info-size +to current-char-info
;

: make-char-alias ( string1 length1 string2 length2 -- )
    cl-string string-upcase dup char-names gethash nil = if s" Invalid character name." error then swap vr-
    ( string length addr )
    -rot cl-string string-upcase char-names sethash drop
;

: store-c-char ( string length -- ) 0 constituent-type store-char-info ;
: store-w-char ( string length -- ) 0 whitespace-type store-char-info ;
: store-t-char ( string length type -- ) terminating-macro-type store-char-info ;
: store-nt-char ( string length type -- ) non-terminating-macro-type store-char-info ;
: store-se-char ( string length -- ) 0 single-escape-type store-char-info ;
: store-me-char ( string length -- ) 0 multiple-escape-type store-char-info ;

s" Nul"                     store-c-char s" Soh"                     store-c-char s" Stx"                     store-c-char s" Etx"                     store-c-char
s" Eot"                     store-c-char s" Enq"                     store-c-char s" Ack"                     store-c-char s" Bel"                     store-c-char
s" Backspace"               store-c-char s" Tab"                     store-w-char s" Newline"                 store-w-char s" Vt"                      store-c-char
s" Page"                    store-w-char s" Return"                  store-w-char s" So"                      store-c-char s" Si"                      store-c-char
s" Dle"                     store-c-char s" Dc1"                     store-c-char s" Dc2"                     store-c-char s" Dc3"                     store-c-char
s" Dc4"                     store-c-char s" Nak"                     store-c-char s" Syn"                     store-c-char s" Etb"                     store-c-char
s" Can"                     store-c-char s" Em"                      store-c-char s" Sub"                     store-c-char s" Esc"                     store-c-char
s" Fs"                      store-c-char s" Gs"                      store-c-char s" Rs"                      store-c-char s" Us"                      store-c-char
s" Space"                   store-w-char s" EXCLAMATION_MARK"        store-c-char s" QUOTATION_MARK"    mc-"  store-t-char s" NUMBER_SIGN"       mc-# store-nt-char
s" DOLLAR_SIGN"             store-c-char s" PERCENT_SIGN"            store-c-char s" AMPERSAND"               store-c-char s" APOSTROPHE"        mc-'  store-t-char
s" LEFT_PARENTHESIS"  mc-(  store-t-char s" RIGHT_PARENTHESIS" mc-)  store-t-char s" ASTERISK"                store-c-char s" PLUS_SIGN"               store-c-char
s" COMMA"             mc-,  store-t-char s" HYPHEN-MINUS"            store-c-char s" FULL_STOP"               store-c-char s" SLASH"                   store-c-char
s" DIGIT_ZERO"              store-c-char s" DIGIT_ONE"               store-c-char s" DIGIT_TWO"               store-c-char s" DIGIT_THREE"             store-c-char
s" DIGIT_FOUR"              store-c-char s" DIGIT_FIVE"              store-c-char s" DIGIT_SIX"               store-c-char s" DIGIT_SEVEN"             store-c-char
s" DIGIT_EIGHT"             store-c-char s" DIGIT_NINE"              store-c-char s" COLON"                   store-c-char s" SEMICOLON"         mc-;  store-t-char
s" LESS-THAN_SIGN"          store-c-char s" EQUAL_SIGN"              store-c-char s" GREATER-THAN_SIGN"       store-c-char s" QUESTION_MARK"           store-c-char
s" AT_SIGN"                 store-c-char s" LATIN_CAPITAL_LETTER_A"  store-c-char s" LATIN_CAPITAL_LETTER_B"  store-c-char s" LATIN_CAPITAL_LETTER_C"  store-c-char
s" LATIN_CAPITAL_LETTER_D"  store-c-char s" LATIN_CAPITAL_LETTER_E"  store-c-char s" LATIN_CAPITAL_LETTER_F"  store-c-char s" LATIN_CAPITAL_LETTER_G"  store-c-char
s" LATIN_CAPITAL_LETTER_H"  store-c-char s" LATIN_CAPITAL_LETTER_I"  store-c-char s" LATIN_CAPITAL_LETTER_J"  store-c-char s" LATIN_CAPITAL_LETTER_K"  store-c-char
s" LATIN_CAPITAL_LETTER_L"  store-c-char s" LATIN_CAPITAL_LETTER_M"  store-c-char s" LATIN_CAPITAL_LETTER_N"  store-c-char s" LATIN_CAPITAL_LETTER_O"  store-c-char
s" LATIN_CAPITAL_LETTER_P"  store-c-char s" LATIN_CAPITAL_LETTER_Q"  store-c-char s" LATIN_CAPITAL_LETTER_R"  store-c-char s" LATIN_CAPITAL_LETTER_S"  store-c-char
s" LATIN_CAPITAL_LETTER_T"  store-c-char s" LATIN_CAPITAL_LETTER_U"  store-c-char s" LATIN_CAPITAL_LETTER_V"  store-c-char s" LATIN_CAPITAL_LETTER_W"  store-c-char
s" LATIN_CAPITAL_LETTER_X"  store-c-char s" LATIN_CAPITAL_LETTER_Y"  store-c-char s" LATIN_CAPITAL_LETTER_Z"  store-c-char s" LEFT_SQUARE_BRACKET"     store-c-char
s" BACKSLASH"              store-se-char s" RIGHT_SQUARE_BRACKET"    store-c-char s" CIRCUMFLEX_ACCENT"       store-c-char s" LOW_LINE"                store-c-char
s" GRAVE_ACCENT"      mc-`  store-t-char s" LATIN_SMALL_LETTER_A"    store-c-char s" LATIN_SMALL_LETTER_B"    store-c-char s" LATIN_SMALL_LETTER_C"    store-c-char
s" LATIN_SMALL_LETTER_D"    store-c-char s" LATIN_SMALL_LETTER_E"    store-c-char s" LATIN_SMALL_LETTER_F"    store-c-char s" LATIN_SMALL_LETTER_G"    store-c-char
s" LATIN_SMALL_LETTER_H"    store-c-char s" LATIN_SMALL_LETTER_I"    store-c-char s" LATIN_SMALL_LETTER_J"    store-c-char s" LATIN_SMALL_LETTER_K"    store-c-char
s" LATIN_SMALL_LETTER_L"    store-c-char s" LATIN_SMALL_LETTER_M"    store-c-char s" LATIN_SMALL_LETTER_N"    store-c-char s" LATIN_SMALL_LETTER_O"    store-c-char
s" LATIN_SMALL_LETTER_P"    store-c-char s" LATIN_SMALL_LETTER_Q"    store-c-char s" LATIN_SMALL_LETTER_R"    store-c-char s" LATIN_SMALL_LETTER_S"    store-c-char
s" LATIN_SMALL_LETTER_T"    store-c-char s" LATIN_SMALL_LETTER_U"    store-c-char s" LATIN_SMALL_LETTER_V"    store-c-char s" LATIN_SMALL_LETTER_W"    store-c-char
s" LATIN_SMALL_LETTER_X"    store-c-char s" LATIN_SMALL_LETTER_Y"    store-c-char s" LATIN_SMALL_LETTER_Z"    store-c-char s" LEFT_CURLY_BRACKET"      store-c-char
s" VERTICAL_BAR"           store-me-char s" RIGHT_CURLY_BRACKET"     store-c-char s" TILDE"                   store-c-char s" Delete"                  store-c-char

s" Bs"       s" Backspace" make-char-alias
s" Ht"       s" Tab"       make-char-alias
s" Lf"       s" Newline"   make-char-alias
s" Ff"       s" Page"      make-char-alias
s" Cr"       s" Return"    make-char-alias
s" Del"      s" Delete"    make-char-alias
s" Linefeed" s" Newline"   make-char-alias
s" Rubout"   s" Delete"    make-char-alias

: read-char-strict ( routine -- char )
    read-char if s" End of file." error then
;

: read-multiple-escape ( str routine -- str )
    swap
    ( routine str )
    begin 
        over read-char-strict
        ( routine str char )
        dup char-type multiple-escape-type <>
    while
        dup char-type single-escape-type = if
            drop over read-char-strict
        then
        box-character vector-push
    repeat
    rot 2drop
;

: read-token ( read-char -- token escape-p )
    false 0 make-character-string
    ( read-char escape-p token )
    begin
        2 pick read-char if
            drop rot drop swap exit
        then
        ( read-char escape-p token char )
        dup char-type case
            constituent-type of
                char-upcase box-character vector-push
            endof
            whitespace-type of
                3 pick unread-char rot drop swap exit
            endof
            terminating-macro-type of
                3 pick unread-char rot drop swap exit
            endof
            non-terminating-macro-type of
                char-upcase box-character vector-push
            endof
            single-escape-type of
                drop 2 pick read-char-strict box-character vector-push
                swap drop true swap
            endof
            multiple-escape-type of
                drop 2 pick read-multiple-escape
                swap drop true swap
            endof
        endcase
    again
;

0 constant  regular-token
1 constant  keyword-token
2 constant external-token
3 constant internal-token

: invalid-token s" Invalid token." error ;

: get-package-part ( @token -- #package-token )
    0 make-character-string
    ( @token #package-token )
    begin 2dup vector-length@ swap vector-length@ < if
        2dup vector-length@ vector@ unbox-character ':' <>
    else false then while
        2dup vector-length@ vector@ vector-push
    repeat
    nip
;

: get-symbol-part  ( #token start -- #token )
    0 make-character-string
    ( #token idx #new-token )
    begin over 3 pick vector-length@ < while
        2 pick 2 pick vector@
        ( #token idx #new-token char )
        dup unbox-character ':' = if invalid-token then vector-push swap 1+ swap
    repeat
    dup vector-length@ 0= if invalid-token then
    swap drop swap vr-
;

: count-package-markers ( @token start -- n )
    tuck
    ( start @token idx )
    begin 2dup swap vector-length@ < if
        2dup vector@ unbox-character ':' =
    else false then while 1+ repeat
    swap drop swap -
;

: split-token-package-marker ( #token -- #package-token #token type )
    dup vector-length@ 0= if unreachable then
    dup get-package-part
    ( #token #package-token )
    dup vector-length@ 0= if
        over 0 count-package-markers case
            1 of swap 1 get-symbol-part keyword-token endof
            invalid-token
        endcase
    else over vector-length@ over vector-length@ = if
        vr- 0 make-character-string swap regular-token
    else
        over over vector-length@ count-package-markers
        ( #token #package-token n )
        dup case
            1 of external-token endof
            2 of internal-token endof
            invalid-token
        endcase >r
        ( #token #package-token n )
        over vector-length@ + rot swap get-symbol-part r>
    then then
;

: convert-token ( #token escape-p -- #v )
    \ TODO: numeric syntax
    drop
    ( #token )
    split-token-package-marker case
        regular-token of
            ( #package-token #token )
            swap vr- *package* symbol-value @ cl-intern drop vr+
        endof
        keyword-token of
            swap vr- keyword-package cl-intern drop vr+
        endof
        external-token of
            2dup swap cl-find-package dup nil <> unless s" Package not found." error then
            ( #package-token #token @token @package )
            cl-find-symbol :: external = unless s" Symbol is not external in package." error then
            ( #package-token #token @sym )
            vr+ -rot vr- vr- 
        endof
        internal-token of
            over cl-find-package dup nil <> unless s" Package not found." error then
            ( #package-token #token @package )
            cl-intern drop vr+ swap vr-
        endof
        unreachable
    endcase
    
;

variable read-ref-table
nil vr+ read-ref-table !

: _read ( read-char -- v eof )
    label
    dup read-char if
        2drop nil vr+ true exit
    then
    dup char-type case
        ( read-char char )
        constituent-type of
            over unread-char read-token convert-token false
        endof
        whitespace-type of
            drop go
        endof
        terminating-macro-type of
            over swap char-data macro-char-routine execute
            ( read-char v n )
            if swap drop false else drop go then
        endof
        non-terminating-macro-type of
            over swap char-data macro-char-routine execute
            ( read-char v n )
            if swap drop false else drop go then
        endof
        single-escape-type of
            over unread-char read-token convert-token false
        endof
        multiple-escape-type of
            over unread-char read-token convert-token false
        endof
    endcase
;

\ TODO: only do this if #n# references are actually used
: replace-refs ( seen addr -- seen )
    dup @ dup sharpsign-equal-wrapper-p if
        ( seen addr wrapper )
        sharpsign-equal-wrapper-value
        ( seen addr next-addr )
        rot over recurse -rot @ vr+ swap v!
    else swap drop
        ( seen v )
        2dup vector-member unless
            dup -rot vr+ vector-push swap
            dup cons-p if
                swap over &car recurse swap
                swap over &cdr recurse swap
            else dup t-vector-p if
                dup vector-data over vector-length@ 0 do
                    ( seen v data )
                    rot over i cells + recurse -rot
                loop drop
            then then
        then
        drop
    then
;

: read ( read-char -- v eof )
    read-ref-table @ nil = dup if
        make-eql-hash-table read-ref-table v!
    then swap
    ( toplevel-p read-char )
    _read
    ( toplevel-p v eof )
    \ TODO: lock this behind an unwind-protect so as to not leak a hash-table if there is an error
    rot if
        ( v eof )
        dsp@ 8+ 0 make-t-vector swap replace-refs vr-
        nil vr+ read-ref-table v!
    then
;

: read-peek-non-whitespace ( read-char -- char eof )
    label
    dup read-char if
        ( read-char char )
        2drop 0 true exit
    then
    dup char-type whitespace-type = if
        drop go
    then
    dup rot unread-char false
;

: peek-char ( read-char -- char eof )
    dup read-char rot 2 pick swap 2 pick unless unread-char then
;

: check-read-parens ( eof -- )
    if s" Missing close parenthesis." error then
;

s" ." cl-string constant dot-str

: read-parens ( read-char -- v 1 )
    [ mc-( set-macro-char-routine ]
    dup read-peek-non-whitespace check-read-parens case
        ')' of skip-char nil vr+ 1 exit endof
        '.' of s" Invalid dot notation." error endof
    endcase
    dup read check-read-parens nil vr+ cons dup
    ( read-char cons end )
    begin
        2 pick read-peek-non-whitespace check-read-parens case
            ')' of rot skip-char drop 1 exit endof
            '.' of
                2 pick read-token unless
                    ( read-char cons end token )
                    dup dot-str equalp if
                        vr- 2 pick read check-read-parens
                        ( read-char cons end last )
                        swap &cdr v! swap
                        ( cons read-char )
                        dup read-peek-non-whitespace check-read-parens
                        ( cons read-char next-char )
                        ')' <> if s" Misplaced dot in list notation." error then skip-char 1
                        exit
                    then
                then
                true
            endof
            false swap
        endcase
        ( read-char cons end token? token-p )
        unless
            2 pick read check-read-parens
        then nil vr+ cons
        ( read-char cons end new-end )
        dup rot &cdr v!
    again
;

: read-close-paren ( read-char -- v 1 )
    [ mc-) set-macro-char-routine ]
    s" Unmatched closing paren." error
;

: check-not-eof ( eof -- )
    if s" End of file." error then
;

: read-quote ( read-char -- v 1 )
    [ mc-' set-macro-char-routine ]
    read check-not-eof '' quote vr+ swap 2list 1
;

: read-quasiquote ( read-char -- v 1 )
    [ mc-` set-macro-char-routine ]
    read check-not-eof '' quasiquote vr+ swap 2list 1
;

: read-unquote ( read-char -- v 1 )
    [ mc-, set-macro-char-routine ]
    dup peek-char drop '@' = if
        dup skip-char '' unquote-splicing
    else
        '' unquote
    then vr+ swap
    ( sym read-char )
    read check-not-eof 2list 1
;

: read-semicolon ( read-char -- 0 0 )
    [ mc-; set-macro-char-routine ]
    begin dup read-char not swap '\n' <> and while
    repeat
    drop 0 0
;

: read-string ( read-char -- v 1 )
    [ mc-" set-macro-char-routine ]
    0 make-character-string
    ( read-char str )
    begin
        over read-char if s" Unmatched double quote." error then
        ( read-char str char )
        dup '\\' = if
            drop over read-char-strict true
        else
            dup '"' <>
        then
        ( read-char str char bool )
    while
        box-character vector-push
    repeat drop
    swap drop 1
;

00 constant mc-##
01 constant mc-#'
02 constant mc-#(
03 constant mc-#*
04 constant mc-#:
05 constant mc-#=
06 constant mc-#\
07 constant mc-#|
08 constant mc-#+
09 constant mc-#-
10 constant mc-#.
11 constant mc-#a
12 constant mc-#b
13 constant mc-#c
14 constant mc-#o
15 constant mc-#p
16 constant mc-#r
17 constant mc-#s
18 constant mc-#x
19 constant mc-#num
19 constant mc-#invalid

num-base-chars allot constant sharpsign-routine-index-map
: set-sri ( idx char -- ) sharpsign-routine-index-map + c! ;
: get-sri ( char -- idx ) sharpsign-routine-index-map + c@ ;
: init-sri num-base-chars 0 do mc-#invalid i set-sri loop ; init-sri

mc-## char # set-sri
mc-#' char ' set-sri
mc-#( char ( set-sri
mc-#* char * set-sri
mc-#: char : set-sri
mc-#= char = set-sri
mc-#\   '\\' set-sri
mc-#| char | set-sri
mc-#+ char + set-sri
mc-#- char - set-sri
mc-#. char . set-sri
mc-#a char a set-sri mc-#a char A set-sri
mc-#b char b set-sri mc-#b char B set-sri
mc-#c char c set-sri mc-#c char C set-sri
mc-#o char o set-sri mc-#o char O set-sri
mc-#p char p set-sri mc-#p char P set-sri
mc-#r char r set-sri mc-#r char R set-sri
mc-#s char s set-sri mc-#s char S set-sri
mc-#x char x set-sri mc-#x char X set-sri

' unimplemented mc-#num copies mc-#num array constant sharpsign-routines
: sharpsign-routine ( type -- routine ) cells sharpsign-routines + @ ;
: set-sharpsign-routine ( type -- )
    cells sharpsign-routines + latest @ >cfa swap !
;

: read-numeric-argument ( read-char -- n )
    false 0 ( read-char num-p n )
    begin 2 pick read-char-strict dup '0' '9' between while
        ( read-char num-p n char )
        '0' - swap 10 * + true 1 poke
    repeat
    -rot swap unless drop nil then
    ( read-char char n )
    -rot swap unread-char
;

: read-sharpsign ( read-char -- v 1 )
    [ mc-# set-macro-char-routine ]
    dup read-numeric-argument over read-char-strict
    ( read-char n char )
    dup num-base-chars >= if mc-#invalid else get-sri then
    ( read-char n idx )
    dup mc-#num >= if s" Invalid dispatch macro character." error then
    sharpsign-routine execute
;

: read-sharpsign-\ ( read-char n -- v 1 )
    [ mc-#\ set-sharpsign-routine ]
    drop '\\' over unread-char read-token drop
    ( token )
    dup vector-length@ 1 = if
        dup 0 vector@ swap vr-
    else
        dup char-names gethash nil = if s" Invalid character name." error then
        ( token info )
        char-info-char box-character swap vr-
    then
    1
;


: read-sharpsign-' ( read-char n -- v 1 )
    [ mc-#' set-sharpsign-routine ]
    drop read check-not-eof '' function vr+ swap 2list 1
;

: read-sharpsign-( ( read-char n -- v 1 )
    [ mc-#( set-sharpsign-routine ]
    0 make-t-vector
    ( read-char n v )
    begin 2 pick read-peek-non-whitespace check-not-eof ')' <> while
        2 pick read check-not-eof vector-push
    repeat
    rot skip-char swap
    ( v n )
    dup nil = if drop else
        over vector-length@ - dup 0< if s" Too many elements in vector." error then
        dup 0<> if
            over vector-length@ dup 0= if s" Need a value to fill vector." error then
            ( v n length )
            1- 2 pick swap vector@ swap 0 do
                ( v val )
                vr+ dup -rot vector-push swap
            loop
            drop
        else drop then
    then
    ( v )
    1
;

: read-sharpsign-* ( read-char n -- v 1 )
    [ mc-#* set-sharpsign-routine ]
    swap read-token if s" Escape characters not supported in #* notation." then
    ( n token )
    over nil = if dup vector-length@ else over then make-bit-vector swap
    ( n v token )
    dup vector-length@ 0 do
        dup i vector@ unbox-character '0' -
        ( n v token bit )
        dup 0 1 between unless s" Bit vector character not 0 or 1." error then
        box-fixnum 2 pick i rot vector!
    loop
    dup vector-length@ swap vr- swap rot
    ( length v n )
    dup nil = if rot 2drop else
        2 pick over > if s" Too many elements in bit vector." error then
        2 pick over <> if
            2 pick 0= if s" Need a value to fill bit vector." error then
            rot dup 3 pick swap 1- vector@ -rot do
                ( v val )
                2dup i swap vector!
            loop
            drop
        else drop then
    then
    ( v )
    1
;

: read-sharpsign-: ( read-char n -- v 1 )
    [ mc-#: set-sharpsign-routine ]
    drop read-token drop cl-make-symbol 1
;

\ read-sharpsign-.
\ read-sharpsign-b
\ read-sharpsign-o
\ read-sharpsign-x
\ read-sharpsign-r
\ read-sharpsign-c
\ read-sharpsign-a
\ read-sharpsign-s
\ read-sharpsign-p

: read-sharpsign-= ( read-char n -- v 1 )
    [ mc-#= set-sharpsign-routine ]
    dup nil = if s" Missing numeric argument to #= syntax." error then
    make-sharpsign-equal-wrapper dup rot read-ref-table @ sethash
    ( read-char wrapper found-p )
    nil <> if s" Numeric argument already used in #= syntax." error then swap
    ( wrapper read-char )
    read check-not-eof dup vr+ rot sharpsign-equal-wrapper-value !
    ( v )
    1
;

: read-sharpsign-# ( read-char n -- v 1 )
    [ mc-## set-sharpsign-routine ]
    dup nil = if s" Missing numeric argument to ## syntax." error then
    read-ref-table @ gethash nil = if s" Numeric reference not found in syntax table." error then
    vr+
    ( read-char v )
    swap drop 1
;

\ read-sharpsign-+
\ read-sharpsign--

: read-sharpsign-| ( read-char n -- nil 0 )
    [ mc-#| set-sharpsign-routine ]
    \ TODO: need vr+ on the nil?
    drop 1 ( read-char level )
    begin dup 0<> while
        over read-char check-not-eof case
            '#' of
                over peek-char check-not-eof '|' = if
                    over skip-char 1+
                then
            endof
            '|' of
                over peek-char check-not-eof '#' = if
                    over skip-char 1-
                then
            endof
        endcase 
    repeat
    2drop nil 0
;
