\ -*- text -*-

: / /mod swap drop ;
: mod /mod drop ;

: '\n' 10 ;
: bl   32 ;

: cr '\n' emit ;
: space bl emit ;

: negate 0 swap - ;


: true 1 ;
: false 0 ;
: not 0= ;

: nop ;

: literal immediate
	' lit , ,
;

: ':' [ char : ] literal ;
: ';' [ char ; ] literal ;
: '(' [ char ( ] literal ;
: ')' [ char ) ] literal ;
: '"' [ char " ] literal ;
: 'a' [ char a ] literal ;
: 'z' [ char z ] literal ;
: 'A' [ char A ] literal ;
: 'Z' [ char Z ] literal ;
: '0' [ char 0 ] literal ;
: '9' [ char 9 ] literal ;
: '-' [ char - ] literal ;
: '.' [ char . ] literal ;
: '@' [ char @ ] literal ;
: '#' [ char # ] literal ;
: '|' [ char | ] literal ;
: '\\' 92 ;

: [compile] immediate
	word find >cfa ,
;

: recurse immediate
	latest @ >cfa ,
;

: if immediate
	' 0branch , here @ 0 ,
;

: then immediate
	dup here @ swap - swap !
;

: else immediate
    ' branch , here @ 0 ,
    swap
    dup here @ swap - swap !
;

: begin immediate
    here @
;

: until immediate
    ' 0branch ,
    here @ - ,
;

: again immediate
    ' branch ,
    here @ - ,
;

: while immediate
    ' 0branch ,
    here @ 0 ,
;

: repeat immediate
    ' branch ,
    swap here @ - ,
    dup here @ swap - swap !
;

: unless immediate
    ' not ,
    [compile] if
;

: ( immediate
    1 begin
        key
        dup 0= if stop then
        dup '(' = if
    	    drop 1+
        else
            ')' = if
                1-
            then
        then
    dup 0= until
    drop
;

: nip ( x y -- y ) swap drop ;
: tuck ( x y -- y x y ) swap over ;
: pick ( x_u ... x_1 x_0 u -- x_u ... x_1 x_0 x_u )
    1+ 8 * dsp@ + @
;
: poke ( x_u ... x_1 x_0 v u -- v ... x_1 x_0 )
    2 + 8 * dsp@ + !
;

: spaces ( n -- )
    begin dup 0> while space 1- repeat drop
;

: decimal ( -- ) 10 base ! ;
: hex ( -- ) 16 base ! ;

: u. ( n -- )
    base @ /mod
    ?dup if recurse then
    dup 10 < if
        '0'
    else
        10 - 'a'
    then
    +
    emit
;

: .s ( -- )
    dsp@
    begin
        dup s0 @ <
    while
        dup @ u.
        space
        8+
    repeat
    drop
;

: uwidth ( n -- )
    base @ /
    ?dup if recurse 1+ else 1 then
;

: u.r ( n width -- )
    swap dup uwidth rot swap - spaces u.
;

: .r ( n width -- )
    swap
    dup 0< if
        negate 1
        swap rot 1-
    else
        0 swap rot
    then
    swap dup uwidth rot swap - spaces
    swap if '-' emit then
    u.
;

: . ( n -- ) 0 .r space ;
: u. ( n -- ) u. space ;

: ? ( addr -- ) @ . ;

: within ( c a b -- a <= c < b )
    -rot over
    <= if >
    else 2drop false then
;

: between ( c a b -- a <= c <= b )
    -rot over
    <= if >=
    else 2drop false then
;

: depth ( -- n )
    dsp@ s0 @ swap -
;

: align ( n align -- m )
    1- dup -rot + swap invert and
;

: 8align ( n -- m ) 8 align ;

: here-align ( -- ) here @ 8align here ! ;
: c, ( c -- ) here @ c! 1 here +! ;
: s" immediate ( -- addr len )
    state @ if
        ' litstring ,
        here @
        0 ,
        begin key dup '"' <> while
            dup '\\' = if drop key then
            c,
        repeat drop
        0 c,
        dup here @ swap - 8- 1- swap !
        here-align
    else
        here @
        ( addr )
        begin key dup '"' <> while
            ( addr key )
            dup '\\' = if drop key then
            over c! 1+
        repeat drop
        0 over c! 1+
        dup here @ - here @ swap rot 8align here !
        ( here len )
        1-
    then
;

: ." immediate
    state @ if
        [compile] s" ' tell ,
    else
        begin
            key
            dup '"' = if drop exit then
            emit
        again
    then
;

: constant
    word create docol ,
    ' lit , , ' exit ,
;

: allot ( n -- addr ) here @ swap here +! ;
: cells ( n -- n ) 8 * ;

: variable
    1 cells allot
    word create docol ,
    ' lit , , ' exit ,
;

: value
    word create docol ,
    ' lit , , ' exit ,
;

: to immediate
    word find >dfa 8+
    state @ if
        ' lit , , ' ! ,
    else
        !
    then
;

: +to immediate
    word find >dfa 8+
    state @ if
        ' lit , , ' +! ,
    else
        +!
    then
;

1 cells allot constant branch-addr

( TODO: implement forwards label )
: label immediate
    here @ branch-addr !
;

: go immediate
    ' branch , branch-addr @ here @ - ,
;

: case immediate 0 ;

: of immediate
    ' over ,
    ' = ,
    [compile] if
    ' drop ,
;

: endof immediate
    [compile] else
;

: endcase immediate
    ' drop ,
    begin ?dup while [compile] then repeat
;

: do immediate
    here @ ' 2dup , ' > , ' 0branch , here @ 0 ,
    ' 1+ , ' >r , ' >r ,
    swap
;

: i immediate
    ' rsp@ , ' 8+ , ' @ , ' 1- ,
;

: loop immediate
    ' r> , ' r> , ' branch , here @ - ,
    dup here @ swap - swap !
    ' 2drop ,
;

: expt2 ( n -- 2^n ) 1 swap lsl ;
: log2 ( n -- log_2(n) ) clz 63 swap - ;
: log2^ ( n -- log_2(n) )
    dup clz 1+ -1 swap lsr + log2
;

: array ( x_n ... x_0 n -- addr )
    dup cells allot swap 1-
    ( ... addr n )
    begin dup 0 >= while
        rot 2 pick 2 pick cells + !
        1-
    repeat
    drop
;

: show-bound-word ( addr -- )
    latest @
    ( addr entry )
    begin ?dup while
        swap
        ( entry addr )
        dup 2 pick >cfa dup @ docol = if
            ( entry addr addr cfa )
            dup begin dup @ ' exit <> while
                ( entry addr addr cfa &exit )
                8+
            repeat
            8+ within if
                drop
                dup 12 + w@ swap 16 + swap tell cr exit
            then
        else 2drop then
        swap @
    repeat
    drop
;

: show-trace ( -- )
    rsp@ begin dup r0 < while
        ( rsp )
        dup @ show-bound-word
        8+
    repeat drop
;

56 constant __NR_openat
57 constant __NR_close
63 constant __NR_read
64 constant __NR_write
93 constant __NR_exit

215 constant __NR_munmap
222 constant __NR_mmap
226 constant __NR_mprotect

4096 constant PAGE_SIZE

-100 constant AT_FDCWD

0 constant O_RDONLY
1 constant O_WRONLY
2 constant O_RDWR

0 constant PROT_NONE
1 constant PROT_READ
2 constant PROT_WRITE
4 constant PROT_EXEC

01 constant MAP_SHARED
02 constant MAP_PRIVATE
03 constant MAP_SHARED_VALIDATE
16 constant MAP_FIXED
32 constant MAP_ANONYMOUS

: os-exit ( code -- )
    __NR_exit syscall1
;

\ TODO: modified from gist tschaefer/linux-error.txt (licence?)
: perror ( errno string length -- )
    tell s" : " tell
    case
    1 of s" Operation not permitted" endof
    2 of s" No such file or directory" endof
    1 of s" Operation not permitted" endof
    2 of s" No such file or directory" endof
    3 of s" No such process" endof
    4 of s" Interrupted system call" endof
    5 of s" Input/output error" endof
    6 of s" No such device or address" endof
    7 of s" Argument list too long" endof
    8 of s" Exec format error" endof
    9 of s" Bad file descriptor" endof
    10 of s" No child processes" endof
    11 of s" Resource temporarily unavailable" endof
    11 of s" Resource temporarily unavailable" endof
    12 of s" Cannot allocate memory" endof
    13 of s" Permission denied" endof
    14 of s" Bad address" endof
    15 of s" Block device required" endof
    16 of s" Device or resource busy" endof
    17 of s" File exists" endof
    18 of s" Invalid cross-device link" endof
    19 of s" No such device" endof
    20 of s" Not a directory" endof
    21 of s" Is a directory" endof
    22 of s" Invalid argument" endof
    23 of s" Too many open files in system" endof
    24 of s" Too many open files" endof
    25 of s" Inappropriate ioctl for device" endof
    26 of s" Text file busy" endof
    27 of s" File too large" endof
    28 of s" No space left on device" endof
    29 of s" Illegal seek" endof
    30 of s" Read-only file system" endof
    31 of s" Too many links" endof
    32 of s" Broken pipe" endof
    33 of s" Numerical argument out of domain" endof
    34 of s" Numerical result out of range" endof
    35 of s" Resource deadlock avoided" endof
    35 of s" Resource deadlock avoided" endof
    36 of s" File name too long" endof
    37 of s" No locks available" endof
    38 of s" Function not implemented" endof
    39 of s" Directory not empty" endof
    40 of s" Too many levels of symbolic links" endof
    42 of s" No message of desired type" endof
    43 of s" Identifier removed" endof
    44 of s" Channel number out of range" endof
    45 of s" Level 2 not synchronized" endof
    46 of s" Level 3 halted" endof
    47 of s" Level 3 reset" endof
    48 of s" Link number out of range" endof
    49 of s" Protocol driver not attached" endof
    50 of s" No CSI structure available" endof
    51 of s" Level 2 halted" endof
    52 of s" Invalid exchange" endof
    53 of s" Invalid request descriptor" endof
    54 of s" Exchange full" endof
    55 of s" No anode" endof
    56 of s" Invalid request code" endof
    57 of s" Invalid slot" endof
    59 of s" Bad font file format" endof
    60 of s" Device not a stream" endof
    61 of s" No data available" endof
    62 of s" Timer expired" endof
    63 of s" Out of streams resources" endof
    64 of s" Machine is not on the network" endof
    65 of s" Package not installed" endof
    66 of s" Object is remote" endof
    67 of s" Link has been severed" endof
    68 of s" Advertise error" endof
    69 of s" Srmount error" endof
    70 of s" Communication error on send" endof
    71 of s" Protocol error" endof
    72 of s" Multihop attempted" endof
    73 of s" RFS specific error" endof
    74 of s" Bad message" endof
    75 of s" Value too large for defined data type" endof
    76 of s" Name not unique on network" endof
    77 of s" File descriptor in bad state" endof
    78 of s" Remote address changed" endof
    79 of s" Can not access a needed shared library" endof
    80 of s" Accessing a corrupted shared library" endof
    81 of s" .lib section in a.out corrupted" endof
    82 of s" Attempting to link in too many shared libraries" endof
    83 of s" Cannot exec a shared library directly" endof
    84 of s" Invalid or incomplete multibyte or wide character" endof
    85 of s" Interrupted system call should be restarted" endof
    86 of s" Streams pipe error" endof
    87 of s" Too many users" endof
    88 of s" Socket operation on non-socket" endof
    89 of s" Destination address required" endof
    90 of s" Message too long" endof
    91 of s" Protocol wrong type for socket" endof
    92 of s" Protocol not available" endof
    93 of s" Protocol not supported" endof
    94 of s" Socket type not supported" endof
    95 of s" Operation not supported" endof
    95 of s" Operation not supported" endof
    96 of s" Protocol family not supported" endof
    97 of s" Address family not supported by protocol" endof
    98 of s" Address already in use" endof
    99 of s" Cannot assign requested address" endof
    100 of s" Network is down" endof
    101 of s" Network is unreachable" endof
    102 of s" Network dropped connection on reset" endof
    103 of s" Software caused connection abort" endof
    104 of s" Connection reset by peer" endof
    105 of s" No buffer space available" endof
    106 of s" Transport endpoint is already connected" endof
    107 of s" Transport endpoint is not connected" endof
    108 of s" Cannot send after transport endpoint shutdown" endof
    109 of s" Too many references: cannot splice" endof
    110 of s" Connection timed out" endof
    111 of s" Connection refused" endof
    112 of s" Host is down" endof
    113 of s" No route to host" endof
    114 of s" Operation already in progress" endof
    115 of s" Operation now in progress" endof
    116 of s" Stale NFS file handle" endof
    117 of s" Structure needs cleaning" endof
    118 of s" Not a XENIX named type file" endof
    119 of s" No XENIX semaphores available" endof
    120 of s" Is a named type file" endof
    121 of s" Remote I/O error" endof
    122 of s" Disk quota exceeded" endof
    123 of s" No medium found" endof
    124 of s" Wrong medium type" endof
    125 of s" Operation canceled" endof
    126 of s" Required key not available" endof
    127 of s" Key has expired" endof
    128 of s" Key has been revoked" endof
    129 of s" Key was rejected by service" endof
    130 of s" Owner died" endof
    131 of s" State not recoverable" endof
    132 of s" Operation not possible due to RF-kill" endof
    133 of s" Unknown error 133" endof
    s" Unknown error"
    endcase
    tell cr
    1 os-exit
;

: error ( string length -- )
    tell cr show-trace 1 os-exit
;

: unreachable
    s" Unreachable" error
;

: unimplemented
    s" Unimplemented" error
;

: mmap ( addr length prot flags fd offset -- addr )
    __NR_mmap syscall6
    dup 0< if negate s" mmap" perror then
;

: munmap ( addr length -- )
    __NR_munmap syscall2 drop
;

: mprotect ( addr len prot -- )
    __NR_mprotect syscall3
    dup 0< if negate s" mprotect" perror then
    drop
;

: open ( pathname flags mode -- fd )
    >r AT_FDCWD -rot r> __NR_openat syscall4
    dup 0< if negate s" open" perror then
;

: close ( fd -- )
    __NR_close syscall1
    0< if negate s" close" perror then
;

: read ( fd string count -- n )
    __NR_read syscall3
    dup 0< if negate s" read" perror then
;

1 cells allot constant utf8-buffer

: utf8-decode-first ( byte -- byte length )
    dup 0x80 and if
        dup 0xe0 and 0xc0 = if
            0x1f and 1
        else
        dup 0xf0 and 0xe0 = if
            0x0f and 2
        else
        dup 0xf8 and 0xf0 = if
            0x07 and 3
        else
            s" UTF-8 decode error." error
        then then then
    else 0 then
;

: utf8-decode-length ( byte -- length )
    utf8-decode-first nip
;

: utf8-decode ( string length -- len codepoint )
    dup 0= if s" No more characters to decode." error then
    over c@ utf8-decode-first
    ( string length byte len )
    -rot swap  2 pick < if s" Not enough characters to decode." error then
    ( string len byte )
    rot 1+ dup 3 pick + swap do
        ( len partial-codepoint )
        6 lsl i c@
        ( len partial-codepoint next-byte )
        dup 0xc0 and 0x80 <> if s" UTF-8 decode error." error then
        0x3f and or
    loop
    swap 1+ swap
;

: abs ( n -- |n| ) dup 0< if negate then ;
: sgn ( n -- sgn ) 0< if 1 else 0 then ;

0x8000000000000000 constant double-sign-mask
                63 constant double-sign-offset
0x7ff0000000000000 constant double-exponent-mask
                52 constant double-exponent-offset
              1023 constant double-exponent-bias
0x000fffffffffffff constant double-significand-mask
0x80000000 constant float-sign-mask
        31 constant float-sign-offset
0x7f800000 constant float-exponent-mask
        23 constant float-exponent-offset
       127 constant float-exponent-bias
0x007fffff constant float-significand-mask
                

: double-exponent ( d -- exp ) double-exponent-mask and double-exponent-offset lsr double-exponent-bias - ;
: double-significand ( d -- n ) double-significand-mask and 1 double-exponent-offset lsl + ;
: double-sign ( d -- sgn ) double-sign-mask and double-sign-offset lsr ;

: itod ( n -- d )
    dup sgn swap abs
    ( sgn |n| )
    dup log2 swap
    ( sgn exp |n| )
    over expt2 -
    over double-exponent-offset swap -
        dup 0< if s" Integer is too big to represent directly as a double." error then
        lsl
    swap double-exponent-bias + double-exponent-offset lsl or
    ( sgn d )
    swap double-sign-offset lsl or
;

: dtoi ( d -- n )
    dup double-exponent double-exponent-offset swap -
    ( d shift )
    over double-significand swap lsr
    over double-sign if -1 else 1 then *
    swap drop
;

: double-exp2-to-exp10+ ( exp2 n -- exp10 n )
    0 swap
    ( exp2 exp10 n )
    begin 2 pick 0<> while
        1 lsl 2 pick 1- 2 poke
        dup 10 double-exponent-offset lsl >= if
            10 / 1 pick 1+ 1 poke
        then
    repeat
;

: double-exp2-to-exp10- ( exp2 n -- exp10 n )
    0 swap
    ( exp2 exp10 n )
    begin 2 pick 0<> while
        1 lsr 2 pick 1+ 2 poke
        dup 1 double-exponent-offset lsl < if
            10 * 1 pick 1- 1 poke
        then
    repeat
;

: double-exp2-to-exp10 ( exp2 n -- exp10 n )
    over 0 >= if double-exp2-to-exp10+ else double-exp2-to-exp10- then
;

: d. ( d -- )
    base @ >r
    decimal
    dup double-sign-mask and if s" -" tell then
    dup double-exponent over double-significand double-exp2-to-exp10
    ( d exp n )
    dup double-exponent-offset lsr '0' + emit s" ." tell
    double-significand-mask and
    1 swap
    ( d exp err n )
    begin over 1 double-exponent-offset lsl < over 0<> and while
        10 * dup double-exponent-offset lsr '0' + emit double-significand-mask and
        over 10 * 1 poke
    repeat 2drop
    ( d exp )
    s" d" tell . drop
    r> base !
;

( TODO: dedublicate this code, the following is exactly the same except 'double' -> 'float', 'd.' -> 'f.', and 's" d"' -> 's" f"' )
( TODO: remove printing code since it is unaccurate and will later be implemented in CL )

: float-exponent ( f -- exp ) float-exponent-mask and float-exponent-offset lsr float-exponent-bias - ;
: float-significand ( f -- n ) float-significand-mask and 1 float-exponent-offset lsl + ;
: float-sign ( f -- sgn ) float-sign-mask and float-sign-offset lsr ;

: itof ( n -- f )
    dup sgn swap abs
    ( sgn |n| )
    dup log2 swap
    ( sgn exp |n| )
    over expt2 -
    over float-exponent-offset swap -
        dup 0< if s" Integer is too big to represent directly as a float." error then
        lsl
    swap float-exponent-bias + float-exponent-offset lsl or
    ( sgn f )
    swap float-sign-offset lsl or
;

: ftoi ( f -- n )
    dup float-exponent float-exponent-offset swap -
    ( f shift )
    over float-significand swap lsr
    over float-sign if -1 else 1 then *
    swap drop
;

: float-exp2-to-exp10+ ( exp2 n -- exp10 n )
    0 swap
    ( exp2 exp10 n )
    begin 2 pick 0<> while
        1 lsl 2 pick 1- 2 poke
        dup 10 float-exponent-offset lsl >= if
            10 / 1 pick 1+ 1 poke
        then
    repeat
;

: float-exp2-to-exp10- ( exp2 n -- exp10 n )
    0 swap
    ( exp2 exp10 n )
    begin 2 pick 0<> while
        1 lsr 2 pick 1+ 2 poke
        dup 1 float-exponent-offset lsl < if
            10 * 1 pick 1- 1 poke
        then
    repeat
;

: float-exp2-to-exp10 ( exp2 n -- exp10 n )
    over 0 >= if float-exp2-to-exp10+ else float-exp2-to-exp10- then
;

: f. ( f -- )
    base @ >r
    decimal
    dup float-sign-mask and if s" -" tell then
    dup float-exponent over float-significand float-exp2-to-exp10
    ( d exp n )
    dup float-exponent-offset lsr '0' + emit s" ." tell
    float-significand-mask and
    1 swap
    ( d exp err n )
    begin over 1 float-exponent-offset lsl < over 0<> and while
        10 * dup float-exponent-offset lsr '0' + emit float-significand-mask and
        over 10 * 1 poke
    repeat 2drop
    ( d exp )
    s" f" tell . 2drop
    r> base !
;

: ndup ( x_n ... x_1 n -- x_n ... x_1 x_n ... x_1 )
    dup 0 do dup pick swap loop drop
;

: copies ( x n -- x ...[n] x )
    1 do dup loop
;

: ndrop ( x_n ... x_1 n -- )
    0 do drop loop
;

: ' immediate
    state @ if ' lit , else word find >cfa then
;

: <cfa ( addr -- addr )
    latest @
    ( addr entry )
    begin dup while
        2dup >cfa = if swap drop exit then
        @
    repeat
    2drop 0
;

: >name ( addr -- string length ) dup 12 + w@ swap 16 + swap ;
