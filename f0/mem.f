\ -*- mode: text -*-

( 1GB each )
0x40000000 constant heap-total-size
0x40000000 constant cons-heap-total-size

0 heap-total-size      PROT_READ MAP_ANONYMOUS MAP_PRIVATE or -1 0 mmap value heap-head
0 cons-heap-total-size PROT_READ MAP_ANONYMOUS MAP_PRIVATE or -1 0 mmap value cons-heap-head
heap-head      value heap-tail
cons-heap-head value cons-heap-tail

heap-total-size log2 constant max-order
16 log2 constant min-order

max-order min-order - 1+ cells allot constant free-lists

: free-list ( order -- addr ) min-order - cells free-lists + ;

: alloc-page ( -- )
    heap-tail PAGE_SIZE PROT_READ PROT_WRITE or mprotect
    PAGE_SIZE +to heap-tail
;

: cons-alloc-page ( -- )
    cons-heap-tail PAGE_SIZE PROT_READ PROT_WRITE or mprotect
    PAGE_SIZE +to cons-heap-tail
;

: in-heap-p ( addr -- b )
    heap-head heap-tail within
;

0x00ffffffffffffff constant block-addr-mask
0x7f constant block-order-mask
0x80 constant block-is-free-flag

: block-order@ ( block -- order ) 7 + c@ block-order-mask and ;
: block-size@ ( block -- size ) block-order@ expt2 ;
: block-is-free@ ( block -- is-free ) 7 + c@ block-is-free-flag and 7 lsr ;

: block-order! ( order block -- )
    dup 7 + c@ block-is-free-flag and rot block-order-mask and or swap 7 + c!
;

: block-prev@ ( block -- addr ) @ block-addr-mask and ;
: block-next@ ( block -- addr ) 8+ @ ;
: block-prev! ( addr block -- )
    dup @ block-addr-mask invert and
    ( addr block not-prev )
    rot block-addr-mask and or
    ( block new-header )
    swap !
;
: block-next! ( addr block -- ) 8+ ! ;

: show-blocks ( -- )
    0 heap-head
    ( i block )
    begin dup heap-tail < while
        s" Block " tell over u. s" at " tell dup . s" [" tell dup @ . s" ]" tell cr
        s"   free: " tell dup block-is-free@ . cr
        s"   order: " tell dup block-order@ . cr
        s"   size: " tell dup block-size@ . cr
        s"   prev: " tell dup block-prev@ . cr
        s"   next: " tell dup block-next@ . cr
        cr
        dup block-size@ +
    repeat
    2drop
;

: incref ( addr -- )
    8- dup block-is-free@ if s" Attempting to incref free block." error then
    dup block-prev@ 1+ swap block-prev!
;

: decref ( addr -- )
    8-
    dup block-is-free@ if s" Attempting to decref free block." error then
    dup block-prev@ 1- swap block-prev!
;

: refcount ( addr -- n )
    8- block-prev@
;

: unfree-block ( block -- )
    dup 7 + dup c@ block-is-free-flag invert and swap c!
    dup block-next@ over block-prev@
    ( block next prev )
    dup if
        2dup block-next!
    else
        over 3 pick block-order@ free-list !
    then
    swap
    dup if block-prev! else 2drop then
    ( block )
    0 swap block-prev!
;

: free-block ( block -- )
    dup 7 + dup c@ block-is-free-flag or swap c!
    dup block-order@ free-list @
        ( block next )
        dup if 2dup block-prev! then
        over block-next!
    0 over block-prev!
    dup block-order@ free-list !
;

: expand-heap ( -- )
   heap-tail dup heap-head -
   dup PAGE_SIZE / 0 do
       alloc-page
   loop
   log2 swap
   ( order prev-tail )
   0 over ! 0 over 8+ !
   dup 7 + rot swap c!
   ( prev-tail )
   free-block
;

: split-block ( order block -- block )
    over min-order < if s" Can not split block at this order." then
    dup unfree-block
    begin
        dup block-order@ 2 pick >
    while
        dup block-size@ 1 lsr over +
        ( order block new-block )
        0 over ! 0 over 8+ !
       over block-order@ 1- dup -rot
        ( order block new-order new-block new-order )
        over block-order! free-block
        over block-order!
    repeat
    swap drop
;

: malloc ( size -- addr )
    dup 8+ log2^
    ( size order )
    dup free-list @ ?dup if
        dup unfree-block
        8+ -rot 2drop
        exit
    then
    dup
    begin
        ( size original-order new-order )
        1+
        dup max-order > if
            heap-tail heap-head - heap-total-size >= if
                2drop drop 0
                exit
            then
            expand-heap
            2drop recurse
            exit
        then
        dup free-list @ ?dup if
            swap drop
            ( size original-order block )
            split-block
            8+ swap drop
            exit
        then
    again
;

: join-block ( block -- block )
    begin
        ( block )
        dup block-size@ over heap-head - over and if
            over swap - 
        else
            over +
        then
        ( block buddy-block )
        dup heap-tail >= if drop exit then
        dup block-is-free@ unless drop exit then
        over block-order@ over block-order@ = unless drop exit then
        dup unfree-block
        2dup > if swap then drop
        ( block )
        dup block-order@ 1+
        ( block new-order )
        dup max-order > if
            drop exit
        then
        over block-order!
    again
;

: free ( addr -- )
    8- join-block free-block
;

: show-free-lists ( -- )
    max-order min-order - 1+ 0 do
        s" Free list for order " tell i 4 + .
        s" to " tell free-lists i cells + @ .
        cr
    loop
    cr
;

: show-mem ( -- )
    0 0 heap-head
    ( m n block )
    begin dup heap-tail < while
        dup block-is-free@ not if
            swap 1+ swap
            rot over block-size@ + -rot
        then
        dup block-size@ +
    repeat drop
    ( m n )
    s" Used blocks: " tell u. cr
    s" Mem used: " tell u. cr
    cr
;

: mem-init ( -- )
    free-lists
    max-order min-order - 1+ 0 do
        dup 0 swap ! 8+
    loop
    drop
    alloc-page
    heap-head PAGE_SIZE log2 free-list !

    \ Write the block header
    block-is-free-flag PAGE_SIZE log2 or 56 lsl heap-head !
    0 heap-head 8+ !
;

mem-init
