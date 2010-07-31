( block 0  )

This block file, kernel.fth, is used to create a target image when
starting riscy.tcl this way:

    ./riscy.tcl -flash 1

(normally, though, that would be done by the makefile)

It will then load block 1 (in this file), which is the load 
block that will load other blocks.

Finally, at some point, SAVE-IMAGE must be executed in order
to save the results to the *.bin and *.dictionary files, 
e.g.,

    SAVE-IMAGE kernel INIT

would save the results to the files kernel.bin and 
kernel.dictionary and set the boot word to QUIT

( shadow 0 )

( block 1  load block to create the kernel image  )

2 LOAD   ( QUIT (ABORT INIT  )

3 LOAD   ( S .CR .H etc )

4 LOAD   ( comparisons)
5 LOAD   ( math)
6 LOAD   ( CMOVE etc)

7 LOAD   ( Timing )

8 LOAD   ( DUMP DU)

9 10 THRU   ( multitasker)

;; 12 24 THRU  ( TEST)


( shadow 1 )

( block 2   QUIT  (ABORT  INIT )

: QUIT ( -) RP! BEGIN PAUSE (SERIN? UNTIL  queryinterpret ( f) ?ok  QUIT ;

: (ABORT ( a -)  COUNT TYPE  SP!  ?ok  QUIT  ;

: INIT ( -) ' (ABORT  'ABORT  !  QUIT  ;
( shadow 2 )

load block for creating the flash image for Riscy Pygness

The ";;" after QUIT is defined switches back to interpret mode so the
additional blocks can be loaded.

The last line on the block saves the image to the names kernel.bin and
kernel.dictionary, while setting that the "boot" or startup word will be
QUIT. 

When controlled by the makefile, the image files will be renamed
appropriately for the specific ARM variant, such as kernel-lpc2106.bin
and kernel-lpc2106.dictionary.

( shadow 2 )






( block 3  miscellaneous)

: .S ( -)  ROT DUP . ROT DUP . ROT DUP .  ;
: CR ( -)   $0D EMIT  $0A EMIT  ;

: .H  ( u -)  HEX U. DECIMAL  ;

: ?  ( a -)  @ . ;


: BL ( - c)  $20  ;
: SPACE ( -)  BL EMIT  ;
: SPACES ( # -)  FOR  SPACE  NEXT  ;

: BYTES ( aabb - bb aa)  DUP $FF AND  SWAP 256/  ;

( : CMOVE ( fr to # -)
(  BYTES ( fr to ls# ms#)    ( SWAP PUSH )
  ( fr to ms#)    ( FOR  2DUP 256 (CMOVE  256 +  256 +UNDER  NEXT )
  ( fr to)   ( POP ?DUP IF (CMOVE ; THEN  2DROP  ;  )



( shadow 3 )
CMOVE ( fr to # -)

 Split the count into ms# full 256-byte moves then a final ls# byte move
 if ls# is not zero.

( block 4   comparisons )

: 0<> ( n - f)   0= NOT  ;

( n is BETWEEN low and high iff y is WITHIN low and high+1 )
: BETWEEN ( n l h - f) 1+  ( fall through )                     
: WITHIN ( n l h - f) OVER - PUSH - POP U< ;

( shadow 4 )

( block 5   math )

: ABS DUP 0< IF NEGATE ; THEN ;                                 
: MAX 2DUP < IF SWAP THEN DROP ;                                
: MIN 2DUP > IF SWAP THEN DROP ;

( Note that MOD is really UMOD )
: MOD  ( a b - r)  U/MOD DROP  ;
 
: U/ ( a b - q)  U/MOD NIP   ; 

( Signed division.  Remember the sign, do an unsigned divide, )
(  finally, apply the correct sign to the result.             )
: /  ( n n - n) 
  SWAP DUP 0< PUSH ABS   
  SWAP DUP 0< PUSH ABS  ( u u) 
  U/   POP POP XOR IF NEGATE THEN  ; 

( shadow 5   math )


( block 6  CMOVE  FILL etc)

( CMOVE>  move bytes from high end of buffer so buffers can overlap.)
(         This needs to be rewritten as a primitive. )
: CMOVE> ( fr to #)  ( PAUSE )
  PUSH R@ +UNDER  R@ + 
  POP FOR ( fr to)  -1 +UNDER 1-  OVER C@ OVER C! NEXT 
  2DROP  ;

: CMOVE ( fr to # -)  ( PAUSE)
  ( Later, rewrite as a fast primitive based on ARM's load and )
  ( store multiple instructions.)
  FOR ( from to) OVER C@  OVER C!  1 +UNDER 1+  NEXT  2DROP  ;


: FILL ( a # c -) SWAP DUP 0= IF 3DROP ;  THEN 
  PUSH ( a c) OVER C! DUP 1+ POP 1- CMOVE  ; 
: BLANK ( a # -) BL FILL  ; 
: ERASE ( a # -)  0 FILL  ;


( shadow 6 )

( block 7  Timing )

: T1@ ( - u)  T1TC @  ;  ( read the timer )

: MS ( # -)
  ( This is very accurate but is only good to about
    1165000 milliseconds, when PCLK is 3,686,400 Hz,
    i.e. about 19 minutes. )
  T1@ ( originalTimer)
  SWAP TICKS/MS * +  ( goal) 
  BEGIN PAUSE T1@ OVER U> UNTIL DROP  ;

( shadow 7  Timing )

MS ( # -)
  Kill the requested number of milliseconds.
( shadow 7)
( shadow 7   math )


( block 8   DUMP )

: DUMP ( a - a') 
  HEX  CR   DUP 5 U.R  ( 2 SPACES) 
  DUP  2 FOR ( a a) 2 SPACES  8 FOR DUP C@ 3 U.R  1+ NEXT  SPACE  NEXT DROP 
  ( a) 2 FOR 2 SPACES  8 FOR 
        DUP C@  DUP 32 127 WITHIN NOT IF DROP 46 THEN EMIT 1+ 
        NEXT  SPACE 
  NEXT   DECIMAL ; 
 
: DU ( a n -) FOR DUMP ( ?SCROLL)  NEXT  DROP ; 
                                                                

( shadow 8   DUMP )

Unlike the usual Pygmy version of DUMP, Riscy Pygness does not save and
restore the base.  It always leaves the base set to DECIMAL.


( block 9  Multitasker   )

: LOCAL ( task a - a')  LINK - +  ;

: AWAKE? ( task - a | 0)
  PUSH (   ) LINK DUP ( start current) 
  BEGIN 
    ( start current) DUP @ ( start current candidate) R@ =
    ( start current flag) IF POP DROP NIP ( a) ; THEN
    ( start current) @ ( start newcurrent)  2DUP = 
  UNTIL 2DROP POP DROP 0  ;
  
: SLEEP ( task -)  
  DUP TERMINAL = IF DROP ; THEN
  DUP AWAKE? ?DUP IF ( task prevTask) 
    OVER @ SWAP ! ( ie unlink) 
  THEN ( task) DROP PAUSE ;
  
: STOP ( -) LINK SLEEP  ;

: WAKE ( newtask -) DUP AWAKE? IF DROP ; THEN
  DUP LINK ( new new curr )
  DUP @ PUSH ( new new curr -- next)
  ! ( ie new is now successor to curr)  ( new)
  POP SWAP ! ( ie next is now successor to new)   ;
  



( shadow 9  Multitasking )

LOCAL ( task a - a')

 Convert an address A local to the current task to an equivalent address
 local to the given task.  E.g.,   TASK1 RP0 LOCAL  answers the RP0 slot
 in TASK1's TCB.

AWAKE? ( task - a | 0)

 The LINK fields form a circular list which is, in effect, the active
 task list.  AWAKE?  checks to see if a given task is in the active task
 list.  If not, a zero is returned.  If it is, the address of the task
 that points to the given task is returned.  This is the link field that
 points to the task we were asking about.  Thus it is the field we need
 to alter if we wish to remove the task from the active list.

 The active task list is circular.  It is never empty, as the foreground
 task should never be put to sleep.  With a single task, the LINK field
 of that task contains the address of itself.

 Executing a task name returns the address of that task, which is the
 start of its user variables (i.e., the address of its LINK field).

 AWAKE? works by running through the LINKs to see if any of them contain
 an address that matches the given task.  We don't really need to know
 how many tasks are present in the LINKs as long as we know when to stop
 looking (when we get back to the one we started with).

SLEEP ( task -)

 Remove TASK from the active task list (if it is not the foreground task).

STOP ( -)

 Put the current task to sleep (if it is not the foreground task).


WAKE ( newtask -)

 Insert NEWTASK into the active task list.


( block 10  Multitasker   )

: TASK! ( word task -)
  PUSH ( word)
  R@ 8 + ( word IPslot) !  (   )
  R@ SP0 LOCAL @   R@ $0C +  ( sp0 dstkSlot) !
  R@ RP0 LOCAL @   POP $10 + ( rp0 rstkSlot) !  ;


( shadow 10  Multitasking )

TASK! ( word task -)

  Set (or reset) TASK so it will run the Forth WORD when it gets
  control.  The LINK, SP0, and RP0 slot have already been initialized.
  The TOS and RLOOP slots are "don't care".  We store the Forth WORD
  into the saved IP slot.  Then we set the saved DSTK and RSTK slots
  from the SP0 and RP0 slots.

  Note that TASK1 LINK $XX + LOCAL  is the same as  TASK1 $XX +


( block 11  Multitasker  Examples )

VARIABLE V1
VARIABLE V2

: TST1 ( -) BEGIN 1500 MS  1 V1 +!  AGAIN  ;  

: TST2 ( -) BEGIN  300 MS  1 V2 +!  AGAIN  ;  

(  ' TST1  TASK1 TASK!   )
(  ' TST2  TASK2 TASK!   )
(  TASK1 WAKE   TASK2 WAKE )

( shadow 11  Multitasking  Examples )


( block 12 bit constants )

VARIABLE RS 
VARIABLE RW 
VARIABLE E  

VARIABLE CNTL-LINES

VARIABLE HIGH-DATA 
VARIABLE LOW-DATA  

VARIABLE DBUS      
VARIABLE CG-ABUS   
VARIABLE DD-ABUS   

VARIABLE BUSY-BIT  

( shadow 12 constants )


( block 13 setters )

: SET-HIGH ( n addr -) 
  DUP @ ROT OR SWAP ! ;

: SET-LOW ( n addr -) 
  DUP @ ROT AND SWAP 
  DUP @ ROT - SWAP ! ;

: CYCLE ( -) 
  E @ DUP DUP 
  IO0DIR SET-HIGH 
  IO0PIN SET-HIGH
  30 MS 
  IO0PIN SET-LOW ;

( shadow 13 setters )


( block 14 CHAR ARRAY )

: CHAR-FROM-CELL ( a n -) 
  8 * DUP $FF SWAP LSHIFT ROT @ AND SWAP RSHIFT ;

: CHAR-FROM-ARRAY ( a n -) 
  4 U/MOD 4 * ROT + SWAP CHAR-FROM-CELL ;
( shadow 14 EMPTY )


( block 15 LCD control )

: BYTE-TO-LCD ( n -)    
  DBUS @ DUP IO0DIR SET-HIGH IO0PIN SET-LOW
  $10000 * IO0PIN SET-HIGH CYCLE ;

: INIT-LCD ( -) 
  DBUS @ DUP IO0DIR SET-HIGH IO0CLR SET-HIGH 
  CNTL-LINES @ DUP IO0DIR SET-HIGH IO0CLR SET-HIGH
  $38 BYTE-TO-LCD $0C BYTE-TO-LCD $01 BYTE-TO-LCD ;

: WRITE-TO-LCD ( n -) RS @ IO0PIN SET-HIGH 1 MS BYTE-TO-LCD 
  1 MS RS @ IO0PIN SET-LOW ;

: STRING-TO-LCD ( a n -) $80 OR BYTE-TO-LCD 
  COUNT FOR COUNT WRITE-TO-LCD NEXT ;

( shadow 15 LCD control )


( block 16 keypad control )

TABLE KEYS $34333231 , $38373635 , $23302A39 ,

VARIABLE CURRENT-KEY

: GET-KEY ( n -) KEYS SWAP CHAR-FROM-ARRAY ;

: INIT-PINS ( -)
  0 CURRENT-KEY ! $7F0000 PINSEL1 SET-LOW
  $700000 IO1DIR SET-LOW $0F0000 IO1DIR SET-HIGH
  $0F0000 IO1SET SET-HIGH ;

: DO-IF-KEY ( w n -) CURRENT-KEY @ = IF EXECUTE ELSE DROP THEN ;

( shadow 16 keypad control )


( block 17 keypad control 2 )

: SCAN-KEY-ROWS ( n -) 
  DUP IO1DIR SET-HIGH IO1SET SET-HIGH $F0000 IO1DIR SET-LOW 
  IO1PIN @ DUP
  $10000 AND IF 0 ELSE DUP $20000 AND IF 3 ELSE DUP
  $40000 AND IF 6 ELSE DUP $80000 AND IF 9 THEN SWAP DROP ;

: READ-STATE ( -) 
  0 IO1PIN @ DUP 
  $100000 AND IF $100000 SCAN-KEY-ROWS 0 + GET-KEY 
  CURRENT-KEY ! NIP 1 SWAP ELSE DUP 
  $200000 AND IF $200000 SCAN-KEY-ROWS 1 + GET-KEY 
  CURRENT-KEY ! NIP 1 SWAP ELSE DUP 
  $400000 AND IF $400000 SCAN-KEY-ROWS 2 + GET-KEY 
  CURRENT-KEY ! NIP 1 SWAP THEN DROP ;



( shadow 17 keypad control 2 )

( block 18 random number generator )

VARIABLE M_W  
VARIABLE M_Z  

: GEN-NUM ( -)
  M_Z @ 65535 AND 36969 * M_Z @ 16 RSHIFT + M_Z !
  M_W @ 65535 AND 18000 * M_W @ 16 RSHIFT + M_W !
  M_Z @ 16 LSHIFT M_W @ 65535 AND + ;

: RAND ( n -) GEN-NUM SWAP U/MOD ;
( block 19 STATE-MACHINE)

VARIABLE DRAW VARIABLE DISPLAY-ACTION VARIABLE INPUT-ACTION
VARIABLE ERROR-MSG VARIABLE CURRENT-STATE VARIABLE NEXT-STATE
VARIABLE FLIP-DASH

: ERROR-ACTION ( -) ERROR-MSG @ 60 STRING-TO-LCD ;

: EXIT-STATE ( -) NEXT-STATE @ EXECUTE ;


: BUSY-DASH ( -) FLIP-DASH @ IF " - - - - - - - - - - " 
  84 STRING-TO-LCD 0 FLIP-DASH ! ELSE "  - - - - - - - - - -"
  84 STRING-TO-LCD 1 FLIP-DASH ! THEN ;
( shadow 19 STATE-MACHINE)

( block 20 MAIN)

: CONSUME-INPUT ( -) CURRENT-KEY @ WRITE-TO-LCD INIT-PINS 200 MS ;

: POLL ( -)  
  BEGIN DISPLAY-ACTION @ EXECUTE IO1PIN @ $700000 AND 
  IF READ-STATE IF INPUT-ACTION @ EXECUTE
  THEN THEN AGAIN ;

: INIT-APP ( -)
  $100 RS ! $200 RW ! $400 E ! $700 CNTL-LINES !                  
  $F00000 HIGH-DATA ! $0F0000 LOW-DATA ! $FF0000 DBUS !    
  $3F0000 CG-ABUS ! $7F0000 DD-ABUS ! $800000 BUSY-BIT !
  $742A4D1F M_W ! $4D4E1C22 M_Z ! ;

( shadow 20 )
( block 21  )

: INIT-DICE-STATE ( -) ;

( shadow 21 MAIN LOOP )
( block 22 SEED ENTRY )

: SEED-DISPLAY ( -) DRAW @ IF 0 DRAW ! 
  $01 BYTE-TO-LCD
  " Enter Seeds:" 0 STRING-TO-LCD   
  " SEED 1: ______ " 40 STRING-TO-LCD 
  " SEED 2: ______ " 20 STRING-TO-LCD THEN ; 

: SEED-INPUT ( -) ' ERROR-ACTION $30 DO-IF-KEY
  ledOn 2000 MS ledOff INIT-PINS ; 

: INIT-SEED-STATE ( -) 
  1  DRAW ! ' SEED-DISPLAY DISPLAY-ACTION !
  ' SEED-INPUT INPUT-ACTION ! ' INIT-DICE-STATE NEXT-STATE ! 
  " Invalid Seed"  ERROR-MSG ! ;
  

( shadow 22 SEED ENTRY )

( block 23 SPLASH )

: SPLASH-DISPLAY ( -) " Big Roller" 5 STRING-TO-LCD 
  " By Michael Compton" 21 STRING-TO-LCD ' BUSY-DASH 
  DISPLAY-ACTION ! ;

: SPLASH-INPUT ( -) ' EXIT-STATE $30 DO-IF-KEY
  ledOn 1000 MS ledOff INIT-PINS 100 MS ;

: INIT-SPLASH-STATE ( -)
  INIT-APP INIT-LCD INIT-PINS 1  DRAW !
  ' SPLASH-DISPLAY DISPLAY-ACTION ! ' SPLASH-INPUT INPUT-ACTION ! 
  ' INIT-SEED-STATE NEXT-STATE ! ;

( shadow 23 )
( block 24 )
SAVE-IMAGE kernel INIT

( shadow 24 )
( block 25 )
( shadow 25 )
( shadow 25 random number generator )

