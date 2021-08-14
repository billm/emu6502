*
* HELLO WORLD TEST
*
          ORG $300

          LDX #0
PRNEXT    LDA TEXT,X
          BEQ DONE
          JSR $1000      ; PRINTA
          INX
          BNE PRNEXT
DONE      BRK            ; JUST STOP
TEXT      ASC 'HELLO, WORLD!'
          HEX 0D00