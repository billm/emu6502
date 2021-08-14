*=$300        ; Set memory location to 0x0300
  ldx #0
printnext:
  lda text, X
  beq done
  jsr $FDED   ; print char in A (apple 2 location)
  inx
  bne printnext
done:
  brk         ; Exit
text:
  .byte "hello world", $0d, 0