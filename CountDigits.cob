       IDENTIFICATION DIVISION. 
       PROGRAM-ID. CountDigits.
       AUTHOR. io.github.scrvrdn.
      * Leetcode problem 2520: Given an integer num,
      * return the number of digits in num that divide num.

       ENVIRONMENT DIVISION.

       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       01 Number PIC 9(9).
       01 Digit PIC 9.
       01 TempNumber PIC 9(9).
       01 Counter PIC 9.

       PROCEDURE DIVISION.
       Main.
           DISPLAY "Enter a number: " WITH NO ADVANCING
           ACCEPT Number

           MOVE Number TO TempNumber

           PERFORM CountDigits
           DISPLAY Counter
           MOVE ZEROS TO Counter

           STOP RUN.

       CountDigits.
           PERFORM UNTIL TempNumber = 0
               DIVIDE TempNumber BY 10 GIVING TempNumber REMAINDER Digit
               IF FUNCTION MOD (Number, Digit) = 0
                  ADD 1 TO Counter 
               END-IF
           END-PERFORM.