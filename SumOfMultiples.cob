       IDENTIFICATION DIVISION.
       PROGRAM-ID. SumOfMultiples.
       AUTHOR. io.github.scrvrdn.
      * Leetcode problem 2625: Given a positive integer n, find the sum
      * of all integers in the range [1, n] inclusive that are divisible
      * by 3, 5, or 7.
      * Return an integer denoting the sum of all numbers in the given
      * range satisfying the constraint.
       
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Num PIC 9(4).
       01 SumOfMultiples PIC 9(9) VALUE ZEROS.
       01 CurrentMultiple PIC 9(9).
       01 Counter PIC 9(4).
       01 PrintSum PIC ZZZ,ZZZ,ZZZ.

       PROCEDURE DIVISION.
       Main.
           DISPLAY "Enter a number >= 1: " WITH NO ADVANCING
           ACCEPT Num
           
           PERFORM VARYING Counter FROM 1 BY 1 UNTIL Counter > Num
              EVALUATE TRUE
                 WHEN FUNCTION MOD (Counter, 3) = 0
                    ADD Counter TO SumOfMultiples
                 WHEN FUNCTION MOD (Counter, 5) = 0
                    ADD Counter TO SumOfMultiples
                 WHEN FUNCTION MOD (Counter, 7) = 0
                    ADD Counter TO SumOfMultiples
               END-EVALUATE 
           END-PERFORM
           
           MOVE SumOfMultiples TO PrintSum
           DISPLAY PrintSum
           STOP RUN.
