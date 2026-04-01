       IDENTIFICATION DIVISION.
       PROGRAM-ID. SumOfMultiples-2.
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
       01 CurrentMultiple PIC 999.
       01 SumOfMultiples PIC 9(9) VALUE ZEROS.
       01 TempSum PIC 9(9).
       01 Temp PIC 9(4).
       01 PrintSum PIC ZZZ,ZZZ,ZZZ.

       PROCEDURE DIVISION.
       Main.
           DISPLAY "Enter a number >= 1: " WITH NO ADVANCING
           ACCEPT Num

           MOVE 3 TO CurrentMultiple
           PERFORM CalcSum
           ADD TempSum TO SumOfMultiples

           MOVE 5 TO CurrentMultiple
           PERFORM CalcSum
           ADD TempSum TO SumOfMultiples

           MOVE 7 TO CurrentMultiple
           PERFORM CalcSum
           ADD TempSum TO SumOfMultiples

           MOVE 105 TO CurrentMultiple
           PERFORM CalcSum
           ADD TempSum TO SumOfMultiples

           MOVE 21 TO CurrentMultiple
           PERFORM CalcSum
           SUBTRACT TempSum FROM SumOfMultiples

           MOVE 35 TO CurrentMultiple
           PERFORM CalcSum
           SUBTRACT TempSum FROM SumOfMultiples
           
           MOVE SumOfMultiples TO PrintSum
           DISPLAY PrintSum

           STOP RUN.

       CalcSum.
           DIVIDE Num BY CurrentMultiple GIVING Temp
           COMPUTE TempSum = CurrentMultiple * Temp * (Temp + 1) / 2.
