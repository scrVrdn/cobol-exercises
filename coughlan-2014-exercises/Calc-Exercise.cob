       IDENTIFICATION DIVISION.
       PROGRAM-ID. Calc-Exercise.
       AUTHOR. io.github.scrvrdn.
      * Programming exercise from: Coughlan (2014), ch. 5

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 Num1 PIC 9.
       01 Num2 PIC 9.
       01 Result PIC S99V99.
       01 Operator PIC X.
           88 AdditionOperator VALUE "+".
           88 SubtractionOperator VALUE "-".
           88 MultiplicationOperator VALUE "*".
           88 DivisionOperator VALUE "/".
           88 ValidOperator VALUE "+", "-", "*", "/".

       PROCEDURE DIVISION.
       Main.
       
           DISPLAY "Enter a single digit number - " WITH NO ADVANCING
           ACCEPT Num1
           DISPLAY "Enter a single digit number - " WITH NO ADVANCING
           ACCEPT Num2
           DISPLAY "Enter operator - " WITH NO ADVANCING
           ACCEPT Operator
           EVALUATE TRUE
               WHEN AdditionOperator ADD Num1 TO Num2 GIVING Result
               WHEN SubtractionOperator SUBTRACT Num1 FROM Num2 GIVING
                 Result
               WHEN MultiplicationOperator MULTIPLY Num1 BY Num2
                 GIVING Result
               WHEN DivisionOperator DIVIDE Num1 By Num2 GIVING Result
           END-EVALUATE
    
           IF ValidOperator
               DISPLAY Result
           END-IF
           STOP RUN.
