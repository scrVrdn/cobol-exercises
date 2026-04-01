       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloCobol.
       AUTHOR. io.github.scrvrdn.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NameVariable PIC A(5).
       PROCEDURE DIVISION.
       Begin.
           MOVE "COBOL" TO NameVariable.
           DISPLAY "Hello ", NameVariable, "!".
           STOP RUN.
