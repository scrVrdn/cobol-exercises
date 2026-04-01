       IDENTIFICATION DIVISION.
       PROGRAM-ID. HelloCobol.
       AUTHOR. io.github.scrvrdn.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Name PIC A(5).
       PROCEDURE DIVISION.
       Begin.
           MOVE "COBOL" TO Name.
           DISPLAY "Hello ", Name, "!".
           STOP RUN.
