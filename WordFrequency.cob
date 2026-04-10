       IDENTIFICATION DIVISION.
       PROGRAM-ID. WordFrequency.
       AUTHOR. io.github.scrvrdn.
      * Programming exercise 1 from: Coughlan (2014), ch. 13

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT Document ASSIGN TO "DocWords.dat"
              ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD Document.
       01 DocumentRec PIC X(20).
           88 EndOfFile VALUES HIGH-VALUES.

       WORKING-STORAGE SECTION.
       01 WordsTable.
           02 NumOfWords PIC 9(4) VALUE ZERO.
           02 CollectedWords OCCURS 0 TO 1000 TIMES
                    DEPENDING ON NumOfWords
                    INDEXED BY Idx.
              03 Word PIC X(20).
              03 Freq PIC 9(4).

       01 TopTen.
           02 TopTenWords OCCURS 11 TIMES INDEXED BY TT-Idx.
              03 TT-Word PIC X(20) VALUE SPACES.
              03 TT-Freq PIC 9(4) VALUE ZEROS.
       
       01 PrintHeader PIC X(25) VALUE "Top Ten Words In Document".
       01 PrintColHeadings.
           02 Pos PIC XXX VALUE "Pos".
           02 FILLER PIC XXX VALUE ZEROS.
           02 Occurrence PIC X(6) VALUE "Occurs".
           02 FILLER PIC XXX VALUE ZEROS.
           02 DocumentWord PIC X(13) VALUE "Document Word".

       01 PrintLine.
           02 PrintPos PIC Z9.
           02 FILLER PIC XB(4) VALUE ".".
           02 PrintOccurs PIC 9(4).
           02 FILLER PIC X(5) VALUE ZEROS.
           02 PrintWord PIC X(20).

       01 TT-Position PIC 99.

       PROCEDURE DIVISION.
       Main.
           OPEN INPUT Document
           READ Document
              AT END SET EndOfFile TO TRUE
           END-READ
           
           PERFORM ReadWords
           PERFORM FindTopTen
           PERFORM DisplayTopTen

           STOP RUN.

       ReadWords.
           PERFORM UNTIL EndOfFile
              SET Idx TO 1
              SEARCH CollectedWords
                 AT END ADD 1 TO NumOfWords
                    MOVE FUNCTION LOWER-CASE(DocumentRec) TO Word(Idx)
                    ADD 1 TO Freq(Idx)
                 WHEN FUNCTION LOWER-CASE(DocumentRec) = Word(Idx)
                    ADD 1 TO Freq(Idx)
              END-SEARCH
              
              READ Document
                 AT END SET EndOfFile TO TRUE
              END-READ
           END-PERFORM.

       FindTopTen.
           PERFORM VARYING Idx FROM 1 BY 1 UNTIL Idx > NumOfWords
              PERFORM VARYING TT-Idx FROM 10 BY -1 UNTIL TT-Idx = 0
                 IF Freq(Idx) > TT-Freq(TT-Idx)
                    MOVE TopTenWords(TT-Idx) TO TopTenWords(TT-Idx + 1)
                    MOVE CollectedWords(Idx) TO TopTenWords(TT-Idx)
                 END-IF
              END-PERFORM
           END-PERFORM.


       DisplayTopTen.
           DISPLAY PrintHeader 
           DISPLAY PrintColHeadings
           PERFORM VARYING TT-Idx FROM 1 BY 1 UNTIL TT-Idx > 10
              SET TT-Position To TT-Idx
              MOVE TT-Position TO PrintPos
              MOVE TT-Freq(TT-Idx) TO PrintOccurs
              MOVE TT-Word(TT-Idx) TO PrintWord
              DISPLAY PrintLine
           END-PERFORM.
