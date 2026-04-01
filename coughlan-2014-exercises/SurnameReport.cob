       IDENTIFICATION DIVISION.
       PROGRAM-ID. SurnameReport.
       AUTHOR. io.github.scrvrdn.
      * Programming exercise 1 from: Coughlan (2014), ch. 9

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CensusFile ASSIGN TO "Census.dat"
              ORGANIZATION IS SEQUENTIAL.

           SELECT SurnameReport ASSIGN TO "SurnameReport.rpt"
              ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD CensusFile.
       01 CensusRec.
           88 EndOfCensusFile VALUE HIGH-VALUES.
           02 CensusNumber PIC 9(8).
           02 Surname PIC X(20).
           02 CountyName PIC X(9).

       FD SurnameReport.
       01 PrintLine PIC X(40).
           
       
       WORKING-STORAGE SECTION.
       01 HeadingLine PIC B(9)X(22)B(9) VALUE "Popular Surname Report".
       01 ColHeaders.
           02 FILLER PIC X(10)BB VALUE "CountyName".
           02 FILLER PIC X(7)B(15) VALUE "Surname".
           02 FILLER PIC X(5) VALUE "Count".
       01 Footer PIC X(40)
           VALUE "************ end of report *************".

       01 SurnameLine.
           02 PrintCountyName PIC X(9)BBB.
           02 PrintSurname PIC X(20)BB.
           02 PrintSurnameCount PIC ZZZ,ZZ9.       
       
       01 PopularSurname PIC X(20).
       01 PopularSurnameCount PIC 9(6).

       01 PrevSurname PIC X(20).
       01 PrevCounty PIC X(9).
       01 CurrentCount PIC 9(6).

       PROCEDURE DIVISION.
       Main.
           OPEN INPUT CensusFile
           OPEN OUTPUT SurnameReport
           WRITE PrintLine FROM HeadingLine AFTER ADVANCING 1 LINE
           WRITE PrintLine FROM ColHeaders AFTER ADVANCING 2 LINES

           READ CensusFile
              AT END SET EndOfCensusFile TO TRUE
           END-READ

           PERFORM ProcessCounties UNTIL EndOfCensusFile
           WRITE PrintLine FROM Footer AFTER ADVANCING 2 LINES

           CLOSE CensusFile, SurnameReport
           STOP RUN.

       ProcessCounties.
           MOVE CountyName TO PrevCounty, PrintCountyName
           PERFORM GetSurnameCount UNTIL CountyName NOT = PrevCounty
           
           MOVE PopularSurname TO PrintSurname
           MOVE PopularSurnameCount TO PrintSurnameCount

           WRITE PrintLine FROM SurnameLine AFTER ADVANCING 1 LINE.

       GetSurnameCount.
           MOVE Surname TO PrevSurname
           MOVE ZEROS TO PopularSurnameCount

           PERFORM VARYING CurrentCount FROM 0 BY 1
                 UNTIL Surname NOT = PrevSurname
                    OR CountyName NOT = PrevCounty 

              READ CensusFile
                 AT END SET EndOfCensusFile TO TRUE
              END-READ
           END-PERFORM

           IF CurrentCount > PopularSurnameCount
              MOVE PrevSurname TO PopularSurname
              MOVE CurrentCount TO PopularSurnameCount 
           END-IF.
           