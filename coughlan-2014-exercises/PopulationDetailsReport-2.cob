       IDENTIFICATION DIVISION.
       PROGRAM-ID. PopulationDetailsReport-2.
       AUTHOR. io.github.scrvrdn.
      * Programming exercise from: Coughlan (2014), ch. 11

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CensusFile ASSIGN TO "CensusFile.dat"
              ORGANIZATION IS SEQUENTIAL.

           SELECT PopulationReport ASSIGN TO "PopulationReport.rpt"
              ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD CensusFile.
       01 CensusRec.
           88 EndOfCensusFile VALUE HIGH-VALUES.
           02 StateNum PIC 99.
           02 Age PIC 9.
              88 Child VALUE 1.
              88 Teen VALUE 2.
              88 Adult VALUE 3.
           02 Gender PIC 9.
              88 Female VALUE 1.
              88 Male VALUE 2.
           02 CarOwner PIC X.
              88 OwnsCar VALUE "Y".
              88 OwnsNoCar VALUE "N".
       
       FD PopulationReport.
       01 ReportLine PIC X(82).

       WORKING-STORAGE SECTION.
       01 ReportHeader.
           02 FILLER PIC X(18) VALUE SPACES.
           02 FILLER PIC X(25) VALUE "Population Details Report".

       01 ColHeaders1.
           02 FILLER PIC X(5) VALUE "State".
           02 FILLER PIC X(4) VALUE SPACES.
           02 FILLER PIC X(3) VALUE "Car".
           02 FILLER PIC X(8) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Male".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Female".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Male".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Female".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(4) VALUE "Male".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Female".

       01 ColHeaders2.
           02 FILLER PIC XXX VALUE "Num".
           02 FILLER PIC X(4) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Owners".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Adults".
           02 FILLER PIC X(5) VALUE SPACES.
           02 FILLER PIC X(6) VALUE "Adults".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(5) VALUE "Teens".
           02 FILLER PIC X(6) VALUE SPACES.
           02 FILLER PIC X(5) VALUE "Teens".
           02 FILLER PIC XXX VALUE SPACES.
           02 FILLER PIC X(8) VALUE "Children".
           02 FILLER PIC XXX VALUE SPACES.
           02 FILLER PIC X(8) VALUE "Children".

       01 PrintLine.
           02 StateNum PIC 99.
           02 Filler PIC X(4) VALUE SPACES.
           02 PrintCarOwners PIC Z,ZZZ,ZZZ,ZZ9.
           02 MaleAdults PIC BZ,ZZZ,ZZZ,ZZ9.
           02 FemaleAdults PIC BZ,ZZZ,ZZZ,ZZ9.
           02 MaleTeens PIC BZ,ZZZ,ZZZ,ZZ9.
           02 FemaleTeens PIC BZ,ZZZ,ZZZ,ZZ9.
           02 MaleChildren PIC BZ,ZZZ,ZZZ,ZZ9.
           02 FemaleChildren PIC BZ,ZZZ,ZZZ,ZZ9.

       01 US-PopulationTable.
           02 State OCCURS 50 TIMES.
              03 PopCarOwners PIC 9(8).
              03 AgeCategory OCCURS 3 TIMES.
                 04 GenderCategory OCCURS 2 TIMES.
                    05 PopTotal PIC 9(8).

       01 StateIdx PIC 99.

       PROCEDURE DIVISION.
       Main.
           MOVE ZEROS TO US-PopulationTable.
           OPEN INPUT CensusFile.
           OPEN OUTPUT PopulationReport.

           READ CensusFile
              AT END SET EndOfCensusFile TO TRUE
           END-READ
           
           PERFORM UNTIL EndOfCensusFile
              ADD 1 TO PopTotal(StateNum OF CensusRec, Age, Gender)
              IF OwnsCar
                 ADD 1 TO PopCarOwners(StateNum OF CensusRec)
              END-IF
           END-PERFORM

           PERFORM WriteReport

           CLOSE CensusFile, PopulationReport
           STOP RUN.

       WriteReport.
           WRITE ReportLine FROM ReportHeader AFTER ADVANCING PAGE
           WRITE ReportLine FROM ColHeaders1 AFTER ADVANCING 2 LINES
           WRITE ReportLine FROM ColHeaders2 AFTER ADVANCING 1 Line
           
           PERFORM VARYING StateIdx FROM 1 BY 1 UNTIL StateIdx > 50
              MOVE StateIdx TO StateNum OF PrintLine
              MOVE PopCarOwners(StateIdx) TO PrintCarOwners
              MOVE PopTotal(StateIdx, 2, 3) TO MaleAdults
              MOVE PopTotal(StateIdx, 1, 3) TO FemaleAdults
              MOVE PopTotal(StateIdx, 2, 2) TO MaleTeens
              MOVE PopTotal(StateIdx, 1, 2) TO FemaleTeens
              MOVE PopTotal(StateIdx, 2, 1) TO MaleChildren
              MOVE PopTotal(StateIdx, 1, 1) TO FemaleChildren

              WRITE ReportLine FROM PrintLine AFTER ADVANCING 1 LINE
           END-PERFORM.
