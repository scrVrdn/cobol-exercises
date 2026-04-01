       IDENTIFICATION DIVISION.
       PROGRAM-ID. SummaryReport-1.
       AUTHOR. io.github.scrvrdn.
      * Programming exercise 1 from: Coughlan (2014), ch. 8
      
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EntrantsFile ASSIGN TO "Entrants.dat"
                      ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD EntrantsFile.
       01 EntrantsRec.
           88 EndOfFile VALUE HIGH-VALUES.
           02 StudentId PIC 9(8).
           02 CourseCode PIC X(5).
           02 Gender PIC X.

       WORKING-STORAGE SECTION.
       01 PrintHeading1 PIC X(30)
           VALUE "   First Year Entrants Summary".
     
       01 PrintHeading2 PIC X(30)
           VALUE "  Course Code    NumOfStudents". 
     
       01 CourseLine.
           02 FILLER PIC X(5) VALUE SPACES.
           02 PrintCourseCode PIC X(5).
           02 FILLER PIC X(10) VALUE SPACES.
           02 PrintNumOfStudents PIC 9(5).
     
       01 PrevCourseCode PIC X(5).
       01 NumOfStudents PIC 9(5).
       01 TotalStudents PIC 9(5).
       01 PrintTotalStudents.
           02 FILLER PIC X(16) VALUE "Total Students: ".
           02 PrintTotalNum PIC 9(5).

       PROCEDURE DIVISION.
       Main.
           DISPLAY PrintHeading1
           DISPLAY PrintHeading2
                      
           OPEN INPUT EntrantsFile
           MOVE CourseCode TO PrevCourseCode
           READ EntrantsFile
               AT END SET EndOfFile TO TRUE
           END-READ

           PERFORM DisplayLine UNTIL EndOfFile
           
           MOVE TotalStudents TO PrintTotalNum
           DISPLAY PrintTotalStudents

           CLOSE EntrantsFile
           STOP RUN.
       
       DisplayLine.
           MOVE CourseCode TO PrintCourseCode

           PERFORM UNTIL CourseCode NOT = PrevCourseCode
               ADD 1 TO NumOfStudents
               MOVE CourseCode TO PrevCourseCode
               READ EntrantsFile
                   AT END SET EndOfFile TO TRUE
               END-READ
           END-PERFORM

           MOVE NumOfStudents TO PrintNumOfStudents
           ADD NumOfStudents TO TotalStudents
           MOVE ZEROS TO NumOfStudents
           DISPLAY CourseLine.
