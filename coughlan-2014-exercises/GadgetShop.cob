       IDENTIFICATION DIVISION.
       PROGRAM-ID GadgetShop.
       AUTHOR. io.github.scrvrdn.
      * Programming exercise 1 and 2 from: Coughlan (2014), ch. 7

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT StockFile ASSIGN TO "GadgetStock.dat"
               ORGANIZATION IS SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD StockFile.
       01 StockRecord.
           88 EndOfStockFile VALUE HIGH-VALUES.
           02 GadgedID PIC 9(6).
           02 GadgetName PIC X(30).
           02 QtyInStock PIC 9(4).
           02 Price PIC 9(4)V99.

       WORKING-STORAGE SECTION.
       01 TotalValue PIC 9(10)V99.
       01 CurrentValue PIC 9(10)V99.

       PROCEDURE DIVISION.
       Main.

           PERFORM AddRecords
           PERFORM DisplayRecords

           STOP RUN.
           
       AddRecords.
           OPEN EXTEND StockFile
           
           MOVE "313245Spy Pen - HD Video Camera     0125003099"
               TO StockRecord
           WRITE StockRecord

           MOVE "593486Scout Cash Capsule - Red      1234000745"
               TO StockRecord
           WRITE StockRecord

           CLOSE StockFile.
        
       DisplayRecords.
           OPEN INPUT StockFile

           READ StockFile
               AT END SET EndOfStockFile TO TRUE
           END-READ

           PERFORM UNTIL EndOfStockFile
               MULTIPLY QtyInStock BY Price GIVING CurrentValue
               DISPLAY GadgetName SPACE CurrentValue
               ADD CurrentValue TO TotalValue
               READ StockFile
                   AT END SET EndOfStockFile TO TRUE
               END-READ
           END-PERFORM

           CLOSE StockFile

           DISPLAY "Stock Total: " TotalValue.
