       IDENTIFICATION DIVISION.
       PROGRAM-ID. BURTEFORCESORT.
       AUTHOR. GREG. MANLEY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT F1
           ASSIGN TO "test.txt".

       DATA DIVISION.
       FILE SECTION.
       FD F1.
       01 FILE-INPUT.
           05 NUM PIC 9(3).

       WORKING-STORAGE SECTION.
       01  WS-CR               PIC X VALUE X'0D'.
       01  ARRAY-SIZE          PIC S9(4) COMP VALUE 10.
       01  INTEGER-ARRAY.
           05  ARRAY-ELEM      OCCURS 10 TIMES PIC 9(3).
       77  ARRAY-INDEX         PIC 9 VALUE 1.
       77  COUNTS              PIC 99 VALUE 0.

       01  WS-VAR.
           05 WS-EOF-SW        PIC X(1).
               88 WS-EOF       VALUE 'Y'.
               88 WS-NOT-EOF   VALUE 'N'.

       PROCEDURE DIVISION.
           OPEN INPUT F1.
           PERFORM READ-FILE.
           CLOSE F1.

           PERFORM VARYING COUNTS FROM 1 BY 1 UNTIL COUNTS>10
               DISPLAY ARRAY-ELEM (COUNTS)
               DISPLAY COUNTS
           END-PERFORM.
           STOP RUN.
       
       READ-FILE.
           PERFORM UNTIL WS-EOF
               READ F1
                   NEXT RECORD
                       AT END SET WS-EOF TO TRUE
                   NOT AT END 
                       IF FILE-INPUT IS NOT = WS-CR THEN
                           MOVE FILE-INPUT TO ARRAY-ELEM (ARRAY-INDEX)
                           ADD 1 TO ARRAY-INDEX GIVING ARRAY-INDEX
      *                     DISPLAY FILE-INPUT
                       END-IF
               END-READ
           END-PERFORM.
