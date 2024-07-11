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
       01  ARRAY-SIZE          PIC S9(4) COMP VALUE 10.
       01  INTEGER-ARRAY       OCCURS 10 TIMES.
           05  ARRAY-ELEM      PIC 9(3).
       77 ARRAY-INDEX PIC 9 VALUE 0.

       01 WS-VAR.
           05 WS-EOF-SW   PIC X(1).
               88 WS-EOF       VALUE 'Y'.
               88 WS-NOT-EOF   VALUE 'N'.

       PROCEDURE DIVISION.
           OPEN INPUT F1.
           PERFORM UNTIL WS-EOF
               READ F1
                   NEXT RECORD
                       AT END SET WS-EOF TO TRUE
                   NOT AT END DISPLAY FILE-INPUT
               END-READ
           END-PERFORM.
           CLOSE F1.
       