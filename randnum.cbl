       IDENTIFICATION DIVISION.
       PROGRAM-ID. RANDNUMGEN.
       AUTHOR. GREG. MANLEY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * LIVE VARIABLES
       77 RAND-SEED    PIC 9(9)        COMPUTATIONAL.
       77 ANSWER       PIC 9999V9(2)    COMPUTATIONAL.
       77 NEX          PIC 9(9)        COMPUTATIONAL.

       01 WS-CURRENT-DATE-FIELDS.
           05 WS-CURRENT-DATE.
               10 WS-CURRENT-YEAR      PIC  9(4).
               10 WS-CURRENT-MONTH     PIC  9(2).
               10 WS-CURRENT-DAY       PIC  9(2).
           05 WS-CURRENT-TIME.
               10 WS-CURRENT-HOUR      PIC  9(2).
               10 WS-CURRENT-MINUTE    PIC  9(2).
               10 WS-CURRENT-SECOND    PIC  9(2).
               10 WS-CURRENT-MS        PIC  9(2).
           05 WS-DIFF-FROM-GMT         PIC S9(4).

       PROCEDURE DIVISION.
           DISPLAY "RANDOM NUMBER GENERATOR".
           DISPLAY " ".
           PERFORM RAND.
           DISPLAY "YOUR RANDOM NUMBER IS: ", ANSWER.
           
       RAND.
           PERFORM GET-SEED.
           ADD RAND-SEED TO 12345 GIVING NEX.
           MULTIPLY NEX BY 1103245 GIVING NEX.
           DIVIDE NEX BY 5000 GIVING ANSWER.

       GET-SEED.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-MS TO NEX.
           MULTIPLY NEX BY 5555555 GIVING NEX.
           MOVE NEX TO RAND-SEED. 
           MOVE 0 TO NEX.
