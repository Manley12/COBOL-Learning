       IDENTIFICATION DIVISION.
       PROGRAM-ID. PITOTHENTH.
       AUTHOR. GREG. MANLEY.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 DIGITS       PIC 99.  
       77 PI           PIC 9V9(34).

       77 K            PIC 999 VALUE 1.
       77 A-K          PIC 9(7).
       77 A-SUM        PIC 9(7).
       77 B-SUM        PIC 9 VALUE 0.
       77 C            PIC 9(10) VALUE 640320.
       77 C3-OVER-24   PIC 9(10).
       77 TEMP         PIC 9(10).
       77 TEMP1        PIC 9(10).
       77 TEMP2        PIC 9(10)V9(4).

       PROCEDURE DIVISION.
           ACCEPT DIGITS FROM COMMAND-LINE.

           IF DIGITS = 0 THEN
               DISPLAY "INVALID INPUT"
               STOP RUN
           ELSE
               ADD DIGITS TO A-K GIVING A-K.
               ADD DIGITS TO A-SUM GIVING A-SUM.
               PERFORM PIESTIMATOR.
               DISPLAY DIGITS.
               DISPLAY PI.
           GOBACK.

       PIESTIMATOR.
           COMPUTE C3-OVER-24 = (C**3).
           DIVIDE C3-OVER-24 BY 24 GIVING TEMP REMAINDER C3-OVER-24.
           PERFORM LOOP UNTIL A-K=0. 
           COMPUTE TEMP1 = 13591409*A-SUM + 545140134*B-SUM.
           COMPUTE TEMP2 = 426880*(10005*DIGITS*DIGITS)**0.5
           DIVIDE TEMP2 BY TEMP1 GIVING PI.

       LOOP.
           COMPUTE TEMP1 = -(6 * K - 5) * (2 * K - 1) * (6 * K - 1).
           COMPUTE TEMP2 = K * K * K * C3-OVER-24.
           DIVIDE TEMP2 BY A-K GIVING TEMP2 REMAINDER A-K.
           ADD A-SUM TO A-K GIVING A-SUM.
           COMPUTE B-SUM = B-SUM + (K * A-K).
           ADD K TO 1 GIVING K.
           DISPLAY A-K.
