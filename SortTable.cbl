       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SORT-TABLE.
          05 SORT-MIN-VALUE           PIC  9(9) VALUE 999999999.
          05 SORT-MAX-VALUE           PIC  9(9) VALUE 0.
          05 SORT-TEMP                PIC S9(9) COMP-3.
          05 SORT-SIZE                PIC S9(9) COMP-3
                                                VALUE 50000.
          05 SUB1                     PIC S9(9) COMP-3.
          05 SUB2                     PIC S9(9) COMP-3.
          05 SUB2A                    PIC S9(9) COMP-3.
          05 SUB3                     PIC S9(9) COMP-3.
          05 SUB1-START-VALUE         PIC S9(9) COMP-3.
          05 SUB2-START-VALUE         PIC S9(9) COMP-3.

          05 SORT-VALUES-C.
             10 SORT-VALUE-C          PIC 9(9) COMP-3
                                               OCCURS 1 TO 100000
                                               TIMES
                                               DEPENDING ON SORT-SIZE.

          05  SORT-VALUES.
             10  SORT-VALUE           PIC 9(9) COMP-3
                                               OCCURS 1 TO 100000
                                               TIMES
                                               DEPENDING ON SORT-SIZE.
       01  WS-VARIABLES.
          05  INCREMENT               PIC S9(9) COMP-3.
          05  WS-TIME.
             10  HH                   PIC 99.
             10  MM                   PIC 99.
             10  SS                   PIC 99.
             10  DD                   PIC 99.
      *   05 WS-TIME-DISPLAY.
          05 WS-TIME-DISPLAY.
             10  HH                   PIC 99.
             10  FILLER               PIC X VALUE ':'.
             10  MM                   PIC 99.
             10  FILLER               PIC X VALUE ':'.
             10  SS                   PIC 99.
             10  FILLER               PIC X VALUE '.'.
             10  DD                   PIC 99.

       LINKAGE SECTION.

       PROCEDURE DIVISION.

       PROGRAM-START.
          PERFORM
             VARYING SUB1 FROM 1 BY 1
                UNTIL SUB1 > SORT-SIZE
                   COMPUTE SORT-VALUE-C (SUB1)
                           = FUNCTION RANDOM * 1000000000
                   IF SORT-VALUE-C (SUB1) <       SORT-MIN-VALUE
                   THEN
                      MOVE SORT-VALUE-C (SUB1) TO SORT-MIN-VALUE
                   END-IF
                   IF SORT-VALUE-C (SUB1) >       SORT-MAX-VALUE
                   THEN
                      MOVE SORT-VALUE-C (SUB1) TO SORT-MAX-VALUE
                   END-IF
          END-PERFORM.

          DISPLAY ' MIN VALUE =' SORT-MIN-VALUE
                  ' MAX VALUE =' SORT-MAX-VALUE
                  ' SORT SIZE =' SORT-SIZE.

          MOVE SORT-VALUES-C TO SORT-VALUES.

          ACCEPT WS-TIME FROM TIME.
          MOVE CORRESPONDING WS-TIME TO WS-TIME-DISPLAY.
          DISPLAY ' '
          DISPLAY 'START TIME OF BUBBLE SORT ' WS-TIME-DISPLAY.

      *   TRUE BUBBLE SORT
          PERFORM
             VARYING SUB1 FROM SORT-SIZE BY -1
             UNTIL SUB1 < 2
                PERFORM
                   VARYING SUB2 FROM 1 BY 1
                   UNTIL SUB2 = SUB1
                      COMPUTE SUB3 = SUB2 + 1
                      IF SORT-VALUE (SUB2)
                         > SORT-VALUE (SUB3)
                      THEN
                         MOVE SORT-VALUE (SUB2) TO SORT-TEMP
                         MOVE SORT-VALUE (SUB3)
                                        TO SORT-VALUE (SUB2)
                         MOVE SORT-TEMP TO SORT-VALUE (SUB3)
                      END-IF
                END-PERFORM
          END-PERFORM.

          ACCEPT WS-TIME FROM TIME.
          MOVE CORRESPONDING WS-TIME TO WS-TIME-DISPLAY.
          DISPLAY 'END   TIME OF BUBBLE SORT ' WS-TIME-DISPLAY.

          PERFORM
             VARYING SUB1 FROM 1 BY 1
             UNTIL SUB1 >= SORT-SIZE
                COMPUTE SUB2 = SUB1 + 1
                IF SORT-VALUE (SUB1) > SORT-VALUE (SUB2)
                THEN
                   DISPLAY 'ERROR IN SORT BETWEEN ' SUB1 ' AND ' SUB2
                   COMPUTE SUB1 = SORT-SIZE
                END-IF
          END-PERFORM.

          MOVE SORT-VALUES-C TO SORT-VALUES.

          ACCEPT WS-TIME FROM TIME.
          MOVE CORRESPONDING WS-TIME TO WS-TIME-DISPLAY.
          DISPLAY ' '.
          DISPLAY 'START TIME OF JUMPUP SORT ' WS-TIME-DISPLAY.



      *   JUMP UP SORT
          PERFORM
             VARYING SUB1 FROM 1 BY 1
             UNTIL SUB1 >= SORT-SIZE
                COMPUTE SUB2-START-VALUE = SUB1 + 1
                PERFORM
                   VARYING SUB2 FROM SUB2-START-VALUE BY 1
                   UNTIL SUB2 > SORT-SIZE
                      IF SORT-VALUE (SUB1)
                         > SORT-VALUE (SUB2)
                      THEN
                         MOVE SORT-VALUE (SUB1) TO SORT-TEMP
                         MOVE SORT-VALUE (SUB2)
                                        TO SORT-VALUE (SUB1)
                         MOVE SORT-TEMP TO SORT-VALUE (SUB2)
                      END-IF
                END-PERFORM
          END-PERFORM.

          ACCEPT WS-TIME FROM TIME.
          MOVE CORRESPONDING WS-TIME TO WS-TIME-DISPLAY.
          DISPLAY 'END   TIME OF JUMPUP SORT ' WS-TIME-DISPLAY.

          PERFORM
             VARYING SUB1 FROM 1 BY 1
             UNTIL SUB1 >= SORT-SIZE
                COMPUTE SUB2 = SUB1 + 1
                IF SORT-VALUE (SUB1) > SORT-VALUE (SUB2)
                THEN
                   DISPLAY 'ERROR IN SORT BETWEEN ' SUB1 ' AND ' SUB2
                   COMPUTE SUB1 = SORT-SIZE
                END-IF
          END-PERFORM.

          MOVE SORT-VALUES-C TO SORT-VALUES.

          ACCEPT WS-TIME FROM TIME.
          MOVE CORRESPONDING WS-TIME TO WS-TIME-DISPLAY.
          DISPLAY ' '
          DISPLAY 'START TIME OF SHELL  SORT ' WS-TIME-DISPLAY.

      *   SHELL SORT
          MOVE 10000 TO INCREMENT

          PERFORM
             UNTIL INCREMENT = 0
                COMPUTE SUB1-START-VALUE = INCREMENT + 1
                PERFORM
                   VARYING SUB1 FROM SUB1-START-VALUE BY 1
                   UNTIL SUB1 > SORT-SIZE
                      MOVE SUB1 TO SUB2
                      COMPUTE SUB2A = SUB2 - INCREMENT
                      MOVE SORT-VALUE (SUB1) TO SORT-TEMP
                      PERFORM
                         UNTIL SUB2 <= INCREMENT
                         OR SORT-VALUE (SUB2A) <= SORT-TEMP
                            COMPUTE SORT-VALUE (SUB2)
                                    = SORT-VALUE (SUB2A)
                            COMPUTE SUB2 = SUB2 - INCREMENT
                            COMPUTE SUB2A = SUB2 - INCREMENT
                      END-PERFORM
                      COMPUTE SORT-VALUE (SUB2) = SORT-TEMP
                END-PERFORM
                IF INCREMENT > 2
                THEN
                   COMPUTE INCREMENT = INCREMENT / 2
                ELSE
                ELSE
                   IF INCREMENT = 1
                   THEN
                      COMPUTE INCREMENT = 0
                   ELSE
                      COMPUTE INCREMENT = 1
                   END-IF
                END-IF
          END-PERFORM.

          ACCEPT WS-TIME FROM TIME.
          MOVE CORRESPONDING WS-TIME TO WS-TIME-DISPLAY.
          DISPLAY 'END   TIME OF SHELL  SORT ' WS-TIME-DISPLAY.

          PERFORM
            VARYING SUB1 FROM 1 BY 1
            UNTIL SUB1 >= SORT-SIZE
              COMPUTE SUB2 = SUB1 + 1
              IF SORT-VALUE (SUB1) > SORT-VALUE (SUB2)
              THEN
                  DISPLAY 'ERROR IN SORT BETWEEN ' SUB1 ' AND ' SUB2
                  COMPUTE SUB1 = SORT-SIZE
              END-IF
          END-PERFORM.


          GOBACK.

