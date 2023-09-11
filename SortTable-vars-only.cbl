        IDENTIFICATION DIVISION.
        PROGRAM-ID. HELLO.


        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  SORT-TABLE.
            05 SORT-SIZE PIC S9(9) COMP-3 VALUE 50.

            05 SORT-VALUES.
               10 SORT-VALUE PIC 9(9) COMP-3
                             OCCURS 1 TO 100
                             TIMES
                             DEPENDING ON SORT-SIZE.

            05 SUB1-START-VALUE PIC S9(9) COMP-3.

            05 SORT-VALUES-C.
               10 SORT-VALUE-C PIC 9(9) COMP-3
                               OCCURS 1 TO 100
                               TIMES
                               DEPENDING ON SORT-SIZE.

            05 SUB2-START-VALUE PIC S9(9) COMP-3.

        PROCEDURE DIVISION.


        GOBACK.
