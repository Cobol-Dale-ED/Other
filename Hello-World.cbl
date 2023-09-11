      *-----------------------------------------------------------------
       IDENTIFICATION DIVISION.
      *-----------------------------------------------------------------
       PROGRAM-ID. HelloWorld.
       AUTHOR.     Dale Dunten
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
      *-----------------------------------------------------------------
       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
       select person
       assign to 
      *     '/Users/gregoryshields/Code/Cobol/Hello-World/person.dat'.
      *      '/temp/gnucobol_wsl/test.cbl/person.dat'.		 
             'person.dat'
           organization is line sequential.
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       FILE SECTION.

       fd person
           record contains 40 characters
           data record is fs-record.
      * Can I comment it out the 'record contains' line above?

      *01 fs-record pic x(40).
      * Instead of the above where the entire record is one string,
      * let's break it into fields.
       01 fs-record.
           05 fs-first-name pic x(10).
           05 fs-last-name  pic x(10).
           05 fs-salary     pic 9(8)v99.
           05 filler        pic x(10).

      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 Data-1         pic 9(4) value 30.
       01 Data-2         pic 9(4) value 20.
      * Here's how we do a character data type.
       77 MyChar         pic A.

      * These two value clauses could be replaced by
      * move statements below.
       01 num1 pic 99 value 0.

       01 string1 pic x(30) value 
           "Greg is a Cobol programmer".

      * This is simply grouped data.
       01 ws-record.
           05 first-name pic x(10).
           05 last-name  pic x(10).
           05 salary     pic 9(8)v99.
           05 filler     pic x(10).
      *-----------------------------------------------------------------

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
      * https://www.ibm.com/docs/en/i/7.2?topic=overview-procedures
      * A procedure consists of:
      * - A section or group of sections
      * - A paragraph or group of paragraphs
      *-----------------------------------------------------------------

      * Tell the program what to do.
       display ' '.
       perform Paragraph-A
       perform Paragraph-C
       perform Gitrdun.
       perform YourMom.
       goback.

      * We can just define paragraphs if we want.
       Paragraph-A.
            display 'Paragraph-A performed.'.
            add Data-1 to Data-2.
            display '    Data-1: ' Data-1.
            display '    Data-2: ' Data-2.
            display ' '.

       Paragraph-B.
            display 'Paragraph-B performed.'.
            display ' '.

       Paragraph-C.
            display 'Paragraph-C performed.'.
            display ' '.

      * Or we can define sections.
       Gitrdun SECTION.
       display 'Gitrdun section performed.'.
       display ' '.

       WritePersonFile.
           display 'WritePersonFile paragraph performed.'.
           display ' '.

      *    Open file for output.
           open output person.

      *    Put data in ws record variables.
           move "Jimmy"    to first-name
           move "Dean"     to last-name
           move 132000.59  to salary
      *    Write ws record to file record.
           write fs-record from ws-record

           move "David"    to first-name
           move "Essex"    to last-name
           move 128000.59  to salary
           write fs-record from ws-record

           move "Rod"      to first-name
           move "Serling"   to last-name
           move 156000.59  to salary
           write fs-record from ws-record

           close person.

       ReadPersonFile.
           display 'ReadPersonFile paragraph performed.'.
           display ' '.

           open input person.

           read person into ws-record.
           display ws-record.

           read person into ws-record.
           display ws-record.

           read person into ws-record.
           display ws-record.
           display ' '.

           close person.

       YourMom SECTION.
           display 'YourMom section performed.'.
           display ' '.

           display string1.
           inspect string1 replacing all "Greg" by "Dale"
           display string1.
           display ' '.

       perform HelloParagraph 2 times.
       display "next 1"
       perform HelloParagraph until num1 > 4.
       display "next 2".
       perform SecondParagraph varying num1 from 20 
           until num1 > 22.

       SecondParagraph.
           display "second " num1.

       HelloParagraph.
           add 1 to num1
           DISPLAY "Hello, World! " num1.
           perform TryAgainParagraph.

       TryAgainParagraph.
           display "Try again".
      *-----------------------------------------------------------------

      * https://stackoverflow.com/questions/48151631/cobol-continuation-line-for-file-path
      * Line continuation:
      * Write your string up to col 72 (no closing quote).
      * On the next line put a hyphen ('-') in col 7.
      * On the same line continue your string with an opening quote.

      * For example:
      * INPUT-OUTPUT SECTION.
      * FILE-CONTROL.
      * select gregorytest assign to 
      *     '/Users/gregoryshields/Code/Cobol/Hello-World/gregorytest.da
      *-    't'.
      * The hyphen is actually supposed to be in the column where the
      * asterisk is above.



