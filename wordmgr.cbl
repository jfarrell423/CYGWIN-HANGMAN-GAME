*> ============================================================
      *> WORDMGR.cbl  --  Hangman Word & Topic Manager  (Program 1)
      *>
      *> Compile:  cobc -free -x WORDMGR.cbl -o wordmgr
      *> Run:      ./wordmgr
*> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WORDMGR.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORD-FILE ASSIGN TO "hangman_words.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WF-STATUS.
           SELECT TEMP-FILE ASSIGN TO "hangman_words.tmp"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS TF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  WORD-FILE.
       01  WORD-RECORD.
           05  WR-TOPIC   PIC X(20).
           05  WR-SEP     PIC X(1).
           05  WR-WORD    PIC X(30).
       FD  TEMP-FILE.
       01  TEMP-RECORD    PIC X(51).

       WORKING-STORAGE SECTION.
       01  WF-STATUS      PIC XX   VALUE SPACES.
       01  TF-STATUS      PIC XX   VALUE SPACES.
       01  WS-CHOICE      PIC 9    VALUE 0.
       01  WS-TOPIC       PIC X(20) VALUE SPACES.
       01  WS-WORD        PIC X(30) VALUE SPACES.
       01  WS-CONFIRM     PIC X    VALUE SPACES.
       01  WS-EOF         PIC X    VALUE "N".
       01  WS-COUNT       PIC 9(4) VALUE 0.
       01  WS-FOUND       PIC X    VALUE "N".
       01  WS-DELETED     PIC X    VALUE "N".
       01  WS-CONT        PIC X    VALUE "Y".
       01  WS-IDX         PIC 99   VALUE 0.
       01  WS-TOPIC-COUNT PIC 99   VALUE 0.
       01  WS-TOPIC-TABLE.
           05  WS-TOPIC-ENTRY OCCURS 50 TIMES.
               10  WS-T-NAME  PIC X(20) VALUE SPACES.
               10  WS-T-CNT   PIC 9(4)  VALUE 0.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY "============================================"
           DISPLAY "    HANGMAN -- Word and Topic Manager"
           DISPLAY "============================================"
           MOVE "Y" TO WS-CONT
           PERFORM 1000-MENU UNTIL WS-CONT = "N"
           STOP RUN.

       1000-MENU.
           DISPLAY " "
           DISPLAY "  MAIN MENU"
           DISPLAY "  --------"
           DISPLAY "  1. Add a word to a topic"
           DISPLAY "  2. List all topics"
           DISPLAY "  3. List words in a topic"
           DISPLAY "  4. Delete a word from a topic"
           DISPLAY "  5. Bulk-add words to a topic"
           DISPLAY "  6. Exit"
           DISPLAY "  Choice (1-6): " WITH NO ADVANCING
           ACCEPT  WS-CHOICE
           EVALUATE WS-CHOICE
               WHEN 1  PERFORM 2000-ADD-WORD
               WHEN 2  PERFORM 3000-LIST-TOPICS
               WHEN 3  PERFORM 4000-LIST-WORDS
               WHEN 4  PERFORM 5000-DELETE-WORD
               WHEN 5  PERFORM 6000-BULK-ADD
               WHEN 6
                   DISPLAY "  Goodbye!"
                   MOVE "N" TO WS-CONT
               WHEN OTHER
                   DISPLAY "  Please enter 1-6."
           END-EVALUATE.

       2000-ADD-WORD.
           DISPLAY " "
           DISPLAY "  ADD WORD"
           DISPLAY "  Topic (20 chars max): " WITH NO ADVANCING
           ACCEPT  WS-TOPIC
           MOVE FUNCTION UPPER-CASE(WS-TOPIC) TO WS-TOPIC
           DISPLAY "  Word  (30 chars max): " WITH NO ADVANCING
           ACCEPT  WS-WORD
           MOVE FUNCTION UPPER-CASE(WS-WORD) TO WS-WORD
           IF WS-TOPIC = SPACES OR WS-WORD = SPACES
               DISPLAY "  Topic/word cannot be blank. Cancelled."
               EXIT PARAGRAPH
           END-IF
           DISPLAY "  Confirm save? (Y/N): " WITH NO ADVANCING
           ACCEPT  WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) = "Y"
               PERFORM 8100-APPEND-RECORD
               DISPLAY "  Word added."
           ELSE
               DISPLAY "  Cancelled."
           END-IF.

       3000-LIST-TOPICS.
           DISPLAY " "
           DISPLAY "  ALL TOPICS"
           DISPLAY "  ----------"
           PERFORM 8200-LOAD-TOPICS
           IF WS-TOPIC-COUNT = 0
               DISPLAY "  (no words on file yet)"
           ELSE
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-TOPIC-COUNT
                   DISPLAY "  " WS-IDX ". "
                       FUNCTION TRIM(WS-T-NAME(WS-IDX))
                       " (" WS-T-CNT(WS-IDX) " words)"
               END-PERFORM
           END-IF.

       4000-LIST-WORDS.
           DISPLAY " "
           DISPLAY "  LIST WORDS IN TOPIC"
           DISPLAY "  Topic: " WITH NO ADVANCING
           ACCEPT  WS-TOPIC
           MOVE FUNCTION UPPER-CASE(WS-TOPIC) TO WS-TOPIC
           DISPLAY " "
           MOVE 0   TO WS-COUNT
           MOVE "N" TO WS-EOF
           OPEN INPUT WORD-FILE
           IF WF-STATUS NOT = "00"
               DISPLAY "  (data file not found)"
               CLOSE WORD-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-EOF = "Y"
               READ WORD-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END
                       IF WR-TOPIC = WS-TOPIC
                           ADD 1 TO WS-COUNT
                           DISPLAY "  " WS-COUNT ". "
                               FUNCTION TRIM(WR-WORD)
                       END-IF
               END-READ
           END-PERFORM
           CLOSE WORD-FILE
           IF WS-COUNT = 0
               DISPLAY "  (no words found for that topic)"
           ELSE
               DISPLAY "  Total: " WS-COUNT " word(s)"
           END-IF.

       5000-DELETE-WORD.
           DISPLAY " "
           DISPLAY "  DELETE WORD"
           DISPLAY "  Topic: " WITH NO ADVANCING
           ACCEPT  WS-TOPIC
           MOVE FUNCTION UPPER-CASE(WS-TOPIC) TO WS-TOPIC
           DISPLAY "  Word : " WITH NO ADVANCING
           ACCEPT  WS-WORD
           MOVE FUNCTION UPPER-CASE(WS-WORD) TO WS-WORD
           DISPLAY "  Confirm delete? (Y/N): " WITH NO ADVANCING
           ACCEPT  WS-CONFIRM
           IF FUNCTION UPPER-CASE(WS-CONFIRM) NOT = "Y"
               DISPLAY "  Cancelled."
               EXIT PARAGRAPH
           END-IF
           PERFORM 8300-REWRITE-WITHOUT
           IF WS-DELETED = "Y"
               DISPLAY "  Deleted successfully."
           ELSE
               DISPLAY "  Word not found."
           END-IF.

       6000-BULK-ADD.
           DISPLAY " "
           DISPLAY "  BULK ADD"
           DISPLAY "  Topic: " WITH NO ADVANCING
           ACCEPT  WS-TOPIC
           MOVE FUNCTION UPPER-CASE(WS-TOPIC) TO WS-TOPIC
           IF WS-TOPIC = SPACES
               DISPLAY "  Topic cannot be blank."
               EXIT PARAGRAPH
           END-IF
           DISPLAY "  Enter words one per line. Blank line = done."
           MOVE 0   TO WS-COUNT
           MOVE "Y" TO WS-CONT
           PERFORM UNTIL WS-CONT = "N"
               DISPLAY "  Word: " WITH NO ADVANCING
               ACCEPT  WS-WORD
               MOVE FUNCTION UPPER-CASE(WS-WORD) TO WS-WORD
               IF WS-WORD = SPACES
                   MOVE "N" TO WS-CONT
               ELSE
                   PERFORM 8100-APPEND-RECORD
                   ADD 1 TO WS-COUNT
               END-IF
           END-PERFORM
           DISPLAY "  " WS-COUNT " word(s) added."
           MOVE "Y" TO WS-CONT.

       8100-APPEND-RECORD.
           OPEN EXTEND WORD-FILE
           IF WF-STATUS NOT = "00"
               OPEN OUTPUT WORD-FILE
           END-IF
           MOVE WS-TOPIC TO WR-TOPIC
           MOVE "|"      TO WR-SEP
           MOVE WS-WORD  TO WR-WORD
           WRITE WORD-RECORD
           CLOSE WORD-FILE.

       8200-LOAD-TOPICS.
           MOVE 0   TO WS-TOPIC-COUNT
           MOVE "N" TO WS-EOF
           OPEN INPUT WORD-FILE
           IF WF-STATUS NOT = "00"
               CLOSE WORD-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-EOF = "Y"
               READ WORD-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM 8210-ADD-TOPIC-ENTRY
               END-READ
           END-PERFORM
           CLOSE WORD-FILE.

       8210-ADD-TOPIC-ENTRY.
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TOPIC-COUNT OR WS-FOUND = "Y"
               IF WS-T-NAME(WS-IDX) = WR-TOPIC
                   MOVE "Y" TO WS-FOUND
                   ADD 1 TO WS-T-CNT(WS-IDX)
               END-IF
           END-PERFORM
           IF WS-FOUND = "N" AND WS-TOPIC-COUNT < 50
               ADD 1 TO WS-TOPIC-COUNT
               MOVE WR-TOPIC TO WS-T-NAME(WS-TOPIC-COUNT)
               MOVE 1        TO WS-T-CNT(WS-TOPIC-COUNT)
           END-IF.

       8300-REWRITE-WITHOUT.
           MOVE "N" TO WS-DELETED
           MOVE "N" TO WS-EOF
           OPEN INPUT WORD-FILE
           IF WF-STATUS NOT = "00"
               DISPLAY "  Data file not found."
               CLOSE WORD-FILE
               EXIT PARAGRAPH
           END-IF
           OPEN OUTPUT TEMP-FILE
           IF TF-STATUS NOT = "00"
               DISPLAY "  Cannot create temp file."
               CLOSE WORD-FILE
               CLOSE TEMP-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-EOF = "Y"
               READ WORD-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM 8310-COPY-OR-SKIP
               END-READ
           END-PERFORM
           CLOSE WORD-FILE
           CLOSE TEMP-FILE
           CALL "CBL_DELETE_FILE"
               USING "hangman_words.dat" & x"00"
           END-CALL
           CALL "CBL_RENAME_FILE"
               USING "hangman_words.tmp" & x"00"
                     "hangman_words.dat" & x"00"
           END-CALL.

       8310-COPY-OR-SKIP.
           IF WR-TOPIC = WS-TOPIC AND WR-WORD = WS-WORD
               MOVE "Y" TO WS-DELETED
           ELSE
               MOVE WORD-RECORD TO TEMP-RECORD
               WRITE TEMP-RECORD
           END-IF.

       END PROGRAM WORDMGR.


