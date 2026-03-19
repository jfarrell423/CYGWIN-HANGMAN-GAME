*> ============================================================
      *> HANGMAN.cbl  --  Hangman Game  (Program 2)
      *>
      *> Reads word database created by WORDMGR.
      *> Player selects topics, game picks random word, classic loop.
      *>
      *> Compile:  cobc -free -x HANGMAN.cbl -o hangman
      *> Run:      ./hangman
*> ============================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HANGMAN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WORD-FILE ASSIGN TO "hangman_words.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               ACCESS MODE  IS SEQUENTIAL
               FILE STATUS  IS WF-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  WORD-FILE.
       01  WORD-RECORD.
           05  WR-TOPIC   PIC X(20).
           05  WR-SEP     PIC X(1).
           05  WR-WORD    PIC X(30).

       WORKING-STORAGE SECTION.
       01  WF-STATUS      PIC XX   VALUE SPACES.
       01  WS-EOF         PIC X    VALUE "N".
       01  WS-FOUND       PIC X    VALUE "N".
       01  WS-IDX         PIC 99   VALUE 0.
       01  WS-IDX2        PIC 9(4) VALUE 0.
       01  WS-CHOICE-NUM  PIC 99   VALUE 0.
       01  WS-CHOICE-STR  PIC X(4) VALUE SPACES.

      *> Topic table
       01  WS-TOPIC-COUNT PIC 99   VALUE 0.
       01  WS-TOPIC-TABLE.
           05  WS-TOPIC-ENTRY OCCURS 20 TIMES.
               10  WS-T-NAME  PIC X(20) VALUE SPACES.
               10  WS-T-SEL   PIC X     VALUE "N".
               10  WS-T-CNT   PIC 9(4)  VALUE 0.

      *> Word pool
       01  WS-WORD-COUNT  PIC 9(4) VALUE 0.
       01  WS-WORD-POOL.
           05  WS-POOL-WORD OCCURS 500 TIMES
                            PIC X(30) VALUE SPACES.

      *> Game state
       01  WS-SECRET      PIC X(30) VALUE SPACES.
       01  WS-SECRET-LEN  PIC 99    VALUE 0.
       01  WS-DISPLAY     PIC X(30) VALUE SPACES.
       01  WS-WRONG-MAX   PIC 99    VALUE 6.
       01  WS-WRONG-CNT   PIC 99    VALUE 0.
       01  WS-WIN         PIC X     VALUE "N".
       01  WS-GAME-OVER   PIC X     VALUE "N".
       01  WS-GUESS       PIC X     VALUE SPACES.
       01  WS-BLANKS      PIC 99    VALUE 0.
       01  WS-HIT         PIC X     VALUE "N".
       01  WS-ALREADY     PIC X     VALUE "N".
       01  WS-PLAY-AGAIN  PIC X     VALUE "Y".

      *> Guessed letters
       01  WS-GUESS-CNT   PIC 99    VALUE 0.
       01  WS-GUESSED     PIC X(26) VALUE SPACES.

      *> Random
       01  WS-RAND        PIC 9(9)  VALUE 0.
       01  WS-SEED        PIC 9(8)  VALUE 0.
       01  WS-PICK        PIC 9(4)  VALUE 0.

      *> Score
       01  WS-WINS        PIC 999   VALUE 0.
       01  WS-LOSSES      PIC 999   VALUE 0.

      *> Date for seeding
       01  WS-DATE-BUF    PIC X(21) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           DISPLAY "============================================"
           DISPLAY "         H A N G M A N"
           DISPLAY "============================================"
           PERFORM 8100-RAND-SEED
           PERFORM 8200-LOAD-ALL-TOPICS
           IF WS-TOPIC-COUNT = 0
               DISPLAY "  No words in hangman_words.dat."
               DISPLAY "  Run WORDMGR first to add words."
               STOP RUN
           END-IF
           MOVE "Y" TO WS-PLAY-AGAIN
           PERFORM 1000-TOPIC-SELECT
           PERFORM 2000-GAME-ROUND
               UNTIL WS-PLAY-AGAIN = "N"
           DISPLAY " "
           DISPLAY "============================================"
           DISPLAY "  Final -- Wins: " WS-WINS
               "  Losses: " WS-LOSSES
           DISPLAY "============================================"
           STOP RUN.

      *> ============================================================
      *>  1000 - TOPIC SELECTION
      *> ============================================================
       1000-TOPIC-SELECT.
           DISPLAY " "
           DISPLAY "  SELECT TOPICS"
           DISPLAY "  -------------"
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TOPIC-COUNT
               DISPLAY "  " WS-IDX ". "
                   FUNCTION TRIM(WS-T-NAME(WS-IDX))
                   " (" WS-T-CNT(WS-IDX) " words)"
           END-PERFORM
           DISPLAY " "
           DISPLAY "  Enter topic # (or 0 for ALL): "
               WITH NO ADVANCING
           ACCEPT WS-CHOICE-STR
           MOVE FUNCTION NUMVAL(WS-CHOICE-STR) TO WS-CHOICE-NUM
           IF WS-CHOICE-NUM = 0
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-TOPIC-COUNT
                   MOVE "Y" TO WS-T-SEL(WS-IDX)
               END-PERFORM
           ELSE
               IF WS-CHOICE-NUM >= 1
                   AND WS-CHOICE-NUM <= WS-TOPIC-COUNT
                   MOVE "Y" TO WS-T-SEL(WS-CHOICE-NUM)
                   PERFORM 1010-MORE-TOPICS
               ELSE
                   DISPLAY "  Invalid. Try again."
                   PERFORM 1000-TOPIC-SELECT
                   EXIT PARAGRAPH
               END-IF
           END-IF
           PERFORM 1100-BUILD-POOL
           IF WS-WORD-COUNT = 0
               DISPLAY "  No words loaded. Try again."
               PERFORM VARYING WS-IDX FROM 1 BY 1
                   UNTIL WS-IDX > WS-TOPIC-COUNT
                   MOVE "N" TO WS-T-SEL(WS-IDX)
               END-PERFORM
               PERFORM 1000-TOPIC-SELECT
               EXIT PARAGRAPH
           END-IF
           DISPLAY "  " WS-WORD-COUNT
               " word(s) loaded. Let's play!".

       1010-MORE-TOPICS.
           DISPLAY "  Add another topic? (Y/N): "
               WITH NO ADVANCING
           ACCEPT WS-FOUND
           IF FUNCTION UPPER-CASE(WS-FOUND) = "Y"
               DISPLAY "  Topic #: " WITH NO ADVANCING
               ACCEPT WS-CHOICE-STR
               MOVE FUNCTION NUMVAL(WS-CHOICE-STR)
                   TO WS-CHOICE-NUM
               IF WS-CHOICE-NUM >= 1
                   AND WS-CHOICE-NUM <= WS-TOPIC-COUNT
                   MOVE "Y" TO WS-T-SEL(WS-CHOICE-NUM)
               END-IF
               PERFORM 1010-MORE-TOPICS
           END-IF.

       1100-BUILD-POOL.
           MOVE 0   TO WS-WORD-COUNT
           MOVE "N" TO WS-EOF
           OPEN INPUT WORD-FILE
           IF WF-STATUS NOT = "00"
               CLOSE WORD-FILE
               EXIT PARAGRAPH
           END-IF
           PERFORM UNTIL WS-EOF = "Y"
               READ WORD-FILE
                   AT END MOVE "Y" TO WS-EOF
                   NOT AT END PERFORM 1110-CHECK-WORD
               END-READ
           END-PERFORM
           CLOSE WORD-FILE.

       1110-CHECK-WORD.
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TOPIC-COUNT OR WS-FOUND = "Y"
               IF WS-T-NAME(WS-IDX) = WR-TOPIC
                   AND WS-T-SEL(WS-IDX) = "Y"
                   MOVE "Y" TO WS-FOUND
               END-IF
           END-PERFORM
           IF WS-FOUND = "Y" AND WS-WORD-COUNT < 500
               ADD 1 TO WS-WORD-COUNT
               MOVE WR-WORD TO WS-POOL-WORD(WS-WORD-COUNT)
           END-IF.

      *> ============================================================
      *>  2000 - ONE GAME ROUND
      *> ============================================================
       2000-GAME-ROUND.
           PERFORM 2100-PICK-WORD
           PERFORM 2200-INIT
           MOVE "N" TO WS-GAME-OVER
           PERFORM 2300-TURN
               UNTIL WS-GAME-OVER = "Y"
           PERFORM 2400-END-ROUND.

       2100-PICK-WORD.
           PERFORM 8100-RAND-SEED
           COMPUTE WS-PICK =
               FUNCTION MOD(WS-RAND, WS-WORD-COUNT) + 1
           MOVE WS-POOL-WORD(WS-PICK) TO WS-SECRET
           MOVE 0 TO WS-SECRET-LEN
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > 30
               IF WS-SECRET(WS-IDX:1) NOT = SPACE
                   MOVE WS-IDX TO WS-SECRET-LEN
               END-IF
           END-PERFORM.

       2200-INIT.
           MOVE 0      TO WS-WRONG-CNT
           MOVE 0      TO WS-GUESS-CNT
           MOVE "N"    TO WS-WIN
           MOVE SPACES TO WS-GUESSED
           MOVE SPACES TO WS-DISPLAY
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-SECRET-LEN
               IF WS-SECRET(WS-IDX:1) = SPACE
                   MOVE SPACE TO WS-DISPLAY(WS-IDX:1)
               ELSE
                   MOVE "_" TO WS-DISPLAY(WS-IDX:1)
               END-IF
           END-PERFORM.

       2300-TURN.
           PERFORM 3000-DRAW-BOARD
           DISPLAY "  Guess a letter: " WITH NO ADVANCING
           ACCEPT WS-GUESS
           MOVE FUNCTION UPPER-CASE(WS-GUESS) TO WS-GUESS
           IF WS-GUESS = SPACE
               DISPLAY "  Please enter a letter."
               EXIT PARAGRAPH
           END-IF
           PERFORM 2310-CHECK-ALREADY
           IF WS-ALREADY = "Y"
               DISPLAY "  Already guessed [" WS-GUESS "]. Try again."
               EXIT PARAGRAPH
           END-IF
           ADD 1 TO WS-GUESS-CNT
           MOVE WS-GUESS TO WS-GUESSED(WS-GUESS-CNT:1)
           MOVE "N" TO WS-HIT
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-SECRET-LEN
               IF WS-SECRET(WS-IDX:1) = WS-GUESS
                   MOVE WS-GUESS TO WS-DISPLAY(WS-IDX:1)
                   MOVE "Y"      TO WS-HIT
               END-IF
           END-PERFORM
           IF WS-HIT = "N"
               ADD 1 TO WS-WRONG-CNT
               DISPLAY "  Wrong! (" WS-WRONG-CNT
                   " of " WS-WRONG-MAX " mistakes)"
           ELSE
               DISPLAY "  Good guess!"
           END-IF
           MOVE 0 TO WS-BLANKS
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-SECRET-LEN
               IF WS-DISPLAY(WS-IDX:1) = "_"
                   ADD 1 TO WS-BLANKS
               END-IF
           END-PERFORM
           IF WS-BLANKS = 0
               MOVE "Y" TO WS-WIN
               MOVE "Y" TO WS-GAME-OVER
           ELSE IF WS-WRONG-CNT >= WS-WRONG-MAX
               MOVE "Y" TO WS-GAME-OVER
           END-IF.

       2310-CHECK-ALREADY.
           MOVE "N" TO WS-ALREADY
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-GUESS-CNT OR WS-ALREADY = "Y"
               IF WS-GUESSED(WS-IDX:1) = WS-GUESS
                   MOVE "Y" TO WS-ALREADY
               END-IF
           END-PERFORM.

       2400-END-ROUND.
           PERFORM 3000-DRAW-BOARD
           IF WS-WIN = "Y"
               ADD 1 TO WS-WINS
               DISPLAY " "
               DISPLAY "  *** YOU WIN! *** Word: "
                   FUNCTION TRIM(WS-SECRET)
           ELSE
               ADD 1 TO WS-LOSSES
               DISPLAY " "
               DISPLAY "  *** GAME OVER *** Word was: "
                   FUNCTION TRIM(WS-SECRET)
           END-IF
           DISPLAY "  Score -- Wins: " WS-WINS
               "  Losses: " WS-LOSSES
           DISPLAY " "
           DISPLAY "  Play again? (Y/N): " WITH NO ADVANCING
           ACCEPT WS-PLAY-AGAIN
           MOVE FUNCTION UPPER-CASE(WS-PLAY-AGAIN)
               TO WS-PLAY-AGAIN
           IF WS-PLAY-AGAIN NOT = "Y"
               MOVE "N" TO WS-PLAY-AGAIN
           END-IF.

      *> ============================================================
      *>  3000 - DRAW BOARD
      *> ============================================================
       3000-DRAW-BOARD.
           DISPLAY " "
           DISPLAY "  +---------+"
           PERFORM 3100-HEAD-ROW
           PERFORM 3200-BODY-ROW
           PERFORM 3300-LEG-ROW
           DISPLAY "  |"
           DISPLAY "  ========="
           DISPLAY " "
           DISPLAY "  Word : " WS-DISPLAY(1:WS-SECRET-LEN)
           DISPLAY "  Wrong: " WS-WRONG-CNT " of " WS-WRONG-MAX
           DISPLAY "  Used : " WS-GUESSED(1:WS-GUESS-CNT)
           DISPLAY " ".

       3100-HEAD-ROW.
           IF WS-WRONG-CNT >= 1
               DISPLAY "  |    O   |"
           ELSE
               DISPLAY "  |        |"
           END-IF.

       3200-BODY-ROW.
           EVALUATE TRUE
               WHEN WS-WRONG-CNT >= 4
                   DISPLAY "  |   /|\\ |"
               WHEN WS-WRONG-CNT >= 3
                   DISPLAY "  |   /|  |"
               WHEN WS-WRONG-CNT >= 2
                   DISPLAY "  |    |  |"
               WHEN OTHER
                   DISPLAY "  |       |"
           END-EVALUATE.

       3300-LEG-ROW.
           EVALUATE TRUE
               WHEN WS-WRONG-CNT >= 6
                   DISPLAY "  |   / \\ |"
               WHEN WS-WRONG-CNT >= 5
                   DISPLAY "  |   /   |"
               WHEN OTHER
                   DISPLAY "  |       |"
           END-EVALUATE.

      *> ============================================================
      *>  8100 - RANDOM NUMBER (LCG)
      *> ============================================================
       8100-RAND-SEED.
           MOVE FUNCTION CURRENT-DATE TO WS-DATE-BUF
           COMPUTE WS-SEED =
               FUNCTION NUMVAL(WS-DATE-BUF(9:6))
           ADD WS-SEED TO WS-RAND
           IF WS-RAND = 0
               MOVE 31337 TO WS-RAND
           END-IF
           COMPUTE WS-RAND = FUNCTION MOD(
               WS-RAND * 1103515245 + 12345, 2147483648).

      *> ============================================================
      *>  8200 - LOAD TOPIC LIST
      *> ============================================================
       8200-LOAD-ALL-TOPICS.
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
                   NOT AT END PERFORM 8210-ADD-TOPIC
               END-READ
           END-PERFORM
           CLOSE WORD-FILE.

       8210-ADD-TOPIC.
           MOVE "N" TO WS-FOUND
           PERFORM VARYING WS-IDX FROM 1 BY 1
               UNTIL WS-IDX > WS-TOPIC-COUNT OR WS-FOUND = "Y"
               IF WS-T-NAME(WS-IDX) = WR-TOPIC
                   MOVE "Y" TO WS-FOUND
                   ADD 1 TO WS-T-CNT(WS-IDX)
               END-IF
           END-PERFORM
           IF WS-FOUND = "N" AND WS-TOPIC-COUNT < 20
               ADD 1 TO WS-TOPIC-COUNT
               MOVE WR-TOPIC TO WS-T-NAME(WS-TOPIC-COUNT)
               MOVE "N"      TO WS-T-SEL(WS-TOPIC-COUNT)
               MOVE 1        TO WS-T-CNT(WS-TOPIC-COUNT)
           END-IF.

       END PROGRAM HANGMAN.


