# CYGWIN-HANGMAN-GAME

HANGMAN GAME Written in GYGWIN COBOL

Here's a quick rundown:

GNUCOBOL is required, I have a video showing the install on windows.

Walk through on installation: https://www.youtube.com/watch?v=vU0wpLtnMkY

WORDMGR.cbl — Word & Topic Manager

Add individual words to any topic
Bulk-add multiple words to a topic at once (blank line to finish)
List all topics with word counts
List all words within a specific topic
Delete a word from a topic (rewrites the file atomically using a temp file + CBL_RENAME_FILE)

HANGMAN.cbl — The Game

Reads hangman_words.dat produced by WORDMGR
Shows numbered topic list at startup; player picks one, multiple, or all topics
Randomly selects a word using an LCG seeded from the system clock
Full ASCII gallows that builds up across 6 wrong guesses (head → body → arms → legs)
Tracks guessed letters, detects duplicate guesses, shows running score across rounds

To build and run:

In bash

cobc -free -x WORDMGR.cbl -o wordmgr

cobc -free -x HANGMAN.cbl -o hangman

./wordmgr      # set up your topics first

./hangman      # play!

The shared data file is hangman_words.dat 
(plain text, one record per line, auto-created in the working directory).

Download Compile and enjoy!
