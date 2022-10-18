# jianpu-ly

Jianpu in Lilypond, from http://ssb22.user.srcf.net/mwrhome/jianpu-ly.html

(also mirrored at http://ssb22.gitlab.io/mwrhome/jianpu-ly.html just in case)

jianpu-ly is a Python program (compatible with both Python 2 and Python 3) that assists with printing jianpu (numbered musical notation) in the GNU Lilypond music typesetter. The jianpu is written on a modiﬁed-appearance “stave” in Lilypond, which means Lilypond’s typesetting capabilities (lyric spacing, slurs, beams etc) will apply to the jianpu without needing to add a 5-line stave. If you prefer, the generated code for the jianpu stave may also be placed in a score with other types of stave.

Using jianpu-ly requires some technical knowledge.  If you don't know what a command line is, what a text editor is, what a directory is or what Python is, then please find out about these things before attempting to use jianpu-ly.  It is not an extension to Lilypond front-ends like Frescobaldi; it is a preprocessor that currently requires you to have command-line experience.

If you have problems, try a different Lilypond version.
jianpu-ly works with Lilypond 2.18, 2.20 and 2.22.

Run jianpu-ly < text-file > ly-file (or jianpu-ly text-files > ly-file)

Text files are whitespace-separated and can contain:

Scale going up: `1 2 3 4 5 6 7 1'`

Accidentals: `1 #1 2 b2 1`

Octaves: `1,, 1, 1 1' 1''`

Shortcuts for 1' and 2': `8 9`

Semiquaver, quaver, crotchet (16/8/4th notes): `s1 q1 1`

Dotted versions of the above (50% longer): `s1. q1. 1.`

Demisemiquaver, hemidemisemiquaver (32/64th notes): `d1 h1`

Minims (half notes) use dashes: `1 -`

Dotted minim: `1 - -`

Semibreve (whole note): `1 - - -`

Time signature: `4/4`

Time signature with quaver anacrusis (8th-note pickup): `4/4,8`

Key signature (major): `1=Bb`

Key signature (minor): `6=F#`

Tempo: `4=85`

Lyrics: `L: here are the syl- la- bles` (all on one line)

Lyrics (verse 1): `L: 1. Here is verse one`

Lyrics (verse 2): `L: 2. Here is verse two`

Hanzi lyrics (auto space): `H: hanzi` (with or without spaces)

Lilypond headers: `title=the title` (on a line of its own)

Multiple movements: `NextScore`

Prohibit page breaks until end of this movement: `OnePage`

Suppress bar numbers: `NoBarNums`

Old-style time signature: `SeparateTimesig 1=C 4/4`

Indonesian 'not angka' style: `angka`

Add a Western staff doubling the tune: `WithStaff`

Tuplets: `3[ q1 q1 q1 ]`

Grace notes before: `g[#45] 1`

Grace notes after: `1 ['1]g`

Simple chords: `135 1 13 1`

Da capo: `1 1 Fine 1 1 1 1 1 1 DC`

Repeat (with alternate endings): `R{ 1 1 1 } A{ 2 | 3 }`

Short repeats (percent): `R4{ 1 2 }`

Ties (like Lilypond's, if you don't want dashes): `1 ~ 1`

Slurs (like Lilypond's): `1 ( 2 )`

Erhu fingering (applies to previous note): `Fr=0 Fr=4`

Erhu symbol (applies to previous note): `souyin harmonic up down bend tilde`

Dynamics (applies to previous note): `\p \mp \f`

Other 1-word Lilypond \ commands: `\fermata \> \! \( \) etc`

Other Lilypond code: `LP: (block of code) :LP` (each delimeter at start of its line)

Ignored: `% a comment`


Copyright and Trademarks
------------------------

(c) Silas S. Brown, licensed under Apache 2.

Apache is a registered trademark of The Apache Software Foundation.
Python is a trademark of the Python Software Foundation.
Any other trademarks I mentioned without realising are trademarks of their respective holders.
