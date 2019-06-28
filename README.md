# jianpu-ly

Jianpu in Lilypond, from http://people.ds.cam.ac.uk/ssb22/mwrhome/jianpu-ly.html

(also mirrored at http://ssb22.gitlab.io/mwrhome/jianpu-ly.html as the Cambridge "DS-Web" server sometimes gets taken down for several days of maintenance)

jianpu-ly is a Python 2 program to assist with printing jianpu (numbered musical notation) in the GNU Lilypond music typesetter. The jianpu is written on a modiﬁed-appearance “stave” in Lilypond, which means Lilypond’s typesetting capabilities (lyric spacing, slurs, beams etc) will apply to the jianpu without needing to add a 5-line stave. If you prefer, the generated code for the jianpu stave may also be placed in a score with other types of stave. _Chords in jianpu are not supported_ by this script, unless written out as one note to a part. If you have problems, try a later Lilypond version (2.15.40-1 and 2.18.2-1 should work).





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

Lyrics: `L: here are the syl- la- bles` (all on one line)

Lyrics (verse 1): `L: 1. Here is verse one`

Lyrics (verse 2): `L: 2. Here is verse two`

Hanzi lyrics (auto space): `H: hanzi` (with or without spaces)

Lilypond headers: `title=the title` (on a line of its own)

Multiple movements: `NextScore`

Prohibit page breaks until end of this movement: `OnePage`

Add a Western staff doubling the jianpu: `WithStaff`

Tuplets: `3[ q1 q1 q1 ]`

Da capo: `1 1 Fine 1 1 1 1 1 1 DC`

Repeat (with alternate endings): `R{ 1 1 1 } A{ 2 | 3 }`

Short repeats (percent): `R4{ 1 2 }`

Ties (like Lilypond's, if you don't want dashes): `1 ~ 1`

Slurs (like Lilypond's): `1 ( 2 )`

Dynamics (applies to previous note): `\p \mp \f`

Other 1-word Lilypond \ commands: `\fermata \> \! \( \) etc`

Other Lilypond code: `LP: (block of code) :LP` (each delimeter at start of its line)

