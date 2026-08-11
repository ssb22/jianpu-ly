# jianpu-ly
[**简体中文**](README_zh-Hans.md)

Jianpu in Lilypond, from https://ssb22.user.srcf.net/mwrhome/jianpu-ly.html

(also mirrored at https://ssb22.gitlab.io/mwrhome/jianpu-ly.html just in case, and available via `pip install jianpu-ly` or `pipx run jianpu-ly`)

jianpu-ly is a Python program (compatible with both Python 2 and Python 3) that assists with printing jianpu (numbered musical notation) in the GNU Lilypond music typesetter. The jianpu is written on a modiﬁed-appearance “stave” in Lilypond, which means Lilypond’s typesetting capabilities (lyric spacing, slurs, beams etc) will apply to the jianpu without needing to add a 5-line stave. If you prefer, the generated code for the jianpu stave may also be placed in a score with other types of stave.

Using jianpu-ly requires some technical knowledge.  If you don't know what a command line is, what a text editor is, what a directory is or what Python is, then please find out about these things before attempting to use jianpu-ly.  It is not an extension to Lilypond front-ends like Frescobaldi; it is a preprocessor that currently requires you to have command-line experience.

If you have problems, try a different Lilypond version.
jianpu-ly works with Lilypond 2.20, 2.22 and 2.24.

Run jianpu-ly < text-file > ly-file (or jianpu-ly text-files > ly-file).  There is experimental support for importing MusicXML via jianpu-ly piece.xml (or jianpu-ly piece.mxl > ly-file) but this does not work for all pieces.

Normal text files are whitespace-separated and can contain words like the following.  Usually the order of characters within a note does not matter, hence #1 is the same as 1# and '1 is the same as 1' and s1 is the same as 1s.

Scale going up: `1 2 3 4 5 6 7 1'`

Accidentals: `1 #1 2 b2 1`

Octaves: `1,, 1, 1 1' 1''`

Shortcuts for 1' and 2': `8 9`

Percussion beat: `x`

Change base octave: `< >`

Semiquaver, quaver, crotchet (16/8/4th notes): `s1 q1 1`

Alternate way to input semiquaver, quaver, crotchet: `1\\ 1\ 1` (any \ must go after the pitch not before)

Sticky durations (4 semiquavers then crotchet): `KeepLength s1 1 1 1 c1`

Dotted versions of the above (50% longer): `s1. q1. 1.`

Alternate dotted versions: `1\\. 1\.`

Demisemiquaver, hemidemisemiquaver (32/64th notes): `d1 h1`

Minims (half notes) use dashes: `1 -`

Dotted minim: `1 - -`

Semibreve (whole note): `1 - - -`

Time signature: `4/4`

Time signature with anacrusis (pickup): `4/4,8 or 6/8,4` (number after comma is pickup's value)

Key signature (major): `1=Bb`

Key signature (minor): `6=F#`

Tempo: `4=85`

Lyrics: `L: here are the syl- la- bles` (all on one line, or newline after the : and double newline to end)

Lyrics (verse 1): `L: 1. Here is verse one`

Lyrics (verse 2): `L: 2. Here is verse two`

Hanzi lyrics (auto space): `H: hanzi` (with or without spaces)

Hanzi lyrics (verse 1): `H: 1. 这是第一节`

Lilypond title: `title=the title` (on a line of its own)

Other Lilypond headers: `subtitle= composer= poet= arranger= copyright= opus= etc.`

Guitar chords: `chords=c2. g:7 c` (on own line, or newline after the = and double newline to end)

Fret diagrams: `frets=guitar` (on own line)

Change guitar chords into Roman numerals: `ChordsRoman`

Multiple parts: `NextPart`

Instrument of current part: `instrument=Flute` (on a line of its own)

Multiple movements: `NextScore`

Prohibit page breaks until end of this movement: `OnePage`

Suppress bar numbers: `NoBarNums`

Suppress first-line indent: `NoIndent`

Ragged last line: `RaggedLast`

Old-style time signature: `SeparateTimesig 1=C 4/4`

Indonesian 'not angka' style: `angka`

Alternate Indonesian-style minim, dotted minim and semibreve: `1 . 1 . . 1 . . .` (dot is treated as dash)

Add a Western staff doubling the tune: `WithStaff`

Tuplets: `3[ q1 q1 q1 ]`

Grace notes before: `g[#45] 1`

Grace notes after: `1 ['1]g`

Grace notes with durations: `g[d4d5s6] 1`

Simple chords: `,135' 1 1b3 1`

Grace note chords: `g[1&3&5] 1`

Arpeggiated chords: `arpUp 135 arpDown 531 arp 135`

Da capo: `1 1 Fine 1 1 1 1 1 1 DC`

Dal segno: `1 1 Segno 1 1 ToCoda 1 1 DS 1 1`

Repeat (with alternate endings): `R{ 1 1 1 } A{ 2 | 3 }`

Short repeats (percent): `R4{ 1 2 }`

Ties (like Lilypond's, if you don't want dashes): `1 ~ 1`

Slurs (like Lilypond's): `1 ( 2 )`

Erhu fingering (applies to previous note): `Fr=0 Fr=4`

Erhu slide: `slideUp 1 slideDown 2`

Erhu custom slide: `slide=𝆱 1`

Erhu symbol (applies to previous note): `souyin harmonic up down bend tilde`

Tremolo: `1/// - 1///5 -`

Glissando: `glis 1 - 5`

Rehearsal marks: `letterA letterB letter3 letterAA`

Multibar rest: `R*8`

Dynamics (applies to previous note): `\p \mp \f`

Other 1-word Lilypond \ commands: `\fermata \> \! \( \) etc`

Text: `^"above note" _"below note"`

Harmonic symbols above main notes: `Harm: (music) :Harm` (main music)

Instrumental breaks in vocal music: `1 [( 2 3 )] 4`

Repeat same-bar accidentals in awkward passages: `RepeatAccidentals #5 #2 #4 #5 NormalAccidentals`

Barlines: `\bar "||" or \bar "|." etc` (no LP: needed)

Other Lilypond code: `LP: (block of code) :LP` (each delimeter at start of its line)

Lilypond header additions: `LPH: (definitions) :LPH` (each at start of line)

Split MIDI files per part: `PartMidi`

Ignored: `% a comment`

**Command-line options:**

`--chinese`: Use Chinese for `--html`, `--markdown` or `--help` options

`--export-jly`: Export the jianpu input to a .jly file instead of converting it (useful e.g. after a MusicXML import)

`--help`: Write command-line help (aliases: -h, /?)

`--html`: Write the HTML help for the website

`--markdown`: Write the Markdown documentation for the Git README

`--noRestHack`: Disable the rest hack (debug option to try if output goes wrong)

`--noStaff`: Force no Western staff, even if the input asks for one

`--nosort`: Don't sort chord notes by pitch (crossing parts when 2-voice music was mistakenly coded as chords)

`--unicode-approx`: Output a Unicode approximation of the jianpu instead of Lilypond code

`--version`: Just write version number (aliases: -v, /v)

`--withStaff`: Add a Western staff doubling the tune in all parts, as if WithStaff is specified everywhere


Copyright and Trademarks
------------------------

(c) Silas S. Brown, licensed under Apache 2.

Apache is a registered trademark of The Apache Software Foundation.
Python is a trademark of the Python Software Foundation.
Any other trademarks I mentioned without realising are trademarks of their respective holders.
