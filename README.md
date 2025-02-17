# jianpu-ly

Jianpu in Lilypond, from http://ssb22.user.srcf.net/mwrhome/jianpu-ly.html

(also mirrored at http://ssb22.gitlab.io/mwrhome/jianpu-ly.html just in case, and available via `pip install jianpu-ly` or `pipx run jianpu-ly`)

jianpu-ly is a Python program (compatible with both Python 2 and Python 3) that assists with printing jianpu (numbered musical notation) in the GNU Lilypond music typesetter. The jianpu is written on a modiﬁed-appearance “stave” in Lilypond, which means Lilypond’s typesetting capabilities (lyric spacing, slurs, beams etc) will apply to the jianpu without needing to add a 5-line stave. If you prefer, the generated code for the jianpu stave may also be placed in a score with other types of stave.

使用 jianpu-ly 需要一些技术知识。如果你不知道什么是命令行，什么是文本编辑器，什么是目录，或者什么是Python，那么请在尝试使用 jianpu-ly 之前了解这些内容。它不是像 Frescobaldi 那样的 Lilypond 前端扩展;它是一个预处理器，目前需要你有命令行经验。

If you have problems, try a different Lilypond version.
jianpu-ly works with Lilypond 2.20, 2.22 and 2.24.

Run jianpu-ly < text-file > ly-file (or jianpu-ly text-files > ly-file).  There is experimental support for importing MusicXML via jianpu-ly piece.xml (or jianpu-ly piece.mxl > ly-file) but this does not work for all pieces.

Normal text files are whitespace-separated and can contain words like the following.  Usually the order of characters within a note does not matter, hence #1 is the same as 1# and '1 is the same as 1' and s1 is the same as 1s.

上行音阶： `1 2 3 4 5 6 7 1'`

变音记号： `1 #1 2 b2 1`

八度音阶 `1,, 1, 1 1' 1''`

1'和 2' 的快捷键: `8 9`

Percussion beat: `x`

Change base octave: `< >`

十六分音符、八分音符、四分音符： `s1 q1 1`

十六分音符、八分音符、四分音符的替代方案： `1\\ 1\ 1` （任何\必须写在音高之后而不是之前）

Sticky durations (4 semiquavers then crotchet): `KeepLength s1 1 1 1 c1`

前文提到的音符的附点版本： `s1. q1. 1.`

三十二分音符、六十四分音符： `d1 h1`

二分音符： `1 -`

附点二分音符 `1 - -`

全音符： `1 - - -`

拍号： `4/4`

用八分音符弱起的拍号： `4/4,8`

调号（大调）： `1=Bb`

调号（小调）： `6=F#`

速度： `4=85`

Lyrics: `L: here are the syl- la- bles` (all on one line, or newline after the : and double newline to end)

歌词（第一节）： `L: 1. Here is verse one`

歌词（第二节）： `L: 2. Here is verse two`

汉字歌词： `H: 汉字` （有无空格都可）

Lilypond 页头： `title=标题` （单独一行）

Guitar chords: `chords=c2. g:7 c` (on own line, or newline after the = and double newline to end)

Fret diagrams: `frets=guitar` (on own line)

多声部： `NextPart`

Instrument of current part: `instrument=Flute` (on a line of its own)

多个乐章： `NextScore`

在乐章结束前禁止换页： `OnePage`

禁止为小节编号： `NoBarNums`

禁止首行缩进： `NoIndent`

Ragged last line: `RaggedLast`

旧式拍号： `SeparateTimesig 1=C 4/4`

Indonesian 'not angka' style: `angka`

Alternate Indonesian-style minim, dotted minim and semibreve: `1 . 1 . . 1 . . .` (dot is treated as dash)

增加一个西方五线谱来显示双谱： `WithStaff`

连音： `3[ q1 q1 q1 ]`

前倚音： `g[#45] 1`

后倚音： `1 ['1]g`

Grace notes with durations: `g[d4d5s6] 1`

Simple chords: `,13'5 1 1b3 1` (chord numbers are sorted automatically)

从头反复： `1 1 Fine 1 1 1 1 1 1 DC`

反复跳跃记号： `R{ 1 1 1 } A{ 2 | 3 }`

Short repeats (percent): `R4{ 1 2 }`

连音线（同Lilypond，如果你不想用短横线）： `1 ~ 1`

圆滑线（同Lilypond）： `1 ( 2 )`

Erhu fingering (applies to previous note): `Fr=0 Fr=4`

Erhu symbol (applies to previous note): `souyin harmonic up down bend tilde`

震音： `1/// - 1///5 -`

Rehearsal letters: `letterA letterB`

Multibar rest: `R*8`

力度变化（适用于之前的音符） `\p \mp \f`

其它Lilypond命令： `\fermata \> \! \( \) etc`

文本： `^"音符上方" _"音符下方"`

Harmonic symbols above main notes: `Harm: (music) :Harm` (main music)

Other Lilypond code: `LP: (block of code) :LP` (each delimeter at start of its line)

Unicode approximation instead of Lilypond: `Unicode`

按声部导出MIDI文件： `PartMidi`

忽略： `% 这是注释`

目前已知问题：1、汉字歌词排列有时会对不齐 2、多声部符号太丑了 3、强制换行（强制将小节换到下一行）的命令没找到 4、波音的命令没找到

Copyright and Trademarks
------------------------

(c) Silas S. Brown, licensed under Apache 2.

Apache is a registered trademark of The Apache Software Foundation.
Python is a trademark of the Python Software Foundation.
Any other trademarks I mentioned without realising are trademarks of their respective holders.
