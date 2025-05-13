# jianpu-ly

Lilypond 简谱，来自 http://ssb22.user.srcf.net/mwrhome/jianpu-ly.html

（为以防万一，还在 http://ssb22.gitlab.io/mwrhome/jianpu-ly.html 上做了镜像，可通过 pip install jianpu-ly 或 pipx run jianpu-ly 获取）。

jianpu-ly 是一个 Python 程序（兼容 Python 2 和 Python 3），用于协助在 GNU 软件 Lilypond 中打印简谱（数字谱）。简谱是写在 Lilypond 中经过修改的 “谱表 ”上的，这意味着 Lilypond 的排版功能（歌词间距、圆滑线、符杠等）将适用于简谱，而无需增加 一个五线谱。如果你愿意，生成的简谱代码也可以与其他类型的谱表一起放在乐谱中。

使用 jianpu-ly 需要一些技术知识。如果你不知道什么是命令行，什么是文本编辑器，什么是目录，或者什么是Python，那么请在尝试使用 jianpu-ly 之前了解这些内容。它不是像 Frescobaldi 那样的 Lilypond 前端扩展；它是一个预处理器，目前需要你有命令行经验。

如果你有问题，可以尝试不同的 Lilypond 版本。jianpu-ly 适用于 Lilypond 2.20、2.22 和 2.24。

运行 jianpu-ly < 文件名.txt > 文件名.ly（或 jianpu-ly 文件名.txt > 文件名.ly）。通过jianpu-ly piece.xml （或 jianpu-ly piece.mxl > 文件名.ly）可以导入MusicXML，但这是实验性质的，并不适合所有乐曲。

普通文本文件以空格分隔的，可以包含类似下面这样的字词。通常，音符中字符的顺序并不重要，因此 #1 与 1# 相同，'1 与 1' 相同，s1 与 1s 相同。

上行音阶： `1 2 3 4 5 6 7 1'`

变音记号： `1 #1 2 b2 1`

八度音阶： `1,, 1, 1 1' 1''`

1'和 2' 的快捷键： `8 9`

打击乐节拍： `x`

高低八度记号： `< >`

十六分音符、八分音符、四分音符： `s1 q1 1`

十六分音符、八分音符、四分音符的替代方案： `1\\ 1\ 1` （任何\必须写在音高之后而不是之前）

保持不变的时值 （4个十六分音符+1个四分音符）： `KeepLength s1 1 1 1 c1`

前文提到的音符的附点版本： `s1. q1. 1.`

替代方案也可以附点： `1\\. 1\.`

三十二分音符、六十四分音符： `d1 h1`

二分音符： `1 -`

附点二分音符： `1 - -`

全音符： `1 - - -`

拍号： `4/4`

以八分音符的时值弱起的拍号： `4/4,8`

调号（大调）： `1=Bb`

调号（小调）： `6=F#`

速度： `4=85`

歌词： `L: here are the syl- la- bles` （单独一行，或在:之后换行输入，并以2个空行结束）

歌词（第一节）： `L: 1. Here is verse one`

歌词（第二节）： `L: 2. Here is verse two`

汉字歌词： `H: 汉字` （有无空格都可）

Lilypond 页头： `title=标题` （单独一行）

吉他和弦符号： `chords=c2. g:7 c` （单独一行，或在=之后换行输入，并以2个空行结束）

和弦指板图： `frets=guitar` （单独一行）

变换吉他和弦为罗马数字: `ChordsRoman`

多声部： `NextPart`

当前分谱使用的乐器： `instrument=Flute` （单独一行）

多个乐章： `NextScore`

在乐章结束前禁止换页： `OnePage`

禁止为小节编号： `NoBarNums`

禁止首行缩进： `NoIndent`

最后一行不规则对齐： `RaggedLast`

旧式拍号： `SeparateTimesig 1=C 4/4`

印尼 not angka 风格： `angka`

交替使用印尼风格的二分音符、附点二分音符和全音符： `1 . 1 . . 1 . . .` （点被视为破折号）

增加一个西方五线谱来显示双谱： `WithStaff`

连音： `3[ q1 q1 q1 ]`

前倚音： `g[#45] 1`

后倚音： `1 ['1]g`

带时值的倚音： `g[d4d5s6] 1`

简单和弦： `,135' 1 1b3 1`

从头反复： `1 1 Fine 1 1 1 1 1 1 DC`

反复跳跃记号： `R{ 1 1 1 } A{ 2 | 3 }`

小节反复（％）： `R4{ 1 2 }`

延音线（同 Lilypond，如果你不想用短横线）： `1 ~ 1`

圆滑线（同 Lilypond）： `1 ( 2 )`

二胡指法符号（适用于前一个音符）： `Fr=0 Fr=4`

二胡其它符号（适用于前一个音符）： `souyin harmonic up down bend tilde`

震音： `1/// - 1///5 -`

排练记号： `letterA letterB`

多小节休止： `R*8`

力度记号（适用于之前的音符）： `\p \mp \f`

其它一語 Lilypond \ 指令： `\fermata \> \! \( \) 等等`

文字： `^"音符上方" _"音符下方"`

主音符上的泛音符号： `Harm: (音乐) :Harm` （主音乐）

诗歌的器乐部分： `1 [( 2 3 )] 4`

其它 Lilypond 代码： `LP: (代码块) :LP` （每个分隔符必须位于各行行首）

用 Unicode 近似值代替 Lilypond 代码： `Unicode`

按声部导出MIDI文件： `PartMidi`

忽略： `% 注释`


版权和商标
------------------------

(c) Silas S. Brown，经 Apache 2.0 许可。

Apache 是 Apache 软件基金会的注册商标。Python 是 Python 软件基金会的商标。我无意中提到的任何其他商标均为其各自持有人的商标。
