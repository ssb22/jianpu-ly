#!/usr/bin/env python
# -*- coding: utf-8 -*-
# (can be run with either Python 2 or Python 3)

r"""
# Jianpu (numbered musical notaion) for Lilypond
# v1.887 (c) 2012-2026 Silas S. Brown
# v1.826 (c) 2024 Unbored

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Home: https://ssb22.user.srcf.net/mwrhome/jianpu-ly.html
# Git repository: https://github.com/ssb22/jianpu-ly
# and on GitLab: https://gitlab.com/ssb22/jianpu-ly
# and on Bitbucket: https://bitbucket.org/ssb22/jianpu-ly
# + at https://gitlab.developers.cam.ac.uk/ssb22/jianpu-ly
# and in China: https://gitee.com/ssb22/jianpu-ly

# All lines with : below are used for the input examples table.
# Currently, any non-ASCII character in a line (before the : if any) indicates it's the Chinese version.

Run jianpu-ly < text-file > ly-file (or jianpu-ly text-files > ly-file).  There is experimental support for importing MusicXML via jianpu-ly piece.xml (or jianpu-ly piece.mxl > ly-file) but this does not work for all pieces.
# (compressed MusicXML must be .mxl)
运行 jianpu-ly < 文件名.txt > 文件名.ly（或 jianpu-ly 文件名.txt > 文件名.ly）。通过jianpu-ly piece.xml （或 jianpu-ly piece.mxl > 文件名.ly）可以导入MusicXML，但这是实验性质的，不适合所有乐曲。
Normal text files are whitespace-separated and can contain words like the following.  Usually the order of characters within a note does not matter, hence #1 is the same as 1# and '1 is the same as 1' and s1 is the same as 1s.
普通文本文件以空格分隔的，可以包含类似下面这样的字词。通常，音符中字符的顺序并不重要，因此 #1 与 1# 相同，'1 与 1' 相同，s1 与 1s 相同。
Scale going up: 1 2 3 4 5 6 7 1'
上行音阶： 1 2 3 4 5 6 7 1'
Accidentals: 1 #1 2 b2 1
变音记号： 1 #1 2 b2 1
Octaves: 1,, 1, 1 1' 1''
八度音阶： 1,, 1, 1 1' 1''
Shortcuts for 1' and 2': 8 9
1'和 2' 的快捷键： 8 9
Percussion beat: x
打击乐节拍： x
Change base octave: < >
高低八度记号： < >
Semiquaver, quaver, crotchet (16/8/4th notes): s1 q1 1
十六分音符、八分音符、四分音符： s1 q1 1
Alternate way to input semiquaver, quaver, crotchet: 1\\ 1\ 1 (any \ must go after the pitch not before)
十六分音符、八分音符、四分音符的替代方案： 1\\ 1\ 1 （任何\必须写在音高之后而不是之前）
Sticky durations (4 semiquavers then crotchet): KeepLength s1 1 1 1 c1
保持不变的时值 （4个十六分音符+1个四分音符）： KeepLength s1 1 1 1 c1
Dotted versions of the above (50% longer): s1. q1. 1.
前文提到的音符的附点版本： s1. q1. 1.
Alternate dotted versions: 1\\. 1\.
替代方案也可以附点： 1\\. 1\.
Demisemiquaver, hemidemisemiquaver (32/64th notes): d1 h1
三十二分音符、六十四分音符： d1 h1
Minims (half notes) use dashes: 1 -
二分音符： 1 -
Dotted minim: 1 - -
附点二分音符： 1 - -
Semibreve (whole note): 1 - - -
全音符： 1 - - -
Time signature: 4/4
拍号： 4/4
Time signature with anacrusis (pickup): 4/4,8 or 6/8,4 (number after comma is pickup's value)
带弱起的拍号： 4/4,8 或 6/8,4 （逗号后的数字表示弱起音符的时值分母）
Key signature (major): 1=Bb
调号（大调）： 1=Bb
Key signature (minor): 6=F#
调号（小调）： 6=F#
Tempo: 4=85
速度： 4=85
Lyrics: L: here are the syl- la- bles (all on one line, or newline after the : and double newline to end)
歌词： L: here are the syl- la- bles （单独一行，或在:之后换行输入，并以2个空行结束）
Lyrics (verse 1): L: 1. Here is verse one
歌词（第一节）： L: 1. Here is verse one
Lyrics (verse 2): L: 2. Here is verse two
歌词（第二节）： L: 2. Here is verse two
Hanzi lyrics (auto space): H: hanzi (with or without spaces)
汉字歌词： H: 汉字 （有无空格都可）
Hanzi lyrics (verse 1): H: 1. 这是第一节
汉字歌词（第1节）： H: 1. 这是第一节
Lilypond title: title=the title (on a line of its own)
Lilypond 标题： title=标题 （单独一行）
Other Lilypond headers: subtitle= composer= poet= arranger= copyright= opus= etc.
其它Lilypond页头： subtitle= composer= poet= arranger= copyright= opus= 等
Guitar chords: chords=c2. g:7 c (on own line, or newline after the = and double newline to end)
吉他和弦符号： chords=c2. g:7 c （单独一行，或在=之后换行输入，并以2个空行结束）
Fret diagrams: frets=guitar (on own line)
和弦指板图： frets=guitar （单独一行）
Change guitar chords into Roman numerals: ChordsRoman
变换吉他和弦为罗马数字: ChordsRoman
Multiple parts: NextPart
多声部： NextPart
Instrument of current part: instrument=Flute (on a line of its own)
当前分谱使用的乐器： instrument=Flute （单独一行）
Multiple movements: NextScore
多个乐章： NextScore
Prohibit page breaks until end of this movement: OnePage
在乐章结束前禁止换页： OnePage
Suppress bar numbers: NoBarNums
禁止为小节编号： NoBarNums
Suppress first-line indent: NoIndent
禁止首行缩进： NoIndent
Ragged last line: RaggedLast
最后一行不规则对齐： RaggedLast
Old-style time signature: SeparateTimesig 1=C 4/4
旧式拍号： SeparateTimesig 1=C 4/4
Indonesian 'not angka' style: angka
印尼 not angka 风格： angka
Alternate Indonesian-style minim, dotted minim and semibreve: 1 . 1 . . 1 . . . (dot is treated as dash)
交替使用印尼风格的二分音符、附点二分音符和全音符： 1 . 1 . . 1 . . . （点被视为破折号）
Add a Western staff doubling the tune: WithStaff
增加一个西方五线谱来显示双谱： WithStaff
Tuplets: 3[ q1 q1 q1 ]
连音： 3[ q1 q1 q1 ]
Grace notes before: g[#45] 1
前倚音： g[#45] 1
Grace notes after: 1 ['1]g
后倚音： 1 ['1]g
Grace notes with durations: g[d4d5s6] 1
带时值的倚音： g[d4d5s6] 1
Simple chords: ,135' 1 1b3 1
简单和弦： ,135' 1 1b3 1
Grace note chords: g[1&3&5] 1
倚音和弦： g[1&3&5] 1
Arpeggiated chords: arpUp 135 arpDown 531 arp 135
琵音和弦： arpUp 135 arpDown 531 arp 135
Da capo: 1 1 Fine 1 1 1 1 1 1 DC
从头反复： 1 1 Fine 1 1 1 1 1 1 DC
Dal segno: 1 1 Segno 1 1 ToCoda 1 1 DS 1 1
从𝄋反复： 1 1 Segno 1 1 ToCoda 1 1 DS 1 1
Repeat (with alternate endings): R{ 1 1 1 } A{ 2 | 3 }
反复跳跃记号： R{ 1 1 1 } A{ 2 | 3 }
Short repeats (percent): R4{ 1 2 }
小节反复（％）： R4{ 1 2 }
Ties (like Lilypond's, if you don't want dashes): 1 ~ 1
延音线（同 Lilypond，如果你不想用短横线）： 1 ~ 1
Slurs (like Lilypond's): 1 ( 2 )
圆滑线（同 Lilypond）： 1 ( 2 )
Erhu fingering (applies to previous note): Fr=0 Fr=4
二胡指法符号（适用于前一个音符）： Fr=0 Fr=4
Erhu slide: slideUp 1 slideDown 2
二胡滑音： slideUp 1 slideDown 2
Erhu custom slide: slide=𝆱 1
二胡定做滑音：slide=𝆱 1
Erhu symbol (applies to previous note): souyin harmonic up down bend tilde
二胡其它符号（适用于前一个音符）： souyin harmonic up down bend tilde
Tremolo: 1/// - 1///5 -
震音： 1/// - 1///5 -
Glissando: glis 1 - 5
滑音： glis 1 - 5
Rehearsal marks: letterA letterB letter3 letterAA
排练记号： letterA letterB letter3 letterAA
Multibar rest: R*8
多小节休止： R*8
Dynamics (applies to previous note): \p \mp \f
力度记号（适用于之前的音符）： \p \mp \f
Other 1-word Lilypond \ commands: \fermata \> \! \( \) etc
其它一語 Lilypond \ 指令： \fermata \> \! \( \) 等等
Text: ^"above note" _"below note"
文字： ^"音符上方" _"音符下方"
Harmonic symbols above main notes: Harm: (music) :Harm (main music)
主音符上的泛音符号： Harm: (音乐) :Harm （主音乐）
Instrumental breaks in vocal music: 1 [( 2 3 )] 4
诗歌的器乐部分： 1 [( 2 3 )] 4
Repeat same-bar accidentals in awkward passages: RepeatAccidentals #5 #2 #4 #5 NormalAccidentals
复杂乐句中重复同小节变音记号: RepeatAccidentals #5 #2 #4 #5 NormalAccidentals
Barlines: \bar "||" or \bar "|." etc (no LP: needed)
小节线： \bar "||" 或 \bar "|." 等（无需 LP:）
Other Lilypond code: LP: (block of code) :LP (each delimeter at start of its line)
其它 Lilypond 代码： LP: (代码块) :LP （每个分隔符必须位于各行行首）
Lilypond header additions: LPH: (definitions) :LPH (each at start of line)
Lilypond头代码： LPH: (定义) :LPH （每个分隔符必须位于各行行首）
Split MIDI files per part: PartMidi
按声部导出MIDI文件： PartMidi
Ignored: % a comment
忽略： % 注释
"""

import sys,os,re,shutil,tempfile
from fractions import Fraction as F # requires Python 2.6+
if type(u"")==type(""): # Python 3
    unichr = chr
    from string import ascii_letters as letters
    from subprocess import getoutput
else: # Python 2 in case anyone has to run on an old machine
    range = xrange
    from string import letters
    from commands import getoutput
def asUnicode(l):
    if type(l)==type(u""): return l
    return l.decode('utf-8')

def lilypond_minor_version():
    global _lilypond_minor_version
    try: return _lilypond_minor_version
    except: pass
    cmd = lilypond_command()
    if cmd:
        m=re.match(r".*ond-2\.([1-9][0-9])\.",cmd)
        if m: _lilypond_minor_version = int(m.group(1))
        else: _lilypond_minor_version = int(getoutput(cmd+" --version").split()[2].split('.')[1])
    else: _lilypond_minor_version = 22 # assume 2.22 if we can't figure it out
    return _lilypond_minor_version

def lilypond_command():
    if hasattr(shutil,'which'):
        w = shutil.which('lilypond')
        if w: return 'lilypond'
    elif not sys.platform.startswith("win"):
        cmd = getoutput('which lilypond 2>/dev/null')
        if os.path.exists(cmd): return 'lilypond'
        placesToTry = ['/Applications/LilyPond.app/Contents/Resources/bin/lilypond'] # e.g. from Mac OS 10.4-10.14 Intel build https://web.archive.org/web/20221121202056/https://lilypond.org/download/binaries/darwin-x86/lilypond-2.22.2-1.darwin-x86.tar.bz2 (unpacked and moved to /Applications), or similarly 2.20 for macOS 10.15+ from https://gitlab.com/marnen/lilypond-mac-builder/-/package_files/9872804/download
        placesToTry = ['/Applications/LilyPond-2.22.2.app/Contents/Resources/bin/lilypond','/Applications/LilyPond-2.20.0.app/Contents/Resources/bin/lilypond'] + placesToTry # if renamed from the above (try specific versions 1st, in case default is older)
        placesToTry += ['lilypond-2.24.0/bin/lilypond','/opt/lilypond-2.24.0/bin/lilypond'] # if unpacked 2.24 (which drops the .app; in macOS 13, might need first to manually open at least lilypond and gs binaries for Gatekeeper approval if installing it this way)
        for t in placesToTry:
            if os.path.exists(t): return t

staff_size = float(os.environ.get("j2ly_staff_size",20))
# Normal: j2ly_staff_size=20
# Large: j2ly_staff_size=25.2
# Small: j2ly_staff_size=17.82
# Tiny: j2ly_staff_size=15.87
lyric_size = float(os.environ.get("j2ly_lyric_size",staff_size))

three_dots = u"\u22EE"
if not type(u"")==type(""): three_dots = three_dots.encode('utf-8') # Python 2

def find_grace_height(music):
    # Need 3.5 if there's demisemiquavers with 2 octaves below.
    # Can have 2.5 if there aren't any of those
    # (Probably looks better if height is the same throughout the score,
    # so we scan ahead to find what the most complex thing is.
    # Could also change height every time, but would need to
    # check if high-quality published music does that.
    # Change every time would require differently parameterised versions of jianpu-grace-curve-stencil though.)
    global grace_height
    grace_height = 2.5
    for word in music.split():
        if word.startswith("g[") or word.endswith("]g"):
            if "d" in word or "h" in word or ",," in word:
                grace_height = 3.5 ; break
                # TODO: more options, e.g. 3.0 if "d" but not ",," ?  (will need to update grace_height dictionary in all_scores_start also)

def all_scores_start(inDat):
    if lilypond_minor_version() < 20: errExit("Lilypond 2.18 and below is no longer supported") # 2.20 is in Ubuntu 20.04 LTS and 2.22 can be installed on a 2011 Mac; if you're stuck on an older EOL'd distro without Internet, use jianpu-ly 1.825 or earlier
    r = r"""\version "2.%d.0"
#(set-global-staff-size %g)""" % ((
    22 if "g[" in inDat or "]g" in inDat else 20),staff_size)
    r += r"""

% un-comment the next line to remove Lilypond tagline:
% \header { tagline="" }

% comment out the next line if you're debugging jianpu-ly
% (but best leave it un-commented in production, since
% the point-and-click locations won't go to the user input)
\pointAndClickOff

\paper {
  print-all-headers = ##t %% allow per-score headers

  % un-comment the next line for A5:
  % #(set-default-paper-size "a5" )

  % un-comment the next line for no page numbers:
  % print-page-number = ##f

  % un-comment the next 3 lines for a binding edge:
  % two-sided = ##t
  % inner-margin = 20\mm
  % outer-margin = 10\mm

  % un-comment the next line for a more space-saving header layout:
  % scoreTitleMarkup = \markup { \center-column { \fill-line { \magnify #1.5 { \bold { \fromproperty #'header:dedication } } \magnify #1.5 { \bold { \fromproperty #'header:title } } \fromproperty #'header:composer } \fill-line { \fromproperty #'header:instrument \fromproperty #'header:subtitle \smaller{\fromproperty #'header:subsubtitle } } } }
"""
    if os.path.exists("/Library/Fonts/Arial Unicode.ttf"): r += r"""
  % As jianpu-ly was run on a Mac, we include a Mac fonts workaround.
  % The Mac version of Lilypond 2.18 used Arial Unicode MS as a
  % fallback even in the Serif font, but 2.20 drops this in Serif
  % (using it only in Sans), which means any Serif text (titles,
  % lyrics etc) that includes Chinese will likely fall back to
  % Japanese fonts which don't support all Simplified hanzi.
  % This brings back 2.18's behaviour on 2.20+:
  #(define fonts
    (set-global-fonts
     #:roman "Source Serif Pro,Source Han Serif SC,Times New Roman,Arial Unicode MS"
     #:factor (/ staff-height pt 20)
    ))
"""
    if has_lyrics: r += r"""
  % Might need to enforce a minimum spacing between systems, especially if lyrics are below the last staff in a system and numbers are on the top of the next
  system-system-spacing = #'((basic-distance . 7) (padding . 5) (stretchability . 1e7))
  score-markup-spacing = #'((basic-distance . 9) (padding . 5) (stretchability . 1e7))
  score-system-spacing = #'((basic-distance . 9) (padding . 5) (stretchability . 1e7))
  markup-system-spacing = #'((basic-distance . 2) (padding . 2) (stretchability . 0))
"""
    r += "}\n" # end of \paper block

    r += r"""
%% 2-dot and 3-dot articulations
#(append! default-script-alist
   (list
    `(two-dots
       . (
           (script-priority . -200)
           (stencil . ,ly:text-interface::print)
           (text . ,#{ \markup \override #'(font-encoding . latin1) \center-align \bold ":" #})
           (padding . 0.20)
           (avoid-slur . inside)
           (side-axis . ,Y)
           (direction . ,UP)))))
#(append! default-script-alist
   (list
    `(three-dots
       . (
           (script-priority . -200)
           (stencil . ,ly:text-interface::print)
           (text . ,#{ \markup \override #'(font-encoding . latin1) \center-align \bold """+'"'+three_dots+'"'+r""" #})
           (padding . 0.30)
           (avoid-slur . inside)
           (side-axis . ,Y)
           (direction . ,UP)))))
"two-dots" =
#(make-articulation 'two-dots)

"three-dots" =
#(make-articulation 'three-dots)

\layout {
  \context {
    \Score
    scriptDefinitions = #default-script-alist
  }
}

note-mod =
#(define-music-function
     (text note)
     (markup? ly:music?)
   #{
     \tweak NoteHead.stencil #ly:text-interface::print
     \tweak NoteHead.text
        \markup \lower #0.5 \sans \bold #text
     \tweak Rest.stencil #ly:text-interface::print
     \tweak Rest.text
        \markup \lower #0.5 \sans \bold #text
     #note
   #})

#(define (jianpu-glissando grob)
   (let* ((left-note (ly:spanner-bound grob LEFT))
          (right-note (ly:spanner-bound grob RIGHT))
          (left-y (ly:grob-property left-note 'Y-offset 0))
          (right-y (ly:grob-property right-note 'Y-offset 0))
          (left-event (ly:grob-property left-note 'cause))
          (right-event (ly:grob-property right-note 'cause))
          (left-pitch (and (ly:stream-event? left-event)
                           (ly:event-property left-event 'pitch)))
          (right-pitch (and (ly:stream-event? right-event)
                            (ly:event-property right-event 'pitch))))
     (if (and left-pitch right-pitch)
         (let* ((left-y-off (if (ly:pitch<? left-pitch right-pitch) (- left-y 0.9) (if (ly:pitch<? right-pitch left-pitch) (+ left-y 1.5) left-y)))
                (right-y-off (if (ly:pitch<? left-pitch right-pitch) (+ right-y 0.9) (if (ly:pitch<? right-pitch left-pitch) (- right-y 1.5) right-y)))
                (bd (ly:grob-property grob 'bound-details))
                (left-bd (list-copy (assoc-get 'left bd '())))
                (right-bd (list-copy (assoc-get 'right bd '())))
                (new-left-bd (assoc-set! left-bd 'Y left-y-off))
                (new-right-bd (assoc-set! right-bd 'Y right-y-off))
                (new-bd (list (cons 'left new-left-bd) (cons 'right new-right-bd))))
           (ly:grob-set-property! grob 'bound-details new-bd)))))"""
    if re.search(r"(\s|^)(angka|Indonesian)(\s|$)",inDat): r += r"""
note-mod-angka = #(define-music-function (text note) (markup? ly:music?)
   #{ \tweak NoteHead.stencil #ly:text-interface::print
     \tweak NoteHead.text \markup \lower #0.5 \bold #text
     \tweak Rest.stencil #ly:text-interface::print
     \tweak Rest.text \markup \lower #0.5 \bold #text
     #note #})
"""
    if inner_beams_below: r += r"""
#(define (flip-beams grob)
   (ly:grob-set-property!
    grob 'stencil
    (ly:stencil-translate
     (let* ((stl (ly:grob-property grob 'stencil))
            (centered-stl (ly:stencil-aligned-to stl Y DOWN)))
       (ly:stencil-translate-axis
        (ly:stencil-scale centered-stl 1 -1)
        (* (- (car (ly:stencil-extent stl Y)) (car (ly:stencil-extent centered-stl Y))) 0) Y))
     (cons 0 -0.8))))
"""
# Draw grace curve according to start and end mark.
# Modify from https://lists.gnu.org/archive/html/lilypond-user/2015-01/msg00142.html
    r += r"""
%%=======================================================
#(define-event-class 'jianpu-grace-curve-event 'span-event)

#(define (add-grob-definition grob-name grob-entry)
   (set! all-grob-descriptions
         (cons ((@@ (lily) completize-grob-entry)
                (cons grob-name grob-entry))
               all-grob-descriptions)))

#(define (jianpu-grace-curve-stencil grob)
   (let* ((elts (ly:grob-object grob 'elements))
          (refp-X (ly:grob-common-refpoint-of-array grob elts X))
          (X-ext (ly:relative-group-extent elts refp-X X))
          (refp-Y (ly:grob-common-refpoint-of-array grob elts Y))
          (Y-ext (ly:relative-group-extent elts refp-Y Y))
          (direction (ly:grob-property grob 'direction RIGHT))
          (x-start (* 0.5 (+ (car X-ext) (cdr X-ext))))
          (y-start (+ (car Y-ext) %g))
          (x-start2 (if (eq? direction RIGHT)(+ x-start 0.5)(- x-start 0.5)))
          (x-end (if (eq? direction RIGHT)(+ (cdr X-ext) 0.2)(- (car X-ext) 0.2)))
          (y-end (- y-start 0.5))
          (stil (ly:make-stencil `(path 0.1
                                        (moveto ,x-start ,y-start
                                         curveto ,x-start ,y-end ,x-start ,y-end ,x-start2 ,y-end
                                         lineto ,x-end ,y-end))
                                  X-ext
                                  Y-ext))
          (offset (ly:grob-relative-coordinate grob refp-X X)))
     (ly:stencil-translate-axis stil (- offset) X)))

#(add-grob-definition
  'JianpuGraceCurve
  `(
     (stencil . ,jianpu-grace-curve-stencil)
     (meta . ((class . Spanner)
              (interfaces . ())))))

#(define jianpu-grace-curve-types
   '(
      (JianpuGraceCurveEvent
       . ((description . "Used to signal where curve encompassing music start and stop.")
          (types . (general-music jianpu-grace-curve-event span-event event))
          ))
      ))

#(set!
  jianpu-grace-curve-types
  (map (lambda (x)
         (set-object-property! (car x)
           'music-description
           (cdr (assq 'description (cdr x))))
         (let ((lst (cdr x)))
           (set! lst (assoc-set! lst 'name (car x)))
           (set! lst (assq-remove! lst 'description))
           (hashq-set! music-name-to-property-table (car x) lst)
           (cons (car x) lst)))
    jianpu-grace-curve-types))

#(set! music-descriptions
       (append jianpu-grace-curve-types music-descriptions))

#(set! music-descriptions
       (sort music-descriptions alist<?))


#(define (add-bound-item spanner item)
   (if (null? (ly:spanner-bound spanner LEFT))
       (ly:spanner-set-bound! spanner LEFT item)
       (ly:spanner-set-bound! spanner RIGHT item)))

jianpuGraceCurveEngraver =
#(lambda (context)
   (let ((span '())
         (finished '())
         (current-event '())
         (event-start '())
         (event-stop '()))
     `(
       (listeners
        (jianpu-grace-curve-event .
          ,(lambda (engraver event)
             (if (= START (ly:event-property event 'span-direction))
                 (set! event-start event)
                 (set! event-stop event)))))

       (acknowledgers
        (note-column-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)
                  (add-bound-item span grob)))
             (if (ly:spanner? finished)
                 (begin
                  (ly:pointer-group-interface::add-grob finished 'elements grob)
                  (add-bound-item finished grob)))))
        (inline-accidental-interface .
          ,(lambda (engraver grob source-engraver)
             (if (ly:spanner? span)
                 (begin
                  (ly:pointer-group-interface::add-grob span 'elements grob)))
             (if (ly:spanner? finished)
                 (ly:pointer-group-interface::add-grob finished 'elements grob))))
        (script-interface .
          ,(lambda (engraver grob source-engraver)
             (let ((is-dyn (or (grob::has-interface grob 'dynamic-interface)
                       (eq? (ly:grob-property grob 'meta) 'DynamicText))))
               (if (and (ly:spanner? span) (not is-dyn))
                (ly:pointer-group-interface::add-grob span 'elements grob))
               (if (and (ly:spanner? finished) (not is-dyn))
                (ly:pointer-group-interface::add-grob finished 'elements grob))))))
       (process-music .
         ,(lambda (trans)
            (if (ly:stream-event? event-stop)
                (if (null? span)
                    (ly:warning "No start to this curve.")
                    (begin
                     (set! finished span)
                     (ly:engraver-announce-end-grob trans finished event-start)
                     (set! span '())
                     (set! event-stop '()))))
            (if (ly:stream-event? event-start)
                (begin
                 (set! span (ly:engraver-make-grob trans 'JianpuGraceCurve event-start))
                 (set! event-start '())))))
       
       (stop-translation-timestep .
         ,(lambda (trans)
            (if (and (ly:spanner? span)
                     (null? (ly:spanner-bound span LEFT)))
                (ly:spanner-set-bound! span LEFT
                  (ly:context-property context 'currentMusicalColumn)))
            (if (ly:spanner? finished)
                (begin
                 (if (null? (ly:spanner-bound finished RIGHT))
                     (ly:spanner-set-bound! finished RIGHT
                       (ly:context-property context 'currentMusicalColumn)))
                 (set! finished '())
                 (set! event-start '())
                 (set! event-stop '())))))
       
       (finalize
        (lambda (trans)
          (if (ly:spanner? finished)
              (begin
               (if (null? (ly:spanner-bound finished RIGHT))
                   (set! (ly:spanner-bound finished RIGHT)
                         (ly:context-property context 'currentMusicalColumn)))
               (set! finished '())))))
       )))

jianpuGraceCurveStart =
#(make-span-event 'JianpuGraceCurveEvent START)

jianpuGraceCurveEnd =
#(make-span-event 'JianpuGraceCurveEvent STOP)
%%===========================================================
""" % {3.5: -0.2, 2.5: +0.32}[grace_height]
    return r+"\n%{ The jianpu-ly input was:\n" + inDat.strip().replace("%}","%/}")+"\n%}\n\n"

def score_start():
    ret = "\\score {\n"
    if midi: ret += "\\unfoldRepeats\n"
    ret += r"<< "
    if not notehead_markup.noBarNums and not midi: ret += ("\\override Score.BarNumber #'break-visibility = #center-visible\n\\override Score.BarNumber #'Y-offset = -1\n\\set Score.barNumberVisibility = #(every-nth-bar-number-visible %d)" % bar_number_every)
    return ret
bar_number_every = 5 # TODO customise?  (anyway don't leave it numbering at start of system, doesn't work well in jianpu+lyrics)

def score_end(**headers):
    ret = ">>\n"
    if headers:
        # since about Lilypond 2.7, music must come
        # before the header block if it's per-score
        ret += r"\header{"+'\n'
        for k,v in headers.items(): ret+=k+'="'+v+'"\n'
        ret += "}\n"
    layoutExtra = ""
    if not lyric_size == staff_size:
        from math import log
        lSize = log(lyric_size/staff_size)*6/log(2)
        if lSize > 3: sys.stderr.write("WARNING: potential layout problems; consider increasing j2ly_staff_size to be closer to j2ly_lyric_size\n") # TODO: is 3 a good threshold for this warning?  (need to check different Lilypond versions)
        layoutExtra=r" \override Lyrics.LyricText.font-size = #"+("+" if lSize>=0 else "")+str(lSize)+" "
    if notehead_markup.noIndent: layoutExtra += ' indent = 0.0 '
    if notehead_markup.raggedLast: layoutExtra += ' ragged-last = ##t '
    if notehead_markup.noBarNums: layoutExtra += r' \context { \Score \remove "Bar_number_engraver" } '
    if notehead_markup.chordsRoman: layoutExtra += r"\context { \ChordNames \consists #(lambda (cx) (let ((tonic #{ c #})) (make-engraver ((initialize engraver) (set! (ly:context-property cx 'chordRootNamer) (lambda (pitch capitalized) (let ((degree (1+ (ly:pitch-notename (ly:pitch-diff pitch tonic)))) (style (if capitalized 'roman-lower 'roman-upper))) (number-format style degree))))) (listeners ((key-change-event engraver event) (set! tonic (ly:event-property event 'tonic))))))) } " # based on a lists.gnu.org snippet
    if midi: ret += r"\midi { \context { \Score tempoWholesPerMinute = #(ly:make-moment 84 4)}}" # will be overridden by any \tempo command used later
    else: ret += r"\layout{"+layoutExtra+r"""
  \context {
    \Global
    \grobdescriptions #all-grob-descriptions
  }
""" + "}"
    return ret + " }"

def uniqName():
    global uniqCount
    r = str(uniqCount) ; uniqCount += 1
    return r.translate((letters*5)[:256])
def jianpu_voice_start(isTemp=0):
    if not isTemp and maxBeams >= 2: stemLenFrac = "0.5" # sometimes needed if the semiquavers occur in isolation rather than in groups (TODO do we need to increase this for 3+ beams in some cases?)
    else: stemLenFrac = "0"
    voiceName = uniqName()
    r = (r"""\new Voice="%s" {"""%voiceName)
    r += r"""
    \override Beam #'transparent = ##f"""
    if not_angka:
        r +=r"""
        \override Stem #'direction = #UP
        \override Tie #'staff-position = #-2.5
        \tupletDown"""
        stemLenFrac=str(0.4+0.2*max(0,maxBeams-1))
    else: r += r"""
    \override Stem #'direction = #DOWN
    \override Tie #'staff-position = #2.5
    \tupletUp
    \tieUp"""
    r += (r"""
    \override Stem #'length-fraction = #%s
    \override Beam #'beam-thickness = #0.1
    \override Beam #'length-fraction = #0.5
    %s
    \override Voice.Rest #'style = #'neomensural %% this size tends to line up better (we'll override the appearance anyway)
    \override Accidental #'font-size = #-4
    \override TupletBracket #'bracket-visibility = ##t""" %
          (stemLenFrac,
           r"\override Beam.after-line-breaking = #flip-beams" if inner_beams_below else ""
           ))
    return r+"\n", voiceName
def jianpu_staff_start(inst=None):
    # (we add "BEGIN JIANPU STAFF" and "END JIANPU STAFF" comments to make it easier to copy/paste into other Lilypond files)
    if notehead_markup.withStaff: inst = None # we'll put the label on the 5-line staff (TODO: use StaffGroup or something?)
    if not_angka: r=r"""
%% === BEGIN NOT ANGKA STAFF ===
    \new RhythmicStaff \with {"""
    else: r=r"""
%% === BEGIN JIANPU STAFF ===
    \new RhythmicStaff \with {
    \consists "Accidental_engraver" """
    r += r"""
    \consists \jianpuGraceCurveEngraver
    \omit Staff.DotColumn \omit Voice.Dots \override Glissando.before-line-breaking = #jianpu-glissando"""
    if inst: r += '\ninstrumentName = "'+inst+'"'
    if notehead_markup.withStaff: r+=r"""
   %% Limit space between Jianpu and corresponding-Western staff
   \override VerticalAxisGroup.staff-staff-spacing = #'((minimum-distance . 7) (basic-distance . 7) (stretchability . 0))
""" # (whether this is needed or not depends on Lilypond version; 2.22 puts more space than 2.18,2.20.  Must set higher than 5, which sometimes gets collisions between beams in 2.20)
    r+=r"""
    %% Get rid of the stave but not the barlines:
    \override StaffSymbol #'line-count = #0 %% tested in 2.15.40, 2.16.2, 2.18.0, 2.18.2, 2.20.0 and 2.22.2
    \override BarLine #'bar-extent = #'(-2 . 2) %% LilyPond 2.18: please make barlines as high as the time signature even though we're on a RhythmicStaff (2.16 and 2.15 don't need this although its presence doesn't hurt; Issue 3685 seems to indicate they'll fix it post-2.18)
    $(add-grace-property 'Voice 'Stem 'direction DOWN)
    $(add-grace-property 'Voice 'Slur 'direction UP)
    $(add-grace-property 'Voice 'Stem 'length-fraction 0.5)
    $(add-grace-property 'Voice 'Beam 'beam-thickness 0.1)
    $(add-grace-property 'Voice 'Beam 'length-fraction 0.3)
    $(add-grace-property 'Voice 'Beam 'after-line-breaking flip-beams)
    $(add-grace-property 'Voice 'Beam 'Y-offset %.1f)
    $(add-grace-property 'Voice 'NoteHead 'Y-offset %.1f)
    }
    { """ % (grace_height, grace_height)
    j,voiceName = jianpu_voice_start()
    r += j+r"""
    \override Staff.TimeSignature #'style = #'numbered
    \override Staff.Stem #'transparent = ##t
    """
    if notehead_markup.separateTimesig: r+=r"\override Staff.TimeSignature #'stencil = ##f"+"\n"
    return r, voiceName
def jianpu_staff_end():
     # \bar "|." is added separately if there's not a DC etc
    if not_angka: return "} }\n% === END NOT ANGKA STAFF ===\n"
    else: return "} }\n% === END JIANPU STAFF ===\n"
def midi_staff_start():
    return r"""
%% === BEGIN MIDI STAFF ===
    \new Staff { \new Voice="%s" {""" % (uniqName(),)
def midi_staff_end(): return "} }\n% === END MIDI STAFF ===\n"
def western_staff_start(inst=None):
    r = r"""
%% === BEGIN 5-LINE STAFF ===
    \new Staff """
    if inst: r += r'\with { instrumentName = "'+inst+'" } '
    voiceName = uniqName()
    return (r+r"""{
    \override Score.SystemStartBar.collapse-height = #11 %% (needed on 2.22)
    \new Voice="%s" {
    #(set-accidental-style 'modern-cautionary)
    \override Staff.TimeSignature #'style = #'numbered
""" % (voiceName,)), voiceName
def western_staff_end(): return "} }\n% === END 5-LINE STAFF ===\n"

def lyrics_start(voiceName):
    return r'\new Lyrics = "I%s" { \lyricsto "%s" { ' % (uniqName(),voiceName)
def lyrics_end(): return "} }"

inner_beams_below = True # Use stencil reflection to invert Lilypond's normal beam positioning (like in David Zhang's jianpu10a.ly) - more accurately reflects jianpu typography but can result in octave dots being too far down because beam spacing is done per system not per beam
dashes_as_ties = True # Implement dash (-) continuations as invisible ties rather than rests; sometimes works better in awkward beaming situations
use_rest_hack = True # Implement some short rests as notes (and if there are lyrics, creates temporary voices so the lyrics miss them); sometimes works better for beaming (at least in 2.15 through 2.24)
sort_chords = True # Normally should be left as True.  See comment on --nosort below
force_staff = None # None=default (respect input), True=--withStaff forces 5-line staff, False=--noStaff disables it
unicode_approx = xml_octaveShift_override = False

class JlyException(Exception): pass # wrapped so you can catch it if you're calling this code as a module
def errExit(msg):
    if __name__=="__main__":
        sys.stderr.write("Error: "+msg+"\n")
        sys.exit(1)
    else: raise JlyException(msg)
def scoreError(msg,word,line):
    if len(word)>60: word=word[:50]+"..."
    msg += " %s in score %d" % (word,scoreNo)
    if len(line)>600: line=line[:500]+"..."
    if not word in line: pass # above truncations caused problems
    elif "xterm" in os.environ.get("TERM",""): # use xterm underline escapes
        msg += "\n"+re.sub(r"(\s|^)"+re.escape(word)+r"(?=\s|$)",lambda m:m.group(1)+"\x1b[4m"+word+"\x1b[m",line)
    elif re.match('[ -~]*$',line): # all ASCII: we can underline the word with ^^s
        msg += "\n"+line+"\n"+re.sub('[^^]',' ',re.sub(r"(\s|^)"+re.escape(word)+r"(?=\s|$)",lambda m:m.group(1)+'^'*(len(word)),line))
    else: # don't try to underline the word (at least not without ANSI): don't know how the terminal will handle character widths
        msg += "\nin this line: "+line
    errExit(msg)

placeholders = {
    # for accidentals and word-fitting to work
    # (we make them relative to the actual key later
    # so that MIDI pitches are correct)
    '0':'r',
    '1':'c',
    '2':'d',
    '3':'e',
    '4':'f',
    '5':'g',
    '6':'a',
    '7':'b',
    'x':'c',
    '-':'r'}

def addOctaves(octave1,octave2):
    octave2=octave2.replace(">","'").replace("<",",") # so it can be used with a base-octave change
    while octave1:
        if octave1[0] in "'>": # go up
            if ',' in octave2: octave2 = octave2[:-1]
            else: octave2 += "'"
        else: # , or < : go down
            if "'" in octave2: octave2 = octave2[:-1]
            else: octave2 += ","
        octave1=octave1[1:]
    return octave2

class NoteheadMarkup:
  def __init__(self,graceType=None):
      self.initOneScore()
      self.graceType = graceType
      self.separateTimesig = False
  def initOneScore(self):
      self.barLength = 64 ; self.beatLength = 16 # in 64th notes
      self.barPos = self.startBarPos = F(0)
      self.inBeamGroup = self.lastNBeams = self.onePage = self.noBarNums = self.chordsRoman = self.noIndent = self.raggedLast = 0
      self.withStaff = force_staff
      self.keepLength = self.repeatAccidentals = self.pendingSlide = self.pendingArp = 0
      self.octavesPosition = None # or "before" (only setting in v1.847 and below) or "after", affects chords and grace notes when an octave mark is between two figures: is it before or after the note it affects.  Starting at None = no default, must specify if anything's ambiguous
      self.last_octave = self.base_octave = ""
      self.octavesSeen = []
      self.current_accidentals = {} # used to predict whether Lilypond will draw the accidental or not, for beam spacing purposes
      self.barNo = 1
      self.tuplet = (1,1)
      self.last_figures = None
      self.last_was_rest = False
      self.notesHad = []
      self.unicode_approx = []
      self.rplacNextIfStillInBeam = None
      self.graceType = None
      self.current_chord = None
  def endScore(self):
      if self.barPos == self.startBarPos: pass
      elif os.environ.get("j2ly_sloppy_bars"): sys.stderr.write("Wrong bar length at end of score %d ignored (j2ly_sloppy_bars set)\n" % scoreNo)
      elif self.startBarPos and not self.barPos: errExit("Score %d should end with a %g-beat bar to make up for the %g-beat anacrusis bar.  Set j2ly_sloppy_bars environment variable if you really want to break this rule." % (scoreNo,self.startBarPos/self.beatLength,(self.barLength-self.startBarPos)/self.beatLength)) # this is on the music theory syllabi at about Grade 3, but you can get up to Grade 5 practical without actually covering it, so we'd better not expect all users to understand "final bar does not make up for anacrusis bar"
      else: errExit("Incomplete bar at end of score %d (%g beats)" % (scoreNo,self.barPos*1.0/self.beatLength))
  def setTime(self,num,denom):
      self.barLength = int(64*num/denom)
      if denom>4 and num%3==0: self.beatLength = 24 # compound time
      else: self.beatLength = 16
  def setAnac(self,denom,dotted):
      self.barPos = F(self.barLength)-F(64)/denom
      if dotted: self.barPos -= F(64)/denom/2
      if self.barPos<0: errExit("Anacrusis is longer than bar in score %d" % scoreNo) # but anacrusis being exactly equal to bar is OK: we'll just interpret that as no anacrusis
      self.startBarPos = self.barPos
  def wholeBarRestLen(self): return {96:"1.",48:"2.",32:"2",24:"4.",16:"4",12:"8.",8:"8"}.get(self.barLength,"1") # TODO: what if irregular?
  def baseOctaveChange(self,change):
      self.base_octave = addOctaves(change,self.base_octave)
  def __call__(self,figures,nBeams,dots,octave,accidental,tremolo,word,line):
    # figures is a chord string of '1'-'7', or 'x' or '0' or '-'
    # nBeams is 0, 1, 2 .. etc (number of beams for this note)
    # dots is "" or "." or ".." etc (extra length)
    # octave is "", "'", "''", "'''", ",", ",," or ",,,"
    # accidental is "", "#", "b"
    # tremolo is "" or ":32"
    # word,line is for error handling
    if len(figures)>1:
        if accidental and not_angka: scoreError("Accidentals in chords not yet implemented in Indonesian not-angka mode:",word,line)
        if '0' in figures: scoreError("Can't have rest in chord:",word,line)
        if 'x' in figures: scoreError("Can't have percussion beat in chord:",word,line)
    self.notesHad.append(figures)

    isChord = len(figures)>1
    if isChord:
        chord_ret,octave,placeholder_chord = chordNotes_markup(re.sub(r'[\\qsdh.]','',word),word,line,self.graceType) # word w/out durations
        if not midi and not western: placeholder_chord = "c"
    else: # not isChord
        placeholder_chord = placeholders[figures]
    
    invisTieLast = dashes_as_ties and self.last_figures and figures=="-" and not self.last_was_rest
    assert not (use_rest_hack and not dashes_as_ties), "This combination has not been tested"
    self.last_was_rest = (figures=='0' or (figures=='-' and self.last_was_rest))
    aftrLastNonDash = tieEnd = ""
    add_cautionary_accidental = accidental_visible = False
    if invisTieLast: # (so figures == "-")
        if self.barPos==0 and not midi and not western and not self.last_figures=="x":
            # dash over barline: write as new note
            figures = self.last_figures
            aftrLastNonDash = r'\=JianpuTie('
            tieEnd = r'\=JianpuTie)'
            add_cautionary_accidental = accidental_visible = self.last_accidental
            tremolo = self.last_tremolo
        else:
            if self.barPos==0 and not midi and not western and not self.last_figures=="x": sys.stderr.write("Warning: jianpu barline-crossing tie won't be done right because your Lilypond version is older than 2.20\n")
            if self.barPos==0: tremolo = self.last_tremolo
        if self.current_chord: # a chord is currently in progress, and we're extending it with a tie
            isChord = True
            chord_ret,octave,placeholder_chord = chordNotes_markup(re.sub(r'[\\qsdh.]','',self.current_chord),word,line)
            if not midi and not western: placeholder_chord = placeholder_chord[:-1].split()[-1] # just have one note of it for dashes
        else: # not a chord, so we can assume len(self.last_figures)==1
            placeholder_chord = placeholders[self.last_figures]
            octave = self.last_octave # for MIDI or 5-line
            accidental = self.last_accidental # ditto
    else: # not invisTieLast
        if not isChord: octave=addOctaves(octave,self.base_octave)
        if not octave in [",,,",",,",",","","'","''","'''"]: scoreError("Can't handle octave "+octave+" in",word,line)
        self.last_octave = octave
        self.octavesSeen.append({",,,":-3,",,":-2,",":-1,"":0,"'":1,"''":2,"'''":3}[octave]) # for figuring out the best clef with WithStaff
        self.last_tremolo = tremolo
        if isChord: self.current_chord = word
        else: self.current_chord = None
    if not isChord and not accidental in ["","#","b"]: scoreError("Can't handle accidental "+accidental+" in",word,line)
    self.last_accidental = accidental

    ret = ""
    if self.barPos==0 and self.barNo > 1:
        ret += "| " # barline
        if self.onePage and not midi: ret += r"\noPageBreak "
        ret += "%{ bar "+str(self.barNo)+": %} "
        self.notesHad.insert(-1,"|")
    if self.pendingArp: ret += {"arpUp":r"\arpeggioArrowUp ","arpDown":r"\arpeggioArrowDown ","arp":r"\arpeggioNormal "}[self.pendingArp]
    if not octave in self.current_accidentals: self.current_accidentals[octave] = [""]*7
    if nBeams==None: # unspecified
        if self.keepLength:
            nBeams = self.lastNBeams
        else: nBeams = 0
    if figures=="-" or all('1'<=figure<='7' and (not accidental==self.current_accidentals[octave][int(figure)-1] or accidental and self.repeatAccidentals and (not self.last_figures or not figure in self.last_figures)) for figure in list(figures)) and nBeams > self.lastNBeams: leftBeams = nBeams # beam needs to fit under the new accidental (or the dash which might be slightly to the left of where digits are), but if it's no more than last note's beams then we'll hang it only if in same beat.  (TODO: the current_accidentals logic may need revising if other accidental styles are used, e.g. modern-cautionary, although then would need to check anyway if our \consists "Accidental_engraver" is sufficient)
    # TODO: if figures=="0" then that might be typeset a bit to the left as well (because it's also a rest), however extending the line TOO far left in this case could be counterproductive
    elif self.inBeamGroup:
        if nBeams < self.lastNBeams: leftBeams = nBeams
        else: leftBeams = self.lastNBeams
    else: leftBeams = 0
    if leftBeams: assert nBeams, "following logic assumes if (leftBeams or nBeams) == if nBeams"
    aftrlast0 = ""
    if not nBeams and self.inBeamGroup:
        if not self.inBeamGroup=="restHack":
            aftrlast0 = "] "
        self.inBeamGroup = 0
    length = 4 ; b = 0 ; toAdd = F(16) # crotchet
    while b < nBeams: b,length,toAdd = b+1,length*2,toAdd/2
    toAdd0 = toAdd
    for _ in dots:
        toAdd0 /= 2 ; toAdd += toAdd0
    toAdd_preTuplet = toAdd
    if not self.tuplet[0]==self.tuplet[1]:
        toAdd = toAdd*self.tuplet[0]/self.tuplet[1]
    if nBeams and not midi and not western: # must set these unconditionally regardless of what we think their current values are (Lilypond's own beamer can change them from note to note)
        if not_angka:
            leftBeams=nBeams
            if (self.barPos+toAdd)%self.beatLength == 0: nBeams = 0
        ret += (r"\set stemLeftBeamCount = #%d"+"\n") % leftBeams
        ret += (r"\set stemRightBeamCount = #%d"+"\n") % nBeams
        if not_angka: nBeams = leftBeams
    for figure in list(figures):
        if '1'<=figure<='7':
            if not accidental==self.current_accidentals[octave][int(figure)-1] or accidental and self.repeatAccidentals and (not self.last_figures or not figure in self.last_figures):
                accidental_visible = True
            self.current_accidentals[octave][int(figure)-1] = accidental # TODO: assumes accidental applies to EVERY note in a chord, see above
    if not figures=="-": self.last_figures = figures
    inRestHack = replaceLast = 0
    if not midi and not western:
        if ret: ret = ret.rstrip()+"\n" # try to keep the .ly code vaguely readable
        if octave=="''" and not invisTieLast: ret += r"  \once \override Score.TextScript.outside-staff-priority = 45" # inside bar numbers etc
        if self.pendingSlide:
            if not type(self.pendingSlide)==type(""): self.pendingSlide=self.pendingSlide.encode("utf-8") # Python 2
            ret += r' \once \override Accidental.font-size = #0 \once \override Accidental.stencil = #ly:text-interface::print \once \override Accidental.text = \markup { \lower #1.0 "'+self.pendingSlide+r'"'+(r' \hspace #0.2 \magnify #0.6 \musicglyph "accidentals.'+("sharp" if accidental == "#" else "flat")+r'"' if accidental_visible else "")+' } '
            add_cautionary_accidental = accidental_visible = True
            self.pendingSlide = 0
        if figures=="-":
            if not_angka: figureDash=u"."
            else: figureDash=u"\u2013"
            if not type(u"")==type(""):
                figureDash=figureDash.encode('utf-8')
            ret += (r' \note-mod-angka "' if not_angka else r' \note-mod "')+figureDash+'" '
        elif isChord: ret += chord_ret
        else: # single, non-dash note
            s = str(figures)
            if not_angka and accidental:
                u338,u20e5=u"\u0338",u"\u20e5" # TODO: the \ looks better than the / in default font
                if not type("")==type(u""): u338,u20e5=u338.encode('utf-8'),u20e5.encode('utf-8')
                s += {'#':u338,'b':u20e5}[accidental]
            ret += (r' \note-mod-angka "' if not_angka else r' \note-mod "')+s+'" '
        if self.rplacNextIfStillInBeam and leftBeams and nBeams: replaceLast = self.rplacNextIfStillInBeam # didn't need the rest-hack here after all
        self.rplacNextIfStillInBeam = None
        if placeholder_chord == "r" and use_rest_hack and nBeams and not (leftBeams and not not_angka):
            placeholder_chord = "c"
            # C to work around diagonal-tail problem with
            # some isolated quaver rests in some Lilypond
            # versions (usually at end of bar); new voice
            # so lyrics miss it as if it were a rest:
            if has_lyrics and not self.withStaff: # (OK if self.withStaff: lyrics will be attached to that instead)
                self.rplacNextIfStillInBeam = ret
                ret = jianpu_voice_start(1)[0]+ret
                inRestHack = 1
                if self.inBeamGroup and not self.inBeamGroup=="restHack": aftrlast0 = "] "
    if placeholder_chord.startswith("<"): ret += placeholder_chord  # chord in western or midi
    elif not isChord or figures.startswith("-"): # single note or rest
        if accidental_visible and not not_angka and not (figures.startswith("-") or midi or western): ret += r"\once \tweak Accidental.extra-offset #'(0 . 0.7)"
        ret += placeholder_chord
        if midi or western or not not_angka: ret += {"":"", "#":"is", "b":"es"}[accidental]
        if not placeholder_chord=="r": ret += {"":"'","'":"''","''":"'''","'''":"''''",",":"",",,":",",",,,":",,"}[octave] # so no-mark starts near middle C
        if add_cautionary_accidental: ret += "!"
    ret += ("%d" % length) + dots
    if self.rplacNextIfStillInBeam: self.rplacNextIfStillInBeam += ("r%d" % length) + dots + '['
    if tremolo:
        if midi or western:
            if placeholder_chord.startswith("<") and len(placeholder_chord.split())==4:
                previous,n1,n2,gtLenDot = ret.rsplit(None,3)
                previous=previous[:-1] # drop <
                ret = r"%s\repeat tremolo %d { %s32 %s32 }" % (previous,int(toAdd_preTuplet/4),n1,n2)
            else: ret += tremolo
        elif lilypond_minor_version()>=22:
            if dots: ret += r"""_\tweak outside-staff-priority ##f ^\tweak avoid-slur #'inside _\markup {\with-dimensions #'(0 . 0) #'(2.8 . 2.1) \postscript "1.6 -0.2 moveto 2.6 0.8 lineto 1.8 -0.4 moveto 2.8 0.6 lineto 2.0 -0.6 moveto 3.0 0.4 lineto stroke" } %{ requires Lilypond 2.22+ %} """
            else: ret += r"""_\tweak outside-staff-priority ##f ^\tweak avoid-slur #'inside _\markup {\with-dimensions #'(0 . 0) #'(2.5 . 2.1) \postscript "1.1 0.4 moveto 2.1 1.4 lineto 1.3 0.2 moveto 2.3 1.2 lineto 1.5 0.0 moveto 2.5 1.0 lineto stroke" } %{ requires Lilypond 2.22+ %} """
        elif dots: ret += r"""_\tweak outside-staff-priority ##f ^\tweak avoid-slur #'inside _\markup {\with-dimensions #'(0 . 0) #'(2.8 . 2.6) \postscript "1.4 1.6 moveto 2.4 2.6 lineto 1.6 1.4 moveto 2.6 2.4 lineto 1.8 1.2 moveto 2.8 2.2 lineto stroke" } %{ requires Lilypond 2.20 %} """
        else: ret += r"""_\tweak outside-staff-priority ##f ^\tweak avoid-slur #'inside _\markup {\with-dimensions #'(0 . 0) #'(2.5 . 2.6) \postscript "1.1 1.6 moveto 2.1 2.6 lineto 1.3 1.4 moveto 2.3 2.4 lineto 1.5 1.2 moveto 2.5 2.2 lineto stroke" } %{ requires Lilypond 2.20 %} """
    # Octave dots:
    if not midi and not western and not '-' in figures:
      if not nBeams:
          oDict = {",":r"-\tweak #'Y-offset #-1.2 ",
                   ",,":r"-\tweak #'Y-offset #-2 ",
                   ",,,":r"-\tweak #'Y-offset #-2.7 ",
                }
          ret += oDict.get(octave,"")
      elif self.graceType:
          oDict = {",":r"-\tweak #'Y-offset #%.1f " % (grace_height-1-nBeams*0.3),
                   ",,":r"-\tweak #'Y-offset #%.1f " % (grace_height-1.6-nBeams*0.3),
                   ",,,":r"-\tweak #'Y-offset #%.1f " % (grace_height-2-nBeams*0.3),
                }
          ret += oDict.get(octave,"")
      oDict = dotsDictWithXTweaks(self.graceType)
      if not_angka: oDict.update({
              "'":r"-\tweak #'extra-offset #'(0.4 . 2.7) -\markup{\bold .}",
              "''":r"-\tweak #'extra-offset #'(0.4 . 3.5) -\markup{\bold :}",
              "'''":r"-\tweak #'extra-offset #'(0.4 . 4.3) -\markup{\bold "+three_dots+"}",
      }) # TODO: do we need this in the chords version also
      ret += oDict[octave]
    if nBeams and (not self.inBeamGroup or (self.inBeamGroup=="restHack" and not replaceLast) or inRestHack) and not midi and not western:
        # We need the above stemLeftBeamCount, stemRightBeamCount override logic to work even if we're an isolated quaver, so do this:
        ret += '['
        self.inBeamGroup = 1
    self.barPos += toAdd
    if self.graceType and self.barPos == self.barLength:
        is_isolated_note = ret.endswith("[")
        if is_isolated_note:
            # Lilypond doesn't like isolated beamed notes in \grace
            # so introduce a skip note for it to beam to.
            # Putting the skip note BEFORE the grace note or AFTER the afterGrace note
            # might help if aligning jianpu with 5-line staves.
            if self.graceType == "before":
                ret = r"s%d [ \jianpuGraceCurveEnd %s" % (length,ret.replace("[",""))
            else:
                ret += r" \jianpuGraceCurveEnd s%d" % length
        else:
            ret = r" \jianpuGraceCurveEnd " + ret 
    # sys.stderr.write(accidental+figure+octave+dots+"/"+str(nBeams)+"->"+str(self.barPos)+" ") # if need to see where we are
    if self.pendingArp: ret,self.pendingArp = ret+r"\arpeggio ",0
    if self.barPos > self.barLength: errExit("(notesHad=%s) barcheck fail: note crosses barline at \"%s\" with %d beams (%d skipped from %d to %d, bypassing %d), scoreNo=%d barNo=%d (but the error could be earlier)" % (' '.join(self.notesHad),figures,nBeams,toAdd,self.barPos-toAdd,self.barPos,self.barLength,scoreNo,self.barNo))
    if (self.barPos%self.beatLength == 0 or self.barPos==self.barLength) and self.inBeamGroup: # (or added for irregular time signatures; self.inBeamGroup is set only if not midi/western)
        # jianpu printouts tend to restart beams every beat
        # (but if there are no beams running anyway, it occasionally helps typesetting to keep the logical group running, e.g. to work around bugs involving beaming a dash-and-rest beat in 6/8) (TODO: what if there's a dash-and-rest BAR?  [..]-notated beams don't usually work across barlines
        ret += ']'
        self.inBeamGroup = 0 # DON'T reset lastNBeams here (needed for start-of-group accidental logic)
    elif inRestHack and self.inBeamGroup:
        ret += ']'
        self.inBeamGroup = "restHack"
    self.lastNBeams = nBeams
    beamC = u'\u0333' if nBeams>=2 else u'\u0332' if nBeams==1 else u""
    self.unicode_approx.append({'#':u"\u266f",'b':u"\u266d"}.get(0 if invisTieLast else accidental,u"")+(u"-" if invisTieLast else figures[-1:])+beamC+(u"" if invisTieLast else (u'\u0323' if "," in octave else u'\u0307' if "'" in octave else u""))+u''.join(c+beamC for c in dots)+(u"" if self.inBeamGroup else u" ")) # (NB inBeamGroup is correct only if not midi and not western)
    if self.barPos == self.barLength:
        self.unicode_approx[-1]=self.unicode_approx[-1].rstrip()+u'\u2502'
        self.barPos = 0 ; self.barNo += 1
        self.current_accidentals = {}
    b4last,aftrlast = "",""
    if invisTieLast:
        if midi or western:
            if not (tremolo and placeholder_chord.startswith("<")): aftrlast = " ~"
        elif not tremolo:
            # For attaching lyrics to long notes:
            b4last,aftrlast = r"\once \override Tie #'transparent = ##t \once \override Tie #'staff-position = #0 "," ~"
    if figures=="x" and western: ret = r"\once \override NoteHead.style = #'cross \once \override NoteHead.no-ledgers = ##t " + ret
    if inRestHack: ret += " } " # end temporary voice for the "-" (non)-note
    elif tieEnd: ret += ' '+tieEnd # end of JianpuTie curve
    return aftrLastNonDash,figures=='-',b4last,replaceLast,aftrlast0+aftrlast,ret, accidental_visible, nBeams,octave

def parseNote(word,origWord,line):
    if word==".": word = "-"
    word = word.replace("8","1'").replace("9","2'")
    if type(u"")==type(""): word = word.replace(u"\u2019","'")
    else: word=word.replace(u"\u2019".encode('utf-8'),"'")
    if "///" in word: tremolo,word=":32",word.replace("///","",1)
    else: tremolo = ""
    if not re.match(note_regex+"$",word): # unrecognised stuff in it: flag as error, rather than ignoring and possibly getting a puzzling barsync fail
        scoreError("Unrecognised command",origWord,line)
    figures = ''.join(re.findall('[01234567x-]',word))
    dots = "".join(c for c in word if c==".")
    nBeams = ''.join(re.findall(r'[cqsdh\\]',word))
    if re.match(r"[\\]+$",nBeams): nBeams=len(nBeams) # requested by a user who found British note-length names hard to remember; won't work if the \ is placed at the start, as that'll be a Lilypond command
    elif nBeams:
        try: nBeams = list("cqsdh").index(nBeams)
        except ValueError: scoreError("Can't calculate number of beams from "+nBeams+" in",origWord,line)
    else: nBeams=None # unspecified
    octaves = re.findall("'+|,+",word)
    # chords of course accept multiple octaves
    if len(octaves)>1 and len(figures) == 1: scoreError("Multiple octaves should not applied to a single note:",origWord,line)
    if octaves: octave = octaves[0]
    else: octave = ""
    accidental = "".join(c for c in word if c in "#b")
    if len(figures) > 1: # octave + accidental dealt with separately BUT still need to keep one for the beaming and accidental_visible logic (TODO actually current_accidentals needs rewriting for chords, but this works in most cases for now)
        accidental = accidental[:1]
    return figures,nBeams,dots,octave,accidental,tremolo

def write_docs():
    def htmlify(l):
        if not html: return l # (htmlify is a no-op if html is not set)
        return re.sub('([hdDsS]emi)',r'\1&shy;',l.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")).replace("approximation","approx&shy;imation").replace("instrument=Flute","instrument=<wbr>Flute").replace("automatically","automat&shy;ically").replace("SeparateTimesig","Separate&shy;Timesig") # not sure about that last one because we don't want to hide that it's 1 word, but we do want Chrome on small devices in not-so-small print to fit it on a line rather than zoom out the whole page
    inTable = 0 ; justStarted=1
    for line in __doc__.split("\n"):
        if line.startswith("#") or not line.strip(): continue
        hasNonAscii = any(ord(c)>127 for c in line.split(':')[0])
        if hasNonAscii ^ chinese: continue
        splitOn = "：" if "：" in line else ":"
        if splitOn in line and line.split(splitOn,1)[1].strip():
            toGet,shouldType = line.split(splitOn,1)
            if not inTable:
                if html:
                    print ("<table border>"+("<tr><th>效果</th><th>输入</th></tr>" if chinese else "<tr><th>To get:</th><th>Type:</th></tr>"))
                else: print ("") # header not really applicable in Markdown or CLI help as it's not a table
                inTable = 1
            if re.match(r".*[A-Za-z]\)$",shouldType):
                shouldType,note = shouldType.rsplit("(",1)
                note = " ("+note
            elif re.match(r".*）$",shouldType):
                shouldType,note = shouldType.rsplit("（",1)
                note = " （"+note
            else: note = ""
            if html: print ("<tr><td>"+htmlify(toGet.strip())+"</td><td><kbd>"+htmlify(shouldType.strip())+"</kbd>"+htmlify(note)+"</td>")
            elif markdown: print (toGet.strip()+splitOn+" `"+shouldType.strip()+"`"+note+"\n")
            else: print (toGet+splitOn+" "+shouldType+note)
        else:
            if not html and not justStarted: print ("")
            elif inTable: print ("</table>")
            elif not justStarted: print ("<br>")
            inTable=justStarted=0
            print (htmlify(line))
    if inTable:
        if html: print ("</table>")
        elif markdown: pass # there will already be a blank line
        else: print ("")
    print(("**" if markdown else "<p><strong>" if html else "")+("命令行选项：" if chinese else "Command-line options:")+("**\n" if markdown else "</strong></p><dl>" if html else ""))
    for k,v in sorted(list(args.items())): # (explicit list needed on Python 3.0 through 3.5, not needed on 2.x or 3.6+)
        enDoc,zhDoc,actionList = v
        if enDoc:
            h = htmlify(zhDoc if chinese and zhDoc else enDoc)
            if html: print("<dt><kbd>"+htmlify(k)+"</kbd></dt><dd>"+re.sub('(--[A-Za-z][A-Za-z-]*)',r'<kbd>\1</kbd>',h)+"</dd>")
            elif markdown: print("`"+k+"`: "+re.sub('(--[A-Za-z][A-Za-z-]*)',r'`\1`',h)+"\n")
            else: print(k+": "+h)
    if html: print("</dl>")

def dotsDictWithXTweaks(graceType):
    x_offset=0.4 if graceType else 0.6
    return {"":"",
            "'":"^.",
            "''":r"-\tweak #'X-offset #%.1f ^\two-dots " % x_offset,
            "'''":r"-\tweak #'X-offset #%.1f ^\three-dots " % x_offset,
            ",":r"-\tweak #'X-offset #%.1f _. " % x_offset,
            ",,":r"-\tweak #'X-offset #%.1f _\two-dots " % x_offset,
            ",,,":r"-\tweak #'X-offset #%.1f -\tweak #'extra-offset #'(0 . 0.3) _\three-dots " % x_offset}

def getInput0(files):
  inDat = []
  for f in files:
    if f.endswith(".mscz") or f.endswith(".mscx"): # somebody sent us a MuseScore file and forgot to export to MusicXML (not sure how to handle mxl already existing when this happens, so use tempdir)
        o,f=f,tempfile.gettempdir()+os.sep+outName([f],"mxl")
        if system(("musescore4" if (shutil.which('musescore4') if hasattr(shutil,'which') else os.path.exists('/usr/bin/musescore4')) else "musescore3" if (shutil.which('musescore3') if hasattr(shutil,'which') else os.path.exists('/usr/bin/musescore3')) else "musescore")+" -f -o "+quote(f)+" "+quote(o)): errExit("Failed to convert MuseScore file "+o+" to MusicXML "+f)
    if f.endswith(".mxl"):
        import zipfile ; z=zipfile.ZipFile(f)
        for F in z.infolist():
            if not F.filename in ["mimetype","META-INF/","META-INF/container.xml"]:
                b = z.read(F)
                if type("")==type(u""): b=b.decode('utf-8')
                inDat.append(b)
    else:
      try:
        try: inDat.append(open(f,encoding="utf-8").read()) # Python 3: try UTF-8 first
        except: inDat.append(open(f).read()) # Python 2, or Python 3 with locale-default encoding in case it's not UTF-8
      except: errExit("Unable to read file "+f)
  if inDat: return inDat
  if not sys.stdin.isatty():
    return [fix_utf8(sys.stdin,'r').read()]
  # They didn't give us any input.  Try to use a
  # file chooser.  If that fails, just print the
  # help text.
  if os.path.exists('/usr/bin/osascript'):
    f = os.popen("osascript -e $'tell application \"System Events\"\\nactivate\\nset f to choose file\\nend tell\\nPOSIX path of f'").read().rstrip()
    if f:
      try: return [open(f,encoding="utf-8").read()]
      except: return [open(f).read()]
  sys.stdout = sys.stderr
  write_help() ; raise SystemExit

def write_help(): write_version(),write_docs()
def write_version():
  versions = [] # output the biggest (might not be 1st listed)
  for l in __doc__.split("\n"):
      if l.startswith("# v"): versions.append((float(l[len("# v"):l.index(' ',len("# v"))]),l.replace("#","jianpu-ly",1)+"\n"))
  print(max(versions)[1])

def get_input(files):
  inDat = getInput0(files)
  for i in range(len(inDat)):
    if inDat[i].startswith('\xef\xbb\xbf'):
      inDat[i] = inDat[i][3:]
    if inDat[i].startswith(r'\version'): errExit("jianpu-ly does not READ Lilypond code.\nPlease see the instructions.")
    elif inDat[i].startswith("<?xml"):
        inDat[i] = xml2jianpu(inDat[i])
  return " NextScore ".join(inDat)

def xml2jianpu(x):
    from xml.parsers.expat import ParserCreate
    xmlparser = ParserCreate()
    positionsInProgress,partsInProgress = [0],[[]]
    lyricsInProgress = [[({},0)]] # voice -> movement -> (verseDict, noteCount)
    voiceFirstMvt = [0] # 0-based index of the first movement each voice belongs to
    chordsByMvt,movementParts = {},[]
    paddingRestList, paddingRestDict = [], {0:0}
    ret = ["OctavesAfter"]
    partList=[""];time=["4","4"];tempo=["",""]
    class State: # We are moving toward putting state into here.  Some things haven't been moved yet, but please try to put all new items into State instead of adding more mutable lists above, thanks :)
        def __init__(self):
            self.getAndResetNote(first=True)
            self.readData,self.readAttrs = "",{}
            self.position = self.lastDuration = 0
            self.keySig,self.barSig,self.barTied = ['']*7,['']*7,None
            self.note1 = "C"
            self.tsigOffsets, self.initialQuavers = None,F(0)
            self.prevChordOffset, self.prevChordNList = None,None
            self.multirestCount = self.multirestTotal = 0
            self.multirestSkip, self.multirestBuffer = False,""
            self.mvtLastBarNo, self.mvtStartBarIndices, self.mvtBarLockedIndices, self.mvtBreakIndicators, self.breakCount = None,[],None,set(),0
            self.activeWedges,self.stoppedWedges = {},{}
            self.pendingWedgeCmd = ""
            self.lastOurRet = None
            self.lyrics_for_current_note = []
            self.lyric_state,self.in_text,self.text_buffer=None,False,""
            self.inEnding = self.pendingRepeatClose = self.inAlternatives = False
            self.fifths = self.mode = self.software = ""
            self.octaveShifts = {}
            self.harm,self.inFrame = None,False
            self.harmonies,self.harmonyBreaks = [],[]
            self.divisions = 0 ; self.tremoloStart = None
        def looksLikeNewMovement(self): return len(self.mvtBreakIndicators) >= 2 # MusicXML standard = only 1 movement per file, but some programs like MuseScore include multiple movements.  To prevent false positives on this, we watch for at least 2 indicators that a new movement has begun before acting on it.
        def getAndResetNote(self,first=False):
            r = None if first else (self.step,self.octave,self.accidental,self.nType,self.dot,self.extras,self.tie,self.tuplet,self.tupletNormal,self.tState,self.chord,self.grace,self.tremolo,self.tremoloType,self.extrasBefore)
            self.step=self.octave=self.accidental=self.nType=self.dot=self.extras=self.tie=self.tuplet=self.tupletNormal=self.tState=self.chord=self.grace=self.tremolo=self.tremoloType=self.extrasBefore=""
            return r
    state = State()
    def allParts(cmd):
        for n,p in enumerate(partsInProgress):
            if positionsInProgress[n]==max(positionsInProgress): p.append(cmd)
    def insertMovementBreak(idxs):
        for n,p in enumerate(partsInProgress):
            idx = idxs[n] if n < len(idxs) else 0
            if idx is None: idx = 0
            p.insert(idx, 'NextScore OctavesAfter')
            lyricsInProgress[n].append(({},0))
        state.harmonyBreaks.append(state.position)
        state.mvtBreakIndicators = set()
        state.breakCount += 1
    kind2ly={"major":"","minor":"m","diminished":"dim","augmented":"aug","major-seventh":"maj7","minor-seventh":"m7","dominant-seventh":"7","diminished-seventh":"dim7","half-diminished":"m7.5-","major-minor":"m7.maj7","augmented-seventh":"aug7","major-sixth":"6","minor-sixth":"m6","dominant-sixth":"6","dominant-ninth":"9","major-ninth":"maj9","minor-ninth":"m9","dominant-11th":"11","major-11th":"maj11","minor-11th":"m11","dominant-13th":"13","major-13th":"maj13","minor-13th":"m13","suspended-second":"sus2","suspended-fourth":"sus4","power":"5","pedal":""}
    def harmPitchName(step,alter):
        try: a = int(float(alter))
        except (ValueError,TypeError): a = 0
        if not alter in ("",None) and not a==float(alter): sys.stderr.write("Warning: ignoring microtonal chord alter "+str(alter)+"\n")
        return step.lower()+{-2:"eses",-1:"es",0:"",1:"is",2:"isis"}.get(a,"")
    def harm2ly(h): # (root,suffix) for chordmode
        kind = h["kind"]
        if kind=="none": return None
        mods = kind2ly.get(kind,"")
        degs = ""
        for deg in h["degrees"]:
            try: v = int(deg.get("value","0"))
            except ValueError: continue
            if not v or deg.get("type","add")=="subtract": continue # not sure what to do with subtracted tones in Lilypond
            try: a = int(float(deg.get("alter","0") or "0"))
            except ValueError: a = 0
            degs += ".%d%s" % (v,{1:"+",-1:"-"}.get(a,""))
        suffix = (":" if mods or degs else "")+mods+degs
        return harmPitchName(h["root"],h["rootAlter"]),suffix+("/"+harmPitchName(h["bass"],h["bassAlter"]) if h["bass"] else "")
    def xmlDuration(whole): # fraction of a whole note -> Lilypond duration string, used for chord mode
        best = None
        for denom in [1,2,4,8,16,32,64]:
            v,add,dots = F(1,denom),F(1,denom),0
            while dots < 4:
                if v==whole: return str(denom)+"."*dots
                if best is None or abs(v-whole)<best[0]: best = (abs(v-whole),str(denom)+"."*dots)
                add /= 2 ; v += add ; dots += 1
        sys.stderr.write("Warning: approximating chord duration %s as %s\n" % (whole,best[1]))
        return best[1]
    types={"64th":"h","32nd":"d","16th":"s","eighth":"q","quarter":"","half":" -","whole":" - - -"}
    typesDot={"64th":"h.","32nd":"d.","16th":"s.","eighth":"q.","quarter":".","half":" - -","whole":" - - - - -"}
    typesMM={"64th":"64","32nd":"32","16th":"16","eighth":"8","quarter":"4","half":"2","whole":"1"}
    quavers={"64th":F(1,8),"32nd":F(1,4),"16th":F(1,2),"eighth":F(1),"quarter":F(2),"half":F(4),"whole":F(8)}
    durByQuavers = {F(1,8):"h",F(1,4):"d",F(1,2):"s",F(1):"q",F(2):"",F(4):" -",F(8):" - - -",F(3,8):"h.",F(3,4):"d.",F(3,2):"q.",F(3):".",F(6):" - -",F(12):" - - - - -"}
    def twoNoteTremoloDur(nType1,dot1,nType2,dot2):
        q = F(0)
        for nType,dot in [(nType1,dot1),(nType2,dot2)]:
            v = quavers.get(nType)
            if v is None: return None
            q += v + (v/2 if dot else F(0))
        return durByQuavers.get(q)
    def s(name,attrs):
        state.readData,state.readAttrs="",attrs
        # TODO: Piano music in MusicXML uses <staves>2</staves> in <attributes> and <staff>1</staff> or <staff>2</staff> on individual notes, and also <pedal type="start|stop|change"/>.  But do any pianists prefer to read jianpu?
        # TODO: <transpose><diatonic>, <chromatic>, <octave-change> to get the key relationships for concert pitch of transposing instruments.  But do any players of Western transposing instruments prefer to read jianpu?
        if name=="measure":
            state.mvtStartBarIndices = [len(p) for p in partsInProgress]
            oldBarsig = state.barSig
            state.barSig = state.keySig[:]
            if state.barTied is not None: state.barSig[state.barTied]=oldBarsig[state.barTied] # for tie
            # Track measure numbers for movement detection
            if attrs.get("number"):
                try:
                    thisBarNo = int(attrs["number"])
                    if state.mvtLastBarNo is not None and thisBarNo < state.mvtLastBarNo:
                        state.mvtBreakIndicators.add("measure reset")
                    state.mvtLastBarNo = thisBarNo
                except ValueError: pass
        elif name=="lyric": state.lyric_state={"verse":attrs.get("number",attrs.get("name","1")),"texts":[],"syllabic":"single","extend":False,"elisions":[]}
        elif state.lyric_state:
            if name=="text": state.in_text,state.text_buffer = True,""
            elif name=="elision" and state.lyric_state["elisions"]: state.lyric_state["elisions"][-1] = True
            elif name=="extend": state.lyric_state["extend"]=(attrs.get("type")!="stop")
        elif name=="part": state.harmonies,state.harmonyBreaks = [],[]
        elif name=="harmony": state.harm={"root":None,"rootAlter":"","kind":"major","bass":None,"bassAlter":"","degrees":[],"print":not attrs.get("print-object")=="no"}
        elif name=="degree" and state.harm: state.harm["degree"]={}
        elif name=="frame": state.inFrame=True
        elif name=="score-timewise": errExit("Import of MusicXML 'timewise' format not yet supported, please re-export using 'partwise'") # this is rare as pretty much everybody exports to partwise by default?
    def c(data):
        state.readData += data
        if state.in_text: state.text_buffer += data
    mxl2artic={"strong-accent":"accent","up-bow":"upbow","down-bow":"downbow","trill-mark":"trill","inverted-mordent":"prall","harmonic":"flageolet","snap-pizzicato":"snappizzicato","breath-mark":"breathe","inverted-turn":"reverseturn","detached-legato":"portato","stress":"marcato"}
    mxlDyn = "ppppp pppp ppp pp p mp mf f ff fff ffff fffff fp sf sfp sfpp sff sfz sffz rfz rf fz".split() # and n, but I'm not sure how to do that in Lilypond so ignoring for now
    for n in mxlDyn + "mordent accent tenuto turn marcato staccatissimo fermata staccato stopped open caesura scoop plop doit falloff".split(): mxl2artic[n]=n # Lilypond command is identical to MusicXML element name
    mxl2all={"da-capo":"DC", "dal-segno":"DS", "segno":"Segno", "coda":"ToCoda", "fine":"Fine"} # TODO: is <coda> sometimes the actual coda not ToCoda?  (hopefully rare)
    def e(name):
        d0 = state.readData.strip()
        if name in ['work-title','movement-title'] or name=='credit-words' and state.readAttrs.get("justify")=="center":
            if name=='movement-title': state.mvtBreakIndicators.add("movement title")
            if not any(r.startswith("title=") for r in ret): ret.append('title='+d0.replace("\n"," "))
        elif (name=="creator" and state.readAttrs.get("type")=="composer" or name=='credit-words' and state.readAttrs.get("justify")=="right") and not any(r.startswith("composer=") for r in ret): ret.append("composer="+d0.replace("\n"," "))
        elif name=="creator" and state.readAttrs.get("type")=="arranger" and not any(r.startswith("arranger=") for r in ret): ret.append("arranger="+d0.replace("\n"," "))
        elif name=="creator" and state.readAttrs.get("type")=="lyricist" and not any(r.startswith("poet=") for r in ret): ret.append("poet="+d0.replace("\n"," "))
        elif name=="opus" and not(any(r.startswith("opus=") for r in ret)): ret.append("opus="+d0.replace("\n"," "))
        elif name=="rights" and not(any(r.startswith("copyright=") for r in ret)): ret.append("copyright="+d0.replace("\n"," "))
        elif name in ["part-name","part-name-display","instrument-name"]: partList[-1]=d0
        elif name=="score-part": partList.append("")
        elif name=="part": # we're assuming score-partwise
            if state.inAlternatives:
                allParts("}") ; state.inAlternatives=False
            instName = partList[0] if partList else None
            for n,p in enumerate(partsInProgress):
                if positionsInProgress[n] < max(positionsInProgress) and positionsInProgress[n] in paddingRestDict: p.append(' '.join(paddingRestList[paddingRestDict[positionsInProgress[n]]:]))
                else: os.environ["j2ly_sloppy_bars"] = "1"
            # Movement detection puts NextScore markers inside the voices, but
            # NextScore must be the OUTER level (NextPart within each score),
            # so split each voice at its markers and regroup movement-major:
            segsByVoice = []
            for p in partsInProgress:
                segs = [[]]
                for tok in p:
                    if tok=='NextScore OctavesAfter': segs.append([])
                    else: segs[-1].append(tok)
                segsByVoice.append(segs)
            lyricsByVoice=lyricsInProgress[:]
            need = max(m0+len(segs) for m0,segs in zip(voiceFirstMvt,segsByVoice))
            while len(movementParts) < need: movementParts.append([])
            segChords = [""]*(len(segsByVoice[0]) if segsByVoice else 1)
            if state.harmonies and not state.divisions: sys.stderr.write("Warning: no <divisions> found, so dropping chord symbols\n")
            elif state.harmonies:
                starts,ends = [0]+state.harmonyBreaks,state.harmonyBreaks+[state.position]
                for segNo in range(len(segChords)):
                    startPos,endPos = starts[segNo] if segNo < len(starts) else state.position, ends[segNo] if segNo < len(ends) else state.position
                    items = [h for h in state.harmonies if h[0]<endPos]
                    toks = []
                    if items and items[0][0] > startPos: toks.append("s"+xmlDuration(F(items[0][0]-startPos,state.divisions*4))) # silent gap before first chord
                    for i,(pos,root,suffix) in enumerate(items):
                        nxt = items[i+1][0] if i+1 < len(items) else endPos
                        if nxt > pos: toks.append(root+xmlDuration(F(nxt-pos,state.divisions*4))+suffix)
                        else: sys.stderr.write("Warning: dropping 0-length chord symbol "+root+suffix+"\n")
                    segChords[segNo] = " ".join(toks)
            for n, (m0,segs) in enumerate(zip(voiceFirstMvt,segsByVoice)):
                for segNo,seg in enumerate(segs):
                    if any(t.strip() for t in seg):
                        mvt_lyrics_dict,_=lyricsByVoice[n][m0+segNo]
                        lyricLines = []
                        for idx,v in enumerate(sorted(mvt_lyrics_dict.keys())):
                            syls = mvt_lyrics_dict[v]["syllables"]
                            while syls and not syls[-1]: syls.pop()
                            line,verseNo = " ".join(s if s else '""' for s in syls),re.search(r'\d+',v)
                            if line: lyricLines.append("L: "+(verseNo.group()+". " if verseNo else str(idx+1)+". " if len(mvt_lyrics_dict)>1 else "")+line)
                        movementParts[m0+segNo].append((instName," ".join(seg)+("\n"+"\n".join(lyricLines) if lyricLines else "")))
                        if segNo < len(segChords) and segChords[segNo]:
                            if chordsByMvt.get(m0+segNo): sys.stderr.write("Warning: more than one part has chord symbols in the same movement; keeping only the first\n")
                            else: chordsByMvt[m0+segNo]=segChords[segNo]
            del partsInProgress[:] ; del positionsInProgress[:]
            positionsInProgress.append(0);partsInProgress.append([])
            del lyricsInProgress[:];lyricsInProgress.append([({},0)])
            del voiceFirstMvt[:] ; voiceFirstMvt.append(0)
            state.position=state.lastDuration=0 ; del paddingRestList[:]
            for k in list(paddingRestDict.keys()):
                del paddingRestDict[k]
            paddingRestDict[0] = 0
            state.mvtLastBarNo = None ; state.mvtBreakIndicators = set()
            state.mvtBarLockedIndices = None ; state.breakCount = 0
            state.prevChordOffset = None ; state.prevChordNList = None
            if partList: del partList[0]
            state.octaveShifts,state.stoppedWedges = {},{}
            state.inAlternatives = state.inEnding = False
        elif name=="fifths": state.fifths=d0
        elif name=="mode": state.mode=d0
        elif name=="key" and state.fifths:
            if state.fifths.startswith('-'): keyAcc,start,inc='b',4-1,4 # Bb (b)4
            else: keyAcc,start,inc='#',7-1,3 # F# (#)7
            for i in range(abs(int(state.fifths))):
                state.keySig[start] = keyAcc
                start = (start+inc) % 7
            state.barSig = state.keySig[:]
            key = ["Gb","Db","Ab","Eb","Bb","F","C","G","D","A","E","B","F#","C#","G#","D#"][int(state.fifths)+(9 if state.mode=="minor" else 6)]
            state.note1=key[0]
            paddingRestList.append(("6=" if state.mode=="minor" else "1=")+key)
            for k,v in list(paddingRestDict.items()):
                if v==len(paddingRestList)-1: paddingRestDict[k] += 1
            allParts(("6=" if state.mode=="minor" else "1=")+key)
            state.fifths=state.mode=""
        elif name=="beats": time[0]=d0
        elif name=="beat-type": time[1]=d0
        elif name=="time":
            state.tsigOffsets = [len(paddingRestList)] # so anacrusis logic can come back and add to this
            state.initialQuavers = F(0) # count quavers in 1st bar
            paddingRestList.append("/".join(time))
            for k,v in list(paddingRestDict.items()):
                if v==len(paddingRestList)-1: paddingRestDict[k] += 1
            for n,p in enumerate(partsInProgress):
                if positionsInProgress[n]==max(positionsInProgress):
                    state.tsigOffsets.append(len(p))
                    p.append("/".join(time))
                else: state.tsigOffsets.append(None) # and hope anacrusis is fixed in paddingRestList before time signature gets copied to this part (TODO in theory this might not happen with all MusicXML generators)
        elif name=="divisions":
            try: state.divisions = int(d0)
            except ValueError: pass
        elif name=="duration": state.lastDuration = int(state.readData.strip()) # last duration (could be inside note or backup,forward: handle when close)
        elif name=="backup":
            state.position -= state.lastDuration
            state.lastDuration = 0
        elif name=="forward":
            state.position += state.lastDuration
            state.lastDuration = 0
        elif name == "measure" and not state.tsigOffsets == None:
            expected = F(int(time[0])*8,int(time[1]))
            if state.initialQuavers != expected and state.initialQuavers > 0:
                a={F(1,2):"16",F(3,4):"16.",F(1):"8",F(3,2):"8.",F(2):"4",F(3):"4.",F(4):"2",F(6):"2.",F(8):"1",F(12):"1."}.get(state.initialQuavers)
                if a is None: sys.stderr.write("Warning: cannot determine anacrusis from %s quavers; ignoring pickup\n" % state.initialQuavers)
                else:
                  a=","+a;paddingRestList[state.tsigOffsets[0]]+=a
                  for n,p in enumerate(state.tsigOffsets[1:]):
                    if not p is None: partsInProgress[n][p]+=a
            state.tsigOffsets=None
        # Handle multibar rest from <multiple-rest> element (always on measure close) (contributed by Eagle Wu)
        if name=="measure" and state.multirestCount > 0:
            state.multirestCount -= 1
            # If signals are active during multirest, lock the insertion point to first measure
            if state.looksLikeNewMovement() and state.mvtBarLockedIndices is None:
                state.mvtBarLockedIndices = state.mvtStartBarIndices[:]
            if state.multirestCount == 0:
                # Check for movement boundary before outputting multibar rest
                if state.looksLikeNewMovement():
                    insertMovementBreak(state.mvtBarLockedIndices if state.mvtBarLockedIndices is not None else state.mvtStartBarIndices)
                    state.mvtBarLockedIndices = None
                for p in partsInProgress: p.append('R*' + str(state.multirestTotal))
                if state.multirestBuffer:
                    partsInProgress[0].append(state.multirestBuffer.strip())
                    state.multirestBuffer = ""
                state.multirestSkip = False
                state.multirestCount = state.multirestTotal = 0
        elif name=="measure" and state.multirestCount == 0:
            if len(state.mvtBreakIndicators) >= 2:
                insertMovementBreak(state.mvtStartBarIndices)
            partsInProgress[0].append("\n")
        elif name=="beat-unit": tempo[0]=typesMM.get(d0,"4")
        elif name=="beat-unit-dot" and tempo[0]: tempo[0]+="."
        elif name=="beat-minute" or name=="per-minute": tempo[1]=d0
        elif name=="metronome":
            if tempo[0] and tempo[1]: allParts("=".join(tempo)) # for now we ignore <metronome> elements that don't specify all parameters; we also ignore <sound tempo="120"/> as it's often some default that isn't the composer's actual intention (plus it's always crotchet= even in 6/8 etc); user can edit it back in if needed
            tempo[0]=tempo[1]=""
        elif name=="step": state.step=d0
        elif name=="multiple-rest":
            text = state.readData.strip() if state.readData else ""
            if text:
                try:
                    state.multirestCount = state.multirestTotal = int(text)
                    state.multirestSkip = True
                except ValueError: pass
        elif name=="rest": state.step="r"
        elif name=="unpitched": state.step="x"
        elif name=="octave": state.octave=int(d0)
        elif name=="accidental": state.accidental=d0
        elif name=="type": state.nType=d0
        elif name=="dot": state.dot=True
        elif name=="slur": state.extras+={"start":" (","continue":" ) (","stop":" )"}[state.readAttrs.get("type")]
        elif name in ["tie","tied"]: state.tie={"start":"~","continue":"~"}.get(state.readAttrs.get("type"),"")
        elif name=="actual-notes": state.tuplet=d0
        elif name=="normal-notes": state.tupletNormal=d0
        elif name=="tuplet": state.tState=state.readAttrs.get("type")
        elif name=="chord": state.chord=True
        elif name=="arpeggiate": state.extrasBefore += {"up":"arpUp ","down":"arpDown "}.get(state.readAttrs.get("direction"),"arp ")
        elif name=="arpeggio": state.extrasBefore += "arp " # TODO: do we also want to pick up on "non-arpeggiate" to emit some Lilypond command to say 'play it straight'?  different from 'arp' which is arpeggiate in unspecified direction.  Probably OK to omit though
        elif name in ["slide","glissando"] and state.readAttrs.get("type")=="start": state.extrasBefore += "glis "
        elif name=="tremolo": state.tremolo,state.tremoloType = "///",state.readAttrs.get("type") or "single"
        elif name=="grace": state.grace=True
        elif name=="wedge":
            wtype = state.readAttrs.get("type")
            wnum = state.readAttrs.get("number","1")
            if wtype in ("crescendo","decrescendo","diminuendo"):
                wcmd = r"\<" if wtype=="crescendo" else r"\>"
                state.activeWedges[wnum] = wcmd # Don't need to \! the oldCmd: Lilypond does that automatically
                state.pendingWedgeCmd = wcmd + state.pendingWedgeCmd
            elif wtype=="continue": # MusicXML encoder broke a hairpin over its own system break: we want to take this out as Lilypond will redo (so stop+continue should cancel)
                undone = state.stoppedWedges.pop(wnum,None)
                if undone:
                    lst,idx,cmd = undone
                    if lst[idx].endswith(r" \!"): lst[idx] = lst[idx][:-3]
                    state.activeWedges[wnum] = cmd
            elif wtype=="stop":
                cmd = state.activeWedges.pop(wnum,None)
                if cmd:
                    if state.pendingWedgeCmd or not state.lastOurRet or not any(item and (r"\<" in item or r"\>" in item) for item in state.lastOurRet): # too short
                        state.pendingWedgeCmd = "" ; sys.stderr.write("Ignoring MusicXML hairpin that's too short at bar "+str(state.mvtLastBarNo)+"\n")
                    else:
                        for i in range(len(state.lastOurRet)-1, -1, -1):
                            if state.lastOurRet[i] and not state.lastOurRet[i].startswith("0") and not state.lastOurRet[i].startswith("/"):
                                state.lastOurRet[i] += r" \!"
                                state.stoppedWedges[wnum] = (state.lastOurRet,i,cmd) # remember in case a continue follows
                                break
        elif name=="bar-style":
            b = {"light-light":"||","light-heavy":"|.","heavy-light":".|","dashed":"dashed","dotted":":","tick":"'","short":"!"}.get(d0)
            if b: allParts(r'\bar "'+b+'"') # Lilypond will de-duplicate with auto final barline
        elif name=="repeat":
            d = state.readAttrs.get("direction")
            if d=="forward": allParts("R{")
            elif d=="backward": state.pendingRepeatClose=not state.inEnding and not state.inAlternatives
        elif name=="ending":
            etype = state.readAttrs.get("type")
            if etype=="start":
                if not state.inAlternatives:
                    allParts("} A{") ; state.inAlternatives=True
                else: allParts("|")
                state.inEnding=True
            elif etype=="stop": state.inEnding=False
        elif name=="barline" and state.pendingRepeatClose:
            allParts("}") ; state.pendingRepeatClose=False
        elif name=="frame": state.inFrame=False
        elif name=="fingering" and not state.inFrame: state.extras += " Fr="+d0
        elif name=="open-string" and not state.inFrame: state.extras += " Fr=0"
        elif name in ["string","pluck"] and not state.inFrame: state.extras += ' ^"'+d0+'"'
        elif name=="root-step" and state.harm: state.harm["root"]=d0
        elif name=="root-alter" and state.harm: state.harm["rootAlter"]=d0
        elif name=="kind" and state.harm: state.harm["kind"]=d0
        elif name=="bass-step" and state.harm: state.harm["bass"]=d0
        elif name=="bass-alter" and state.harm: state.harm["bassAlter"]=d0
        elif name in ("degree-value","degree-alter","degree-type") and state.harm: state.harm.setdefault("degree",{})[{"degree-value":"value","degree-alter":"alter","degree-type":"type"}[name]]=d0
        elif name=="degree" and state.harm: state.harm["degrees"].append(state.harm.pop("degree",{}))
        elif name=="harmony":
            h,state.harm = state.harm,None
            if h and h["print"] and h["root"]:
                r2 = harm2ly(h)
                if r2:
                    if state.harmonies and state.harmonies[-1][0]==state.position: state.harmonies[-1]=(state.position,)+r2
                    else: state.harmonies.append((state.position,)+r2)
            elif h and h["print"]: sys.stderr.write("Warning: ignoring chord symbol without root (function-based harmony?)\n")
        elif name=="wavy-line": state.extras += {'start':r' \startTrillSpan','stop':r' \stopTrillSpan'}.get(state.readAttrs.get("type")," tilde")
        elif name=="bend": state.extras += " bend"
        elif name in mxl2all: allParts(mxl2all[name])
        elif name=="sound": # earlier versions of some mxl2all
            a = state.readAttrs
            if a.get("dacapo")=="yes": allParts("DC")
            elif a.get("dalsegno") not in (None,"","no"): allParts("DS")
            if a.get("fine")=="yes": allParts("Fine")
            if a.get("tocoda")=="yes": allParts("ToCoda")
        elif name in mxl2artic:
            if name in mxlDyn: state.activeWedges.clear() # dynamics: don't need \! as Lilypond does it automatically
            state.extras += " \\"+mxl2artic[name]
        elif name=="print":
            if state.readAttrs.get("new-page") == "yes" or state.readAttrs.get("new-system") == "yes":
                state.mvtBreakIndicators.add("page break")
        elif name=="text" and state.in_text:
            state.in_text=False
            if state.lyric_state:
                state.lyric_state["texts"].append(state.text_buffer.strip()) ; state.lyric_state["elisions"].append(False)
        elif name=="syllabic" and state.lyric_state: state.lyric_state["syllabic"]=state.readData.strip()
        elif name=="lyric" and state.lyric_state:
            elisions = state.lyric_state["elisions"]
            state.lyrics_for_current_note.append({
                "verse": state.lyric_state["verse"],
                "text": ''.join(re.sub(r'[-_]+$','',t)+("_" if i<len(elisions) and elisions[i] else "") for i,t in enumerate(state.lyric_state["texts"])),
                "syllabic": state.lyric_state["syllabic"],
                "extend": state.lyric_state["extend"]})
            state.lyric_state=None
        elif name in ["words","other-dynamics"]:
            if name=="words" and state.readAttrs.get("valign")=="top": state.mvtBreakIndicators.add("valign-top words")
            toAdd = ' '+{"bottom":'_'}.get(state.readAttrs.get("valign","bottom" if name=="other-dynamics" or float(state.readAttrs.get("default-y","0"))<0 else 0),'^')+'"'+state.readData.strip().replace('"',"'")+'"'
            if state.multirestSkip: state.multirestBuffer += toAdd
            elif not toAdd in state.extras: state.extras += toAdd
        elif name=="rehearsal" and d0:
            paddingRestList.append("letter" + d0)
            for k,v in list(paddingRestDict.items()):
                if v==len(paddingRestList)-1: paddingRestDict[k] += 1
            allParts("letter" + d0)
# in e(name)
        elif name=="software": state.software = d0
        elif name=="octave-shift":
            mode = xml_octaveShift_override or ("written" if "musescore" in state.software.lower() else "sounding") # TODO: can we ask people to send test 8vas exported from Sibelius, Finale, XunScore, Ziipoo, Xihang and the others to see how they've done it?  W3C spec wording is unclear: my pedantic read suggests "sounding" is correct but it's understandable if some developers read it the other way
            if mode=="sounding": return # pitch data already sounds as the dots will indicate (feel free to add \ottava to the Western staff but this is best redone manually anyway: different typesetters and musicians of different abilities may need it in different places for best reading)
            if state.readAttrs.get("type")=="continue": return
            try: octs=int((int(state.readAttrs.get("size","8"))-1)/7)
            except ValueError: octs=1
            state.octaveShifts[state.readAttrs.get("number","1")]=octs*{"up":-1,"down":1}.get(state.readAttrs.get("type"),0) # MusicXML "up" is actually 8vb: it's not "play this an octave up", it's "this *has been* shifted up for printing".  I would have argued against that if I'd been on the committee but it's done now :)
        elif name=="note":
            if state.inAlternatives and not state.inEnding:
                allParts("}") ; state.inAlternatives=False
            # Try to find which voice it goes onto, if we're MuseScore
            # or similar and have parts as voices within a part.
            # TODO: sometimes the XML will give us a voice or staff number; for now we just find the first one to fit
            ourRet = ourI = None
            for i,p in enumerate(positionsInProgress):
                if p == state.position: # exact match
                    ourRet,ourI = partsInProgress[i],i ; break
            if ourRet is None:
                for i,p in enumerate(positionsInProgress):
                    if p < state.position and p in paddingRestDict: # match but need padding
                        ourRet,ourI = partsInProgress[i],i
                        if not state.multirestSkip: ourRet.append(' '.join(paddingRestList[paddingRestDict[p]:paddingRestDict[state.position]])) # TODO: collapse to whole-bar rests when needed (low priority because this should not happen often)
                        positionsInProgress[i] = state.position
                        break
            if ourRet is None: # need new part
                partsInProgress.append(paddingRestList[:paddingRestDict[state.position]])
                positionsInProgress.append(state.position)
                voiceFirstMvt.append(state.breakCount)
                state.mvtStartBarIndices.append(None)
                ourRet,ourI = partsInProgress[-1],len(partsInProgress)-1
            state.lastOurRet = ourRet
            # Now OK to add the note to the part (voice)
            step,octave,acc,nType,dot,extras,tie,tuplet,tupletNormal,tState,chord,grace,tremolo,tremoloType,extrasBefore = state.getAndResetNote()
            if state.multirestSkip: # just clean up
                state.position+=state.lastDuration
                positionsInProgress[ourI]=state.position
                state.lastDuration=0
                if ourI==0: paddingRestDict[state.position]=len(paddingRestList)
                state.lyrics_for_current_note=[] ; return
            if not chord and not grace and not (tremoloType=="stop" and state.tremoloStart): # handle lyrics
                voiceLlist=lyricsInProgress[ourI]
                voiceLdict,count=voiceLlist[-1]
                present_verses=set()
                for lyc in state.lyrics_for_current_note:
                    v=lyc["verse"] ; present_verses.add(v)
                    if v not in voiceLdict: voiceLdict[v]={"syllables":[""]*count,"melisma": False}
                for lyc in state.lyrics_for_current_note:
                    v,text,syllabic = lyc["verse"],lyc["text"],lyc["syllabic"]
                    voiceLdict[v]["syllables"].append(text+("-" if syllabic in ("begin", "middle") else ""))
                    if lyc["extend"]: voiceLdict[v]["melisma"]=True
                    elif syllabic in ("end","single"): voiceLdict[v]["melisma"]=False
                for v, data in voiceLdict.items():
                    if v not in present_verses: data["syllables"].append("_" if data["melisma"] else "")
                voiceLlist[-1]=(voiceLdict,count+1)
            state.lyrics_for_current_note=[]
            if step=="r": r="0"
            elif step=="x": r="x"
            else:
                dTone=ord(step[0])-ord(state.note1)+7*(sum(state.octaveShifts.values())+octave-4)
                if step[0] < 'C': dTone += 7
                r=str((dTone%7)+1)
                while dTone<0: # we use OctavesAfter
                    r+="," ; dTone+=7
                while dTone>6:
                    r+="'" ; dTone-=7
                acc=state.barSig[dTone%7]={"flat":"b","sharp":"#","natural":""}.get(acc,state.barSig[dTone%7])
                state.barTied=(dTone%7) if tie else None
                if state.keySig[dTone%7]=="#": acc="" if acc=="#" else "b"
                if state.keySig[dTone%7]=="b": acc="" if acc=="b" else "#"
            if chord and grace:
                i=ourRet[-1].rindex("]")
                ourRet[-1]=ourRet[-1][:i]+"&"+r+ourRet[-1][i:]
                return
            elif chord:
                extrasBefore,rr = state.prevChordNList[state.prevChordOffset].split(chr(0))
                chord,dashes = rr.split(" ",1)
                if dashes: dashes=" "+dashes.rstrip()
                state.prevChordNList[state.prevChordOffset] = extrasBefore+chr(0)+(tremolo if not tremolo in rr else '')+chord+r+dashes
                return
            if tremoloType=="stop" and state.tremoloStart and not grace: # combine 2-note tremolo
                sNList,sOff,sExtrasBefore,sW1,sExtras,sTie,sNType,sDot = state.tremoloStart
                combinedDur = twoNoteTremoloDur(sNType,sDot,nType,dot)
                state.tremoloStart = None
                if combinedDur is None: sys.stderr.write("Warning: failed to calculate combined duration of two-note tremolo at bar "+str(state.mvtLastBarNo)+"; leaving it as two single-note tremolos\n")
                else: # this gets tedious (can we refactor to reuse code elsewhere?  low priority as working for now)
                    if state.pendingWedgeCmd:
                        extras = extras+' '+state.pendingWedgeCmd
                        state.pendingWedgeCmd = ""
                    sNList[sOff] = sExtrasBefore+chr(0)+sW1+r+acc+sExtras+extras+' '+combinedDur+' '+sTie
                    if not state.tsigOffsets==None and ourI==0:
                        add = quavers[nType]
                        if dot: add += add/2
                        if tuplet: add *= F(int(tupletNormal) if tupletNormal else 2,int(tuplet))
                        state.initialQuavers += add
                    if ourI==0: paddingRestList.append("0"+(typesDot if dot else types)[nType])
                    state.position += state.lastDuration
                    positionsInProgress[ourI] = state.position
                    state.lastDuration = 0
                    if ourI==0: paddingRestDict[state.position] = len(paddingRestList)
                    return
            if tState=="start":
                ourRet.append(tuplet+"[")
                if ourI==0: paddingRestList.append(tuplet+"[")
            if not nType: # full-bar rest
                assert (r,acc) == ("0","") and not tie, 'MusicXML standard at W3C does not allow measure="yes" for notes, you have found a non-standard file'
                wantQ = F(int(time[0])*8,int(time[1]))
                nn = [k for k,v in quavers.items() if v==wantQ]
                if not nn: nn,dot = [k for k,v in quavers.items() if v*F(3,2)==wantQ],True
                if nn: nType = nn[0]
                else: # need to split rests, so handle this separately
                    nList = []
                    if not state.tsigOffsets==None and ourI==0: state.initialQuavers += wantQ # shouldn't be needed
                    while wantQ:
                        nn=[k for k,v in quavers.items() if v <= wantQ][-1]
                        wantQ -= quavers[nn]
                        nList.append(r+types[nn]+' ')
                        if ourI==0: paddingRestList.append(r+types[nn])
                    ourRet.append(''.join(nList)+' ')
                    state.position += state.lastDuration
                    positionsInProgress[ourI] = state.position
                    state.lastDuration = 0
                    if ourI==0: paddingRestDict[state.position] = len(paddingRestList)
                    return
            if not grace and not state.tsigOffsets==None and ourI==0: # we're counting the length of the first bar, for anacrusis
                add = quavers[nType]
                if dot: add += add/2
                if tuplet: add *= F(int(tupletNormal) if tupletNormal else 2,int(tuplet))
                state.initialQuavers += add
            if dot: d=typesDot
            else: d = types
            r += acc+('///' if tremolo else '')+d[nType]+' ' # typesDot or types, may add " -"s
            if ourI==0: paddingRestList.append("0"+d[nType]) # we hope the subsequent voices are not cross-rhythm with the first voice, at least not at points where <backup> and <forward> occur
            state.prevChordOffset,state.prevChordNList=len(ourRet),ourRet
            w1,w2 = r.split(' ',1)
            if grace: w1="g["+w1+"]"
            if state.pendingWedgeCmd:
                extras = extras+' '+state.pendingWedgeCmd
                state.pendingWedgeCmd = ""
            ourRet.append(extrasBefore+chr(0)+w1+extras+' '+w2+' '+tie)
            state.tremoloStart = (ourRet,len(ourRet)-1,extrasBefore,w1,extras,tie,nType,dot) if tremoloType=="start" and not grace else None
            if tState=="stop":
                ourRet.append("]")
                if ourI==0: paddingRestList.append("]")
            state.position += state.lastDuration
            positionsInProgress[ourI] = state.position
            state.lastDuration = 0
            if ourI==0:
                paddingRestDict[state.position] = len(paddingRestList)
    xmlparser.StartElementHandler = s
    xmlparser.CharacterDataHandler = c
    xmlparser.EndElementHandler = e
    xmlparser.Parse(x,True)
    movementParts = [mv for mv in movementParts if mv]
    for m,mv in enumerate(movementParts):
        for i,(instName,txt) in enumerate(mv):
            if instName: ret.append('instrument='+instName)
            if i==0 and chordsByMvt.get(m): txt += "\nchords=" + chordsByMvt[m]
            ret.append(txt)
            ret.append("WithStaff"+(" NextPart" if i<len(mv)-1 else ""))
        if m < len(movementParts)-1: ret.append("NextScore OctavesAfter")
    ret = '\n'.join(ret).replace(chr(0),"").replace(" DC DC"," DC").replace(" DS DS"," DS").replace(" Fine Fine"," Fine").replace(" ToCoda ToCoda"," ToCoda") # de-duplicating in case some MusicXML 4 exporter sets both <sound> attributes *and* mxl2all elements
    if not type("")==type(ret): ret=ret.encode('utf-8') # Python 2
    return ret

def fix_utf8(stream,mode):
    if type("")==type(u""): # Python 3: please use UTF-8 for Lilypond, even if the system locale says something else
        import codecs
        if mode=='r': return codecs.getreader("utf-8")(stream.buffer)
        else: return codecs.getwriter("utf-8")(stream.buffer)
    else: return stream

def fix_fullwidth(t):
    if type(u"")==type(""): utext = t
    else: utext = t.decode('utf-8')
    r = []
    for c in utext:
        if 0xff01<=ord(c)<=0xff5e: r.append(unichr(ord(c)-0xfee0))
        elif c==u'\u201a': r.append(",") # sometimes used as comma (incorrectly)
        elif c==u'\uff61': r.append(".")
        else: r.append(c)
    utext = u"".join(r)
    if type(u"")==type(""): return utext
    else: return utext.encode('utf-8')

def graceNotes_markup(notes,word,line,isAfter,harmonic=False):
    if lilypond_minor_version()<22: errExit("grace notes requires Lilypond 2.22+, we found 2."+str(lilypond_minor_version()))
    thinspace = u'\u2009'
    if not type("")==type(u""): thinspace = thinspace.encode('utf-8')
    notes = grace_octave_fix(notes,word,line) # ensures octaves come before notes
    notemark = NoteheadMarkup(graceType="after" if isAfter else "before")
    # Calculate length of grace section and tell
    # NoteheadMarkup that's the "bar length", so it
    # ends the beams at the end of it for us
    notemark.barPos = notemark.barLength = 0 ; ignoreNext = False
    curLen = 4 # default semiquaver, in 64th notes
    for n in notes:
        curLen = {'q':8,'s':4,'d':2,'h':1}.get(n,curLen)
        if '0'<=n<='9': 
            if not ignoreNext: notemark.barLength += curLen
            curLen = 4 # reset after each note
            ignoreNext = False
        elif n=='&': ignoreNext = True
    notemark.beatLength = notemark.barLength
    accidental = ""
    beams = maxBeams = 2 # default semiquaver
    figure = ""
    octave = ""
    mr = [] ; i = 0
    if isAfter: mr.append(r"\once \override Score.JianpuGraceCurve.direction = #LEFT ")
    mr.append(r"\jianpuGraceCurveStart ")
    while i < len(notes):
        chord = re.match("([^1-7&]*[1-7]&)+[^1-7&]*[1-7](?!&)",notes[i:])
        if chord:
            chord=chord.group(0) ; i+=len(chord)
            for n in chord:
                if n in 'qsdh': beams='*qsdh'.index(n)
            maxBeams=max(maxBeams,beams)
            mr.append(notemark("11",beams,"","","","",chord,"")[5])
            if harmonic: mr[-1]+=r" \flageolet "
            continue
        n = notes[i]
        if n=='#':
            accidental = "#"
        elif n=='b': 
            accidental = "b"
        elif n in "'," and i and n==notes[i-1]: pass # handled it already when we saw the first one
        elif n=="'":
            if notes[i:i+3]=="'''": octave = "'''"
            elif notes[i:i+2]=="''": octave = "''"
            else: octave = "'"
        elif n==',':
            if notes[i:i+3]==",,,": octave = ",,,"
            elif notes[i:i+2]==",,": octave = ",,"
            else: octave = ","
        elif n in 'qsdh':
            beams = '*qsdh'.index(n)
            maxBeams=max(maxBeams,beams)
        else:
            # number should be the last char of a note
            figure = n
            mr.append(notemark(figure, beams, "", octave, accidental, "", "", "")[5])
            if harmonic: mr[-1]+=r" \flageolet " # deal with harmonic articulations
            accidental = ""
            beams = 2 # reset after each note
            figure = ""
            octave = ""
        i += 1
    beamPos = grace_height/2-2.15-0.3*maxBeams # needed especially when no below-octave dots
    mr.insert(0,r"\once \override Beam.positions = #'(%.1f . %.1f) \once \override Beam.length-fraction = #0.3 " % (beamPos,beamPos))
    return ''.join(mr)
def grace_octave_fix(notes,word,line):
    """Ensures octaves, durations and accidentals come before the
    main notes, not after.  For octaves we check ambiguous cases
    and insist on OctavesBefore or OctavesAfter being set if so"""
    if '&' in notes: return '&'.join(grace_octave_fix(n,word,line) for n in notes.split('&'))
    def gof_inner(notes): return re.sub(
            "^(.*)([1-9])([^1-9]+)$",
            lambda m:gof_inner(m.group(1))+chr(0)+m.group(3)+m.group(2),notes) # the chr(0) is a temporary marker for below
    n2 = gof_inner(notes)
    def find_ambiguous_octaves(n): return re.sub("^[^',1-9]*[',]+[^',1-9]*[1-9](.*)$",lambda m:find_ambiguous_octaves(m.group(1)),n)
    L=n2.split(chr(0))
    ambiguous_part = find_ambiguous_octaves(L[0])
    if re.search("[,']",ambiguous_part):
        if notehead_markup.octavesPosition=="after":
            ap2=re.sub(r"([1-9][^1-9,']*)([,']+)",r'\2\1',ambiguous_part) # after to before (no recursion needed)
            L[0]=L[0][:len(L[0])-len(ambiguous_part)]+ap2
            n2="".join(L)
        elif not notehead_markup.octavesPosition:
            msg="Ambiguous octave marks (please set OctavesBefore or OctavesAfter if writing them in the middle)"
            if not ambiguous_part==notes: msg += " in the "+ambiguous_part+" part of"
            else: msg += ":"
            scoreError(msg,word,line)
    return n2.replace(chr(0),"").replace("8","'1").replace("9","'2")
def gracenotes_western(notes,word,line):
    # for western and MIDI staffs
    notes = grace_octave_fix(notes,word,line)
    nextAcc = "" ; next8ve = "'"
    r = [] ; i = 0
    duration = 16
    while i < len(notes):
        chord = re.match("([^1-7&]*[1-7]&)+[^1-7&]*[1-7](?!&)",notes[i:])
        if chord:
            chord=chord.group(0) ; i+=len(chord)
            for n in chord: # update durations:
                if n in 'qsdh':
                    duration = {'q':8, 's':16, 'd':32, 'h':64}[n]
            _,_,chord = chordNotes_markup(chord,word,line)
            r.append(chord+str(duration)) ; continue
        n = notes[i]
        if n=='#': nextAcc = "is"
        elif n=='b': nextAcc = "es"
        elif n in "'," and i and n==notes[i-1]: pass
        elif n=="'":
            if notes[i:i+3]=="'''": next8ve = "''''"
            elif notes[i:i+2]=="''": next8ve = "'''"
            else: next8ve = "''"
        elif n==',':
            if notes[i:i+3]==",,,": next8ve = ",,"
            elif notes[i:i+2]==",,": next8ve = ","
            else: next8ve = ""
        elif n in 'qsdh': duration = {'q':8, 's':16, 'd':32, 'h':64}[n]
        else:
            if not n in placeholders:
                i += 1 ; continue # TODO: errExit ?
            r.append(placeholders[n]+nextAcc+next8ve+str(duration))
            nextAcc = "" ; next8ve = "'"
        i += 1
    return ' '.join(r)
def chordNotes_markup(notes,word,line,graceType=None):
    notes = grace_octave_fix(notes,word,line) # ensures octaves and accidentals come before notes
    accidental = ""
    figure = ""
    octave = ""
    sortKey = 0
    dNotes = []
    mr = []
    
    for i in range(len(notes)):
        n = notes[i]
        if n=='#':
            accidental = "#"
            sortKey += 0.5
        elif n=='b': 
            accidental = "b"
            sortKey -= 0.5
        elif n=="'":
            if i and notes[i-1]==notes[i]: continue
            if notes[i:i+3]=="'''": 
                octave = "'''"
                sortKey += 22
            elif notes[i:i+2]=="''": 
                octave = "''"
                sortKey += 15
            else: 
                octave = "'"
                sortKey += 8
        elif n==',':
            if i and notes[i-1]==notes[i]: continue
            if notes[i:i+3]==",,,": 
                octave = ",,,"
                sortKey -= 22
            elif notes[i:i+2]==",,": 
                octave = ",,"
                sortKey -= 15
            else: 
                octave = ","
                sortKey -= 8
        else:
            # number should be the last char of a note
            if n not in '01234567' : continue
            figure = n
            if int(n) == 0: sortKey = 0
            else: sortKey += int(n)
            dNotes.append({
                'sortKey':sortKey,
                'figure':figure,
                'octave':octave,
                'accidental':accidental})
            accidental = ""
            figure = ""
            octave = ""
            sortKey = 0
    if sort_chords: dNotes.sort(key=lambda element:element['sortKey'])
    placeholder_chord= "< "
    for f in dNotes:
        placeholder_chord += placeholders[f['figure']]+{"":"", "#":"is", "b":"es"}[f['accidental']]
        if "," in f['octave']: placeholder_chord += f['octave'][:-1]+" "
        else: placeholder_chord += f['octave']+"' "
    placeholder_chord += ">"

    # skip the bottom octave dots
    # as the they are dealed with markups outside.
    bottom_octave = top_octave = ""
    if "," in dNotes[0]['octave']:
        bottom_octave = dNotes[0]['octave']
        dNotes[0]['octave'] = ""

    # let's put octaves inside chord
    offsets = {"'":1,
        "''":1.6,
        "'''":2.2,
        ",":1,
        ",,":1.6,
        ",,,":2.2}
    oDict = dotsDictWithXTweaks(graceType)
    ret = "< "
    baseline = 0
    for f in dNotes:
        if ',' in f['octave'] and (baseline or not graceType): baseline += offsets[f['octave']]/(2 if graceType else 1) # octaves below raises the baseline, except if the lowest one goes below the beam (non-gracenote chords can also have lowest octave below beam but height gets normalised)
        ret += ((r"\tweak #'Y-offset #%.1f " % baseline) if baseline else "")+(r'\note-mod-angka "' if not_angka else r'\note-mod "')+f['figure']+'" '+placeholders[f['figure']]+{"":"", "#":"is", "b":"es"}[f['accidental']]+(f['octave'][:-1]+" " if "," in f['octave'] else f['octave']+"' ")
        if "," in f['octave']: ret += r"-\tweak #'Y-offset #%.1f " % (baseline -0.1 - (1.2 * offsets[f['octave']])/(1.4 if graceType else 1))
        elif "'" in f['octave']: ret += r"-\tweak #'Y-offset #%.1f " % (baseline + (0.9+grace_height if graceType and baseline<1.5+grace_height else 1 if graceType else 1.6) + 0.02 * offsets[f['octave']])
        ret += oDict[f['octave']]+" "
        baseline += 1.5 if graceType and baseline>=1.5+grace_height else 1.5+grace_height if graceType else 2
        if "'" in f['octave']: baseline += offsets[f['octave']]/(2 if graceType else 1)
    ret += ">"
    return ret,bottom_octave,placeholder_chord

note_regex = (
    # Define a note regex as precisely as we can, as different from Lilypond commands etc.
    # Optionally before the figure:
    r"(?:[.,'cqsdh#b]" + # non-\ note attribute
    r"[.,'cqsdh\\#b]*)?" + # and possibly other note attributes that can include \ (just not as the first character).
    r"[0-9x-]" + # At least one figure (or rest or continuation)
    r"[0-9x.,'cqsdh\\#b-]*") # and other figures or attrs after

def getLY(score,headers=None,have_final_barline=True):
   if not headers: headers = {} # Python 2 persists this dict if it's in the default args
   lyrics = []
   notehead_markup.initOneScore()
   out = [] ; maxBeams = 0
   need_final_barline = False
   repeatStack = [] ; lastPtr = 0
   lastNonDashPtr = 0
   rStartP = None
   escaping = inTranspose = pendingGliss = 0
   aftrnext = None
   aftrnext2 = None ; DS = "}"
   isInHarmonic = False
   LP_between_head_and_first_score = ""
   # Please be careful adding extra re.sub's here: they will apply
   # to the WHOLE SCORE, including Lilypond blocks, headers, etc.
   # See comment below for a place where you can add re.sub's that
   # apply just to the jianpu parts after we've already dealt with
   # Lilypond blocks, headers and lyrics.
   score = re.sub("(?s)(^|\n)(L:|H:|chords=)\n(.*?)(?=\n\n|$)",lambda m:"\n"+" ".join(m.group().split()),score) # this one DOES apply to lyrics etc: if newline immediately after, collapse until next double newline
   for line in score.split("\n"):
    line = fix_fullwidth(line).strip()
    line=re.sub(r"^%%\s*tempo:\s*(\S+)\s*$",r"\1",line) # to provide an upgrade path for jihuan-tian's fork
    if line.startswith("LP:") or line.startswith("LPH:"):
        # Escaped LilyPond block.  Thanks to James Harkins for this suggestion.
        # (Our internal barcheck does not understand code in LP blocks, so keep it to complete bars.)
        escaping = 1+len(out)
        esc,rest = line.split(':',1)
        if rest.strip(): out.append(rest.strip()+"\n") # remainder of current line
    elif line.startswith(":LP") or line.startswith(":LPH"):
        if line.startswith(":LPH") or r"\paper {" in "".join(out[escaping-1:]):
            LP_between_head_and_first_score += "".join(out[escaping-1:])
            del out[escaping-1:]
        escaping = 0
        if re.sub('^:LPH?','',line).strip(): sys.stderr.write("Warning: current implementation ignores anything after :LP or :LPH on same line\n") # TODO
    elif escaping:
        out.append(line+"\n")
    elif not line: pass
    elif line.startswith("L:") or line.startswith("H:"):
        # lyrics
        do_hanzi_spacing = line.startswith("H:")
        line = line[2:].strip()
        toAdd = ""
        if line and '1' <= line[0] <= '9' and (line[1]=='.' or asUnicode(line)[1]==u"\uff0e"):
            # a verse number
            toAdd = r'\set stanza = #"%s." ' % line[:1]
            if line[1]=='.': line=line[2:]
            elif not type(line)==type(u""): line=line[4:] # for utf-8 full-width dot in Python 2
            else: line = line[2:] # for full-width dot in Python 3
            line = line.strip()
        if do_hanzi_spacing: # this is not 100% perfect...
            l2 = [r"\override LyricText #'self-alignment-X = #LEFT "] # for overhanging commas etc to work
            if toAdd:
                l2.append(toAdd) ; toAdd = ""
            needSpace = 0
            for c in list(asUnicode(line)):
                is_hanzi = (0x3400 <= ord(c) < 0xa700) # TODO: also cover those outside the BMP?  but beware narrow Python builds
                is_openquote = c in u"\u2018\u201c\u300A"
                if needSpace and (is_hanzi or is_openquote):
                    l2.append(' ') ; needSpace = 0
                    if is_openquote: # hang left
                        l2.append(r"\once \override LyricText #'self-alignment-X = #CENTER ") # or RIGHT if there's no punctuation after
                if is_hanzi: needSpace=1
                if c=="_": needSpace=0 # TODO: document this: separate hanzi with _ to put more than one on same note
                else: l2.append(c)
            line = u"".join(l2)
            if not type("")==type(u""): line = line.encode('utf-8') # Python 2
        lyrics.append(toAdd+re.sub("(?<=[^- ])- "," -- ",line).replace(" -- "," --\n"))
    elif re.match(r"\s*[A-Za-z]+\s*=",line):
        # Lilypond header (or guitar chords)
        hName,hValue = line.split("=",1)
        hName,hValue = hName.strip().lower(),hValue.strip()
        if not headers.get(hName,hValue)==hValue:
            if hName=='instrument': missing='NextPart or NextScore'
            else: missing='NextScore'
            errExit("Changing header '%s' from '%s' to '%s' (is there a missing %s?)" % (hName,headers[hName],hValue,missing))
        headers[hName] = hValue
    else:
        # If we get HERE, we know we're not in a Lilypond header, a
        # lyrics line, or Lilypond code.  This is a good place to
        # put any regex replacements we want to apply only to the
        # jianpu parts of the input before we split into words.
        # First, merge multiple grace notes.  This is needed for the
        # output of some MusicXML conversions, and might be useful to
        # have around anyway:
        line=re.sub(r"(?<=\s)(g\[[#b',1-9qsdh]+\]\s*)+g\[([#b',1-9qsdh]+)\](?=\s)",lambda m:re.sub(r"\]\s*g\[","",m.group()),line)
        # To support multi-word text above/below the stave, we'll
        # replace space with chr(0) inside quoted strings so they
        # end up being one word per item (we'll put it back to space
        # before giving it to Lilypond)
        line=re.sub('(?<= )[_^]?"[^" ]* [^"]*"(?= |$)',lambda m:m.group().replace(' ',chr(0))," "+line)[1:]
        # and YesGH's suggestion: allow slurs and ties to be attached
        # to the right-hand side of the notes to which they apply
        # (i.e. auto insert the space if there's not one already).
        # Not yet doing this with \ Lilypond commands, because
        # currently \ can indicate a duration if used anywhere other
        # than the first character of a note, so it could be quite
        # tricky to identify exactly when we can definitely say it's
        # a Lilypond command and not a duration (but if your
        # particular input doesn't use \ for duration then you could
        # do the replacement in another tool before jianpu-ly).
        # So currently you still need a space before \command, but
        # don't need a space before ( or ) or ~ after the note
        # (and more than one of these can be added to the same note)
        line=re.sub(r"((?:^|\s)"+note_regex+r")([()~]+)(?=\s|$)", lambda m:" ".join([m.group(1)]+list(m.group(2))), line)
        for word in line.split():
            word=word.replace(chr(0)," ")
            if word in ["souyin","harmonic","up","down","bend","tilde"]: word="Fr="+word # (Fr= before these is optional)
            elif word in [r"\staccato","staccato","-."]: word=r"\staccato" if midi or western else "Fr=staccato" # TODO: document that 'staccato' and '-.' are supported words
            if re.match("[16]=[#b][A-Ga-g]$",word): word=word[:2]+word[3]+word[2] # somebody wrote a key name backwards (bE instead of Eb), we can fix that here
            # -----------------------------------
            # Start of main 'switch' on each word
            # -----------------------------------
            if word.startswith('%'): break # a comment
            elif word == "Harm:":
                isInHarmonic = True
            elif word==":Harm":
                isInHarmonic = False
            elif re.match("[1-468]+[.]*=[1-9][0-9]*$",word): out.append(r'\tempo '+word) # TODO: reduce size a little?
            elif re.match("[16]=[A-Ga-g][#b]?$",word): #key
                # Must use \transpose because \transposition doesn't always work.
                # However, don't use \transpose if printing - it adds extra accidentals to the rhythm staff.
                # So we have to do separate runs of \layout and \midi (hence the outer loop).
                notehead_markup.unicode_approx.append(u''+re.sub('(?<!=)b$',u'\u266d',word.replace('#',u'\u266f')).upper()+u' ')
                if midi or western:
                    if inTranspose: out.append('}')
                    if word[0]=="6": transposeFrom = "a"
                    else: transposeFrom = "c"
                    transposeTo = word[word.index('=')+1:].replace("#","is").replace("b","es").lower()
                    if transposeFrom=="c" and transposeTo[0] in "gab": transposeTo += ','
                    if transposeFrom=="a" and transposeTo[0] in "cd": transposeTo += "'"
                    out.append(r"\transpose "+transposeFrom+" "+transposeTo+r" { \key c \major ") # so that MIDI or Western pitches are correct
                    inTranspose = 1
                else: out.append(r'\mark \markup{%s}' % word.replace("b",r"\flat").replace("#",r"\sharp"))
            elif word.startswith("Fr="):
              finger = word.split("=")[1]
              finger = {
                  "0": u"\u5b80",
                  "1": u"\u4e00", "2": u"\u4e8c",
                  "3": u"\u4e09", "4": u"\u56db",
                  "souyin": u"\u4e45", # jiu3
                  "harmonic": u"\u25cb", # white circle: TODO: can we use Lilypond's ^\flageolet command (not in a \finger{}) which doesn't require a font with 25CB in it? or would that get wrong size? (can be tweaked)
                  "up": u"\u2197", # NE arrow
                  "down": u"\u2198", # SE arrow
                  "bend": u"\u293b", # bottom arc anticlockwise arrow
                  "tilde": u"\u223c", # full-width tilde.  Could also use U+1D008 "Byzantine musical symbol syrmatiki" but that (a) won't display on macOS (as of 12.6) and (b) needs special consideration for old versions of Python 2 on narrow Unicode builds
                  "staccato": u"\u25bc",
                  }.get(finger, finger)
              if not type("")==type(u""): finger = finger.encode('utf-8') # Python 2
              out.append(r'\finger \markup { \fontsize #-4 "%s" } ' % finger)
            elif word=="[(":
                if western or midi: out.append(r'\new Voice="%s" { ' % uniqName())
                else: out.append(jianpu_voice_start(1)[0])
                if not midi: out.append(r'\cadenzaOn \note-mod "(" r8 \cadenzaOff ')
            elif word==")]":
              if not midi:
                if notehead_markup.barPos: out.append(r'\cadenzaOn \note-mod ")" r8 \cadenzaOff ')
                else: out.append(r'\once \omit Score.BarNumber \noBreak \bar "" \cadenzaOn \note-mod ")" r8 \cadenzaOff \bar "|" ') # and if re.search(r"\sNextPart\s"," "+score+" ") then at least some versions of Lilypond may need \cadenzaOn s8 \cadenzaOff at the same place in the other parts (TODO: automate? but check user hasn't already added it in an LP block?)
              out.append("}")
            elif re.match("letter[A-Z0-9]+$",word):
                out.append(r'\mark \markup{ \box { "%s" } }' % word[6:])
            elif re.match(r"R\*[1-9][0-9]*$",word):
                if not western: out.append(r"\set Score.skipBars = ##t \override MultiMeasureRest #'expand-limit = #1 ") # \compressFullBarRests on Lilypond 2.20, \compressEmptyMeasures on 2.22, both map to \set Score.skipBars
                out.append(r"R"+notehead_markup.wholeBarRestLen()+word[1:])
                notehead_markup.barNo += int(word[2:])
            elif re.match("[1-9][0-9]*/[1-468]+(,[1-9][0-9]*[.]?)?$",word): # time signature
                if ',' in word: # anacrusis
                    word,anac = word.split(",",1)
                else: anac=""
                if notehead_markup.separateTimesig and not midi: out.append(r'\mark \markup{'+word+'}') # don't try to add RehearsalMark overrides here: it will go wrong during mark merging; we do that later
                out.append(r'\time '+word)
                num,denom = word.split('/')
                notehead_markup.setTime(int(num),int(denom))
                if anac:
                    if anac.endswith("."): # e.g. 2.
                        a2 = anac[:-1] ; anacDotted = 1
                    else: a2,anacDotted = anac,0
                    notehead_markup.setAnac(int(a2),anacDotted)
                    out.append(r'\partial '+anac)
            elif word == "slideUp": notehead_markup.pendingSlide=u"\u2197"
            elif word == "slideDown": notehead_markup.pendingSlide=u"\u2198"
            elif word.startswith("slide="): notehead_markup.pendingSlide=word.split('=',1)[1]
            elif word in ['arpUp','arpDown','arp']: notehead_markup.pendingArp=word
            elif word in ["glis","gliss"]: pendingGliss=3
            elif word=="OnePage":
                if notehead_markup.onePage: sys.stderr.write("WARNING: Duplicate OnePage, did you miss out a NextScore?\n")
                notehead_markup.onePage=1
            elif word=="RepeatAccidentals":
                notehead_markup.repeatAccidentals=True
                out.append(r'\accidentalStyle neo-modern')
            elif word=="NormalAccidentals":
                notehead_markup.repeatAccidentals=False
                out.append(r'\accidentalStyle default')
            elif word=="KeepOctave": pass # undocumented option removed in 1.7, no effect
            elif word=="KeepLength":
                notehead_markup.keepLength=1
            elif word=="OctavesBefore":
                notehead_markup.octavesPosition="before"
            elif word=="OctavesAfter":
                notehead_markup.octavesPosition="after"
            elif word=="ChordsRoman":
                if notehead_markup.chordsRoman: sys.stderr.write("WARNING: Duplicate ChordsRoman, did you miss out a NextScore?\n")
                notehead_markup.chordsRoman=1
            elif word=="NoBarNums":
                if notehead_markup.noBarNums: sys.stderr.write("WARNING: Duplicate NoBarNums, did you miss out a NextScore?\n")
                notehead_markup.noBarNums=1
            elif word=="NoIndent":
                if notehead_markup.noIndent: sys.stderr.write("WARNING: Duplicate NoIndent, did you miss out a NextScore?\n")
                notehead_markup.noIndent=1
            elif word=="RaggedLast":
                if notehead_markup.raggedLast: sys.stderr.write("WARNING: Duplicate Raggedlast, did you miss out a NextScore?\n")
                notehead_markup.raggedLast=1
            elif word=="SeparateTimesig":
                if notehead_markup.separateTimesig and not midi and not western: sys.stderr.write("WARNING: Duplicate SeparateTimesig, did you miss out a NextScore?\n")
                notehead_markup.separateTimesig=1
            elif word in ["angka","Indonesian"]:
                global not_angka
                if not_angka: sys.stderr.write("WARNING: Duplicate angka, did you miss out a NextScore?\n")
                not_angka = True
            elif word=="WithStaff":
                if notehead_markup.withStaff: sys.stderr.write("WARNING: Duplicate WithStaff, did you miss out a NextScore?\n")
                notehead_markup.withStaff=not force_staff==False
            elif word=="PartMidi": pass # handled in process_input
            elif word=="R{":
                repeatStack.append((1,notehead_markup.barPos,0,len(out)))
                if out: out[-1]=re.sub(r' \\bar "|."$',"",out[-1]) # in case starting after a Fine
                out.append(r'\repeat volta 2 {')
            elif re.match("R[1-9][0-9]*{$",word):
                times = int(word[1:-1])
                repeatStack.append((1,notehead_markup.barPos,times-1,len(out)))
                if out: out[-1]=re.sub(r' \\bar "|."$',"",out[-1])
                out.append(r'\repeat percent %d {' % times)
            elif word=="}":
                numBraces,oldBarPos,extraRepeats,rStartP = repeatStack.pop()
                out.append("}"*numBraces)
                # Re-synchronise so bar check still works if percent is less than a bar:
                newBarPos = notehead_markup.barPos
                while newBarPos < oldBarPos: newBarPos += notehead_markup.barLength
                # newBarPos-oldBarPos now gives the remainder (mod barLength) of the percent section's length
                if numBraces==1: notehead_markup.barPos = (notehead_markup.barPos + (newBarPos-oldBarPos)*extraRepeats) % notehead_markup.barLength
                # TODO: update barNo also (but it's used only for error reports)
            elif word=="A{":
                out[rStartP] = out[rStartP].replace('percent','volta') # for time-bars with 3 or more times, you can say R3{ ... } A{ ... } (TODO document this)
                repeatStack.append((2,notehead_markup.barPos,0,rStartP))
                out.append(r'\alternative { {')
            elif word=="|" and repeatStack and repeatStack[-1][0]==2:
                out.append("} {") # separate repeat alternates (if the repeatStack conditions are not met i.e. we're not in an A block, then we fall through to the undocumented use of | as barline check below)
                numBraces,oldBarPos,extraRepeats,rStartP = repeatStack.pop()
                notehead_markup.barPos = oldBarPos
                repeatStack.append((numBraces,oldBarPos,extraRepeats+1,rStartP))
                out[rStartP] = out[rStartP].replace(('volta %d ' % (extraRepeats+1)),('volta %d ' % (extraRepeats+2))) # ensure there's enough repeats for the alternatives
            elif word.startswith("\\") or word.startswith('^\\') or word.startswith('_\\') or word in ["(",")","~","->","|"] or word.startswith('^"') or word.startswith('_"') or word.startswith('"'):
                # Lilypond command, \p, ^"text", barline check (undocumented, see above), etc
                if re.match(r"\\[.,'cqsdh\\#b]*[0-9x-][0-9x.,'cqsdh\\#b-]*$",word): sys.stderr.write("Warning: '"+word+"' is being interpreted as a Lilypond command.\nIf you meant it as a note, move the \\ away from the start.\n")
                if word=="~" and not midi and not western and lastNonDashPtr < lastPtr: # tie from the number, not the last dash
                    out.insert(lastNonDashPtr+1,r'\=JianpuTie(')
                    lastPtr += 1
                    aftrnext2 = r'\=JianpuTie)'
                elif out and "afterGrace" in out[lastPtr]:
                    # apply to inside afterGrace in midi/western
                    out[lastPtr] = out[lastPtr][:-1] + word + " }"
                elif out and out[-1].startswith(r'\grace'):
                    out[-1] = out[-1][:-1] + word + " }"
                else:
                    out.append(word)
                    if word=="~" and not midi and not western and lastNonDashPtr < lastPtr: sys.stderr.write("Warning: jianpu long-note tie won't be done right because your Lilypond version is older than 2.20\n")
            elif re.match(r"[1-9][0-9]*\[$",word):
                # tuplet start, e.g. 3[
                fitIn = int(word[:-1])
                i=2
                while i<fitIn: i*=2
                if i==fitIn: num=int(fitIn*3/2)
                else: num=int(i/2)
                out.append("\\times %d/%d {" % (num,fitIn))
                notehead_markup.tuplet = (num,fitIn)
            elif word==']': # tuplet end
                out.append("}")
                notehead_markup.tuplet = (1,1)
            elif re.match(r"g\[[#b',1-9qsdh&]+\]$",word):
                if midi or western: out.append(r"\grace { " + gracenotes_western(word[2:-1],word,line) + " }")
                else: out.append(r"\grace { " + graceNotes_markup(word[2:-1],word,line,0,isInHarmonic) + " }")
            elif re.match(r"\[[#b',1-9qsdh&]+\]g$",word):
                if midi or western: out[lastPtr] = r" \afterGrace { " + out[lastPtr] + " } { " + gracenotes_western(word[1:-2],word,line) + " }"
                else: out[lastPtr] = r" \afterGrace { " + out[lastPtr] + " } { " + graceNotes_markup(word[1:-2],word,line,1,isInHarmonic) + " }"
            elif word=="Fine":
                need_final_barline = False
                if lilypond_minor_version()>=24: out.append(r'\volta 2 \fine \volta 1')
                else: out.append(r'''\once \override Score.RehearsalMark #'break-visibility = #begin-of-line-invisible \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT \mark "Fine" \bar "|."''')
            elif word=="DC":
                need_final_barline = False
                if lilypond_minor_version()>=24:
                    out.insert(0,r'\repeat segno 2 {') ; lastPtr += 1
                    out.append(DS)
                else: out.append(r'''\once \override Score.RehearsalMark #'break-visibility = #begin-of-line-invisible \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT \mark "D.C. al Fine" \bar "||"''')
            elif word=="Segno":
                if lilypond_minor_version() < 24: errExit("Need at least Lilypond 24 for Segno")
                out.append(r'\repeat segno 2 {')
            elif word=="DS": out.append(DS)
            elif word=="ToCoda":
                if lilypond_minor_version() < 24: errExit("Need at least Lilypond 24 for coda")
                out.append(r'\alternative { \volta 1 {')
                DS=r'''} } \volta 2 \volta #'() { \section \sectionLabel "Coda" } }'''
            else: # note (or unrecognised)
                word0 = word
                baseOctaveChange = "".join(c for c in word if c in "<>")
                if baseOctaveChange:
                    notehead_markup.baseOctaveChange(baseOctaveChange)
                    word = "".join(c for c in word if not c in "<>")
                    if not word: continue # allow just < and > by itself in a word
                figures,nBeams,dots,octave,accidental,tremolo = parseNote(word,word0,line)
                need_final_barline = True
                aftrLastNonDash,isDash,b4last,replaceLast,aftrlast,this,accidental_visible,nBeams,octave = notehead_markup(figures,nBeams,dots,octave,accidental,tremolo,word0,line)
                if replaceLast: out[lastPtr]=replaceLast
                if b4last: out[lastPtr]=b4last+out[lastPtr]
                if aftrlast and not (isDash and pendingGliss): out.insert(lastPtr+1,aftrlast)
                if aftrLastNonDash: out.insert(lastNonDashPtr+1,aftrLastNonDash)
                if not isDash:
                    if pendingGliss==2:
                        pendingGliss = 1 if out[-1]=="~" else 0
                        out.append(r"\glissando")
                    elif pendingGliss and not (pendingGliss==1 and out[-1]=="~"): pendingGliss -= 1
                    if pendingGliss==1 and out[-1] in ["~",r"\glissando"]: out.append(r"\once \override NoteColumn.glissando-skip = ##t")
                    lastNonDashPtr = len(out)
                elif pendingGliss:
                    if midi or western: cB,cA = "()","$"
                    else: cB,cA = "(note-mod \".\" [a-g][',]*)",""
                    longerNote = re.sub(cB+'4'+cA,r'\g<1>2',re.sub(cB+'2(?![.])'+cA,r'\g<1>2.',re.sub(cB+'2[.]'+cA,r'\g<1>1',out[lastPtr],count=1),count=1),count=1)
                    if out[lastPtr]==longerNote: sys.stderr.write("Warning: failed to elongate this note for glissando, may spoil typesetting: {"+longerNote+"}\n")
                    else: out[lastPtr],this = longerNote,"" # suppress dash, elongate instead
                if this:
                    lastPtr = len(out) ; out.append(this)
                if aftrnext2:
                    out.append(aftrnext2)
                    aftrnext2 = None
                if aftrnext:
                    if accidental_visible: aftrnext = aftrnext.replace(r"\markup",r"\markup \halign #2 ",1)
                    out.append(aftrnext)
                    aftrnext = None
                if not_angka and "'" in octave: maxBeams=max(maxBeams,len(octave)*.8+nBeams)
                else: maxBeams=max(maxBeams,nBeams)
                if isInHarmonic and not midi and not western and not figures=='-': out[-1]+=r" \flageolet "
   if notehead_markup.barPos == 0 and notehead_markup.barNo == 1: errExit("No jianpu in score %d" % scoreNo)
   if notehead_markup.inBeamGroup and not midi and not western and not notehead_markup.inBeamGroup=="restHack": out[lastPtr] += ']' # needed if ending on an incomplete beat
   if inTranspose: out.append("}")
   if repeatStack: errExit("Unterminated repeat in score %d" % scoreNo)
   if escaping: errExit("Unterminated LP: in score %d" % scoreNo)
   notehead_markup.endScore() # perform checks
   if have_final_barline and need_final_barline and not midi: out.append(r'\bar "|."')
   # Merge \mark commands (at least Lilypond 2.20..2.24 can't take more than one in one place)
   i,needLeftAlign = 0, notehead_markup.noIndent and not midi and not western
   while i < len(out)-1:
       if out[i].startswith(r'\mark \markup{'):
         if needLeftAlign: out[i]=r"\once \override Score.RehearsalMark #'self-alignment-X = #LEFT "+out[i]
         j=i+1
         while j<len(out):
          if out[j].startswith(r'\mark \markup{') and out[j].endswith('}'):
           out[i]=out[i][:-1]+' '+out[j][len(r'\mark \markup{'):]
           del out[j]
          elif out[j].startswith(r"\time"): j += 1
          else: break
       elif out[i].startswith("| "): needLeftAlign = False
       i += 1
   # format and combine:
   for i in range(len(out)-1):
       if not out[i].endswith('\n'):
           if '\n' in out[i] or len(out[i])>60:
               out[i] += '\n'
           else: out[i]+=' '
   out = ''.join(out)
   if midi or western: # collapse/combine tied notes into longer notes (even in MIDI, for 2-note tremolo extension)
       for numNotes,dot,result in [
               (4,r"\.","1."), # in 12/8, 4 dotted crotchets = dotted semibreve
               (4,"","1"), # 4 crotchets = semibreve
               (3,"","2."), # 3 crotchets = dotted minim
               (2,r"\.","2."), # in 6/8, 2 dotted crotchets = dotted minim
               (2,"","2")]: # 2 crotchets = minim
           out = re.sub("(?P<note>[^<][^ ]*|<[^>]*>)4"+dot+r'((?::32)?) +~(( \\[^ ]+| [_^]"[^"]*")*) '+" +~ ".join(["(?P=note)4"+dot]*(numNotes-1)),r"\g<1>"+result+r"\g<2>\g<3>",out)
           out = re.sub("r4"+dot+r'(( \\[^ ]+| [_^]"[^"]*")*) '+" ".join(["r4"+dot]*(numNotes-1)),"r"+result+r"\g<1>",out)
           if dot: chkLen=6
           else: chkLen = 4
           out = re.sub(r"\\repeat tremolo "+str(chkLen)+r" { (?P<note1>[^ ]+)32 (?P<note2>[^ ]+)32 } +~(( \\[^ ]+)*) "+" +~ ".join(["< (?P=note1) (?P=note2) >4"+dot]*(numNotes-1)),r"\\repeat tremolo "+str(chkLen*numNotes)+r" { \g<1>32 \g<2>32 }\g<3>",out)
       out = re.sub(r"(\\repeat tremolo [^{]+{ [^ ]+)( [^}]+ })(( +\\[pmf][^ ]*)+)",r"\g<1>\g<3>\g<2>",out) # dynamics need to attach inside the tremolo (but not other \ commands like \bar or \time or another tremolo \repeat)
       out = re.sub(r'(%\{ bar [0-9]*: %\} | \\major ) *r(?=[^ ]*(?: [_^]"[^"]*")?[| ]* (?:\\noPageBreak )?(?:%\{ bar|\\bar|\}$))',r"\g<1>R",out)
       out = out.replace(r"\new RhythmicStaff \with {",r"\new RhythmicStaff \with { \override VerticalAxisGroup.default-staff-staff-spacing = #'((basic-distance . 6) (minimum-distance . 6) (stretchability . 0)) ") # don't let it hang too far up in the air
   if not_angka: out=out.replace("make-bold-markup","make-simple-markup")
   return out,maxBeams,lyrics,headers,LP_between_head_and_first_score

def process_input(inDat):
 ret = []
 global scoreNo, western, has_lyrics, midi, not_angka, maxBeams, uniqCount, notehead_markup
 uniqCount = 0 ; notehead_markup = NoteheadMarkup()
 scoreNo = 0 # incr'd to 1 below
 western = False
 find_grace_height(inDat)
 inDat = re.sub(r"(%.*Next)(?=(Part|Score)\s)",r"\1 ",inDat) # in case someone commented out NextScore or NextPart
 for score in re.split(r"\sNextScore\s"," "+inDat+" "):
  if not score.strip(): continue
  scoreNo += 1
  has_lyrics = not not re.search("(^|\n)[LH]:",score) # The occasional false positive doesn't matter: has_lyrics==False is only an optimisation so we don't have to create use_rest_hack voices.  It is however important to always detect lyrics if they are present.
  parts = [p for p in re.split(r"\sNextPart\s"," "+score+" ") if p.strip()]
  for midi in [False,True]:
   not_angka = False # may be set by getLY
   if scoreNo==1 and not midi: ret.append(all_scores_start(inDat)) # now we've established non-empty
   separate_score_per_part = midi and re.search(r"\sPartMidi\s"," "+score+" ") and len(parts)>1 # (results in 1st MIDI file containing all parts, then each MIDI file containing one part, if there's more than 1 part)
   for separate_scores in [False,True] if separate_score_per_part else [False]:
    headers = {} # will accumulate below
    notehead_markup.separateTimesig=False
    for partNo,part in enumerate(parts):
     if partNo==0 or separate_scores:
         ret.append(score_start())
     out,maxBeams,lyrics,headers,LP_between_head_and_first_score = getLY(part,headers,partNo==0 or separate_scores) # assume 1st part doesn't have 'tacet al fine'
     if not midi: ret[0] += LP_between_head_and_first_score # (if midi it will already have been done, but we do catch LPH from any part)
     if len(parts)>1 and "instrument" in headers:
         inst = headers["instrument"]
         del headers["instrument"]
     else: inst = None
     if "chords" in headers:
         if "frets" in headers:
             frets = headers["frets"]
             assert frets in ["guitar","ukulele","mandolin"]
             fretsInc = r'\include "predefined-'+frets+'-fretboards.ly"\n'
             if not fretsInc in ret: ret.insert(1,fretsInc) # after all-scores-start
             del headers["frets"]
         else: frets = None
         ret.append(r'\new ChordNames { \set chordChanges = ##t ')
         if notehead_markup.chordsRoman: ret.append(r'\set chordNameLowercaseMinor = ##t')
         ret.append(r'\chordmode { '+headers["chords"]+' } }')
         if frets: ret.append(r'\new FretBoards { '+('' if frets=='guitar' else r'\set Staff.stringTunings = #'+frets+'-tuning')+r' \chordmode { '+headers["chords"]+' } }')
         del headers["chords"]
     if midi:
       ret.append(midi_staff_start()+" "+out+" "+midi_staff_end())
     else:
       staffStart,voiceName = jianpu_staff_start(inst)
       ret.append(staffStart+" "+out+" "+jianpu_staff_end())
       if notehead_markup.withStaff:
           western=True
           staffStart,voiceName = western_staff_start(inst)
           average_octave = sum(notehead_markup.octavesSeen)*1.0/len(notehead_markup.octavesSeen) if notehead_markup.octavesSeen else 0
           if average_octave < -0.5: staffStart += r" \clef bass" # might want to say <0 but being conservative for now
           ret.append(staffStart+" "+getLY(part,have_final_barline=False)[0]+" "+western_staff_end())
           western = False
       if lyrics: ret.append("".join(lyrics_start(voiceName)+l+" "+lyrics_end()+" " for l in lyrics))
     if partNo==len(parts)-1 or separate_scores:
       ret.append(score_end(**headers))
 ret = "".join(r+"\n" for r in ret)
 if lilypond_minor_version() >= 24: ret=re.sub(r"(\\override [A-Z][^ ]*) #'",r"\1.",ret) # needed to avoid deprecation warnings on Lilypond 2.24
 return ret

def get_unicode_approx(inDat):
    if re.search(r"\sNextPart\s"," "+inDat+" "): errExit("multiple parts in Unicode mode not yet supported")
    if re.search(r"\sNextScore\s"," "+inDat+" "): errExit("multiple scores in Unicode mode not yet supported")
    # TODO: also pick up on other not-supported stuff e.g. grace notes (or check for unicode_approx when these are encountered)
    global notehead_markup, western, midi, uniqCount, scoreNo, has_lyrics, not_angka, maxBeams
    notehead_markup = NoteheadMarkup()
    western = midi = not_angka = False
    has_lyrics = True # doesn't matter for our purposes (see 'false positive' comment above)
    uniqCount = 0 ; scoreNo = 1
    getLY(inDat,{})
    u=u''.join(notehead_markup.unicode_approx)
    if u.endswith(u'\u2502'): u=u[:-1]+u'\u2551'
    return u

try: from shlex import quote
except:
    def quote(f): return "'"+f.replace("'","'\"'\"'")+"'"

def outName(files,ext):
    if files: fn=os.path.split(files[0])[1].rsplit(os.extsep,1)[0]
    else: fn = 'jianpu'
    return fn+os.extsep+ext

def write_exported(inDat,fn):
    if not sys.stdout.isatty():
        return fix_utf8(sys.stdout,'w').write(inDat)
    if os.path.exists(fn):
        if not sys.stdin.isatty(): errExit(fn+" already exists and we cannot ask for overwrite confirmation as stdin is not a tty")
        print(fn+" already exists, press Enter to overwrite it or Ctrl+C:")
        (input if type("")==type(u"") else raw_input)()
    o=open(fn,'w')
    fix_utf8(o,'w').write(inDat)
    o.close() ; print("Saved to "+fn)

def write_unicode(outDat):
    if sys.stdout.isatty() and sys.platform=='win32' and sys.version_info() < (3,6):
        # Unicode on this console could be a problem
        print ("""
For Unicode approximation on this system, please do one of these things:
(1) redirect output to a file,
(2) upgrade to Python 3.6 or above, or
(3) switch from Microsoft Windows to GNU/Linux""")
    else: fix_utf8(sys.stdout,'w').write(outDat+"\n")

def write_output(outDat,fn):
    if sys.stdout.isatty(): # No output redirect.  Previous versions used temp directory, so be careful with overwrites:
        cwd = os.getcwd()
        if os.path.exists(fn) and not b"\n%{ The jianpu-ly input was:\n" in open(fn,"rb").read():
            print(cwd+os.sep+fn+" already exists\nand doesn't look like jianpu-ly output, so not overwriting it")
            os.chdir(tempfile.gettempdir())
        print("Outputting to "+os.getcwd()+os.sep+fn)
        o = open(fn,'w')
        fix_utf8(o,'w').write(outDat)
        o.close()
        pdf = fn.rsplit(os.extsep,1)[0]+os.extsep+'pdf'
        try: os.remove(pdf) # so won't show old one if lilypond fails
        except: pass
        cmd = lilypond_command()
        if cmd:
            cmd += ' -dstrokeadjust' # if will be viewed on-screen rather than printed, and it's not a Retina display
            if os.system(cmd+" "+quote(fn)): errExit("Lilypond failure")
            elif sys.platform=='darwin':
                os.system("open "+quote(pdf))
            elif sys.platform.startswith('win'):
                import subprocess
                subprocess.Popen([quote(pdf)],shell=True)
            elif (shutil.which('evince') if hasattr(shutil,'which') else os.path.exists('/usr/bin/evince')): os.system("evince "+quote(pdf))
        os.chdir(cwd) ; return
    # If get here, stdout redirected or Unicode-approx set
    fix_utf8(sys.stdout,'w').write(outDat)

export = html = markdown = chinese = False
args = {
    '--noRestHack': ("Disable the rest hack (debug option to try if output goes wrong)", "禁用休止符替代（调试选项，输出有误时可尝试）", [('use_rest_hack',False)]),
    '--nosort': ("Don't sort chord notes by pitch (crossing parts when 2-voice music was mistakenly coded as chords)", "不按音高排序和弦音符（适用于两个声部被写成和弦、又需要声部交叉的情况）", [('sort_chords',False)]),
    '--withStaff': ("Add a Western staff doubling the tune in all parts, as if WithStaff is specified everywhere","在每个声部增加一个西方五线谱，相当于到处指定WithStaff",[('force_staff',True)]),
    '--noStaff': ("Force no Western staff, even if the input asks for one","强制不输出西方五线谱（即使输入指定了 WithStaff）",[('force_staff',False)]),
    '--html': ("Write the HTML help for the website","输出HTML格式的文档（用于网站）",[('html',True),write_docs]),
    '--markdown': ("Write the Markdown documentation for the Git README","输出Markdown格式的文档（用于Git README）",[('markdown',True),write_docs]),
    '--help':("Write command-line help (aliases: -h, /?)","显示命令行帮助（别名：-h、/?）",[write_help]),'-h':("","",[write_help]),'/?':("","",[write_help]),
    '--chinese': ("Use Chinese for --html, --markdown or --help options","以中文输出--html、--markdown或--help的内容",[('chinese',True)]),
    '--version': ("Just write version number (aliases: -v, /v)","只显示版本号（别名：-v、/v）",[write_version]),'-v':("","",[write_version]),'/v':("","",[write_version]),
    '--export-jly': ("Export the jianpu input to a .jly file instead of converting it (useful e.g. after a MusicXML import)","导出简谱输入到 .jly 文件而不进行转换（例如在导入 MusicXML 之后）",[('export', True)]),
    '--export-txt': ("","",[('export',True)]), # hidden alias for --export-jly for backward compatibility
    '--unicode-approx': ("Output a Unicode approximation of the jianpu instead of Lilypond code","用Unicode近似值代替Lilypond代码",[('unicode_approx',True)]),
    '--octaveShiftSounding': ("Treat MusicXML octave-shift pitch data as sounding pitch (as unclearly stated in the W3C MusicXML 4 specification and as done by musicxml2ly); the default unless the file says it was encoded by MuseScore","强制将MusicXML octave-shift区域内的音高数据视为实际音高（W3C规范／musicxml2ly的行为）；除非文件注明由MuseScore生成，否则默认为此行为",[('xml_octaveShift_override','sounding')]),
    '--octaveShiftWritten': ("Treat MusicXML octave-shift pitch data as written pitch (MuseScore behaviour, at least in versions 2 and 3); jianpu dots are adjusted to sounding pitch","强制将MusicXML octave-shift区域内的音高数据视为记谱音高（MuseScore的行为）；简谱的八度点将调整为实际音高",[('xml_octaveShift_override','written')]),
}
def read_args():
    files,actions = [],[]
    for a in sys.argv[1:]:
        if a in args:
            enDoc,zhDoc,actionList = args[a]
            for action in actionList:
                if type(action)==tuple:
                    globals()[action[0]] = action[1]
                elif action not in actions: actions.append(action)
        else: files.append(a)
    if actions:
        for a in actions: a()
        raise SystemExit
    else: return files
def main():
    files = read_args()
    inDat = get_input(files)
    if export: return write_exported(inDat,outName(files,"jly")) # not txt please because we don't want to imply arbitrary wrap is OK on header or lyric lines
    if unicode_approx: return write_unicode(get_unicode_approx(inDat))
    out = process_input(inDat) # you can also call this if importing as a module
    write_output(out,outName(files,"ly"))

if __name__=="__main__": main()
