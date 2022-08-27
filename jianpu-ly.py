#!/usr/bin/env python
# (can be run with either Python 2 or Python 3)

# Jianpu (numbered musical notaion) for Lilypond
# v1.581 (c) 2012-2022 Silas S. Brown

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

# Homepage: http://ssb22.user.srcf.net/mwrhome/jianpu-ly.py
# Git repository: https://github.com/ssb22/jianpu-ly
# and on GitLab: https://gitlab.com/ssb22/jianpu-ly
# and on Bitbucket: https://bitbucket.org/ssb22/jianpu-ly
# and at https://gitlab.developers.cam.ac.uk/ssb22/jianpu-ly
# and in China: git clone https://gitee.com/ssb22/jianpu-ly

# (The following doc string's format is fixed, see --html)
r"""Run jianpu-ly < text-file > ly-file (or jianpu-ly text-files > ly-file)
Text files are whitespace-separated and can contain:
Scale going up: 1 2 3 4 5 6 7 1'
Accidentals: 1 #1 2 b2 1
Octaves: 1,, 1, 1 1' 1''
Shortcuts for 1' and 2': 8 9
Semiquaver, quaver, crotchet (16/8/4th notes): s1 q1 1
Dotted versions of the above (50% longer): s1. q1. 1.
Demisemiquaver, hemidemisemiquaver (32/64th notes): d1 h1
Minims (half notes) use dashes: 1 -
Dotted minim: 1 - -
Semibreve (whole note): 1 - - -
Time signature: 4/4
Time signature with quaver anacrusis (8th-note pickup): 4/4,8
Key signature (major): 1=Bb
Key signature (minor): 6=F#
Tempo: 4=85
Lyrics: L: here are the syl- la- bles (all on one line)
Lyrics (verse 1): L: 1. Here is verse one
Lyrics (verse 2): L: 2. Here is verse two
Hanzi lyrics (auto space): H: hanzi (with or without spaces)
Lilypond headers: title=the title (on a line of its own)
Multiple movements: NextScore
Prohibit page breaks until end of this movement: OnePage
Suppress bar numbers: NoBarNums
Old-style time signature: SeparateTimesig 1=C 4/4
Indonesian 'not angka' style: angka
Add a Western staff doubling the tune: WithStaff
Tuplets: 3[ q1 q1 q1 ]
Grace notes before: g[#45] 1
Grace notes after: 1 ['1]g
Simple chords: 135 1 13 1
Da capo: 1 1 Fine 1 1 1 1 1 1 DC
Repeat (with alternate endings): R{ 1 1 1 } A{ 2 | 3 }
Short repeats (percent): R4{ 1 2 }
Ties (like Lilypond's, if you don't want dashes): 1 ~ 1
Slurs (like Lilypond's): 1 ( 2 )
Dynamics (applies to previous note): \p \mp \f
Other 1-word Lilypond \ commands: \fermata \> \! \( \) etc
Other Lilypond code: LP: (block of code) :LP (each delimeter at start of its line)
Ignored: % a comment
"""

import sys,os,re
from fractions import Fraction as F # requires Python 2.6+
if type(u"")==type(""): # Python 3
    unichr,xrange = chr,range
    from string import ascii_letters as letters
else: from string import letters # Python 2
def asUnicode(l):
    if type(l)==type(u""): return l
    return l.decode('utf-8')

def all_scores_start(staff_size = 20):
    # staff_size is the 5-line size in points; jianpu is smaller
    return r"""\version "2.18.0"
#(set-global-staff-size %d)

%% un-comment the next line to remove Lilypond tagline:
%% \header { tagline="" }

\pointAndClickOff

\paper {
  print-all-headers = ##t %% allow per-score headers

  %% un-comment the next line for A5:
  %% #(set-default-paper-size "a5" )

  %% un-comment the next line for no page numbers:
  %% print-page-number = ##f

  %% un-comment the next 3 lines for a binding edge:
  %% two-sided = ##t
  %% inner-margin = 20\mm
  %% outer-margin = 10\mm

  %% un-comment the next line for a more space-saving header layout:
  %% scoreTitleMarkup = \markup { \center-column { \fill-line { \magnify #1.5 { \bold { \fromproperty #'header:dedication } } \magnify #1.5 { \bold { \fromproperty #'header:title } } \fromproperty #'header:composer } \fill-line { \fromproperty #'header:instrument \fromproperty #'header:subtitle \smaller{\fromproperty #'header:subsubtitle } } } }
}
""" % staff_size

def score_start():
    ret = "\\score {\n"
    if midi: ret += "\\unfoldRepeats\n"
    ret += r"<< "
    if not notehead_markup.noBarNums and not midi: ret += ("\\override Score.BarNumber #'break-visibility = #end-of-line-invisible\n\\override Score.BarNumber #'Y-offset = -1\n\\set Score.barNumberVisibility = #(every-nth-bar-number-visible %d)" % bar_number_every)
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
    if midi: ret += r"\midi { \context { \Score tempoWholesPerMinute = #(ly:make-moment 84 4)}}" # will be overridden by any \tempo command used later
    elif notehead_markup.noBarNums: ret += r'\layout { \context { \Score \remove "Bar_number_engraver" } }'
    else: ret += r"\layout{}"
    return ret + " }"

tempCount = 0
def jianpu_voice_start(voiceName="tmp"):
    stemLenFrac = "0" # unless overridden to 0.5 below
    if voiceName=="tmp": # make it unique just in case
        global tempCount
        voiceName += str(tempCount) ; tempCount += 1
    elif maxBeams >= 2: stemLenFrac = "0.5" # sometimes needed if the semiquavers occur in isolation rather than in groups (TODO do we need to increase this for 3+ beams in some cases?)
    r = (r"""\new Voice="%s" {"""%voiceName)+"\n"
    r += r"""
    \override Beam #'transparent = ##f % (needed for LilyPond 2.18 or the above switch will also hide beams)
    """
    if not_angka:
        r +=r"""
        \override Stem #'direction = #UP
        \override Tie #'staff-position = #-2.5
        \tupletDown"""
        stemLenFrac=str(0.4+0.2*max(0,maxBeams-1))
    else: r += r"""\override Stem #'direction = #DOWN
    \override Tie #'staff-position = #2.5
    \tupletUp"""+"\n"
    r += (r"""
    \override Stem #'length-fraction = #%s
    \override Beam #'beam-thickness = #0.1
    \override Beam #'length-fraction = #0.5
    \override Voice.Rest #'style = #'neomensural %% this size tends to line up better (we'll override the appearance anyway)
    \override Accidental #'font-size = #-4
    \override TupletBracket #'bracket-visibility = ##t""" % stemLenFrac)
    r += "\n"+r"""\set Voice.chordChanges = ##t %% 2.19 bug workaround""" # LilyPond 2.19.82: \applyOutput docs say "called for every layout object found in the context Context at the current time step" but 2.19.x breaks this by calling it for ALL contexts in the current time step, hence breaking our WithStaff by applying our jianpu numbers to the 5-line staff too.  Obvious workaround is to make our function check that the context it's called with matches our jianpu voice, but I'm not sure how to do this other than by setting a property that's not otherwise used, which we can test for in the function.  So I'm 'commandeering' the "chordChanges" property (there since at least 2.15 and used by Lilypond only when it's in chord mode, which we don't use, and if someone adds a chord-mode staff then it won't print noteheads anyway): we will substitute jianpu numbers for noteheads only if chordChanges = #t.
    return r+"\n"
def jianpu_staff_start():
    # (we add "BEGIN JIANPU STAFF" and "END JIANPU STAFF" comments to make it easier to copy/paste into other Lilypond files)
    if not_angka: voiceName="notAngka"
    else: voiceName="jianpu"
    if not_angka: r=r"""
%% === BEGIN NOT ANGKA STAFF ===
    \new RhythmicStaff \with {"""
    else: r=r"""
%% === BEGIN JIANPU STAFF ===
    \new RhythmicStaff \with {
    \consists "Accidental_engraver" """
    r+=r"""
    %% Get rid of the stave but not the barlines:
    \override StaffSymbol #'line-count = #0 %% tested in 2.15.40, 2.16.2, 2.18.0, 2.18.2 and 2.20.0
    \override BarLine #'bar-extent = #'(-2 . 2) %% LilyPond 2.18: please make barlines as high as the time signature even though we're on a RhythmicStaff (2.16 and 2.15 don't need this although its presence doesn't hurt; Issue 3685 seems to indicate they'll fix it post-2.18)
    }
    { """+jianpu_voice_start(voiceName)+r"""
    \override Staff.TimeSignature #'style = #'numbered
    \override Staff.Stem #'transparent = ##t
    """
    return r
def jianpu_staff_end():
     # \bar "|." is added separately if there's not a DC etc
    if not_angka: return "} }\n% === END NOT ANGKA STAFF ===\n"
    else: return "} }\n% === END JIANPU STAFF ===\n"
def midi_staff_start(voiceName="midi"):
    return r"""
%% === BEGIN MIDI STAFF ===
    \new Staff { \new Voice="%s" {""" % (voiceName,)
def midi_staff_end(): return "} }\n% === END MIDI STAFF ===\n"
def western_staff_start(voiceName="5line"):
    return r"""
%% === BEGIN 5-LINE STAFF ===
    \new Staff { \new Voice="%s" {
    #(set-accidental-style 'modern-cautionary)
    \override Staff.TimeSignature #'style = #'numbered
    \set Voice.chordChanges = ##f %% for 2.19.82 bug workaround
""" % (voiceName,)
def western_staff_end(): return "} }\n% === END 5-LINE STAFF ===\n"

lyricsPtr = 0
def lyrics_start(voiceName="jianpu"):
    global lyricsPtr ; lyricsPtr += 1 # TODO: encapsulate
    return r'\new Lyrics = "I%s" { \lyricsto "%s" { ' % (str(lyricsPtr).translate((letters*5)[:256]),voiceName)
def lyrics_end(): return "} }"

dashes_as_ties = True # Implement dash (-) continuations as invisible ties rather than rests; sometimes works better in awkward beaming situations
use_rest_hack = True # Implement short rests as notes (and if there are lyrics, creates temporary voices so the lyrics miss them); sometimes works better for beaming (at least in 2.15, 2.16 and 2.18)
if __name__=="__main__" and '--noRestHack' in sys.argv: # TODO: document
    use_rest_hack=False ; sys.argv.remove('--noRestHack')
assert not (use_rest_hack and not dashes_as_ties), "This combination has not been tested"

def errExit(msg):
    if __name__=="__main__":
        sys.stderr.write("Error: "+msg+"\n")
        sys.exit(1)
    else: raise Exception(msg)

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
    '-':'r'}

class notehead_markup:
  def __init__(self):
      self.defines_done = {} ; self.initOneScore()
  def initOneScore(self):
      self.barLength = 64 ; self.beatLength = 16 # in 64th notes
      self.barPos = self.startBarPos = F(0)
      self.inBeamGroup = self.lastNBeams = self.onePage = self.noBarNums = self.separateTimesig = self.withStaff = 0
      self.current_accidentals = {}
      self.barNo = 1
      self.tuplet = (1,1)
      self.last_figures = None
      self.last_was_rest = False
      self.notesHad = []
  def endScore(self):
      if not self.barPos == self.startBarPos: errExit("Incomplete bar at end of score %d (pos %d, should be %d)" % (scoreNo,self.barPos,self.startBarPos))
  def setTime(self,num,denom):
      self.barLength = int(64*num/denom)
      if denom>4 and num%3==0: self.beatLength = 24 # compound time
      else: self.beatLength = 16
  def setAnac(self,denom,dotted):
      self.barPos = F(self.barLength)-F(64)/denom
      if dotted: self.barPos -= F(64)/denom/2
      if not self.barPos: errExit("Anacrusis should be shorter than bar in score %d" % scoreNo)
      self.startBarPos = self.barPos
  def __call__(self,figures,nBeams,dot,octave,accidental):
    # figures is a chord string of '1'-'7', or '0' or '-'
    # nBeams is 0, 1, 2 .. etc (number of beams for this note)
    # dot is "" or "." (dotted length)
    # octave is "", "'", "''", "," or ",,"
    # accidental is "", "#", "b"
    if len(figures)>1 and accidental: errExit("Accidentals in chords not yet implemented") # see TODOs below
    self.notesHad.append(figures)
    names = {'0':'nought',
             '1':'one',
             '2':'two',
             '3':'three',
             '4':'four',
             '5':'five',
             '6':'six',
             '7':'seven',
             '-':'dash'}
    def get_placeholder_chord(figures):
        if len(figures)==1:
            return placeholders[figures]
        elif not midi and not western: return 'c' # we'll override its appearance
        else: return "< "+" ".join(placeholders[f] for f in list(figures))+" >"
    placeholder_chord = get_placeholder_chord(figures)
    invisTieLast = dashes_as_ties and self.last_figures and figures=="-" and not self.last_was_rest
    self.last_was_rest = (figures=='0' or (figures=='-' and self.last_was_rest))
    name = ''.join(names[f] for f in figures)
    if not_angka:
        # include accidental in the lookup key
        # because it affects the notehead shape
        figures += accidental # TODO: chords?
        name += {"#":"-sharp","b":"-flat","":""}[accidental]
    if invisTieLast: # (so figures == "-")
        figures += self.last_figures # (so "-" + last)
        name += ''.join(names[f] for f in self.last_figures)
        placeholder_chord = get_placeholder_chord(self.last_figures)
        octave = self.last_octave # for MIDI or 5-line
        accidental = self.last_accidental # ditto
    self.last_figures = figures
    if len(self.last_figures)>1 and self.last_figures[0]=='-': self.last_figures = self.last_figures[1:]
    self.last_octave = octave
    self.last_accidental = accidental
    if figures not in self.defines_done and not midi and not western:
        # Define a notehead graphical object for the figures
        self.defines_done[figures] = "note-"+name
        if figures.startswith("-"):
          if not_angka: figuresNew="."
          else:
            figuresNew=u"\u2013"
            if not type(u"")==type(""):
                figuresNew=figuresNew.encode('utf-8')
        else: figuresNew = figures
        ret = """#(define (%s grob grob-origin context)
  (if (and (eq? (ly:context-property context 'chordChanges) #t)
      (or (grob::has-interface grob 'note-head-interface)
        (grob::has-interface grob 'rest-interface)))
    (begin
      (ly:grob-set-property! grob 'stencil
        (grob-interpret-markup grob
          """ % self.defines_done[figures]
        if len(figuresNew)==1 or figures.startswith("-"): ret += """(make-lower-markup 0.5 (make-bold-markup "%s")))))))
""" % figuresNew
        elif not_angka and accidental: # not chord
            u338,u20e5=u"\u0338",u"\u20e5" # TODO: the \ looks better than the / in default font
            if not type("")==type(u""): u338,u20e5=u338.encode('utf-8'),u20e5.encode('utf-8')
            ret += '(make-lower-markup 0.5 (make-bold-markup "%s%s")))))))\n' % (figures[:1],{'#':u338,'b':u20e5}[accidental])
        else: ret += """(markup (#:lower 0.5
          (#:override (cons (quote direction) 1)
          (#:override (cons (quote baseline-skip) 1.8)
          (#:dir-column (\n""" + "".join('    #:line (#:bold "'+f+'")\n' for f in figuresNew) + """)))))))))))
""" # TODO: can do accidentals e.g. #:halign 1 #:line ((#:fontsize -5 (#:raise 0.7 (#:flat))) (#:bold "3")) but might cause the beam not to extend its full length if this chord occurs at the end of a beamed group, + accidentals won't be tracked by Lilypond and would have be taken care of by jianpu-ly (which might mean if any chord has an accidental on one of its notes we'd have to do all notes in that bar like this, whether they are chords or not)
    else: ret = ""
    if self.barPos==0 and self.barNo > 1:
        ret += "| " # barline in Lilypond file: not strictly necessary but may help readability
        if self.onePage and not midi: ret += r"\noPageBreak "
        ret += "%{ bar "+str(self.barNo)+": %} "
    if not octave in self.current_accidentals: self.current_accidentals[octave] = [""]*7
    if figures=="-" or all('1'<=figure<='7' and not accidental==self.current_accidentals[octave][int(figure)-1] for figure in list(figures)) and nBeams > self.lastNBeams: leftBeams = nBeams # beam needs to fit under the new accidental (or the dash which might be slightly to the left of where digits are), but if it's no more than last note's beams then we'll hang it only if in same beat.  (TODO: the current_accidentals logic may need revising if other accidental styles are used, e.g. modern-cautionary, although then would need to check anyway if our \consists "Accidental_engraver" is sufficient)
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
    if dot: toAdd += toAdd/2
    if not self.tuplet[0]==self.tuplet[1]:
        toAdd = toAdd*self.tuplet[0]/self.tuplet[1]
    if nBeams and not midi and not western: # must set these unconditionally regardless of what we think their current values are (Lilypond's own beamer can change them from note to note)
        if not_angka:
            leftBeams=nBeams
            if (self.barPos+toAdd)%self.beatLength == 0: nBeams = 0
        ret += (r"\set stemLeftBeamCount = #%d"+"\n") % leftBeams
        ret += (r"\set stemRightBeamCount = #%d"+"\n") % nBeams
        if not_angka: nBeams = leftBeams
    need_space_for_accidental = False
    for figure in list(figures):
        if '1'<=figure<='7':
            if not accidental==self.current_accidentals[octave][int(figure)-1]:
                need_space_for_accidental = True
            self.current_accidentals[octave][int(figure)-1] = accidental # TODO: not sensible (assumes accidental applies to EVERY note in the chord, see above)
    inRestHack = 0
    if not midi and not western:
        if ret: ret = ret.rstrip()+"\n" # try to keep the .ly code vaguely readable
        ret += r"  \applyOutput #'Voice #"+self.defines_done[figures]+" "
        if placeholder_chord == "r" and use_rest_hack and nBeams:
            placeholder_chord = "c"
            # C to work around diagonal-tail problem with
            # some isolated quaver rests in some Lilypond
            # versions (usually at end of bar); new voice
            # so lyrics miss it as if it were a rest:
            if has_lyrics and not self.withStaff: # (OK if self.withStaff: lyrics will be attached to that instead)
                ret = jianpu_voice_start() + ret
                inRestHack = 1
                if self.inBeamGroup and not self.inBeamGroup=="restHack": aftrlast0 = "] "
    if placeholder_chord.startswith("<"):
        # Octave with chords: apply to last note if up, 1st note if down
        notes = placeholder_chord.split()[1:-1]
        assert len(notes) >= 2
        notes[0] += {",":"",",,":","}.get(octave,"'")
        notes[-1] += {"'":"''","''":"'''"}.get(octave,"'")
        ret += "< "+" ".join(notes)+" >"
    else: # single note or rest
        ret += placeholder_chord + {"":"", "#":"is", "b":"es"}[accidental]
        if not placeholder_chord=="r": ret += {"":"'","'":"''","''":"'''",",":"",",,":","}[octave] # for MIDI + Western, put it so no-mark starts near middle C
    ret += ("%d" % length) + dot
    if nBeams and (not self.inBeamGroup or self.inBeamGroup=="restHack" or inRestHack) and not midi and not western:
        # We need the above stemLeftBeamCount, stemRightBeamCount override logic to work even if we're an isolated quaver, so do this:
        ret += '['
        self.inBeamGroup = 1
    self.barPos += toAdd
    # sys.stderr.write(accidental+figure+octave+dot+"/"+str(nBeams)+"->"+str(self.barPos)+" ") # if need to see where we are
    if self.barPos > self.barLength: errExit("(notesHad=%s) barcheck fail: note crosses barline at \"%s\" with %d beams (%d skipped from %d to %d, bypassing %d), scoreNo=%d barNo=%d (but the error could be earlier)" % (' '.join(self.notesHad),figures,nBeams,toAdd,self.barPos-toAdd,self.barPos,self.barLength,scoreNo,self.barNo))
    if self.barPos%self.beatLength == 0 and self.inBeamGroup: # (self.inBeamGroup is set only if not midi/western)
        # jianpu printouts tend to restart beams every beat
        # (but if there are no beams running anyway, it occasionally helps typesetting to keep the logical group running, e.g. to work around bugs involving beaming a dash-and-rest beat in 6/8) (TODO: what if there's a dash-and-rest BAR?  [..]-notated beams don't usually work across barlines
        ret += ']'
        self.inBeamGroup = 0 # DON'T reset lastNBeams here (needed for start-of-group accidental logic)
    elif inRestHack and self.inBeamGroup:
        ret += ']'
        self.inBeamGroup = 'restHack'
    self.lastNBeams = nBeams
    if self.barPos == self.barLength:
        self.barPos = 0 ; self.barNo += 1
        self.current_accidentals = {}
    # Octave dots:
    if not midi and not western and not invisTieLast:
      # Tweak the Y-offset, as Lilypond occasionally puts it too far down:
      if not nBeams: ret += {",":r"-\tweak #'Y-offset #-1.2 ",
                             ",,":r"-\tweak #'Y-offset #1 "}.get(octave,"")
      oDict = {"":"",
            "'":"^.",
            "''":r"-\tweak #'X-offset #0.3 ^\markup{\bold :}",
            ",":r"-\tweak #'X-offset #0.6 _.",
            ",,":r"-\tweak #'X-offset #0.3 _\markup{\bold :}"}
      if not_angka: oDict.update({
              "'":r"-\tweak #'extra-offset #'(0.4 . 2.7) -\markup{\bold .}",
              "''":r"-\tweak #'extra-offset #'(0.4 . 3.5) -\markup{\bold :}",
              })
      ret += oDict[octave]
    if invisTieLast:
        if midi or western: b4last, aftrlast = "", " ~"
        else: b4last,aftrlast = r"\once \override Tie #'transparent = ##t \once \override Tie #'staff-position = #0 "," ~"
    else: b4last,aftrlast = "",""
    if inRestHack: ret += " } "
    return b4last,aftrlast0+aftrlast,ret, need_space_for_accidental

notehead_markup = notehead_markup()

def parseNote(word):
    if word==".": word = "-" # (for not angka, TODO: document that this is now acceptable as an input word?)
    word = word.replace("8","1'").replace("9","2'")
    if type(u"")==type(""): word = word.replace(u"\u2019","'")
    else: word=word.replace(u"\u2019".encode('utf-8'),"'")
    if not re.match("[0-7.,'qsdh\\#b-]+$",word): figures = None # unrecognised stuff in it: flag as error, rather than ignoring and possibly getting a puzzling barsync fail
    else: figures = ''.join(re.findall('[01234567-]',word))
    if "." in word: dot="."
    else: dot=""
    if "q" in word: nBeams=1
    elif "s" in word: nBeams=2
    elif "d" in word: nBeams=3
    elif "h" in word: nBeams=4
    elif "\\" in word: nBeams=len(word.split("\\"))-1 # requested by a user who found British note-length names hard to remember; won't work if the \ is placed at the start, as that'll be a Lilypond command, so to save confusion we won't put this in the docstring
    else: nBeams=0
    octave = ""
    for o in ["''","'",",,",","]:
        if o in word:
            octave = o ; break
    accidental = ""
    for acc in ["#","b"]:
        if acc in word:
            accidental = acc ; break
    return figures,nBeams,dot,octave,accidental

def write_docs():
    # Write an HTML or Markdown version of the doc string
    def htmlify(l):
        if "--html" in sys.argv:
            return l.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")
        else: return l
    inTable = 0 ; justStarted=1
    for line in __doc__.split("\n"):
        if not line.strip(): continue
        if ":" in line and line.split(":",1)[1].strip():
            toGet,shouldType = line.split(":",1)
            if not inTable:
                if "--html" in sys.argv:
                    print ("<table border>") # "<tr><th>To get:</th><th>Type:</th></tr>"
                else: print ("")
                inTable = 1
            if re.match(r".*[A-Za-z]\)$",shouldType):
                shouldType,note = shouldType.rsplit("(",1)
                note = " ("+note
            else: note = ""
            if "--html" in sys.argv: print ("<tr><td>"+toGet.strip()+"</td><td><kbd>"+shouldType.strip()+"</kbd>"+note+"</td>")
            else: print (toGet.strip()+": `"+shouldType.strip()+"`"+note+"\n")
        else:
            if "--markdown" in sys.argv: print ("")
            elif inTable: print ("</table>")
            elif not justStarted: print ("<br>")
            inTable=justStarted=0
            print (htmlify(line))
    if inTable and "--html" in sys.argv: print ("</table>")

def get_input():
  inDat = []
  for f in sys.argv[1:]:
    try:
        try: inDat.append(open(f,encoding="utf-8").read()) # Python 3: try UTF-8 first
        except: inDat.append(open(f).read()) # Python 2, or Python 3 with locale-default encoding in case it's not UTF-8
    except: errExit("Unable to read file "+f)
  if type("")==type(u""): # Python 3: please use UTF-8 for Lilypond, even if the system locale says something else
    import codecs
    stdin=codecs.getreader("utf-8")(sys.stdin.buffer)
    stdout=codecs.getwriter("utf-8")(sys.stdout.buffer)
    old_stdout, sys.stdout = sys.stdout, stdout # for print() (and keep a reference to the old one in case of overzealous gc)
  else: stdin = sys.stdin
  if not inDat:
    if sys.stdin.isatty():
        sys.stderr.write(__doc__)
        raise SystemExit
  inDat=[stdin.read()]
  for i in xrange(len(inDat)):
    if inDat[i].startswith('\xef\xbb\xbf'):
      inDat[i] = inDat[i][3:]
    if inDat[i].startswith(r'\version'): errExit("jianpu-ly does not READ Lilypond code.\nPlease see the instructions.")
  return " NextScore ".join(inDat)

def fix_fullwidth(t):
    if type(u"")==type(""): utext = t
    else: utext = t.decode('utf-8')
    r = []
    for c in utext:
        if 0xff01<=ord(c)<=0xff5e: r.append(unichr(ord(c)-0xfee0))
        elif c==unichr(0x201a): r.append(",") # sometimes used as comma (incorrectly)
        elif c==unichr(0xff61): r.append(".")
        else: r.append(c)
    utext = u"".join(r)
    if type(u"")==type(""): return utext
    else: return utext.encode('utf-8')

def graceNotes_markup(notes,isAfter):
    if isAfter: cmd = "jianpu-grace-after"
    else: cmd = "jianpu-grace"
    r = [] ; aftrNext = None
    thinspace = unichr(0x2009)
    if not type("")==type(u""): thinspace = thinspace.encode('utf-8')
    notes = grace_octave_fix(notes)
    for i in xrange(len(notes)):
        n = notes[i]
        if n=='#': r.append(r'\fontsize #-4 { \raise #0.6 { \sharp } }')
        elif n=='b': r.append(r'\fontsize #-4 { \raise #0.4 { \flat } }')
        elif n=="'":
            if i and notes[i-1]==notes[i]: continue
            if notes[i:i+2]=="''": above = ":"
            else: above = "."
            r.append(r"\override #'(direction . 1) \override #'(baseline-skip . 1.2) \dir-column { \line {")
            aftrNext = r"} \line { "+'"'+thinspace+above+'" } }'
        elif n==',':
            if i and notes[i-1]==notes[i]: continue
            if notes[i:i+2]==",,": below = ":"
            else: below = "."
            r.append(r"\override #'(baseline-skip . 1.0) \center-column { \line { ")
            aftrNext = r"} \line { \pad-to-box #'(0 . 0) #'(-0.2 . 0) "+'"'+below+'" } }'
        else:
            if r and r[-1].endswith('"'):
                r[-1] = r[-1][:-1] + n + '"'
            else: r.append('"%s"' % n)
            if aftrNext:
                r.append(aftrNext) ; aftrNext = None
    return r"^\tweak outside-staff-priority ##f ^\tweak avoid-slur #'inside ^\markup \%s { \line { %s } }" % (cmd,' '.join(r))
def grace_octave_fix(notes):
    notes = notes.replace("8","'1").replace("9","'2")
    if notes.endswith(',,') or notes.endswith("''"):
        # oops, should write this BEFORE the affected note
        return notes[:-3]+notes[-2:]+notes[-3]
    elif notes.endswith(',') or notes.endswith("'"):
        return notes[:-2]+notes[-1]+notes[-2]
    else: return notes
def gracenotes_western(notes):
    # for western and MIDI staffs
    notes = grace_octave_fix(notes)
    nextAcc = "" ; next8ve = "'" ; current_accidentals = [0]*7
    r = []
    for i in xrange(len(notes)):
        n = notes[i]
        if n=='#': nextAcc = "is"
        elif n=='b': nextAcc = "es"
        elif n=="'":
            if i and notes[i-1]==notes[i]: continue
            if notes[i:i+2]=="''": next8ve = "'''"
            else: next8ve = "''"
        elif n==',':
            if i and notes[i-1]==notes[i]: continue
            if notes[i:i+2]==",,": next8ve = ","
            else: next8ve = ""
        else:
            if not n in placeholders: continue # TODO: errExit ?
            r.append(placeholders[n]+nextAcc+next8ve+"16")
            nextAcc = "" ; next8ve = "'"
    return ' '.join(r)

def getLY(score):
   lyrics = "" ; headers = {}
   notehead_markup.initOneScore()
   out = [] ; maxBeams = 0 ; need_final_barline = 0
   repeatStack = [] ; lastPtr = 0
   escaping = inTranspose = 0
   aftrnext = defined_jianpuGrace = defined_JGR = None
   for line in score.split("\n"):
    line = fix_fullwidth(line).strip()
    line=re.sub(r"^%%\s*tempo:\s*(\S+)\s*$",r"\1",line) # to provide an upgrade path for jihuan-tian's fork
    if line.startswith("LP:"):
        # Escaped LilyPond block.  Thanks to James Harkins for this suggestion.
        # (Our internal barcheck does not understand code in LP blocks, so keep it to complete bars.)
        # E.g. for multibar rests:
        # LP:
        # \compressFullBarRests \override MultiMeasureRest #'expand-limit = #1
        # R1*5
        # :LP
        escaping = 1
        if len(line)>3: out.append(line[3:]+"\n") # remainder of current line
    elif line.startswith(":LP"):
        escaping = 0 # TODO: and process the rest of the line?  (assume on line of own for now)
    elif escaping:
        out.append(line+"\n")
    elif not line: pass
    elif line.startswith("L:") or line.startswith("H:"):
        # lyrics
        do_hanzi_spacing = line.startswith("H:")
        line = line[2:].strip()
        lyrics += lyrics_start()
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
                if needSpace and (0x4e00 <= ord(c) < 0xa700 or c in u"\u2018\u201c"):
                    l2.append(' ') ; needSpace = 0
                    if c in u"\u2018\u201c":
                        # we're just about to have an open quote - this needs to hang left.  Try:
                        l2.append(r"\once \override LyricText #'self-alignment-X = #CENTER ") # or RIGHT if there's no punctuation after
                if 0x4e00 <= ord(c) < 0xa700: needSpace=1
                l2.append(c)
            line = u"".join(l2)
            if not type("")==type(u""): line = line.encode('utf-8') # Python 2
        lyrics += toAdd+re.sub("(?<=[^- ])- "," -- ",line).replace(" -- "," --\n")+" "+lyrics_end()+" "
    elif re.match(r"\s*[A-Za-z]+\s*=",line):
        # Lilypond header
        hName,hValue = line.split("=",1)
        headers[hName.strip()] = hValue.strip()
    else:
        for word in line.split():
            if word.startswith('%'): break # a comment
            elif re.match("[1-468]+[.]*=[1-9][0-9]*$",word): out.append(r'\tempo '+word) # TODO: reduce size a little?
            elif re.match("[16]=[A-Ga-g][#b]?$",word): #key
                # Must use \transpose because \transposition doesn't always work.
                # However, don't use \transpose if printing - it adds extra accidentals to the rhythm staff.
                # So we have to do separate runs of \layout and \midi (hence the outer loop).
                if midi or western:
                    if inTranspose: out.append('}')
                    if word[0]=="6": transposeFrom = "a"
                    else: transposeFrom = "c"
                    transposeTo = word[word.index('=')+1:].replace("#","is").replace("b","es").lower()
                    if midi and transposeTo[0] in "gab": transposeTo += ','
                    out.append(r"\transpose c "+transposeTo+r" { \key c \major ") # so that MIDI or Western pitches are correct
                    inTranspose = 1
                else: out.append(r'\mark \markup{%s}' % word.replace("b",r"\flat").replace("#",r"\sharp"))
            elif re.match("[1-9][0-9]*/[1-468]+(,[1-9][0-9]*[.]?)?$",word): # time signature
                if ',' in word: # anacrusis
                    word,anac = word.split(",",1)
                else: anac=""
                if notehead_markup.separateTimesig and not midi: out.append(r'\mark \markup{'+word+'}')
                out.append(r'\time '+word)
                num,denom = word.split('/')
                notehead_markup.setTime(int(num),int(denom))
                if anac:
                    if anac.endswith("."): # e.g. 2.
                        a2 = anac[:-1] ; anacDotted = 1
                    else: a2,anacDotted = anac,0
                    notehead_markup.setAnac(int(a2),anacDotted)
                    out.append(r'\partial '+anac)
            elif word.startswith("\\") or word in ["(",")","~"]:
                # Lilypond command, \p etc
                if out and "afterGrace" in out[lastPtr]:
                    # apply to inside afterGrace in midi/western
                    out[lastPtr] = out[lastPtr][:-1] + word + " }"
                else: out.append(word)
            elif word=="OnePage":
                if notehead_markup.onePage: sys.stderr.write("WARNING: Duplicate OnePage, did you miss out a NextScore?\n")
                notehead_markup.onePage=1
            elif word=="NoBarNums":
                if notehead_markup.noBarNums: sys.stderr.write("WARNING: Duplicate NoBarNums, did you miss out a NextScore?\n")
                notehead_markup.noBarNums=1
            elif word=="SeparateTimesig":
                if notehead_markup.separateTimesig: sys.stderr.write("WARNING: Duplicate SeparateTimesig, did you miss out a NextScore?\n")
                notehead_markup.separateTimesig=1
                out.append(r"\override Staff.TimeSignature #'stencil = ##f")
            elif word in ["angka","Indonesian"]:
                global not_angka
                if not_angka: sys.stderr.write("WARNING: Duplicate angka, did you miss out a NextScore?\n")
                not_angka = True
            elif word=="WithStaff":
                if notehead_markup.withStaff: sys.stderr.write("WARNING: Duplicate WithStaff, did you miss out a NextScore?\n")
                notehead_markup.withStaff=1
            elif word=="R{":
                repeatStack.append((1,0,0))
                out.append(r'\repeat volta 2 {')
            elif re.match("R[1-9][0-9]*{$",word):
                times = int(word[1:-1])
                repeatStack.append((1,notehead_markup.barPos,times-1))
                out.append(r'\repeat percent %d {' % times)
            elif word=="}":
                numBraces,oldBarPos,multiplier = repeatStack.pop()
                out.append("}"*numBraces)
                # Re-synchronise so bar check still works if percent is less than a bar:
                newBarPos = notehead_markup.barPos
                while newBarPos < oldBarPos: newBarPos += notehead_markup.barLength
                # newBarPos-oldBarPos now gives the remainder (mod barLength) of the percent section's length
                notehead_markup.barPos = (notehead_markup.barPos + (newBarPos-oldBarPos)*multiplier) % notehead_markup.barLength
                # TODO: update barNo also (but it's used only for error reports)
            elif word=="A{":
                repeatStack.append((2,0,0))
                out.append(r'\alternative { {')
            elif word=="|":
                if not (repeatStack and repeatStack[-1][0]==2):
                    sys.stderr.write("| should be in an A{ .. } block (scoreNo=%d barNo=%d)\n" % (scoreNo,notehead_markup.barNo))
                out.append("} {")
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
            elif re.match(r"g\[[#b',1-9]+\]$",word):
                if midi or western: out.append(r"\grace { " + gracenotes_western(word[2:-1]) + " }")
                else:
                    aftrnext = graceNotes_markup(word[2:-1],0)
                    if not notehead_markup.withStaff: out.append(r"\once \textLengthOn ")
                    if not defined_jianpuGrace:
                        defined_jianpuGrace = True
                        out.append(r"""#(define-markup-command (jianpu-grace layout props text)
(markup?) "Draw right-pointing jianpu grace under text."
(let ((textWidth (cdr (ly:stencil-extent (interpret-markup layout props (markup (#:fontsize -4 text))) 0))))
(interpret-markup layout props
(markup
  #:line
  (#:right-align
   (#:override
    (cons (quote baseline-skip) 0.2)
    (#:column
     (#:line
      (#:fontsize -4 text)
      #:line
      (#:pad-to-box
       (cons -0.1 0)  ; X padding before grace
       (cons -1.6 0)  ; affects height of grace
       (#:path
        0.1
        (list (list (quote moveto) 0 0)
              (list (quote lineto) textWidth 0)
              (list (quote moveto) 0 -0.3)
              (list (quote lineto) textWidth -0.3)
              (list (quote moveto) (* textWidth 0.5) -0.3)
              (list (quote curveto) (* textWidth 0.5) -1 (* textWidth 0.5) -1 textWidth -1)))))))))))) """)
            elif re.match(r"\[[#b',1-9]+\]g$",word):
                if midi or western: out[lastPtr] = r" \afterGrace { " + out[lastPtr] + " } { " + gracenotes_western(word[1:-2]) + " }"
                else:
                    if not notehead_markup.withStaff:
                        out[lastPtr] = r"\once \textLengthOn " + out[lastPtr]
                    out.insert(lastPtr+1,graceNotes_markup(word[1:-2],1))
                    if not defined_JGR:
                        defined_JGR = True
                        out[lastPtr] = r"""#(define-markup-command (jianpu-grace-after layout props text)
(markup?) "Draw left-pointing jianpu grace under text."
(let ((textWidth (cdr (ly:stencil-extent (interpret-markup layout props (markup (#:fontsize -4 text))) 0))))
(interpret-markup layout props
(markup
  #:line
  (#:halign -4
   (#:override
    (cons (quote baseline-skip) 0.2)
    (#:column
     (#:line
      (#:fontsize -4 text)
      #:line
      (#:pad-to-box (cons 0 0)
       (cons -1.6 0)  ; affects height of grace
      (#:path
       0.1
       (list (list (quote moveto) 0 0)
             (list (quote lineto) textWidth 0)
             (list (quote moveto) 0 -0.3)
             (list (quote lineto) textWidth -0.3)
             (list (quote moveto) (* textWidth 0.5) -0.3)
             (list (quote curveto) (* textWidth 0.5) -1 (* textWidth 0.5) -1 0 -1)))))))))))) """ + out[lastPtr]
            elif word=="Fine":
                need_final_barline = 0
                out.append(r'''\once \override Score.RehearsalMark #'break-visibility = #begin-of-line-invisible \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT \mark "Fine" \bar "|."''')
            elif word=="DC":
                need_final_barline = 0
                out.append(r'''\once \override Score.RehearsalMark #'break-visibility = #begin-of-line-invisible \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT \mark "D.C. al Fine" \bar "||"''')
            else: # note (or unrecognised)
                figures,nBeams,dot,octave,accidental = parseNote(word)
                if figures:
                    need_final_barline = 1
                    b4last,aftrlast,this,need_space_for_accidental = notehead_markup(figures,nBeams,dot,octave,accidental)
                    if b4last: out[lastPtr]=b4last+out[lastPtr]
                    if aftrlast: out.insert(lastPtr+1,aftrlast)
                    lastPtr = len(out)
                    out.append(this)
                    if aftrnext:
                        if need_space_for_accidental: aftrnext = aftrnext.replace(r"\markup",r"\markup \halign #2 ",1)
                        out.append(aftrnext)
                        aftrnext = None
                    if not_angka and "'" in octave: maxBeams=max(maxBeams,len(octave)*.8+nBeams)
                    else: maxBeams=max(maxBeams,nBeams)
                else:
                    if len(word)>60: word=word[:50]+"..."
                    msg = "Unrecognised command %s in score %d" % (word,scoreNo)
                    if len(line)>600: line=line[:500]+"..."
                    if not word in line: pass # above truncations caused problems
                    elif "xterm" in os.environ.get("TERM",""): msg += "\n"+re.sub(r"(\s|^)"+re.escape(word)+r"(?=\s|$)",lambda m:m.group()[:1]+"\x1b[4m"+m.group()[1:]+"\x1b[m",line)
                    elif re.match('[ -~]*$',line): # all ASCII: we can underline the word with ^^s
                        msg += "\n"+line+"\n"+re.sub('[^^]',' ',re.sub(r"(\s|^)"+re.escape(word)+r"(?=\s|$)",lambda m:' '+'^'*(len(m.group())-1),line))
                    else: # don't try to underline the word (at least not without ANSI): don't know how the terminal will handle character widths
                        msg += "\nin this line: "+line
                    errExit(msg)
   if notehead_markup.barPos == 0 and notehead_markup.barNo == 1: errExit("No jianpu in score %d" % scoreNo)
   if notehead_markup.inBeamGroup and not midi and not western and not notehead_markup.inBeamGroup=="restHack": out[lastPtr] += ']' # needed if ending on an incomplete beat
   if inTranspose: out.append("}")
   if repeatStack: errExit("Unterminated repeat in score %d" % scoreNo)
   if escaping: errExit("Unterminated LP: in score %d" % scoreNo)
   notehead_markup.endScore() # perform checks
   if need_final_barline and not midi: out.append(r'\bar "|."')
   i=0
   while i < len(out)-1:
       while i<len(out)-1 and out[i].startswith(r'\mark \markup{') and out[i].endswith('}') and out[i+1].startswith(r'\mark \markup{') and out[i+1].endswith('}'):
           # merge time/key signatures
           nbsp = unichr(0xA0)
           if not type(u"")==type(""): # Python 2
               nbsp = nbsp.encode('utf-8')
           out[i]=out[i][:-1]+nbsp+' '+out[i+1][len(r'\mark \markup{'):]
           del out[i+1]
       i += 1
   for i in xrange(len(out)-1):
       if not out[i].endswith('\n'):
           if '\n' in out[i] or len(out[i])>60:
               out[i] += '\n'
           else: out[i]+=' '
   out = ''.join(out)
   if western: # collapse tied notes into longer notes
       out = re.sub(r"(?P<note>[^ ]*)4 +~ (?P=note)4 +~ (?P=note)4 +~ (?P=note)4",r"\g<1>1",out)
       out = re.sub(r"(?P<note>[^ ]*)4 +~ (?P=note)4 +~ (?P=note)4",r"\g<1>2.",out)
       out = re.sub(r"(?P<note>[^ ]*)4 +~ (?P=note)4",r"\g<1>2",out)
       out = out.replace("r4 r4 r4 r4","r1").replace("r4 r4 r4","r2.").replace("r4 r4","r2")
       out = re.sub(r"(%\{ bar [0-9]*: %\} )r([^ ]* \\bar)",r"\g<1>R\g<2>",out)
       out = out.replace(r"\new RhythmicStaff \with {",r"\new RhythmicStaff \with { \override VerticalAxisGroup.default-staff-staff-spacing = #'((basic-distance . 6) (minimum-distance . 6) (stretchability . 0)) ") # don't let it hang too far up in the air
   if not_angka: out=out.replace("make-bold-markup","make-simple-markup")
   return out,maxBeams,lyrics,headers

def process_input(inDat):
 ret = []
 global scoreNo, western, has_lyrics, midi, not_angka, maxBeams
 scoreNo = 0 # incr'd to 1 below
 western = False
 for score in re.split(r"\sNextScore\s"," "+inDat+" "):
  if not score.strip(): continue
  scoreNo += 1
  wordSet = set(score.split())
  has_lyrics = "L:" in wordSet or "H:" in wordSet # the occasional false positive doesn't matter: has_lyrics==False is only an optimisation
  for midi in [0,1]:
   not_angka = False # may be set by getLY
   out,maxBeams,lyrics,headers = getLY(score)
   if notehead_markup.withStaff and notehead_markup.separateTimesig: errExit("Use of both WithStaff and SeparateTimesig in the same piece is not yet implemented")
   if scoreNo==1 and not midi: ret.append(all_scores_start()) # not before here, so as not to confuse beginners who don't input a valid score 1
   ret.append(score_start())
   if midi:
       ret.append(midi_staff_start()+" "+out+" "+midi_staff_end())
   else:
       ret.append(jianpu_staff_start()+" "+out+" "+jianpu_staff_end())
       if notehead_markup.withStaff:
           western=True ; ret.append(western_staff_start()+" "+getLY(score)[0]+" "+western_staff_end()) ; western = False
           lyrics = lyrics.replace(r'\lyricsto "jianpu"',r'\lyricsto "5line"')
       if lyrics: ret.append(lyrics)
   ret.append(score_end(**headers))
 return "".join(r+"\n" for r in ret)

def main():
    if "--html" in sys.argv or "--markdown" in sys.argv:
        return write_docs()
    inDat = get_input()
    out = process_input(inDat) # <-- you can also call this if importing as a module
    print (out)

if __name__=="__main__": main()
