#!/usr/bin/env python

# Jianpu (numbered musical notaion) for Lilypond
# v1.157 (c) 2012-2018 Silas S. Brown

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be ueful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# Homepage: http://people.ds.cam.ac.uk/ssb22/mwrhome/jianpu-ly.py
# Git repository: https://github.com/ssb22/jianpu-ly
# or on GitLab: https://gitlab.com/ssb22/jianpu-ly
# or Bitbucket: https://bitbucket.org/ssb22/jianpu-ly

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
Lyrics: L: here are the syl- la- bles (all on one line)
Lyrics (verse 1): L: 1. Here is verse one
Lyrics (verse 2): L: 2. Here is verse two
Hanzi lyrics (auto space): H: hanzi (with or without spaces)
Lilypond headers: title=the title (on a line of its own)
Multiple movements: NextScore
Prohibit page breaks until end of this movement: OnePage
Add a Western staff doubling the jianpu: WithStaff
Tuplets: 3[ q1 q1 q1 ]
Da capo: 1 1 Fine 1 1 1 1 1 1 DC
Repeat (with alternate endings): R{ 1 1 1 } A{ 2 | 3 }
Short repeats (percent): R4{ 1 2 }
Ties (like Lilypond's, if you don't want dashes): 1 ~ 1
Slurs (like Lilypond's): 1 ( 2 )
Dynamics (applies to previous note): \p \mp \f
Other 1-word Lilypond \ commands: \fermata \> \! \( \) etc
Other Lilypond code: LP: (block of code) :LP (each delimeter at start of its line)
"""

import sys,os,re,string
if not sys.version_info[0]==2:
    sys.stderr.write("Sorry, jianpu-ly cannot run on Python "+repr(sys.version_info[0])+"\nPlease use Python 2.x\n")
    sys.exit(1)

def all_scores_start(staff_size = 20):
    # staff_size is the 5-line size in points; jianpu is smaller
    return r"""\version "2.12.2"
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
    if not midi: ret += ("\\override Score.BarNumber #'break-visibility = #end-of-line-invisible\n\\set Score.barNumberVisibility = #(every-nth-bar-number-visible %d)" % bar_number_every)
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
    if midi: ret += r"\midi { \context { \Score tempoWholesPerMinute = #(ly:make-moment 84 4)}}" # TODO: make this customisable (and/or check how to print BPMs in jianpu)
    else: ret += r"\layout{}"
    return ret + " }"

tempCount = 0
def jianpu_voice_start(voiceName="tmp"):
    if voiceName=="tmp": # make it unique just in case
        global tempCount
        voiceName += str(tempCount) ; tempCount += 1
    if maxBeams >= 2: stemLenFrac = "0.5" # sometimes needed if the semiquavers occur in isolation rather than in groups (TODO do we need to increase this for 3+ beams in some cases?)
    else: stemLenFrac = "0"
    return r"""\new Voice="%s" {
    \override Beam #'transparent = ##f %% (needed for LilyPond 2.18 or the above switch will also hide beams)
    \override Stem #'direction = #DOWN
    \override Stem #'length-fraction = #%s
    \override Beam #'beam-thickness = #0.1
    \override Beam #'length-fraction = #0.5
    \override Voice.Rest #'style = #'neomensural %% this size tends to line up better (we'll override the appearance anyway)
    \override Accidental #'font-size = #-4
    \override Tie #'staff-position = #2.5
    \override TupletBracket #'bracket-visibility = ##t
    \tupletUp
""" % (voiceName,stemLenFrac)
def jianpu_staff_start(voiceName="jianpu"):
    # (we add "BEGIN JIANPU STAFF" and "END JIANPU STAFF" comments to make it easier to copy/paste into other Lilypond files)
    return r"""
%% === BEGIN JIANPU STAFF ===
    \new RhythmicStaff \with {
    \consists "Accidental_engraver"
    %% Get rid of the stave but not the barlines.
    %% This changes between Lilypond versions.
    %% \remove Staff_symbol_engraver %% worked pre-2.18, but 2.18 results in missing barlines (adding Barline_engraver won't help). Do this instead:
    \override StaffSymbol #'line-count = #0 %% tested in 2.15.40, 2.16.2, 2.18.0 and 2.18.2
    \override BarLine #'bar-extent = #'(-2 . 2) %% LilyPond 2.18: please make barlines as high as the time signature even though we're on a RhythmicStaff (2.16 and 2.15 don't need this although its presence doesn't hurt; Issue 3685 seems to indicate they'll fix it post-2.18)
    }
    { """+jianpu_voice_start(voiceName)+r"""
    \override Staff.TimeSignature #'style = #'numbered
    \override Staff.Stem #'transparent = ##t
    
    \set Voice.chordChanges = ##t
    % This is to work around a bug in LilyPond 2.19.82.
    % \applyOutput docs say "called for every layout object
    % found in the context Context at the current time step"
    % but 2.19.x breaks this by calling it for ALL contexts
    % in the current time step, hence breaking our WithStaff
    % by applying our jianpu numbers to the 5-line staff too.
    % Obvious workaround is to make our function check that
    % the context it's called with matches our jianpu voice,
    % but I'm not sure how to do this other than by setting a
    % property that's not otherwise used, which we can test
    % for in the function.  So I'm 'commandeering' the
    % "chordChanges" property (there since at least 2.15 and
    % used by Lilypond only when it's in chord mode, which we
    % don't use, and if someone adds a chord-mode staff then
    % it won't print noteheads anyway): we will substitute
    % jianpu numbers for noteheads only if chordChanges = #t.
    
    """
def jianpu_staff_end(): return "} }\n% === END JIANPU STAFF ===\n" # \bar "|." is added separately if there's not a DC etc
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
    return r'\new Lyrics = "I%s" { \lyricsto "%s" { ' % (str(lyricsPtr).translate((string.letters*5)[:256]),voiceName)
def lyrics_end(): return "} }"

dashes_as_ties = True # Implement dash (-) continuations as invisible ties rather than rests; sometimes works better in awkward beaming situations
use_rest_hack = True # Implement short rests as notes (and if there are lyrics, creates temporary voices so the lyrics miss them); sometimes works better for beaming (at least in 2.15, 2.16 and 2.18)
assert not (use_rest_hack and not dashes_as_ties), "This combination has not been tested"

def errExit(msg): sys.stderr.write("ERROR: "+msg+"\n"),sys.exit(1)

class notehead_markup:
  def __init__(self):
      self.defines_done = {} ; self.initOneScore()
  def initOneScore(self):
      self.barLength = 64 ; self.beatLength = 16 # in 64th notes
      self.barPos = self.startBarPos = self.inBeamGroup = self.lastNBeams = self.onePage = self.withStaff = 0
      self.current_accidentals = {}
      self.barNo = 1
      self.tuplet = (1,1)
      self.last_figure = None
      self.notesHad = []
  def endScore(self):
      if not self.barPos == self.startBarPos: errExit("Incomplete bar at end of score %d (pos %d, should be %d)" % (scoreNo,self.barPos,self.startBarPos))
  def setTime(self,num,denom):
      self.barLength = 64*num/denom
      if denom>4 and num%3==0: self.beatLength = 24 # compound time
      else: self.beatLength = 16
  def setAnac(self,denom,dotted):
      self.barPos = self.barLength-64/denom
      if dotted: self.barPos -= 64/denom/2
      if not self.barPos: errExit("Anacrusis should be shorter than bar in score %d" % scoreNo)
      self.startBarPos = self.barPos
  def __call__(self,figure,nBeams,dot,octave,accidental):
    # figure is '1'-'7' or '0' or '-'
    # nBeams is 0, 1, 2 .. etc (number of beams for this note)
    # dot is "" or "." (dotted length)
    # octave is "", "'", "''", "," or ",,"
    # accidental is "", "#", "b"
    self.notesHad.append(figure)
    names = {'0':'nought',
             '1':'one',
             '2':'two',
             '3':'three',
             '4':'four',
             '5':'five',
             '6':'six',
             '7':'seven',
             '-':'dash'}
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
    placeholder_note = placeholders[figure]
    invisTieLast = dashes_as_ties and self.last_figure and figure=="-"
    name = names[figure]
    if invisTieLast:
        figure += self.last_figure
        name += names[self.last_figure]
        placeholder_note = placeholders[self.last_figure]
        octave = self.last_octave # for MIDI or 5-line
        accidental = self.last_accidental # ditto
    self.last_figure = figure[-1]
    self.last_octave = octave
    self.last_accidental = accidental
    if figure not in self.defines_done and not midi and not western:
        # Define a notehead graphical object for the figure
        self.defines_done[figure] = "note-"+name
        if figure.startswith("-"):
            figure2=u"\u2013".encode('utf-8')
        else: figure2 = figure
        ret = """#(define (%s grob grob-origin context)
  (if (and (eq? (ly:context-property context 'chordChanges) #t)
      (or (grob::has-interface grob 'note-head-interface)
        (grob::has-interface grob 'rest-interface)))
    (begin
      (ly:grob-set-property! grob 'stencil
        (grob-interpret-markup grob
          (make-lower-markup 0.5 (make-bold-markup "%s")))))))
""" % (self.defines_done[figure],figure2)
        # TODO: chords?  Could have something like
        # (make-bold-markup (make-center-column-markup '("1" "3")))
        # but would then need to take over accidentals from
        # Lilypond, + make sure all notes aligned correctly
        # (+ Tie's overriden position may need adjusting)
    else: ret = ""
    if self.barPos==0 and self.barNo > 1:
        ret += "| " # barline in Lilypond file: not strictly necessary but may help readability
        if self.onePage and not midi: ret += r"\noPageBreak "
    if not octave in self.current_accidentals: self.current_accidentals[octave] = [""]*7
    if figure=="-" or ('1'<=figure<='7' and not accidental==self.current_accidentals[octave][int(figure)-1]) and nBeams > self.lastNBeams: leftBeams = nBeams # beam needs to fit under the new accidental (or the dash which might be slightly to the left of where digits are), but if it's no more than last note's beams then we'll hang it only if in same beat.  (TODO: the current_accidentals logic may need revising if other accidental styles are used, e.g. modern-cautionary, although then would need to check anyway if our \consists "Accidental_engraver" is sufficient)
    # TODO: if figure=="0" then that might be typeset a bit to the left as well (because it's also a rest), however extending the line TOO far left in this case could be counterproductive
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
    if nBeams and not midi and not western: # must set these unconditionally regardless of what we think their current values are (Lilypond's own beamer can change them from note to note)
        # TODO: is there any version of Lilypond that will need this lot done even if leftBeams==nBeams==0 ?
        ret += (r"\set stemLeftBeamCount = #%d"+"\n") % leftBeams
        ret += (r"\set stemRightBeamCount = #%d"+"\n") % nBeams
    if '1'<=figure<='7': self.current_accidentals[octave][int(figure)-1] = accidental
    inRestHack = 0
    if not midi and not western:
        if ret: ret = ret.rstrip()+"\n" # try to keep the .ly code vaguely readable
        ret += r"  \applyOutput #'Voice #"+self.defines_done[figure]+" "
        if placeholder_note == "r" and use_rest_hack and nBeams:
            placeholder_note = "c"
            # C to work around diagonal-tail problem with
            # some isolated quaver rests in some Lilypond
            # versions (usually at end of bar); new voice
            # so lyrics miss it as if it were a rest:
            if has_lyrics and not self.withStaff: # (OK if self.withStaff: lyrics will be attached to that instead)
                ret = jianpu_voice_start() + ret
                inRestHack = 1
                if self.inBeamGroup and not self.inBeamGroup=="restHack": aftrlast0 = "] "
    ret += placeholder_note
    ret += {"":"", "#":"is", "b":"es"}[accidental]
    if not placeholder_note=="r": ret += {"":"'","'":"''","''":"'''",",":"",",,":","}[octave] # for MIDI + Western, put it so no-mark starts near middle C
    length = 4 ; b = 0 ; toAdd = 16 # crotchet
    while b < nBeams: b,length,toAdd = b+1,length*2,toAdd/2
    if dot: toAdd += toAdd/2
    ret += ("%d" % length) + dot
    if nBeams and (not self.inBeamGroup or self.inBeamGroup=="restHack" or inRestHack) and not midi and not western:
        # We need the above stemLeftBeamCount, stemRightBeamCount override logic to work even if we're an isolated quaver, so do this:
        ret += '['
        self.inBeamGroup = 1
    if not self.tuplet[0]==self.tuplet[1]:
        toAdd = 1.0*toAdd*self.tuplet[0]/self.tuplet[1] # and hope it rounds OK (otherwise should get barcheck fail)
    self.barPos += toAdd
    # sys.stderr.write(accidental+figure+octave+dot+"/"+str(nBeams)+"->"+str(self.barPos)+" ") # if need to see where we are
    if self.barPos > self.barLength: errExit("(notesHad=%s) barcheck fail: note crosses barline at \"%s\" with %d beams (%d skipped from %d to %d, bypassing %d), scoreNo=%d barNo=%d (but the error could be earlier)" % (' '.join(self.notesHad),figure,nBeams,toAdd,self.barPos-toAdd,self.barPos,self.barLength,scoreNo,self.barNo))
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
      ret += {"":"",
            "'":"^.",
            "''":r"-\tweak #'X-offset #0.3 ^\markup{\bold :}",
            ",":r"-\tweak #'X-offset #0.6 _.",
            ",,":r"-\tweak #'X-offset #0.3 _\markup{\bold :}"}[octave]
    if invisTieLast:
        if midi or western: b4last, aftrlast = "", " ~"
        else: b4last,aftrlast = r"\once \override Tie #'transparent = ##t \once \override Tie #'staff-position = #0 "," ~"
    else: b4last,aftrlast = "",""
    if inRestHack: ret += " } "
    return b4last,aftrlast0+aftrlast,ret

notehead_markup = notehead_markup()

def parseNote(word):
    figure = None
    for fig in list("01234567-"):
        if fig in word:
            figure = fig ; break
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
    if not figure and not "," in octave and not octave=="''":
        if "8" in word: # 8 = 1'  8' = 1''
            figure = "1" ; octave += "'"
        elif "9" in word: # 9 = 2'  9' = 2''
            figure = "2" ; octave += "'"
    accidental = ""
    for acc in ["#","b"]:
        if acc in word:
            accidental = acc ; break
    return figure,nBeams,dot,octave,accidental

if "--html" in sys.argv:
    # Write an HTML version of the doc string
    def htmlify(l): return l.replace("&","&amp;").replace("<","&lt;").replace(">","&gt;")
    inTable = 0 ; justStarted=1
    for line in __doc__.split("\n"):
        if not line.strip(): continue
        if ":" in line and line.split(":",1)[1].strip():
            toGet,shouldType = line.split(":",1)
            if not inTable:
                print "<table border>" # "<tr><th>To get:</th><th>Type:</th></tr>"
                inTable = 1
            print "<tr><td>"+toGet.strip()+"</td><td><kbd>"+shouldType.strip()+"</kbd></td>"
        else:
            if inTable: print "</table>"
            elif not justStarted: print "<br>"
            inTable=justStarted=0
            print htmlify(line)
    if inTable: print "</table>"
    raise SystemExit
inDat = []
for f in sys.argv[1:]:
    try: inDat.append(open(f).read())
    except: errExit("Unable to read file "+f)
if not inDat:
    if sys.stdin.isatty():
        sys.stderr.write(__doc__)
        raise SystemExit
    inDat=[sys.stdin.read()]

def fix_fullwidth(t):
    utext = t.decode('utf-8')
    r = []
    for c in utext:
        if 0xff01<=ord(c)<=0xff5e: r.append(unichr(ord(c)-0xfee0))
        elif c==unichr(0x201a): r.append(",") # sometimes used as comma (incorrectly)
        elif c==unichr(0xff61): r.append(".")
        else: r.append(c)
    return u"".join(r).encode('utf-8')

def intor0(w):
    try: return int(w)
    except: return 0

for i in xrange(len(inDat)):
    if inDat[i].startswith('\xef\xbb\xbf'):
        inDat[i] = inDat[i][3:]
    if inDat[i].startswith(r'\version'): errExit("jianpu-ly does not READ Lilypond code.\nPlease see the instructions.")

inDat = " NextScore ".join(inDat)

def getLY(score):
   lyrics = "" ; headers = {}
   notehead_markup.initOneScore()
   out = [] ; maxBeams = 0 ; need_final_barline = 0
   repeatStack = [] ; lastPtr = 0
   escaping = inTranspose = 0
   for line in score.split("\n"):
    line = fix_fullwidth(line).strip()
    if line.startswith("LP:"):
        # Escaped LilyPond block.  Thanks to James Harkins for this suggestion.
        # (Our internal barcheck does not understand code in LP blocks, so keep it to complete bars.)
        # E.g. for multibar rests:
        # LP:
        # \compressFullBarRests \override MultiMeasureRest #'expand-limit = #1
        # R1*5
        # :LP
        escaping = 1
        if len(line)>3: out.append(line[3:]) # remainder of current line
    elif line.startswith(":LP"):
        escaping = 0 # TODO: and process the rest of the line?  (assume on line of own for now)
    elif escaping:
        out.append(line)
    elif not line: pass
    elif line.startswith("L:") or line.startswith("H:"):
        # lyrics
        do_hanzi_spacing = line.startswith("H:")
        line = line[2:].strip()
        lyrics += lyrics_start()
        toAdd = ""
        if line and '1' <= line[0] <= '9' and (line[1]=='.' or line.decode('utf-8')[1]==u"\uff0e"):
            # a verse number
            toAdd = r'\set stanza = #"%s." ' % line[:1]
            if line[1]=='.': line=line[2:]
            else: line=line[4:] # for utf-8 full-width dot
            line = line.strip()
        if do_hanzi_spacing: # this is not 100% perfect...
            l2 = [r"\override LyricText #'self-alignment-X = #LEFT "] # for overhanging commas etc to work
            if toAdd:
                l2.append(toAdd) ; toAdd = ""
            needSpace = 0
            for c in list(line.decode('utf-8')):
                if needSpace and (0x4e00 <= ord(c) < 0xa700 or c in u"\u2018\u201c"):
                    l2.append(' ') ; needSpace = 0
                    if c in u"\u2018\u201c":
                        # we're just about to have an open quote - this needs to hang left.  Try:
                        l2.append(r"\once \override LyricText #'self-alignment-X = #CENTER ") # or RIGHT if there's no punctuation after
                if 0x4e00 <= ord(c) < 0xa700: needSpace=1
                l2.append(c)
            line = u"".join(l2).encode('utf-8')
        lyrics += toAdd+line.replace(" -- "," --\n")+" "+lyrics_end()+" "
    elif line.replace(' =','=').split()[0].find('=') >= 2:
        # not (e.g.) 1=C, so assume it's a Lilypond header
        hName,hValue = line.split("=",1)
        headers[hName.strip()] = hValue.strip()
    else:
        for word in line.split():
            if '=' in word: # e.g. 1=C; mark
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
            elif '/' in word: # time signature
                if ',' in word: # anacrusis
                    word,anac = word.split(",",1)
                else: anac=""
                out.append(r'\time '+word)
                num,denom = word.split('/')
                notehead_markup.setTime(int(num),int(denom))
                if anac:
                    if anac.endswith("."): # e.g. 2.
                        a2 = anac[:-1] ; anacDotted = 1
                    else: a2,anacDotted = anac,0
                    notehead_markup.setAnac(int(a2),anacDotted)
                    out.append(r'\partial '+anac)
            elif word.startswith("\\") or word in ["~","(",")"]:
                out.append(word) # Lilypond command, \p etc
            elif word=="OnePage":
                if notehead_markup.onePage: sys.stderr.write("WARNING: Duplicate OnePage, did you miss out a NextScore?\n")
                notehead_markup.onePage=1
            elif word=="WithStaff":
                if notehead_markup.withStaff: sys.stderr.write("WARNING: Duplicate WithStaff, did you miss out a NextScore?\n")
                notehead_markup.withStaff=1
            elif word=="R{":
                repeatStack.append((1,0,0))
                out.append(r'\repeat volta 2 {')
            elif word.startswith("R") and word.endswith("{"):
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
                    sys.stderr.write("| should be in an A{ .. } block (scoreNo=%d barNo=%d)" % (scoreNo,notehead_markup.barNo))
                out.append("} {")
            elif word.endswith('[') and intor0(word[:-1]):
                # tuplet start, e.g. 3[
                fitIn = int(word[:-1])
                i=2
                while i<fitIn: i*=2
                if i==fitIn: num=fitIn*3/2
                else: num=i/2
                out.append("\\times %d/%d {" % (num,fitIn))
                notehead_markup.tuplet = (num,fitIn)
            elif word==']': # tuplet end
                out.append("}")
                notehead_markup.tuplet = (1,1)
            elif word=="Fine":
                need_final_barline = 0
                out.append(r'''\once \override Score.RehearsalMark #'break-visibility = #begin-of-line-invisible \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT \mark "Fine" \bar "|."''')
            elif word=="DC":
                need_final_barline = 0
                out.append(r'''\once \override Score.RehearsalMark #'break-visibility = #begin-of-line-invisible \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT \mark "D.C. al Fine" \bar "||"''')
            else:
                figure,nBeams,dot,octave,accidental = parseNote(word)
                if figure:
                    need_final_barline = 1
                    b4last,aftrlast,this = notehead_markup(figure,nBeams,dot,octave,accidental)
                    if b4last: out[lastPtr]=b4last+out[lastPtr]
                    if aftrlast: out[lastPtr] += aftrlast
                    lastPtr = len(out)
                    out.append(this)
                    if nBeams > maxBeams: maxBeams = nBeams
                else: errExit("Unrecognised command "+word+" in score "+str(scoreNo))
   if notehead_markup.barPos == 0 and notehead_markup.barNo == 1: errExit("No jianpu in score %d" % scoreNo)
   if notehead_markup.inBeamGroup and not midi and not western and not notehead_markup.inBeamGroup=="restHack": out[lastPtr] += ']' # needed if ending on an incomplete beat
   if inTranspose: out.append("}")
   if repeatStack: errExit("Unterminated repeat in score %d" % scoreNo)
   if escaping: errExit("Unterminated LP: in score %d" % scoreNo)
   notehead_markup.endScore() # perform checks
   if need_final_barline and not midi: out.append(r'\bar "|."')
   if midi or western: out = ' '.join(out)
   else: out = '\n'.join(out)
   if western: # collapse tied notes into longer notes
       out = re.sub(r"(?P<note>[^ ]*)4 ~ (?P=note)4 ~ (?P=note)4 ~ (?P=note)4",r"\g<1>1",out)
       out = re.sub(r"(?P<note>[^ ]*)4 ~ (?P=note)4 ~ (?P=note)4",r"\g<1>2.",out)
       out = re.sub(r"(?P<note>[^ ]*)4 ~ (?P=note)4",r"\g<1>2",out)
       out = out.replace(r"\new RhythmicStaff \with {",r"\new RhythmicStaff \with { \override VerticalAxisGroup.default-staff-staff-spacing = #'((basic-distance . 6) (minimum-distance . 6) (stretchability . 0)) ") # don't let it hang too far up in the air
   return out,maxBeams,lyrics,headers

print all_scores_start() ; scoreNo = 0 # incr'd to 1 below
western = False
for score in re.split(r"\sNextScore\s"," "+inDat+" "):
  if not score.strip(): continue
  scoreNo += 1
  wordSet = set(score.split())
  has_lyrics = "L:" in wordSet or "H:" in wordSet # the occasional false positive doesn't matter: has_lyrics==False is only an optimisation
  for midi in [0,1]:
   print score_start()
   out,maxBeams,lyrics,headers = getLY(score)
   if midi:
       print midi_staff_start(),out,midi_staff_end()
   else:
       print jianpu_staff_start(),out,jianpu_staff_end()
       if notehead_markup.withStaff:
           western=True ; print western_staff_start(),getLY(score)[0],western_staff_end() ; western = False
           lyrics = lyrics.replace(r'\lyricsto "jianpu"',r'\lyricsto "5line"')
       if lyrics: print lyrics
   print score_end(**headers)
