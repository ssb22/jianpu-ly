# Changes

## MusicXML Rehearsal Mark Support

### Problem
MusicXML files with `<rehearsal>` elements were not being converted properly to jianpu-ly format. Rehearsal marks like "A", "1", "6" in the source MusicXML were not appearing in the output.

### Solution
Added a new handler for `<rehearsal>` elements in `xml2jianpu()` function (lines 1255-1262):

```python
elif name=="rehearsal":
    if d0:
        paddingRestList.append("letter" + d0)
        for k,v in list(paddingRestDict.items()):
            if v==len(paddingRestList)-1: paddingRestDict[k] += 1
        for n,p in enumerate(partsInProgress):
            if positionsInProgress[n]==max(positionsInProgress):
                p.append("letter" + d0)
```

The handler:
- Extracts the rehearsal text content from `<rehearsal>` elements
- Converts them to jianpu-ly format (`letterX` where X matches regex `letter[A-Z0-9]+$`)
- Synchronizes across all instrument parts using `paddingRestList`/`paddingRestDict` pattern (same as key signature handling)
- Places the mark only on the part(s) at the current latest position

### Testing
- Created test files with single-character rehearsal marks ("A", "1") - handled correctly
- Created test files with multi-character rehearsal marks ("10") - handled correctly
- Verified conversion of `test.mxl` (complex file with multiple key signatures and rehearsal marks) - no longer crashes with KeyError

## Mark Merging Fix

### Problem
When key signature changes and rehearsal marks appeared in the same measure, the output contained an abnormal "?" character after the key signature (e.g., `\mark \markup{1=E\flat?  \box { "6" } }`).

### Root Cause
The mark merging code at lines 1945-1949 used a non-breaking space character (`nbsp = u'\u00a0'`) to separate merged markup commands. This character was displaying as "?" due to encoding issues when written to UTF-16 LE output files.

### Solution
Removed the non-breaking space character and replaced it with a regular space in the merge logic:

**Before:**
```python
if out[j].startswith(r'\mark \markup{') and out[j].endswith('}'):
    nbsp = u'\u00a0'
    if not type(u"")==type(""): # Python 2
        nbsp = nbsp.encode('utf-8')
    out[i]=out[i][:-1]+nbsp+' '+out[j][len(r'\mark \markup{'):]
```

**After:**
```python
if out[j].startswith(r'\mark \markup{') and out[j].endswith('}'):
    out[i]=out[i][:-1]+' '+out[j][len(r'\mark \markup{'):]
```

### Result
Output now correctly shows:
- `\mark \markup{1=B\flat  \box { "1" } }` - no "?" character
- `\mark \markup{1=E\flat  \box { "6" } }` - no "?" character
