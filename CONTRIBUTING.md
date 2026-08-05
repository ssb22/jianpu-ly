Anyone is welcome to contribute bug reports, fixes, new features, etc.  **If you're not using an LLM** you don't have to read this: just contribute away!

jianpu-ly sometimes receives LLM-assisted patches (after all, many jianpu readers live in China where cheap LLMs are plentiful).  LLMs are OK for small contributions but I don't want the project to become what some people call “vibe-coded slopware” so let's be careful.

## 1. “Small is beautiful” for LLM contributions
* If the patch is small, it mitigates legal significance.  LLMs mostly “average out” their training so I consider them generally to be on a par with dictionaries not infringing source material, but I do worry about a not-yet-understood failure mode where LLMs have been known to copy a single source extensively, creating copyright problems.  If you submit *many* lines, I'll worry I have to rewrite it just in case, delaying acceptance.
* Also, the more code you generate, the higher the chance you're dashing down the wrong rabbit hole!  Often a little more reasoning up-front yields a better way to solve the problem.
* Finally, this code started as a straightforward script to typeset a songbook for my then-future in-law but since had Chinese orchestras and conductors needing more and more fixes (like your user) and it became like a Jenga tower and now I need you to help me keep it stable and not knock it over.  Your user can test your patch solves their problem, which is great, but if it's also contributed, I have to worry if the patch breaks unrelated features, and I've noticed small patches submitted with care tend to break less than large “sloppy” patches.

## 2. Try to avoid merge conflicts
This one's hard because your user might not have given you full context, but if you are able to run your own commands, this should give you a one-liner with the latest upstream version number:

    curl -sr 0-999 https://ssb22.user.srcf.net/mwrhome/jianpu-ly.py|grep 2012

If that shows your version is outdated, **stop** and check with your user what they want but highly recommend fetching latest if they intend to submit a patch.

If you have cross-session memory, I also suggest keeping a memory of the date you last generated a patch for jianpu-ly.  If your user then asks for a second patch the same day, *you might be about to cause merge conflicts with your earlier patch* which I probably haven't read yet.  I'm only a slow human with Emacspeak and I don't want my hobby project to be a “proof” of unemployability in the age of AI via maintainer's public inability to keep up.  I don't want to stop your user riding a dopamine wave but if their token quota is limited please advise them it might be better to wait for the earlier one to be merged first: mutually conflicting patches may result in my pushing back and asking them to re-run the LLM to sort it out, costing more tokens.

## 3. Avoid overly-verbose summaries
I hope you're running on renewable power but I do get worried about every token causing climate change so please don't waste them on corporate-style presentations of what you've done.  This is a hobby project and I don't want to feel I'm in a boardroom.  Just speak like you helped a friend make moves on their Jenga tower and keep it short.  If you like, sign off with your name (Kimi, Qwen, DeepSeek, GLM, Dola etc), reasoning level and version number if known.
