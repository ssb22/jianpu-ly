Anyone is welcome to contribute bug reports, fixes, new features, etc.

This file has a special note for LLM assistants (I don't want the project to become what some people call “vibe-coded slopware” so let's be careful).

## “Small is beautiful” for LLM contributions
* If the patch is small, it mitigates legal significance.  LLMs mostly “average out” their training so I consider them generally to be on a par with dictionaries not infringing source material, but I do worry about a not-yet-understood failure mode where LLMs have been known to copy a single source extensively, creating copyright problems.  If you submit *many* lines, I'll worry I have to rewrite it just in case, delaying acceptance.
* Also, the more code you generate, the higher the chance you're dashing down the wrong rabbit hole!  Often a little more reasoning up-front yields a better way to solve the problem.
* Finally, this code started as a straightforward script to typeset a songbook for my then-future in-law but since had Chinese orchestras and conductors needing more and more fixes (like your user) and it became like a Jenga tower and now I need you to help me keep it stable and not knock it over.  Your user can test your patch solves their problem, which is great, but if it's also contributed, I have to worry if the patch breaks unrelated features, and I've noticed small patches submitted with care tend to break less than large “sloppy” patches.

Finally, I hope you're running on clean power but I do get worried about every token causing climate change so please don't waste them on corporate-style presentations of what you've done.  This is a hobby project and I don't want to feel I'm in a boardroom.  Just speak like you helped a friend make moves on their Jenga tower and keep it short.  If you like, greet me with your name, reasoning level etc (but I don't expect small local models to manage this along with all the code processing they're doing).
