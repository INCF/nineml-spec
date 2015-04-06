
Before editing the specification documents here, please read 
the following notes on using subversion and latex:

[http://en.wikibooks.org/wiki/LaTeX/Collaborative_Writing_of_LaTeX_Documents](http://en.wikibooks.org/wiki/LaTeX/Collaborative_Writing_of_LaTeX_Documents)

In short:

Directives for using LaTeX with version control systems

   1. Avoid 'ineffective' modifications.
   2. Do not change line breaks without good reason.
   3. Turn off automatic line wrapping of your LaTeX editor.
   4. Start each new sentence in a new line.
   5. Split long sentences into several lines so that each line 
      has at most 80 characters.
   6. Put only those files under version control that are directly 
      modified by the user.
   7. Verify that your code can be compiled flawlessly before committing 
      your modifications to the repository.
   8. Use Subversion's diff feature to critically review your modifications
      before committing them to the repository.
   9. Add a meaningful and descriptive comment when committing your 
      modifications to the repository.


How to build the pdf
====================

If running from Ubuntu linux or some other linux flavour it may not be enough to simply install the base distribution that comes with a latex editing environment you to compile this document you be required to install the texlive-full package.

```
$ sudo apt-get install texlive-full 
```

Build the pdf

```
$ cd latex
$ pdflatex NineMLSpec.tex
$ bibtex NineMLSpec
$ pdflatex NineMLSpec.tex
$ pdflatex NineMLSpec.tex
```
