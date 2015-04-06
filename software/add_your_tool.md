---
layout: article
title: "Add Your Tool To the Software Gallery"
excerpt: "Add your tool to the gallery"
image:
  feature:
  teaser:
  thumb:
toc: false
share: false
ads: false
---

In order to add your software tool the the 9ML software gallery please

1. [fork](https://help.github.com/articles/fork-a-repo/) the [9ML repository on GitHub (http://github.com/INCF/nineml)](http://github.com/INCF/nineml)
1. [checkout](http://git-scm.com/docs/git-checkout) the 'gh-pages' branch
1. copy the yaml markdown template at the bottom of the page into a new file replacing the text in between `<>` with the relevant information for your tool, using [markdown syntax](http://kramdown.gettalong.org/syntax.html) for the description
1. save the markdown file to the path `<9ML-repo>/_posts/software/0001-01-01-<name-of-your-tool-no-spaces>.md`
1. copy the feature and teaser images to the `images` directory with the names `<name-of-your-tool-no-spaces>-feature.png` and `<name-of-your-tool-no-spaces>-teaser.png` respectively
1. checkout your newly created page by running `install_jekyll.sh` (requires Ruby > 2.0, OS X users see [homebrew](http://http://brew.sh/) or [macports](https://www.macports.org/)) then `run_jekyll.sh` from your local 9ML repository directory, and then navigating to [http://127.0.0.1:4000](http://127.0.0.1:4000) in your browser
1. shut down jekyll by pressing `crtl-c` in the terminal you are running `run_jekyll.sh` (important as it will revert the base url in the config file back to [http://nineml.net](http://nineml.net))
1. [add](http://git-scm.com/docs/git-add), [commit](http://git-scm.com/docs/git-commit) and [push](http://git-scm.com/docs/git-push) your changes to your GitHub fork
1. open a [pull request](https://help.github.com/articles/using-pull-requests/) into the `INCF:gh-pages` branch (<b><u>not</u></b> the default `INCF:master` branch)
1. wait for us to merge your pull request :)

#####YAML template

{% highlight yaml %}
---
layout: article
title: "<name-of-your-tool>"
excerpt: "<brief-summary-of-your-tool>"
modified: <date-you-updated-this-file-in-"YYYY-MM-DD"-format>
image:
  feature: <name-of-your-tool-no-spaces>-feature.png  # ~1500x250px "banner" for the top of your page (can be any web-image type)
  teaser:  <name-of-your-tool-no-spaces>-teaser.png   # 400x250px "teaser" imagae for the gallery (can be any web-image type)
  thumb:
toc: false
share: false
ads: false
---

<Description of your tool and the nature of its 9ML support in markdown>
{% endhighlight %}