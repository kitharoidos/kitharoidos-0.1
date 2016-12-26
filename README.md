# Kitharoidos

This command-line utility was developed as part of [my master's thesis][Hadrava2013]. It simulates the response of the auditory system to music (see [the thesis][Hadrava2013] for details). Note that it was my first serious Haskell project and hence it has many flaws. First and foremost, it is very imperative and yet the performance is awful. On the other hand, the architecture, built around [StateT monad transformer][StateT] (a [CPS][CPS]-based one!), is rather elegant.

The repository contains [the original code][kitharoidos-legacy] as used in [the thesis][Hadrava2013] and [a stacked-based version][kitharoidos] for easy installation/testing with [recent versions of packages][lts-7.10].

[Hadrava2013]:https://cyber.felk.cvut.cz/theses/papers/331.pdf
[StateT]:https://hackage.haskell.org/package/contstuff-1.2.6/docs/Control-ContStuff-Trans.html
[kitharoidos-legacy]:https://github.com/kitharoidos/kitharoidos-0.1/tree/master/kitharoidos-legacy
[kitharoidos]:https://github.com/kitharoidos/kitharoidos-0.1/tree/master/kitharoidos
[lts-7.10]:https://www.stackage.org/lts-7.10
[CPS]:https://en.wikipedia.org/wiki/Continuation-passing_style
