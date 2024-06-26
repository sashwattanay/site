---
title: Integrability and action-angle variables of binary black holes
permalink: /2023/04/13/pn-aa-variables
categories:
- Research
- Lecture notes
- Research papers
- Mathematica
- Software
- Computation
---



Two of the problems I worked on during my Ph.D. with 
[Leo Stein](https://duetosymmetry.com/) led to


1. discovery of two new constants of motion for the 2nd 
post-Newtonian (PN) binary black hole (BBH) systems.
[This](https://arxiv.org/abs/2012.06586) is the paper.


2. action-angle (AA) based closed-form solutins to the 
orbital dynamics of the 1.5PN BBH system. The relevant
papers are [this](https://arxiv.org/abs/2012.06586),
[this](https://arxiv.org/abs/2110.15351), and
[this](https://arxiv.org/abs/2210.01605).



For a set of lectures delivered to 
[Prof. Nicolas Yunes'](https://physics.illinois.edu/people/directory/profile/nyunes)
 group at the University of Illinois
Urbana-Champaign, I prepared 
[these](https://github.com/sashwattanay/lectures_integrability_action-angles_PN_BBH/blob/gh-action-result/pdflatex/lecture_notes/main.pdf)
set of lecture notes. To cite these, please use the 
[arXiv version](https://arxiv.org/abs/2206.05799).
NOTE: these notes do not re-present the material of my above 
papers on AA-based solution. They instead are meant to bring a
beginning graduate student up to speed so that they can read these
papers. These lecture notes assume the familiarity with the standard
graduate level physics courses on the reader's part.



[This](https://github.com/sashwattanay/BBH-PN-Toolkit)
GitHub repo contains a Mathematica package which implements
the above AA-based solution (among other things).



As a by-product, 
[here]({{ site.url }}/assets/2023/2023-04-13-Poisson_bracket_notebook.nb)
is a Mathematica notebook (authored by 
[Leo Stein](https://duetosymmetry.com/)), 
which lets you compute the Poisson brackets (PBs)
between any two functions of the phase-space variables
(position, momenta, and spins). The fundamental 
PB relations that this notebook
 assumes are Eqs. (6) of [this paper](https://arxiv.org/abs/2012.06586).
 You must have [xAct](http://www.xact.es/)
 installed on your computer for this notebook to work.
 
 
Additionally, 
[this](https://github.com/sashwattanay/EccentricIMR) 
Mathematica package generates the orbital solution as well as
gravitational waves for non-spinning binary black hole systems
up to 4PN (3PN) accuracy in the conservative (radiative) sector.




