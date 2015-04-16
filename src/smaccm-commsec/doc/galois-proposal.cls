\ProvidesClass{galois-proposal}[]
\newif\if@xelatex
\@xelatexfalse
\DeclareOption{xelatex}{\@xelatextrue}
\DeclareOption*{%
\PassOptionsToClass{\CurrentOption}{article}%
}
\ProcessOptions\relax
\LoadClass{article}

% Useful packages:
\RequirePackage{xcolor}
\RequirePackage{graphicx}
\RequirePackage[headheight=144pt,top=2in,left=1in,right=1in,bottom=1in]{geometry}
\RequirePackage{lastpage}

% Define fonts
\if@xelatex
\RequirePackage{fontspec}
\RequirePackage{xunicode}
\RequirePackage{xltxtra}
\setmainfont[Mapping=tex-text]{Palatino}
\setsansfont[Mapping=tex-text]{Arial}
\else
\RequirePackage[sc]{mathpazo}
\linespread{1.05}
\RequirePackage[scaled]{helvet}
\fi
\RequirePackage{sectsty}
\allsectionsfont{\sffamily}

% Misc defines
\newcommand{\url}[1]{\texttt{#1}}

\definecolor{galoislightgray}{gray}{0.6}

% Define header and footer
\RequirePackage{fancyhdr}
\pagestyle{fancy}
\fancyheadoffset[l]{1in}
\fancyheadoffset[r]{0.2in}
\fancyhead{}%clear
\fancyfoot{}%clear
\renewcommand{\headrulewidth}{0pt}
\renewcommand{\footrulewidth}{1pt}
\setlength{\parindent}{0pt}
\setlength{\parskip}{11pt} % TODO!

\lhead{\sffamily\hspace{24pt}\includegraphics{galois.png}}
\rhead{}
\rfoot{\sffamily \color{galoislightgray}
\small Galois, Inc.\\
\tiny
421 SW 6th Avenue $\mid$ Suite 300 $\mid$ Portland, Oregon 97204\\
T 503.626.6616 $\mid$ F 503.350.0833\\
www.galois.com}
