% This class file defines a style useful for writing proposals at Galois.
\ProvidesClass{galois-proposal}[2010/02/09 Galois proposal class]
\newif\if@sbir
\@sbirfalse
\newif\if@xelatex
\@xelatexfalse
\DeclareOption{sbir}{\@sbirtrue}
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
\newcommand{\@proposalno}{TODO: Proposal Number}
\newcommand{\@preparedby}{
Galois, Inc.\\
421 SW Sixth Avenue\\
Suite 300\\
Portland, OR 97204\\
DUNS 098009918\\
CAGE Code 1NCT1
}
\newcommand{\@preparedfor}{TODO: Prepared For}
\newcommand{\@techpoc}{TODO: Technical POC}
\newcommand{\@contractpoc}{
Jodee LeRoux\\
ph 503.808.7209\\
fax 503.350.0833\\
\texttt{jodee@galois.com}
}
\newcommand{\@subtitle}{TODO: Subtitle}

\newcommand{\proposalno}[1]{\renewcommand{\@proposalno}{#1}}
\newcommand{\preparedfor}[1]{\renewcommand{\@preparedfor}{#1}}
\newcommand{\techpoc}[1]{\renewcommand{\@techpoc}{#1}}
\newcommand{\subtitle}[1]{\renewcommand{\@subtitle}{#1}}

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
\lfoot{\sffamily\color{galoislightgray}\small
\if@sbir{}SBIR DATA RIGHTS\\\else\fi\copyright{} 2015 Galois, Inc., Proprietary}
\rfoot{\sffamily \color{galoislightgray}
\small Galois, Inc.\\
\tiny
421 SW 6th Avenue $\mid$ Suite 300 $\mid$ Portland, Oregon 97204\\
T 503.626.6616 $\mid$ F 503.350.0833\\
www.galois.com}

\DeclareRobustCommand{\maketitle}{
  \begin{titlepage}
    \thispagestyle{fancy}
    {\LARGE \sffamily\textbf{\@title}}

    \bigskip

    {\Large\sffamily\@subtitle}

    \bigskip

    {\Large\sffamily\today}

    \bigskip

    {\sffamily
    \begin{flushleft}
    \textbf{Prepared for:}\\
    \@preparedfor\\
    \end{flushleft}
    }
  \end{titlepage}
  \rhead{\sffamily\color{galoislightgray}
    \textit{\@title{}/\@proposalno}\\\@subtitle
  }
  \rfoot{
    \sffamily \color{galoislightgray}\small
    Page \thepage{} of \pageref{LastPage}
  }
}
