============================================================================
                              Eps2pgf 0.7.0
============================================================================

Website: http://sourceforge.net/projects/eps2pgf
Author: Paul Wagenaars <paul@wagenaars.org>

----------------------------------------------------------------------------
Introduction
----------------------------------------------------------------------------
Eps2pgf is a PostScript interpreter that converts Encapsulated PostScript
(EPS) figures to the Portable Graphics Format (PGF)
<http://sourceforge.net/projects/pgf/>. PGF/TikZ is a TeX macro package for
generating graphics. It support several back-end drivers, including pdfTeX
and Dvips. The major advantage of Eps2pgf is that all texts are typeset by
LaTeX, giving you all the powerful typesetting features and a uniform look
of the final document. It has several options to control how text in figures
is handled: (i) reproduce text labels accurately (with same font size and
formatting as in EPS figure), (ii) copy text labels verbatim (text in EPS
figure is LaTeX code), or (iii) replace text labels using PSfrag-compatible
rules from a separate file, or using tags embedded in the text labels.

The goal of Eps2pgf is to support all PostScript figures created by programs
regularly used by LaTeX users to create figures, such as MATLAB, Mathematica
and Maple. If you encounter a figure that Eps2pgf fails to process, please
report it using the bug tracker
<http://sourceforge.net/tracker/?group_id=188852&atid=926973>, or send it
via email.


----------------------------------------------------------------------------
Usage
----------------------------------------------------------------------------
See the user manual doc\eps2pgf_manual.pdf, or run Eps2pgf with the command
line argument '--help'.


----------------------------------------------------------------------------
Building
----------------------------------------------------------------------------
Building Eps2pgf from source requires Apache Ant <http://ant.apache.org/>,
and a Java Development Kit (e.g. Sun's JDK <http://java.sun.com/>). To build
Eps2pgf run Ant from the base directory:

    ant <target>
    
Replace <target> by one of the following targets:
    - jar  - Builds the jar and prepares the distribution directory. After
             compilation the directory 'dist_root' contains the complete
             Eps2pgf distribution.
    - test - Run a test suite with small synthetic PostScript tests and the
             test figures found in the 'testsuite\test_figures' directory.
    - zip  - Prepares the distribution directory ('jar' target) and zips it.


----------------------------------------------------------------------------
Copyright and License
----------------------------------------------------------------------------
See the files NOTICE.txt and LICENSE.txt. Or run Eps2pgf with the command
line option '--version'.


----------------------------------------------------------------------------
Changelog
----------------------------------------------------------------------------
v0.7.0 (2008-08-24)
  - Added: feature #1856797 Bitmap images
  - Added: feature #1952604 Improve compilation speed
  - Added: support for CIEBasedA and CIEBasedABC color spaces
  - Fixed: feature #1917088 add a percent character at the each of each line
           in the LOLDevice output.
  - Fixed: bug #1886475 Undefined operator in Quartz EPS

v0.6.0 (2008-02-04)
  - Added: feature #1856800 PSfrag emulation
  - Added: feature #1856798 Interpret text labels directly as TeX
  - Added: feature #1856794 Indexed color spaces
  - Added: support for embedded (non-bitmapped) type 3 fonts
  - Added: output device (LOLDevice) that writes only the text labels
  - Added: user manual
  - Fixed: bug #1809102 (partially) Problems with Inkscape figure with text.
  - Fixed: bug #1807713 In some situations Eps2pgf produced lines that are
           too long. 
  - Fixed: bug #1874016 divison by zero in multiplication operator
  - Fixed: bug #1859820 Improve error and info messages presented to the user
  - Fixed: bug #1858099 assertValidFont is run multiple times on the same font
  - Fixed: bug #1859810 Example figure from MathPSfrag package
  - Changed: Lots of internal changes, making the design clearer, fixing style
             issues, etc...

v0.5.0 (2007-11-04)
  - Added: handling of binary headers found in some eps files
  - Added: support for embedded fonts
  - Added: special support for embedded Mathematica fonts (not yet complete)

v0.4.0 (2007-08-04)
  - Changed: Eps2pgf is now distributed under the Apache license
  - Added: lots of PostScript commands
  - Added: Font substitution list

v0.3.0 (2007-05-28)
  - Added: tons of PostScript commands
  - Added: automated test scripts

v0.2.0 (2007-04-06)
  - Added: added several PostScript commands
  - Added: radial shadings 

v0.1.0 (2007-03-11)
  - Initial release
