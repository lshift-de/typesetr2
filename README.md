typesetr2
========

A revived project that converts different document formats
to a prettified output, such as PDF or latex.

## Comparison to the 1.x version

Instead of writing our own complete converters we decided to a
well-established Pandoc frameowrk <http://pandoc.org/> to do most
of the heavy lifting of the conversion.

Pandoc is not capable of providing prettified documents out-of-the-box
so we have to perform some optimizations and document manipulations
in order to be Pandoc friendly.
This means that we perform some pre-processing and post-processing of
the documents before and after Pandoc.

## Instructions for running (command line tool atm ONLY)

Typesetr 2.x is written in Scala. We use Simple Build Tool (sbt)
for compiling and running the tool.
Install the latest version of [sbt](http://www.scala-sbt.org/).
Then enter sbt's console

    > sbt

(wait for the internet to download if that's the first time you did it)

    sbt> project core
    sbt> run

This will display a number of parameters (required or optional).
We also need to checkout the old `typesetr` where the existing
templates are located.
For example, you can transform an ODT document into a pdf using the
following command:

    sbt> run -i odt -o pdf --infile $INPUTFILE.odt --outfile $OUTPUTFILE.pdf -s lshift/pitch --style-base $TYPESETRSTYLES

where
 - $INPUTFILE.odt - is some ODF-compliant file
 - $OUTPUTFILE.pdf - is the desired location where the pdf is to be created
 - lshift/pitch - determines the template/style to use
 - $TYPESETRSTYLES - is the location where all styles are defined
