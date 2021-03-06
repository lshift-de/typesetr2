typesetr2
========

A revived project that converts different document formats
to a prettified output, such as PDF or latex.

## Comparison to the 1.x version

Instead of writing our own complete converters we decided to a
well-established Pandoc framework <http://pandoc.org/> to do most
of the heavy lifting of the conversion.

Pandoc is not capable of providing prettified documents out-of-the-box
so we have to perform some optimizations and document manipulations
in order to be Pandoc friendly.
This means that we perform some pre-processing and post-processing of
the documents before and after Pandoc.

## Instructions for running (command line tool atm ONLY)

### Dependencies

 - Simple Build Tool - install the latest version of [sbt](http://www.scala-sbt.org/)
 - Pandoc - requires a local build of the [master](http://pandoc.org/installing.html) branch that contains our latest contributions (there was no release that contains them, yet). Since some of our PRs may still be in the queue to the official master we have a [fork of Pandoc](https://github.com/hubertp-lshift/pandoc) and a special branch called `master-typesetr` that contains the latest changes. Same applied to Pandoc's definitions, [pandoc-types](https://github.com/hubertp-lshift/pandoc-types). 
 - Scala - (optional) `sbt` handles all the compilation process and provides a console but you may still find it useful

### Compiling and running

Then enter sbt's console

    > sbt

(wait for the Internet to download if that's the first time you did it)

    sbt> project core
    sbt> run

This will display a number of parameters (required or optional).
We also need to checkout the old `typesetr` where the existing
templates are located.
For example, you can transform an ODT document into a pdf using the
following command:

    sbt> run -i odt -o pdf --infile $INPUTFILE.odt --outfile $OUTPUTFILE.pdf -s lshift/pitch --style-base $TYPESETRSTYLES

where
 - `$INPUTFILE.odt` - is some ODF-compliant file
 - `$OUTPUTFILE.pdf` - is the desired location where the pdf is to be created
 - `lshift/pitch` - determines the template/style to use
 - `$TYPESETRSTYLES` - is the location where all styles are defined

To test, simply run:

    sbt> test


## Limitations

### Overview
 
 - [o] headers - no sections above heading level 5
 - [X] inline code - ODT Parser in Pandoc does not recognize any code blocks (other input formats support it). See [PR](https://github.com/jgm/pandoc/pull/3186)
 - [ ] removal of bogus paragraph elements - won't be implemented (Pandoc understands those correctly)
 - [ ] list items - custom typesetr's styling - desired?
 - [X] list items - starting/restarting numbering - currently Pandoc hardcoded 1 as a starting number for ODT documents (see [PR](https://github.com/jgm/pandoc/pull/3146))
 - [X] bullet items - only a single character representing bullet points (original typesetr had the possibility of 'disc', 'circle', 'square', 'hyphen'). Will reuse lshift style.
 - [X] tables are always empty (see [PR](https://github.com/jgm/pandoc/pull/3199))
 - [X] images - not supported in the ODT Parser in Pandoc (see [PR](https://github.com/jgm/pandoc/pull/3165) and follow ups)
 - [i] floating figures - not supported in Pandoc's LaTeX writer (see our [PR](https://github.com/jgm/pandoc/pull/3180))
 - [X] labels - needs to translate local href references to bookmark references
 - [X] inline math formulas - Pandoc translates backslashes to \textbackslash{}, which breaks the latex conversion
 - [X] simple tables - currently ignored
 - [ ] inner quoations (American style) - unknown state
 - [ ] negative numbers - (also broken in the old typesetr)
 - [i] page breaks - Pandoc [limitation](https://github.com/jgm/pandoc/issues/1934), see [our PR](https://github.com/jgm/pandoc/pull/3230)
 - [X] page numbering - total number of pages is broken
 - [X] table of contents 
 - [ ] links as new commands - remains to be decided whether it is necessary with the new design
 - [X] deeply nested items - currently not supported ([Pandoc limitation](https://github.com/jgm/pandoc/issues/2922)). We added a workaround in our templates.
 - [o] odt - no native support for inline math text - we can add Pandoc support for it potentially. Partially supported by doing some postprocessing.
 - [o] multiple text style changes - Pandoc only picks the first one (see [issue](https://github.com/jgm/pandoc/issues/3223)). Looks like it is partially the fault of Typesetr's templates. 
 - [ ] underline formatting - Pandoc [limitation](https://github.com/jgm/pandoc/issues/2264)
Legend:
 - empty - not supported
 - o - partially supported
 - i - implementation in progress
 - w - won't be implemented
 - X - implemented

## Bugs

 - [X] duplicate entries for references, bookmarks etc (see [reported bug](https://github.com/jgm/pandoc/issues/3143))

### Headers

Unlike the old oversion of Typesetr, Pandoc does not handle heading styles
above the level 5. While a workaround can be integrated, currently no styling
is applied above level 5.

