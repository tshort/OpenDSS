TPerlRegEx is a Delphi VCL wrapper around the open source PCRE library, which implements Perl-Compatible Regular Expressions.

This version of TPerlRegEx is compatible with the TPerlRegEx class in the RegularExpressionsCore unit in Delphi XE.  In fact, the unit in Delphi XE is derived from the version of TPerlRegEx that you are using now.

The supplied pcrelib.dll contains PCRE 7.9, compiled with Unicode support.  The supplied OBJ files contain PCRE 7.9, compiled with Unicode support.  By default, the OBJ files are used.  You can use the DLL if you have multiple applications using TPerlRegEx and you don't want to waste space by linking the OBJ files to be linked into each of those applications.

For more information about PCRE, please visit http://www.regular-expressions.info/pcre.html

For more information about regular expressions in general, please visit http://www.regular-expressions.info/

You can download the latest version of TPerlRegEx at http://www.regular-expressions.info/delphi.html

TPerlRegEx is licensed under the Mozilla Public License, version 1.1.

This new version of TPerlRegEx descends from TObject.  There are no packages to install into Delphi and nothing appears on the component palette.  Simply add PerlRegEx to the uses clause of any units you want to use it in.  There's no need to add the pcre unit to the uses clause.  This unit is used internally by TPerlRegEx.