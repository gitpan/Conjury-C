require 5.004;
use strict;
use ExtUtils::MakeMaker;

WriteMakefile
  NAME => 'Conjury::C',
  VERSION_FROM => 'C.pm',
  dist => { COMPRESS => 'gzip', SUFFIX => 'gz' };
