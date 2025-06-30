use v6.c;

unit package Proto::Raw::Exports;

use CompUnit::Util :re-export;

our @proto-exports is export = <
  Proto::Debug
  Proto::Exceptions
  Proto::Subs
>;

our %exported;

sub proto-re-export ($compunit) is export {
  return if %exported{$compunit}:exists;

  re-export-everything($compunit);
  %exported{$compunit} = True;
}
