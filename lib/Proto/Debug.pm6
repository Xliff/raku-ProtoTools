our $PROTO-DEBUG            is export = 0;
our $PROTO-SCRIPT-DEBUG     is export = 0;

sub checkDEBUG ($d = 1) is export {
  $PROTO-DEBUG >= $d;
}

INIT {
  my $v = %*ENV<P6_GLIB_DEBUG> // $*ENV<RAKU_GLIB_DEBUG>;
  if $v {
    say '»————————————> setting debug';
    if $v.Int -> \v {
      $PROTO-DEBUG = v unless v ~~ Failure;
    }
    $PROTO-DEBUG //= 1;

    say "Setting DEBUG level to { $PROTO-DEBUG }" if $PROTO-DEBUG;
  }

  Nil;
}
