use v6.c;

use NativeCall;

use Proto::Exceptions;

unit package Proto::Subs;

# cw: Thank you SO MUCH!
# https://stackoverflow.com/questions/47124405/parsing-a-possibly-nested-braced-item-using-a-grammar
our token nested-parens is export {
   '(' ~ ')' [
     || <- [()] >+
     || <.before '('> <~~>
   ]*
}

our token nested-braces is export {
   '{' ~ '}' $<before>=[
     || ( <- [{}] >+ )
     || <.before '{'> <~~>
   ]*
}

 our rule class-def is export {
  'class'
    $<name>=[ \S+ ]
    [ $<misc>=(<-[{]>+) ]?
  <nested-braces>
}

our regex method-def is export {
  $<mod>=[ [ 'proto' | 'multi' ] <.ws> ]?
  $<m>=[ 'sub'?'method' ]        <.ws>
  $<name>=(<-[)(}{\s]>+)         <.ws>
  $<args>=<nested-parens>?       <.ws>
  [ $<misc>=(<-[{]>+) ]?
  <nested-braces>
}


my token d { <[0..9 x]> }
my token m { '-' }
my token L { 'L' }
my token w { <[A..Za..z0..9 _]> }

my rule comment {
  '/*'$<text>=.+?'*/'
}

my regex name {
  <[_ A..Z a..z]>+
}

my token       p  { [ '*' [ \s* 'const' \s* ]? ]+ }
my token       n  { <[\w _]>+ }
my token       t  { <n> | '(' '*' <n> ')' }
my token     mod  { 'unsigned' | 'long' }
my token    mod2  { 'const' | 'struct' | 'enum' }
my rule     type  { <mod2>? [ <mod>+ ]? $<n>=\w+ <p>? }
my rule array-def { '[]' | '['$<size>=.+?']' }
my rule       var { <t><array-def>? }

our token classname is export {
  [ \w+ ]+ % '::' [':' [\w+'<'.+?'>']+ % ':' ]?
}
our token parent is export {
  <classname>
}
our token typename is export { \w+ }

our rule also-does is export {
  'also' 'does' <classname> ';'
}

our rule class-start is export {
  'class' <classname> ['is' <parent>]? '{'
}

our rule struct-entry is export {
  <type> <var>+ %% ','
}

our rule solo-struct is export {
  'struct' <sn=name>? '{'
    [ <struct-entry>\s*';' ]+
  '}'
}

our rule struct is export {
  'typedef'? <solo-struct> <rn=name>? ';'
}

my rule enum-entry is export {
  \s* ( <w>+ ) (
    [ '=' '('?
      [
        <m>?<d>+<L>?
        |
        <w>+
      ]
      [ '<<' ( [<d>+ | <w>+] ) ]?
      ')'?
    ]?
  ) ','?
  <comment>?
  \v*
}

my rule solo-enum is export {
  'enum' <n=name>? <comment>? \v* '{'
  <comment>? \v* [ <comment> | <enum-entry> ]+ \v*
  '}'
}

my rule enum is export {uint32
  [ 'typedef' <solo-enum> <rn=name> | <solo-enum> ] ';'
}

sub find-files (
  $dir,
  :$pattern   is copy,
  :$extension is copy,
  :$exclude,
  :$depth
) is export {
  my @pattern-arg;
  my @targets = dir($dir);

  with $pattern {
    when Regex { @pattern-arg.push: $_}

    when Str   {
      $pattern .= trans( [ '/', '-' ] => [ '\\/', '\\-' ] );
      @pattern-arg.push( rx/     <{ $pattern   }>   / );
    }
  }
  $extension .= substr(1) if $extension && $extension.starts-with('.');
  @pattern-arg.push: rx/ '.' <{ $extension }> $/ if $extension;

  gather {
    WHILE: while @targets {
      say "T: { @targets.gist }" if $GTK-SCRIPT-DEBUG;

      my $elem = @targets.shift;
      say "E: $elem" if $GTK-SCRIPT-DEBUG;
      do given $elem {
        when .d {
          if $depth {
            next unless
              $*SPEC.splitdir($elem).grep( * ).elems < max($depth - 1, 0)
          }
          @targets.append: $elem.dir;
          next;
        }

        when .f {
          if $exclude.defined {
            given $exclude {
              when Array    { next if $elem.absolute ~~ .any         }
              when Str      { next if $elem.absolute.contains($_)    }
              when Regex    { next if $elem.absolute ~~ $_           }
              when Callable { next if $_($elem)                      }

              default {
                die "Don't know how to handle { .^name } as an exclude!";
              }
            }
          }

          for @pattern-arg -> $p {
            say "Testing: { $elem.absolute } / P: { $p.gist }"
              if $GTK-SCRIPT-DEBUG;
            next WHILE unless $elem.absolute ~~ $p
          }
          say "Valid: { $elem.absolute }" if $GTK-SCRIPT-DEBUG;
          take $elem;
        }

        default {
          say "Skupping non-directory, non-file path element { .absolute }!";
        }
      }
    }
  }
}

sub get-lib-files (:$pattern, :$extension) is export {
  die 'get-lib-files() must be called with a :$pattern and/or an :$extension'
    unless $pattern.defined || $extension.defined;

  (do gather for getLibDirs().split(',') {
    take find-files($_, :$pattern, :$extension);
  }).flat;
}

sub get-module-files is export {
  get-lib-files( extension => 'pm6' );
}

sub get-raw-module-files is export {
  get-lib-files( pattern => / '/Raw/' /, extension => 'pm6' )
}

sub malloc         (size_t                    --> Pointer)  is export is native { * }
sub realloc        (Pointer, size_t           --> Pointer)  is export is native { * }
sub calloc         (size_t,  size_t           --> Pointer)  is export is native { * }
sub memcpy         (Pointer, Pointer, size_t  --> Pointer)  is export is native { * }
sub memcmp         (Pointer, Pointer, size_t  --> int32)    is export is native { * }
sub memset         (Pointer, int32,   size_t)               is export is native { * }
sub dup2           (int32,   int32            --> int32)    is export is native { * }
sub isatty         (int32                     --> int32)    is export is native { * }

# Needed for minimal I18n
sub setlocale      (int32,   Str              --> Str)      is export is native { * }
sub ftruncate      (int32,   uint64)                        is export is native { * }

sub native-open    (Str, int32, int32 $m = 0)
  is export
  is symbol('open')
  is native
{ * }

# cw: Because using "-> *@a { ... } for callbacks was pissing me off...
#     ... so pick your poison...
sub SUB  (&block, :b(:$block) = False) is export {
  # cw: -YYY- You sure you want to limit by # of BLOCK params?
  my $p = &block.signature.params.elems;

  $block
    ??
      -> *@a {
        my ($*A, @*A, $*ARGS, @*ARGS) := @a xx 4;
        $p ?? &block( |@a[ ^$p ] ) !! &block()
      }
    !!
      sub ( *@a ) {
        my ($*A, @*A, $*ARGS, @*ARGS) := @a xx 4;
        $p ?? &block( |@a[ ^$p ] ) !! &block()
      }
}
sub CB   (&block, :b(:$block) = False) is export { SUB(&block) }
sub DEF  (&block, :b(:$block) = False) is export { SUB(&block) }
sub FUNC (&block, :b(:$block) = False) is export { SUB(&block) }

sub new-pointer (\T = Pointer) is export {
  my $p = malloc( $*KERNEL.bits / 8 );
  nativecast( Pointer[T], $p ) if T !== Pointer;
  $p;
}

sub marquee ($s) is export {
  say "{ $s }\n{ $s x $s.elems }";
}
sub mq ($s) is export {
  $s.&marquee;
}

role StructActualName[Str $N] is export {
  method actual-name { $N }
}

role StructSkipsTest[Str $R] is export {
  method reason { $R }
}

role NativeSized[$size] is export {

  method sizeof { $size }

  method alloc {
    cast( self.WHAT, malloc(self.sizeof) )
  }

}

# So as not to collide with the CORE sub.
sub native-close   (int32 --> int32)
  is export
  is symbol('close')
  is native
{ * }

our proto sub free (|) is export { * }
multi sub free (Pointer)                           is export is native { * }

# Cribbed from https://stackoverflow.com/questions/1281686/determine-size-of-dynamically-allocated-memory-in-c
sub malloc_usable_size (Pointer --> size_t)        is export is native { * }

# Implement memcpy_pattern. Take pattern and write pattern.^elem bytes to successive areas in dest.

sub getEndian is export {
  ( $*KERNEL.endian == BigEndian, $*KERNEL.endian == LittleEndian );
}

sub cast ($cast-to, $obj) is export {
  nativecast($cast-to, $obj);
}

# This is a version of cast() that is ammenable to the .& form.
# So:
#      $a-poiner.&recast(AnotherType)
sub recast ($obj, $cast-to) is export {
  cast($cast-to, $obj);
}

sub real-resolve-uint64($v) is export {
  $v +& 0xffffffffffffffff;
}

# p = Pointer
sub p ($p) is export {
  cast(Pointer, $p);
}

# ba = Byte Array
sub ba ($o) is export {
  cast(CArray[uint8], $o);
}

# Moved from p6-GStreamer
sub nolf ($s) is export {
  $s.subst("\n", ' ', :g);
}

sub lf ($s) is export {
  $s.subst("\r\n", "\n", :g);
}

sub crlf ($s) is export {
  $s.subst("\n", "\r\n", :g);
}

sub indent ($s, $n) is export {
  $s.lines.map({ (' ' x $n) ~ $_ }).join("\n");
}

proto sub intRange (|) is export
{ * }

multi sub intRange (\T) {
  my $name = T.^name;
  (my $bits = $name) ~~ s/.+? (\d+) $/$0/;
  $bits .= Int;
  samewith( :$bits, signed => $name.starts-with('u').not )
}
multi sub intRange ($bits where *.defined, $signed = False) {
  intRange(:$bits, :$signed);
}
multi sub intRange (:$bits, :$signed = False) {
  $signed ??
    -1 * 2 ** ($bits - 1) .. 2 ** ($bits - 1) - 1
    !!
    0 .. 2 ** $bits
}

# cw: Coerces the value of $a to a value within $r.
multi sub clamp($a, $l, $h) is export {
  max( $l, min($a, $h) )
}
multi sub clamp($a, Range() $r) is export {
  samewith($a, $r.min, $r.max);
}

sub lcfirst ($s is copy) is export {
  my $a := $s.substr-rw(0, 1);
  $a .= lc;
  $s;
}

sub ucfirst ($s is copy) is export {
  my $a := $s.substr-rw(0, 1);
  $a .= uc;
  $s;
}

my token lc-word is export { <:lower>+ };
my token uc-word is export { <:upper> <:lower>+ };

sub un-camel ($w, $s = '-', :$lc = True, :$uc = $lc.not) is export {
  return Nil unless $w ~~ / <lc-word> <uc-word>+ /;

  my &case = $uc ?? &uc !! &lc;

  ( $/<lc-word>, |$/<uc-word> )».Str».&case.join($s);
}
sub deCamelCase ($w) is export { un-camel($w) }

sub under-to-dash ($w) is export {
  $w.subst('_', '-', :g);
}

# This should be in CORE!
sub separate (Str() $s, Int() $p) is export {
  die '$p outside of string range!' unless $p ~~ 0 .. $s.chars;
  ( $s.substr(0, $p), $s.substr($p, *) )
}

sub createReturnArray (\T) is export {
  my \at = T.REPR eq 'CStruct' ?? Pointer[T] !! T;
  (my $a = CArray[at].new)[0] = at;
  $a;
}

sub updateHash (%h, %ct, :$reverse = True) {
  state $l = Lock.new;

  $l.protect: {
    %h.append(
      $reverse ?? %ct.antipairs.Hash !! %ct
    );
  }
}

sub resolveNativeType (\T) is export {
  say "Resolving { T.^name } to its Raku equivalent..."
    if checkDEBUG(2);
uint32
  do given T {
    when num32 | num64     { Num }

    when int8  | uint8  |
         int16 | uint16 |
         int32 | uint32 |
         int64 | uint64
                           { Int }

    when str               { Str }

    default                {
      do if T.REPR eq <CPointer CStruct>.any {
        T
      } else {
        X::Proto::InvalidValue.new(
          routine => &?ROUTINE.name,
          message => "Do not know how to handle a type of { .^name }!"
        ).throw
      }
    }
  }
}

sub checkForType(\T, $v is copy) is export {
  if T !=== Nil {
    unless $v ~~ uint32T {
      #say "Attempting to convert a { $v.^name } to { T.^name }...";
      my $resolved-name = resolveNativeType(T).^name;
      $resolved-name ~= "[{ T.of.^name }]" if $resolved-name eq 'CArray';
      #say "RN: { $resolved-name }";
      if $v.^lookup($resolved-name) -> $m {
        #say "Using coercer at { $v.^name }.$resolved-name...";
        $v = $m($v);
      }
      # Note reversal of usual comparison. This is due to the fact that
      # nativecall types must be compatible with the value, not the
      # other way around. In all other cases, T and $v should have
      # the same type value.
      die "Value does not support { $v.^name } variables. Will only accept {
        T.^name }!" unless T ~~ $v.WHAT;
    }
  }
  $v;
}

sub ArrayToCArray(\T, Array() $a, :$size = $a.elems, :$null = False)
  is export
{
  enum Handling <P NP>;
  my $handling;
  my $ca = (do given (T.REPR // '') {
    when 'P6opaque' {
      when T ~~ Str                     { $handling = P;  CArray[T]          }

      proceed
    }

    when 'CPointer' | 'P6str'  |
         'P6num'    | 'P6int'           { $handling = P;  CArray[T]          }
    when 'CStruct'  | 'CUnion'          { $handling = NP; CArray[Pointer[T]] }

    default {
      "ArrayToCArray does not know how to handle a REPR of '$_' for type {
       T.^name }"
    }
  });

  say "CA: { $ca.^name } / { $size }" if $DEBUG;

  return $ca unless $a.elems;
  $ca = $ca.new;
  for ^$size {
    my $typed = checkForType(T, $a[$_]);

    $ca[$_] = $handling == P ?? $typed !! cast(Pointer[T], $typed)
  }
  if $null {
    $ca[$size] = do given T {
      when Int             { 0          }
      when Num             { 0e0        }
      when Str             { Str        }
      when $handling == P  { T          }
      when $handling == NP { Pointer[T] }
    }
  }

  $ca;
}
