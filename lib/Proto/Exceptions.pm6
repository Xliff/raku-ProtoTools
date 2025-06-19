use v6.c;

role X::Proto::Roles::Message {
  has $!message is built;
  has $!routine is built;

  method message {
    (
      $!routine
        ?? "[{ $!routine } - { self.^name }]"
        !! "[{ self.^name }] "
    ) ~ $!message
  }
}

class X::Proto::Exception is Exception does X::Proto::Roles::Message { }

class X::Proto::NYI is X::NYI does X::Proto::Roles::Message {

  multi method new (
    :$item  is required,
    :$class
  ) {
    my $message = $item;
    $message ~= " in { $class.^name }" unless $class === Any;

    self.bless( message => "{ $message } NYI!" )
  }

}

class X::Proto::WTF is X::Proto::Roles::Message {
  method new {
    self.bless(
      message => 'WTF?!? This was not supposed to happen!'
    )
  }
}

class X::Proto::InvalidState is X::Proto::Exception {

  multi method new ( :$message = 'Invalid State' ) {
    self.bless(:$message);
  }
}

class X::Proto::Object::AttributeNotFound is X::Proto::Exception {
  has $.attribute;

  multi method new (
    :$attribute is required,
    :$message                = "Attribute '{ $attribute }' not found!"
  ) {
    self.bless( :$attribute, :$message );
  }
}

class X::Proto::Object::AttributeValueOutOfBounds is X::Proto::Exception {
  has $.attribute;
  has $.value;
  has $.range;

  method new (
    :$message = "[{ self.^name }] "
                ~
                "{ $!value     } is outside the valid range of {
                   $!range     } for the '{
                   $!attribute }' attribute"
  ) {
    self.bless( :$message );
  }
}

class X::Proto::Variant::NotAContainer is X::Proto::Exception {

  method new (
    :$message = 'Variant is not a container, so cannot serve as a Positional!'
  ) {
    self.bless( :$message );
  }

}

class X::Proto::InvalidSize is X::Proto::Exception {
  multi method new ( :$message = 'Invalid size!' ) {
    self.bless( :$message );
  }
}

class X::Proto::CArrayUnknownSize is X::Proto::InvalidSize {
  method message {
    "CArray came from C and therefore, its size is unknown!";
  }
}

class X::Proto::UnknownType is X::Proto::Exception {
  multi method new ( :$message = 'Unknown type!' ) {
    self.bless( :$message );
  }
}

class X::Proto::InvalidType is X::Proto::Exception {
  multi method new ( :$message = 'Invalid type!' ) {
    self.bless( :$message );
  }
}

class X::Proto::InvalidArgument is X::Proto::Exception {
  multi method new ( :$message = 'Invalid argument' ) {
    self.bless( :$message );
  }
}

class X::Proto::InvalidArguments is X::Proto::InvalidArgument { }
class X::Proto::InvalidValue     is X::Proto::Exception       { }

class X::Proto::InvalidNumberOfArguments is X::Proto::Exception {
  method new ( :$message = 'Invalid number of arguments' ) {
    self.bless( :$message );
  }
}

class X::Proto::InvalidIndex is X::Proto::Exception {
}

class X::Proto::OnlyOneOf is X::Proto::Exception {
  has @.values  is built;

  submethod BUILD ( :$values ) {
    @!values = $values;
  }

  method new ( :
    :$values  is required,
    :$message              = "Can use only one of the following variables: {
                               @.values.join(', ') }"
  ) {
    self.bless( :$values, :$message );
  }

}

class X::Proto::ProtectedMethod is X::Proto::Exception {
  method new ( :$message = 'Cannot execute a protected method from here.' ) {
    self.bless( :$message );
  }
}

class X::Proto::DynamicObjectFailure is X::Proto::Exception {

  method new ($type, :$any = False) {
    self.bless(
      message => "Dynamic module loading of { $type.^name } " ~ (
        $any ?? "resulted in an object with no type!"
             !! "failed!"
      )
    );
  }

}

package Proto::Raw::Exceptions { }
