! Copyright (C) 2010 Jon Harper.
! See http://factorcode.org/license.txt for BSD license.
USING: tools.test physics kernel math accessors sequences math.functions ;
IN: physics.tests


[ t t ]
[ <test-world> 5000 [ dup 0.001 step ] times particules>> first x>> first2
-12.3961152703545 -0.005051482520286889 [ 0.000001 ~ ] bi-curry@ bi*
] unit-test
