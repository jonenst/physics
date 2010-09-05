! Copyright (C) 2010 Jon Harper.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays calendar calendar.unix colors.constants constructors
combinators.smart fonts generalizations kernel math
math.functions math.parser math.ranges math.rectangles
math.vectors opengl random sequences threads ui.gadgets
ui.gestures ui.render ui.text locals ;
IN: physics

CONSTANT: g { 0 -9.81 }

GENERIC: interact ( particule -- )

TUPLE: physics-world < gadget particules time running? ;
TUPLE: particule x v m mobile? { temp-force initial: { 0.0 0.0 } } ;
TUPLE: spring < particule k l0 particule ;
TUPLE: lintel < particule dim { bouncy initial: 1.0 } particules ;

: dv ( f dt m -- dv ) / v*n ; inline
: dx ( dt v -- dx ) n*v ;
: (apply-force) ( particule f dt -- )
    pick m>> [ dv [ v+ ] curry change-v drop ] [ 3drop ] if* ;
: apply-force ( particule dt -- )
    [ [ dup temp-force>> ] dip (apply-force) ]
    [ drop { 0.0 0.0 } >>temp-force drop ] 2bi ;
: store-temp-force ( particule force -- )
    [ v+ ] curry change-temp-force drop ;
: apply-speed ( particule dt -- )
    over v>> dx [ v+ ] curry change-x drop ;
: move-particle ( particule dt -- )
    over mobile?>> [
        [ apply-force ] [ apply-speed ] 2bi
    ] [ 2drop ] if ;


: weight ( particule -- force ) m>> [ g n*v ] [ { 0 0 } ] if* ;
: apply-g ( particule -- )
    dup weight store-temp-force ;

: l0-scale-factor ( force spring -- force )
    l0>> over norm [ swap - ] keep / v*n ;
: spring-force ( particule spring -- force )
    [ swap [ x>> ] bi@ v- ] [ nip l0-scale-factor ] [ nip k>> ] 2tri v*n ;
: apply-mutual-force ( p1 p2 fp1/p2 -- )
    dup vneg [ store-temp-force ] bi-curry@ bi* ;
: apply-k ( particule spring -- )
    2dup spring-force apply-mutual-force ;

: find-side ( rect particule -- pos )
    [ [ x>> ] [ dim>> ] bi ] [ x>> ] bi* 2drop ;
: move-to-side ( rect particule -- )
    [ find-side ] keep x<< ;
: lintel>rect ( lintel -- rect ) [ x>> ] [ dim>> ] bi <rect> ;
:: ((normal-vector)) ( {x,y} {X,Y} -- ? ? )
    {x,y} first2 :> ( x y )
    {X,Y} first2 :> ( X Y )
    Y X / x * :> Y/X*x
    Y/X*x y <
    Y/X*x neg Y + y < ;
: (normal-vector) ( lintel particule -- rel-pos dim )
    [ swap [ x>> ] bi@ v- ] [ drop dim>> ] 2bi ;
: normal-vector ( lintel particule -- v )
    (normal-vector) ((normal-vector))
    [ [ { 0 1 } ] [ { 1 0 } ] if ] [ [ { -1 0 } ] [ { 0 -1 } ] if ] if ;
: (contact-force) ( lintel particule -- force )
    [ normal-vector ] [ nip temp-force>> ] 2bi
    over v. v*n ;
: project ( v1 v2 -- v3 )
    over v. v*n ;
: apply-contact ( lintel particule -- )
!    [ 2dup (contact-force) apply-mutual-force ] call ; ! [ ?move-to-side ] 2bi ;
     [ normal-vector reverse ] keep [ project ] with
     [ change-v ] [ change-temp-force drop ] bi ;
: ?apply-contact ( lintel particule -- )
    2dup swap [ x>> ] [ lintel>rect ] bi* contains-point? [ apply-contact ] [ 2drop ] if ;

M: spring interact
    [ particule>> ] keep apply-k ;
M: particule interact drop ;
M: lintel interact
    dup particules>> [ ?apply-contact ] with each ;
: step ( world dt -- )
    [ particules>> ] dip
    [ drop [ apply-g ] each ]
    [ drop [ interact ] each ]
    [ [ move-particle ] curry each ] 2tri ;
: system-seconds ( -- dt )
    system-micros -6 10^ * ;
: dt ( world -- dt )
     system-seconds [ swap time>> - ] [ >>time drop ] 2bi ;
: world-loop ( world -- )
   [ dup dt step ] [ relayout-1 ]
   [ dup running?>> [ 1/30 seconds sleep world-loop ] [ drop ] if ] tri ;

: invert-y ( {x,y} -- {x,y}' ) first2 neg 2array ;
: {x,y}>{px,py} ( gadget {x,y} -- {px,py} )
    [ rect-bounds nip 2 v/n ] [ invert-y ] bi* v+ ;
: rectangle>screen ( gadget loc dim -- loc' dim' )
    [ {x,y}>{px,py} ] dip
    [ [ first2 ] [ second - ] bi* 2array ] keep ;

CONSTRUCTOR: lintel ( x v m mobile? dim bouncy particules --  lintel ) ;
CONSTRUCTOR: particule ( x v m mobile? -- particule ) ;
CONSTRUCTOR: spring ( x v m mobile? k l0 particule -- spring ) ;
: <immobile-spring> ( x k l0 particule -- spring ) [ { 0 0 } f f ] 3dip <spring> ;

: random-spring ( -- spring particule )
    2 -200 200 [a,b] [ random ] curry replicate { 0 0 } 1 t <particule>
    2 -200 200 [a,b] [ random ] curry replicate
    0 1 uniform-random-float 100 random 4 npick <immobile-spring> ;

: random-springs ( n -- seq )
    [ random-spring 2array ] replicate concat ;
! : <physics-world> ( -- world )
!    physics-world new
!    10 random-springs >>particules ;

: <physics-world> ( -- world )
   physics-world new
   [
    { 0 -110 } { 0 0 } 10 t <particule>
     [ { -5 -45 } { 0 0 } 10.0 t 25.0 60.0 ] keep <spring>
     [ { 10 0 } { 0 0 } 30.0 t 50.0 80 ] keep <spring>
     [ { 0 60 } 50.0 70 ] keep <immobile-spring>
   ] output>array
   [ { 100 -200 } { 0 0 } 1 f { 20 270 } 1 ] keep <lintel> suffix
   [ { -100 -200 } { 0 0 } 1 f { 20 270 } 1 ] keep <lintel> suffix
   [ { -100 -200 } { 0 0 } 1 f { 220 20 } 1 ] keep <lintel> suffix
   [ { -100 50 } { 0 0 } 1 f { 220 20 } 1 ] keep <lintel> suffix
   >>particules ;

GENERIC: draw-particule ( gadget particule -- )
: particule-size ( particule -- size )
    m>> [ dup 2array ] [ { 3 3 } ] if* ;
M: particule draw-particule
    COLOR: black gl-color
    dup particule-size [ [ x>> {x,y}>{px,py} ] [ 2 v/n ] bi* v- ] keep gl-rect ;
M: spring draw-particule
    [ COLOR: red gl-color [ particule>> x>> {x,y}>{px,py} ] [ x>> {x,y}>{px,py} ] 2bi gl-line ]
    [ call-next-method ] 2bi ;
M: lintel draw-particule
    COLOR: purple gl-color [ x>> ] [ dim>> ] bi rectangle>screen gl-rect ;

GENERIC: particule-energy ( particule -- energy )
M: particule particule-energy ( particule -- energy )
    dup mobile?>> [
        [ [ m>> ] [ x>> second ] bi g second neg * * ]
        [ [ m>> ] [ v>> norm sq ] bi 0.5 * * ] bi +
    ] [ drop 0 ] if ;
M: spring particule-energy ( spring -- energy )
    [ call-next-method ] [
        [ [ dup particule>> [ x>> ] bi@ v- norm ] [ l0>> ] bi - sq ]
        [ k>> ] bi 0.5 * *
    ] bi + ;
: energy ( world -- energy )
    particules>> [ particule-energy ] [ + ] map-reduce ;

M: physics-world pref-dim* drop { 640 480 } ;
M: physics-world draw-gadget*
    [ energy number>string "Total Energy : " prepend [ monospace-font ] dip draw-text ]
    [ dup particules>> [ draw-particule ] with each ] bi ;

: create-thread ( demo-gadget -- thread )
  [ world-loop ] curry "Physics thread" spawn ;
: start-demo ( demo-gadget -- )
  [ create-thread drop ] [ t >>running? system-seconds >>time drop ] bi ;
: stop-demo ( demo-gadget -- )
  f >>running? drop ;
: toggle-demo ( demo-gadget -- )
  dup running?>> [ stop-demo ] [ start-demo ] if ;

M: physics-world graft* start-demo ;
M: physics-world ungraft* stop-demo ;
physics-world H{
  { T{ button-down f f 1 } [ toggle-demo ] }
} set-gestures
