! Copyright (C) 2010 Jon Harper.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays calendar calendar.unix colors.constants combinators constructors
combinators.smart fonts generalizations kernel math
math.functions math.parser math.ranges math.rectangles
math.vectors opengl random sequences threads ui.gadgets
ui.gestures ui.render ui.text locals delegate namespaces ;
IN: physics

CONSTANT: g { 0 -9.81 }
SYMBOL: time-source
SINGLETON: real-time
SINGLETON: fixed-time
TUPLE: (swap-state) x v { temp-force initial: { 0.0 0.0 } } ;
: <(swap-state)> ( x v -- swap-state )
    (swap-state) new swap >>v swap >>x ;
TUPLE: swap-state current-state previous-state ;
: <swap-state> ( x v -- swap-state )
    <(swap-state)> dup clone swap-state new
    swap >>current-state swap >>previous-state ;

GENERIC: interact1 ( particule -- )
GENERIC: interact2 ( particule -- )

TUPLE: physics-world < gadget particules time running? ;
TUPLE: particule swap-state m mobile? ;
TUPLE: spring < particule k l0 particule ;
TUPLE: lintel < particule dim { bouncy initial: 1.0 } particules ;

PROTOCOL: state-access x>> v>> x<< v<< temp-force>> temp-force<< ;
CONSULT: state-access particule swap-state>> current-state>> ;

: dv ( f dt m -- dv ) / v*n ; inline
: dx ( dt v -- dx ) n*v ; inline
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
: det ( v1 v2 -- n )
    [ [ first ] [ second ] bi* * ] [ [ second ] [ first ] bi* * ] 2bi - ;
: ?project ( v1 v2 -- v3 )
    2dup det 0 > [ project ] [ nip ] if ;
: invert-y ( {x,y} -- {x,y}' ) first2 neg 2array ;
: rotate-90 ( v -- v' ) invert-y reverse ; inline
: tangent-vector ( lintel particule -- v ) normal-vector rotate-90 ; inline

: apply-normal-contact ( lintel particule -- )
     [ tangent-vector ] keep [ ?project ] with
     [ change-v ] [ change-temp-force drop ] bi ;
: tangent-speed ( lintel particule -- v )
    [ tangent-vector ] [ nip v>> ] 2bi project ;
: tangent-contact-k ( lintel particule -- k )
    [ normal-vector ] [ nip temp-force>> ] 2bi project norm 0.1 * neg ; ! Todo c'est k
: tangent-contact-force ( lintel particule -- force )
    [ tangent-speed ] [ tangent-contact-k ] 2bi v*n  ;
: apply-tangent-contact ( lintel particule -- )
   2dup tangent-contact-force swapd apply-mutual-force ;
: apply-contact ( lintel particule -- )
    [ apply-tangent-contact ] [ apply-normal-contact ] 2bi ;
: ?apply-contact ( lintel particule -- )
    2dup swap [ x>> ] [ lintel>rect ] bi* contains-point? [ apply-contact ] [ 2drop ] if ;

M: spring interact1
    [ particule>> ] keep apply-k ;
M: spring interact2 drop ;
M: particule interact1 drop ;
M: particule interact2 drop ;
M: lintel interact1 drop ;
M: lintel interact2
    dup particules>> [ ?apply-contact ] with each ;
: step ( world dt -- )
    [ particules>> ] dip
    { [ drop [ apply-g ] each ]
    [ drop [ interact1 ] each ]
    [ drop [ interact2 ] each ]
    [ [ move-particle ] curry each ] } 2cleave ;
: system-seconds ( -- dt )
    system-micros -6 10^ * ;
HOOK: dt time-source ( world -- dt )
M: real-time dt ( world -- dt )
     system-seconds [ swap time>> - ] [ >>time drop ] 2bi ;
CONSTANT: debug-dt 0.05
M: fixed-time dt ( world -- dt )
    drop debug-dt ;
: world-loop ( world -- )
   [ dup dt step ] [ relayout-1 ]
   [ dup running?>> [ 1/30 seconds sleep world-loop ] [ drop ] if ] tri ;

: {x,y}>{px,py} ( gadget {x,y} -- {px,py} )
    [ rect-bounds nip 2 v/n ] [ invert-y ] bi* v+ ;
: rectangle>screen ( gadget loc dim -- loc' dim' )
    [ {x,y}>{px,py} ] dip
    [ [ first2 ] [ second - ] bi* 2array ] keep ;

: new-particule ( x v m mobile? class -- particule )
    new swap >>mobile? swap >>m
    [ <swap-state> ] dip swap >>swap-state ;
: <particule> ( x v m mobile? -- particule )
    particule new-particule ;
:: new-lintel ( x v m mobile? dim bouncy particules class --  lintel )
     x v m mobile? class new-particule dim >>dim bouncy >>bouncy particules >>particules ;
: <lintel> ( x v m mobile? dim bouncy particules -- lintel )
    lintel new-lintel ;
:: new-spring ( x v m mobile? k l0 particule class -- spring )
    x v m mobile? class new-particule k >>k l0 >>l0 particule >>particule ;
: <spring> ( x v m mobile? k l0 particule -- spring )
    spring new-spring ;
: <immobile-spring> ( x k l0 particule -- spring ) [ { 0 0 } f f ] 3dip <spring> ;

: random-pair ( [a,b] [c,d] -- pair )
    [ random ] bi@ 2array ; inline
: random-particule ( -- particule )
    -200 200 [a,b] 0 200 [a,b] random-pair
    2 -90 90 [a,b] [ random ] curry replicate 10 [1,b] random t <particule> ;
: random-spring ( -- spring particule )
    random-particule
    2 -200 200 [a,b] [ random ] curry replicate
    0 1 uniform-random-float 100 random 4 npick <immobile-spring> ;
: random-particules  ( n -- particules )
    [ random-particule ] replicate ;
: random-springs ( n -- seq )
    [ random-spring 2array ] replicate concat ;
! : <physics-world> ( -- world )
!    physics-world new
!    10 random-springs >>particules ;
: <test-world> ( -- world )
   physics-world new
    { -10 40 } { 0 0 } 10 t <particule>
    [ { 10 130 } { 0 0 } 30.0 t 50.0 80 ] keep <spring> 2array
   [ { -100 -100 } { 0 0 } 1 f { 320 100 } 1 ] keep <lintel> suffix
   >>particules ;
: <cool-world> ( -- world )
   physics-world new
   [
!    { 50 -110 } { 0 0 } 10 t <particule>
!    { -250 110 } { 40 0 } 10 t <particule>
    ! { -150 50 } { 40 0 } 10 t <particule>
    ! { 150 50 } { -70 0 } 10 t <particule>
    ! { -150 150 } { 40 -40 } 10 t <particule>
    { -10 40 } { 0 0 } 10 t <particule>
   ! [ { -5 -45 } { 0 0 } 10.0 t 25.0 60.0 ] keep <spring>
     [ { 10 130 } { 0 0 } 30.0 t 50.0 80 ] keep <spring>
     [ { 0 200 } 50.0 70 ] keep <immobile-spring>
    ] output>array
    500 random-particules append
!   [ { 100 -200 } { 0 0 } 1 f { 20 270 } 1 ] keep <lintel> suffix
!   [ { -100 -200 } { 0 0 } 1 f { 20 270 } 1 ] keep <lintel> suffix
!   [ { -100 -200 } { 0 0 } 1 f { 220 20 } 1 ] keep <lintel> suffix
   [ { -100 -100 } { 0 0 } 1 f { 320 100 } 1 ] keep <lintel> suffix
   [ { -500 -140 } { 0 0 } 1 f { 1520 100 } 1 ] keep <lintel> suffix
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

fixed-time time-source set-global
