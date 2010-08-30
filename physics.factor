! Copyright (C) 2010 Jon Harper.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays calendar calendar.unix colors.constants
combinators.smart generalizations kernel math math.functions
math.ranges math.rectangles math.vectors opengl random
sequences threads ui.gadgets ui.gestures ui.render ;
IN: physics

CONSTANT: g { 0 -9.81 }

GENERIC# interact 1 ( particule dt -- )

TUPLE: physics-world < gadget particules time running? ;
TUPLE: particule x v m mobile? ;
TUPLE: spring < particule k l0 particule ;
TUPLE: lintel < particule dim { bouncy initial: 1.0 } particules ;

: dv ( f dt m -- dv ) / v*n ; inline
: dx ( dt v -- dx ) n*v ;
: apply-force ( particule f dt -- )
    pick m>> [ dv [ v+ ] curry change-v drop ] [ 3drop ] if* ;
: weight ( particule -- force ) m>> [ g n*v ] [ { 0 0 } ] if* ;
: apply-g ( particule dt -- )
    [ dup weight ] [ apply-force ] bi* ;
: l0-scale-factor ( force spring -- force )
    l0>> over norm [ swap - ] keep / v*n ;
: spring-force ( particule spring -- force )
    [ swap [ x>> ] bi@ v- ] [ nip l0-scale-factor ] [ nip k>> ] 2tri v*n ;
: apply-k ( particule spring dt -- )
    [ 2dup spring-force dup vneg ] [ [ apply-force ] curry bi-curry@ bi* ] bi* ;
: find-side ( rect particule -- pos )
    [ [ x>> ] [ dim>> ] bi ] [ x>> ] bi* 2drop ;
: move-to-side ( rect particule -- )
    [ find-side ] keep x<< ;
: lintel>rect ( lintel -- rect ) [ x>> ] [ dim>> ] bi <rect> ;
: apply-contact ( lintel particule -- )
    2dup swap [ x>> ] [ lintel>rect ] bi* contains-point? [ move-to-side ] [ 2drop ] if ;
: move-particle ( particule dt -- )
    over mobile?>> [
        over v>> dx [ v+ ] curry change-x drop
    ] [ 2drop ] if ;
M: spring interact
    [ [ particule>> ] keep ] [ apply-k ] bi* ;
M: particule interact 2drop ;
M: lintel interact
    drop dup particules>> [ apply-contact ] with each ;
: step ( world dt -- )
    [ particules>> ] dip
    [ [ apply-g ] [ interact ] [ move-particle ] 2tri ] curry each ;
: system-seconds ( -- dt )
    system-micros -6 10^ * ;
: dt ( world -- dt )
     system-seconds [ swap time>> - ] [ >>time drop ] 2bi ;
: world-loop ( world -- )
   [ dup dt step ] [ relayout-1 ]
   [ dup running?>> [ yield world-loop ] [ drop ] if ] tri ;

: invert-y ( {x,y} -- {x,y}' ) first2 neg 2array ;
: {x,y}>{px,py} ( gadget {x,y} -- {px,py} )
    [ rect-bounds nip 2 v/n ] [ invert-y ] bi* v+ ;
: rectangle>screen ( gadget loc dim -- loc' dim' )
    [ {x,y}>{px,py} ] dip
    [ [ first2 ] [ second - ] bi* 2array ] keep ;

: <lintel> ( x v m mobile? dim bouncy particules -- lintel ) lintel boa ;
: <particule> ( x v m mobile? -- particule ) particule boa ;
: <spring> ( x v m mobile? k l0 particule -- spring ) spring boa ;
: <immobile-spring> ( x k l0 particule -- spring ) [ f f f ] 3dip <spring> ;
    
: random-spring ( -- spring particule )
    2 -200 200 [a,b] [ random ] curry replicate { 0 0 } 1 t <particule>
    2 -200 200 [a,b] [ random ] curry replicate
    0 1 uniform-random-float 100 random 4 npick <immobile-spring> ;

: random-springs ( n -- seq )
    [ random-spring 2array ] replicate concat ;
: <physics-world> ( -- world )
    physics-world new
    100 random-springs >>particules ;

! : <physics-world> ( -- world )
!    physics-world new
!    [
!    { 20 -100 } { 0 0 } 1 t <particule>
!    dup [ { -20 -40 } { 0 0 } 1.0 t 0.5 ] dip <spring>
    ! dup [ { 1 0 } { 0 0 } 1.0 t 0.5 ] dip <spring>
!    { 0 100 } 1.0 pick <immobile-spring>
!    ] output>array
    ! dup [ { 50 -150 } { 0 0 } 1 f { 200 200 } 1 ] dip <lintel> suffix
!    >>particules ;

GENERIC: draw-particule ( gadget particule -- )
M: particule draw-particule
    COLOR: black gl-color
    x>> {x,y}>{px,py} { 5 5 } gl-rect ;
M: spring draw-particule
    [ COLOR: red gl-color [ particule>> x>> {x,y}>{px,py} ] [ x>> {x,y}>{px,py} ] 2bi gl-line ]
    [ call-next-method ] 2bi ;
M: lintel draw-particule
    COLOR: purple gl-color [ x>> ] [ dim>> ] bi rectangle>screen gl-rect ;

M: physics-world pref-dim* drop { 640 480 } ;
M: physics-world draw-gadget*
    dup particules>> [ draw-particule ] with each ;

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
