! Copyright (C) 2010 Jon Harper.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors arrays calendar calendar.unix colors.constants
combinators.smart kernel math math.functions math.rectangles math.vectors opengl
sequences threads ui.gadgets ui.gestures ui.render ;
IN: physics

CONSTANT: g { 0 -9.81 }
CONSTANT: k 0.49 

GENERIC# interact 1 ( particule dt -- )

TUPLE: physics-world < gadget particules time running? ;
TUPLE: particule x v m mobile? ;
TUPLE: spring < particule k particule ;

: dv ( f dt m -- dv ) / v*n ; inline
: dx ( dt v -- dx ) n*v ;
: apply-force ( particule f dt -- )
    pick m>> [ dv [ v+ ] curry change-v drop ] [ 3drop ] if* ;
: weight ( particule -- force ) m>> [ g n*v ] [ { 0 0 } ] if* ;
: apply-g ( particule dt -- )
    [ dup weight ] [ apply-force ] bi* ;
: apply-k ( particule spring dt -- )
    [ [ drop ] [ swap [ x>> ] bi@ v- ] [ nip k>> ] 2tri v*n ] [ apply-force ] bi* ;
: move-particle ( particule dt -- )
    over mobile?>> [ 
        over v>> dx [ v+ ] curry change-x drop 
    ] [ 2drop ] if ; 
M: spring interact
    [ [ particule>> ] keep ] [ apply-k ] bi* ;
M: particule interact 2drop ;
: step ( world dt -- )
    [ particules>> ] [ 
        [ [ apply-g ] [ interact ] [ move-particle ] 2tri ] curry ] bi*
    each ; 
: system-seconds ( -- dt )
    system-micros -6 10^ * ;
: dt ( world -- dt )
     system-seconds [ swap time>> - ] [ >>time drop ] 2bi ;
: world-loop ( world -- )
   [ dup dt step ] [ relayout-1 ] 
   [ dup running?>> [ 1/20 seconds sleep world-loop ] [ drop ] if ] tri ;

: invert-y ( {x,y} -- {x,y}' ) first2 neg 2array ;
: {x,y}>{px,py} ( gadget {x,y} -- {px,py} )
    [ rect-bounds nip 2 v/n ] [ invert-y ] bi* v+ ;

: <particule> ( x v m mobile? -- particule ) particule boa ;
: <spring> ( x v m mobile? k particule -- spring ) spring boa ;
: <immobile-spring> ( x k particule -- spring ) [ f f f ] 2dip spring boa ;
    
: <physics-world> ( -- world )
    physics-world new
    [
    { 160 -140 } { -300 0 } 1 t <particule> 
    { 0 40 } 0.5 pick <immobile-spring>
    
    { -50 0 } { 0 0 } 1 t <particule> 
    { -50 0 } 1 pick <immobile-spring>
    ] output>array >>particules ;

GENERIC: draw-particule ( gadget particule -- )
M: particule draw-particule
    COLOR: black gl-color
    x>> {x,y}>{px,py} { 5 5 } gl-rect ;
M: spring draw-particule 
    [ COLOR: red gl-color [ particule>> x>> {x,y}>{px,py} ] [ x>> {x,y}>{px,py} ] 2bi gl-line ]
    [ call-next-method ] 2bi ;
    
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
