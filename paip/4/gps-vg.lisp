; condition is an atom object
; setc is a set of conditions
;
; goals: setc
; state: setc
; op: map a state (stateA) to another state (stateB)
;     action: op name
;     preconds: stateA
;     add-list: stateB - stateA
;     del-list: stateA - stateB
;     We can think of add-list as the effect of the op
;
; gps: given init-state, ops and goals, find a series of ops to lead init-state to goals
;
; model: a graph
;   vertex: setc
;   edge: setc to condition

; middle-goals = (goals)
; path = ()
;
; while middle-goals is not null {
;   var gs = (pop middle-goals)
;   if init-state contains gs {
;     break
;   } else {
;     for g in gs {
;       for op in (op fit g) {
;         ???
;       }
;     }
;   }
; }