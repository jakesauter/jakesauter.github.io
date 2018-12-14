;;;;File: tethered_goat_solution.1
;;;;Line: Lisp solution for "The Goat" problem

;observations
(setf length-of-tether 77)
(setf long-side-of-barn 100)
(setf short-side-of-barn 60)

;related deductions
(setf radius-of-large-circle length-of-tether)
(setf radius-of-small-circle (- length-of-tether short-side-of-barn))
(setf area-of-large-circle (* pi radius-of-large-circle radius-of-large-circle))
(setf area-of-small-circle (* pi radius-of-small-circle radius-of-small-circle))

;compute final result
(setf grazing-area (+ (* area-of-large-circle .75) (* area-of-small-circle .25)))
