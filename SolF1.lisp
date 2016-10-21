
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

(defun isObstaclep (pos track) 
	"check if there is an obstacle at position pos of the track"
	(let ((a (car pos))(b (cadr pos)))
		(cond
			((equal nil (nth a (nth b track))) t)
			(t nil))
	t))

(defun isGoalp (st) 
  "check if st is a goal state"
	(let ((lst (state-track-endpositions st)))
		(dolist (n lst)
			(cond ((equal n (state-pos st)) t)
				(t nil)))
	t))

(defun nextState (st act)
  "generate the nextState after state st and action act"
; (make-STATE :POS '(3 16)
;	      :VEL '(1 3)
;	      :ACTION act
;	      :COST -100))
	let(((posx (car (state-pos st)))(posy (cadr(state-pos st))) (velx (car(state-vel st))) 
		(vely (cadr(state-vel st))) (trk (state-track st))(cst (state-cost st))) 
	"pos l = pos l + vel l + act l"
	"pos c = pos c + vel c + act c"
	"v l = v l + act l"
	"v c = v c + act c"
	"nova pos is obstacle nova pos = pos; v = 0; cost += 20"
	"nova pos is goal cost -= 99"
	(cond
		(
	)
	
	))
	
	
	
	

