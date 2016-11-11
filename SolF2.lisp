(load "datastructures.fas")
(load "auxfuncs.fas")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;;; Pedir 1,2
(defun nextState (st act)
  "generate the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"
	(let ((lst '()))
        (dolist (act (possible-actions) lst)
            (push (nextState st act) lst)
        )
    )
)

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
     
     (recursive_ldfs (problem-initial-state problem) problem lim)
)




(defun recursive_ldfs (state problem lim)
    (let ((corte_flag nil))
    (if (funcall (problem-fn-isGoal problem) state) (return-from recursive_ldfs (list state))
    
    (if (= lim 0) (return-from recursive_ldfs ':corte)))
    (setf corte_flag nil)
    (let ((successors (nextstates state)))
        (loop for successor in successors do
            (let ((result (recursive_ldfs successor problem (- lim 1))))
                (if (equal result ':corte) (setf corte_flag t)
                (if (not (equal result nil)) (return-from recursive_ldfs (append (list state) result ) )))
            )
        )
        (if (equal corte_flag t) (return-from recursive_ldfs ':corte)
        (return-from recursive_ldfs nil))
    )
    )
)
                
				      

;;; iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
    
    (loop for lim from 0 do
    
        (if (equal nil (limdepthfirstsearch problem lim) ) (return-from iterlimdepthfirstsearch nil))
        (if (equal ':corte (limdepthfirstsearch problem lim)) () (return-from iterlimdepthfirstsearch (limdepthfirstsearch problem lim)))
    )
    
     
)

	
	
	
	
	
	
	
	
	
	
	
	
	
