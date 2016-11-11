(load "datastructures.lisp")
(load "auxfuncs.lisp")


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
(defun limdepthfirstsearch (problem lim &key cutoff?)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
     
     ;(print "head")
     
     (let* ((this (make-node :state (problem-initial-state problem)))
            (successors (nextStates (node-state this))))
            
        
        ;(dolist (successor successors)
            (cond
                    ;IF this-state is goal, returns state
                    ((isGoalp (node-state this)) (return-from limdepthfirstsearch this))
                    
                    
                    
                    ;IF this-state has no successors, return nil, aka, exit cond and continue doList
                    ((endp successors) nil)
                    
                    
                    ;IF depth limit is achieved, dont expand this, instead continue cond
                    ((<= lim 0) nil)
                    
                
                    ;ELSE expand this (recursive)
                    (T (dolist (successor successors)
                       ; (print "inside loop")
                        (print successor)
                        (let ((new_problem (make-problem 
                                                :initial-state successor
                                                :fn-isGoal #'isGoalp
                                                :fn-nextStates #'nextStates))
                                            (lim (- lim 1)))
                        ;(let ((solution (limdepthfirstsearch new_problem (- lim 1))))
                         ;   (when solution (RETURN solution))
                            
                        (if (funcall 'limdepthfirstsearch new_problem lim)(print "goal")(print "rip"))
             
                        )
                    )
                )
            )
            
            
            
            
;;             limdepthfirstsearch((append(mapcar #'(lambda (s) (make-problem
;;                                                                     :initial-state s
;;                                                                     :fn-isGoal #'isGoalp
;;                                                                     :fn-nextstates #'nextStates))) 
;;                                                                         lim-1)))))
            
	)
)
	
	
#|		
(defun depth-limited-search (problem &optional (limit infinity)(node (create-start-node problem)))
  "Search depth-first, but only up to LIMIT branches deep in the tree."
  (cond 
        ((goal-test problem node) node)
        ((>= (node-depth node) limit) :cut-off)
        (t (for each n in (expand node problem) do
            (let ((solution (depth-limited-search problem limit n)))
            (when solution (RETURN solution)))))))
|#
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
#|				      

;;; iterlimdepthfirstsearch
(defun iterlimdepthfirstsearch (problem &key (lim most-positive-fixnum))
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
	(list (make-node :state (problem-initial-state problem))) )

|#
	
	
	
	
	
	
	
	
	
	
	
	
	
