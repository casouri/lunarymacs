;;; vundo.el --- visual undo      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Equivalence set:
;;
;; We keep track of who is equivalent to who by equivalence sets. Say
;; m1 = m2 = m3, m4 = m5. We represent that with a list of equivalence
;; sets (call equivalence list):
;;
;;     ((m1 m2 m3) (m4 m5))
;;
;; In such a list, each element can only appear at most once. So
;;
;;     ((m1 m2) (m2 m3))
;;
;; is corrupt.

;;; Code:
;;

(defun vundo--setup-test-buffer (type)
  "Setup and pop a testing buffer.
TYPE is the type of buffer you want."
  (interactive (list (string-to-number
                      (completing-read
                       "Type: " '(1 2 3)))))
  (let ((buf (get-buffer "*vundo-test*")))
    (if buf (kill-buffer buf))
    (setq buf (get-buffer-create "*vundo-test*"))
    (pop-to-buffer buf)
    (pcase type
      (1 ; Redo
       (execute-kbd-macro (kbd "woome"))
       (execute-kbd-macro (kbd "C-/"))
       (kmacro-keyboard-quit)
       (execute-kbd-macro (kbd "C-/")))
      (2 ; With branching.
       (execute-kbd-macro (kbd "woome"))
       (execute-kbd-macro (kbd "C-/"))
       (execute-kbd-macro (kbd "veemo")))
      (3 ; only undo
       (execute-kbd-macro (vconcat
                           (kbd "woome\n")
                           (kbd "woome\n")
                           (kbd "woome\n")
                           (kbd "C-/")
                           (kbd "C-/")
                           (kbd "C-/")) )))))

;;; Undo list to mod list

(cl-defstruct vundo-mod-list
  "Stores the undo list and the position of each modification."
  mod-list
  undo-list
  map)

(defun vundo--mod-list-from (undo-list)
  "Return a list of modifications in UNDO-LIST, earliest first.
Each modification is an undo list, the modification it represents
is at the top of that list."
  (let (mod-list
        (idx 0)
        (len (length undo-list)))
    (while (and (consp undo-list) (< idx len))
      ;; Skip leading nils.
      (while (null (nth idx undo-list))
        (cl-incf idx))
      (push idx mod-list)
      ;; “Collect” entries.
      (while (nth idx undo-list)
        (cl-incf idx)))
    (make-vundo-mod-list
     :mod-list mod-list :undo-list undo-list)))

(defun vundo--mod-list-nth (n mod-list)
  "Return the undo-list from the beginning to the Nth modification...
in MOD-LIST."
  (if (eq n -1)
      t
    (let ((idx (nth n (vundo-mod-list-mod-list mod-list))))
      (nthcdr idx (vundo-mod-list-undo-list mod-list)))))

(defun vundo--mod-list-len (mod-list)
  "Return the length of MOD-LIST."
  (length (vundo-mod-list-mod-list mod-list)))

(defun vundo--mod-list-pos (undo-list mod-list)
  "Return the position of UNDO-LIST in MOD-LIST."
  ;; TODO: use hash map.
  (cl-loop for m from 0 to (vundo--mod-list-len mod-list)
           if (equal (vundo--mod-list-nth m mod-list)
                     undo-list)
           return m
           finally return nil))

;;; Mod list to equiv set list

(defsubst vundo--A (m)
  "Return (A M)."
  (list 'A m))

(defsubst vundo--m (s)
  "Return m in S = (A m) or (B m)."
  (cadr s))

(defsubst vundo--s (s)
  "Return A or B in S = (A m) or (B m)."
  (car s))

(defun vundo--equiv-set-merge (s1 s2 set-list)
  "Merge equivalence set of S1 and S2 in SET-LIST.
SET-LIST is a list of equivalence sets (see Commentary)."
  (let ((s1-set (or (seq-find (lambda (set) (member s1 set)) set-list)
                    (list s1)))
        (s2-set (or (seq-find (lambda (set) (member s2 set)) set-list)
                    (list s2))))
    (setq set-list (remove s1-set set-list))
    (setq set-list (remove s2-set set-list))
    (cons (append s1-set s2-set) set-list)))

(defun vundo--equiv-set-find (s1 set-list)
  "Return the equivalence set containing S1 in SET-LIST."
  (seq-find (lambda (set) (member s1 set)) set-list))

(defalias 'vundo--equiv-set-normalize #'vundo--equiv-set-earliest)
(defun vundo--equiv-set-earliest (s1 set-list &optional reverse)
  "Normalize S1 in SET-LIST...
...so that all states in a equivalence set has the same
representation. E.g., if A1 = B2 = A3, they are all
normalized to A1.

If REVERSE is non-nil, normalize in the other direction: instead
of minimizing m, maximize it."
  (vundo--equiv-set-find-earliest
   (vundo--equiv-set-find s1 set-list) reverse))

(defun vundo--equiv-set-find-earliest (set &optional reverse)
  "Return state (A m) or (B m) in SET...
... such that m is the minimum among all states in SET.

If REVERSE is non-nil, return the state with largest m instead."
  (let ((s1 (pop set)))
    (dolist (s2 set s1)
      (if (if reverse
              (> (vundo--m s2) (vundo--m s1))
            (< (vundo--m s2) (vundo--m s1)))
          (setq s1 s2)))))

(defun vundo--equiv-set-from (mod-list)
  "Turn MOD-LIST into an equivalence set list of states.
Each state is either (A m) or (B m), (A m) means the state after
modification m, B means before. Some states are equivalent, in
which case they are in the same equivalence set."
  (let (equiv-set-list)
    (cl-loop
     for m from 0 to (1- (vundo--mod-list-len mod-list))
     for mod = (vundo--mod-list-nth m mod-list)
     ;; Add (B m). Naturally (B i) = (A i-1), e.g., (B 2) = (A 1).
     do (if (eq m 0)
            ;; (push (list (vundo--B m)) equiv-set-list)
            (push (list (vundo--A -1)) equiv-set-list)
          ;; (setq equiv-set-list
          ;;       (vundo--equiv-set-merge
          ;;        (vundo--B m) (vundo--A (1- m)) equiv-set-list))
          )
     ;; Add (A m). If mod is an undo, we can connect (A m) with a
     ;; previous state.
     do (if-let* ((prev (undo--last-change-was-undo-p mod)))
            ;; FIXME: t means this undo is region-undo, currently for
            ;; the convenience of testing we regard t as undo to the
            ;; beginning of history.
            (if (eq prev t)
                (setq equiv-set-list
                      ;; (vundo--equiv-set-merge
                      ;;  (vundo--A m) (vundo--B 0) equiv-set-list)
                      (vundo--equiv-set-merge
                       (vundo--A m) (vundo--A -1) equiv-set-list))
              (if-let ((prev-m (vundo--mod-list-pos prev mod-list)))
                  (setq equiv-set-list
                        (vundo--equiv-set-merge
                         (vundo--A m) (vundo--A prev-m) equiv-set-list))
                (error "PREV-M shouldn’t be nil but is")))
          ;; If mod isn’t an undo, A m is a new state.
          (push (list (vundo--A m)) equiv-set-list)))
    equiv-set-list))

(defun vundo--connected-state (s1)
  "Return the state connected to S1."
  (pcase s1
    (`(A ,m) (list 'B m))
    (`(B ,m) (list 'A m))))

;;; Equiv set list to tree

(cl-defstruct vundo-node
  "A node in a undo tree."
  states
  children
  parent
  marker)

(defmacro vundo--alist-get (key alist &optional default remove testfn)
  "The same as ‘alist-get’ but use ‘equal’ by default.
KEY ALIST DEFAULT REMOVE TESTFN see ‘alist-get’."
  `(alist-get ,key ,alist ,default ,remove ,(or testfn #'#'equal)))

(defun vundo--sort-children (node)
  "Sort the children of NODE, oldest first."
  (let ((children (vundo-node-children node)))
    (setf (vundo-node-children node)
          (seq-sort (lambda (c1 c2)
                      (let ((s1 (vundo--equiv-set-find-earliest
                                 (vundo-node-states c1)))
                            (s2 (vundo--equiv-set-find-earliest
                                 (vundo-node-states c2))))
                        (< (vundo--m s1) (vundo--m s2))))
                    children))))

(defun vundo--tree-from (equiv-set-list mod-list)
  "Generate a tree from EQUIV-SET-LIST & MOD-LIST.
Return an alist of nodes. Each key is the normalized state (A m),
each value is the node struct."
  (let* ((m 0)
         (root (vundo--A -1))
         ;; Visited states, each state is normalized.
         (normed-visited-states (list root))
         (node-alist (list (cons root
                                 (make-vundo-node
                                  :states
                                  (vundo--equiv-set-find
                                   root equiv-set-list))))))
    (while (vundo--equiv-set-find (vundo--A m) equiv-set-list)
      (let ((normed-state-prev (vundo--equiv-set-normalize
                                (vundo--A (1- m)) equiv-set-list))
            (normed-state-this (vundo--equiv-set-normalize
                                (vundo--A m) equiv-set-list))
            (mod (vundo--mod-list-nth m mod-list)))
        ;; Add NORMED-STATE-THIS. NORMED-STATE-PREV must already
        ;; exists.
        (unless (member normed-state-this normed-visited-states)
          (push normed-state-this normed-visited-states)
          (setq node-alist
                (cons (cons normed-state-this
                            (make-vundo-node
                             :states (vundo--equiv-set-find
                                      normed-state-this equiv-set-list)))
                      node-alist)))
        ;; Connect the two state nodes if the mod between them isn’t
        ;; an undo.
        (unless (undo--last-change-was-undo-p mod)
          (let ((node-prev (vundo--alist-get
                            normed-state-prev node-alist))
                (node-this (vundo--alist-get
                            normed-state-this node-alist)))
            ;; Add child.
            (unless (memq node-this (vundo-node-children node-prev))
              (setf (vundo-node-children node-prev)
                    (cons node-this (vundo-node-children node-prev)))
              ;; Sort children, so old state comes before young state.
              ;; This way the shape of the tree is stable and only the
              ;; tip changes. TODO: maybe we can sort less often.
              (vundo--sort-children node-prev))
            ;; Add parent to child.
            (setf (vundo-node-parent node-this) node-prev))))
      (setq m (1+ m)))
    ;; Return node alist.
    node-alist))

;;; Draw tree

(defun vundo--replace-at-col (from to col)
  "Replace FROM at COL with TO in each line of current buffer.
If a line is not COL columns long, we skip it."
  (save-excursion
    (let ((run t))
      (goto-char (point-min))
      (while run
        (move-to-column col)
        (if (and (eq (current-column) col)
                 (looking-at (regexp-quote from)))
            (replace-match to))
        ;; If ‘forward-line’ returns 0, we haven’t hit the end of
        ;; buffer.
        (setq run (eq (forward-line) 0))))))

(defun vundo--put-node-at-point (node)
  "Store the corresponding NODE as text property at point."
  (put-text-property (1- (point)) (point)
                     'vundo-node
                     node))

(defun vundo--get-node-at-point ()
  "Retrieve the corresponding NODE as text property at point."
  (plist-get (text-properties-at (1- (point)))
             'vundo-node))

(defun vundo--marker-node (marker)
  "Return the node at MARKER."
  (save-excursion
    (goto-char marker)
    (vundo--get-node-at-point)))

(defun vundo--draw-tree (node-alist buffer)
  "Draw the tree in NODE-ALIST in BUFFER."
  (with-current-buffer buffer
    (let* (;; (root (vundo--alist-get (vundo--B 0) node-alist))
           (root (vundo--alist-get (vundo--A -1) node-alist))
           (node-queue (list root))
           (inhibit-read-only t))
      (erase-buffer)
      (while node-queue
        (let* ((node (pop node-queue))
               (children (vundo-node-children node))
               (parent (vundo-node-parent node))
               ;; I’m the nth child of PARENT.
               (my-idx (if parent
                           (seq-position
                            (vundo-node-children parent) node)
                         0))
               ;; I’m the last child of PARENT.
               (node-last-child-p
                (if parent
                    (eq node (car (last (vundo-node-children parent))))
                  nil)))
          ;; Draw this node. First go to parent.
          (if parent (goto-char (vundo-node-marker parent)))
          (cond ((null parent)
                 (insert "●"))
                ((eq my-idx 0)
                 (insert "──●"))
                (t
                 (let ((col (max 0 (1- (current-column)))))
                   ;; New line at bottom.
                   (goto-char (point-max))
                   (insert "\n")
                   ;; Go under parent node in the new line.
                   (indent-to-column col)
                   ;; Connect parent down.
                   (vundo--replace-at-col " " "│" col)
                   (if node-last-child-p
                       (insert "└──●")
                     (insert "├──●")))))
          ;; Store marker so we can later come back to this node.
          (setf (vundo-node-marker node) (point-marker))
          ;; Associate the node in buffer with the node object.
          (vundo--put-node-at-point node)
          ;; Depth-first search.
          (setq node-queue (append children node-queue)))))))

;;; Undo tree buffer and invocation

(defun vundo--buffer ()
  "Return the vundo buffer."
  (get-buffer-create " *vundo tree*"))

(defun vundo--kill-buffer-if-point-left (window)
  "Kill the vundo buffer if point left WINDOW.
WINDOW is the window that was/is displaying the vundo buffer."
  (if (and (eq (window-buffer window) (vundo--buffer))
           (not (eq window (selected-window))))
      (with-selected-window window
        (kill-buffer-and-window))))

(defvar vundo--mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") #'vundo-forward)
    (define-key map (kbd "b") #'vundo-backward)
    (define-key map (kbd "n") #'vundo-next)
    (define-key map (kbd "p") #'vundo-previous)
    (define-key map (kbd "q") #'kill-buffer-and-window)
    (define-key map (kbd "i") #'vundo--inspect)
    (define-key map (kbd "c") #'vundo--show-cursor)
    map)
  "Keymap for ‘vundo--mode’.")

(define-derived-mode vundo--mode special-mode
  "Vundo" "Mode for displaying the undo tree."
  (setq mode-line-format nil
        truncate-lines t
        cursor-type nil)
  ;; If you leave the vundo buffer for the orig buffer and do some
  ;; modifications, you have to refresh the buffer. We can
  ;; auto-refresh or auto-quit, I choose to auto-quit.
  (add-hook 'window-state-change-functions
            #'vundo--kill-buffer-if-point-left
            0 t))

(defun vundo--state-node (state node-alist equiv-set-list)
  "Return the corresponding node of STATE in NODE-ALIST.
EQUIV-SET-LIST is used for normalizing STATE."
  (vundo--alist-get (vundo--equiv-set-normalize state equiv-set-list)
                    node-alist))

(defvar-local vundo--mod-list nil
  "Modification list generated by ‘vundo--mod-list-from’.")
(defvar-local vundo--equiv-set-list nil
  "Equivalence set list generated by ‘vundo--equiv-set-from’.")
(defvar-local vundo--node-alist nil
  "Node alist generated by ‘vundo--tree-from’.")
(defvar-local vundo--orig-buffer nil
  "Vundo buffer displays the undo tree for this buffer.")
(defvar-local vundo--latest-node nil
  "The latest node.")

(defun vundo--refresh-buffer (orig-buffer vundo-buffer)
  "Refresh VUNDO-BUFFER with the undo history of ORIG-BUFFER."
  ;; If ‘buffer-undo-list’ is nil, then we do nothing.
  (when-let* ((mod-list (vundo--mod-list-from
                         (buffer-local-value
                          'buffer-undo-list orig-buffer)))
              (equiv-set-list (vundo--equiv-set-from mod-list))
              (node-alist (vundo--tree-from equiv-set-list mod-list))
              (inhibit-read-only t))
    (with-current-buffer vundo-buffer
      (vundo--mode)
      (setq vundo--mod-list mod-list
            vundo--equiv-set-list equiv-set-list
            vundo--node-alist node-alist
            vundo--orig-buffer orig-buffer)
      (vundo--draw-tree node-alist vundo-buffer)
      ;; Highlight current node.
      (let* ((last-m (1- (vundo--mod-list-len mod-list)))
             (last-state (vundo--A last-m))
             (norm-last-state (vundo--equiv-set-normalize
                               last-state equiv-set-list))
             (node (vundo--alist-get norm-last-state node-alist)))
        (goto-char (vundo-node-marker node))
        (put-text-property (1- (point)) (point) 'face 'error)
        (setq vundo--latest-node node)))))

(defun vundo ()
  "Display visual undo for current buffer."
  (interactive)
  (if buffer-undo-list
      (let* ((vundo-buf (vundo--buffer))
             (orig-buf (current-buffer)))
        (vundo--refresh-buffer orig-buf vundo-buf)
        (select-window
         (display-buffer-in-side-window
          vundo-buf
          '((side . bottom)
            (slot . 1)
            (window-height . 5))))
        (fit-window-to-buffer nil 5))
    (message "There is no undo history")))

;;; Traverse undo tree

(defun vundo--calculate-shortest-route (from-set to-set)
  "Calculate the shortest route from FROM-SET to TO-SET.
Both SETs are an equivalence set of states. Return (SOURCE .
DEST) where SOURCE and DEST are states (A m)."
  (let (route-list)
    ;; Find all valid routes.
    (dolist (source from-set)
      (dolist (dest to-set)
        ;; We only allow route in this direction.
        (if (> (vundo--m source) (vundo--m dest))
            (push (cons source dest) route-list))))
    ;; Find the shortest route.
    (car (seq-sort (lambda (r1 r2)
                     ;; I.e., distance between SOURCE and DEST in R1
                     ;; compare against distance in R2.
                     (< (- (vundo--m (car r1)) (vundo--m (cdr r1)))
                        (- (vundo--m (car r2)) (vundo--m (cdr r2)))))
                   route-list))))

(defun vundo--latest-n-mod (n undo-list)
  "Return N latest modifications in UNDO-LIST.
Each modification is delimited by nil in UNDO-LIST."
  (let ((end 0)
        (start 0))
    ;; Skip through initial nil’s.
    (while (null (nth end undo-list))
      (cl-incf end)
      (cl-incf start))
    (while (> n 0)
      ;; Skip through nil’s.
      (while (null (nth end undo-list))
        (cl-incf end))
      ;; “Collect” a modification
      (while (nth end undo-list)
        (cl-incf end))
      ;; Hit a nil, this modification ended.
      (cl-decf n))
    (seq-subseq undo-list start end)))

(defun vundo--move-to-node
    (current dest orig-buffer mod-list equiv-set-list)
  "Move from CURRENT node to DEST node by undoing in ORIG-BUFFER.
ORIG-BUFFER must be at CURRENT state. MOD-LIST is the list you
get from ‘vundo--mod-list-from’. EQUIV-SET-LIST is what you get
from ‘vundo--equiv-set-from’. You should refresh vundo buffer
after calling this function."
  (if-let* ((source-state-set (vundo-node-states current))
            (dest-state-set (vundo-node-states dest))
            (route (vundo--calculate-shortest-route
                    source-state-set dest-state-set)))
      (let* ((source-state (car route))
             (dest-state (cdr route))
             (step (- (vundo--m source-state)
                      (vundo--m dest-state)))
             ;; The complete undo-list that stops at SOURCE-STATE. If
             ;; there exists a possible route, SOURCE-STATE must not
             ;; be (A -1).
             (undo-list-at-source
              (vundo--mod-list-nth (vundo--m source-state) mod-list))
             ;; The complete undo-list that stops at DEST-STATE. This
             ;; is used for recording in ‘undo-equiv-table’.
             (undo-list-at-dest
              (let ((idx (vundo--m dest-state)))
                (if (eq idx -1) t
                  (vundo--mod-list-nth idx mod-list))))
             ;; We will undo these modifications.
             (planned-undo
              (vundo--latest-n-mod step undo-list-at-source)))
        (with-current-buffer orig-buffer
          ;; Undo. This will undo modifications in PLANNED-UNDO and
          ;; add new entries to ‘buffer-undo-list’.
          (primitive-undo step planned-undo)
          ;; Now we can try trimming ‘buffer-undo-list’.
          (let ((latest-unique-state-m
                 (seq-max (mapcar (lambda (set)
                                    (vundo--m
                                     (vundo--equiv-set-find-earliest
                                      set)))
                                  equiv-set-list))))
            (if (>= (vundo--m dest-state) latest-unique-state-m)
                ;; Can trim undo-list, trim to DEST.
                (setq buffer-undo-list undo-list-at-dest)
              ;; Can’t trim undo-list, update ‘undo-equiv-table’.
              (let ((list buffer-undo-list))
                ;; Strip leading nils.
                (while (eq (car list) nil)
	          (setq list (cdr list)))
                (puthash list undo-list-at-dest undo-equiv-table))))))
    ;; TODO
    (error "What?")))

(defun vundo-forward (arg)
  "Move forward ARG nodes in the undo tree.
If ARG < 0, move backward"
  (interactive "p")
  (let ((step (abs arg)))
    (let* ((step-fn (if (> arg 0)
                        (lambda (node)
                          (or (car (vundo-node-children node))
                              node))
                      (lambda (node)
                        (or (vundo-node-parent node) node))))
           (node vundo--latest-node)
           (dest node))
      (while (> step 0)
        (setq dest (funcall step-fn dest))
        (cl-decf step))
      (unless (eq node dest)
        (vundo--move-to-node
         node dest vundo--orig-buffer
         vundo--mod-list vundo--equiv-set-list)
        (vundo--refresh-buffer
         vundo--orig-buffer (current-buffer))))))

(defun vundo-backward (arg)
  "Move back ARG nodes in the undo tree.
If ARG < 0, move forward."
  (interactive "p")
  (vundo-forward (- arg)))

(defun vundo-next (arg)
  "Move to node below the current one. Move ARG steps."
  (interactive "p")
  (let* ((node vundo--latest-node)
         (parent (vundo-node-parent node)))
    (if parent
        (let* ((siblings (vundo-node-children parent))
               (idx (seq-position siblings node))
               (new-idx (+ idx arg))
               ;; TODO: Move as far as possible instead of not
               ;; moving when ARG is too large.
               (dest (or (nth new-idx siblings) node)))
          (unless (eq node dest)
            (vundo--move-to-node
             node dest vundo--orig-buffer
             vundo--mod-list vundo--equiv-set-list)
            (vundo--refresh-buffer
             vundo--orig-buffer (current-buffer)))))))

(defun vundo-previous (arg)
  "Move to node above the current one. Move ARG steps."
  (interactive "p")
  (vundo-next (- arg)))

;;; Debug

(defun vundo--inspect ()
  "Print some useful info at point"
  (interactive)
  (message "States: %s" (vundo-node-states (vundo--get-node-at-point))))

(defun vundo--show-cursor ()
  "Make cursor visible."
  (interactive)
  (setq cursor-type t))

;;; Tests

(ert-deftest vundo--misc-test ()
  ;; `vundo--pop-mod'.
  (let ((lst (list 1 2 3 nil 4 5 6)))
    (should (equal (cons lst '(4 5 6))
                   (vundo--pop-mod lst))))
  ;; `vundo--mod-list-from'.
  (let ((lst (list 6 5 4 nil 3 2 1)))
    (should (equal (list (list 3 2 1)
                         (list 6 5 4 nil 3 2 1))
                   (vundo--mod-list-from lst))))
  ;; ‘vundo--equiv-set-merge’.
  (let ((lst (list (list 1 2) (list 3 4) (list 5 6))))
    (should (equal (list (list 1 2 3 4) (list 5 6))
                   (vundo--equiv-set-merge 1 3 lst))))
  (let ((lst nil))
    (should (equal (list (list 1 3))
                   (vundo--equiv-set-merge 1 3 lst))))
  ;; ‘vundo--equiv-set-find-earliest’.
  (should (equal (vundo--equiv-set-find-earliest '((A 1) (B 2) (A 3)))
                 '(A 1))))

(provide 'vundo)

;;; vundo.el ends here
