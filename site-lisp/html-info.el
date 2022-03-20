;;; html-info.el --- HTML Info  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'pcase)
(require 'dom)
(require 'shr)

;;; Errors

(define-error 'html-info-error "Html-info generic error" 'error)
(define-error 'html-info-libxml-missing
              "Libxml library reqired but missing" 'html-info-error)
(define-error 'html-info-file-not-found
              "File not found" 'html-info-error)
(define-error 'html-info-not-full-path
              "File path not a full path" 'html-info-error)

(defvar-local html-info--current-file nil
  "The path of the currently visiting Info file.")

(defvar html-info--dom-cache nil
  "An alist of (PATH . DOM).
PATH is the path to an Info file, DOM is that file’s DOM.")

(defvar html-info--shr-override-map
  (let ((map (make-sparse-keymap )))
    (set-keymap-parent map shr-map)
    (define-key map [remap shr-browse-url]
                #'html-info--follow-link-at-point)
    map)
  "The keymap used to override the map on shr links.")

(defvar html-info--dom-classes-of-node
  '("chapter" "section" "subsection"
    "subsubsection" "subsubsubsection")
  "All possible class attributes of a node.")

(defvar html-info--navigation nil
  "A list of three anchor nodes.
Contains the next, previous and up node of the current node, in
that order.  Each anchor node is like (a ((href ...)) ...).")

(defun html-info--expand-node-name (node-name)
  "Expand NODE-NAME according to Texinfo algorithm."
  ;; See https://www.gnu.org/software/texinfo/manual/texinfo/html_node/HTML-Xref-Node-Name-Expansion.html
  (cl-labels ((need-encoding (ch)
                (not (string-match (rx (or letter digit " "))
                                   (char-to-string ch)))))
    (setq node-name
          (replace-regexp-in-string (rx (+ space)) " " node-name))
    (setq node-name (string-trim node-name))
    (setq node-name
          (mapconcat (lambda (ch)
                       (if (need-encoding ch)
                           (format "_%04x" ch)
                         (char-to-string ch)))
                     node-name))
    (setq node-name (string-replace " " "-" node-name))))

(defun html-info--check-full-path (path)
  (unless (file-name-absolute-p path)
    (signal 'html-info-not-full-path path)))

(defun html-info--dom-remove-subsections (dom)
  "Remove subsections from DOM.

A node in HTML is a div, this div contains not only the content
of that node, but also its sub-nodes.  We don’t want to display
the content of the sub-nodes, so we remove them with this
function."
  (append (list (dom-tag dom) (dom-attributes dom))
          (cl-remove-if (lambda (node)
                          (and (consp node)
                               (eq (dom-tag node) 'div)
                               (member (dom-attr node 'class)
                                       html-info--dom-classes-of-node)))
                        (dom-children dom))))

(defun html-info--dom-remove-header (dom)
  "Remove the header of DOM and return (DOM . (NEXT PREV UP)).

NEXT, PREV, UP are the anchor node (<a href=...>) of the
previous, next and up node, respectively."
  ;; We must not remove header destructively because we cache the dom
  ;; object.
  (let* ((dom (copy-tree dom))
         (header (car (dom-by-class dom "header")))
         (anchors (dom-by-tag header 'a))
         (navi
          ;; ANCHORS is either (NEXT PREV UP CONTENT INDEX)
          ;; or (NEXT UP CONTENT INDEX)
          ;; or (PREV UP CONTENT INDEX).
          (cond ((eq (length anchors) 5)
                 (cl-subseq anchors 0 3))
                ;; Prev is missing.
                ((string-match "Next" (dom-texts header))
                 (list (car anchors) nil (nth 2 anchors)))
                ;; Next is missing.
                ((string-match "Prev" (dom-texts header))
                 (cons nil (cl-subseq anchors 0 2))))))
    (dom-remove-node dom header)
    (cons dom navi)))

(defun html-info--get-dom (file)
  "Return the DOM of Info file FILE."
  (when (not (file-exists-p file))
    (signal 'html-info-file-not-found file))
  (if (fboundp 'libxml-parse-html-region)
      (or (alist-get file html-info--dom-cache nil nil #'equal)
          (setf (alist-get file html-info--dom-cache nil nil #'equal)
                (with-temp-buffer
                  (insert-file-contents file)
                  (libxml-parse-html-region (point-min) (point-max)))))
    (signal 'html-info-libxml-missing nil)))

(defun html-info--dom-bfs (dom pred)
  "Search DOM breadth-first with PRED. Return the first match.
PRED takes a single argument, the node."
  (declare (indent 1))
  (catch 'found
    (let ((queue (dom-children dom)))
      (while queue
        (let ((node (pop queue)))
          (when (consp node)
            (if (funcall pred node)
                (throw 'found node)
              (setq queue (append queue (dom-children node))))))))))

(defun html-info--find-node-1 (file id strict-case)
  "Return the DOM of the node with ID in FILE.
First search for ID case-sensitively, then case-insensitively
 (unless STRICT-CASE is t).  If ID is nil, we find the first node
 which its class is one of chapter, section, subsection, subsubsection."
  ;; We must find a chapter, section, subsection or subsubsection to
  ;; be NODE, and must not find an arbitrary node that contains the
  ;; chapter, section, etc that we actually want.  That is because we
  ;; use ‘html-info--dom-remove-subsections’ to remove any chapter etc
  ;; that’s _under_ NODE.  So if the target chapter etc is contained
  ;; in NODE rather than being NODE, it will be removed.  And that’s
  ;; bad.  If ID is non-nil, NODE must be a chapter etc.  If ID is
  ;; nil, we find the first chapter etc and use that (instead of the
  ;; top-level DOM).
  (let ((dom (html-info--get-dom file)))
    ;; If we are visiting the Top node, directly find shortcontents,
    ;; skip ‘dom-by-id’, because it traverses the whole DOM, and Top
    ;; node could be very large (contains the entire manual when the
    ;; manual is single-file).
    (if (or (equal (file-name-base file) "index")
            (equal id "Top"))
        (let* ((short-toc
                (html-info--dom-bfs dom
                  (lambda (node)
                    (equal (dom-attr node 'class) "shortcontents"))))
               (long-toc
                (html-info--dom-bfs dom
                  (lambda (node)
                    (equal (dom-attr node 'class) "contents"))))
               (links (dom-by-tag short-toc 'a))
               (real-links
                (mapcar
                 (lambda (anchor)
                   (let ((id (substring (dom-attr anchor 'href) 1)))
                     `(li nil ,@(dom-by-id long-toc id))))
                 links)))
          `(ul nil ,@real-links))
      (if id
          (if-let ((node
                    (or (let ((case-fold-search t))
                          (car (dom-by-id dom (format "^%s$" id))))
                        (if strict-case nil
                          (let ((case-fold-search nil))
                            (car (dom-by-id dom (format "^%s$" id))))))))
              (html-info--dom-remove-subsections node)
            nil)
        ;; ID is nil, find the first chapter, section, etc.  Search
        ;; breadth-first.
        (html-info--dom-remove-subsections
         (html-info--dom-bfs dom
           (lambda (node)
             (member (dom-attr node 'class)
                     html-info--dom-classes-of-node))))))))

(defun html-info--find-node (current-file node &optional strict-case)
  "Find the NODE, return its DOM and file path.

Specifically, return (DOM . PATH).

NODE can be either a string (node name) or (FILE . ID), where
FILE is the full path to the HTML Info file and ID is the node’s
ID.  CURRENT-FILE is the full path to the currently displayed HTML
Info file.

First looks for a case-sensitive match for the node part of
NODE-NAME; if none is found it then tries a case-insensitive
match \(unless STRICT-CASE is non-nil).

Return nil if no node is found."
  (if-let ((node-dom
            (if (stringp node)
                (html-info--find-node-1
                 current-file (html-info--expand-node-name node)
                 strict-case)
              nil)))
      (cons node-dom current-file)
    (pcase node
      ((pred stringp)
       (if-let ((path (expand-file-name
                       (concat (html-info--expand-node-name node) ".html")
                       (file-name-directory current-file)))
                (node-dom
                 (html-info--find-node-1 path nil strict-case)))
           (cons node-dom path)
         nil))
      (`(,file . ,id)
       (html-info--check-full-path file)
       (if-let ((node-dom (html-info--find-node-1 file id strict-case)))
           (cons node-dom file)
         nil)))))

(defun html-info-goto-node (node &optional fork strict-case)
  "Go to Info node named NODE.

TODO: (FILENAME)NODE-NAME.
TODO: Completion.
TODO: Empty NODE-NAME -> top node.
TODO: FORK as a string.

NODE can be either a string (node name) or (FILE . ID), where
FILE is the full path to the HTML Info file and ID is the node’s
HTML id.

FILE is the full path to the file in where the node resides.  If
omitted, FILE defaults to the currently visited Info file.

This function first looks for a case-sensitive match for the node part
of NODE-NAME; if none is found it then tries a case-insensitive match
\(unless STRICT-CASE is non-nil)."
  (pcase-let ((`(,node-dom . ,node-file)
               (html-info--find-node
                html-info--current-file node strict-case))
              (info-buffer (get-buffer-create
                            (if fork
                                (format "*html info-%s*"
                                        (if (stringp node)
                                            node (cdr node)))
                              "*html info*"))))
    (when (null node-dom)
      (user-error "No such node or anchor: %s" node))
    (switch-to-buffer info-buffer)
    (pcase-let*
        ((`(,final-dom . ,navi)
          (html-info--dom-remove-header node-dom))
         (inhibit-read-only t)
         (shr-map html-info--shr-override-map)
         (breadcrumb (if navi
                         (apply #'html-info--breadcrumbs navi)
                       "TODO breadcrumb for Top")))
      (erase-buffer)
      (if Info-use-header-line
          (setq header-line-format breadcrumb)
        (insert breadcrumb))
      (shr-insert-document final-dom)
      (unless (derived-mode-p 'html-info-mode)
        (html-info-mode))
      (setq html-info--current-file node-file)
      (setq html-info--navigation navi)
      (goto-char (point-min)))))

(define-button-type 'html-info-navigation-button
  'action #'html-info--button-follow-link
  'mouse-action #'html-info--button-follow-link
  'help-echo "Go to node"
  'follow-link t)

(defun html-info--breadcrumbs (next prev up)
  "Generate the breadcrumbs.
NEXT, PREV and UP are anchor nodes of the previous, next and up
node.  See ‘html-info--navigation’."
  (cl-labels ((node-name (dom)
                (if (equal (dom-attr dom 'href) "index.html")
                    "Top" (dom-text dom))))
    (with-temp-buffer
      (when next
        (insert "Next: ")
        (insert-text-button (node-name next)
                            'type 'html-info-navigation-button
                            'target (dom-attr next 'href)))
      (when (and next prev)
        (insert ", "))
      (when prev
        (insert "Prev: ")
        (insert-text-button (node-name prev)
                            'type 'html-info-navigation-button
                            'target (dom-attr prev 'href)))
      (insert ", Up: ")
      (insert-text-button (node-name up)
                          'type 'html-info-navigation-button
                          'target (dom-attr up 'href))
      (buffer-string))))

(defun html-info--follow-link (url)
  "Go to the node represented by URL.
URL could be “file”, or “file#anchor” or “#anchor.”"
  ;; The "file#anchor" case.
  (cl-labels ((expand (file)
                (expand-file-name
                 file (file-name-directory html-info--current-file))))
    (cond ((string-match (rx (seq bol (group (+ any)) "#"
                                  (group (+ any)) eol))
                         url)
           (html-info-goto-node
            (cons (expand (match-string 1 url))
                  (match-string 2 url))))
          ;; The "#anchor" case.
          ((string-match (rx (seq bol "#" (group (+ any)) eol)) url)
           (html-info-goto-node
            (cons html-info--current-file (match-string 1 url))))
          ;; The "file" case.
          (t (html-info-goto-node (cons (expand url) nil))))))

(defun html-info--button-follow-link (button)
  "Follow the link in BUTTON."
  (interactive)
  (html-info--follow-link (button-get button 'target)))

(defun html-info--follow-link-at-point ()
  "Follow the link at point."
  (interactive)
  (let ((url (get-text-property (point) 'shr-url)))
    (if (not url)
        (user-error "No link at point")
      (html-info--follow-link url))))

(defun html-info-next ()
  "Go to the “next” node, staying on the same hierarchical level.
This command doesn’t descend into sub-nodes, like
\\<html-info-mode-map>\\[html-info-forward-node] does."
  (interactive)
  (if-let ((next (nth 0 html-info--navigation)))
      (html-info--follow-link (dom-attr next 'href))
    (user-error "Node has no next")))

(defun html-info-prev ()
  "Go to the “previous” node, staying on the same hierarchical level.
This command doesn't go up to the parent node, like
\\<html-info-mode-map>\\[html-info-backward-node] does."
  (interactive)
  (if-let ((prev (nth 1 html-info--navigation)))
      (html-info--follow-link (dom-attr prev 'href))
    (user-error "Node has no previous")))

(defun html-info-up ()
  "Go to the superior node, staying on the same hierarchical level.
This command doesn’t descend into sub-nodes, like
\\<html-info-mode-map>\\[html-info-forward-node] does."
  (interactive)
  (html-info--follow-link
   (dom-attr (nth 2 html-info--navigation) 'href)))

(defvar html-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'html-info-next)
    (define-key map "p" #'html-info-prev)
    (define-key map "u" #'html-info-up)
    map)
  "Keymap for ‘html-info-mode’.")

(define-derived-mode html-info-mode special-mode
  "HTML Info"
  "HTML Info mode lets you browse HTML Info files.
Documentation in Info is divided into \"nodes\", each of which
discusses one topic and contains references to other nodes which
discuss related topics.  Info has commands to follow the
references and show you other nodes."
  (setq case-fold-search t)
  (setq buffer-read-only t)
  ;; TODO: tool-bar
  ;; TODO: desktop-save-buffer
  ;; TODO: revert buffer
  ;; TODO: bookmark-make-record-function
  ;; TODO: mode-line
  (setq-local widen-automatically nil)
  (when Info-standalone
    (add-hook 'quit-window-hook 'save-buffers-kill-emacs nil t)))

(defun html-info-lispref ()
  "Open Emacs Lisp Reference Info."
  (interactive)
  (let ((lispref (expand-file-name "doc/lispref/elisp.html"
                                   source-directory)))
    (if (file-exists-p lispref)
        (let ((html-info--current-file
               (if (file-directory-p lispref)
                   (expand-file-name "index.html" lispref)
                 lispref)))
          (html-info-goto-node "Top"))
      (error "Cannot find lispref at %s" lispref))))

(provide 'html-info)

;;; html-info.el ends here

;; Local Variables:
;; sentence-end-double-space: t
;; End:
