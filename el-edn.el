;;; el-edn.el --- EDN data format goodness

;; Copyright (C) 2013 Shaun Gilchrist

;; Author: Shaun Gilchrist <shaunxcode@gmail.com>
;; Maintainer: Shaun Gilchrist <shaunxcode@gmail.com>
;; Created: 11 Jul 2013
;; Keywords: EDN, Datomic, Clojure
;; Version: 0.0.1

(require 's)

(defun edn--only-containing-chars (str char-string)
  (let ((found nil))
    (mapc (lambda (c)
	    (if (and (> (length c) 0) (not (s-contains-p c char-string)))
	        (setq found t)))
	  (split-string str ""))
    (not found)))

(defun edn--valid-symbol (str)
  (let ((ucstr (upcase str))
        (fchar (substring str 0 1)))
    (and
     ;;can only have valid chars
     (edn--only-containing-chars ucstr "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ.*+!-_?$%&=:#/")
     ;;cannot start with a number
     (not (s-contains-p fchar "0123456789"))
     ;;if first char is - + or . next must not be numeric
     (not (and (s-contains-p fchar "-+.")
	       (> (length ucstr) 1)
	       (s-contains-p (substring ucstr 1 2) "0123456789")))
     ;;only allow one slash per symbol
     (<= (s-count-matches "/" str) 1))))

(defun edn--valid-keyword (str)
  (and (string-equal (substring str 0 1) ":")
       (edn--valid-symbol (substring str 1))))

(defun edn--valid-nil (str)
  (string-equal str "nil"))

(defun edn--valid-bool (str)
  (or (string-equal str "true")
      (string-equal str "false")))

(defun edn--valid-int (str &optional disallow-sign)
  (progn
    (if (and (s-contains-p (substring str 0 1) "-+")
	     (> (length str) 1)
	     (not disallow-sign))
	(setq str (substring str 1)))
    (if (s-contains-p (substring str -1) "NM")
	(setq str (substring str 0 -1)))
    (edn--only-containing-chars str "0123456789")))

(defun edn--valid-float (str)
  (let ((front "")
	(back "")
	(e-pos 0)
	(period-pos (string-match "\\." str))
	(result t))
    (if period-pos
	(progn
	  (setq front (substring str 0 period-pos))
	  (setq back (substring str (+ 1 period-pos))))
      (setq back str))
    (if (or (= (length front) 0)
	    (edn--valid-int front))
	(progn
	  (setq e-pos (string-match "E" back))
	  (if e-pos
	      (if (or (= e-pos (- (length back) 1))
		      (not (edn--valid-int (substring back 0 e-pos) t))
		      (not (edn--valid-int (substring back (+ e-pos 1)))))
		  (setq result nil))
            (progn
              (if (s-contains-p (substring back -1) "M")
                  (setq back (substring back 0 -1)))
              (if (not (edn--valid-int back t))
                  (setq result nil))))))
    result))

(defun edn--valid-char (str)
  (and (string-equal (substring str 0 1) "\\")
       (= (length str) 2)))

(defun edn--token (type line value)
  (let ((token (make-hash-table :test 'equal)))
    (puthash 'type type token)
    (puthash 'line line token)
    (puthash 'value value token)
    token))

(defun edn--node (type line value)
  (let ((node (make-hash-table :test 'equal)))
    (puthash 'type type node)
    (puthash 'line line node)
    (puthash 'value value node)
    node))

(defun edn--handle-atom (token)
  (let ((val (gethash 'value token)))
    (edn--node
     (cond ((edn--valid-nil val) 'EdnNil)
           ((eq (gethash 'type token) 'String) 'EdnString)
           ((edn--valid-char val) 'EdnChar)
           ((edn--valid-bool val) 'EdnBool)
           ((edn--valid-keyword val) 'EdnKeyword)
           ((edn--valid-symbol val) 'EdnSymbol)
           ((edn--valid-int val) 'EdnInt)
           ((edn--valid-float val) 'EdnFloat)
           (t (error (concat "unknown type for " val))))
     (gethash 'line token)
     val)))

(defun edn--handle-collection (token values)
  (let ((val (gethash 'value token)))
    (edn--node
     (cond ((string-equal val "(") 'EdnList)
           ((string-equal val "[") 'EdnVector)
           ((string-equal val "{") 'EdnMap))
     (gethash 'line token)
     values)))

(defun edn--handle-tagged (token value)
  (let ((tag-name (substring (gethash 'value token) 1)))
    (edn--node
     (cond ((string-equal tag-name "_") 'EdnDiscard)
           ((string-equal tag-name "") 'EdnSet)
           (t 'EdnTagged))
     (gethash 'line token)
     (let ((content (make-hash-table :test 'equal)))
       (if (string-equal tag-name "")
           (progn (puthash 'type 'EdnSet content)
                  (puthash 'value (gethash 'value value) content))
         (puthash 'tag (edn--node 'EdnSymbol (gethash 'line token) tag-name) content)
         (puthash 'value value content))

       content))))

(defun edn--lex (edn-string)
  (let ((escaping nil)
        (in-string nil)
        (string-content "")
        (in-comment nil)
        (token "")
        (paren "")
        (escape-char "\\")
        (string-char "\"")
        (line 1)
        (tokens '()))
    (cl-labels
        ((create-token (type line value)
                       (progn
                         (setq tokens
                               (append tokens (list (edn--token type line value))))
                         (setq token "")
                         (setq string-content ""))))
      (mapc
       (lambda (c)
         (progn
           ;;keep track of line
           (if (or (string-equal c "\n")
                   (string-equal c "\r"))
               (setq line (+ line 1)))
           (cond
            ((string-equal c "") nil)
	    ;;comments
	    (in-comment
	     (if (string-equal c "\n")
		 (progn
		   (setq in-comment nil)
		   (if (> (length token) 0)
		       (create-token 'Atom line token)))))
            ;;strings
            ((and (string-equal c string-char) (not escaping))
             (if in-string
                 (progn
                   (setq in-string nil)
                   (create-token 'String line string-content))
               (setq in-string t)))
            ;;build-string
            (in-string
             (if (and (string-equal c escape-char) (not escaping))
                 (setq escaping t)
               (progn
                 (if escaping
                     (progn
                       (setq escaping nil)
                       (if (s-contains-p c "tnfr")
                           (setq string-content (concat string-content escape-char)))))
                 (setq string-content (concat string-content c)))))
            ;;paren or whitespace
            ((s-contains-p c "()[]{}\t\n\r ,")
             (progn
               (if (> (length token) 0)
                   (create-token 'Atom line token))
               (if (s-contains-p c "()[]{}")
                   (create-token 'Paren line c))))
            (t (progn
                 (if escaping
                     (setq escaping nil)
                   (if (string-equal c escape-char)
                       (setq escaping t)))
                 (if (or (string-equal token "#_")
                         (and (= (length token) 2)
                              (string-equal (substring token 0 1) escape-char)))
                     (create-token 'Atom line token))
                 (setq token (concat token c)))))))
       (split-string edn-string ""))
      (if (> (length token) 0)
          (create-token 'Atom line token))
      tokens)))

(defun edn--read (edn-string1)
  (let ((tokens (edn--lex edn-string1)))
    (cl-labels
        ((read-ahead (token)
                     (let ((type (gethash 'type token))
                           (val (gethash 'value token)))
                       (cond
                        ((eq type 'Paren)
                         (let ((L '())
                               (close-paren (cond ((string-equal val "(") ")")
                                                  ((string-equal val "[") "]")
                                                  ((string-equal val "{") "}")))
                               (next-token nil))
                           (catch 'break
                             (while tokens
                               (setq next-token (pop tokens))
                               (if (string-equal (gethash 'value next-token) close-paren)
                                   (throw 'break (edn--handle-collection token L))
                                 (setq L (append L (list (read-ahead next-token))))))
                             (error "Unexpected end of list"))))
                        ((s-contains-p val ")]}")
                         (progn
                           (print (list token tokens))
                           (error "Unexpected closing paren")))
                        ((and (> (length val) 0)
                              (string-equal (substring val 0 1) "#"))
                         (edn--handle-tagged token (read-ahead (pop tokens))))
                        (t (edn--handle-atom token))))))
      (read-ahead (pop tokens)))))

(defun edn--node-is-collection (node)
  (member (gethash 'type node)
          '(EdnList EdnSet EdnVector EdnMap)))

(defun edn-pprint (node)
  (let ((type (gethash 'type node))
        (value (gethash 'value node)))
    (cond ((edn--node-is-collection node)
           (let ((vals (join-string (mapcar 'edn-pprint value))))
             (cond ((eq type 'EdnList) (concat "(" vals ")"))
                   ((eq type 'EdnVector) (concat "[" vals "]"))
                   ((eq type 'EdnMap) (concat "{" vals "}")))))
          ((eq type 'EdnString)
           (concat "\"" value "\""))
          ((eq type 'EdnTagged)
           (concat "#"
                   (edn-pprint (gethash 'tag value)) " "
                   (edn-pprint (gethash 'value value))))
          (t value))))

(setq edn--tag-handlers (make-hash-table :test 'equal))
(defun edn--set-tag-handler (tag handler)
  (puthash tag handler edn--tag-handlers))

;;handlers for reification
(setq edn--reify-handlers (make-hash-table :test 'equal))
(defun edn--set-reify-handler (type handler)
  (puthash type handler edn--reify-handlers))

(edn--set-reify-handler 'EdnInt (lambda (val) (string-to-number val)))
(edn--set-reify-handler 'EdnFloat (lambda (val) (string-to-number val)))
(edn--set-reify-handler 'EdnChar (lambda (val) (substring val 1)))
(edn--set-reify-handler 'EdnString (lambda (val) val))
(edn--set-reify-handler 'EdnSymbol (lambda (val) (intern val)))
(edn--set-reify-handler 'EdnKeyword (lambda (val) (intern val)))
(edn--set-reify-handler 'EdnNil (lambda (val) nil))
(edn--set-reify-handler 'EdnBool (lambda (val) (string= val "true")))
(edn--set-reify-handler 'EdnList (lambda (vals) vals))
(edn--set-reify-handler 'EdnVector (lambda (vals) (vconcat vals)))
(edn--set-reify-handler 'EdnSet (lambda (vals) vals))
(edn--set-reify-handler 'EdnMap
                        (lambda (vals)
                          (let ((M (make-hash-table :test 'equal)))
                            (while (> (length vals) 0)
                              (puthash (pop vals) (pop vals) M))
                            M)))

(edn--set-tag-handler 'inst (lambda (val) (date-to-time val)))
(edn--set-tag-handler 'uuid (lambda (val) val))

(defun edn--reify-tagged (value)
  (let* ((tag (edn--reify (gethash 'tag value)))
         (val (edn--reify (gethash 'value value)))
         (handler (or (gethash tag edn--tag-handlers) (lambda (val) value))))
    (funcall handler val)))

(defun edn--reify (node)
  (let* ((type (gethash 'type node))
         (value (gethash 'value node)))
    (when (edn--node-is-collection node)
      (if (listp value)
          (setq value (mapcar 'edn--reify value))
        (setq value (edn--reify value))))
    (if (eq type 'EdnTagged)
        (edn--reify-tagged value)
      (funcall (gethash type edn--reify-handlers) value))))

(defun edn-parse (edn)
  (edn--reify (edn--read edn)))
