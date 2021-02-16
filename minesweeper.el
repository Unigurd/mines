(setq lexical-binding t)

;; Could use point
(defun mines-make-field (py px)
  (let ((field (make-bool-vector (* mines-max-y mines-max-x) nil))
         (yx (* mines-max-y mines-max-x)))
    ;; Put mines in start of array
    (dotimes (i mines-mine-num)
      (aset field i t))
    ;; Scatter the mines randomly
    ;; Last elt of array untouched
    (dotimes (i (- yx 1))
      (let* ((r (mod (+ i (random (- yx i 1))) yx))
             (tmp (aref field i)))
        (aset field i (aref field r))
        (aset field r tmp)))
    (setq mines-board (plist-put (plist-put mines-board 'arr field)
                                 'saved-pos
                                 (mines-2d-to-bufpos py px)))
    (let* ((tmp (mines-aref mines-board py px))
           (last (- (* mines-max-y mines-max-x) 1)))
      ;; Start-position and last elt switched to make sure first click
      ;; isn't a mine
      (mines-aset mines-board py px (aref field last))
      (aset field last tmp)
      mines-board)))

(defun mines-2d-to-arrpos (y x)
  (+ (* y mines-max-x) x))

;; adds one to max-x to account for newlines before point in the buffer
(defun mines-bufpos-to-2d (p)
  (let ((x+1 (+ 1 mines-max-x))
        (p-1 (- p 1)))
    (cons (/ p-1 x+1) (mod p-1 x+1))))

(defun mines-2d-to-bufpos (y x)
  (+ x (* y (+ 1 mines-max-x)) 1))

(defun mines-aset (field y x newelt)
  (aset (plist-get field 'arr) (mines-2d-to-arrpos y x) newelt))

(defun mines-aref (field y x)
  (aref (plist-get field 'arr) (mines-2d-to-arrpos y x)))

;; Uses 2d coords
(defun mines-neighbor-indices (board y x)
  (let* ((yp (+ y 1)) (ym (- y 1))
         (xp (+ x 1)) (xm (- x 1))
         (all-indices `((,ym . ,xm) (,ym . ,x) (,ym . ,xp)
                        (,y . ,xm) (,y . ,xp)
                        (,yp . ,xm) (,yp . ,x) (,yp . ,xp))))
    (seq-filter (lambda (elt) (and (< -1 (car elt) mines-max-y)
                                   (< -1 (cdr elt) mines-max-x)))
                all-indices)))

;; transitively uses 2d
(defun mines-neighbor-count (y x)
  (let* ((indices (mines-neighbor-indices mines-board y x))
         (mine-list (mapcar (lambda (elt) (mines-aref mines-board (car elt) (cdr elt))) indices))
         (valid-mines (seq-filter (lambda (elt) elt) mine-list)))
    (length valid-mines)))

(defun mines-draw-field ()
  (save-excursion
    (let ((buffer-read-only nil))
      (dotimes (i (* mines-max-y mines-max-x))
        (if (and (/= 0 i) (= 0 (mod i mines-max-x)))
            (newline))
        (insert mines-empty-char)
        (put-text-property (- (point) 1) (point) 'font-lock-face 'mines-empty)))))

(defun mines-retry (&optional prefix-arg)
  (interactive (list current-prefix-arg))
  (let ((saved-pos (if prefix-arg (point) (plist-get mines-board 'saved-pos))))
    (let ((buffer-read-only nil)) (erase-buffer))
    (mines-draw-field)
    (goto-char saved-pos)))

(defun mines-new-game (&optional prefix-arg)
  (interactive (list current-prefix-arg))
  (when prefix-arg
    (let ((new-x (read-minibuffer "Width: " nil))
          (new-y (read-minibuffer "Height: " nil))
          (new-mines (read-minibuffer "Mines: " nil)))
      (cond ((integerp new-x)
             (setq mines-max-x new-x))
            ;; These cases aren't reached because we read s-exps instead of strings
            ((equal "" new-x))
            (t (error "Not an integer: %s" new-x)))
      (cond ((integerp new-y)
             (setq mines-max-y new-y))
            ((equal "" new-y))
            (t (error "Not an integer: %s" new-y)))
      (cond ((integerp new-mines)
             (setq mines-mine-num new-mines))
            ((equal "" new-mines))
            (t (error "Not an integer: %s" new-mines)))))
  (mines-retry t)
  (plist-put mines-board 'arr nil))

;; needs 1d
(defun mines-sweep-empty ()
  ;; Point might be different than (y,x), so can't use char-after
  (let ((buffer-read-only nil)
        (yx (mines-bufpos-to-2d (point))))
    (save-excursion
      (delete-char 1)
      (let ((retval (if (mines-aref mines-board (car yx) (cdr yx))
                        mines-bomb-char
                      (mines-neighbor-count (car yx) (cdr yx)))))
        (insert (cond ((equal retval mines-bomb-char) mines-bomb-char)
                      ((equal retval 0) mines-zero-char)
                      (t (+ 48 retval))))
        (put-text-property (- (point) 1) (point) 'font-lock-face
                           (if (eq retval mines-bomb-char) 'mines-bomb 'mines-num))
        retval))))

;; ((= new-char mines-bomb-char)
;; (princ (format-time-string "%s.%2N"
;;                            (time-subtract

(defun mines-sweep-empties ()
  ;; Deletes characters so save-excursion doesn't work properly
  (let* ((saved-point (point))
         (yx (mines-bufpos-to-2d saved-point))
         (indices `((,(car yx) . ,(cdr yx)))))
    (while (consp indices)
      (destructuring-bind (y . x) (car indices)
        (setq indices (cdr indices))
        (goto-char (mines-2d-to-bufpos y x))
        (when (char-equal mines-empty-char (char-after))
          (when (= 0 (mines-sweep-empty))
            (setq indices (append (mines-neighbor-indices mines-board y x) indices))))))
    (goto-char saved-point)))

(defun mines-sweep-neighbors (y x)
  (let* ((saved-point (point))
         (indices (mines-neighbor-indices mines-board y x))
         (flag-count (seq-reduce (lambda (acc elt)
                                   (goto-char (mines-2d-to-bufpos (car elt) (cdr elt)))
                                   (+ acc (if (equal (char-after) mines-flag-char) 1 0)))
                                 indices 0)))
    (if (= flag-count (mines-neighbor-count y x))
        (mapcar (lambda (elt)
                  (goto-char (mines-2d-to-bufpos (car elt) (cdr elt)))
                  (mines-sweep-empties))
                indices))
    (goto-char saved-point)))

(defun mines-sweep ()
  (interactive)
  (destructuring-bind
      (y . x) (mines-bufpos-to-2d (point))
    (unless (plist-get mines-board 'arr)
      (mines-make-field y x)
      (setq mines-start-time (current-time)))
    (cond
     ((equal (char-after) mines-empty-char) (mines-sweep-empties))
     ((<= #x31 (char-after) #x39) (mines-sweep-neighbors y x)))))

(defun mines-flag ()
  (interactive)
  (let ((buffer-read-only nil)
        (saved-point (point))
        (char (char-after)))
    (cond
     ((equal char mines-empty-char)
      (delete-char 1)
      (insert mines-flag-char)
      (put-text-property saved-point (+ saved-point 1) 'font-lock-face 'mines-flag))
     ((equal char mines-flag-char)
      (delete-char 1)
      (insert mines-empty-char)
      (put-text-property saved-point (+ saved-point 1) 'font-lock-face 'mines-empty)))
    (goto-char saved-point)))

(defvar mines-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "SPC") 'mines-sweep)
    (define-key map (kbd "`") 'mines-flag)
    (define-key map (kbd "C-c r") 'mines-retry)
    (define-key map (kbd "C-c n") 'mines-new-game)
    map))

(define-derived-mode mines-mode special-mode "Minesweeper"
  "A simple minesweeper game."
  ;; keymap
  ;; syntax table
  ;; buffer-local vars
  (set (make-local-variable 'mines-board) nil)
  (set (make-local-variable 'mines-max-y) 16)
  (set (make-local-variable 'mines-max-x) 30)
  (set (make-local-variable 'mines-mine-num) 99)
  (set (make-local-variable 'mines-empty-char) ?.)
  (set (make-local-variable 'mines-zero-char) ?\s)
  (set (make-local-variable 'mines-bomb-char) ?X)
  (set (make-local-variable 'mines-flag-char) ?f)
  (set (make-local-variable 'mines-remaining-mines) nil)
  (make-local-variable 'mines-start-time))

(defun minesweeper ()
  (interactive)
  (switch-to-buffer "Minesweeper")
  (let ((buffer-read-only nil)) (erase-buffer))
  (mines-mode)
  (mines-draw-field))

(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "SPC") 'mines-sweep)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "<tab>") 'mines-flag)))

(defface mines-bomb '((t . (:foreground "red")))
  " face of the bombs in minesweeper")
(defface mines-flag '((t . (:foreground "yellow")))
  " face of the flags in minesweeper")
(defface mines-num '((t . nil))
  " face of the numbers and empty spaces in minesweeper")
(defface mines-empty '((t . nil))
  "Face of the unexplored spaces in minesweper")
