(setq lexical-binding t)

(defmacro mines-save-excursion (&rest excursion)
  "Save-excursion, but restores point to the same numeric value. That is, if text has been deleted before point, point would have moved leftwards when restored by save-excursion but not by mines-save-excursion. That also means it might be restored over different text than it was over when saved."
  (let ((saved-point (make-symbol "saved-point")))
    `(let ((,saved-point (point))
           (retval ,(cons 'save-current-buffer excursion)))
       (goto-char ,saved-point)
       retval)))

(defun mines-replace-char (char face)
  "replaces whatever point is over with char and sets face as the font-lock-face. Works in read-only buffers."
  (mines-save-excursion
   (let ((buffer-read-only nil)
         (mouse-face (get-text-property (point) 'mouse-face)))
     (delete-char 1)
     (insert char)
     (put-text-property (- (point) 1) (point) 'font-lock-face face)
     (put-text-property (- (point) 1) (point) 'mouse-face mouse-face))))

(defun mines-make-field ()
  "Makes a playing field for minesweeper"
  (let* ((py (mines-point-y))
         (px (mines-point-x))
         (field (make-bool-vector (* mines-max-y mines-max-x) nil))
         (yx (* mines-max-y mines-max-x))
         (neighbors (if (eq mines-start 'safe-neighbors)
                        (mines-neighbor-indices) nil))
         (section2 (length neighbors))
         (section1 (- yx section2 1)))
    ;; Put mines in start of array
    (dotimes (i mines-mine-num)
      (aset field i t))
    ;; Scatter the mines randomly but don't move mines to the last spaces
    (dotimes (i section1)
      (let* ((r (+ i (random (- section1 i))))
             (tmp (aref field i)))
        (aset field i (aref field r))
        (aset field r tmp)))
    ;; exchange the neighbors with some of the last mine-free spaces
    (seq-reduce
     (lambda (i elm)
       (let* ((idx (+ i section1))
              (tmp (aref field idx))
              (neighbor (mines-2d-to-arrpos (car elm) (cdr elm))))
         (aset field idx (aref field neighbor))
         (aset field neighbor tmp)
         (1+ i)))
     neighbors 0)
    ;; Start-position and last elt switched to make sure first click
    ;; isn't a mine
    (let* ((pos (mines-2d-to-arrpos py px))
           (tmp (aref field pos))
           (last (- yx 1)))

      (aset field pos (aref field last))
      (aset field last tmp)
      mines-board)
    ;; Set variables
    (setq mines-remaining-fields (- yx mines-mine-num))
    (setq mines-board (plist-put (plist-put mines-board 'arr field)
                                 'saved-pos
                                 (mines-2d-to-bufpos py px)))
    (setq mines-start-time (current-time))
    ;; Return the created board
    mines-board))

(defun mines-point-y ()
  "The line point is over, zero-indexed"
  (/ (- (point) 1) (+ mines-max-x 1)))

(defun mines-point-x ()
  "The column point is in, zero-indexed"
  (mod (- (point) 1) (+ mines-max-x 1)))
;; Could use point

(defun mines-2d-to-arrpos (y x)
  "Converts 2d coordinates to the same position in the flattened version of the array"
  (+ (* y mines-max-x) x))

;; adds one to max-x to account for newlines before point in the buffer
(defun mines-2d-to-bufpos (y x)
  (+ x (* y (+ 1 mines-max-x)) 1))

;; Should it use (plist-get 'arr)?
(defun mines-aset (field y x newelt)
  "Sets position (y,x) row-major in the array in property arr of the plist field to newelt"
  (aset (plist-get field 'arr) (mines-2d-to-arrpos y x) newelt))

;; Should it use (plist-get 'arr)?

(defun mines-aref (field y x)
  "Returns the value at position (y,x) row-major in the array in property arr of the plist field"
  (aref (plist-get field 'arr) (mines-2d-to-arrpos y x)))

(defun mines-neighbor-indices ()
  "Returns the indices of the valid neighbors of point. Indices are of the form (y . x)"
  (let* ((y (mines-point-y))
         (x (mines-point-x))
         (yp (+ y 1)) (ym (- y 1))
         (xp (+ x 1)) (xm (- x 1))
         (all-indices `((,ym . ,xm) (,ym . ,x) (,ym . ,xp)
                        (,y . ,xm) (,y . ,xp)
                        (,yp . ,xm) (,yp . ,x) (,yp . ,xp))))


    (seq-filter (lambda (elt) (and (< -1 (car elt) mines-max-y)
                                   (< -1 (cdr elt) mines-max-x)))
                all-indices)))

(defmacro mines-do-neighbors (&rest body)
  "Loops over the neighbors and sets point accordingly."
  (let ((yx (make-symbol "yx")))
    `(mines-save-excursion
      (dolist (,yx (mines-neighbor-indices))
        (goto-char (mines-2d-to-bufpos (car ,yx) (cdr ,yx)))
        ,(cons 'progn body)))))

(defun mines-map-neighbors (f)
  "Maps over the neighbors and sets point accordingly. The mapping function f takes no argument as it is meant to read point."
  (mines-save-excursion
   (let ((acclist nil))
     (mines-do-neighbors (setq acclist (cons (funcall f) acclist)))
     (reverse acclist))))

(defun mines-reduce-neighbors (f initval)
  "Reduces over the neighbors and sets point accordingly. The reducing function f only takes the accumulator argument as it is meant to read poitn"
  (mines-save-excursion
   (let ((acc initval))
     (mines-do-neighbors (setq acc (funcall f acc)))
     acc)))

(defun mines-neighbor-count ()
  "Returns the number of neighboring mines"
  (mines-reduce-neighbors
   (lambda (acc)
     (+ acc (if (mines-aref mines-board (mines-point-y) (mines-point-x))
                1 0)))
   0))

(defun mines-draw-field ()
  "Draws an empty minesweeper field"
  (mines-save-excursion
   (let ((buffer-read-only nil))
     (dotimes (i (* mines-max-y mines-max-x))
       (when (and (/= 0 i) (= 0 (mod i mines-max-x)))
         (newline)
         (put-text-property (- (point) 1) (point) 'mouse-face 'mines-newline))
       (insert mines-empty-char)
       (put-text-property (- (point) 1) (point) 'font-lock-face 'mines-empty)
       (put-text-property (- (point) 1) (point) 'mouse-face
                          (if (evenp i) 'mines-mouse-1 'mines-mouse-2)))
     (newline)
     (put-text-property (- (point) 1) (point) 'mouse-face 'mines-newline))))

(defun mines-retry (&optional prefix-arg)
  "Retries the same field again. Restores point to where it was when this field was first tried unless the prefix argument is specified."
  (interactive (list current-prefix-arg))
  (let ((saved-pos (if prefix-arg (point) (plist-get mines-board 'saved-pos))))
    (let ((buffer-read-only nil)) (erase-buffer))
    (mines-draw-field)
    (goto-char saved-pos)))

(defun mines-new-game (&optional prefix-arg)
  "Starts a new game of minesweeper. If prefix-arg is given it queries for new dimensions and number of mines"
  (interactive (list current-prefix-arg))
  (setq mines-start-time nil)
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

(defun mines-sweep-empty ()
  "Reveals whether there is a mine under point"
  (mines-save-excursion
   (let* ((retval (if (mines-aref mines-board (mines-point-y) (mines-point-x))
                      (progn (setq mines-remaining-fields nil)
                             mines-bomb-char)
                    ;; It's ugly that mines-remaining-fields is set in the definition of retval
                    (when mines-remaining-fields
                      (setq mines-remaining-fields (- mines-remaining-fields 1))
                      (when (= mines-remaining-fields 0)
                        (setq mines-finish-time (current-time))))
                    (mines-neighbor-count)))
          (char (cond ((equal retval mines-bomb-char) mines-bomb-char)
                      ((equal retval 0) mines-zero-char)
                      (t (+ 48 retval))))
          (face (if (eq retval mines-bomb-char) 'mines-bomb 'mines-num)))
     (mines-replace-char char face)
     retval)))

(defun mines-sweep-empties ()
  "Reveals whether there is a mine under point. If there are no neighboring mines, it also reveals those the neighbors, and continues to do so iteratively"
  (mines-save-excursion
   (let ((indices `((,(mines-point-y) . ,(mines-point-x)))))
     (while (consp indices)
       (destructuring-bind (y . x) (car indices)
         (setq indices (cdr indices))
         (goto-char (mines-2d-to-bufpos y x))
         (when (char-equal mines-empty-char (char-after))
           (when (= 0 (mines-sweep-empty))
             (setq indices (append (mines-neighbor-indices) indices)))))))))


(defun mines-sweep-neighbors ()
  "Reveals mines under all neighbors if used on a number and the right number of flags is set (or bombs revealed)."
  (mines-save-excursion
   (let ((flag-count (mines-reduce-neighbors
                      (lambda (acc)
                        (+ acc (if (or (equal (char-after) mines-flag-char)
                                       (equal (char-after) mines-bomb-char))
                                   1 0)))
                      0)))
     (if (= flag-count (mines-neighbor-count))
         (mines-map-neighbors #'mines-sweep-empties)))))

(defun mines-sweep ()
  "Reveal mines"
  (interactive)
  (unless (plist-get mines-board 'arr)
    (mines-make-field)
    (setq mines-start-time (current-time)))
  (cond
   ((equal (char-after) mines-empty-char) (mines-sweep-empties))
   ((and (char-after) (<= #x31 (char-after) #x39)) (mines-sweep-neighbors)))
  (when (and mines-finish-time (= 0 mines-remaining-fields))
    (let ((time (time-subtract mines-finish-time mines-start-time)))
      (princ (format-time-string "hej %s.%3N" time)))
    (setq mines-finish-time nil
          mines-remaining-fields nil)))

(defun mines-flag-single (&optional on-off)
  "If on-off is 'on' it places a flag under point. If it is 'off' it removes the flag under point. If on-off is anything else, it toggles the flag."
  (interactive)
  (mines-save-excursion
   (let ((char (char-after)))
     (cond
      ((and (equal char mines-empty-char) (not (equal on-off 'off)))
       (mines-replace-char mines-flag-char 'mines-flag))
      ((and (equal char mines-flag-char) (not (equal on-off 'on)))
       (mines-replace-char mines-empty-char 'mines-empty))))))

(defun mines-flag ()
  "Toggle the flag under point. If used on a number it will place flags in all empty neighboring spaces, unless they're all filled with flags, in which case it will remove them."
  (interactive)
  (mines-save-excursion
   (cond ((or (equal (char-after) mines-empty-char)
              (equal (char-after) mines-flag-char))
          (mines-flag-single))
         ((<= 49 (char-after) 57)
          (let ((empties 0)
                (toggle 'on))
            (mines-do-neighbors
             (when (equal (char-after) mines-empty-char)
               (setq empties (+ 1 empties))))
            (when (= 0 empties) (setq toggle 'off))
            (mines-do-neighbors
             (mines-flag-single toggle)))))))

(defun mines-mousify (f event)
  (let ((mouse-point (posn-point (event-end event))))
    (when mouse-point
      (mines-save-excursion
       (with-current-buffer "Minesweeper"
         (goto-char mouse-point)
         (funcall f))))))

(defun mines-sweep-mouse (event)
  (interactive "e")
  (mines-mousify 'mines-sweep event))

(defun mines-flag-mouse (event)
  (interactive "e")
  (mines-mousify 'mines-flag event))

(defun mines-new-game-mouse (event)
  (interactive "e")
  (mines-mousify 'mines-new-game event))

(defvar mines-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "SPC") 'mines-sweep)
    (define-key map (kbd "`") 'mines-flag)
    (define-key map (kbd "C-c r") 'mines-retry)
    (define-key map (kbd "C-c n") 'mines-new-game)
    (define-key map (kbd "<mouse-1>") 'mines-sweep-mouse)
    (define-key map (kbd "<drag-mouse-1>") 'mines-sweep-mouse)
    (define-key map (kbd "<down-mouse-1>") 'ignore)
    (define-key map (kbd "<mouse-3>") 'mines-flag-mouse)
    (define-key map (kbd "<drag-mouse-3>") 'mines-flag-mouse)
    (define-key map (kbd "<down-mouse-3>") 'ignore)
    (define-key map (kbd "<mouse-2>") 'ignore)
    (define-key map (kbd "<down-mouse-2>") 'ignore)
    (define-key map (kbd "<double-down-mouse-2>") 'mines-new-game-mouse)

    map))

(define-derived-mode mines-mode special-mode "Minesweeper"
  "The major mode for my minesweeper game!"
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
  (set (make-local-variable 'mines-remaining-fields) nil)
  (set (make-local-variable 'mines-start) 'safe-neighbors)
  (make-local-variable 'mines-start-time)
  (set (make-local-variable 'mines-finish-time) nil))

(defun minesweeper ()
  "Play minesweeper!"
  (interactive)
  (switch-to-buffer "Minesweeper")
  (let ((buffer-read-only nil)) (erase-buffer))
  (mines-mode)
  (mines-draw-field))

(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "SPC") 'mines-sweep)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "<tab>") 'mines-flag)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "<down-mouse-1>") 'ignore)))
(add-hook 'mines-mode-hook
          (lambda () (define-key evil-normal-state-local-map (kbd "<mouse-2>") 'ignore)))


(defface mines-bomb '((t . (:foreground "red")))
  "Face of the bombs in minesweeper")
(defface mines-flag '((t . (:foreground "yellow")))
  "Face of the flags in minesweeper")
(defface mines-num '((t . nil))
  "Face of the numbers and empty spaces in minesweeper")
(defface mines-empty '((t . nil))
  "Face of the unexplored spaces in minesweper")
(defface mines-mouse-1 '((t . (:background "dark gray")))
  "Face for the field the mouse is hovering over in minesweeper")
(defface mines-mouse-2 '((t . (:background "dark gray")))
  "Face for the field the mouse is hovering over in minesweeper")

(defface mines-newline '((t . nil))
  "")


