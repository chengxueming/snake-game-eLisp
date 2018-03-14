;; Snake game
;; TODO let snake be loner when eat food
;; TODO solve crash when snake beyond the boundary

(defvar snake-buffer-name "*Snake*")


(defun snake_game ()
  "Start playing snake"
  (interactive)
  (switch-to-buffer snake-buffer-name)
  (snake-mode)
  (snake-init))

(define-derived-mode snake-mode special-mode
  "snake"
  (define-key snake-mode-map (kbd "<right>") 'snake-move-right-key)
  (define-key snake-mode-map (kbd "<left>") 'snake-move-left-key)
  (define-key snake-mode-map (kbd "<up>")  'snake-move-up-key)
  (define-key snake-mode-map (kbd "<down>") 'snake-move-down-key))

(defconst *area-height* 20
  "The height of the area")

(defconst *area-width* 40
  "The width of the area")

(defvar *snake-length* 5
  "The length of snake, beginning at 5")

(defvar *snake-body* (list)
  "A list of cells that are part of the snake's body. For
example, ((0 0) (0 1) (0 2)) is a snake of length 3")

(defvar *snake-head* nil
  "Will contain the coordinance of the head e.g (2,0)")

(defvar *food-row* (random *area-height*)
  "food location row count at game area")

(defvar *food-column* (random *area-width*)
  "food location column count at game area")

(defvar *direction* 3
  "direction the snake move in game")

(defun snake-init()
  "Start a new game of snake"
  (setq *snake-body* nil)
  (setq *snake-area*
        (make-vector
         (* *area-height* *area-width*) ?\-))
  (generate-food)
  (setq *snake-timer* (run-with-timer 1 1 'update-game))
  (snake-player-init)
  (snake-set-player)
  (snake-print-area)
  )

(defun snake-move-up-key()
  (interactive)
  (snake-move 1)
  )

(defun snake-move-right-key()
  (interactive)
  (snake-move 2)
  )

(defun snake-move-down-key()
  (interactive)
  (snake-move 3)
  )

(defun snake-move-left-key()
  (interactive)
  (snake-move 4)
  )

(defun snake-move(direction)
  (setq *direction* direction)
    )

(defun update-game()
  "update game main logic"
  (if (eq *direction* 1) (snake-move-up)
    (if (eq *direction* 2) (snake-move-right)
                        (if (eq *direction* 3) (snake-move-down)
                          (if (eq *direction* 4) (snake-move-left)
                            ))))
  (touch-food)
  (snake-set-player)
  (snake-print-area))

(defun kill-timer()
  (interactive)
  (when fireplace--timer
    (cancel-timer fireplace--timer)
  ))

(defun generate-food()
  "generate food random"
  (setq *food-row* (random *area-height*))
  (setq *food-column* (random *area-width*))
  )

(defun snake-print-area ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *area-height*)
      (dotimes(column *area-width*)
        (insert "-"))
      (insert "\n"))

    (dolist (cell *snake-body*)
      (goto-char (point-min))
      (forward-line (1- (nth 0 cell)))
      (forward-char (nth 1 cell))
      (delete-forward-char 1)
      (insert "O"))

    (goto-char (point-min))
    (forward-line (- *food-row* 1))
    (forward-char *food-column*)
    (delete-forward-char 1)
    (insert "X")
    (goto-char (point-max))
    ));; Put cursor outside the area

(defun snake-longer ()
   "make snake longer"
   (interactive)
   (setq *snake-length* (+ *snake-length* 1))
   (snake-init))

(defun print-snake-body ()
  "print snake body"
  (interactive)
  (dolist (cell *snake-body*)
    (message "%s,%s;" (nth 0 cell) (nth 1 cell))))

(defun area-get-square (row column)
  "Get the value in the (row, column) square."
  (elt *snake-area* (+ column (* row *area-width*))))

(defun area-set-square (row column value)
  "Set the value in the (row, column) square."
  (aset *snake-area*
        (+ column
           (* row *area-width*)) value))

(defun snake-player-init ()
  "Set point of initial snake"
  (dotimes (length *snake-length*)
    (setq *snake-body*
          (append (list (list 0 (+ 15 (+ length 1)))) *snake-body*))))

(defun snake-set-player ()
  "Set the snakes squares in the area"
  (setq *snake-area*
        (make-vector
         (* *area-height* *area-width*) ?\-))
  (dotimes (length (length *snake-body*))
    (area-set-square
          (nth 0 (nth length *snake-body*))
          (nth 1 (nth length *snake-body*)) ?\O)))

(defun touch-food ()
  "check is snake touch food, and do somthing"
  (setq snake-head (nth 0 *snake-body*))
  (message "snake head is %s,%s;" (nth 0 snake-head) (nth 1 snake-head))
  (if (and (eq (nth 0 snake-head) *food-row*) (eq (nth 1 snake-head) *food-column*))
      (message "touch food")))


(defun snake-move-right ()
  "Move snake to the right by setting extra
   point to the right and remove point from tail"
  (interactive)
  (message "move-right")
  (setq head-column (+ 1 (nth 1 (nth 0 *snake-body*))))
  (if (>= head-column *area-width*)
      (setq head-column 0)
      )
  (setq *snake-body*
        (butlast (cons (list (nth 0 (nth 0 *snake-body*)) ;;Add element to list
                             head-column)
                       *snake-body*))))

(defun snake-move-down ()
  "Move snake down by setting extra
   point to the bottom and remove point from tail"
  (interactive)
  (message "move-down")
  (setq head-row (+ 1 (nth 0 (nth 0 *snake-body*))))
  (if (>= head-row *area-height*)
      (setq head-row 0))
  (setq *snake-body*
        (butlast (cons (list head-row ;;Add element to list
                             (nth 1 (nth 0 *snake-body*)))
                       *snake-body*))))

(defun snake-move-left ()
  "Move snake to the left by setting extra
   point to the left and remove point from tail"
  (interactive)
  (message "move-left")
  (setq *snake-body*
        (butlast (cons (list (nth 0 (nth 0 *snake-body*)) ;;Add element to list
                             (- (nth 1 (nth 0 *snake-body*)) 1))
                       *snake-body*))))

(defun snake-move-up ()
  "Move snake to the up by setting extra
   point to the top and remove point from tail"
  (interactive)
    (message "move-up")
    (setq *snake-body*
          (butlast (cons (list (- (nth 0 (nth 0 *snake-body*)) 1) ;;Add element to list
                               (nth 1 (nth 0 *snake-body*)))
                         *snake-body*))))


