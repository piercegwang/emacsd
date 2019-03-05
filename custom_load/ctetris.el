;;; ctetris.el --- implementation of Ctetris for Emacs

;; Copyright (C) 1997, 2001-2018 Free Software Foundation, Inc.

;; Author: Glynn Clements <glynn@sensei.co.uk>
;; Version: 2.01
;; Created: 1997-08-13
;; Keywords: games

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'gamegrid)

;; ;;;;;;;;;;;;; customization variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup ctetris nil
  "Play a game of Ctetris."
  :prefix "ctetris-"
  :group 'games)

(defcustom ctetris-use-glyphs t
  "Non-nil means use glyphs when available."
  :group 'ctetris
  :type 'boolean)

(defcustom ctetris-use-color t
  "Non-nil means use color when available."
  :group 'ctetris
  :type 'boolean)

(defcustom ctetris-draw-border-with-glyphs t
  "Non-nil means draw a border even when using glyphs."
  :group 'ctetris
  :type 'boolean)

(defcustom ctetris-default-tick-period 0.3
  "The default time taken for a shape to drop one row."
  :group 'ctetris
  :type 'number)

(defcustom ctetris-update-speed-function
  'ctetris-default-update-speed-function
  "Function run whenever the Ctetris score changes.
Called with two arguments: (SHAPES ROWS)
SHAPES is the number of shapes which have been dropped.
ROWS is the number of rows which have been completed.

If the return value is a number, it is used as the timer period."
  :group 'ctetris
  :type 'function)

(defcustom ctetris-mode-hook
  (lambda ()
    (evil-emacs-state))
  "Hook run upon starting Ctetris."
  :group 'ctetris
  :type 'hook)

(defcustom ctetris-tty-colors
  ["blue" "white" "yellow" "magenta" "cyan" "green" "red"]
  "Vector of colors of the various shapes in text mode."
  :group 'ctetris
  :type '(vector (color :tag "Shape 1")
		 (color :tag "Shape 2")
		 (color :tag "Shape 3")
		 (color :tag "Shape 4")
		 (color :tag "Shape 5")
		 (color :tag "Shape 6")
		 (color :tag "Shape 7")))

(defcustom ctetris-x-colors
  [[0 0 1] [0.7 0 1] [1 1 0] [1 0 1] [0 1 1] [0 1 0] [1 0 0]]
  "Vector of RGB colors of the various shapes."
  :group 'ctetris
  :type '(vector (vector :tag "Shape 1" number number number)
                 (vector :tag "Shape 2" number number number)
                 (vector :tag "Shape 3" number number number)
                 (vector :tag "Shape 4" number number number)
                 (vector :tag "Shape 5" number number number)
                 (vector :tag "Shape 6" number number number)
                 (vector :tag "Shape 7" number number number)))

(defcustom ctetris-buffer-name "*Ctetris*"
  "Name used for Ctetris buffer."
  :group 'ctetris
  :type 'string)

(defcustom ctetris-buffer-width 30
  "Width of used portion of buffer."
  :group 'ctetris
  :type 'number)

(defcustom ctetris-buffer-height 22
  "Height of used portion of buffer."
  :group 'ctetris
  :type 'number)

(defcustom ctetris-width 10
  "Width of playing area."
  :group 'ctetris
  :type 'number)

(defcustom ctetris-height 20
  "Height of playing area."
  :group 'ctetris
  :type 'number)

(defcustom ctetris-top-left-x 3
  "X position of top left of playing area."
  :group 'ctetris
  :type 'number)

(defcustom ctetris-top-left-y 1
  "Y position of top left of playing area."
  :group 'ctetris
  :type 'number)

(defvar ctetris-next-x (+ (* 2 ctetris-top-left-x) ctetris-width)
  "X position of next shape.")

(defvar ctetris-next-y ctetris-top-left-y
  "Y position of next shape.")

(defvar ctetris-score-x ctetris-next-x
  "X position of score.")

(defvar ctetris-score-y (+ ctetris-next-y 6)
  "Y position of score.")

;; It is not safe to put this in /tmp.
;; Someone could make a symlink in /tmp
;; pointing to a file you don't want to clobber.
(defvar ctetris-score-file "ctetris-scores"
;; anybody with a well-connected server want to host this?
;(defvar ctetris-score-file "/anonymous@ftp.pgt.com:/pub/cgw/ctetris-scores"
  "File for holding high scores.")

;; ;;;;;;;;;;;;; display options ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ctetris-blank-options
  '(((glyph colorize)
     (t ?\040))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defvar ctetris-cell-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\040))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    ;; color information is taken from ctetris-x-colors and ctetris-tty-colors
    ))

(defvar ctetris-border-options
  '(((glyph colorize)
     (t ?\+))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0.5 0.5 0.5])
     (color-tty "white"))))

(defvar ctetris-space-options
  '(((t ?\040))
    nil
    nil))

;; ;;;;;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ctetris-shapes
  [[[[0  0] [1  0] [0  1] [1  1]]] ;; O

   [[[0  0] [1  0] [2  0] [2  1]]
    [[1 -1] [1  0] [1  1] [0  1]]
    [[0 -1] [0  0] [1  0] [2  0]]
    [[1 -1] [2 -1] [1  0] [1  1]]] ;; L

   [[[0  0] [1  0] [2  0] [0  1]]
    [[0 -1] [1 -1] [1  0] [1  1]]
    [[2 -1] [0  0] [1  0] [2  0]]
    [[1 -1] [1  0] [1  1] [2  1]]] ;; J

   [[[0  0] [1  0] [1  1] [2  1]]
    [[1  0] [0  1] [1  1] [0  2]]] ;; S

   [[[1  0] [2  0] [0  1] [1  1]]
    [[0  0] [0  1] [1  1] [1  2]]] ;; Z

   [[[1  0] [0  1] [1  1] [2  1]]
    [[1  0] [1  1] [2  1] [1  2]]
    [[0  1] [1  1] [2  1] [1  2]]
    [[1  0] [0  1] [1  1] [1  2]]] ;; T

   [[[0  0] [1  0] [2  0] [3  0]]
    [[2  2] [2  1] [2  0] [2 -1]]
    [[0 -1] [1 -1] [2 -1] [3 -1]]
    [[1  2] [1  1] [1  0] [1 -1]]]] ;; I
  "Each shape is described by a vector that contains the coordinates of
each one of its four blocks.")

;;the scoring rules were taken from "xctetris".  Blocks score differently
;;depending on their rotation

(defconst ctetris-shape-scores
  [[6] [6 7 6 7] [6 7 6 7] [6 7] [6 7] [5 5 6 5] [5 8]] )

(defconst ctetris-shape-dimensions
  [[2 2] [3 2] [3 2] [3 2] [3 2] [3 2] [4 1]])

(defconst ctetris-blank 7)

(defconst ctetris-border 8)

(defconst ctetris-space 9)

(defun ctetris-default-update-speed-function (_shapes rows)
  (/ 20.0 (+ 50.0 rows)))

;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ctetris-shape 0)
(defvar ctetris-rot 0)
(defvar ctetris-next-shape 0)
(defvar ctetris-n-shapes 0)
(defvar ctetris-n-rows 0)
(defvar ctetris-score 0)
(defvar ctetris-pos-x 0)
(defvar ctetris-pos-y 0)
(defvar ctetris-paused nil)

(make-variable-buffer-local 'ctetris-shape)
(make-variable-buffer-local 'ctetris-rot)
(make-variable-buffer-local 'ctetris-next-shape)
(make-variable-buffer-local 'ctetris-n-shapes)
(make-variable-buffer-local 'ctetris-n-rows)
(make-variable-buffer-local 'ctetris-score)
(make-variable-buffer-local 'ctetris-pos-x)
(make-variable-buffer-local 'ctetris-pos-y)
(make-variable-buffer-local 'ctetris-paused)

;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ctetris-mode-map
  (let ((map (make-sparse-keymap 'ctetris-mode-map)))
    (define-key map "n"		'ctetris-start-game)
    (define-key map "q"		'ctetris-end-game)
    (define-key map "p"		'ctetris-pause-game)

    (define-key map " "		'ctetris-move-bottom)
    (define-key map [left]	'ctetris-move-left)
    (define-key map [right]	'ctetris-move-right)
    (define-key map [up]	'ctetris-rotate-prev)
    (define-key map "z"         'ctetris-rotate-prev)
    (define-key map "x"         'ctetris-rotate-next)
    (define-key map [down]	'ctetris-move-down)
    map))

(defvar ctetris-null-map
  (let ((map (make-sparse-keymap 'ctetris-null-map)))
    (define-key map "n"		'ctetris-start-game)
    map))

;; ;;;;;;;;;;;;;;;; game functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ctetris-display-options ()
  (let ((options (make-vector 256 nil)))
    (dotimes (c 256)
      (aset options c
	    (cond ((= c ctetris-blank)
                   ctetris-blank-options)
                  ((and (>= c 0) (<= c 6))
		   (append
		    ctetris-cell-options
		    `((((glyph color-x) ,(aref ctetris-x-colors c))
		       (color-tty ,(aref ctetris-tty-colors c))
		       (t nil)))))
                  ((= c ctetris-border)
                   ctetris-border-options)
                  ((= c ctetris-space)
                   ctetris-space-options)
                  (t
                   '(nil nil nil)))))
    options))

(defun ctetris-get-tick-period ()
  (if (boundp 'ctetris-update-speed-function)
      (let ((period (apply ctetris-update-speed-function
			   ctetris-n-shapes
			   ctetris-n-rows nil)))
	(and (numberp period) period))))

(defun ctetris-get-shape-cell (block)
  (aref (aref  (aref ctetris-shapes
                     ctetris-shape) ctetris-rot)
        block))

(defun ctetris-shape-width ()
  (aref (aref ctetris-shape-dimensions ctetris-shape) 0))

(defun ctetris-shape-rotations ()
  (length (aref ctetris-shapes ctetris-shape)))

(defun ctetris-draw-score ()
  (let ((strings (vector (format "Shapes: %05d" ctetris-n-shapes)
			 (format "Rows:   %05d" ctetris-n-rows)
			 (format "Score:  %05d" ctetris-score))))
    (dotimes (y 3)
      (let* ((string (aref strings y))
             (len (length string)))
        (dotimes (x len)
          (gamegrid-set-cell (+ ctetris-score-x x)
                             (+ ctetris-score-y y)
                             (aref string x)))))))

(defun ctetris-update-score ()
  (ctetris-draw-score)
  (let ((period (ctetris-get-tick-period)))
    (if period (gamegrid-set-timer period))))

(defun ctetris-new-shape ()
  (setq ctetris-shape ctetris-next-shape)
  (setq ctetris-rot 0)
  (setq ctetris-next-shape (random 7))
  (setq ctetris-pos-x (/ (- ctetris-width (ctetris-shape-width)) 2))
  (setq ctetris-pos-y 0)
  (if (ctetris-test-shape)
      (ctetris-end-game)
    (ctetris-draw-shape)
    (ctetris-draw-next-shape)
    (ctetris-update-score)))

(defun ctetris-draw-next-shape ()
  (dotimes (x 4)
    (dotimes (y 4)
      (gamegrid-set-cell (+ ctetris-next-x x)
                         (+ ctetris-next-y y)
                         ctetris-blank)))
  (dotimes (i 4)
    (let ((ctetris-shape ctetris-next-shape)
          (ctetris-rot 0))
      (gamegrid-set-cell (+ ctetris-next-x
                            (aref (ctetris-get-shape-cell i) 0))
                         (+ ctetris-next-y
                            (aref (ctetris-get-shape-cell i) 1))
                         ctetris-shape))))

(defun ctetris-draw-shape ()
  (dotimes (i 4)
    (let ((c (ctetris-get-shape-cell i)))
      (gamegrid-set-cell (+ ctetris-top-left-x
                            ctetris-pos-x
                            (aref c 0))
                         (+ ctetris-top-left-y
                            ctetris-pos-y
                            (aref c 1))
                         ctetris-shape))))

(defun ctetris-erase-shape ()
  (dotimes (i 4)
    (let ((c (ctetris-get-shape-cell i)))
      (gamegrid-set-cell (+ ctetris-top-left-x
                            ctetris-pos-x
                            (aref c 0))
                         (+ ctetris-top-left-y
                            ctetris-pos-y
                            (aref c 1))
                         ctetris-blank))))

(defun ctetris-test-shape ()
  (let ((hit nil))
    (dotimes (i 4)
      (unless hit
        (setq hit
              (let* ((c (ctetris-get-shape-cell i))
                     (xx (+ ctetris-pos-x
                            (aref c 0)))
                     (yy (+ ctetris-pos-y
                            (aref c 1))))
                (or (>= xx ctetris-width)
                    (>= yy ctetris-height)
                    (/= (gamegrid-get-cell
                         (+ xx ctetris-top-left-x)
                         (+ yy ctetris-top-left-y))
                        ctetris-blank))))))
    hit))

(defun ctetris-full-row (y)
  (let ((full t))
    (dotimes (x ctetris-width)
      (if (= (gamegrid-get-cell (+ ctetris-top-left-x x)
                                (+ ctetris-top-left-y y))
             ctetris-blank)
          (setq full nil)))
    full))

(defun ctetris-shift-row (y)
  (if (= y 0)
      (dotimes (x ctetris-width)
	(gamegrid-set-cell (+ ctetris-top-left-x x)
			   (+ ctetris-top-left-y y)
			   ctetris-blank))
    (dotimes (x ctetris-width)
      (let ((c (gamegrid-get-cell (+ ctetris-top-left-x x)
                                  (+ ctetris-top-left-y y -1))))
        (gamegrid-set-cell (+ ctetris-top-left-x x)
                           (+ ctetris-top-left-y y)
			   c)))))

(defun ctetris-shift-down ()
  (dotimes (y0 ctetris-height)
    (when (ctetris-full-row y0)
      (setq ctetris-n-rows (1+ ctetris-n-rows))
      (cl-loop for y from y0 downto 0 do
               (ctetris-shift-row y)))))

(defun ctetris-draw-border-p ()
  (or (not (eq gamegrid-display-mode 'glyph))
      ctetris-draw-border-with-glyphs))

(defun ctetris-init-buffer ()
  (gamegrid-init-buffer ctetris-buffer-width
			ctetris-buffer-height
			ctetris-space)
  (let ((buffer-read-only nil))
    (if (ctetris-draw-border-p)
	(cl-loop for y from -1 to ctetris-height do
                 (cl-loop for x from -1 to ctetris-width do
                          (gamegrid-set-cell (+ ctetris-top-left-x x)
                                             (+ ctetris-top-left-y y)
                                             ctetris-border))))
    (dotimes (y ctetris-height)
      (dotimes (x ctetris-width)
        (gamegrid-set-cell (+ ctetris-top-left-x x)
                           (+ ctetris-top-left-y y)
                           ctetris-blank)))
    (if (ctetris-draw-border-p)
	(cl-loop for y from -1 to 4 do
                 (cl-loop for x from -1 to 4 do
                          (gamegrid-set-cell (+ ctetris-next-x x)
                                             (+ ctetris-next-y y)
                                             ctetris-border))))))

(defun ctetris-reset-game ()
  (gamegrid-kill-timer)
  (ctetris-init-buffer)
  (setq ctetris-next-shape (random 7))
  (setq ctetris-shape	0
	ctetris-rot	0
	ctetris-pos-x	0
	ctetris-pos-y	0
	ctetris-n-shapes	0
	ctetris-n-rows	0
	ctetris-score	0
	ctetris-paused	nil)
  (ctetris-new-shape))

(defun ctetris-shape-done ()
  (ctetris-shift-down)
  (setq ctetris-n-shapes (1+ ctetris-n-shapes))
  (setq ctetris-score
	(+ ctetris-score
	   (aref (aref ctetris-shape-scores ctetris-shape) ctetris-rot)))
  (ctetris-update-score)
  (ctetris-new-shape))

(defun ctetris-update-game (ctetris-buffer)
  "Called on each clock tick.
Drops the shape one square, testing for collision."
  (if (and (not ctetris-paused)
	   (eq (current-buffer) ctetris-buffer))
      (let (hit)
	(ctetris-erase-shape)
	(setq ctetris-pos-y (1+ ctetris-pos-y))
	(setq hit (ctetris-test-shape))
	(if hit
	    (setq ctetris-pos-y (1- ctetris-pos-y)))
	(ctetris-draw-shape)
	(if hit
	    (ctetris-shape-done)))))

(defun ctetris-move-bottom ()
  "Drop the shape to the bottom of the playing area."
  (interactive)
  (unless ctetris-paused
    (let ((hit nil))
      (ctetris-erase-shape)
      (while (not hit)
        (setq ctetris-pos-y (1+ ctetris-pos-y))
        (setq hit (ctetris-test-shape)))
      (setq ctetris-pos-y (1- ctetris-pos-y))
      (ctetris-draw-shape)
      (ctetris-shape-done))))

(defun ctetris-move-left ()
  "Move the shape one square to the left."
  (interactive)
  (unless ctetris-paused
    (ctetris-erase-shape)
    (setq ctetris-pos-x (1- ctetris-pos-x))
    (if (ctetris-test-shape)
        (setq ctetris-pos-x (1+ ctetris-pos-x)))
    (ctetris-draw-shape)))

(defun ctetris-move-right ()
  "Move the shape one square to the right."
  (interactive)
  (unless ctetris-paused
    (ctetris-erase-shape)
    (setq ctetris-pos-x (1+ ctetris-pos-x))
    (if (ctetris-test-shape)
	(setq ctetris-pos-x (1- ctetris-pos-x)))
    (ctetris-draw-shape)))

(defun ctetris-move-down ()
  "Move the shape one square to the bottom."
  (interactive)
  (unless ctetris-paused
    (ctetris-erase-shape)
    (setq ctetris-pos-y (1+ ctetris-pos-y))
    (if (ctetris-test-shape)
	(setq ctetris-pos-y (1- ctetris-pos-y)))
    (ctetris-draw-shape)))

(defun ctetris-rotate-prev ()
  "Rotate the shape clockwise."
  (interactive)
  (unless ctetris-paused
      (ctetris-erase-shape)
      (setq ctetris-rot (% (+ 1 ctetris-rot)
                          (ctetris-shape-rotations)))
      (if (ctetris-test-shape)
          (setq ctetris-rot (% (+ 3 ctetris-rot)
                              (ctetris-shape-rotations))))
      (ctetris-draw-shape)))

(defun ctetris-rotate-next ()
  "Rotate the shape counter-clockwise."
  (interactive)
  (unless ctetris-paused
        (ctetris-erase-shape)
        (setq ctetris-rot (% (+ 3 ctetris-rot)
                            (ctetris-shape-rotations)))
        (if (ctetris-test-shape)
            (setq ctetris-rot (% (+ 1 ctetris-rot)
                                (ctetris-shape-rotations))))
        (ctetris-draw-shape)))

(defun ctetris-end-game ()
  "Terminate the current game."
  (interactive)
  (gamegrid-kill-timer)
  (use-local-map ctetris-null-map)
  (gamegrid-add-score ctetris-score-file ctetris-score))

(defun ctetris-start-game ()
  "Start a new game of Ctetris."
  (interactive)
  (ctetris-reset-game)
  (use-local-map ctetris-mode-map)
  (let ((period (or (ctetris-get-tick-period)
		    ctetris-default-tick-period)))
    (gamegrid-start-timer period 'ctetris-update-game))
  (run-hooks 'ctetris-mode-hook))

(defun ctetris-pause-game ()
  "Pause (or resume) the current game."
  (interactive)
  (setq ctetris-paused (not ctetris-paused))
  (message (and ctetris-paused "Game paused (press p to resume)")))

(defun ctetris-active-p ()
  (eq (current-local-map) ctetris-mode-map))

(put 'ctetris-mode 'mode-class 'special)

(define-derived-mode ctetris-mode nil "Ctetris"
  "A mode for playing Ctetris."

  (add-hook 'kill-buffer-hook 'gamegrid-kill-timer nil t)

  (use-local-map ctetris-null-map)

  (unless (featurep 'emacs)
    (setq mode-popup-menu
	  '("Ctetris Commands"
	    ["Start new game"	ctetris-start-game]
	    ["End game"		ctetris-end-game
	     (ctetris-active-p)]
	    ["Pause"		ctetris-pause-game
	     (and (ctetris-active-p) (not ctetris-paused))]
	    ["Resume"		ctetris-pause-game
	     (and (ctetris-active-p) ctetris-paused)])))

  (setq show-trailing-whitespace nil)

  (setq gamegrid-use-glyphs ctetris-use-glyphs)
  (setq gamegrid-use-color ctetris-use-color)

  (gamegrid-init (ctetris-display-options)))

;;;###autoload
(defun ctetris ()
  "Play the Ctetris game.
Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

ctetris-mode keybindings:
   \\<ctetris-mode-map>
\\[ctetris-start-game]	Starts a new game of Ctetris
\\[ctetris-end-game]	Terminates the current game
\\[ctetris-pause-game]	Pauses (or resumes) the current game
\\[ctetris-move-left]	Moves the shape one square to the left
\\[ctetris-move-right]	Moves the shape one square to the right
\\[ctetris-rotate-prev]	Rotates the shape clockwise
\\[ctetris-rotate-next]	Rotates the shape anticlockwise
\\[ctetris-move-bottom]	Drops the shape to the bottom of the playing area

"
  (interactive)

  (select-window (or (get-buffer-window ctetris-buffer-name)
		     (selected-window)))
  (switch-to-buffer ctetris-buffer-name)
  (gamegrid-kill-timer)
  (ctetris-mode)
  (ctetris-start-game))

(provide 'ctetris)

;;; ctetris.el ends here
