#!/usr/bin/guile -s
!#

;;; TODOs

;; DONE Render a single clickable UI element

;; TODO Create and render a grid of clickable buttons

;; TODO Implement event dispatching to handle button clicks

;; TODO Wrap everything in classes

;; TODO Retrieve gamedata from a network port

;; TODO Implement player turns

;; TODO Implement server to connect to remote instances of the game

;;; Modules

(use-modules (oop goops)
	     (system repl coop-server) ; For live coding while the game is running
	     (chickadee)
	     (chickadee math vector)
	     (chickadee math rect)
	     (chickadee graphics texture)
	     (chickadee graphics sprite)
	     (chickadee graphics font)
	     (chickadee scripting))

;; (define-class <tile> ()
;;   (image #:init-keyword image)
;;   (rect))

;;; Classes

(define-class <board> ()
  (tiles #:init-keyword #:tiles
	 #:accessor     tiles))

;;; Global variables

;; Live coding setup
(define repl (spawn-coop-repl-server))

;; `tile' is temporary debug code
(define tile-1 #f)
(define tile-2 #f)
(define tile-3 #f)

;; Global resources, set during load-time
(define image  #f)
(define width  #f)
(define height #f)

;;; Helper functions

(define (button-clicked? button x y)
  (and (eq? button 'left)
       (rect-contains? button x y)))

(define (draw-tile tile image x y)
  (let ((width  (texture-width  image))
	(height (texture-height image)))
    (set! tile (rect x y width height))
    (draw-sprite image (vec2 x y)
		 #:origin (vec2 (/ width 2)
				(/ height 2)))))

;;; Core event-loop

(define (load)
  (set! image  (load-image "./image.bmp")))

(define (draw alpha)
  #t
  ;; (let* ((tile-1 #f)
  ;; 	 (tile-2 #f)
  ;; 	 (tile-3 #f))
  ;;   (draw-tile tile-1 image))
  )

(define (update dt)
  (poll-coop-repl-server repl))

(define (click mouse-button x y)
  (cond ((button-clicked? tile-1 x y) (display "Gotcha!\n"))
	(else #f)))

(run-game #:load load #:update update #:draw draw #:mouse-release click)
