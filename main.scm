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

(define-class <player-board> (<board>))

(define-class <enemy-board> (<board>))

;;; Global variables

;; Live coding setup
(define repl (spawn-coop-repl-server))

;; Global resources, set during load-time
(define image #f)

(define width  #f)
(define height #f)
(define center #f)

(define test #f)

;;; Helper functions

(define (button-clicked? button x y)
  (and (eq? button 'left)
       (rect-contains? button x y)))

;; Basic range function beacuse the Scheme standard is very minimal
(define (range start stop step)
  (if (or (and (>  step 0)
               (>= start stop))
          (and (<= step 0)
               (<= start stop)))
      '()
      (cons start (range (+ start step) stop step))))

;; Index aware map function
(define (map-range f start step . xs)
  (let ((stop (+ start (* step (length (car xs))))))
    (apply map (cons f
                     (cons (range start stop step)
                           xs)))))

;; Draws a grid of images, WARNING: Assumes that all images are the
;; same size!
(define (draw-board images origin)
  (define (f is xs)
    (define (g i x)
      (draw-sprite x (vec2+ origin
			    (vec2* (vec2 (texture-width x) (texture-height x))
				   (vec2 i                 is)))))
    (map-range g 1 1 xs))
  (map-range f 1 1 images))

;;; Initialization

(define (load)
  ;; Chickadee auto-adjusts the absolute position of sprites to
  ;; accomodate for the window resizing, trying to center a sprite
  ;; dynamically using the window width and height will fail since the
  ;; position adjustment is done after the draw function is called.
  ;; Therefore I store the the initial center of the window and use
  ;; that to align graphical elements.
  (set! center (vec2 (/ (window-width  (current-window)) 2)
		     (/ (window-height (current-window)) 2)))
  ;; TODO For testing purposes only!
  (set! image  (load-image "./image.bmp"))
  (set! test (list (list image image image)
		   (list image image image)
		   (list image image image))))

;;; Core event loop

(define (update dt)
  (poll-coop-repl-server repl))

(define (draw alpha)
  (let ((width  (texture-width  image))
	(height (texture-height image))
	(left   (vec2 (/ (vec2-x center)       2) (vec2-y center)))
	(right  (vec2 (/ (* 3 (vec2-x center)) 2) (vec2-y center))))
    (draw-board test (vec2 0 0))
    ;; (draw-sprite image left
    ;; 		 #:origin (vec2 (/ width  2) (/ height 2)))
    ;; (draw-sprite image right
    ;; 		 #:origin (vec2 (/ width  2) (/ height 2)))
    ))

(define (mouse-release mouse-button x y)
  ;; (cond ((button-clicked? tile-1 x y) (display "Gotcha!\n"))
  ;; 	(else #f))
  #f
  )

(run-game #:load load #:update update #:draw draw #:mouse-release mouse-release)
