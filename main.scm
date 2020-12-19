;;; TODOs

;; DONE Render a single clickable UI element

;; DONE* Create and render a grid of clickable buttons

;; TODO Implement event dispatching to handle button clicks

;; DONE Wrap everything in classes

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

;;; Classes

;; NOTE GOOPS class slots can't take type specifiers unlike CLOS slots
(define-class <tile> ()
  (image #:init-keyword #:image
	 #:accessor     image)
  (rect  #:init-keyword #:bound
	 #:accessor     bound)
  ;; (width #:getter tile-width ; Immutable, depends on the image field
  ;; 	 ;; Virtual allocation means that the slot is computed and not
  ;; 	 ;; stored as a part of the object
  ;; 	 #:allocation #:virtual
  ;; 	 #:slot-ref
  ;; 	 (lambda (object)
  ;; 	   (texture-width  (image object))))
  ;; (height #:getter tile-height ; Immutable, depends on the image field
  ;; 	  #:allocation #:virtual
  ;; 	  #:slot-ref
  ;; 	  (lambda (object)
  ;; 	    (texture-height (image object))))
  )

(define-class <board> ()
  (tiles #:init-keyword #:tiles
	 #:accessor     tiles))

(define (update-board! tiles)
  ;; WARNING Requires the image of each tile to already be set!
  (define (f y-index row)
    (define (g x-index col)
      (set! (bound col)
	(rect (+ (vec2-x origin)
		 (* (texture-width  (image col))
		    x-index))
	      (+ (vec2-y origin)
		 (* (texture-height (image col))
		    y-index))
	      (texture-width  (image col))
	      (texture-height (image col)))))
    (map-range g 1 1 row))
  (map-range f 1 1 tiles))

(define player #f)
(define enemy  #f)

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
  (if (or (and (>  step  0)
               (>= start stop))
          (and (<= step  0)
               (<= start stop)))
      '()
      (cons start (range (+ start step) stop step))))

;; Index aware map function
(define (map-range f start step . xs)
  (let ((stop (+ start (* step (length (car xs))))))
    (apply map (cons f
                     (cons (range start stop step)
                           xs)))))

;; Draws a grid of images, WARNING: Assumes that all images are set
;; and same size!
(define (draw-board tiles origin)
  (define (f y-index xs)
    (define (g x-index x)
      (draw-sprite x (vec2+ origin
			    (vec2* (vec2 (texture-width  (image x))
					 (texture-height (image x)))
				   (vec2 x-index
					 y-index)))))
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
  (set! player (make <board>
		 #:tiles (list (list (make <tile> #:image image)
				     (make <tile> #:image image)
				     (make <tile> #:image image))
			       (list (make <tile> #:image image)
				     (make <tile> #:image image)
				     (make <tile> #:image image))
			       (list (make <tile> #:image image)
				     (make <tile> #:image image)
				     (make <tile> #:image image)))))
  (set! enemy (make <board>
		#:tiles (list (list (make <tile> #:image image)
				    (make <tile> #:image image)
				    (make <tile> #:image image))
			      (list (make <tile> #:image image)
				    (make <tile> #:image image)
				    (make <tile> #:image image))
			      (list (make <tile> #:image image)
				    (make <tile> #:image image)
				    (make <tile> #:image image))))))

;;; Core event loop

(define (update dt)
  ;; For live coding
  (poll-coop-repl-server repl))

(define (draw alpha)
  (let ((width  (texture-width  image))
	(height (texture-height image))
	(left   (vec2 (/ (vec2-x center)       2) (vec2-y center)))
	(right  (vec2 (/ (* 3 (vec2-x center)) 2) (vec2-y center))))
    ;; (draw-board player left)
    ;; (draw-board enemy  right)
    #f))

(define (mouse-release mouse-button x y)
  (cond ((map (lambda (ts) (map (lambda (t) button-clicked?)
				ts))
	      (tiles player))
	 (display "Gotcha!\n"))
	((map (lambda (ts) (map (lambda (t) (button-clicked? mouse-button ))
				ts))
	      (tiles enemy))
	 (display "Bombing!\n"))
	(else #f)))

(run-game #:load load #:update update #:draw draw #:mouse-release mouse-release)
