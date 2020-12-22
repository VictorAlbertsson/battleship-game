;;; TODOs

;; NOTE DONE* means that the feature specification is complete but not
;; fully implemented/working

;; DONE Render a single clickable UI element

;; DONE* Create and render a grid of clickable buttons

;; TODO Implement event dispatching to handle button clicks

;; DONE Wrap everything in classes

;; DONE* Retrieve gamedata from a network port

;; TODO Implement player turns

;; TODO Implement server to connect to remote instances of the game

;; TODO Limit the number of connections to only 2

;; TODO Connect the main loop of the server with that of the client

;; TODO Connect the 2 clients' main loops

;; TODO Handle game logic on the server

;; TODO Serialize game data as S-exprs sent as strings over the socket

;;; Modules

(define-module (game-lib)
  #:export (<tile>
	    <board>
	    tile-width
	    tile-height
	    update-board!
	    tile-contains?
	    rect-clicked?
	    range
	    map-range
	    deep-map
	    draw-board))

(use-modules (ice-9 rdelim)
	     (oop goops)
	     (rnrs bytevectors)
	     (srfi srfi-26)
	     (system repl coop-server) ; For live coding while the game is running
	     (chickadee)
	     (chickadee math vector)
	     (chickadee math rect)
	     (chickadee graphics texture)
	     (chickadee graphics sprite)
	     (chickadee graphics font)
	     (chickadee scripting))

;;; Classes

;; NOTE GOOPS class slots (fields) can't take type specifiers unlike CLOS slots
(define-class <tile> ()
  (image #:init-keyword #:image
	 #:accessor     image)
  (bound #:init-keyword #:bound
	 #:accessor     bound)
  ;; (status #:init-keyword #:status
  ;; 	  #:accessor     status)
  )

(define-class <board> ()
  (tiles #:init-keyword #:tiles
	 #:accessor     tiles))

;;; Helper functions and methods

(define-method (tile-width  (tile <tile>))
  (texture-width  (image tile)))

(define-method (tile-height (tile <tile>))
  (texture-height (image tile)))

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

(define-method (tile-contains? (tile <tile>) x y)
  (rect-contains? (bound tile) x y))

;; (define (load-board)
;;   (let ((file (with-input-from-file "data.scm" read)))
;;     ))

;; (define (save-board)
;;   (with-output-from-file
;;    "data.scm"
;;    (lambda ()
;;      )))

(define (rect-clicked? button x y)
  (and (eq? button 'left)
       ;; See SRFI-26 for the meaning of <> in combination with cut
       (or (deep-map (cut tile-contains? <> x y)
		     player)
	   (deep-map (cut tile-contains? <> x y)
		     enemy))))

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

(define (deep-map f xs)
  (let deep ((x xs))
    (cond ((null? x) x)
	  ((pair? x) (map deep x))
	  (else (f x)))))

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
