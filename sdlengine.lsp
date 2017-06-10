(ql:quickload "sdl2")
(ql:quickload "cl-opengl")
(ql:quickload "sdl2-image")
(ql:quickload "sdl2-ttf")
(require :sdl2)
(require :cl-opengl)
(require :futile)

(defun debug-log (fstr &rest r)
  (apply #'format t fstr r))

(defstruct key-event-result
  type
  value)
(defvar +ke-open-file+ 0)
(defvar +ke-quit+ -1)

(defparameter *image-cache* (make-hash-table :size 16))

(defun setup-gl (win gl-context)
  (debug-log "Setting up window/gl.~%")
  (sdl2:gl-make-current win gl-context)
  (gl:viewport 0 0 800 60)
  (gl:matrix-mode :projection)
  (gl:ortho -2 2 -2 2 -2 2)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:clear-color 0.0 0.0 0.0 1.0))

(declaim (ftype (function (t string) (sdl2-ffi:sdl-texture)) load-png))
(defun load-png (renderer fp)
  (sdl2:create-texture-from-surface renderer (sdl2-image:load-png-rw fp)))

(declaim (ftype (function (t sdl2-ffi:sdl-texture t t t t)) draw-image))
(defun draw-image (renderer sdl-tex X Y W H)
  (sdl2:render-copy renderer sdl-tex :source-rect nil :dest-rect (sdl2:make-rect X Y W H)))

(defun render-triangle (r)
  (sdl2:render-draw-rect r (sdl2:make-rect 10 10 30 30)))

(defun layer-list ()
  nil)
(defun render-overlay (renderer))
(defun render-layers (renderer layer_list))

(defun load-img-test (rend)
  (draw-image rend (load-png rend "../resources/test-tile.png") 200 200 64 64))

(defun test (rend))

(defun render (renderer)
  (let ((orderer_render (lambda (lst)
                          (render-layers renderer lst)
                          (render-overlay renderer))))
    (funcall orderer_render (layer-list))))

(defun open-file-browser ()
  (uiop:run-program "zenity --file-selection --file-filter=\"*.png\""
                    :output '(:string :stripped t)
                    :ignore-error-status t))

(defun key-handler--ctrl+o ()
  (make-key-event-result :type +ke-open-file+ :value (open-file-browser)))

(defun key-handler--ctrl+q ()
  (sdl2:push-quit-event)
  (make-key-event-result :type nil :value nil))

(defun ctrl-pressed? (mod-value) (= 64 mod-value))

(defun handle-key (scancode sym mod-value)
  (cond
    ((and (sdl2:scancode= scancode :scancode-o) (ctrl-pressed? mod-value)) (key-handler--ctrl+o))
    ((and (sdl2:scancode= scancode :scancode-q) (ctrl-pressed? mod-value)) (key-handler--ctrl+q))
    (t (make-key-event-result :type nil :value nil))
    ))

(defun post-key-handler (renderer result)
  (case (key-event-result-type result)
   (0 (let ((imgtex (load-png renderer (key-event-result-value result))))
                     (draw-image renderer imgtex 0 0 (sdl2:texture-width imgtex) (sdl2:texture-height imgtex))))))

(defun main-game ()
  "Main start of our game"
  (sdl2:with-init (:everything)
    (debug-log "using sdl library version: ~D.~D.~D~%"
               sdl2-ffi:+sdl-major-version+
               sdl2-ffi:+sdl-minor-version+
               sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (win :flags '(:shown :opengl))
      (sdl2:with-renderer (main-renderer win)
        (main-loop win main-renderer #'test)))))

(defun main-loop (win renderer main_render_fun)
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
     (funcall main_render_fun renderer)
     (sdl2:render-present renderer))
    (:quit () t)
    (:keydown (:keysym keysym)
     (let* ((scancode (sdl2:scancode-value keysym))
           (sym (sdl2:sym-value keysym))
           (mod-value (sdl2:mod-value keysym))
           (keyresult (handle-key scancode sym mod-value)))
       (when (key-event-result-type keyresult) (post-key-handler renderer keyresult))))))

