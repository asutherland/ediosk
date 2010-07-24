;; expose emacs state via JSON and support manipulation of state.
;; we're primarily concerned about enabling fancy buffer switching for now.

;; requires emacs-httpd from:
;;  http://git.nullprogram.com/emacs-httpd.git

(require 'json)

(defun httpd/list-buffers-json (uri-query req uri-path)
  "List all buffers as a JSON blob."
  (insert
   (json-encode
    `((rev . 1)
      (buffers . ,(vconcat
                   (mapcar
                    (lambda (b)
                      (with-current-buffer b
                        `((name . ,(buffer-name))
                          (filename . ,(buffer-file-name))
                          (modified . ,(buffer-modified-p))
                          (modcount . ,(buffer-chars-modified-tick))
                          (readonly . ,buffer-read-only)
                          (mode . ,mode-name)
                          (size . ,(buffer-size))
                          (displayTime . ,buffer-display-time)
                          )))
                   (buffer-list))))
      )))
  "application/json")

(defvar httpd-list-frames-next-window-id 1
  "Next window id.")

(defun httpd/list-frames-json (uri-query req uri-path)
  "List all frames and their windows as a JSON blob."
  (insert
   (json-encode
    `((rev . 1)
      (frames . ,(vconcat
                  (mapcar
                   (lambda (f)
                     `((selected . ,(eq f (selected-frame)))
                       (id . ,(frame-parameter f 'window-id))
                       (left . ,(frame-parameter f 'left))
                       (top . ,(frame-parameter f 'top))
                       (height . ,(frame-pixel-height f))
                       (width . ,(frame-pixel-width f))
                       (windows . ,(vconcat
                                    (mapcar
                                     (lambda (w)
                                       (unless (window-parameter w 'window-id)
                                         (set-window-parameter w 'window-id
                                                               (setq httpd-list-frames-next-window-id (1+ httpd-list-frames-next-window-id)))
                                         )
                                       `((point . ,(window-point w))
                                         (id . ,(window-parameter w 'window-id))
                                         (buffer . ,(buffer-name
                                                     (window-buffer w)))
                                         (selected . ,(eq w
                                                          (frame-selected-window f)))
                                         (pixelEdges . ,(window-pixel-edges w))
                                         ))
                                     (window-list f))))
                        ))
                     (frame-list))))
      )))
  "application/json")

(defun get-window-with-id (window-id)
  (get-window-with-predicate
   (lambda (w)
     (when (= (window-parameter w 'window-id) window-id)
       w)) nil t))

;; the following functions are GETs with side-effects, which is very bad of us.
;; even worse, they just return empty junk.

;; select the given window (and its frame)
(defun httpd/select-window (uri-query req uri-path)
  "Select the window identified by the given window-id."
  (let ((window (get-window-with-id (string-to-number (cadr (assoc "window" uri-query))))))
    (select-window window)
    (select-frame-set-input-focus (window-frame window))
    "text/plain"))

(defun httpd/show-buffer-in-window (uri-query req uri-path)
  "Make the referenced buffer visible in the referenced window."
  (let ((buffer (get-buffer (cadr (assoc "buffer" uri-query))))
        (window (get-window-with-id (string-to-number (cadr (assoc "window" uri-query))))))
    (set-window-buffer window buffer)
    "text/plain"))

