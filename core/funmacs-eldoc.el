;;; funmacs-eldoc.el --- Eldoc and Eldoc-Mouse popup hover configuration -*- lexical-binding: t; -*-

;; Requires: eglot, eldoc, eldoc-mouse, posframe
;; Provides: persistent hover documentation popups with mouse movement detection

;;; Code:

(use-package eldoc
  :ensure t
  :custom
  (eldoc-idle-delay 0.2)
  :init
  (global-eldoc-mode 1))

(use-package posframe
  :ensure t)

(use-package eldoc-mouse
  :ensure t
  :hook (prog-mode . eldoc-mouse-mode)
  :custom
  (eldoc-mouse-hover-delay 0.15)
  :config

  (defvar funmacs/eldoc--last-mouse-pixel-pos nil
    "Last mouse pixel position where popup was shown.")

  (defvar funmacs/eldoc--monitor-timer nil
    "Timer to monitor mouse movement.")

  (defvar funmacs/eldoc--allow-hide nil
    "Flag to control when hiding is allowed.")

  (defvar funmacs/eldoc--debug nil
    "Enable debug messages for mouse tracking.")

  (defun funmacs/eldoc-posframe-hide ()
    "Hide the eldoc posframe popup."
    (when funmacs/eldoc--debug
      (message "Hiding posframe"))
    (when (posframe-workable-p)
      (posframe-hide "*doc-posframe-buffer*"))
    (setq funmacs/eldoc--last-mouse-pixel-pos nil)
    ;; Cancel monitor timer
    (when funmacs/eldoc--monitor-timer
      (cancel-timer funmacs/eldoc--monitor-timer)
      (setq funmacs/eldoc--monitor-timer nil)))

  (defun funmacs/eldoc-check-mouse-moved ()
    "Check if mouse has moved and hide popup if so."
    (when funmacs/eldoc--last-mouse-pixel-pos
      (condition-case err
          (let* ((current-pixel-pos (mouse-pixel-position))
                 (current-frame (car current-pixel-pos))
                 (current-x (cadr current-pixel-pos))
                 (current-y (cddr current-pixel-pos))
                 (last-frame (car funmacs/eldoc--last-mouse-pixel-pos))
                 (last-x (cadr funmacs/eldoc--last-mouse-pixel-pos))
                 (last-y (cddr funmacs/eldoc--last-mouse-pixel-pos)))
            
            (when funmacs/eldoc--debug
              (message "Checking mouse - Current: [%s %s], Last: [%s %s]"
                       current-x current-y last-x last-y))
            
            ;; Hide if mouse moved
            (when (and current-x current-y last-x last-y
                       (or (not (eq current-frame last-frame))
                           (not (= current-x last-x))
                           (not (= current-y last-y))))
              (when funmacs/eldoc--debug
                (message "Mouse moved! Hiding popup"))
              (funmacs/eldoc-posframe-hide)))
        (error
         (when funmacs/eldoc--debug
           (message "Error in mouse check: %S" err))))))

  ;; Override eldoc-mouse's hide function
  (defun eldoc-mouse--hide-posframe ()
    "Override eldoc-mouse hide function - only hide if we allow it."
    (when funmacs/eldoc--allow-hide
      (when funmacs/eldoc--debug
        (message "eldoc-mouse--hide-posframe called with allow-hide=t"))
      (when (posframe-workable-p)
        (posframe-hide "*doc-posframe-buffer*")))
    (when (and (not funmacs/eldoc--allow-hide) funmacs/eldoc--debug
      (message "Blocked eldoc-mouse auto-hide"))))

  (defun funmacs/start-monitoring ()
    "Start monitoring mouse movement."
    (condition-case err
        (let* ((pos (mouse-pixel-position))
               (x (cadr pos))
               (y (cddr pos)))
          ;; Check if x and y are valid numbers
          (when (and (numberp x) (numberp y))
            (setq funmacs/eldoc--last-mouse-pixel-pos pos)
            (setq funmacs/eldoc--allow-hide nil)
            
            (when funmacs/eldoc--debug
              (message "Started monitoring at position: %S" pos))
            
            ;; Cancel existing monitor timer
            (when funmacs/eldoc--monitor-timer
              (cancel-timer funmacs/eldoc--monitor-timer))
            
            ;; Start new monitor timer
            (setq funmacs/eldoc--monitor-timer
                  (run-with-timer 0.1 0.1 #'funmacs/eldoc-check-mouse-moved))))
      (error
       (when funmacs/eldoc--debug
         (message "Error starting monitoring: %S" err)))))

  ;; Enhanced error handler
  (defun funmacs/eldoc-mouse-show-doc-at-advice (orig-fun &rest args)
    "Wrap eldoc-mouse-show-doc-at to catch errors."
    (condition-case err
        (apply orig-fun args)
      (error
       (when funmacs/eldoc--debug
         (message "Suppressed eldoc-mouse error: %S" err))
       nil)))

  (advice-add 'eldoc-mouse-show-doc-at :around
              #'funmacs/eldoc-mouse-show-doc-at-advice)

  ;; Start monitoring after posframe shows
  (defun funmacs/after-posframe-show (buffer-name &rest args)
    "After posframe shows, start mouse monitoring."
    (when (string= buffer-name "*doc-posframe-buffer*")
      (funmacs/start-monitoring)))

  (advice-add 'posframe-show :after #'funmacs/after-posframe-show))

(provide 'funmacs-eldoc)
;;; funmacs-eldoc.el ends here
