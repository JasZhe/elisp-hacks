;;; jaszhe-hacks.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Jason Zhen
;;
;; Author: Jason Zhen <jaszhe@gmail.com>
;; Maintainer: Jason Zhen <jaszhe@gmail.com>
;; Created: July 05, 2024
;; Modified: July 05, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/jasonzhen/jaszhe-hacks
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar ace-choose-window-prefix-window nil)
(defun ace-choose-window-prefix ()
  "Interactively pick which window to show the next command's buffer in."
  (interactive)
  (setq ace-choose-window-prefix-window nil)
  (let ((aw-dispatch-always t))
    (aw-select "Choose window" (lambda (window) (setq ace-choose-window-prefix-window window)))
    )
  (when ace-choose-window-prefix-window
    (display-buffer-override-next-command
     (lambda (buffer alist)
       (select-window ace-choose-window-prefix-window)
       (setq alist (append '((inhibit-same-window . nil)) alist))
       (cons (or
              (display-buffer-same-window buffer alist)
              (display-buffer-use-some-window buffer alist))
             'reuse))
     nil "[same-window]")
    (message "Display next command buffer in the chosen window...")
    )
  )

(transient-define-suffix transient-other-window-prefix ()
  "Common transient to be used to open next transient command in other window."
  :transient t
  (interactive)
  (other-window-prefix))

(transient-define-suffix transient-ace-choose-window-prefix ()
  "Common transient to be used to open next transient command in other chosen window."
  :transient t
  (interactive)
  (ace-choose-window-prefix))

(transient-append-suffix 'transient-common-commands '(0 -1)
  (vector "Window"
          (list "M-o" "other-window" #'transient-other-window-prefix)
          (list "M-O" "choose-window" #'transient-ace-choose-window-prefix)
          ))

(defun pdf-next-page-all-windows-for-doc ()
  "Next page for all pdf buffers in the current frame."
  (interactive)
  (let ((buf (current-buffer))
        (cur-win (selected-window)))
    (dolist (win (get-buffer-window-list buf))
      (save-window-excursion
        (select-window win)
        (pdf-view-next-page-command)))))

(defun pdf-prev-page-all-windows-for-doc ()
  "Prev page for all pdf buffers in the current frame."
  (interactive)
  (let ((buf (current-buffer))
        (cur-win (selected-window)))
    (dolist (win (get-buffer-window-list buf))
      (save-window-excursion
        (select-window win)
        (pdf-view-previous-page-command)))))

(defun ripgrep ()
  (interactive)
  (call-interactively 'grep))

(defun rripgrep ()
  (interactive)
  (call-interactively 'rgrep))

(defun rripgrep-multiline ()
  (interactive)
  (call-interactively 'rgrep))

(defun rgrep-multiline ()
  (interactive)
  (grep-apply-setting 'grep-command "grep -Pazo --color=auto -nH --null -e ")
  (call-interactively 'rgrep))

(defun grep-options-advice ()
  "A convenient way for us to put different options depending on the grep command being run.
See notes:emacs-notes-and-tips for more details."
  (cond ((or (eq this-command 'ripgrep) (eq this-command 'rripgrep))
         (progn
           (grep-apply-setting 'grep-command "rg -nS --no-heading ") ;; for normal single file grep
           (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec rg <C> -nS --no-heading -H  <R> /dev/null {} +"))) ;; for rgrep; uses grep-find-template
        ((eq this-command 'rripgrep-multiline)
         (progn
           (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec rg <C> -nSU --no-heading -H  <R> /dev/null {} +")))
        ((eq this-command 'rgrep-multiline)
         (progn
           (grep-apply-setting 'grep-find-template "find -H <D> <X> -type f <F> -exec grep -zo <C> -nH --null -e <R> \\{\\} +")))
        (t (progn ;; defaults in case I want to change them later to do something different, otherwise don't really need this last case
             (grep-apply-setting 'grep-find-template "find -H <D> <X> -type f <F> -exec grep <C> -nH --null -e <R> \\{\\} +")
             (grep-apply-setting 'grep-command "grep --color=auto -nH --null -e ")))
        )
  )

(advice-add #'grep-compute-defaults :before #'grep-options-advice)

(defun copy-env-vars-from-shell ()
  (interactive)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
                 (name (cl-first split))
                 (val (cl-second split)))
            (when (and name val)
              (setq val (string-replace " " "\\ " val))
              (setenv name val)
              (when (string-equal "PATH" name)

                ;; eshell path
                (setq-default eshell-path-env val)
                (when (fboundp 'eshell-set-path) (eshell-set-path val))))))
        (split-string (shell-command-to-string "bash --login -i -c printenv") "\n")))

(defun copy-env-vars-from-envrc ()
  (interactive)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
                 (name (cl-first split))
                 (val (cl-second split)))
            (when (and name val)
              (setq val (string-replace " " "\\ " val))
              (setenv name val))))
        (-filter (lambda (line) (not (string-empty-p (string-trim line))))
                 (-filter (lambda (line) (not (string-prefix-p "#" line)))
                          (split-string
                           (with-temp-buffer
                             (insert-file-contents-literally (concat (project-root (project-current)) ".envrc"))
                             (buffer-string)
                             )
                           "\n"))
                 )
        )
  )

(defun unset-env-vars-from-envrc ()
  (interactive)
  (mapc (lambda (env-var-string)
          (let* ((split (split-string env-var-string "="))
                 (name (cl-first split))
                 (val (cl-second split)))
            (when (and name val)
              (setq val (string-replace " " "\\ " val))
              (setenv name nil))))
        (-filter (lambda (line) (not (string-empty-p (string-trim line))))
                 (-filter (lambda (line) (not (string-prefix-p "#" line)))
                          (split-string
                           (with-temp-buffer
                             (insert-file-contents-literally (concat (project-root (project-current)) ".envrc"))
                             (buffer-string)
                             )
                           "\n"))
                 )
        )
  )

(defvar my-ediff-prior-window-configuration nil)
(defun vc-ediff-file-at-point ()
  (interactive)
  (when (eq major-mode 'diff-mode)
    (setq my-ediff-prior-window-configuration (current-window-configuration))
    (let ((old-revision (first diff-vc-revisions))
          (new-revision (second diff-vc-revisions))
          (file-to-diff (save-window-excursion
                          (diff-goto-source)
                          (buffer-file-name))))
      (vc-version-ediff `(,file-to-diff) old-revision new-revision))))

(add-hook 'ediff-quit-hook
          (lambda () (when (window-configuration-p my-ediff-prior-window-configuration)
                       (set-window-configuration my-ediff-prior-window-configuration)))
          100)
