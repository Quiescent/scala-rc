;;; scala-rc --- scala repl completion -*- lexical-binding: t; -*-

;;; Commentary:

;; In a Emacs shell processes (i.e. those started with comint) all
;; input to the shell process is wrapped and interpreted by Emacs
;; first.  This is great for uniform features across shell processes
;; in Emacs and makes for a great experience except that in some cases
;; we miss out on features.
;;
;; In a Scala REPL one can hit tab to receive a list of candidate
;; completions for a symbol.  In shell this is not possible because
;; <TAB> calls the Emacs function `completion-at-point'.  We need to
;; teach completion at point how to complete symbols when we're in a
;; scala repl.
;;
;; This file provides a minor mode which can be activated at any time.
;; (It will refuse to activate if the mode is not a comint or eshell
;; derivitive.)  It will modify `completion-at-point' functions so
;; that it includes the ability to complete Scala, but:
;;
;; /IT WILL NOT ENSURE THAT YOU'RE ACTUALLY IN A SCALA REPL!/
;;
;; To use this mode, simply call it when the Scala REPL is up and use
;; tab as you usually would :)

;;; Code:

(eval-when-compile
  (require 'cl))

(define-minor-mode scala-rc-mode
  "Scala REPL Completion mode.

Activate this mode when the repl is up (i.e. the prompt is
\"scala>\") and you'll be able to complete symbols using
completion at point."
  :init-value nil
  :lighter nil
  :group 'scala-rc
  :global nil
  (if (not (derived-mode-p 'comint-mode 'eshell-mode))
      (error "Can't start scala-rc in a mode which is not derived from comint or eshell")
    (if (null scala-rc-mode)
        (scala-rc-deactivate)
      (scala-rc-activate))))

(defvar scala-rc-old-completion-functions nil
  "The value of `completion-at-point' before the mode was activated.")

(make-variable-buffer-local 'scala-rc-old-completion-functions)

(defconst scala-rc-error-output-magic-string "<console>"
  "If this string is found then it's assumed that the request for completions failed.")

(defconst scala-rc-get-completions-scala-source-string
  "scala.reflect.runtime.currentMirror.classSymbol(%s.getClass).toType.members.map(_.toString.split(\" \")(1)).toSet.foreach(println)"
  "A template line of scala code used to get completions.

Should be used with `format' in order to replace the %s with the
target symbol.

Each completion is printed back one per line for easy
interpretation by `scala-rc'.")

(defvar scala-rc-busy nil
  "Used to prevent multiple simultaneous commands to scala process.")

(make-variable-buffer-local 'scala-rc-busy)

(defvar scala-rc-output-filter-string nil
  "A buffer which holds the output of `scala-rc-send-string-no-output'.")

(make-variable-buffer-local 'scala-rc-output-filter-buffer)

;; The plumbing for this is basically an adaptation of what python.el
;; does.  I don't attribute this work to myself.  The file distributed
;; with emacs attributes this to:
;; Author: Fabián E. Gallina <fgallina@gnu.org>
(defun scala-rc-send-string-no-output (string)
  "Send STRING to this buffers scala process and return the output."
  (let ((process (get-buffer-process (current-buffer)))
        (comint-preoutput-filter-functions (list #'scala-rc-output-filter))
        (scala-rc-busy t)
        (inhibit-quit t))
    (or (with-local-quit
          (comint-send-string process (concat string "\n"))
          (while scala-rc-busy
            (accept-process-output process))
          (prog1
              scala-rc-output-filter-string
            (setq scala-rc-output-filter-string nil)))
        (with-current-buffer (process-buffer process)
          (comint-interrupt-subjob)))))

(defun scala-rc-output-filter (output)
  "Concatenate OUTPUT onto the output buffer.

Signal done when the process tells us so."
  (progn
    (setq scala-rc-output-filter-string
          (concat scala-rc-output-filter-string output))
    (when (scala-rc-end-of-output-p scala-rc-output-filter-string)
      (setq scala-rc-output-filter-string
            (substring scala-rc-output-filter-string
                       0
                       (cl-position ? scala-rc-output-filter-string)))
      (setq scala-rc-busy nil))
    ""))

(defun scala-rc-end-of-output-p (output)
  "Produce t if OUTPUT is the end of output from the Scala REPL."
  (string-match "\r?\n?scala>" output))

(defun scala-rc-class-members (class)
  "Produce the members of CLASS."
  (let* ((cleaned (substring class 0 (cl-position ?. class :from-end t)))
         (command (format scala-rc-get-completions-scala-source-string
                          cleaned)))
    (mapcar (lambda (option) (concat cleaned "." option))
            (cl-remove-if (apply-partially #'string-match-p scala-rc-error-output-magic-string)
                          (split-string (scala-rc-send-string-no-output command) "\n" t)))))

(defun scala-rc-completion-at-point ()
  "A completion at point function of scala REPLs.

So far it handles expanding class members."
  (let* ((point (point))
         (end   (point))
         (start (save-excursion
                  (search-backward " ")
                  (forward-char)
                  (point))))
    (list start end
          (completion-table-dynamic #'scala-rc-class-members))))

(defun scala-rc-activate ()
  "Perform setup for `scala-rc' mode."
  (add-to-list 'completion-at-point-functions #'scala-rc-completion-at-point))

(defun scala-rc-deactivate ()
  "Perform teardown for `scala-rc' mode."
  (setq completion-at-point-functions (remove #'scala-rc-completion-at-point
                                              completion-at-point-functions)))

(provide 'scala-rc)
;;; scala-rc ends here
