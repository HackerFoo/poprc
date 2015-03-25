;;; poprc-mode --- mode for the poprc interpreter

;;; Commentary:
;; based on http://www.masteringemacs.org/article/comint-writing-command-interpreter

;;; Code:
(require 'comint)

(defvar poprc-cli-file-path "~/src/poprc/eval"
  "Path to the program used by `run-poprc'.")

(defvar poprc-cli-arguments '()
  "Commandline arguments to pass to `poprc-cli'.")

(defvar poprc-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-poprc'.")

(defvar poprc-prompt-regexp "^\\(?::\\) "
  "Prompt for `run-poprc'.")

(defun run-poprc ()
  "Run an inferior instance of `poprc-cli' inside Emacs."
  (interactive)
  (let* ((poprc-program poprc-cli-file-path)
         (buffer (comint-check-proc "Poprc")))
    ;; pop to the "*Poprc*" buffer if the process is dead, the
    ;; buffer is missing or it's got the wrong mode.
    (pop-to-buffer-same-window
     (if (or buffer (not (derived-mode-p 'poprc-mode))
             (comint-check-proc (current-buffer)))
         (get-buffer-create (or buffer "*Poprc*"))
       (current-buffer)))
    ;; create the comint process if there is no buffer.
    (unless buffer
      (apply 'make-comint-in-buffer "Poprc" buffer
             poprc-program poprc-cli-arguments)
      (poprc-mode))))

(defun poprc--initialize ()
  "Helper function to initialize Poprc."
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode poprc-mode comint-mode "Poprc"
  "Major mode for `run-poprc'.

\\<poprc-mode-map>"
  nil "Poprc"
  ;; this sets up the prompt so it matches things like: [foo@bar]
  (setq comint-prompt-regexp poprc-prompt-regexp)
  ;; this makes it read only; a contentious subject as some prefer the
  ;; buffer to be overwritable.
  (setq comint-prompt-read-only t)
  ;; this makes it so commands like M-{ and M-} work.
  (set (make-local-variable 'paragraph-separate) "^[^:].*$")
  (set (make-local-variable 'font-lock-defaults) '(poprc-font-lock-keywords t))
  (set (make-local-variable 'paragraph-start) poprc-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'poprc-mode-hook 'poprc--initialize)

(set (make-local-variable 'font-lock-defaults) '(poprc-font-lock-keywords t))

(defconst poprc-keywords
  '(":l" ":c" "cut" "drop" "dup" "id" "popr" "pushl" "pushr" "select" "swap"))

(defvar poprc-font-lock-keywords
  (list
   ;; highlight all the reserved commands.
   `(,(concat "\\_<" (regexp-opt poprc-keywords) "\\_>") . font-lock-keyword-face))
  "Additional expressions to highlight in `poprc-mode'.")

(provide 'poprc-mode)
;;; poprc-mode.el ends here
