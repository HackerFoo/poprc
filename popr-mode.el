;;; popr-mode.el --- Major mode for editing Popr code.

;; Copyright (C) 2020 Dustin DeWeese

;; Author: Dustin DeWeese
;; Version: 0.1

;;; Commentary:

;; This package provides a major mode for editing Popr code.

;;; Code:

(defvar popr-bracket-face 'popr-bracket-face)

(defface popr-bracket-face
    '((((class color))
       :weight bold))
  "Face for displaying a popr bracket."
  :group 'faces)

(defvar popr-highlights
      '(("\\(?:^\\| \\)\\(_+[^_[:space:]].*\\(\n.*?\\)*.*[^_[:space:]]_+[[:space:]]\\|__+ .*$\\)" 0 font-lock-comment-face t)
        ("\\<\\(module\\|imports\\)\\>" . font-lock-keyword-face)
        ("\\[\\|\\]" . popr-bracket-face)
        ("\\(^ *[^[:space:]]+\\)\\s-*:" . font-lock-function-name-face)
        ("\\<\\([A-Z][A-Za-z0-9]+\\b\\|\\b[0-9]+\\)\\>" . font-lock-constant-face)))

(defvar popr-imenu-generic-expression
  '(("Module" "^ *module +\\([^ \t\r\n]+\\):" 1)
    ("Word" "^ *\\([^ \t\r\n]+\\):" 1)))

;;;###autoload
(define-derived-mode popr-mode fundamental-mode "popr"
  "Major mode for editing Popr code."
  (setq font-lock-defaults '(popr-highlights))
  (setq imenu-generic-expression popr-imenu-generic-expression)
  (set (make-local-variable 'font-lock-multiline) t)
  (add-hook 'font-lock-extend-region-functions
            'test-font-lock-extend-region)
)

;; from http://makble.com/emacs-font-lock-how-to-highlight-multiline-text
(defun test-font-lock-extend-region ()
  "Extend the search region to include an entire block of text."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t) (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (beginning-of-line)
        (setq font-lock-end (point)))
      (setq font-lock-beg found))))

(provide 'popr-mode)

;;; popr-mode.el ends here
