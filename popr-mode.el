;;; package --- Major mode for editing Popr code.
;;; Commentary:

;;; Code:

(defvar popr-bracket-face 'popr-bracket-face)

(defface popr-bracket-face
    '((((class color))
       :weight bold))
  "Face for displaying a popr bracket."
  :group 'faces)

(defvar popr-highlights
      '(("\\(^\\| \\)\\(_+[^_[:space:]].*\\(\n.*?\\)*.*[^_[:space:]]_+\\)\\|\\(__+ .*$\\)" . font-lock-comment-face)
        ("\\<\\(module\\|imports\\)\\>" . font-lock-keyword-face)
        ("\\[\\|\\]" . popr-bracket-face)
        ("\\([^[:space:]]+\\)\\s-*:\\s-" . font-lock-function-name-face)
        ("\\<\\([A-Z][A-Za-z0-9]+\\b\\|\\b[0-9]+\\)\\>" . font-lock-constant-face)))

(defvar popr-imenu-generic-expression
  '(("Module" "^ *module +\\([^ \t\r\n]+\\):" 1)
    ("Word" "^ *\\([^ \t\r\n]+\\):" 1)))

(define-derived-mode popr-mode fundamental-mode "popr"
  "Major mode for editing Popr code."
  (setq font-lock-defaults '(popr-highlights))
  (setq imenu-generic-expression popr-imenu-generic-expression))

(provide 'popr-mode)

;;; popr-mode.el ends here
