(defvar popr-brecket-face 'popr-brecket-face)

(defface popr-brecket-face
    '((((class color))
       :weight bold))
  "Face for displaying a popr bracket."
  :group 'faces)

(setq popr-highlights
      '(("__ .*". font-lock-comment-face)
        ("_\\w.*\\w_" . font-lock-comment-face)
        ("module\\|imports" . font-lock-keyword-face)
        ("\\[\\|\\]" . popr-brecket-face)
        ("^\\s-*\\(\\w+\\):" . font-lock-function-name-face)))

(define-derived-mode popr-mode fundamental-mode "popr"
  "major mode for editing popr language code."
  (setq font-lock-defaults '(popr-highlights)))
