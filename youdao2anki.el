;; -*- lexical-binding: t; -*-
(require 'AnkiConnect)
(require 'youdao-dictionary)
(eval-when-compile (require 'names))

(defgroup youdao2anki nil
  "youdao2anki interface for Emacs."
  :prefix "youdao2anki-"
  :group 'tools
  :link '(url-link :tag "Github" "https://github.com/lujun9972/youdao2anki.el"))

;;;###autoload
(define-namespace youdao2anki-

(defvar config-info nil
  "Use `youdao2anki-set-config-info' to set the config info")

(defcustom config-file "~/.youdao2anki.cfg"
  "File used to store configure information")

(defun load-config-info (&optional cfg-file)
  "Load configure information from CFG-FILE"
  (let ((cfg-file (or cfg-file config-file)))
    (setq config-info (with-temp-buffer
                        (insert-file-contents cfg-file)
                        (car (read-from-string (buffer-string)))))))

(defun save-config-info (&optional cfg-file)
  "Save configure information into CFG-FILE"
  (let ((cfg-file (or cfg-file config-file)))
    (with-temp-file cfg-file
      (prin1 config-info (current-buffer)))))

(defun set-config-info ()
  "Set the deck,model and fields map relationship"
  (interactive)
  (let* ((deck (completing-read "Select a deck:" (AnkiConnect-DeckNames)))
         (model (completing-read "Select a model:" (AnkiConnect-ModelNames)))
         (fields (cons " " (AnkiConnect-ModelFieldNames model)))
         (keys '(word phonetic explains sentence))
         (fields-map-alist (mapcar (lambda (key)
                                     (let ((prompt (format "which field used to store the %s" key)))
                                       (cons key (completing-read prompt fields))))
                                   keys)))
    (setq config-info
          `((deck . ,deck)
            (model . ,model)
            (fields-map-alist . ,fields-map-alist))))
  (save-config-info))


(defun save-to-anki (json)
  "Save the JSON to anki."
  (or config-info (load-config-info) (set-config-info))
  (let* ((query        (or (assoc-default 'query       json) "")) ; string
         (sentence     (replace-regexp-in-string (regexp-quote query)
                                                 (lambda (match)
                                                   (format "<b>%s</b>" match))
                                                 (sentence-at-point)))
         (translation  (or (assoc-default 'translation json) "")) ; array
         (errorCode    (or (assoc-default 'errorCode   json) "")) ; number
         (web          (or (assoc-default 'web         json) "")) ; array
         (basic        (or (assoc-default 'basic       json) "")) ; alist
         ;; construct data for display
         (phonetic (or (assoc-default 'phonetic basic) ""))
         (translation-str (mapconcat
                           (lambda (trans) (concat "- " trans))
                           translation "<br>"))
         (basic-explains-str (mapconcat
                              (lambda (explain) (concat "- " explain))
                              (assoc-default 'explains basic) "<br>"))
         (web-str (mapconcat
                   (lambda (k-v)
                     (format "- %s :: %s"
                             (assoc-default 'key k-v)
                             (mapconcat 'identity (assoc-default 'value k-v) "; ")))
                   web "<br>"))
         (explains (if basic
                       (format "* Basic Explains<br>%s<br><br>* Web References<br>%s<br>"
                               basic-explains-str web-str)
                     (format "* Translation<br>%s<br>"
                             query translation-str)))
         (deck (assoc-default 'deck config-info))
         (model (assoc-default 'model config-info))
         (fields-map-alist (assoc-default 'fields-map-alist config-info)))
    (AnkiConnect-AddNote deck model `((,(cdr (assoc 'word fields-map-alist)) . ,query)
                                      (,(cdr (assoc 'phonetic fields-map-alist)) . ,phonetic)
                                      (,(cdr (assoc 'explains fields-map-alist)) . ,explains)
                                      (,(cdr (assoc 'sentence fields-map-alist)) . ,sentence))))
  json)

(defun turn-on ()
  (interactive)
  (advice-add 'youdao-dictionary--request :filter-return 'youdao2anki-save-to-anki))
(defun turn-off ()
  (interactive)
  (advice-remove 'youdao-dictionary--request 'youdao2anki-save-to-anki))
)


