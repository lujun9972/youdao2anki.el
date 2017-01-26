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
            (fields-map-alist . ,fields-map-alist)))))


(defun save-to-anki (json)
  "Format request result of WORD."
  (or config-info (set-config-info))
  (let* ((deck (cdr assoc 'deck config-info))
         (model (cdr assoc 'model config-info))
         (fields-map-alist (cdr assoc 'fields-map-alist config-info))
         (sentence     (sentence-at-point))
         (query        (assoc-default 'query       json)) ; string
         (translation  (assoc-default 'translation json)) ; array
         (errorCode    (assoc-default 'errorCode   json)) ; number
         (web          (assoc-default 'web         json)) ; array
         (basic        (assoc-default 'basic       json)) ; alist
         ;; construct data for display
         (phonetic (assoc-default 'phonetic basic))
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
                             query translation-str))))
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


