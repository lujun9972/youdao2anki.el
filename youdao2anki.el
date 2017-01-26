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

(defcustom deck nil
  "Specify which deck the note will be saved into"
  :type 'string)

(defcustom model nil
  "Specify the note's model"
  :type 'string)

(defcustom fields-map-alist '((word . "")
                              (phonetic . "")
                              (explains . "")
                              (sentence . ""))
  "Specify how to save to note"
  :type 'alist)

(defun set-deck ()
  "Set the deck"
  (interactive)
  (setq deck (completing-read "Select a deck:" (AnkiConnect-DeckNames))))

(defun set-model ()
  "Set the model"
  (interactive)
  (setq model (completing-read "Select a model:" (AnkiConnect-ModelNames))))

(defun set-fields-map-alist ()
  (interactive)
  (let* ((deck (set-deck))
         (model (set-model))
         (fields (AnkiConnect-ModelFieldNames model))
         (keys '(word phonetic explains sentence)))
    (setq fields-map-alist (mapcar (lambda (key)
                                     (let ((prompt (format "which field used to store the %s" key)))
                                       (cons key (completing-read prompt fields))))
                                   keys))))

(defun save-to-anki (json)
  "Format request result of WORD."
  (fields-map-alist or (set-fields-map-alist))
  (let* ((sentence     (sentence-at-point))
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
                   web "<br>")))
    (if basic
        (let ((explains (format "* Basic Explains<br>%s<br><br>* Web References<br>%s<br>"
                                basic-explains-str web-str)))
          (AnkiConnect-AddNote deck model `((,(cdr (assoc 'word fields-map-alist)) . ,query)
                                            (,(cdr (assoc 'phonetic fields-map-alist)) . ,phonetic)
                                            (,(cdr (assoc 'explains fields-map-alist)) . ,explains)
                                            (,(cdr (assoc 'sentence fields-map-alist)) . ,sentence))))
      (let ((explains (format "* Translation<br>%s<br>"
                              query translation-str)))
        (AnkiConnect-AddNote deck model `((,(cdr (assoc 'word fields-map-alist)) . ,query)
                                          (,(cdr (assoc 'explains fields-map-alist)) . ,explains)
                                          (,(cdr (assoc 'sentence fields-map-alist)) . ,sentence))))))
  json)

(defun turn-on ()
  (interactive)
  (advice-add 'youdao-dictionary--request :filter-return 'youdao2anki-save-to-anki))
(defun turn-off ()
  (interactive)
  (advice-remove 'youdao-dictionary--request 'youdao2anki-save-to-anki))
)


