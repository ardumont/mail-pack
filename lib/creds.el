;; A small library (non optimal) to deal with more entries than just credentials
;; (The search is linear so not optimal)

;; Here is an example of .authinfo
;; machine machine0 port http login nouser password nopass
;; machine machine1 login some-login password some-pwd port 993
;; machine machine2 login some-login port 587 password some-pwd
;; machine jabber         login some-login password some-pwd
;; machine description    name "my name is" blog some-blog mail some-mail

(defun read-lines (filepath)
  "Return a list of lines from a file."
  (with-temp-buffer
    (insert-file-contents filepath)
    (mapcar (lambda (l) (split-string l "[ ]+")) (split-string (buffer-string) "\n" t))))

;; Here is the result
;; (read-lines "~/.authinfo")
;; (("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")
;;  ("machine" "machine1" "login" "some-login" "password" "some-pwd" "port" "993")
;;  ("machine" "machine2" "login" "some-login" "port" "587" "password" "some-pwd")
;;  ("machine" "jabber" "login" "some-login" "password" "some-pwd")
;;  ("machine" "description" "name" "\"my" "name" "is\"" "blog" "some-blog" "mail" "some-mail"))
;; (setq dat (read-lines "~/.authinfo"))

(defun get-creds (data entry-name)
  "Return the data list for the line entry-name"
  (if data
      (let* ((d     (car data))
             (entry (cadr d)))
        (if (equal entry entry-name)
            d
          (get-creds (cdr data) entry-name)))))

;;(get-creds dat "machine0")
;; ("machine" "machine0" "port" "http" "login" "nouser" "password" "nopass")

;;(get-creds da "nil")
;; nil

(defun get-entry (data entry)
  "Given a data list, return the entry in that list"
  (if data
      (let* ((k (car data))
             (v (cadr data)))
        (if (equal k entry)
            v
          (get-entry (cddr data) entry)))))
;; (setq machine (get-creds dat "machine0"))
;; (get-entry machine "machine")
;; "machine0"
;; (get-entry machine "port")
;; "http"
;; (get-entry machine "login")
;; "nouser"
;; (get-entry machine "password")
;; "nopass"
