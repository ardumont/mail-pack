(source gnu)
(source melpa)

(package-file "mail-pack.el")

(files "mail-pack.el"
       "mail-pack-rules.el"
       "mail-pack-pkg.el"
       "README.md")

(depends-on "google-contacts")
(depends-on "mu" :git "https://github.com/djcb/mu" :files ("mu4e/*.el"))
;; hack as mu4e not in melpa nor marmalade
;; mu4e in el-get but el-get not supported by Cask...

(development
 (depends-on "undercover")
 (depends-on "ert-runner")
 (depends-on "ert")
 (depends-on "ert-expectations")
 (depends-on "el-mock"))
