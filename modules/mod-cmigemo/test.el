(set-language-environment "Japanese")
(require 'ert)

(add-to-list 'load-path
             (file-name-directory (or #$ (expand-file-name (buffer-file-name)))))
(require 'mod-cmigemo)
(setq *cmigemo* (cmigemo-open "utf8.d/migemo-dict"))

(ert-deftest mod-cmigemo-query-kanji ()
  (should (string-equal
           (cmigemo-query *cmigemo* "kuni")
           "\\(?:[訓國圀国邦]\\|ｸ\\s-*ﾆ\\|ク\\s-*ニ\\|地\\s-*祇\\|都\\s-*子\\|州\\s-*光\\|狗\\s-*肉\\|苦\\s-*肉\\|六\\s-*合\\|久\\s-*\\(?:邇\\|仁\\s-*[子雄]\\)\\|く\\s-*に\\|ｋ\\s-*ｕ\\s-*ｎ\\s-*ｉ\\|k\\s-*u\\s-*n\\s-*i\\)")))

(ert-deftest mod-cmigemo-query-mata ()
  (should (string-equal
           (cmigemo-query *cmigemo* ".*+?[]^$")
           "\\(?:．\\s-*＊\\s-*＋\\s-*？\\s-*［\\s-*］\\s-*＾\\s-*＄\\|\\.\\s-*\\*\\s-*\\+\\s-*\\?\\s-*\\[\\s-*\\]\\s-*\\^\\s-*\\$\\)")))


;; Local Variables:
;; coding:utf-8
;; End: