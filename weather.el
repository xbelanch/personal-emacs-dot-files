;; Weather Forecast
;; Sunshine
;; https://github.com/aaronbieber/sunshine.el

(use-package sunshine
  :defer t
  :config
  (progn
    ;; The "openweathermap-api" file is supposed to contain this line:
    ( setq sunshine-appid "70058ebcaaae53064d6b197c1880be4a")
    ;; Sign up at http://openweathermap.org/ to get your API KEY.
    (load (locate-user-emacs-file "openweathermap-api") :noerror :nomessage)
    (setq sunshine-units 'metric)
    (setq sunshine-location "Barcelona,Spain")
    (setq sunshine-show-icons t)))
