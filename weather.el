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

;; Forecast
;; https://github.com/cadadr/forecast.el
(use-package forecast
  ;; deferring not needed as the package is set to autoload on M-x forecast
  ;; :defer 1 ; Wait for at least a second after emacs has loaded.
  ;;          ; The emacs frame needs to be set up properly before `find-font' call.
  :defer t
  :config
  (progn
    ;; Use Quivira font for moon phases
    (when (find-font (font-spec :name "Quivira"))
      (set-face-attribute 'forecast-moon-phase nil :font "Quivira"))

    ;; The "darksky-api" file is supposed to contain this line:
    (setq forecast-api-key "8993a42dbbda3be76efbf8ed840496ce")
    ;; Register at https://darksky.net/dev/account/ to get your API KEY.
    (load (locate-user-emacs-file "darksky-api") :noerror :nomessage)

    ;; The below calendar-* variables from `solar.el' are used by `forecast.el'.
    (use-package solar
      :config
      (progn
        (setq calendar-latitude 41.3851)
        (setq calendar-longitude 2.1734)
	(setq forecast-language "es")
	(setq forecast-units "ca")
        (setq calendar-location-name "Barcelona, Catalunya")))

    (setq forecast-units 'es)))

