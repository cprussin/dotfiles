;;; emacs-rc-hass.el --- Configuration for home-assistant
;;;
;;; Commentary:
;;;
;;; Configuration for home-assistant dashboard.
;;;
;;; Code:

(eval-when-compile (require 'use-package))

(use-package emacs-rc-keybindings
  :demand
  :commands general-define-key)

(use-package hass
  :after general
  :general (apps-menu-def "h" '(hass-dash-open :which-key "Home Assistant"))
  :init (setq hass-host "home-assistant.internal.prussin.net"
              hass-port 443
              hass-apikey (lambda () (auth-source-pass-get "API Key" "Connor/Home/Home Assistant"))
              hass-dash-layouts
              '((default . ((hass-dash-group :title "Lights"
                                             :format "%t\n%v\n"
                                             (hass-dash-group :title "Kitchen / Dining"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Kitchen"
                                                                                :entity-id "light.kitchen_lights")
                                                              (hass-dash-toggle :label "Dining Room"
                                                                                :entity-id "light.dining_room_lights")
                                                              (hass-dash-toggle :label "Bar"
                                                                                :entity-id "light.bar_lights"))

                                             (hass-dash-group :title "Living Room"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Lights"
                                                                                :entity-id "light.living_room_lights")
                                                              (hass-dash-toggle :label "Entertainment Center"
                                                                                :entity-id "light.entertainment_center_lights"))

                                             (hass-dash-group :title "Family Room"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Lights"
                                                                                :entity-id "light.family_room_lights"))

                                             (hass-dash-group :title "Master Bedroom"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Lights"
                                                                                :entity-id "light.master_bedroom_lights")
                                                              (hass-dash-toggle :label "Fan Light"
                                                                                :entity-id "light.master_bedroom_fan_light"))

                                             (hass-dash-group :title "Bodhi's Room"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Lights"
                                                                                :entity-id "light.bodhi_s_room_lights")
                                                              (hass-dash-toggle :label "Fan Light"
                                                                                :entity-id "light.bodhi_s_room_fan_light"))

                                             (hass-dash-group :title "Aiden's Room"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Lights"
                                                                                :entity-id "light.aiden_s_room_lights")
                                                              (hass-dash-toggle :label "Fan Light"
                                                                                :entity-id "light.aiden_s_room_fan_light"))

                                             (hass-dash-group :title "Guest Room"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Lights"
                                                                                :entity-id "light.guest_room_lights")
                                                              (hass-dash-toggle :label "Fan Light"
                                                                                :entity-id "light.guest_room_fan_light"))

                                             (hass-dash-group :title "Miscellaneous"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Front Porch"
                                                                                :entity-id "switch.front_porch_light")
                                                              (hass-dash-toggle :label "Entry"
                                                                                :entity-id "switch.entry_light")
                                                              (hass-dash-toggle :label "Hall"
                                                                                :entity-id "light.hall_lights"))

                                             (hass-dash-group :title "Turtles"
                                                              :title-face outline-2
                                                              (hass-dash-toggle :label "Left Turtle Tank"
                                                                                :entity-id "switch.plug_5")
                                                              (hass-dash-toggle :label "Right Turtle Tank"
                                                                                :entity-id "switch.plug_1")))

                            (hass-dash-group :title "Fans"
                                             :format "%t\n%v\n"
                                             (hass-dash-toggle :label "Master Bedroom"
                                                               :entity-id "fan.master_bedroom_fan")
                                             (hass-dash-toggle :label "Bodhi's Room"
                                                               :entity-id "fan.bodhi_s_room_fan")
                                             (hass-dash-toggle :label "Aiden's Room"
                                                               :entity-id "fan.aiden_s_room_fan")
                                             (hass-dash-toggle :label "Guest Room"
                                                               :entity-id "fan.guest_room_fan"))

                            (hass-dash-group :title "Occupancy"
                                             :format "%t\n%v\n"
                                             (hass-dash-state :label "Dining Room"
                                                              :entity-id "binary_sensor.thermostat_occupancy")
                                             (hass-dash-state :label "Master Bedroom"
                                                              :entity-id "binary_sensor.room_sensor_occupancy"))

                            (hass-dash-group :title "Power Consumption"
                                             (hass-dash-state :label "Consumption"
                                                              :entity-id "sensor.emu_2")))))))

(provide 'emacs-rc-hass)
;;; emacs-rc-hass.el ends here
